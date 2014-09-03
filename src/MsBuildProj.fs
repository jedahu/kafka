namespace Faux

open System
open System.IO
open System.Xml
open System.Xml.Linq
open NuGet
open Microsoft.Build
open Microsoft.Build.Framework
open Microsoft.Build.Logging
open Show
open Types
open Versions
open XmlSyntax

module MsBuildProj =

  let projAssemblyId : MsBuildProj -> AssemblyId =
    fun p -> AssemblyId(p.Id)

  let private msProjNS : XNamespace =
    xnamespace "http://schemas.microsoft.com/developer/msbuild/2003"

  let private elem = xelem' msProjNS

  let private projectSearchPaths : MsBuildProj -> string seq =
    fun p ->
      seq { for d in p.ProjectDeps do
              yield sprintf "../%s/bin/Release" d.Dir
              yield sprintf "../%s/bin/Debug" d.Dir }

  let private dllPaths : MsBuildProj -> string list =
    fun p ->
      List.ofSeq
        (Seq.distinct
          (seq { for dll in
                   Directory.EnumerateFiles
                              ( (p.Dir + "/packages"),
                                "*.dll",
                                (SearchOption.AllDirectories) )
                                do
                 yield Path.GetDirectoryName(dll).Substring(p.Dir.Length + 1) }))

  let private searchPaths : MsBuildProj -> XElement =
    fun p ->
      let paths =
        String.concat
          ";"
          (List.concat
            [ (dllPaths p)
              (List.ofSeq (projectSearchPaths p))
              ["$(AssemblySearchPaths)"] ])
      elem "Target" ["Name", "BeforeResolveReferences"]
        [ elem "CreateProperty" ["Value", paths]
            [ elem "Output"
                [ "TaskParameter", "Value"
                  "PropertyName", "AssemblySearchPaths" ]
                [] ] ]

  let private reference : AssemblyId -> XNode =
    fun aid ->
      match aid with
        | AssemblyNameId(_) ->
          elem "Reference" ["Include", show aid]
            [elem "SpecificVersion" [] [xstr "False"]]
            :> XNode
        | AssemblyPathId(_) ->
          elem "Reference" ["Include", show aid] [] :> XNode
        | AssemblyId(id) ->
          elem "Reference"
            [("Include",
              sprintf "%s, Version=%s"
                (String.concat "." id.Name.Segments)
                (show id.Version))]
            [elem "SpecificVersion" [] [xstr "False"]]
            :> XNode

  let private references : MsBuildProj -> XElement =
    fun p ->
      elem "ItemGroup"
        []
        (List.map (reference << Dependency.assemblyId) (p.Deps))

  let private sources : MsBuildProj -> XNode =
    fun p ->
      elem "ItemGroup"
        []
        (List.map
          (fun s -> elem "Compile" ["Include", s] [] :> XNode)
          p.Sources)
        :> XNode

  let private imports : MsBuildProj -> XNode list =
    fun p ->
      List.map
        (fun (i : ProjImport) -> elem "Import" ["Project", show i] [] :> XNode)
        p.Imports

  let msProjXml : MsBuildProj -> XDocument =
    fun p ->
      xdoc
        (elem "Project"
          [ "ToolsVersion", "4.0"
            "DefaultTargets", "Build"]
          (List.concat
            [ [ elem "PropertyGroup"
                  []
                  [ elem "Configuration"
                      ["Condition", " '$(Configuration)' == '' "]
                      [xstr "Debug"]
                    elem "Platform"
                      ["Condition", " '$(Platform)' == '' "]
                      [xstr "AnyCPU"]
                    elem "OutputType" [] [xstr "Library"]
                    elem "AssemblyName" [] [xstr (show p.Name)]
                    elem "TargetFrameworkVersion" [] [xstr "v4.5"]
                    elem "OutputPath" [] [xstr "./build"] ]
                (references p)
                (sources p) ]
              (imports p)
              [ (searchPaths p) ] ]))

  let fetchProjDeps : IPackageRepository -> MsBuildProj -> bool =
    fun repo p ->
      let pkgMgr = new PackageManager(repo, p.Dir + "/packages")
      try
        Seq.iter
          (fun (nid : NugetId) ->
            pkgMgr.InstallPackage(
              show nid.Name,
              nugetSemVer nid.Version))
          p.NugetDeps
        true
      with
        | _ -> false

  let nuspecXml : MsBuildProj -> XDocument =
    fun p ->
      xdoc
        (xelem "package" []
          [ xelem "metadata" []
              [ xelem "id" [] [xstr (show p.Name)]
                xelem "version" [] [xstr (show p.Version)]
                xelem "description" [] [xstr p.Meta.Description]
                xelem "authors" [] [xstr (String.concat "; " p.Meta.Authors)]
                xelem "owners" [] [xstr (String.concat "; " p.Meta.Authors)]
                // xelem "licenseUrl" [] []
                // xelem "projectUrl" [] []
                // xelem "iconUrl" [] []
                xelem "requireLicenseAcceptance" [] [xstr "false"]
                // xelem "releaseNotes" [] []
                // xelem "copyright" [] []
                // xelem "tags" [] []
                xelem "dependencies" []
                  (List.ofSeq
                    (seq { for d in p.NugetDeps do
                           yield xelem "dependency"
                                   [ "id", (show d.Name)
                                     "version", (show d.Version) ]
                                   []
                                   :> XNode })) ]
            xelem "files" []
              [ xelem "file"
                  [ "src", sprintf "build\\%s.dll" (show p.Name)
                    "target", "lib" ]
                  [] ] ])

  let projFilePath : MsBuildProj -> string =
    fun p -> sprintf "%s/%s" p.Dir p.ProjFile

  let nuspecFilePath : MsBuildProj -> string =
    fun p -> sprintf "%s/%s.nuspec" p.Dir p.ProjFile

  let writeProjFile : MsBuildProj -> bool =
    fun p ->
      try
        (msProjXml p).Save(projFilePath p)
        true
      with
        | _ -> false

  let writeNuspecFile : MsBuildProj -> bool =
    fun p ->
      try
        (nuspecXml p).Save(nuspecFilePath p)
        true
      with
        | _ -> false

  let packProj : MsBuildProj -> bool =
    fun p ->
      try
        use nuspecStream = new MemoryStream()
        (nuspecXml p).Save(nuspecStream)
        nuspecStream.Position <- 0L
        let pb = new PackageBuilder(nuspecStream, Path.GetDirectoryName(nuspecFilePath p))
        use stream =
          new FileStream(
            sprintf "%s.nupkg" (show (projAssemblyId p)),
            FileMode.OpenOrCreate)
        pb.Save(stream)
        true
      with
        | ex ->
          printf "%s" ex.Message
          printf "%s" ex.StackTrace
          false

  let buildProj : string array -> MsBuildProj -> bool =
    fun targets p ->
      use projStream = new MemoryStream()
      (msProjXml p).Save(projStream)
      projStream.Position <- 0L
      let ep = new Evaluation.Project (XmlReader.Create(projStream))
      ep.FullPath <- projFilePath p
      ep.Build(targets, [new ConsoleLogger() :> ILogger])