namespace Faux

open System
open NuGet
open Show

module Types =
  [<StructuralEquality
   ;StructuralComparison>]
  type SemVer =
    { Major : uint16
      Minor : uint16
      Build : uint16
      Special : string }
    static member show (v : SemVer) : string =
      if v.Special <> "" then
        sprintf "%i.%i.%i.%s" v.Major v.Minor v.Build v.Special
      else
        sprintf "%i.%i.%i" v.Major v.Minor v.Build

  [<StructuralEquality
   ;StructuralComparison>]
  type Version =
    | SemVer of SemVer
    | FreeVer of string
    static member show (v : Version) : string =
      match v with
        | SemVer(sv) -> show sv
        | FreeVer(fv) -> fv

  let semVer : int -> int -> int -> string -> Version =
    fun maj min b s ->
      SemVer({ Major = uint16 maj;
               Minor = uint16 min;
               Build = uint16 b;
               Special = s })

  let freeVer : string -> Version =
    fun s -> FreeVer(s)

  [<StructuralEquality
   ;StructuralComparison>]
  type AssemblyName =
    { Root : string
      Subs : string list }
    member v.Segments = v.Root :: v.Subs
    static member show (n : AssemblyName) : string =
      String.Join(".", n.Root :: n.Subs)

  let assemblyName : string -> AssemblyName =
    fun name ->
      let segments = List.ofArray (name.Split([|'.'|]))
      if List.isEmpty segments then
        failwith "AssemblyName requires at least one segment."
      else
        { Root = List.head segments
          Subs = List.tail segments }

  [<StructuralEquality
   ;StructuralComparison>]
  type NugetId =
    { Name : AssemblyName
      Version : Version }
    static member show (n : NugetId) : string =
      sprintf "%s.%s" (show n.Name) (show n.Version)

  let nugetId : string -> Version -> NugetId =
    fun name version ->
      { Name = assemblyName name
        Version = version }

  [<StructuralEquality
   ;StructuralComparison>]
  type AssemblyId =
    | AssemblyId of NugetId
    | AssemblyPathId of string
    | AssemblyNameId of string list
    static member show (id : AssemblyId) : string =
      match id with
        | AssemblyId(nid) -> show nid
        | AssemblyPathId(path) -> path
        | AssemblyNameId(segments) -> String.concat "." segments

  let assemblyId : string -> Version -> AssemblyId =
    fun name version ->
      AssemblyId(nugetId name version)

  let assemblyPathId : string -> AssemblyId =
    fun path -> AssemblyPathId(path)

  let assemblyNameId : string -> AssemblyId =
    fun name ->
      let segments = List.ofArray (name.Split([|'.'|]))
      if List.isEmpty segments then
        failwith "AssemblyName requires at least one segment."
      else
        AssemblyNameId(segments)

  [<StructuralEquality
   ;StructuralComparison>]
  type ProjImport =
    { Path : string }
    static member show (i : ProjImport) : string =
      i.Path

  [<StructuralEquality
   ;StructuralComparison>]
  type ProjMeta =
    { Authors : string list
      Description : string }

  [<StructuralEquality
   ;StructuralComparison>]
  type Dependency =
    | AssemblyDep of AssemblyId
    | NugetDep of NugetId
    | ProjectDep of MsBuildProj
    static member show (d : Dependency) : string =
      match d with
        | AssemblyDep(ad) -> show ad
        | NugetDep(nd) -> show nd
        | ProjectDep(pd) -> show pd.Id
    static member assemblyId : Dependency -> AssemblyId =
      fun d ->
        match d with
          | AssemblyDep(ad) -> ad
          | NugetDep(nd) -> AssemblyId(nd)
          | ProjectDep(pd) -> AssemblyId(pd.Id)

  and MsBuildProj =
    { Id : NugetId
      Sources : string list
      Imports : ProjImport list
      Dir : string
      ProjFile : string
      Deps : Dependency list
      Meta : ProjMeta }
    member p.Name = p.Id.Name
    member p.Version = p.Id.Version
    member p.AssemblyDeps =
      seq { for dep in p.Deps do
            match dep with
              | AssemblyDep(d) -> yield! [d]
              | _ -> yield! [] }
    member p.NugetDeps =
      seq { for dep in p.Deps do
            match dep with
              | NugetDep(d) -> yield! [d]
              | _ -> yield! [] }
    member p.ProjectDeps =
      seq { for dep in p.Deps do
            match dep with
              | ProjectDep(d) -> yield! [d]
              | _ -> yield! [] }


  type NugetRepo =
    | LocalRepo of string
    | HttpRepo of string
    | AggregateRepo of NugetRepo list
    member r.PackageRepository : IPackageRepository =
      match r with
        | LocalRepo(s) ->
          new LocalPackageRepository(s) :> IPackageRepository
        | HttpRepo(s) ->
          new DataServicePackageRepository(
            new RedirectedHttpClient(new Uri(s))) :> IPackageRepository
        | AggregateRepo(rs) ->
          new AggregateRepository(
            Seq.map
              (fun (r' : NugetRepo) -> r'.PackageRepository)
              rs) :> IPackageRepository
