namespace Faux

open Types

module Project =

  type Project =
    { Name : string
      Version : Version
      Dir : string
      Sources : string list
      Deps : Dependency list
      Meta : ProjMeta }

  let fsProj : Project -> MsBuildProj =
    fun p ->
      { Id = { Name = assemblyName p.Name; Version = p.Version }
        Dir = p.Dir
        Sources = p.Sources
        Deps = p.Deps
        Meta = p.Meta
        ProjFile = p.Dir + ".fsproj"
        Imports = [{ Path = "$(MSBuildExtensionsPath32)/../Microsoft SDKs/F#/3.0/Framework/v4.0/Microsoft.FSharp.Targets" }] }

  let csProj : Project -> MsBuildProj =
    fun p ->
      { Id = { Name = assemblyName p.Name; Version = p.Version }
        Dir = p.Dir
        Sources = p.Sources
        Deps = p.Deps
        Meta = p.Meta
        ProjFile = p.Dir + ".csproj"
        Imports = [{ Path = "$(MSBuildToolsPath)/Microsoft.CSharp.targets" }] }

  let semVer = Types.semVer
  let freeVer = Types.freeVer

  module Dep =
    let name : string -> Dependency =
      fun n -> AssemblyDep(assemblyNameId n)

    let path : string -> Dependency =
      fun p -> AssemblyDep(assemblyPathId p)

    let assembly : string -> int -> int -> int -> string -> Dependency =
      fun n maj min b s ->
        AssemblyDep(assemblyId n (semVer maj min b s))

    let nuget : string -> int -> int -> int -> string -> Dependency =
      fun n maj min b s ->
        NugetDep(nugetId n (semVer maj min b s))

    let assembly' : string -> string -> Dependency =
      fun n v -> AssemblyDep(AssemblyId(nugetId n (FreeVer(v))))

    let nuget' : string -> string -> Dependency =
      fun n v -> NugetDep(nugetId n (FreeVer(v)))
