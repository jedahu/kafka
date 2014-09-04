#I "bootstrap/build"
#r "System.dll"
#r "Steel.Finabs.dll"

open Faux.Cli
open Faux.Project

open System
open System.IO

let lib =
  fsProj
    { Name = "Steel.Finabs"
      Version = semVer 0 1 0 ""
      Dir = "src"
      Sources =
        [ "Task.fs"
          "Show.fs"
          "Types.fs"
          "Versions.fs"
          "XmlSyntax.fs"
          "MsBuildProj.fs"
          "Project.fs"
          "Targets.fs"
          "TransitiveTargets.fs"
          "Cli.fs" ]
      Deps =
        [ Dep.name "mscorlib"
          Dep.name "FSharp.Core"
          Dep.nuget "FSharpx.Core" 1 8 41 ""
          Dep.name "System"
          Dep.name "System.Xml"
          Dep.name "System.Xml.Linq"
          Dep.name "Microsoft.Build"
          Dep.name "Microsoft.Build.Framework"
          Dep.name "WindowsBase"
          Dep.nuget "NuGet.Core" 2 8 2 "" ]
      Meta =
        { Authors = [ "Jeremy Hughes" ]
          Description = "A small build system." } }

let test =
  fsProj
    { Name = "Steel.Finabs.Test"
      Version = semVer 0 1 0 ""
      Dir = "test"
      Sources =
        [ "Arbitraries.fs" ]
      Deps =
        [ Dep.name "mscorlib"
          Dep.name "FSharp.Core"
          Dep.path "../src/build/Steel.Finabs.dll"
          Dep.nuget "FSharpx.Core" 1 8 41 ""
          Dep.name "System"
          Dep.nuget "FsCheck" 1 0 0 "" ]
      Meta =
        { Authors = [ "Jeremy Hughes" ]
          Description = "Tests" } }

run defaultNugetRepo [ lib; test ] fsi.CommandLineArgs
