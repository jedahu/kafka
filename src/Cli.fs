namespace Faux

open Types
open Targets
open TransitiveTargets

module Cli =

  let filterTargets : MultiTarget -> string list -> MultiTarget =
    fun mt ps ->
      let pset = Set.ofList ps
      filterTarget (fun t -> Set.contains t.Dir pset) mt

  let selectTarget : NugetRepo -> string -> string list -> MultiTarget =
    fun repo target args ->
      let mt =
        match target with
          | "clean" -> runClean
          | "deps" -> runDeps repo
          | "build" -> runBuild repo
          | "rebuild" -> runRebuild repo
          | "pack" -> runPack repo
          | "writeProj" -> runWriteProj
          | "writeNuspec" -> runWriteNuspec
          | other -> failwith (sprintf "no such target: %s" other)
      filterTargets mt args

  let runTarget : MsBuildProj list -> NugetRepo -> string list -> bool =
    fun ps repo args ->
      let (target, args') =
        match args with
          | [] -> ("build", [])
          | h :: t -> (h, t)
      selectTarget repo target args' ps

  let defaultNugetRepo : NugetRepo =
    HttpRepo("https://packages.nuget.org/api/v2")

  let run : NugetRepo -> MsBuildProj list -> string seq -> int =
    fun repo ps args ->
      if runTarget ps repo (List.tail (List.ofSeq args)) then 0 else 1