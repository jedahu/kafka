namespace Faux

open Types
open Targets

module TransitiveTargets =

  [<ReferenceEquality>]
  type TransTarget =
    { Target : MultiTarget
      TargetDeps : TransTarget list }

  let rec transTargetList : TransTarget -> TransTarget list =
    fun tt -> tt :: List.collect transTargetList tt.TargetDeps

  let topoSort : TransTarget list -> TransTarget list =
    fun ts ->
      let input = List.toArray ts
      let visited = Array.create (input.Length) false
      let result = new ResizeArray<TransTarget>()
      let rec visit i =
        if not visited.[i] then
          visited.[i] <- true
          result.Add input.[i]
          for j in 0 .. input.Length - 1 do
            if List.exists (fun x -> x = input.[j]) input.[i].TargetDeps then
              visit j
      for j in 0 .. input.Length - 1 do
        visit j
      List.ofSeq result

  let transitive : MultiTarget -> TransTarget list -> TransTarget =
    fun target deps -> { Target = target; TargetDeps = deps }

  let runTransTarget : TransTarget -> MultiTarget =
    fun tt ps ->
      tt
      |> transTargetList
      |> topoSort
      |> List.rev
      |> (not << Seq.exists not << Seq.map (fun t -> t.Target ps))

  let clean : TransTarget = transitive cleanTarget []

  let deps : NugetRepo -> TransTarget =
    fun repo -> transitive (depsTarget repo) []

  let build : NugetRepo -> TransTarget =
    fun repo -> transitive buildTarget [deps repo]

  let rebuild : NugetRepo -> TransTarget =
    fun repo -> transitive rebuildTarget [deps repo]

  let pack : NugetRepo -> TransTarget =
    fun repo -> transitive packTarget [build repo]

  let writeProj : TransTarget = transitive writeProjTarget []

  let writeNuspec : TransTarget = transitive writeNuspecTarget []


  let runClean : MultiTarget = runTransTarget clean
  let runDeps : NugetRepo -> MultiTarget = runTransTarget << deps
  let runBuild : NugetRepo -> MultiTarget = runTransTarget << build
  let runRebuild : NugetRepo -> MultiTarget = runTransTarget << rebuild
  let runPack : NugetRepo -> MultiTarget = runTransTarget << pack
  let runWriteProj : MultiTarget = runTransTarget writeProj
  let runWriteNuspec : MultiTarget = runTransTarget writeNuspec
