namespace Faux

open Types
open MsBuildProj

module Targets =

  type MultiTarget = MsBuildProj list -> bool

  let filterTarget : (MsBuildProj -> bool) -> MultiTarget -> MultiTarget =
    fun pred f ps ->
      let filtered = List.filter pred ps
      if List.isEmpty filtered then
        f ps
      else
        f filtered

  let multiTarget : (MsBuildProj -> bool) -> MultiTarget =
    fun f -> not << Seq.exists not << Seq.map f

  let msBuildTarget : string array -> MultiTarget = multiTarget << buildProj

  let buildTarget : MultiTarget = msBuildTarget [| "build" |]

  let cleanTarget : MultiTarget = msBuildTarget [| "clean" |]

  let rebuildTarget : MultiTarget = msBuildTarget [| "rebuild" |]

  let depsTarget : NugetRepo -> MultiTarget =
    fun repo -> multiTarget (fetchProjDeps repo.PackageRepository)

  let packTarget : MultiTarget = multiTarget packProj

  let writeProjTarget : MultiTarget = multiTarget writeProjFile

  let writeNuspecTarget : MultiTarget = multiTarget writeNuspecFile
