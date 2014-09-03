namespace Faux

open NuGet
open Types

module Versions =

  let nugetSemVer : Version -> SemanticVersion =
    fun v ->
      match v with
        | SemVer(sv) ->
          new SemanticVersion(
            int sv.Major,
            int sv.Minor,
            int sv.Build,
            sv.Special)
        | FreeVer(fv) ->
          new SemanticVersion(fv)
