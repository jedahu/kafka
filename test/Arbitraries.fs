namespace Faker.Test

open FSharpx
open FSharpx.Collections
open FsCheck
open Faker.Control.Task

type Arbitraries =
  static member Task() : Arbitrary<Task<'r, 's, 'w, 'e, 'a>> =
    { new Arbitrary<Task<'r, 's, 'w, 'e, 'a>>() with
      override this.Generator =
        Gen.map
          (fun f -> Task(f))
          Arb.generate<'r -> 's -> 's * 'w * Choice<'a, 'e>> }

  static member DList() : Arbitrary<DList<'a>> =
    Arb.Default.Array()
      |> Arb.convert (DList.ofSeq << Seq.ofArray) (Array.ofSeq << DList.toSeq)