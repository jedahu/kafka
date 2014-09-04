namespace Faker.Test

open FSharpx
open FSharpx.Reader
open FSharpx.Writer
open FSharpx.State
open FSharpx.Collections
open FsCheck
open Faker.Control.Task
open Faker.Control.RWSAV

type Arbitraries =
  static member Task() : Arbitrary<Task<'r, 's, 'w, 'e, 'a>> =
    { new Arbitrary<Task<'r, 's, 'w, 'e, 'a>>() with
      override this.Generator =
        Gen.map
          (fun f -> Task(f))
          Arb.generate<'r -> 's -> 's * 'w * Choice<'a, 'e>> }

  static member RWSAV() : Arbitrary<RWSAV<'r, 'w, 's, 'e, 'a>> =
    { new Arbitrary<RWSAV<'r, 'w, 's, 'e, 'a>>() with
      override this.Generator =
        Gen.map rwsav
          Arb.generate<
            Reader<'r, Writer<'w, State<Async<Choice<'a, 'e>>, 's>>>> }

  static member Async() : Arbitrary<Async<'a>> =
    { new Arbitrary<Async<'a>>() with
      override this.Generator =
        Gen.map Async.returnM Arb.generate<'a> }

  static member DList() : Arbitrary<DList<'a>> =
    Arb.Default.Array()
      |> Arb.convert (DList.ofSeq << Seq.ofArray) (Array.ofSeq << DList.toSeq)
