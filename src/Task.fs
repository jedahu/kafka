#nowarn "62"

namespace Faker

open FSharpx
open FSharpx.Operators
open FSharpx.Validation
open FSharpx.Choice
open FSharpx.Collections

module Control =

  module Task =

    type Validation<'e>() =
      inherit CustomValidation<'e DList>(DList.monoid<'e>)

    type Task<'r, 's, 'w, 'e, 'a> =
      Task of ('r -> 's -> 's * 'w * ('a, 'e) Choice)

    let returnM (o : Monoid<_>) (a : 'a) : Task<'r, 's, 'w, 'e, 'a> =
      Task (fun r s -> (s, o.Zero(), Choice1Of2(a)))

    let inline map
      (f : 'a -> 'b) (Task k : Task<'r, 's, 'w, 'e, 'a>) :
      Task<'r, 's, 'w, 'e, 'b> =
        Task (fun r s ->
          let (s1, w1, x1) = k r s
          (s1, w1, f <!> x1))

    let bind
      (o : Monoid<_>)
      (f : 'a -> Task<'r, 's, 'w, 'e, 'b>)
      (Task k : Task<'r, 's, 'w, 'e, 'a>) :
      Task<'r, 's, 'w, 'e, 'b> =
        Task (fun r s ->
          let (s1, w1, x1) = k r s
          choice
            (fun a ->
              let (Task k2) = f a
              let (s2, w2, x2) = k2 r s1
              (s2, o.Combine(w1, w2), x2))
            (fun e -> (s1, w1, Choice2Of2(e)))
            x1)

    type TaskBuilder<'w>(o : Monoid<'w>) =

      member this.Return(a) = returnM o a

      member this.Bind(m, f) = bind o f m

    let task = TaskBuilder(DList.monoid<string>)

    let inline unit a = Operators.returnM task a

    let inline (<!>) f m = map f m

    let inline (>>=) m f = Operators.bindM task m f

    let inline (>=>) f g = fun x -> f x >>= g

    let inline (<=<) x = flip (>=>) x

    let inline ap m f = Operators.applyM task task f m

    let inline (<*>) f m = ap m f

    let inline ( *>) x y = (fun _ z -> z) <!> x <*> y

    let inline (<* ) x y = (fun z _ -> z) <!> x <*> y

    let run : Task<'r, 's, 'w, 'e, 'a> -> 'r -> 's -> 's * 'w * Choice<'a, 'e> =
      fun (Task k) -> k

    let exec : Task<'r, 's, 'w, 'e, 'a> -> 'r -> 's -> Choice<'a, 'e> =
      fun (Task k) r s ->
        let (_, _, x) = k r s
        x