#nowarn "62"

namespace Faker

open FSharpx
open FSharpx.Operators
open FSharpx.Reader
open FSharpx.Writer
open FSharpx.State
open FSharpx.Validation
open FSharpx.Choice
open FSharpx.Collections

module Control =

  module RWSAV =
    type RWSAV<'r, 'w, 's, 'e, 'a> =
      RWSAV of Reader<'r, Writer<'w, State<Async<Choice<'a, 'e>>, 's>>>

    let rwsav x = RWSAV(x)

    let unrwsav (RWSAV m) = m

    let private wmap (wb : WriterBuilder<_>) f m =
      Operators.liftM wb f m

    let private wap (wb : WriterBuilder<_>) m f =
      Operators.applyM wb wb f m

    let private wlift2 o f a b =
      let wb = WriterBuilder(o)
      Writer.returnM o f |> wap wb a |> wap wb b

    let liftAsync' o (m : Async<Choice<'a, 'e>>) : RWSAV<'r, 'w, 's, 'e, 'a> =
      rwsav
        (m |> State.returnM
           |> Writer.returnM o
           |> Reader.returnM)

    let liftAsync'' o (m : Async<'a>) : RWSAV<'r, 'w, 's, 'e, 'a> =
      liftAsync' o (Async.map Choice.returnM m)

    let unit' o a : RWSAV<'r, 'w, 's, 'e, 'a> =
      rwsav
        (a |> Choice.returnM
           |> Async.returnM
           |> State.returnM
           |> Writer.returnM o
           |> Reader.returnM)

    let map' o f (m : RWSAV<'r, 'w, 's, 'e, 'a>) : RWSAV<'r, 'w, 's, 'e, 'b> =
      let map =
        Reader.map
          << wmap (WriterBuilder(o))
          << State.map
          << Async.map
          << Choice.map
      rwsav (map f (unrwsav m))

    let ap' o g (m : RWSAV<'r, 'w, 's, 'e, 'a>) f : RWSAV<'r, 'w, 's, 'e, 'b> =
      let lift = Reader.lift2 << wlift2 o << State.lift2 << Async.lift2
      let vap = flip (Validation.apm g)
      rwsav (lift vap (unrwsav f) (unrwsav m))

    let ask' o : RWSAV<'r, 'w, 's, 'e, 'r> =
      rwsav
        (Writer.returnM o
          << State.returnM
          << Async.returnM
          << Choice.returnM)

    let tell w : RWSAV<'r, 'w, 's, 'e, unit> =
      rwsav
        (fun _ ->
          fun () ->
            (Choice.returnM () |> Async.returnM |> State.returnM, w))

    let listen (m : RWSAV<'r, 'w, 's, 'e, 'a>) : RWSAV<'r, 'w, 's, 'e, 'a * 'w> =
      rwsav
        (fun r ->
          fun () ->
            let (x, w) = ((unrwsav m) r)()
            let x' =
              x |> (State.map << Async.map << Choice.map) (fun a -> (a, w))
            (x', w))

    let get' (o : Monoid<'w>) : RWSAV<'r, 'w, 's, 'e, 's> =
      rwsav
        (fun _ ->
          fun () ->
            ( (fun s -> (s |> Choice.returnM |> Async.returnM, s)),
              o.Zero() ))

    let put' (o : Monoid<'w>) s : RWSAV<'r, 'w, 's, 'e, unit> =
      rwsav
        (fun _ ->
          fun () ->
            ( (fun _ -> (() |> Choice.returnM |> Async.returnM, s)),
              o.Zero() ))

    let run (m : RWSAV<'r, 'w, 's, 'e, 'a>) r s : 'w * 's * Async<Choice<'a, 'e>> =
      let (x, w) = ((unrwsav m) r)()
      let (y, s) = x s
      (w, s, y)

    let exec m r s : Async<Choice<'a, 'e>> =
      let (_, _, x) = run m r s in x


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

    let inline (<=<) y x = x >=> y

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
