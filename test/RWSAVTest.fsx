#I "build"
#I "../src/build"
#r "FSharpx.Core.dll"
#r "FsCheck.dll"
#r "Steel.Finabs.dll"
#r "Steel.Finabs.Test.dll"

open FSharpx.Collections
open FsCheck
open Faker.Control
open Faker.Control.RWSAV

Arb.register<Faker.Test.Arbitraries>() |> ignore

let private config =
  if Array.exists (fun x -> x = "-v") fsi.CommandLineArgs then
    Config.Verbose
  else
    Config.Quick

let private check name prop =
  Check.One (name, config, prop)

let private eq (t1 : RWSAV<'r, 'w, 's, 'e, 'a>) (t2 : RWSAV<'r, 'w, 's, 'e, 'a>) r s =
  let (w1, s1, a1) = run t1 r s
  let (w2, s2, a2) = run t2 r s
  let a1' = Async.RunSynchronously (a1)
  let a2' = Async.RunSynchronously (a2)
  let x = (w1, s1, a1')
  let y = (w2, s2, a2')
  x = y |@ sprintf "%A = %A" x y

let private eq' m1 m2 a =
  eq (m1 a) (m2 a)

let unit a = RWSAV.unit' DList.monoid<int> a
let map f m = RWSAV.map' DList.monoid<int> f m
let ap m f = RWSAV.ap' DList.monoid<int> DList.monoid<int> m f
let inline (<!>) f m = map f m
let inline (<*>) f m = ap m f

check "functor identity" <|
  fun m -> eq (id <!> m) (id m)

check "functor distributivity" <|
  fun m f g -> eq ((f << g) <!> m) (f <!> (g <!> m))

check "applicative identity" <|
  fun f -> eq (unit id <*> f) f

check "applicative composition" <|
  // fun f g h -> eq (unit (<<) |> ap f |> ap g |> ap h) (f |> ap (g |> ap h))
  fun f g h -> eq (unit (<<) <*> f <*> g <*> h) (f <*> (g <*> h))

check "applicative homomorphism" <|
  fun f x -> eq (unit f <*> unit x) (unit (f x))

check "applicative interchange" <|
  fun f x -> eq (f <*> unit x) (unit (fun g -> g x) <*> f)
