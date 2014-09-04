#I "build"
#I "../src/build"
#r "FSharpx.Core.dll"
#r "FsCheck.dll"
#r "Steel.Finabs.dll"
#r "Steel.Finabs.Test.dll"

open FSharpx.Collections
open FsCheck
open Faker.Control
open Faker.Control.Task

Arb.register<Faker.Test.Arbitraries>() |> ignore

let private config =
  if Array.exists (fun x -> x = "-v") fsi.CommandLineArgs then
    Config.Verbose
  else
    Config.Quick

let private check name prop =
  Check.One (name, config, prop)

let private eq t1 t2 r s =
  let x = run t1 r s
  let y = run t2 r s
  x = y |@ sprintf "%A = %A" x y

let private eq' m1 m2 a =
  eq (m1 a) (m2 a)

check "functor identity" <|
  fun m -> eq (id <!> m) (id m)

check "functor distributivity" <|
  fun m f g -> eq ((f << g) <!> m) (f <!> (g <!> m))

check "applicative identity" <|
  fun f -> eq (unit id <*> f) f

check "applicative composition" <|
  fun f g h -> eq (unit (<<) <*> f <*> g <*> h) (f <*> (g <*> h))

check "applicative homomorphism" <|
  fun f x -> eq (unit f <*> unit x) (unit (f x))

check "applicative interchange" <|
  fun f x -> eq (f <*> unit x) (unit (fun g -> g x) <*> f)

check "monad left identity" <|
  fun f -> eq' (unit >=> f) f

check "monad right identity" <|
  fun f -> eq' (f >=> unit) f

check "monad associativity" <|
  fun f g h -> eq' ((f >=> g) >=> h) (f >=> (g >=> h))
