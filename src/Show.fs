namespace Faux

module Show =
  let inline show
    (a : ^a when ^a : (static member show : ^a -> string)) : string =
    (^a : (static member show : ^a -> string) (a))
