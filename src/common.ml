module type SYMBOLS = sig
  type t

  val empty: t

  val def_term: string -> t -> t
  val def_non_term: string -> t -> t
  val def_fun: string -> string list -> t -> t

  val terminals: t -> string list
  val non_terminals: t -> string list
  val defined: t -> string list

  val is_term: string -> t -> bool
  val is_non_term: string -> t -> bool
  val is_defined: string -> t -> string list option

end

module Symbols : SYMBOLS = struct

  module M = Map.Make(String)
  type t = symbols M.t
  and symbols =
    | Terminal
    | NonTerminal
    | Fun of string list

  let empty = M.empty

  let def_term, def_non_term =
    let add a x = M.add x a in
    add Terminal, add NonTerminal
  let def_fun x xs = M.add x (Fun xs)

  let terminals m =
    fst List.(split (filter (function _, Terminal -> true | _ -> false)
        (M.bindings m)))
 let non_terminals m =
    fst List.(split (filter (function _, NonTerminal -> true | _ -> false)
        (M.bindings m)))
 let defined m =
    fst List.(split (filter (function _, Terminal -> false | _ -> true)
        (M.bindings m)))

  let is_term x m =
    try begin match M.find x m with
      | Terminal -> true
      | _ -> false
    end
    with Not_found -> false

  let is_non_term x m =
    try begin match M.find x m with
      | NonTerminal -> true
      | _ -> false
    end
    with Not_found -> false

  let is_defined x m =
    try begin match M.find x m with
      | NonTerminal -> Some []
      | Fun xs -> Some xs
      | _ -> None
    end
    with Not_found -> None

end
