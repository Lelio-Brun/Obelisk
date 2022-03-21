(** Some common facilities.  *)

(** Find a rule by its left-hand side (name) in the grammar. *)
let find_rule r rules =
  let open ExtendedAst in
  List.find_opt (fun { name; _ } -> name = r) rules

(** The signature for the set of symbols appearing in a grammar. *)
module type SYMBOLS = sig
  (** The set of symbols. *)
  type t

  (** The empty set.  *)
  val empty: t

  (** Add a terminal to the symbols. *)
  val def_term: string -> t -> t

  (** Add a non terminal to the symbols *)
  val def_non_term: string -> t -> t

  (** Add a functional non terminal along with
      its parameters to the symbols.  *)
  val def_fun: string -> string list -> t -> t

  (** Get the list of terminals.  *)
  val terminals: t -> string list

  (** Get the list of non terminals.  *)
  val non_terminals: t -> string list

  (** Get the list of functional non terminals.  *)
  val functionals: t -> string list

  (** Get the list of "defined" symbols, that is both non terminals and
      functional non terminals. *)
  val defined: t -> string list

  (** Test if the given symbol is a terminal. *)
  val is_term: string -> t -> bool

  (** Test if the given symbol is a non terminal.  *)
  val is_non_term: string -> t -> bool

  (** Test if the given symbol is "defined" and returns its parameters.
      See {!defined}. *)
  val is_defined: string -> t -> string list option

end

(** The actual implementation for the set of symbols, see {!SYMBOLS}.  *)
module Symbols : SYMBOLS = struct

  (** The set of symbols is implemented by a map
      from identifiers to {!symbols} *)

  module M = Map.Make(String)
  type t = symbol M.t
  and symbol =
    | Terminal
    | NonTerminal
    | Fun of string list

  (** See {!SYMBOLS.empty}.  *)
  let empty = M.empty

  (** An internal helper to add a symbol. *)
  let add a x = M.add x a

  (** See {!SYMBOLS.def_term}. *)
  let def_term = add Terminal

  (** See {!SYMBOLS.def_non_term}. *)
  let def_non_term = add NonTerminal

  (** See {!SYMBOLS.def_fun}. *)
  let def_fun x xs = add (Fun xs) x

  (** See {!SYMBOLS.terminals}. *)
  let terminals m =
    fst List.(split (filter (function _, Terminal -> true | _ -> false)
        (M.bindings m)))

  (** See {!SYMBOLS.non_terminals}. *)
  let non_terminals m =
    fst List.(split (filter (function _, NonTerminal -> true | _ -> false)
        (M.bindings m)))

 (** See {!SYMBOLS.functionals}. *)
  let functionals m =
    fst List.(split (filter (function _, Fun _ -> true | _ -> false)
        (M.bindings m)))

  (** See {!SYMBOLS.defined}. *)
  let defined m =
    fst List.(split (filter (function _, Terminal -> false | _ -> true)
        (M.bindings m)))

  (** See {!SYMBOLS.is_term}. *)
  let is_term x m =
    try begin match M.find x m with
      | Terminal -> true
      | _ -> false
    end
    with Not_found -> false

  (** See {!SYMBOLS.is_non_term}. *)
  let is_non_term x m =
    try begin match M.find x m with
      | NonTerminal -> true
      | _ -> false
    end
    with Not_found -> false

  (** See {!SYMBOLS.is_defined}. *)
  let is_defined x m =
    try begin match M.find x m with
      | NonTerminal -> Some []
      | Fun xs -> Some xs
      | _ -> None
    end
    with Not_found -> None

end
