(** Provide a generic functor to build a specific printer. *)

open ExtendedAst

(** Alias for the {!Helper} signature. *)
module type HELPER = module type of Helper

(** Alias for the {!Printer} signature. *)
module type PRINTER = module type of Printer

(** The functor to build a printer of signature {!Printer}.

    The printing schema is fixed by the implementation of the functor,
    but is defined modularly with the functions of the [H] parameter. *)
module Make (H : HELPER) : PRINTER = struct

  (** Print a {!val:Helper.space} space. *)
  let print_space () = H.print_string (H.space ())

  (** [print_sep_encl print sep op cl xs] prints the elements of [xs] with
      the printer [print], separated by [sep] end enclosed by [op] and [cl] *)
  let print_sep_encl print sep op cl =
    let rec aux = function
      | [] -> ()
      | [x] -> print x
      | x :: xs ->
        print x; H.print_string sep; aux xs
    in
    function
    | [] -> ()
    | xs ->
      H.print_string op;
      aux xs;
      H.print_string cl

  (** [print_sep print sep xs] prints the elements of [xs] with
      the printer [print], separated by [sep].
  *)
  let print_sep print sep =
    print_sep_encl print sep "" ""

  (** Print a production by first calling {!val:Helper.production_begin}
      then printing the actuals and
      finally calling {!val:Helper.production_end}. *)
  let rec print_production symbols actuals =
    H.production_begin ();
    print_actuals symbols actuals;
    H.production_end ()

  (** Print a list of actuals.
      If the list is [nil], then the empty word {!val:Helper.eps} is printed. *)
  and print_actuals symbols = function
    | [] -> H.print_string (H.eps ()); print_space ()
    | xs -> print_sep (print_actual symbols false) (H.space ()) xs

  (** Print a possibly parenthesized actual. *)
  and print_actual symbols e = function
    | Symbol (x, ps) ->
      print_symbol symbols e x ps
    | Pattern p ->
      print_pattern symbols e p
    | Modifier (a, m) ->
      print_modifier m e (fun () -> print_actual symbols true a)
    | Anonymous ps ->
      print_sep (print_actuals symbols) (H.bar ()) ps

  (** Print a possibly parenthesized "modified" actual.
      Modular: see {!val:Helper.opt}, {!val:Helper.plus}
      and {!val:Helper.star}. *)
  and print_modifier = function
    | Opt -> H.opt
    | Plus -> H.plus
    | Star -> H.star

  (** Print a possibly parenthesized pattern.
      - [option(x)] is printed as [x] with the optional modifier ([[x]])
      - [pair(x, y)] is printed as [x y]
      - [separated_pair(x, sep, y)] is printed as [x sep y]
      - [preceded(opening, x)] is printed as [opening x]
      - [terminated(x, closing)] is printed as [x closing]
      - [delimited(opening, x, closing)] is printed as [opening x closing]
      - [list(x)] is printed as [x] with the list modifier ([x*])
      - [nonempty_list(x)] is printed as [x] with the non empty list modifier
        ([x+])
      - [separated_list(sep, x)] and [separated_nonempty_list(sep, x)] are
        modularly printed, see {!val:Helper.print_sep_list} *)
  and print_pattern symbols e =
    let print' = print_actual symbols false in
    let print'' x () = print' x in
    function
    | Option x ->
      H.opt e (print'' x)
    | Pair (x, y) ->
      H.par e (fun () -> print' x; print_space (); print' y)
    | SepPair (x, sep, y) ->
      H.par e (fun () ->
          print' x; print_space (); print' sep; print_space (); print' y)
    | Preceded (o, x) ->
      H.par e (fun () -> print' o; print_space (); print' x)
    | Terminated (x, c) ->
      H.par e (fun () -> print' x; print_space (); print' c)
    | Delimited (o, x, c) ->
      H.par e (fun () ->
          print' o; print_space (); print' x; print_space (); print' c)
    | List x ->
      H.star e (print'' x)
    | NEList x ->
      H.plus e (print'' x)
    | SepList (sep, x) ->
      H.print_sep_list e false (print'' sep) (print'' x)
    | SepNEList (sep, x) ->
      H.print_sep_list e true (print'' sep) (print'' x)

  (** [print_symbols symbols e s xs] modularly prints the symbol [s] and
      its parameters [xs]. The output is parenthesized if [e] is [true] AND
      the symbol is functional (ie. [xs <> nil]).
      See {!val:Helper.print_symbol}. *)
  and print_symbol symbols e s xs =
    H.par (e && xs <> []) (fun () ->
        H.print_symbol symbols s
          (fun () ->
             print_sep_encl (print_actual symbols e)
               ("," ^ (H.space ())) "(" ")" xs))

  (** Print a rule:
      + calls {!val:Helper.rule_begin}
      + calls {!val:Helper.print_rule_name} to print the left-hand side
      + prints the parameters
      + prints the definition symbol {!val:Helper.def}
      + prints the productions, separated by a line break {!val:Helper.break}
        and a bar {!val:Helper.prod_bar}
      + calls {!val:Helper.rule_end}
  *)
  let print_rule symbols {name; params; prods} =
    H.rule_begin ();
    let print_params =
      if params <> []
      then Some (fun () -> print_sep_encl H.print_string ", " "(" ")" params)
      else None
    in
    H.print_rule_name name print_params;
    H.print_string (H.def ());
    print_sep (print_production symbols) (H.break () ^ H.prod_bar ()) prods;
    H.rule_end ()

  (** Print the grammar by first calling {!val:Helper.print_header},
      then printing the rules and finally calling {!val:Helper.print_footer}. *)
  let print_spec o symbols s =
    H.p := o;
    H.print_header symbols;
    H.print_string "@[<v 0>";
    List.iter (print_rule symbols) s;
    H.print_string "@]";
    H.print_footer ()

end
