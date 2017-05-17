open ExtendedAst
open Format

module type HELPER = module type of Helper
module type PRINTER = module type of Printer

module Make (H : HELPER) : PRINTER = struct

  module OrdString = struct
    type t = string
    let compare = Pervasives.compare
  end
  module StringSet = Set.Make(OrdString)

  let add_non_terminal nts {name; params} =
    if params = [] then StringSet.add name nts else nts

  let add_terminal nts ts {prods} =
    let rec add_terminal_actual ts = function
      | Symbol (s, ps) ->
        let ts = List.fold_left add_terminal_actual ts ps in
        if not (StringSet.mem s nts) && String.uppercase_ascii s = s
           && ps = []
        then StringSet.add s ts else ts
      | Pattern p ->
        add_terminal_pattern ts p
      | Modifier (a, _) ->
        add_terminal_actual ts a
      | Anonymous ps ->
        List.fold_left add_terminal_prod ts ps
    and add_terminal_prod ts actuals =
      List.fold_left add_terminal_actual ts actuals
    and add_terminal_pattern ts = function
      | Option x | List x | NEList x ->
        add_terminal_actual ts x
      | Pair (x, y) | Preceded (x, y) | Terminated (x, y)
      | SepList (x, y) | SepNEList (x, y) ->
        add_terminal_actual (add_terminal_actual ts x) y
      | SepPair (x, y, z) | Delimited (x, y, z) ->
        add_terminal_actual (add_terminal_actual (add_terminal_actual ts x) y) z
    in
    List.fold_left add_terminal_prod ts prods

  let scan s =
    let nts = List.fold_left add_non_terminal StringSet.empty s in
    let ts = List.fold_left (add_terminal nts) StringSet.empty s in
    ts, nts

  let print_space () = H.print_string H.space

  let print_sep_encl_gen ini_sep print sep op cl =
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
      if ini_sep then H.print_string sep; aux xs;
      H.print_string cl
  let print_sep_encl not_sing print =
    print_sep_encl_gen not_sing print
  let print_sep not_sing print sep =
    print_sep_encl not_sing print sep "" ""

  let rec print_production not_sing symbols actuals =
    H.production_begin not_sing;
    print_actuals symbols actuals;
    H.production_end not_sing

  and print_actuals symbols = function
    | [] -> H.print_string H.eps; print_space ()
    | xs -> print_sep false (print_actual symbols false) H.space xs

  and print_actual symbols e = function
    | Symbol (x, ps) ->
      print_symbol symbols e x ps
    | Pattern p ->
      print_pattern symbols e p
    | Modifier (a, m) ->
      print_modifier m e (fun () -> print_actual symbols true a)
    | Anonymous ps ->
      print_sep false (print_actuals symbols) H.bar ps

  and print_modifier = function
    | Opt -> H.opt
    | Plus -> H.plus
    | Star -> H.star

  and print_pattern symbols e =
    let print' = print_actual symbols e in
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

  and print_symbol (ts, nts as symbols) e x ps =
    H.par (e && ps <> []) (fun () ->
        H.print_terminal (StringSet.mem x ts) (StringSet.mem x nts) x;
        print_sep_encl false (print_actual symbols e) ("," ^ H.space) "(" ")" ps)

  let print_rule symbols {name; params; prods} =
    H.rule_begin ();
    H.print_rule_name (params = []) name;
    print_sep_encl false H.print_string ", " "(" ")" params;
    H.print_string H.def;
    let not_sing = (List.length prods > 1) in
    print_sep not_sing (print_production not_sing symbols) (H.break ^ H.bar) prods;
    H.rule_end ()

  let print_spec o s =
    let ts, nts = scan s in
    H.p := o;
    H.print_header (StringSet.elements ts);
    H.print_string "@[<v 0>";
    List.iter (print_rule (ts, nts)) s;
    H.print_string "@]";
    H.print_footer ()

end
