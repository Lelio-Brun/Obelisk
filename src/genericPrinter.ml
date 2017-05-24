open ExtendedAst
open Common
open Format

module type HELPER = module type of Helper
module type PRINTER = module type of Printer

module Make (H : HELPER) : PRINTER = struct

  let print_space () = H.print_string H.space

  let print_sep_encl_gen print sep op cl =
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
  let print_sep_encl print =
    print_sep_encl_gen print
  let print_sep print sep =
    print_sep_encl print sep "" ""

  let rec print_production not_sing symbols actuals =
    H.production_begin not_sing;
    print_actuals symbols actuals;
    H.production_end not_sing

  and print_actuals symbols = function
    | [] -> H.print_string H.eps; print_space ()
    | xs -> print_sep (print_actual symbols false) H.space xs

  and print_actual symbols e = function
    | Symbol (x, ps) ->
      print_symbol symbols e x ps
    | Pattern p ->
      print_pattern symbols e p
    | Modifier (a, m) ->
      print_modifier m e (fun () -> print_actual symbols true a)
    | Anonymous ps ->
      print_sep (print_actuals symbols) H.bar ps

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

  and print_symbol symbols e x ps =
    H.par (e && ps <> []) (fun () ->
        H.print_symbol
          (Symbols.is_term x symbols)
          (Symbols.is_non_term x symbols) x
          (fun () ->
             print_sep_encl (print_actual symbols e)
               ("," ^ H.space) "(" ")" ps))

  let print_rule symbols {name; params; prods} =
    H.rule_begin ();
    H.print_rule_name (params = []) name;
    print_sep_encl H.print_string ", " "(" ")" params;
    H.print_string H.def;
    let not_sing = (List.length prods > 1) in
    print_sep (print_production not_sing symbols)
      (H.break ^ H.prod_bar) prods;
    H.rule_end ()

  let print_spec o symbols s =
    H.p := o;
    H.print_header symbols;
    H.print_string "@[<v 0>";
    List.iter (print_rule symbols) s;
    H.print_string "@]";
    H.print_footer ()

end
