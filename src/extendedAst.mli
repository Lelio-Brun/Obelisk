type spec = rule list

and rule = {
  name: string;
  params: string list;
  prods: production list
}

and production = actual list

and actual =
  | Symbol of string * actual list
  | Pattern of pattern
  | Modifier of actual * modifier
  | Anonymous of production list

and modifier =
  | Opt
  | Plus
  | Star

and pattern =
  | Option of actual
  | Pair of actual * actual | SepPair of actual * actual * actual
  | Preceded of actual * actual | Terminated of actual * actual
  | Delimited of actual * actual * actual
  | List of actual | NEList of actual
  | SepList of actual * actual | SepNEList of actual * actual
