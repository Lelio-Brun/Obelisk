type spec = rule list

and rule = {
  name: string;
  params: string list;
  groups: group list
}

and group = production list

and production = producer list

and producer = {
  bind: string option;
  actual: actual
}

and actual =
  | Symbol of string * actual list
  | Modifier of actual * modifier
  | Anonymous of group list

and modifier =
  | Opt
  | Plus
  | Star
