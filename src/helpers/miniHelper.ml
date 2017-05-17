open Format

let p = ref std_formatter
let print_string s = fprintf !p (Scanf.format_from_string s "")
let print_fmt s = fprintf !p s
