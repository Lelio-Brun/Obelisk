open GenericPrinter
open Default

module Default = Make(Default)

module LatexTabular = Make(LatexTabular)
module LatexSyntax = Make(LatexSyntax)
module LatexBacknaur = Make(LatexBacknaur)

module Html = Make (Html)
