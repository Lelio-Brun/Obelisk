SRC=src
MISC=misc
PARSER=$(SRC)/parser.mly
RECO=$(MISC)/reco.mly
IMAGES=tabular simplebnf syntax backnaur
PREFIX=my
MAIN=main
EXE=dune exec $(SRC)/main.exe --

.PHONY: publish all latex htmlcss html default ebnf reco readme tests clean

all:
	@dune build

publish:
	@dune-release tag
	@dune-release distrib
	@dune-release publish
	@dune-release opam pkg
	@dune-release opam submit

%.tex:
	@$(EXE) latex -prefix $(PREFIX) -$* $(PARSER) -o $@

%.pdf: %.tex
	pdflatex -interaction batchmode $<

%.png: %.pdf
	@magick convert -quiet -density 150 $< -format png $(MISC)/$@
	@rm -f $*.tex $< $*.aux $*.log

latex: $(IMAGES:%=%.png)

htmlcss:
	@$(EXE) html $(PARSER) -o test.html
	@wkhtmltoimage -f png --width 800 test.html $(MISC)/htmlcss.png
	@rm -f test.html

html:
	@$(EXE) html -nocss $(PARSER) -o test.html
	@wkhtmltoimage -f png --width 800 test.html $(MISC)/html.png
	@rm -f test.html

default:
	@printf "\nDefault output on $(PARSER):\n"
	@$(EXE) $(PARSER)

ebnf:
	@printf "\nEBNF output on $(PARSER):\n"
	@$(EXE) ebnf $(PARSER)

reco:
	@printf "Default output on $(RECO):\n"
	@$(EXE) $(RECO)
	@printf "Default output on $(RECO) with '-i' switch:\n"
	@$(EXE) -i $(RECO)
	@printf "EBNF output on $(RECO):\n"
	@$(EXE) ebnf $(RECO)
	@printf "EBNF output on $(RECO) with '-i' switch:\n"
	@$(EXE) ebnf -i $(RECO)

readme: latex htmlcss html default ebnf reco

tests:
	@dune test

clean:
	@dune clean
