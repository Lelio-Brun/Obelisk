SRC=src
MISC=misc
PARSER=$(SRC)/parser.mly
RECO=$(MISC)/reco.mly
IMAGES=tabular syntax backnaur
PREFIX=my
MAIN=main
EXE=dune exec $(SRC)/main.exe --

.PHONY: all latex html default reco readme doc tests clean cleandoc install uninstall publish travis

all:
	@dune build

%.tex:
	@$(EXE) latex -prefix $(PREFIX) -$* $(PARSER) -o $@

%.pdf: %.tex
	@pdflatex -interaction batchmode $<

%.png: %.pdf
	@convert -quiet -density 150 $< -format png $(MISC)/$@
	@rm -f $*.tex $< $*.aux $*.log

latex: $(IMAGES:%=%.png)

html:
	@$(EXE) html $(PARSER) -o test.html
	@wkhtmltoimage -f png --width 800 test.html $(MISC)/html.png
	@rm -f test.html

default:
	@printf "\nDefault output on $(PARSER):\n"
	@$(EXE) $(PARSER)

reco:
	@printf "Default output on $(RECO):\n"
	@$(EXE) $(RECO)
	@printf "Default output on $(RECO) with '-i' switch:\n"
	@$(EXE) -i $(RECO)

readme: latex html default reco

tests:
	@dune test

clean:
	@dune clean
