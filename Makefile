INFOS=infos.env

include $(INFOS)

SRC=src
DOC=docs
MISC=misc
TARGET=native
MAIN=main.$(TARGET)
CC=ocamlbuild
FLAGS=-use-menhir -use-ocamlfind -pkgs str -Is $(SRC),$(SRC)/helpers
PARSER=$(SRC)/parser.mly
RECO=$(MISC)/reco.mly
IMAGES=tabular syntax backnaur
PREFIX=my

.PHONY: all latex html default reco readme doc tests clean cleandoc install uninstall publish travis

all: opam
	@$(CC) $(FLAGS) $(SRC)/$(MAIN)
	@mv $(MAIN) $(EXE)

%.tex:
	@./$(EXE) latex -prefix $(PREFIX) -$* $(PARSER) -o $@

%.pdf: %.tex
	@pdflatex -interaction batchmode $<

%.png: %.pdf
	@convert -quiet -density 150 $< -format png $(MISC)/$@
	@rm -f $*.tex $< $*.aux $*.log

latex: all $(IMAGES:%=%.png)

html: all
	@./$(EXE) html $(PARSER) -o test.html
	@wkhtmltoimage -f png --width 800 test.html $(MISC)/html.png
	@rm -f test.html

default:
	@printf "\nDefault output on $(PARSER):\n"
	@./$(EXE) $(PARSER)

reco:
	@printf "Default output on $(RECO):\n"
	@./$(EXE) $(RECO)
	@printf "Default output on $(RECO) with '-i' switch:\n"
	@./$(EXE) -i $(RECO)

readme: latex html default reco

doc: cleandoc $(DOC)/$(EXE).odocl $(DOC)/doc.css
	@$(CC) $(FLAGS) $(DOC)/$(EXE).docdir/index.html
	@cp $(EXE).docdir/*.html $(DOC)
	@rm -f $(EXE).docdir

opam: Makefile opam.in
	@sed -e "s|%%VERSION%%|$(VERSION)|"\
       -e "s|%%AUTHOR%%|$(AUTHOR)|"\
       -e "s|%%MAIL%%|$(MAIL)|"\
       -e "s|%%URL%%|$(URL)|"\
       -e "s|%%NAME%%|$(EXE)|" < $@.in > $@

travis: Makefile travis.in
	@sed -e "s|%%NAME%%|$(EXE)|" < $@.in > .$@.yml

Makefile: $(INFOS)

tests: all
	@$(MAKE) -C $@

cleandoc:
	@rm -rf $(DOCS)/*.html

clean: cleandoc
	@$(CC) -clean
	@$(MAKE) -C tests $@

install: all
	@echo $(BINDIR)
	@mkdir -p $(BINDIR)
	@install $(EXE) $(BINDIR)/$(EXE)

uninstall:
	@rm -f $(BINDIR)/$(EXE)

prepare:
	@opam-publish prepare $(URL)/archive/v$(VERSION).tar.gz
	@printf "url file:\n"
	@cat $(PACKAGE)/url
	@printf "descr file:\n"
	@cat $(PACKAGE)/descr

publish: prepare
	@opam-publish submit $(PACKAGE)
