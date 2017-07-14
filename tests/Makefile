EXE=obelisk
MLYS=$(wildcard *.mly)
TMP=tmp
PKG=pkg
TMPPKG=tmppkg
MAIN=main

exec=../$(EXE) $(1) -o $(TMP) $(2) && printf "\tTesting $(2)\tok\n"
exec_pkg=../$(EXE) $(1) -o $(TMPPKG).tex -package $(PKG) -prefix my_prefix42 $(2) && printf "\tTesting $(2) (package mode)\tok\n"
test=$(foreach f, $(MLYS), $(call exec,$(1),$(f));)
test_latex=$(foreach f, $(MLYS), $(call exec,$(1),$(f));\
	pdflatex -interaction batchmode $(TMP))
test_latex_pkg=$(foreach f, $(MLYS), $(call exec_pkg,$(1),$(f));\
	pdflatex -interaction batchmode $(MAIN))

.PHONY: all clean latex tabular syntax backnaur html default

all: default latex html

default:
	@printf "Default mode."
	@$(call test,,)

latex: $(MAIN) tabular syntax backnaur

$(MAIN): Makefile
	@printf "\documentclass[preview]{standalone}\n\n\
\\\\usepackage{$(PKG)}\n\n\
\\\\begin{document}\n\
\include{$(TMPPKG)}\n\
\\\\end{document}\n" > $@

tabular:
	@printf "\nLatex tabular mode.\n"
	@$(call test_latex,latex -$@)
	@$(call test_latex_pkg,latex -$@)

syntax:
	@printf "\nLatex syntax mode.\n"
	@$(call test_latex,latex -$@)
	@$(call test_latex_pkg,latex -$@)

backnaur:
	@printf "Latex backnaur mode.\n"
	@$(call test_latex,latex -$@)
	@$(call test_latex_pkg,latex -$@)

html:
	@printf "\nHtml mode.\n"
	@$(call test,$@)

clean:
	@rm -rf $(TMP)* $(PKG)* $(MAIN)*
