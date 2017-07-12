EXE=obelisk
MLYS=$(wildcard *.mly)
TMP=tmp
PKG=pkg
TMPPKG=tmppkg
MAIN=main

exec=../$(EXE) $(1) -o $(TMP) $(2) && echo -e "\tTesting $(2)\tok"
exec_pkg=../$(EXE) $(1) -o $(TMPPKG).tex -package $(PKG) -prefix my_prefix42 $(2) && echo -e "\tTesting $(2) (package mode)\tok"
test=$(foreach f, $(MLYS), $(call exec,$(1),$(f));)
test_latex=$(foreach f, $(MLYS), $(call exec,$(1),$(f));\
	pdflatex -interaction batchmode $(TMP))
test_latex_pkg=$(foreach f, $(MLYS), $(call exec_pkg,$(1),$(f));\
	pdflatex -interaction batchmode $(MAIN))

.PHONY: all clean latex tabular syntax backnaur html default

all: default latex html

default:
	@echo "Default mode."
	@$(call test,,)

latex: $(MAIN) tabular syntax backnaur

$(MAIN): Makefile
	@printf "\documentclass[preview]{standalone}\n\n\
\\\\usepackage{$(PKG)}\n\n\
\\\\begin{document}\n\
\include{$(TMPPKG)}\n\
\\\\end{document}\n" > $@

tabular:
	@echo "Latex tabular mode."
	@$(call test_latex,latex -$@)
	@$(call test_latex_pkg,latex -$@)

syntax:
	@echo "Latex syntax mode."
	@$(call test_latex,latex -$@)
	@$(call test_latex_pkg,latex -$@)

backnaur:
	@echo "Latex backnaur mode."
	@$(call test_latex,latex -$@)
	@$(call test_latex_pkg,latex -$@)

html:
	@echo "Html mode."
	@$(call test,$@)

clean:
	@rm -rf $(TMP)* $(PKG)* $(MAIN)*
