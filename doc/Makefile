# After Cabal build.
EBNF2PS = ../dist/build/ebnf2ps/ebnf2ps

EBNF_EPS_FILES = Atom.eps Character.eps ExtAtom.eps Factor.eps File.eps Nonterminal.eps Production.eps String.eps Term.eps

UNFOLD_EBNF_EPS_FILES = Atom_unfold.eps Production_unfold.eps

REGULAR_EPS_FILES = RAtom.eps RExtAtom.eps RFactor.eps Regexp.eps

UNFOLD_REGULAR_EPS_FILES = RAtom_unfold.eps

EPS_FILES = $(UNFOLD_EBNF_EPS_FILES) \
	$(EBNF_EPS_FILES) \
	$(UNFOLD_REGULAR_EPS_FILES) \
	$(REGULAR_EPS_FILES)

test:	$(EPS_FILES)

$(UNFOLD_EBNF_EPS_FILES):	../examples/ebnf.BNF Makefile
	$(EBNF2PS) -verbose -tScale 8 -ntScale 8 +unfold $< '.*' -- String '.*\(t\|T\)erm.*'
	mv Atom.eps Atom_unfold.eps
	mv Production.eps  Production_unfold.eps

$(EBNF_EPS_FILES):	../examples/ebnf.BNF Makefile
	$(EBNF2PS) -verbose -tScale 8 -ntScale 8 $< '.*' -- String '.*\(t\|T\)erm.*'

$(UNFOLD_REGULAR_EPS_FILES):	../examples/regular.BNF Makefile
	$(EBNF2PS) +unfold $< RAtom -- Regexp
	mv RAtom.eps RAtom_unfold.eps

$(REGULAR_EPS_FILES):	../examples/regular.BNF Makefile
	$(EBNF2PS) -titleFont AvantGarde-Book -ntBoxColor White -fatLineWidth 50 $< '.*'

docs:	doc.pdf doc.ps

doc.pdf:	doc.tex $(EPS_FILES)
	latexmk -pdf $<

doc.ps:	doc.tex $(EPS_FILES)
	latexmk -ps $<

.PHONY:	test docs
