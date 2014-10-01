FILES=	  src/Parsers.hs src/StringMatch.hs src/Info.hs src/Fonts.hs src/IOSupplement.hs \
	  src/EbnfLayout.hs src/EbnfOutput.hs src/PsOutput.hs src/Fig31Output.hs src/Version.hs \
          src/Color.hs src/AbstractSyntax.hs src/Lexer.hs src/GrammarTransform.hs src/GrammarUnfold.hs \
	  src/Ebnf2ps.hs \
	  src/Ebnf2psParser.ly src/Ebnf2psParser.hs src/CommandLineFlags src/Makefile \
	  src/README src/CommandLine.hs \
	  examples/ebnf.BNF examples/gofer.BNF examples/regular.BNF \
	  doc/Atom.eps doc/ExtAtom.eps doc/Factor.eps doc/File.eps \
	  doc/Production_unfold.eps doc/Nonterminal.eps \
	  doc/Production.eps doc/String.eps doc/Term.eps doc/Character.eps doc/Atom_unfold.eps \
	  doc/RAtom.eps doc/RAtom_unfold.eps doc/Export.eps \
	  doc/RExtAtom.eps doc/RFactor.eps doc/Regexp.eps \
	  doc/doc.tex doc/doc.ps doc/examples.ps

all:
	runhaskell Setup.hs configure
	runhaskell Setup.hs build
#	cd src; $(MAKE)

ebnf2ps.tar.gz: exclude
	-rm ebnf2ps*.tar*
	RELNAME=`sed -e /module/d -e 's/version[^0-9]*/ebnf2ps-/' -e 's/ .*//' src/Version.hs`; \
	tar cvfXCzh $$RELNAME.tar.gz exclude .. Ebnf2ps

clean:
	runhaskell Setup.hs clean
#	cd src; $(MAKE) clean

