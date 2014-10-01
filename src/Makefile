#
# Makefile for Ebnf2ps (Haskell98)
# copyright 1996, 1998 by Peter Thiemann
#

HC=	  ghc
HC_SPEC_FLAGS=	-cpp -package unix -fglasgow-exts
#HC_SPEC_FLAGS=	-cpp -package data -fglasgow-exts
HCLOADFLAGS=

#for ghc-4.04
#HC=	  ghc
#HC_SPEC_FLAGS=	-cpp -syslib misc -syslib posix -fglasgow-exts -H27M
#HCLOADFLAGS=

#for hbc-0.9999.4
#HC=       hbc
#HC_SPEC_FLAGS=	-cpp -I.
#HCLOADFLAGS=	LOGBYTESPERWORD.c	# for hbc-0.9999.4 on AIX 4.2

#OPT=		-fvia-C -O2-for-C 
#OPT=		-O
OPT=
HSCPP=	/usr/local/lib/ghc-4.00/hscpp


# change it, or correct it later with environment variables (see README)
AFMPATH= /usr/local/tex/Adobe
RGBPATH= /usr/lib/X11


OBJECTS=  Parsers.o StringMatch.o Info.o Fonts.o IOSupplement.o EbnfLayout.o \
	  PsOutput.o Fig31Output.o EbnfOutput.o Color.o CommandLine.o AbstractSyntax.o \
	  Lexer.o Ebnf2psParser.o GrammarTransform.o GrammarUnfold.o Version.o \
	  Defaults.o PathExpansion.o Ebnf2ps.o

OTHER_FLAGS=	-DAFMPATH=\\\"$(AFMPATH)\\\" -DRGBPATH=\\\"$(RGBPATH)\\\"

HCFLAGS=	$(OPT) $(HC_SPEC_FLAGS)

HAPPY=		happy
HAPPYFLAGS=	-i
RM=		rm -f


ebnf2ps: $(OBJECTS)
	$(HC) -o ebnf2ps $(HCLOADFLAGS) $(HCFLAGS) $(OBJECTS)

Ebnf2psrun:	Ebnf2psParser.hs
	runhugs +h10M -F'$(HSCPP) $(OTHER_FLAGS)' Ebnf2ps.hs $(args)

CommandLine.hs: CommandLineFlags
	$(RM) CommandLine.hs
	Mkhprog -haskell-1.3 -m CommandLine -f parse_cmds -o CommandLine.hs -- `sed '/^#/d' CommandLineFlags`

afmpath.h: Makefile
	echo '                               "'$(AFMPATH)'"' > afmpath.h
rgbpath.h: Makefile
	echo '                               "'$(RGBPATH)'"' > rgbpath.h

Defaults.o: afmpath.h rgbpath.h

clean: 
	$(RM) $(OBJECTS) ebnf2ps *.hi afmpath.h rgbpath.h

#-----------------------------------------------------------------------------
Version.o: Version.hs
Info.o: Info.hs Fonts.hi Color.hi
AbstractSyntax.o: AbstractSyntax.hs
GrammarTransform.o: GrammarTransform.hs
GrammarTransform.hi: GrammarTransform.o
Lexer.o: Lexer.hs
Parsers.o: Parsers.hs
StringMatch.o: StringMatch.hs Parsers.hi
Fonts.o: Fonts.hs
IOSupplement.o: IOSupplement.hs
EbnfLayout.o: EbnfLayout.hs AbstractSyntax.hi Fonts.hi Info.hi Version.hi
PsOutput.o: PsOutput.hs EbnfLayout.hi Fonts.hi Color.hi Info.hi Version.hi
Fig31Output.o: Fig31Output.hs EbnfLayout.hi Fonts.hi Color.hi Info.hi Version.hi
Ebnf2psParser.o: Ebnf2psParser.hs AbstractSyntax.hi Lexer.hi 
CommandLine.o: CommandLine.hs
Color.o: Color.hs
GrammarUnfold.o: GrammarUnfold.hs AbstractSyntax.hi GrammarTransform.hi StringMatch.hi
EbnfOutput.o : EbnfOutput.hs AbstractSyntax.hi GrammarUnfold.hi
Ebnf2ps.o : Ebnf2ps.hs \
            IOSupplement.hi CommandLine.hi Fonts.hi Fig31Output.hi PsOutput.hi \
            Ebnf2psParser.hi EbnfLayout.hi AbstractSyntax.hi Color.hi Version.hi \
            Info.hi EbnfOutput.hi GrammarTransform.hi GrammarUnfold.hi StringMatch.hi 

Ebnf2psParser.hs: Ebnf2psParser.ly

#-----------------------------------------------------------------------------
# Generic make rules for haskell programs.

%.hs : %.ly
	$(RM) $@
	$(HAPPY) $< $(HAPPYFLAGS)

%.o : %.lhs
	$(RM) $@
	$(HC) -c $(HCFLAGS) $< -o $*.o

%.o : %.hs
	$(RM) $@
	$(HC) -c $(HCFLAGS) $< -o $*.o

%.hi : %.o
	@if [ ! -f $@ ] ; then \
	echo $(RM) $< ; \
	$(RM) $< ; \
	set +e ; \
	echo $(MAKE) $< ; \
	$(MAKE) $< ; \
	if [ $$? -ne 0 ] ; then \
	exit 1; \
	fi ; \
	fi
