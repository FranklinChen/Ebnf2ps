module AbstractSyntax (Production (..)) where
-- Copyright 1994 by Peter Thiemann
-- Last Modified By: M. Walter

data Production
	= ProdFile [Production]
	| ProdProduction String [String] Production	    -- optional print name
	| ProdTerm [Production]				    -- an alternative of terms
	| ProdFactor [Production]			    -- a sequence of factors
	| ProdNonterminal String
	| ProdTerminal String
	| ProdOption Production
	| ProdRepeat Production
	| ProdRepeatWithAtom Production Production
	| ProdRepeat1 Production
	| ProdPlus					    -- a helper
	| ProdSlash Production				    -- another helper
	| ProdEmpty
	deriving Eq

