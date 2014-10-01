--                            -*- Mode: Haskell -*- 
-- Copyright © 1996, 1998 by Peter Thiemann
-- GrammarUnfold.hs 
-- main functions for ebnfInput, yaccInput and happyInput.
-- Initial revision
-- 
-- Last Modified By: M. Walter
--


module GrammarUnfold (
     GrammarInfo(..),
     rawEBNFInput, rawHappyInput, rawYaccInput,
     getNonterminals, getTerminals, getReduces, getProdsInfo, getEmpties, getRedProds
     ) where
 
import AbstractSyntax
import GrammarTransform 
import StringMatch      (stringMatch)
import List

import qualified Data.Map (Map, keys, elems, findWithDefault, fromList)

type FiniteMap k x = Data.Map.Map k x
keysFM = Data.Map.keys
eltsFM = Data.Map.elems
listToFM = Data.Map.fromList
lookupWithDefaultFM theMap theDefault theKey = 
  Data.Map.findWithDefault theDefault theKey theMap

type UnfoldInfo = FiniteMap String Production

disjointLists [] bs = True
disjointLists (a:as) bs | a `elem` bs = False
			| otherwise = disjointLists as bs

type Alphabet    = ([String],      -- nonterminals 
  	            [String])      -- terminals 


data GrammarInfo = GrammarInfo
 		 [Production]	   -- all productions 
 		 [String]	   -- Nonterminals
 		 [String]	   -- Terminals
 		 [String]	   -- Nonterminals (option: +unfold)
 		 [String]          -- Nonterminals with replacements
 		 [String]	   -- identifiers of empty productions

----------------------------------------------------------------------------------------------

-- happyInput

rawHappyInput :: Bool		   -- flag: +ebnf 
	      -> Bool		   -- flag: +simplify 
	      -> Bool		   -- flag: +unfold 
	      -> [String] 
	      -> [Production] -> GrammarInfo
rawHappyInput ebnfoutput doSimplify True unfold_nts prods =
     let (nts, prods') = (if doSimplify then happysimplifyExt else happysimplify) prods
         (rnts, names, prods'') = unfoldGrammar unfold_nts prods'
 	 (nonterminals, terminals) | ebnfoutput = allSymbols prods''
				   | otherwise  = ([], [])
     in GrammarInfo prods'' nonterminals terminals rnts names nts
 
rawHappyInput ebnfoutput doSimplify False _ prods =
     let (nts ,prods') = (if doSimplify then happysimplifyExt else happysimplify) prods 
	 (nonterminals, terminals) | ebnfoutput = allSymbols prods'
				   | otherwise  = ([], [])
     in GrammarInfo prods' nonterminals terminals [] [] nts 



-- yaccInput

rawYaccInput :: Bool		   -- flag: +ebnf 
	     -> Bool		   -- flag: +simplify 
	     -> Bool		   -- flag: +unfold 
	     -> [String] 
	     -> [Production] -> GrammarInfo
rawYaccInput ebnfoutput doSimplify True unfold_nts prods =
    let (nts, prods') = (if doSimplify then yaccsimplifyExt else yaccsimplify) prods
	(rnts, names, prods'') = unfoldGrammar unfold_nts prods'
	((nonterminals , terminals), oprods) | ebnfoutput = (allSymbols prods'', reverse prods'')
					     | otherwise  = (([], []), prods'')
    in  GrammarInfo oprods nonterminals terminals rnts names nts 
 
rawYaccInput ebnfoutput doSimplify False _ prods =
    let (nts ,prods') = (if doSimplify then yaccsimplifyExt else yaccsimplify) prods 
	((nonterminals, terminals), oprods) | ebnfoutput = (allSymbols prods', reverse prods')
					    | otherwise  = (([], []), prods')
    in  GrammarInfo oprods nonterminals terminals [] [] nts 



-- ebnfInput

rawEBNFInput :: Bool		   -- flag: +simplify 
	     -> Bool		   -- flag: +unfold 
	     -> [String] -> [Production] -> GrammarInfo
rawEBNFInput doSimplify True unfold_nts prods =
    let prods' = if doSimplify then simplify prods else prods
	(rnts, names, prods'') = unfoldGrammar unfold_nts prods'
    in GrammarInfo prods'' [] [] rnts names []
 
rawEBNFInput doSimplify False _ prods = 
    GrammarInfo (if doSimplify then simplify prods else prods) [] [] [] [] []

--------------------------------------------------------------------------------------------------

unfoldGrammar :: [String] 
	      -> [Production] 
	      --  (nonterminals, nonterminals with a replacement, new productions)
	      -> ([String], [String], [Production])
unfoldGrammar [] prods = ([], [], prods)
unfoldGrammar rnts prods = unfoldGrammar' rnts prods
 
unfoldGrammar' :: [String] -> [Production] -> ([String],[String],[Production])
unfoldGrammar' [] prods = ([], [], prods)
unfoldGrammar' rnts prods =
     (keysFM rntsFM, reducedProds, foldr (++) [] [(eltsFM rntsFM), prods', rest])
     where  (rntsInfo,reducedProds, prodsToUnfold, rest) = splitInputGrammar rnts prods
	    rntsFM = listToFM rntsInfo
 	    prods' = map (unfoldSimp . unfoldProd []) prodsToUnfold
 
 	    unfoldProd unf (ProdProduction nt ntAliases p) = ProdProduction nt ntAliases (unfoldProd unf p)
	    unfoldProd unf (ProdTerm prods) = ProdTerm (map (unfoldProd unf) prods)
	    unfoldProd unf (ProdFactor prods) = ProdFactor (map (unfoldProd unf) prods)
 	    unfoldProd unf p@(ProdNonterminal nt) 
	      | nt `elem` unf = p
	      | otherwise     = unfoldProd (nt:unf) $ lookupWithDefault p nt rntsFM
 	    unfoldProd unf (ProdOption p) = ProdOption (unfoldProd unf p)
 	    unfoldProd unf (ProdRepeat p) = ProdRepeat (unfoldProd unf p)
 	    unfoldProd unf (ProdRepeatWithAtom p1 p2) = ProdRepeatWithAtom (unfoldProd unf p1) (unfoldProd unf p2)
 	    unfoldProd unf (ProdRepeat1 p) = ProdRepeat1 (unfoldProd unf p)
 	    unfoldProd unf (ProdSlash p) = ProdSlash (unfoldProd unf p)
 	    unfoldProd unf p = p
 
 	    unfoldSimp (ProdProduction nt ntAliases p) = ProdProduction nt ntAliases (unfoldSimp p)
 	    unfoldSimp (ProdTerm prods) = ProdTerm (map unfoldSimp prods)
 	    unfoldSimp (ProdFactor prods) = ProdFactor (map unfoldSimp prods)
 	    unfoldSimp (ProdOption (ProdOption p)) = ProdOption (unfoldSimp p)
 	    unfoldSimp (ProdOption (ProdRepeat p)) = ProdRepeat (unfoldSimp p)
 	    unfoldSimp (ProdOption p) = ProdOption (unfoldSimp p)
 	    unfoldSimp (ProdRepeat (ProdOption p)) = ProdRepeat (unfoldSimp p)
 	    unfoldSimp (ProdRepeat p) = ProdRepeat (unfoldSimp p)
 	    unfoldSimp (ProdRepeatWithAtom p1 p2) = ProdRepeatWithAtom (unfoldSimp p1) (unfoldSimp p2)
 	    unfoldSimp (ProdRepeat1 p) = ProdRepeat1 (unfoldSimp p)
 	    unfoldSimp (ProdSlash p) = ProdSlash (unfoldSimp p)
 	    unfoldSimp p = p
     

lookupWithDefault :: Production -> String -> UnfoldInfo -> Production
lookupWithDefault deflt nt info =
    case (lookupWithDefaultFM info deflt nt) of
	(ProdProduction nt' _ ProdEmpty) -> deflt 
	(ProdProduction _ _ p) -> p
	p -> p


splitInputGrammar :: [String] -> [Production] -> ([(String,Production)],[String],[Production],[Production])
splitInputGrammar rnts prods
    = foldr splitGrammar ([],[],[],[]) prods
    where
 	splitGrammar prod@(ProdProduction nt _ ProdEmpty) (yes,ps,todo,no) = (yes,ps,todo,prod:no)
 	splitGrammar prod@(ProdProduction nt _ p) (yes,ps,todo,no)
 	  | any (flip stringMatch nt) rnts = ((nt,prod):yes, ps, todo, no)
 	  | any (\x -> any (flip stringMatch x) rnts) (freents p) = (yes,nt:ps,prod:todo, no)
 	  | otherwise = (yes, ps, todo, prod:no)


allSymbols :: [Production] -> Alphabet
allSymbols prodsInput = 
    (\(nts,ts) -> (nts, nub ts)) $ (foldr f ([],[]) prodsInput)
    where f (ProdProduction nt _ p) (nts, ts) = (nt:nts, (nub (freets p))++ts)
	    -- compute the set of all terminals in a Production
	  freets (ProdTerm prods)           = concat (map freets prods)
	  freets (ProdFactor prods)         = concat (map freets prods)
	  freets (ProdNonterminal s)        = []
	  freets (ProdTerminal s)           = [s]
	  freets (ProdOption p)             = freets p
	  freets (ProdRepeat p)             = freets p
	  freets (ProdRepeat1 p)            = freets p
	  freets (ProdRepeatWithAtom p1 p2) = freets p1 ++ freets p2
	  freets (ProdPlus)                 = []
	  freets (ProdEmpty)                = []
	  freets (ProdSlash p)              = freets p
 
 	
getProdsInfo    (GrammarInfo prods _ _ _ _ _) = prods
getNonterminals (GrammarInfo _ nts _ _ _ _)   = nts
getTerminals    (GrammarInfo _ _ ts _ _ _)    = ts
getReduces      (GrammarInfo _ _ _ rs _ _)    = rs
getRedProds     (GrammarInfo _ _ _ _ rp _)    = rp
getEmpties      (GrammarInfo _ _ _ _ _ es)    = es

	    
	
