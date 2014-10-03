--                            -*- Mode: Haskell -*-
-- Copyright 1994 by Peter Thiemann
-- GrammarTransform.hs --- some transformations on parse trees
-- Author          : Peter Thiemann
-- Created On      : Thu Oct 21 16:44:17 1993
-- simplifying transformations on grammars

module GrammarTransform
    (simplify,
     happysimplify, happysimplifyExt,
     yaccsimplify, yaccsimplifyExt,
     freents, simp0)
where

import AbstractSyntax
import Data.List


data RInfo
    = ROk [Production] [Production] Production
    | RKo


simplify :: [Production] -> [Production]
simplify         = map (simplify2' . simplify') . simp3
happysimplify    = simp0
yaccsimplify     = simp0
happysimplifyExt = (\ p -> (fst p, map (simplify2' . simplify') (snd p))) . (simp0 . simp3o)
yaccsimplifyExt  = (\ p -> (fst p, map (simplify2' . simplify') (snd p))) . (simp0 . simp3o)



-- simp1 gets the body of a ProdFactor as an argument
-- and provides the transformations
--	beta { X } X gamma	--->	beta (X)+ gamma
--	beta X { X } gamma	--->	beta (X)+ gamma
--	beta { X Y } X gamma	--->	beta (X)/ (Y) gamma
--	beta X { Y X } gamma	--->	beta (X)/ (Y) gamma
simp1 [] = []
simp1 [p] = [p]
simp1 (ProdRepeat p:p':prods)
	| p `eqProduction` p' = ProdRepeat1 p: simp1 prods
simp1 (p:ProdRepeat p':prods)
	| p `eqProduction` p' = ProdRepeat1 p: simp1 prods
simp1 (ProdRepeat (ProdFactor [p1, p2]):p:prods)
	| p1 `eqProduction` p = ProdRepeatWithAtom p p2: simp1 prods
simp1 (p:ProdRepeat (ProdFactor [p1, p2]):prods)
	| p `eqProduction` p2 = ProdRepeatWithAtom p p1: simp1 prods
simp1 (p:prods) = p: simp1 prods


-- simp2'r gets the body of a ProdTerm as an argument
-- and provides the transformations (slow)
--   gamma X | delta X  ---> (gamma | delta)  X
--   gamma X | X        ---> [gamma] X
--   X | gamma  X       ---> [gamma| X

rightpart2'r [p] x = (x `eqProduction` p, [])
rightpart2'r ps x =
    let l = last ps
	n = length ps
    in (x `eqProduction` l, init ps)

rightParts rest rest' =
    let l' = last rest'
    in case (rightpart2'r rest l') of
	   (True, front) -> ROk front (init rest') l'
	   (False, _) -> RKo

simp2'r' term@(ProdTerm prods) =
    let res = simplifier term
    in	case res of
	    (ProdFactor _) -> res
	    (ProdTerm ps) -> ProdTerm (simp2'r ps)
	    _ -> term

simp2'r (first@(ProdFactor (p:rest)): next@(ProdFactor (p':rest')): more) =
    case (rest, rest') of
	([], [])
	    | p `eqProduction` p' -> simp2'r (ProdFactor [p]: more)
	    | otherwise -> first: simp2'r (next:more)
        ([], _) -> case (rightpart2'r rest' p) of
		       (True,front) -> simp2'r (ProdFactor [ProdOption (ProdFactor (p':front)), p] : more)
		       (False,_) -> first: simp2'r (next:more)
        (_, []) -> case (rightpart2'r rest p') of
		       (True, front) -> simp2'r (ProdFactor [ProdOption (ProdFactor (p:front)),p']: more)
		       (False,_) -> first: simp2'r (next:more)
        (_, _) -> case (rightParts rest rest') of
		      RKo -> first: simp2'r (next: more)
		      (ROk front1 front2 l) -> let t = simp2'r' (ProdTerm [ProdFactor (p:front1), ProdFactor (p':front2)])
		                               in simp2'r (ProdFactor [t,l] : more)
simp2'r [p] = [p]
simp2'r [] = []




-- simp2 gets the body of a ProdTerm as an argument
-- and provides the transformations
--     X gamma | X delta  ---> X (gamma | delta)
--     X gamma | X        ---> X [ gamma ]
--     X | X gamma        ---> X [ gamma ]
getPrefixes ps1@(p1:_:_) ps2@(p2:_:_) firsts
    | p1 `eqProduction` p2 = getPrefixes (tail ps1) (tail ps2) (firsts++[p1])
getPrefixes ps1 ps2 firsts = (firsts, ps1, ps2)

lookahead x more = partition (la x) more
    where la x (ProdFactor (p:_)) = x `eqProduction` p
	  la x _ = False


simplifier term@(ProdTerm [a,b]) =
    case (a,b) of
	(ProdFactor [ProdTerm psa@(x:y:_)], ProdFactor [ProdTerm psb@(x':y':_)]) -> ProdTerm (psa++psb)
	(ProdFactor [ProdTerm psa@(x:y:_)], _) -> ProdTerm (psa++[b])
	(_, ProdFactor [ProdTerm psb@(x':y':_)]) -> ProdTerm (a:psb)
	(_,_) -> term
simplifier term = term

simp2' term@(ProdTerm prods) =
    let res = simplifier term
    in	case res of
	    (ProdFactor _) -> res
	    (ProdTerm ps) -> ProdTerm (simp2 ps)
	    _ -> term


simp2 (ProdFactor (p:rest): next@(ProdFactor (p':rest')): more)
    | p `eqProduction` p' = case (rest, rest') of
	([], []) -> simp2 (ProdFactor [p]: more)
	([], _)  -> case (lookahead p more) of
			([],_) -> simp2 (ProdFactor [p, ProdOption (ProdFactor rest')]: more)
			(others,more') -> simp2 ((next:others)++(ProdFactor [p]:more'))
	(_,  []) -> simp2 (ProdFactor [p, ProdOption (ProdFactor rest)]: more)
	(_,  _)  -> let (firsts,rs,rs') = getPrefixes rest rest' []
			pfirsts = p:firsts
			next =  simp2' (ProdTerm [ProdFactor rs, ProdFactor rs'])
		    in simp2 ((ProdFactor (pfirsts++[next])):more)
   | otherwise = ProdFactor (p:rest): simp2 (ProdFactor (p':rest'):more)
simp2 [p] = [p]
simp2 [] = []


-- simp3 gets a list of ProdProductions and looks for left and right recursive productions
-- it executes the transformations
--	A -> A gamma_1 | ... | A gamma_k | delta
--	--->
--	A -> delta { gamma_1 | ... | gamma_k }
-- and
--	A -> gamma_1 A | ... | gamma_k A | delta
--	--->
--	A -> { gamma_1 | ... | gamma_k } delta

leftParty nt (ProdTerm ps) = foldr f ([], []) ps
  where f (ProdFactor (ProdNonterminal nt':rest)) (yes, no)
	  | nt == nt' = (ProdFactor rest:yes, no)
        f p (yes, no) = (yes, p:no)

simp3'l prod@(ProdProduction nt nts p@(ProdTerm _))
  = case leftParty nt p of
	(lefties@(_:_), others@(_:_)) ->
		ProdProduction nt nts
		  (ProdFactor [ProdTerm others, ProdRepeat (ProdTerm lefties)])
	_ -> prod
simp3'l prod = prod

rightParty nt (ProdTerm ps) = foldr f ([], []) ps
  where f (ProdFactor ps) (yes, no)
	  | length ps > 1 && rightmost nt ps = (ProdFactor (init ps):yes, no)
	f p (yes, no) = (yes, p:no)

rightmost nt [ProdNonterminal nt'] = nt == nt'
rightmost nt [p] = False
rightmost nt (p:ps) = rightmost nt ps

simp3'r prod@(ProdProduction nt nts p@(ProdTerm _))
  = case rightParty nt p of
	(righties@(_:_), others@(_:_)) ->
		ProdProduction nt nts
		  (ProdFactor [ProdRepeat (ProdTerm righties), ProdTerm others])
	_ -> prod
simp3'r prod = prod

simp3 :: [Production] -> [Production]
simp3 = map (simp3'r . simp3'l)



-- [happyInput && yaccInput]
-- simp0 gets a  list of productions and looks for empty productions on the right
-- side of happy rule and provides the transformations
--      N  -> X  |  .                   --->  N  ->  [X].
-- and  N ->  X1 | X2 | ... |  Xk |  .  --->  N -> [X1| X2 | ... | Xk].

simp0' prod@(ProdProduction nt nts p@(ProdTerm prods)) =
  case (partition (\p -> not (p `eqProduction` emptyProd)) prods) of
   ([], _) -> ([nt], ProdProduction nt nts ProdEmpty)
   (prods', []) -> ([],prod)
   (prods', (x:_)) -> ([], ProdProduction nt nts (ProdOption (ProdTerm prods')))
  where emptyProd = ProdFactor []
simp0' prod = ([],prod)

simp0 prods = foldr f ([],[]) (map simp0' prods)
   where f ([],p)  (nts,ps) = (nts,p:ps)
         f ([n],p) (nts,ps) = (n:nts,p:ps)



-- [happyInput && yaccInput]
-- simp3o gets a list of ProdProductions and looks for left and right recursive productions
-- it executes the transformations

simp3o :: [Production] -> [Production]
simp3o = map (simp3o'r . simp3o'l)

-- [simp3o'l]
-- A -> A gamma_1 | ... | A gamma_k | delta    -->  A -> delta { gamma_1 | ... | gamma_k }
-- A -> A gamma_1 | ... | A gamma_k |  'empty' -->  A -> { gamma_1 | ... | gamma_k }
--
-- A -> A gamma_1 | ... | A gamma_k | delta_1 | ... | delta_k | 'empty'
--  -->
-- A -> [delta_1 | ... | delta_k] { gamma_1 | ... | gamma_k }

leftParty' nt (ProdTerm ps) = foldr f ([], [], False) ps
  where f (ProdFactor []) (yes,no,_) = (yes,no,True)
        f (ProdFactor (ProdNonterminal nt':rest)) (yes, no,emptyProds)
	  | nt == nt' = (ProdFactor rest:yes, no,emptyProds)
        f p (yes, no,emptyProds) = (yes, p:no,emptyProds)

-- repeatWithAtom'l:  X {delta X} --> X / delta
repeatWithAtom'l prod@(ProdFactor [p1,p2]) =
    case (p1,p2) of
	(front@(ProdTerm [ProdFactor [x]]), ProdRepeat (ProdTerm [ProdFactor p2s@(a:b:_)])) ->
	  if (last p2s) `eqProduction` x
	  then ProdRepeatWithAtom front (ProdFactor (init p2s))
	  else prod
	_ -> prod
repeatWithAtom'l p = p

simp3o'l prod@(ProdProduction nt nts p@(ProdTerm _))
  = case leftParty' nt p of
        (lefties@(_:_), [], _) ->
	        ProdProduction nt nts
                  (ProdRepeat (ProdTerm lefties))
        (lefties@(_:_), others@(_:_), True) ->
                ProdProduction nt nts
                  (ProdFactor [ProdOption (ProdTerm others), ProdRepeat (ProdTerm lefties)])
	(lefties@(_:_), others@(_:_), False) ->
		ProdProduction nt nts
		  (repeatWithAtom'l (ProdFactor [ProdTerm others, ProdRepeat (ProdTerm lefties)]))
	_ -> prod
simp3o'l prod = prod



-- [simp3o'r]
-- A -> gamma_1 A | ... | gamma_k A | delta    --> A -> { gamma_1 | ... | gamma_k } delta
-- A -> gamma_1 A | ... | gamma_k A | 'empty'  --> A -> { gamma_1 | ... | gamma_k }
-- A -> gamma_1 A | ... | gamma_k A | delta_1 | ... | delta_k | 'empty'
--  -->
-- A -> { gamma_1 | ... | gamma_k } [delta_1 | ... | delta_k]

-- repeatWithAtom'r:  {X delta} X --> X / delta
repeatWithAtom'r prod@(ProdFactor [p1,p2]) =
    case (p1,p2) of
	(ProdRepeat (ProdTerm [ProdFactor p2s@(a:b:_)]), front@(ProdTerm [ProdFactor [x]])) ->
	  if a `eqProduction` x
	  then ProdRepeatWithAtom front (ProdFactor (tail p2s))
	  else prod
	_ -> prod
repeatWithAtom'r p = p

rightParty' nt (ProdTerm ps) = foldr f ([], [], False) ps
  where f (ProdFactor ps) (yes, no, emptyProds)
  	  | length ps == 0 = (yes,no,True)
	  | length ps > 1 && rightmost nt ps = (ProdFactor (init ps):yes, no, emptyProds)
	f p (yes, no,emptyProds) = (yes, p:no,emptyProds)

simp3o'r prod@(ProdProduction nt nts p@(ProdTerm _))
  = case rightParty' nt p of
	(righties@(_:_), [], _) ->
                ProdProduction nt nts
                  (ProdRepeat (ProdTerm righties))
	(righties@(_:_), others@(_:_), True) ->
                ProdProduction nt nts
	          (ProdFactor [ProdRepeat (ProdTerm righties), ProdOption (ProdTerm others)])
	(righties@(_:_), others@(_:_), False) ->
		ProdProduction nt nts
		  (repeatWithAtom'r (ProdFactor [ProdRepeat (ProdTerm righties), ProdTerm others]))
	_ -> prod
simp3o'r prod = prod


-- compute the set of all nonterminals in a Production
freents :: Production -> [String]
freents (ProdTerm prods)           = concat (map freents prods)
freents (ProdFactor prods)         = concat (map freents prods)
freents (ProdNonterminal s)        = [s]
freents (ProdTerminal s)           = []
freents (ProdOption p)             = freents p
freents (ProdRepeat p)             = freents p
freents (ProdRepeat1 p)            = freents p
freents (ProdRepeatWithAtom p1 p2) = freents p1 ++ freents p2
freents (ProdPlus)                 = []
freents (ProdEmpty)		   = []
freents (ProdSlash p)              = freents p


simplify' (ProdProduction s1 s2 prod)	= ProdProduction s1 s2 (simplify' prod)
simplify' (ProdFactor prods)		= ProdFactor (simp1 (map simplify' prods))
simplify' (ProdNonterminal s)		= ProdNonterminal s
simplify' (ProdTerminal s)		= ProdTerminal s
simplify' (ProdOption prod)		= ProdOption (simplify' prod)
simplify' (ProdRepeat prod)		= ProdRepeat (simplify' prod)
simplify' (ProdRepeat1 prod)		= ProdRepeat1 (simplify' prod)
simplify' (ProdRepeatWithAtom p1 p2)    = ProdRepeatWithAtom (simplify' p1) (simplify' p2)
simplify' (ProdPlus)			= ProdPlus
simplify' (ProdEmpty)	                = ProdEmpty
simplify' (ProdSlash prod)		= ProdSlash (simplify' prod)
simplify' (ProdTerm prods)		= ProdTerm ((simp2 . map simplify') prods)

simplify2' (ProdProduction s1 s2 prod)	= ProdProduction s1 s2 (simplify2' prod)
simplify2' (ProdFactor prods)		= ProdFactor (simp1 (map simplify2' prods))
simplify2' (ProdNonterminal s)		= ProdNonterminal s
simplify2' (ProdTerminal s)		= ProdTerminal s
simplify2' (ProdOption prod)		= ProdOption (simplify2' prod)
simplify2' (ProdRepeat prod)		= ProdRepeat (simplify2' prod)
simplify2' (ProdRepeat1 prod)		= ProdRepeat1 (simplify2' prod)
simplify2' (ProdRepeatWithAtom p1 p2)   = ProdRepeatWithAtom (simplify2' p1) (simplify2' p2)
simplify2' (ProdPlus)			= ProdPlus
simplify2' (ProdEmpty)	                = ProdEmpty
simplify2' (ProdSlash prod)		= ProdSlash (simplify2' prod)
simplify2' (ProdTerm prods)		= ProdTerm ((simp2'r . map simplify2') prods)


-- Goferisms:
eqList [] [] = True
eqList (x:xs) (y:ys) = eqProduction x y && eqList xs ys
eqList _ _ = False

eqProduction (ProdFile ps) (ProdFile ps') = eqList ps ps'
eqProduction (ProdProduction str ostr p) (ProdProduction str' ostr' p') = str == str' && ostr == ostr' && eqProduction p p'
eqProduction (ProdTerm ps) (ProdTerm ps') = eqList ps ps'
eqProduction (ProdFactor ps) (ProdFactor ps') = eqList ps ps'
eqProduction (ProdNonterminal str) (ProdNonterminal str') = str == str'
eqProduction (ProdTerminal str) (ProdTerminal str') = str == str'
eqProduction (ProdOption p) (ProdOption p') = eqProduction p p'
eqProduction (ProdRepeat p) (ProdRepeat p') = eqProduction p p'
eqProduction (ProdRepeatWithAtom p1 p2) (ProdRepeatWithAtom p1' p2') = eqProduction p1 p1' && eqProduction p2 p2'
eqProduction (ProdRepeat1 p) (ProdRepeat1 p') = eqProduction p p'
eqProduction (ProdPlus) (ProdPlus) = True
eqProduction (ProdEmpty) (ProdEmpty) = True
eqProduction (ProdSlash p) (ProdSlash p') = eqProduction p p'
eqProduction _ _ = False
