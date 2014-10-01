--------------------------------------------------
-- Copyright 1994-2000 by Peter Thiemann
--------------------------------------------------

module PsOutput (psShowsWrapper) where

-- import EbnfLayout

import Version
import Fonts (FONT, fontName, fontScale, noFont)
import Color (Color, showsPsColor, noColor)
import Info (Container, GObject (..), TDirection (..), WrapperType)

infixr `comb`

-- psState = (currentColor, currentFont, currentLinewidth)

type PsState  = ((Color, FONT, Int), ShowS)
type PsTrafo  = PsState -> PsState

type PsState' = (Color, FONT, Int)
type PsTrafo' = PsState' -> PsState
 
initialState :: PsState'
initialState = (noColor, noFont, -1)
 
setColor :: Color -> PsTrafo'
setColor clr st@(clr0, fnt0, lw0)
   | clr == clr0 = (st, id)
   | otherwise   = ((clr, fnt0, lw0), showsPsColor clr)
 
setFont :: FONT -> PsTrafo'
setFont font st@(clr0, fnt0, lw0)
   | font == fnt0 = (st, id)
   | otherwise    = ((clr0, font, lw0),
                     showString ('/':fontName font) . showString " findfont " .
                     shows (fontScale font) . showString " scalefont" .
     		     showString " setfont\n")
 		   
setLineWidth :: Int -> PsTrafo'
setLineWidth lw st@(clr0, fnt0, lw0)
   | lw == lw0 = (st, id)
   | otherwise = ((clr0, fnt0, lw), showsPsNum lw . showString " slw\n")
 
drawBox :: Bool -> Int -> Int -> Int -> Int -> Int -> Color -> PsTrafo'
drawBox rounded ax ay width height lw clr@(r,g,b) (clr0, fnt0, lw0) = 
 	 ((clr0, fnt0, lw),
 	  showsPsNum ax . showsPsNum ay .
 	  showsPsNum width . showsPsNum height . showsPsNum lw . showsPsRGB r g b .
 	  showString (if rounded then " RBox\n" else " Box\n"))
 	  where showsPsRGB r g b = showChar ' ' . shows r .
 	                           showChar ' ' . shows g .
 	                           showChar ' ' . shows b 
 
drawString :: Int -> Int -> String -> PsTrafo'
drawString ax ay str (clr0, fnt0, lw0) = 
 	((clr0, fnt0, lw0),
 	 showsMoveto ax ay .
 	 showChar '(' . showString (psString str) . showChar ')' .
 	 showString " show\n")
 
drawRLine :: Int -> Int -> [(Int, Int)] -> PsTrafo'
drawRLine ax ay rels (clr0, fnt0, lw0) =
 	((clr0, fnt0, lw0),
 	 showString "n" .
 	 showsMoveto ax ay .
 	 foldr (.) (showString " s\n") [ showsRLineto rx ry | (rx, ry) <- rels ])
 
insertShowS :: ShowS -> PsTrafo'
insertShowS shower1 (clr0, fnt0, lw0) = ((clr0, fnt0, lw0), shower1)
 
runTrafo :: PsTrafo' -> ShowS
runTrafo f = shower where
 		      ((_, _, _), shower) = f initialState


comb :: PsTrafo' -> PsTrafo' -> PsTrafo'
comb pst1 pst2 st =
  let (r1, shower1) = pst1 st 
      st2@(r2, shower2) = pst2 r1
  in (r2, (shower1 . shower2))

returnPs :: ShowS -> PsTrafo'
returnPs s = \v -> (v, s)
 
psShowsWrapper :: String -> WrapperType
psShowsWrapper dateOutput title
 	 (borderDistX, borderDistY, lineWidth, fatLineWidth, arrowSize, titleFont,ntFont, tFont,
 	  (ntColor, tColor, lineColor, ntBoxColor, tBoxColor,_,_,_))
 		container@(rx, ry, width, height, inOutY, gobj) =
 	showString "%!PS-Adobe-2.0 EPSF-2.0" .
 	showString ("\n%%Creator: Ebnf2ps Version " ++ version ++ "(C) 1996-2000 by Peter Thiemann") .
	showString ("\n%%CreationDate: " ++ dateOutput) .
 	showString "%%DocumentFonts: " .
 	showString titleFontName .
 	printFonts titleFontName ntFontName tFontName .
 	showString "\n%%Title: " . showString title .
 	showString "\n%%Pages: 0" .
 	showString "\n%%BoundingBox:" . 
 	showsPsNum (psFloor rx) . showsPsNum (psFloor ry) .
 	showsPsNum (psCeil (rx+width)) . showsPsNum (psCeil (ry+height)) .
 	showString "\n%%EndComments\n" .
        showPsProlog psProlog .
 	showString "\n%%EndProlog\n" .
 	showString "\n$Ebnf2psBegin\n" .
 	runTrafo (psShowsContainer rx ry container) .
 	showString "\n$Ebnf2psEnd\n"
 	where
    titleFontName = fontName titleFont	
    ntFontName 	= fontName ntFont
    tFontName  	= fontName tFont
    showPsProlog s = foldr1 (\a b -> a . showString "\n" . b) (map showString s)
    printFonts f1 f2 f3 
   	| f1 == f2  = if f2 == f3 then id else (showChar ' ' . showString f3)
   	| otherwise = showChar ' ' . showString f2 . (if f2 == f3 then id else (showChar ' ' . showString f3))
 
    psShowsContainer :: Int -> Int -> Container -> PsTrafo'
    psShowsContainer ax ay (rx, ry, width, height, inOutY, gobj) =
 	case gobj of
 	AString color font theString ->
                setFont font                   `comb`
                setColor color                 `comb`
 		drawString ax1 ay1 theString 
 	ABox color bgcolor rounded content ->
 		setColor color                 `comb`
 		drawBox rounded ax1 ay1 width height fatLineWidth bgcolor `comb`
 		psShowsContainer ax1 ay1 content 
        Arrow color size ->
 	    setColor color                      `comb`
 	    setLineWidth lineWidth              `comb`
 	    drawRLine  (ax1-size) (ay1+abs size) [(size, -abs size), (-size, -abs size)] 
 	Aline color ->
 	    setColor color                      `comb`
 	    setLineWidth lineWidth              `comb`
 	    drawRLine ax1 ay1 [(width, height)] 
 	ATurn color dir ->
 	    setColor color                      `comb`
 	    setLineWidth lineWidth              `comb`
 	    returnPs (
 		showString "n" .
 		showsIt dir .
 		showString " s\n") 
 		where
 		  showsIt SE = showsMoveto ax1 ay1 .
 			       showsArcto ax1 (ay1+height) (ax1+width) (ay1+height) radius .
 			       showsLineto (ax1+width) (ay1+height)
 		  showsIt WN = showsMoveto ax1 ay1 .
 			       showsArcto (ax1+width) ay1 (ax1+width) (ay1+height) radius .
 			       showsLineto (ax1+width) (ay1+height)
 		  showsIt SW = showsMoveto (ax1+width) ay1 .
 			       showsArcto (ax1+width) (ay1+height) ax1 (ay1+height) radius .
 			       showsLineto ax1 (ay1+height)
 		  showsIt NE = showsMoveto (ax1+width) ay1 .
 			       showsArcto ax1 ay1 ax1 (ay1+height) radius .
 			       showsLineto ax1 (ay1+height)
 		  radius = min height width
 	AComposite contents ->
 		foldr comb (returnPs id) (map (psShowsContainer ax1 ay1) contents)
      where
        ax1 = ax + rx
        ay1 = ay + ry


-- showsPsColor color =	showString " col" . showsColor color


showsSetlinewidth lineWidth = showsPsNum lineWidth . showString " slw"
 
showsMoveto x y	=	showsPsXY x y . showString " m"
 
showsLineto x y =	showsPsXY x y . showString " l"
 
showsArcto x1 y1 x2 y2 r = showsPsXY x1 y1 . showsPsXY x2 y2 . showsPsNum r .
 			   showString " apr\n"
 
showsRMoveto x y =	showsPsXY x y . showString " rm"
 
showsRLineto x y =	showsPsXY x y . showString " rl"
 
showsPsXY x y =		showsPsNum x . showsPsNum y
 
showsPsNum :: Int -> ShowS
showsPsNum x =		showChar ' ' . shows x100 .
 			if x99 == 0 then id
 			else showChar '.' . shows x1 . shows x2
 			where (x100,x99) = x `divMod` 100
 			      (x1,x2) = x99 `divMod` 10
 
psFloor, psCeil :: Int -> Int
psFloor x = 100 * (x `div` 100)
psCeil  x = 100 * ((x + 99) `div` 100)

-- showsPsInt :: Int -> showS
-- showsPsInt x = showChar ' ' . showInt (x `div` 100)
	
psString "" = ""
psString ('(':cs) = "\\("   ++ psString cs
psString (')':cs) = "\\)"   ++ psString cs
psString ('\\':cs)= "\\\\"  ++ psString cs
psString ('-':cs) = "\\261" ++ psString cs		    -- endash looks much nicer
psString (c:cs)   = c:psString cs

-- Box:		width height linewidth Box -> -
-- draw box at current point

psProlog :: [String]
psProlog = [
 	      "/$Ebnf2psDict 100 dict def",
 	      "$Ebnf2psDict begin",
 	      "/l {lineto} bind def",
 	      "/m {moveto} bind def",
 	      "/rl {rlineto} bind def",
 	      "/rm {rmoveto} bind def",
 	      "/s {stroke} bind def",
 	      "/n {newpath} bind def",
 	      "/gs {gsave} bind def",
 	      "/gr {grestore} bind def",
 	      "/clp {closepath} bind def",
 	      "/slw {setlinewidth} bind def",
 	      "/graycol {dup dup currentrgbcolor 4 -2 roll mul 4 -2 roll mul",
 	      "4 -2 roll mul setrgbcolor} bind def",
 	      "/scol {3 {255 div 3 1 roll} repeat setrgbcolor} bind def",
 	      " /apr {arcto 4 {pop} repeat} def",
 	      "/Box {",
 	      "  /bgB exch def",
 	      "  /bgG exch def",
 	      "  /bgR exch def",
 	      "  /linewidth exch def",
 	      "  /height exch def",
 	      "  /width  exch def",
 	      "  /lly exch def",
 	      "  /llx exch def",
 	      "  linewidth 2 div dup llx add /llx exch def lly add /lly exch def",
 	      "  /height height linewidth sub def",
 	      "  /width width linewidth sub def",
 	      "  /linewidth0 {clp gsave bgR bgG bgB scol fill grestore} def",
 	      "  /linewidth1 {clp linewidth gsave bgR bgG bgB scol fill grestore slw s} def",
 	      "   n llx lly m",
 	      "  width 0 rl",
 	      "  0 height rl",
 	      "  width neg 0 rl",
 	      "  0 height neg rl",
 	      "  linewidth 0 eq {linewidth0} {linewidth1} ifelse",
 	      "} def",
 	      "/RBox {",
 	      "  /bgB exch def",
 	      "  /bgG exch def",
 	      "  /bgR exch def",
 	      "  /linewidth exch def",
 	      "  /height exch def",
 	      "  /width exch def",
 	      "  /lly exch def",
 	      "  /llx exch def",
 	      "  linewidth 2 div dup llx add /llx exch def lly add /lly exch def",
 	      "  /height height linewidth sub def",
 	      "  /width  width  linewidth sub def",
 	      "  /height2 height 2 div def",
 	      "  /width2  width  2 div def",
 	      "  /urx llx width add def",
 	      "  /ury lly height add def",
 	      "  /mmx llx width2 add def",
 	      "  /mmy lly height2 add def",
 	      "  /linewidth0 {clp gsave bgR bgG bgB scol fill grestore} def",
 	      "  /linewidth1 {clp linewidth gsave bgR bgG bgB scol fill grestore slw s} def",
 	      "  /radius width2 height2 ge {height2} {width2} ifelse def",
 	      "   n mmx lly m",
 	      "  urx lly urx mmy radius apr",
 	      "  urx ury mmx ury radius apr",
 	      "  llx ury llx mmy radius apr",
 	      "  llx lly mmx lly radius apr",
 	      "  mmx lly l",
 	      "  linewidth 0 eq {linewidth0} {linewidth1} ifelse",
 	      "} def",
 	      "end",
 	      "/$Ebnf2psBegin {$Ebnf2psDict begin /$Ebnf2psEnteredState save def} def",
 	      "/$Ebnf2psEnd {$Ebnf2psEnteredState restore end} def\n"]

{-
NOTE: '-cpp is not friendly to "string gaps"' 

psProlog :: String
psProlog = "\
\/$Ebnf2psDict 100 dict def\n\
\$Ebnf2psDict begin\n\
\/l {lineto} bind def\n\
\/m {moveto} bind def\n\
\/rl {rlineto} bind def\n\
\/rm {rmoveto} bind def\n\
\/s {stroke} bind def\n\
\/n {newpath} bind def\n\
\/gs {gsave} bind def\n\
\/gr {grestore} bind def\n\
\/clp {closepath} bind def\n\
\/slw {setlinewidth} bind def\n\
\/graycol {dup dup currentrgbcolor 4 -2 roll mul 4 -2 roll mul\n\
\4 -2 roll mul setrgbcolor} bind def\n\
\/scol {3 {255 div 3 1 roll} repeat setrgbcolor} bind def\n\
\ \
\/apr {arcto 4 {pop} repeat} def\n\
\/Box {\n\
\  /bgB exch def\n\
\  /bgG exch def\n\
\  /bgR exch def\n\
\  /linewidth exch def\n\
\  /height exch def\n\
\  /width  exch def\n\
\  /lly exch def\n\
\  /llx exch def\n\
\  linewidth 2 div dup llx add /llx exch def lly add /lly exch def\n\
\  /height height linewidth sub def\n\
\  /width width linewidth sub def\n\
\  /linewidth0 {clp gsave bgR bgG bgB scol fill grestore} def\n\
\  /linewidth1 {clp linewidth gsave bgR bgG bgB scol fill grestore slw s} def\n\
\ \
\  n llx lly m\n\
\  width 0 rl\n\
\  0 height rl\n\
\  width neg 0 rl\n\
\  0 height neg rl\n\
\  linewidth 0 eq {linewidth0} {linewidth1} ifelse\n\
\} def\n\
\ \
\/RBox {\n\
\  /bgB exch def\n\
\  /bgG exch def\n\
\  /bgR exch def\n\
\  /linewidth exch def\n\
\  /height exch def\n\
\  /width exch def\n\
\  /lly exch def\n\
\  /llx exch def\n\
\  linewidth 2 div dup llx add /llx exch def lly add /lly exch def\n\
\  /height height linewidth sub def\n\
\  /width  width  linewidth sub def\n\
\  /height2 height 2 div def\n\
\  /width2  width  2 div def\n\
\  /urx llx width add def\n\
\  /ury lly height add def\n\
\  /mmx llx width2 add def\n\
\  /mmy lly height2 add def\n\
\  /linewidth0 {clp gsave bgR bgG bgB scol fill grestore} def\n\
\  /linewidth1 {clp linewidth gsave bgR bgG bgB scol fill grestore slw s} def\n\
\  /radius width2 height2 ge {height2} {width2} ifelse def\n\
\ \
\  n mmx lly m\n\
\  urx lly urx mmy radius apr\n\
\  urx ury mmx ury radius apr\n\
\  llx ury llx mmy radius apr\n\
\  llx lly mmx lly radius apr\n\
\  mmx lly l\n\
\  linewidth 0 eq {linewidth0} {linewidth1} ifelse\n\
\} def\n\
\end\n\
\/$Ebnf2psBegin {$Ebnf2psDict begin /$Ebnf2psEnteredState save def} def\n\
\/$Ebnf2psEnd {$Ebnf2psEnteredState restore end} def\n\
\\n"
-}

