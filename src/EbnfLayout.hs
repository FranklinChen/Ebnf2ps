--------------------------------------------------
-- Copyright 1994 by Peter Thiemann
-- $Log: EbnfLayout.hs,v $
-- Revision 1.1.1.1  1998/12/09 13:34:08  pjt
-- Imported sources
--
-- Revision 1.4  1994/03/15  15:34:53  thiemann
-- added full color support, XColorDB based
--
-- Revision 1.3  1994/02/18  11:59:29  thiemann
-- save state before adding "withTentacle"
--
-- Revision 1.2  1993/08/31  12:31:32  thiemann
-- reflect changes in type FONT
--
-- Revision 1.1  1993/08/17  12:34:29  thiemann
-- Initial revision
--
-- $Locker:  $
-- Last Modified By: M. Walter
--------------------------------------------------
module EbnfLayout where

import AbstractSyntax
import Color
import Fonts (FONT, stringWidth, stringHeight, fontDescender)
import Info
import Data.List

-- all arithmetic is done in 1/100 pt

-- tFont, ntFont, titleFont :: Font
-- arrowSize, lineWidth, fatLineWidth, borderDistY, borderDistX :: Int

-- borderDistX = 500
-- borderDistY = 500
-- lineWidth   = 20
-- fatLineWidth = 200
-- arrowSize   = 300
-- titleFont   = ("Times-Roman", 10)
-- ntFont      = ("Times-Roman", 10)
-- tFont       = ("Times-Roman", 10)

makePictureLayout :: INFO -> Production -> Container
makePictureLayout
    (borderDistX, borderDistY, lineWidth, fatLineWidth, arrowSize, titleFont, ntFont, tFont,
	(ntColor, tColor, lineColor, ntBoxColor, tBoxColor, ntBgColor, tBgColor, titleClr))
    prod
    = makePicture 0 0 1 prod
	where

  mkTitleNonTerminal :: String -> Int -> Int -> Container
  mkTitleNonTerminal str rx ry = (rx, ry, width, height, 0, AString titleClr titleFont str)
	where
	      width  = stringWidth  titleFont str
	      height = stringHeight titleFont str

  mkNonTerminal :: String -> Int -> Int -> Container
  mkNonTerminal str rx ry = (rx, ry, width, height, 0, AString ntColor ntFont str)
	where
	      width  = stringWidth  ntFont str
	      height = stringHeight ntFont str

  mkTerminal :: String -> Int -> Int -> Container
  mkTerminal str rx ry = (rx, ry, width, height, 0, AString tColor tFont str)
	where
	      width  = stringWidth  tFont str
	      height = stringHeight tFont str

  mkBox :: Int -> Int -> Int -> Int -> Int -> Container -> Container
  mkBox rx ry width height inOutY content
    = (rx, ry, width, height, inOutY, ABox ntBoxColor ntBgColor False content)

  mkRoundBox :: Int -> Int -> Int -> Int -> Int -> Container -> Container
  mkRoundBox rx ry width height inOutY content
    = (rx, ry, width, height, inOutY, ABox tBoxColor tBgColor True content)

  mkLine ::  Int -> Int -> Int -> Int -> Container
  mkLine rx ry w h = (rx, ry, w, h, 0, Aline lineColor)

  mkArrow :: Int -> Int -> Int -> Container
  mkArrow rx ry dir = (rx, ry, 0, 0, 0, Arrow lineColor (dir*arrowSize))

  mkTurn :: Int -> Int -> Int -> Int -> TDirection -> Container
  mkTurn rx ry w h t = (rx, ry, w, h, 0, ATurn lineColor t)

  ------------------------------------------------------------------------

  withTentacle :: Int -> Int -> Int -> Production -> Container
  withTentacle rx ry direction prod =
	(rx, ry, width, height, inOutY, AComposite [contents, theLine, theArrow])
	where (_, _, width1, height, inOutY, _) = contents
	      contents = makePicture rx1 0 direction prod
	      width = width1 + borderDistX
	      rx1      | direction > 0 = 0
                       | otherwise     = borderDistX
	      theLine  | direction > 0 = mkLine  width1 inOutY borderDistX 0
	               | otherwise     = mkLine  0      inOutY borderDistX 0
	      theArrow | direction > 0 = mkArrow width  inOutY direction
                       | otherwise     = mkArrow 0      inOutY direction

  makePicture :: Int -> Int -> Int -> Production -> Container

  makePicture rx ry direction (ProdProduction ntName ntAliases ProdEmpty) =
	(rx, ry, width, height, 0, AComposite [content])
	where content@(_,_, width2, height2,_,_) = mkTitleNonTerminal str rx2 ry2
	      ry1 = fatLineWidth `div` 2
	      rx2 = 0
	      ry2 = ry1 + distance - fontDescender ntFont
	      distance = 2*borderDistY
	      width = 2*borderDistX + width2
	      height = fatLineWidth + height2 + distance
	      str  = (case ntAliases of
			[] -> ntName
			newName:_ -> newName)++"\t<empty production>"

  makePicture rx ry direction (ProdProduction ntName ntAliases prod) =
	(rx, ry, width, height, 0, AComposite ([content1, content2]++glue))
	where (_, _, width1, height1, inOutY1, _) = content1
	      content1 = withTentacle rx1 ry1 direction prod
	      content2@(_,_, width2, height2,_,_) = mkTitleNonTerminal str rx2 ry2
	      rx1 = 2*borderDistX
	      ry1 = fatLineWidth `div` 2
	      rx2 = 0
	      ry2 = ry1 + height1 + distance - fontDescender ntFont
	      distance = 2*borderDistY
	      width = 2*borderDistX + max width1 width2
	      height = height1 +fatLineWidth + height2 + distance
	      glue = [
		mkLine 0 (ry1 + inOutY1) (2*borderDistX) 0,
		mkArrow rx1 (ry1 + inOutY1) direction]
	      str = case ntAliases of
			[] -> ntName
			newName:_ -> newName

  makePicture rx ry direction (ProdTerm [prod]) =
	makePicture rx ry direction prod
  makePicture rx ry direction (ProdTerm prods) =
	(rx, ry, width, height, inOutY, AComposite (newcontents ++ glue))
	where newcontents = zip6 rxs rys widths heights inOutYs gobjs
	      (_, _, widths, heights, inOutYs, gobjs) = unzip6 contents
	      ncontents = length prods
	      -- sadly enough it's not possible to take rxs and rys in place of the fakes!
	      fakes = take ncontents (repeat 0)
	      contents = zipWith4 makePicture fakes fakes directions prods
	      height = sum heights + (ncontents-1) * borderDistY
	      maxwidth = maximum widths
	      width = maxwidth + 4 * borderDistX
	      rxs | direction > 0 = take ncontents (repeat (2 * borderDistX))
		  | otherwise     = map ((+ (2*borderDistX)) . (maxwidth -)) widths
	      rys = tail (scanr f 0 heights) where f h q = h + q + borderDistY
              directions = take ncontents (repeat direction)
	      entries = zipWith (+) rys inOutYs		    -- frame relative Y positions of entries
	      firstEntry = entries!!0
	      lastEntry = entries!!(ncontents-1)
	      middleEntries = init (tail entries)
	      inOutY = (firstEntry + lastEntry) `div` 2
	      inOutDiff = firstEntry - lastEntry - 2*borderDistY
	      glue = fixedglue ++ variableglue
	      fixedglue = [
		mkLine 0 inOutY borderDistX 0,
		mkLine (width-borderDistX) inOutY borderDistX 0,
		mkTurn borderDistX (firstEntry - borderDistY) borderDistX borderDistY SE,
		mkTurn borderDistX lastEntry   borderDistX borderDistY NE,
		mkLine borderDistX (lastEntry + borderDistY) 0 inOutDiff,
		mkTurn (width-2*borderDistX) (firstEntry - borderDistY) borderDistX borderDistY SW,
		mkTurn (width-2*borderDistX) lastEntry borderDistX borderDistY WN,
		mkLine (width-borderDistX) (lastEntry + borderDistY) 0 inOutDiff] ++
		map f middleEntries ++
		map g middleEntries
			where f y = mkLine borderDistX           y borderDistX 0
			      g y = mkLine (width-2*borderDistX) y borderDistX 0
	      variableglue | direction > 0 = zipWith g widths entries
			   | otherwise     = zipWith h widths entries
			where g w y = mkLine (2*borderDistX + w) y (maxwidth - w) 0
			      h w y = mkLine (2*borderDistX)     y (maxwidth - w) 0
	      obsoleteglue =
			map (f (2*borderDistX)) entries
			where f x y = mkArrow x y direction

{- the following works for two terms, both directions
  makePicture rx ry direction (ProdTerm [prod1, prod2])
    | direction > 0 =
	let   (_, _, width1, height1, inOutY1, _) = content1
	      content1 = makePicture rx1 ry1 direction prod1
	      (_, _, width2, height2, inOutY2, _) = content2
	      content2 = makePicture rx2 ry2 direction prod2
	      rx1 = 2*borderDistX
	      rx2 = 2*borderDistX
	      ry2 = 0
	      ry1 = height2 + borderDistY
	      maxwidth = max width1 width2
	      width = 4*borderDistX + maxwidth
	      height = height2 + borderDistY + height1
	      inOutY = (inOutY2 + ry1 + inOutY1) `div` 2
	      inOutDiff = ry1 + inOutY1 - inOutY2 - 2*borderDistY
	      glue = [
		mkLine 0 inOutY borderDistX 0,
		mkLine (width - borderDistX) inOutY borderDistX 0,
		mkLine borderDistX (ry2 + inOutY2 + borderDistY) 0 inOutDiff,
		mkLine (width - borderDistX) (ry2 + inOutY2 + borderDistY) 0 inOutDiff,
		mkTurn borderDistX (ry1 + inOutY1 - borderDistX) borderDistX borderDistY SE,
		mkTurn borderDistX inOutY2 borderDistX borderDistY NE,
		mkTurn (width - 2*borderDistX) (ry1 + inOutY1 - borderDistX) borderDistX borderDistY SW,
		mkTurn (width - 2*borderDistX) inOutY2 borderDistX borderDistY WN,
		mkLine (rx1 + width1) (ry1 + inOutY1) (maxwidth - width1) 0,
		mkLine (rx2 + width2) (ry2 + inOutY2) (maxwidth - width2) 0]
	in      (rx, ry, width, height, inOutY, AComposite ([content1,content2]++glue))
    | otherwise =
	let   (_, _, width1, height1, inOutY1, _) = content1
	      content1 = makePicture rx1 ry1 direction prod1
	      (_, _, width2, height2, inOutY2, _) = content2
	      content2 = makePicture rx2 ry2 direction prod2
	      maxwidth = max width1 width2
	      width = 4*borderDistX + maxwidth
	      height = height2 + borderDistY + height1
	      inOutY = (inOutY2 + ry1 + inOutY1) `div` 2
	      inOutDiff = ry1 + inOutY1 - inOutY2 - 2*borderDistY
	      rx1 = 2*borderDistX + (maxwidth - width1)
	      rx2 = 2*borderDistX + (maxwidth - width2)
	      ry2 = 0
	      ry1 = height2 + borderDistY
	      glue = [
		mkLine 0 inOutY borderDistX 0,
		mkLine (width - borderDistX) inOutY borderDistX 0,
		mkLine borderDistX (ry2 + inOutY2 + borderDistY) 0 inOutDiff,
		mkLine (width - borderDistX) (ry2 + inOutY2 + borderDistY) 0 inOutDiff,
		mkTurn borderDistX (ry1 + inOutY1 - borderDistX) borderDistX borderDistY SE,
		mkTurn borderDistX inOutY2 borderDistX borderDistY NE,
		mkTurn (width - 2*borderDistX) (ry1 + inOutY1 - borderDistX) borderDistX borderDistY SW,
		mkTurn (width - 2*borderDistX) inOutY2 borderDistX borderDistY WN,
		mkLine (2*borderDistX) (ry1 + inOutY1) (maxwidth - width1) 0,
		mkLine (2*borderDistX) (ry2 + inOutY2) (maxwidth - width2) 0]
	in      (rx, ry, width, height, inOutY, AComposite ([content1,content2]++glue))
-}

  makePicture rx ry direction (ProdFactor [prod]) =
	makePicture rx ry direction prod
{-
  makePicture rx ry direction (ProdFactor prods) =
	(rx, ry, width, height, inOutY, AComposite (glue++contents))
	where (_, _, widths, heights, inOutYs, gobjs) = unzip6 contents
	      contents = zipWith4 makePicture rxs rys directions prods
	      ncontents = length prods
	      aboves = zipWith (-) heights inOutYs
	      maxIO = maximum inOutYs
	      height = maxIO + maximum aboves
	      width = sum widths + (ncontents-1)*borderDistX
	      inOutY = maxIO
	      rxs
		  -- = take ncontents [0, 20*borderDistX .. ]
		  | direction > 0 = init (scanl f 0 widths)
	          | otherwise     = tail (scanr f 0 widths)
		       where f q w = q + w + borderDistX

	      rys =
			take ncontents (repeat 0)
	      		-- map (inOutY -) inOutYs
	      directions = take ncontents (repeat direction)
	      glue | direction > 0 = map f (tail rxs)
		   | otherwise     = map f (init rxs)
			where  f x = mkLine (x-borderDistX) inOutY borderDistX 0
-}

  makePicture rx ry direction (ProdFactor [prod1,prod2])
    | direction > 0 =
	let   (_, _, width1, height1, inOutY1, _) = content1
	      content1 = withTentacle rx1 ry1 direction prod1
	      (_, _, width2, height2, inOutY2, _) = content2
	      content2 = makePicture rx2 ry2 direction prod2
	      rx1 = 0
	      rx2 = width1
	      width = width1 + width2
	      inOutY = max inOutY1 inOutY2
	      ry1 = inOutY - inOutY1
	      ry2 = inOutY - inOutY2
	      height = inOutY + max (height1 - inOutY1) (height2 - inOutY2)
	in    (rx, ry, width, height, inOutY, AComposite ([content1,content2]))
    | otherwise =
	let   (_, _, width1, height1, inOutY1, _) = content1
	      content1 = withTentacle rx1 ry1 direction prod1
	      (_, _, width2, height2, inOutY2, _) = content2
	      content2 = makePicture rx2 ry2 direction prod2
	      rx2 = 0
	      rx1 = width2
	      width = width1 + width2
	      inOutY = max inOutY1 inOutY2
	      ry1 = inOutY - inOutY1
	      ry2 = inOutY - inOutY2
	      height = inOutY + max (height1 - inOutY1) (height2 - inOutY2)
	in    (rx, ry, width, height, inOutY, AComposite ([content1,content2]))

  makePicture rx ry direction (ProdFactor (prod:prods)) =
	makePicture rx ry direction (ProdFactor [prod, ProdFactor prods])
-- this is a ghastly hack!

  makePicture rx ry direction (ProdNonterminal str) =
	mkBox rx ry width height inOutY content
	where content@(_,_,width', height',_,_) = mkNonTerminal str rx' ry'
	      width   = width' + 2*borderDistX + 2*fatLineWidth
	      height  = height' + borderDistY + 2*fatLineWidth
	      rx'     = fatLineWidth + borderDistX
	      ry'     = fatLineWidth + borderDistY `div` 2 - fontDescender ntFont
	      inOutY  = height `div` 2

  makePicture rx ry direction (ProdTerminal str) =
	mkRoundBox rx ry width height inOutY content
	where content@(_,_,width', height',_,_) = mkTerminal str rx' ry'
	      width   = width' + 2*borderDistX + 2*fatLineWidth
	      height  = height' + borderDistY + 2*fatLineWidth
	      rx'     = fatLineWidth + borderDistX
	      ry'     = fatLineWidth + borderDistY `div` 2 - fontDescender tFont
	      inOutY  = height `div` 2

  makePicture rx ry direction (ProdOption prod) =
	(rx, ry, width, height, inOutY, AComposite (content:glue))
	where (_, _, width', height', inOutY', gobj) = content
	      content = makePicture rx' ry' direction prod
	      width = width' + 6*borderDistX
	      height = height' + borderDistY
	      rx' = 3*borderDistX
	      ry' = borderDistY
	      inOutY = 0
	      glue = variableglue ++ fixedglue
	      fixedglue = [
		mkLine 0 0 width 0,
	        mkTurn 0 0 borderDistX bby WN,
	        mkTurn borderDistX (inOutY'+borderDistY-bby) borderDistX bby SE,
		mkLine (2*borderDistX) (ry'+inOutY') borderDistX 0,
	        mkTurn (width-borderDistX) 0 borderDistX bby NE,
	        mkTurn (width-2*borderDistX) (inOutY'+borderDistY-bby) borderDistX bby SW,
		mkLine (width-3*borderDistX) (ry'+inOutY') borderDistX 0,
	        mkLine borderDistX         bby 0 (inOutY'+borderDistY-2*bby),
	        mkLine (width-borderDistX) bby 0 (inOutY'+borderDistY-2*bby)]
		where bby = min borderDistY ((inOutY'+borderDistY) `div` 2)
	      variableglue
		| direction > 0 = [mkArrow (3*borderDistX) (ry'+inOutY') direction]
		| otherwise     = [mkArrow (width-3*borderDistX) (ry'+inOutY') direction]

  makePicture rx ry direction (ProdRepeat prod) =
	(rx, ry, width, height, inOutY, AComposite (content:glue))
	where (_, _, width', height', inOutY', gobj) = content
	      content = makePicture rx' ry' (-direction) prod
	      width = width' + 4*borderDistX
	      height = height' + borderDistY
	      rx' = 2*borderDistX
	      ry' = borderDistY
	      inOutY = 0
	      glue = variableglue ++ fixedglue
	      fixedglue = [
		mkLine 0 0 width 0,
	        mkTurn borderDistX 0 borderDistX bby NE,
	        mkTurn borderDistX (inOutY'+borderDistY-bby) borderDistX bby SE,
	        mkTurn (width-2*borderDistX) 0 borderDistX bby WN,
	        mkTurn (width-2*borderDistX) (inOutY'+borderDistY-bby) borderDistX bby SW,
	        mkLine borderDistX     bby 0 (inOutY'+borderDistY-2*bby),
	        mkLine (width-borderDistX) bby 0 (inOutY'+borderDistY-2*bby)]
		where bby = min borderDistY ((inOutY'+borderDistY) `div` 2)
	      variableglue
		| direction < 0 = [mkArrow (2*borderDistX) (inOutY'+borderDistY) (-direction)]
		| otherwise     = [mkArrow (width-2*borderDistX) (inOutY'+borderDistY) (-direction)]

  makePicture rx ry direction (ProdRepeat1 prod) =
	(rx, ry, width, height, inOutY, AComposite (content:glue))
	where (_, _, width', height', inOutY', gobj) = content
	      content = makePicture rx' ry' (direction) prod
	      width = width' + 4*borderDistX
	      height = height' + borderDistY
	      rx' = 2*borderDistX
	      ry' = 0
	      inOutY = inOutY'
	      glue = [
		mkLine 0 inOutY rx' 0,
		mkLine (rx'+width') inOutY rx' 0,
		mkTurn borderDistX inOutY borderDistX borderDistY NE,
		mkTurn borderDistX (height-borderDistY) borderDistX borderDistY SE,
		mkTurn (width-rx') inOutY borderDistX borderDistY WN,
		mkTurn (width-rx') (height-borderDistY) borderDistX borderDistY SW,
		mkLine borderDistX (inOutY+borderDistY) 0 (height'-inOutY'-borderDistY),
		mkLine (width-borderDistX) (inOutY+borderDistY) 0 (height'-inOutY'-borderDistY),
		mkLine rx' height width' 0,
		mkArrow (rx'+width' `div` 2) height (-direction)]

  makePicture rx ry direction (ProdRepeatWithAtom prod1 prod2) =
	(rx, ry, width, height, inOutY, AComposite (content1:content2:glue))
	where (_, _, width1, height1, inOutY1, _) = content1
	      (_, _, width2, height2, inOutY2, _) = content2
	      content1 = makePicture rx1 ry1 direction prod1
	      content2 = makePicture rx2 ry2 (-direction) prod2
	      maxwidth = max width1 width2
	      width = maxwidth + 4*borderDistX
	      height = height1 + height2 + borderDistY
	      adjx1 = (maxwidth - width1) `div` 2
	      rx1 = 2*borderDistX + adjx1
	      ry1 = 0
	      adjx2 = (maxwidth - width2) `div` 2
	      rx2 = 2*borderDistX + adjx2
	      ry2 = height1 + borderDistY
	      inOutY = inOutY1
	      glue = variableglue ++ fixedglue
	      fixedglue = [
		mkLine 0 inOutY rx1 0,
	        mkLine (rx1 + width1) inOutY rx1 0,
	        mkLine (2*borderDistX) (ry2+inOutY2) adjx2 0,
	        mkLine (2*borderDistX + adjx2 + width2) (ry2+inOutY2) adjx2 0,
	        mkTurn borderDistX inOutY borderDistX borderDistY NE,
	        mkTurn borderDistX (ry2+inOutY2-borderDistY) borderDistX borderDistY SE,
	        mkTurn (rx1+width1+adjx1) inOutY borderDistX borderDistY WN,
	        mkTurn (rx1+width1+adjx1) (ry2+inOutY2-borderDistY) borderDistX borderDistY SW,
	        mkLine borderDistX (inOutY+borderDistY) 0 (height1-inOutY1 + inOutY2 - borderDistY),
	        mkLine (rx1+width1+adjx1+borderDistX) (inOutY+borderDistY) 0 (height1-inOutY1 + inOutY2 - borderDistY)]
	      variableglue
		| direction > 0 = [mkArrow (2*borderDistX + adjx2 + width2) (ry2+inOutY2) (-direction)]
		| otherwise     = [mkArrow (2*borderDistX + adjx2) (ry2+inOutY2) (-direction)]
