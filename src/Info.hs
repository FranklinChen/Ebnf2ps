--                            -*- Mode: Haskell -*- 
-- Copyright 1994 by Peter Thiemann
--
-- just a provider of type declarations
-- 
-- $Locker:  $
-- $Log: Info.hs,v $
-- Revision 1.1.1.1  1998/12/09 13:34:08  pjt
-- Imported sources
--
-- Revision 1.1  1994/03/15  15:34:53  thiemann
-- Initial revision
--
-- Last Modified By: M. Walter
--


module Info where

import Color	(Color)
import Fonts	(FONT)

type ColorInfo = (Color, Color, Color, Color, Color, Color, Color, Color)
type INFO =  (Int, Int, Int, Int, Int, FONT, FONT, FONT, ColorInfo)
type WrapperType = String -> INFO -> Container -> ShowS

type Container = (
--      rx, ry		origin relative to bounding box
	Int, Int,
--	widht height	of content's bounding box
	Int, Int,
--	inOutY		relative position of entry/exit point
	Int,
        GObject)

data GObject
	= AString Color FONT String			    -- AString font theString
	| ABox Color Color Bool Container		    -- round edges toggle
	| Arrow Color Int				    -- Arrow size
	| Aline Color					    -- width height rlineto
	| ATurn Color TDirection			    -- a turn
	| AComposite [Container]


data TDirection = SE | SW | WN | NE

