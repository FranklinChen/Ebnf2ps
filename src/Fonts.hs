--------------------------------------------------------------------------------
-- Copyright 1994 by Peter Thiemann
-- $Log: Fonts.hs,v $
-- Revision 1.1.1.1  1998/12/09 13:34:08  pjt
-- Imported sources
--
-- Revision 1.1  1993/08/31  12:31:32  thiemann
-- Initial revision
--
-- Revision 1.1  1993/08/31  12:31:32  thiemann
-- Initial revision
--
-- $Locker:  $
-- Last Modified By: M. Walter
--------------------------------------------------------------------------------

module Fonts (FONT, makeFont, fontDescender, stringWidth, stringHeight, fontName, fontScale, noFont)
where

import Data.Char
import Numeric

data FONT = FONT String Int Int (String -> Int)

instance Eq FONT where
  (==) (FONT s1 m1 n1 f1) (FONT s2 m2 n2 f2) = s1 == s2 && m1 == m2 && n1 == n2

noFont = FONT "" 0 0 (const 0)

data Afm = Descender Int
	 | CharMetric Int    Int    String   Int Int Int Int
--	   CharMetric charNo charWX charName llx lly urx ury
--	 deriving Text

fontName :: FONT -> String
fontName (FONT name _ _ _) = name

fontScale :: FONT -> Int
fontScale (FONT _ scale _ _) = scale

fontDescender :: FONT -> Int
fontDescender (FONT _ _ theDescender _) = theDescender

stringWidth :: FONT -> String -> Int
stringWidth (FONT _ _ _ theStringWidth) = theStringWidth

stringHeight :: FONT -> String -> Int
stringHeight (FONT _ scale _ _) _ = scale * 100

makeFont :: String -> Int -> String -> FONT
makeFont fontName fontScale fontAfm =
	FONT fontName fontScale theDescender
	((`div` 10). (* fontScale). getStringWidth parsedAfm)
    where
	parsedAfm = parseAfmFile (lines fontAfm)
	theDescender = getDescender parsedAfm

getStringWidth :: [Afm] -> String -> Int
getStringWidth afms str = sum (map (getCharWidth afms . ord) str)

getCharWidth :: [Afm] -> Int -> Int
getCharWidth (CharMetric charNo charWX charName llx lly urx ury: afms) chNo
	| charNo == chNo = charWX
	| otherwise      = getCharWidth afms chNo
getCharWidth (_:afms) chNo = getCharWidth afms chNo
getCharWidth [] chNo = 0

getDescender :: [Afm] -> Int
getDescender (Descender d: _) = d
getDescender (_:rest) = getDescender rest
getDescender [] = 0

--------------------------------------------------------------------------------

parseAfmFile :: [String] -> [Afm]
parseAfmFile [] = []
parseAfmFile (('D':'e':'s':'c':'e':'n':'d':'e':'r':line):lines) =
	Descender descender: parseAfmFile lines
	where (descender,_):_ = readSigned readDec (skipWhite line)
parseAfmFile (('E':'n':'d':'C':'h':'a':'r':'M':'e':'t':'r':'i':'c':'s':_):_) = []
parseAfmFile (('C':' ':line):lines) = CharMetric charNo charWX charName llx lly urx ury:
				  parseAfmFile lines
	where	(charNo, rest1):_ = readSigned readDec (skipWhite line)
		'W':'X':rest2	  = skipWhiteOrSemi rest1
		(charWX, rest3):_ = readDec (skipWhite rest2)
		'N':rest4	  = skipWhiteOrSemi rest3
		(charName, rest5) = span isAlpha (skipWhite rest4)
		'B':rest6	  = skipWhiteOrSemi rest5
		(llx, rest7):_	  = readSigned readDec (skipWhite rest6)
		(lly, rest8):_	  = readSigned readDec (skipWhite rest7)
		(urx, rest9):_	  = readSigned readDec (skipWhite rest8)
		(ury, _):_	  = readSigned readDec (skipWhite rest9)
parseAfmFile (_:lines) = parseAfmFile lines

skipWhite = dropWhile isSpace
skipWhiteOrSemi = dropWhile isSkipChar
isSkipChar c = isSpace c || c == ';'
