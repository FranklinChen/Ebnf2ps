--                            -*- Mode: Haskell -*- 
-- Copyright 1996 by Peter Thiemann
-- Last Modified By: M. Walter
-- $Log: Fig31Output.hs,v $
-- Revision 1.1.1.1  1998/12/09 13:34:08  pjt
-- Imported sources
--
-- added full color support, XColorDB based
-- made conformant with FIG format 3.1 
--

module Fig31Output (fig31ShowsWrapper) where

import Version
import Fonts (FONT, fontName, fontScale)
import Color
import Info

--------------------------------------------------------------------------------
fig31ShowsWrapper :: WrapperType
fig31ShowsWrapper title
         (borderDistX, borderDistY, lineWidth, fatLineWidth, arrowSize, titleFont, ntFont, tFont, 
	  colorinfo@(c1,c2,c3,c4,c5,c6,c7,c8)) container@(rx, ry, width, height, inOutY, gobj) =
	showString "#FIG 3.1\n" .
	showString "Landscape\nCenter\nInches\n1200 2\n"  .
	showString "#Creator: Ebnf2ps (Copyright 1996 by Peter Thiemann / Michael Walter)\n" .
	showString ("#Version: " ++ version ++ "\n") .
	fig31ShowsObjectDefinition colorinfo .
	fig31ShowsContainer rx height container
   where 
   fig31ShowsObjectDefinition cinfo@(c1,c2,c3,c4,c5,c6,c7,c8) = 
        showsRGBColorObject cinfo 
   fig31ShowsContainer ax ay (rx, ry, width, height, inOutY, gobj) =
        case gobj of
        AString color font theString -> 
	        showString "4 " .				-- object: TEXT <4>
	        showString "0" .				-- sub_type : Left justified <0>
	        showsfigColor color colorinfo .			-- color 
                showString " 0 " .				-- depth: <0>
	        showString "0" .				-- pen_style (not used)
	        showsTrueNum (figFont (fontName font)) .	-- font 
 	        showsTrueNum (fontScale font) .	                -- font_size
	        showString " 0.0000000 " .			-- angle <0.00000>
	        showString "4".				        -- font_flags: Postscript Fonts <4>
	        showsFigUnit height .				-- height
     	        showsFigUnit width  .				-- length
        	showsFigUnit ax' .				-- x
                showsFigUnit ay' .				-- y
                showString (' ':figString theString)		-- string[]	
        ABox color bgcolor rounded content ->
	        fig31ShowsContainer ax' ay' content .
	        showString "2 " .				-- object: POLYLINE <2>
	        showsPolySubType rounded "4 " "2 "  .		-- subject type
       	        showString "0" . 				-- line_style
	        showsFigNum fatLineWidth .			-- thickness
	        showsfigColor color colorinfo .			-- pen_color
	        showsfigColor bgcolor colorinfo .		-- fill_color
	        showString " 0 " .				-- depth
	        showString "0 " .				-- pen_style
	        showString "20 " .				-- area_fill
	        showString "0.000 " .				-- style_val
	        showString "0 " .				-- join_style (2.1 default)
	        showString "0" .				-- cap_style (2.1 default)
	        (if rounded then showsFigNum (min height width `div` 2)
   	        else showsFigNum 0) .			        -- radius
	        showString " 0 0 " .				-- forward_arrow , backwar_arrow
	        showString "5\n" . 				-- npoints
	        showString "       " .
	        showsFigBox ax' ay' width height .
	        showString "\n"					-- END of object
        Arrow color size -> 
	        showString "2 " .				-- object: POLYLINE <2>
	        showString "1 " . 				-- sub_type <polyline>
	        showString "0" .				-- line_style <solid>
	        showsFigNum lineWidth . 			-- thickness
	        showsfigColor color colorinfo .			-- pen_color
	        showString " -1" .				-- fill_color
	        showString " 0 " .				-- depth
	        showString "0 " .				-- pen_style
	        showString "-1 " .				-- area_fill
	        showString "0.000 " .		 		-- style_val
 	        showString "0 " . 				-- join_style
	        showString "0" . 				-- cap_style
	        showsFigNum (-1)  . 				-- radius
	        showString " 1 0 " . 				-- forward_arrow, backward_arrow
	        showString "2\n"	.			-- npoints
	        showString "        0 0" . 		  	-- arrow_type, arrow_style
	        showsFigNum lineWidth . showString ".00 " .	-- arrow_thickness
	        showsFigUnit (abs size * 2). showString ".00 ". -- arrow_width
	        showsFigUnit (abs size * 2). showString ".00 ". -- arrow_height
	        showString "\n        " .
	        showsFigPoint (ax'- size) ay' .
	        showsFigPoint ax' ay' .
	        showString "\n"					-- END of object
        Aline color ->
   	        showString "2 " .				-- object: POLYLINE <2>
	        showString "1 " .				-- sub_type <polyline>
	        showString "0" .				-- line_style <solid>
   	        showsFigNum lineWidth .				-- thickness
	        showsfigColor color colorinfo .			-- pen_color
	        showString " -1" .				-- fill_color
	        showString " 0 " .				-- depth
	        showString "0 " .				-- pen_style 
	        showString "-1 " .				-- area_fill
	        showString "0.000 " . 				-- style_val
	        showString "0 " . 				-- join_style
	        showString "0" . 				-- cap_style (2.1 Butt)
	        showsFigNum (-1)  . 				-- radius
	        showString " 0 0 " . 				-- forward_arrow, backward_arrow
	        showString "2\n" .				-- npoints
	        showString "       " .
	        showsFigPoint ax' ay' .
	        showsFigPoint (ax'+width) (ay'-height) .
	        showString "\n"					-- END of object
        ATurn color dir ->
  	        showString "3 " . 				-- object: SPLINE <3>
	        showString "0 " .				-- sub_type <open spline>
	        showString "0" .				-- line_style
	        showsFigNum lineWidth . 			-- thickness
	        showsfigColor color colorinfo .			-- pen_color
	        showString " -1" .				-- fill_color  	
	        showString " 0 " .				-- depth
	        showString "0 " .				-- pen_style
	        showString "-1 " . 				-- area_fill
	        showString "0.000 " .				-- style_val
	        showString "0 " . 				-- cap_style (2.1 Butt)
	        showString "0 0 " .				-- forward_arrow,  backward_arrow
	        showString "3\n" . 				-- npoints
	        showString "       " .
	        showsIt dir . 
	        showString "\n"
	        where   showsIt SE =    showsFigPoint ax' ay' .
			                showsFigPoint ax' (ay'-height) .
		                        showsFigPoint (ax'+width) (ay'-height)
   	                showsIt WN =    showsFigPoint ax' ay' .
	  	                        showsFigPoint (ax'+width) ay' .
		                        showsFigPoint (ax'+width) (ay'-height)				
	                showsIt SW =    showsFigPoint (ax'+width) ay' .
	  	                        showsFigPoint (ax'+width) (ay'-height) .
		                        showsFigPoint ax' (ay'-height)
                        showsIt NE =    showsFigPoint ax' (ay'-height) .
	  	                        showsFigPoint ax' ay' .
		                        showsFigPoint (ax'+width) ay'		
        AComposite contents ->
   	        showString "6" .				-- object: COMPOUND
	        showsFigPoint (ax'+width) (ay'-height) .	-- upperright_corner_(x,y)
	        showsFigPoint ax' ay' .				-- lowerleft_corner_(x,y)
	        showChar '\n' .
	        foldr (.) (showString "-6\n") (map (fig31ShowsContainer ax' ay') contents)
    where       ax' = ax + rx 
   	        ay' = ay - ry

showsfigColor :: Color -> ColorInfo -> ShowS
showsfigColor c (c1,c2,c3,c4,c5,c6,c7,c8)
    | c `eqColor` c1 = showString " 33"
    | c `eqColor` c2 = showString " 34"
    | c `eqColor` c3 = showString " 35"
    | c `eqColor` c4 = showString " 36"
    | c `eqColor` c5 = showString " 37"
    | c `eqColor` c6 = showString " 38"
    | c `eqColor` c7 = showString " 39"
    | c `eqColor` c8 = showString " 40"
    | otherwise	 = showsFigColor c
		  where (r1,g1,b1) `eqColor` (r2,g2,b2) =  (r1==r2) && (g1==g2) && (b1==b2)
   
figString ['\\'] = "\\134\\001\n" 
figString cs = figString' cs
 	       where figString' "" = "\\001\n"
		     figString' ('\\':cs) = '\\':'\\': figString' cs
		     figString' (c:cs) = c:figString' cs

figFont name = lookup figFontList 0
    where
        lookup [] _ = -1
	lookup (font: fonts) n | font == name = n
			       | otherwise    = lookup fonts (n+1)


figFontList = [			   -- stolen from u_fonts.c
	"Times-Roman",
	"Times-Italic",
	"Times-Bold",
	"Times-BoldItalic",
	"AvantGarde-Book",
	"AvantGarde-BookOblique",
	"AvantGarde-Demi",
	"AvantGarde-DemiOblique",
	"Bookman-Light",
	"Bookman-LightItalic",
	"Bookman-Demi",
	"Bookman-DemiItalic",
	"Courier",
	"Courier-Oblique",
	"Courier-Bold",
	"Courier-BoldOblique",
	"Helvetica",
	"Helvetica-Oblique",
	"Helvetica-Bold",
	"Helvetica-BoldOblique",
	"Helvetica-Narrow",
	"Helvetica-Narrow-Oblique",
	"Helvetica-Narrow-Bold",
	"Helvetica-Narrow-BoldOblique",
	"NewCenturySchlbk-Roman",
	"NewCenturySchlbk-Italic",
	"NewCenturySchlbk-Bold",
	"NewCenturySchlbk-BoldItalic",
	"Palatino-Roman",
	"Palatino-Italic",
	"Palatino-Bold",
	"Palatino-BoldItalic",
	"Symbol",
	"ZapfChancery-MediumItalic",
	"ZapfDingbats"]


showsFigBox ax1 ay1 width height =
        showsFigPoint ax1  ay1 .				
	showsFigPoint (ax1+width) ay1 .
	showsFigPoint (ax1+width) (ay1-height) .
	showsFigPoint ax1 (ay1-height) .
	showsFigPoint ax1 ay1 

	      
showsRGBColorObject :: ColorInfo -> ShowS
showsRGBColorObject info@(c1,c2,c3,c4,c5,c6,c7,c8) =
	showsRGBObject 33 c1 . 
	showsRGBObject 34 c2 .
	showsRGBObject 35 c3 .
	showsRGBObject 36 c4 .
	showsRGBObject 37 c5 .
	showsRGBObject 38 c6 .
	showsRGBObject 39 c7 .
	showsRGBObject 40 c8

showsRGBObject :: Int -> Color -> ShowS
showsRGBObject cno (r,g,b) = 
	showString "0 " . shows cno . showString " #" . 
	showsRGB cno r . showsRGB cno g .  showsRGB cno b .
	showString "\n"

showsRGB :: Int -> Int -> ShowS
showsRGB cno 0 = showString "00" 
showsRGB cno n = let (d1,o1) = n `divMod` 16
		     (_,o2) = d1 `divMod` 16
		 in f o2 . f o1 
		 where  f 10 = showChar 'a'	
		    	f 11 = showChar 'b'
			f 12 = showChar 'c'
			f 13 = showChar 'd'
			f 14 = showChar 'e'
			f 15 = showChar 'f'
			f x  = shows x

showsTrueNum :: Int -> ShowS
showsTrueNum x = showChar ' ' . shows x

showsFigNum :: Int -> ShowS
showsFigNum x = showChar ' ' . shows (calcNumUnit x)

showsFigUnit :: Int -> ShowS
showsFigUnit x = showChar ' ' . shows (calcFigUnit x)

calcFigUnit :: Int -> Int
calcFigUnit x = 15*((x*9 + 999) `div` 1000) 

calcNumUnit :: Int -> Int
calcNumUnit x = (x*9 + 999) `div` 1000

showsFigPoint :: Int -> Int -> ShowS
showsFigPoint x y = 
	showChar ' ' . showsxFigUnit x . showChar ' ' . showsyFigUnit y
	where showsxFigUnit x = shows (calcFigUnit x) -- + 'xfig' offset  
	      showsyFigUnit y = shows (calcFigUnit y) -- - 'xfig' offset 

showsPolySubType True a _  = showString a 
showsPolySubType False _ b = showString b
