--                            -*- Mode: Haskell -*- 
-- Copyright © 1994,1998 by Peter Thiemann
-- Ebnf2ps.hs --- the driver module for the syntax diagram generator
-- Author          : Peter Thiemann
-- Created On      : Fri Aug 27 09:09:15 1993
--

module Main (main)
where

import Version
import IOSupplement
import CommandLine	(parse_cmds)
import StringMatch	(stringMatch)
import Fonts		(FONT, makeFont)
import Ebnf2psParser	(theEbnfParser, theHappyParser, theYaccParser)
import AbstractSyntax
import GrammarTransform (simplify, happysimplify, yaccsimplify, happysimplifyExt, yaccsimplifyExt)
import GrammarUnfold    (GrammarInfo(..), rawEBNFInput, rawHappyInput, rawYaccInput, getReduces, getRedProds, getEmpties, getProdsInfo)
import EbnfLayout
import Fig31Output	(fig31ShowsWrapper)
import PsOutput		(psShowsWrapper)
import EbnfOutput       (genEbnfFile)
import Defaults		(afmPathDefault, ebnfInputDefault, rgbPathDefault)
import PathExpansion	(expandPath) 
import Color
import Info
import IO
import Monad
import Numeric
import System 
import Time

--------------------------------------------------------------------------------
main :: IO () 
main =  parse_cmds program
--------------------------------------------------------------------------------
program   
          :: String -> Int -> String                        -- titleFont, titleScale, titleColor
          -> String -> Int -> String -> String -> String    -- ntFont, ntScale, ntColor, ntBoxColor
          -> String -> Int -> String -> String -> String    -- tFont, tScale, tColor, tBoxColor
          -> String                                         -- lineColor
          -> Int -> Int                                     -- borderDistX, borderDistY
          -> Int -> Int                                     -- lineWidth, fatLineWidth
          -> Int                                            -- arrowSize
          -> String                                         -- rgbFileName
          -> Bool                                           -- happyInput
          -> Bool                                           -- yaccInput
          -> Bool                                           -- +simplify
          -> Bool                                           -- +unfold
          -> Bool                                           -- +ps
          -> Bool                                           -- +fig 
          -> Bool                                           -- +ebnf
          -> Bool                                           -- help
          -> Bool                                           -- verbose
          -> [String] -> IO ()
program
	  titleFontName titleFontScale titleColor
	  ntFontName ntFontScale ntColor ntBg ntBoxColor
	  tFontName  tFontScale  tColor  tBg  tBoxColor
	  lineColor 
	  borderDistX borderDistY
	  lineWidth fatLineWidth
	  arrowSize 
	  rgbFileName
	  happyInput
	  yaccInput
          doSimplify
          doUnfold
	  psOutput 
	  fig31Output
          ebnfOutput
	  helpFlag verbose strs
	  | helpFlag =
            do progName <- getProgName
	       putStrLn ("Ebnf2ps Version " ++version)
	       putStrLn ("\nUsage: "++progName++" [options] Grammarfile Nonterminal ... [-- Nonterminal ...]")
	       putStr (unlines usageBlurb)
	  | length strs < 2 =
	    putStrLn "Grammarfile and one Nonterminal required"
	  | doUnfold && null unfold_nonterminals = 
	    do putStrLn "Nothing to unfold:"
	       putStrLn ("Grammarfile: "++bnfName)
	       putStr "Nonterminals: "
	       putStr (unlines nonterminals)
	       putStrLn ""
	       putStr "Unfolded Nonterminals: "
	       putStr (unlines unfold_nonterminals)
	       putStrLn ""
	  | otherwise =
	   do ct <- getClockTime
	      calt <- toCalendarTime ct
              let dateOutput = calendarTimeToString calt
	      putStrLn ("Grammarfile: "++bnfName)
	      putStrLn ("doUnfold = "++show doUnfold)
	      putStr "Nonterminals: "
	      putStrLn (unlines nonterminals)
	      putStr "Unfolded Nonterminals: "
	      putStrLn (unlines unfold_nonterminals)
	      afmPath' <- getPath "AFMPATH" afmPathDefault
	      afmPath <- expandPath afmPath'
	      titleAFM <- readPathFile afmPath (titleFontName++".afm")
	      ntAFM <- readPathFile afmPath (ntFontName++".afm")
	      tAFM <- readPathFile afmPath (tFontName++".afm")
	      let
 		sc = \rgbFileContents ->
		  let colorTable = prepareColors rgbFileContents
		                   [ntColor,tColor,lineColor,ntBoxColor,tBoxColor,ntBg,tBg,titleColor]
		      colorInfo@(c1,c2,c3,c4,c5,c6,c7,c8) = (lookupColor ntColor colorTable,
		                   lookupColor tColor colorTable,
                                   lookupColor lineColor colorTable,
		                   lookupColor ntBoxColor colorTable,
		                   lookupColor tBoxColor colorTable,
				   lookupColor ntBg colorTable,
				   lookupColor tBg colorTable,
				   lookupColor titleColor colorTable)
		      info = (borderDistX, borderDistY, lineWidth, fatLineWidth, arrowSize,
		          makeFont titleFontName titleFontScale titleAFM,
		          makeFont ntFontName ntFontScale ntAFM,
		          makeFont tFontName tFontScale tAFM,
		  	  colorInfo)
		  in do
		    message ("using colors:\n"++
				  (showsAColor c1 "ntColor"   .
				   showsAColor c2 "tColor"    .
		                   showsAColor c3 "lineColor" . 
	                           showsAColor c4 "ntBoxColor" .
				   showsAColor c5 "tBoxColor"  .
                                   showsAColor c6 "ntBg" .
				   showsAColor c7 "tBg"  . 
                                   showsAColor c8 "titleColor")
		      "from rgbPathDefault: "++show rgbPathDefault)
		    inputPath <-  getPath "EBNFINPUTS" ebnfInputDefault
		    message ("generating nonterminals: "++show nonterminals++
		      "\nfrom "++bnfName++
		      "\nusing input path "++show inputPath)
		    bnfContent <- readPathFile inputPath bnfName
		    let theParser | happyInput = theHappyParser
			          | yaccInput  = theYaccParser
				  | otherwise  = theEbnfParser
			getInfo   | happyInput = rawHappyInput ebnfOutput
				  | yaccInput  = rawYaccInput ebnfOutput
				  | otherwise  = rawEBNFInput
			usingInput| happyInput = "happyInput"
				  | yaccInput  = "yaccInput"
				  | otherwise  = "ebnfInput"
			rawInput = theParser bnfContent
			grammarinfo = getInfo doSimplify doUnfold unfold_nonterminals rawInput
			prods = getProdsInfo grammarinfo
		    message ("using "++usingInput)
		    message "empty productions"
		    domessages True (getEmpties grammarinfo)
		    domessage  doUnfold "unfold nonterminals"
		    domessages doUnfold (getReduces grammarinfo)
		    domessage  doUnfold "in productions"
		    domessages doUnfold (getRedProds grammarinfo)
		    domessage ebnfOutput ("produce EBNF file from "++usingInput)
		    writeEbnf bnfName grammarinfo
		    message "produce layout and output"
		    writeAll outExtension (layoutAll (outWrapper dateOutput) info prods nonterminals)
	      
	      rgbPath <- getPath "RGBPATH" rgbPathDefault
              rgbRead <- readRGBPathFile rgbPath rgbFileName
              case rgbRead of 
                   [] -> message "\nColor database not found, using fall back data" >> sc ""
                   _  -> sc rgbRead
    where
	(bnfName, nonterminals, unfold_nonterminals) = matchInput strs
	(outWrapper, outExtension)
	    | fig31Output = (const fig31ShowsWrapper, ".fig")
	    | otherwise   = (psShowsWrapper,    ".eps")
	message = domessage True
        domessage flag what = when (verbose && flag) 
				   (putStrLn what)
	domessages flag strs = when (verbose && flag)
				    (mapM_ (putStrLn . ('\t':)) strs)
        writeEbnf file grammar_info =
	  let  fileName = file++".BNF" in
	  when ebnfOutput $
	    do when verbose (putStrLn ('\t':fileName))
	       catch (writeFile fileName (genEbnfFile file grammar_info))
	             (\e -> putStrLn ("Problem writing "++fileName))

--------------------------------------------------------------------------------
layoutAll :: WrapperType -> INFO -> [Production] -> [String] -> [(String, String)]
layoutAll wrapper info prods nonterminals =
	[ (ntName, wrapper ntName info (makePictureLayout info prod) "")
	| prod@(ProdProduction ntName ntAliases _) <- prods,
	  any (flip stringMatch ntName) nonterminals ]
--------------------------------------------------------------------------------

usageBlurb =
    [ "",
      "where options may be chosen from the following list:",
      "",
      "  -titleFont       <font>\tPostScript font used for diagrams (default \"Times-Roman\")",
      "  -titleScale       <int>\tpointsize of typeface for diagrams (default \"10\")",
      "  -titleColor     <color>\tcolor of typeface for diagrams (default \"black\")",
      "  -ntFont          <font>\tPostScript font used for nonterminals (default \"Times-Roman\")",
      "  -ntScale          <int>\tpointsize of typeface for nonterminals (default \"10\")",
      "  -ntColor        <color>\tcolor of typeface for nonterminals (default \"black\")",
      "  -ntBg           <color>\tbackground color of typeface for nonterminals (default \"white\")",
      "  -ntBoxColor     <color>\tused for boxes (nonterminals). Default: \"black\"", 
      "  -tFont           <font>\tPostScript font used for terminal strings (default \"Times-Roman\")",
      "  -tScale           <int>\tpointsize of typeface for terminals (default \"10\")",
      "  -tColor         <color>\tcolor of typeface for terminals (default \"black\")",
      "  -tBg            <color>\tbackground color of typeface for terminals (default \"white\")",
      "  -tBoxColor      <color>\tused for boxes (terminals). Default: \"black\"",
      "  -lineColor      <color>\tcolor used for connecting lines (default \"black\")",
      "  -borderDistX      <int>\thorizontal distance of objects from their container (default \"500\")",
      "  -borderDistY      <int>\tvertical distance of objects from their container (default \"500\")",
      "  -lineWidth        <int>\tused for connecting lines (default \"30\")",
      "  -fatLineWidth     <int>\tused for boxes (default \"100\")",
      "  -arrowSize        <int>\tsize of (invisible) box containing an arrow (default \"200\")",
      "  -rgbFileName <filename>\tfile name for color definitions (default \"rgb.txt\")",
      "  -happy                 \taccept happy input format",
      "  -yacc                  \taccept yacc/bison input format",
      "  +ps                    \tproduce encapulated PostScript output (default)",
      "  +fig                   \tproduce fig output (FORMAT 3.1)",
      "  +ebnf                  \tproduce EBNF grammar (\".BNF\") for yacc or happy input format files",
      "  +simplify              \tsimplify productions (experimental)",
      "  +unfold                \treplace nonterminals in productions (experimental)",
      "  -verbose               \tprint some progress messages",
      "  -help                  \tproduces this list",
      "",
      "Happy is a parser generator system (LALR(1)) for Haskell, similar to the tool yacc for C.",
      "Only the first occurrence of an option is recognized.",
      "Environment variables:",
      "",
      "  AFMPATH\tsearch path for Adobe Font Metric files",
      "  EBNFINPUTS\tsearch path for BNFfiles",
      "  RGBPATH\tsearch path for color definitions"
      ]

--------------------------------------------------------------------------------

matchInput :: [String] -> (String, [String], [String])
matchInput (bnfName:more) = (bnfName,nonterminals, red_nonterminals)
    where (nonterminals,red_nonterminals) = f more []
          f [] nts = (nts, [])
          f ("--":rest) nts = (nts, rest)
          f (c:cs) nts = f cs (c:nts)

writeAll ext [] = putStr "\n"
writeAll ext ((ntName, content): more) =
    do catch (writeFile fileName content)
             (\e -> putStr ("Problem writing "++fileName))
       writeNext
    where
	writeNext = writeAll ext more 
	fileName = ntName ++ ext

--------------------------------------------------------------------------------

str2int :: String -> Int
str2int s = case readDec s of
	    [] -> 0
	    (x,_):_ -> x
