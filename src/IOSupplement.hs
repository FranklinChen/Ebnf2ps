{-# LANGUAGE CPP #-}
--                            -*- Mode: Haskell -*- 
-- Copyright 1996 by Peter Thiemann
-- IOSupplement.hs --- some enhancements to the IO operations
-- Author          : Peter Thiemann
-- Created On      : Mon Aug 30 09:41:30 1993
-- Last Modified By: Peter Thiemann
-- Last Modified On: Thu Dec  2 10:37:39 1993
-- Update Count    : 13
-- Status          : Unknown, Use with caution!
-- 
-- $Log: IOSupplement.hs,v $
-- Revision 1.3  1999/08/15 20:28:48  thiemann
-- *** empty log message ***
--
-- Revision 1.2  1999/04/16 07:17:36  thiemann
-- modified for Haskell98
--
-- Revision 1.1.1.1  1998/12/09 13:34:08  pjt
-- Imported sources
--
-- Revision 1.2  1994/03/15  15:34:53  thiemann
-- generalized readPathFile
--
-- Revision 1.1  1993/08/31  12:31:32  thiemann
-- Initial revision
--

module IOSupplement (getPath, readPathFile, readRGBPathFile)
where

import System
import IO

#ifdef __HBC__
ioError = fail
#endif
--------------------------------------------------------------------------------

getPath :: String -> [String] -> IO [String]
--
-- accepts the name of an environment variable and a [String] of default paths
-- and returns the default or the resulting search path .
getPath envVar dflt =
    catch 
    ( do path <- getEnv envVar
         return (manglePath path dflt) )
    ( \err -> if (isDoesNotExistError err) 
                then return dflt
	        else ioError err )


-- mangle a colon separated pathstring with a default path
manglePath :: String -> [String] -> [String]
manglePath "" dflt = dflt
manglePath cs dflt = case span (/= ':') cs of
	                       ("",':':cs') -> dflt ++ manglePath cs' []
			       ("", "") -> dflt
			       (path,':':cs') -> path: manglePath cs' dflt
			       (path,"") -> [path]

--------------------------------------------------------------------------------

readPathFile :: [String] -> String -> IO String
-- readPathFile searchPath fileName 
-- scan searchPath for fileName and read it
-- unless fileName starts with '.' or is absolute (starts with '/')
--
readPathFile _  fileName@('/':_) = readFile fileName 
readPathFile _  fileName@('.':_) = readFile fileName 
readPathFile [] fileName =
    putStr ("readPathFile failed on : " ++ fileName) >>
    error "Exit Ebnf2ps (C) Peter Thiemann\n"
readPathFile (path: paths) fileName =
    -- putStr ("trying in "++path++" : "++fullName ++"\n") >>
    try (readFile fullName) >>= \ result ->
    case result of
	Left _ -> readPathFile paths fileName
	Right cont -> return cont
    where
	fullName   = path ++ '/': fileName



readRGBPathFile :: [String] -> String -> IO String
-- 
readRGBPathFile _  fileName@('/':_) = tryreadFile fileName 
readRGBPathFile _  fileName@('.':_) = tryreadFile fileName 
readRGBPathFile [] fileName  =
    putStr ("readPathFile failed on : " ++ fileName) >> 
    return ""
readRGBPathFile (path: paths) fileName =
    try (readFile fullName) >>= \ result ->
    case result of
	Left _ -> readRGBPathFile paths fileName 
	Right cont -> return cont
    where fullName   = path ++ '/': fileName

tryreadFile :: String -> IO String
tryreadFile file =
    try (readFile file)        >>= \result ->
    return (case result of 
             Left _ -> ""
	     Right str -> str)   
