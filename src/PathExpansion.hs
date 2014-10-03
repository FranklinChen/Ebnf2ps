{-# LANGUAGE CPP #-}
module PathExpansion (expandPath)
where

#ifdef __GLASGOW_HASKELL__
import System.Environment (getEnv)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Control.Monad
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Posix.User (getUserEntryForName, homeDirectory)
#endif

expandPath :: [String] -> IO [String]

#if __GLASGOW_HASKELL__ || __HBC__

expandPath [] = return []
expandPath (path:paths) = do pathSpec1 <- replaceEnv(fst(stringToPathSpec([],path)))
			     pathSpec2 <- replaceHome(pathSpec1)
			     rest <- expandPath paths
			     beginn <- expandDirs(pSTS(rpBrace(replaceColon pathSpec2)))
			     return(beginn ++ rest)

-------------- Datenstruktur PathSpec -------------

data PathSpec =  EmptyPath|
		 Chars Char |
		 EnVar String |
		 Colon |
		 Tilde String|
		 Brace [PathSpec]|
		 Home String
		 deriving Show

------------- String in Datentyp PathSpec Parser ---------
stringToPathSpec :: ([PathSpec],String) -> ([PathSpec],[PathSpec])
stringToPathSpec (x,"") = (x++[EmptyPath],[EmptyPath])
stringToPathSpec (path,(x:xs))
	|x == '$' = stringToPathSpec (path ++ [EnVar (envName xs)],(envRest xs))
	|x == '{' = (path++[Brace (fst(stringToPathSpec([],xs)))]++snd(stringToPathSpec([],xs)),[EmptyPath])
	|x == '}' = (path++[EmptyPath],fst(stringToPathSpec ([],xs)))
	|x == ':' = stringToPathSpec (path ++[Colon],xs)
	|x == '~' && nextSign xs '/' = stringToPathSpec (path, "$HOME"++xs)
	|x == '~' = stringToPathSpec(path ++ [Home (envName xs)],envRest xs)
	|otherwise = stringToPathSpec (path ++ [Chars x],xs)


envName :: String -> String
envName "" = ""
envName (x:xs)
	|x == '{'  = takeWhile (not.(== '}')) xs
	|otherwise = takeWhile (not.specialSign) (x:xs)

envRest :: String -> String
envRest (x:xs)
	|x == '{'  = tail(dropWhile (not.(== '}')) xs)
	|otherwise = dropWhile (not.specialSign) (x:xs)

specialSign :: Char -> Bool
specialSign x = x == '/' || x == ':' || x == '{' || x == '}'


----- PathSpec, der nur aus Chars, in String ----------

pSTS :: [[PathSpec]] -> [String]
pSTS [] = []
pSTS (x:xs) = pathSpecToString x : pSTS xs
pathSpecToString :: [PathSpec] -> String
pathSpecToString [] = ""
pathSpecToString (Chars c : xs) = c:pathSpecToString xs
pathSpecToString (_:xs) = pathSpecToString xs
-------------------------------------------------------


-------- Ersetzen des Usernamen ----------
replaceHome :: [PathSpec] -> IO [PathSpec]
replaceHome [] = return []
replaceHome ((Home t):xs)= do user <- getUserEntryForName t
	   		      replaceHome(fst(stringToPathSpec([],homeDirectory user))++xs)
replaceHome ((Brace t):xs)= do braceIn <- replaceHome t
		               rest <- replaceHome xs
		               return(Brace(braceIn):(rest))
replaceHome (x:xs) = do t <- replaceHome xs
			return (x:t)


-------- Ersetzen der Umgebungsvariablen --------------

replaceEnv :: [PathSpec] -> IO [PathSpec]
replaceEnv [EmptyPath] = return []
replaceEnv ((EnVar t):xs)= do
			     name <- getEnv2 t
			     replaceEnv (fst(stringToPathSpec ([],name))++xs)
replaceEnv ((Brace t):xs) = do
                              braceIn <- replaceEnv t
			      rest <- replaceEnv xs
			      return (Brace (braceIn):(rest))
replaceEnv (x:xs) = do t <- replaceEnv xs
		       return (x:t)

getEnv2 t
	|t == "HOME" = catchIOError (getEnv t) (\e -> if isDoesNotExistError e then return ['.'] else ioError e)
	|otherwise = catchIOError (getEnv t) (\e -> if isDoesNotExistError e then return [] else ioError e)
-------------------------------------------------------

---------------- Teilen nach Doppelpunkt --------------
isColon :: PathSpec -> Bool
isColon Colon = True
isColon _ = False

replaceColon :: [PathSpec] -> [[PathSpec]]
replaceColon xs = let (l,s) = break (isColon) xs
	in l : case s of
		[] 	-> []
		(_:t) 	-> replaceColon t
-------------------------------------------------------

------- Ersetze Klammer -------------------------------

rpBrace :: [[PathSpec]] -> [[PathSpec]]
rpBrace [] = []
rpBrace (y:ys) = ((replaceBrace [] y) ++ (rpBrace ys))

replaceBrace :: [PathSpec] -> [PathSpec] -> [[PathSpec]]
replaceBrace dummy [] = [dummy]
replaceBrace dummy (Brace t : xs) = [foldr (++) [] (dummy:([a]++[b]))| q <- replaceColon t,a<- replaceBrace [] q, b <- replaceBrace [] xs]
replaceBrace dummy (x:xs) = replaceBrace (dummy ++ [x]) xs
-------------------------------------------------------
------- Subdirectory ----------------------------------

expandDirs :: [String] -> IO [String]
expandDirs [] = return []
expandDirs (path:paths) =
	do newpath <- expandOnePath [] path
	   newpaths <- expandDirs paths
	   return (newpath++newpaths)

expandOnePath :: String -> String -> IO [String]
expandOnePath anfang [] = return [anfang]
expandOnePath anfang (x:xs)
	|x == '/' && nextSign xs '/' =
		do allDirs <- expandAllSubPaths [(anfang ++ [x])]
		   const <- getConstraints (xs,[])
		   --expandDirs (map (++(fst(getConstraints (xs,[])))) [sub|sub <- allDirs, elemSub(reverse(snd(getConstraints (xs,[])))) (reverse sub)])
		   expandDirs (map (++ (fst const)) [sub|sub <- allDirs, elemSub(reverse(snd const)) (reverse sub)])
	|otherwise = do expandOnePath (anfang ++ [x]) xs


getConstraints :: (String,String) -> IO (String,String)
getConstraints ([],ys) = return ([],ys)
getConstraints (['/'],ys) = return ([],ys)
getConstraints ((x:xs),ys)
	|x == '/' && nextSign xs '/' = return ((x:xs),ys)
	|otherwise = getConstraints(xs,(ys ++ [x]))

elemSub :: String -> String -> Bool
elemSub [] _ = True
elemSub _ [] = False
elemSub (x:xs) (y:ys)
	|x == y = elemSub xs ys
	|otherwise = False

nextSign :: String -> Char -> Bool
nextSign [] _ = False
nextSign (x:xs) y = x == y

expandAllSubPaths :: [String] -> IO [String]
expandAllSubPaths [] = return []
expandAllSubPaths (x:xs) = do dirs <- searchForDirs (checkPath x)
		              subdirs <- expandAllSubPaths (dirs++xs)
		              return (x:subdirs)

searchForDirs :: FilePath -> IO [FilePath]
searchForDirs dir = do all_dir <- getDirectoryContents dir
		       dir_input <- filterM (specialPath) all_dir
	               filterM (doesDirectoryExist) (map (dir ++) dir_input)

specialPath :: FilePath -> IO Bool
specialPath p = return (special p)
  where special "." = False
	special ".." = False
	special _ = True

checkPath :: FilePath -> FilePath
checkPath path
	|last path == '/' = path
	|otherwise = path++"/"

----------------------------------------------------------------------------------
#else
-- for Hugs 1.4
expandPath = return

#endif
