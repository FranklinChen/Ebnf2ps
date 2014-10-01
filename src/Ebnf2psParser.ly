Ebnf2psParser.ly --- happy grammar for Ebnf2ps input
(1)  A happy specification for a grammar in EBNF
(2)  A happy specification for the happy input language
(3)  A happy specification for the yacc input language


> {
> module Ebnf2psParser (theEbnfParser, theHappyParser, theYaccParser) where
> import AbstractSyntax
> import Lexer
> import List
> }


> %name ebnf2psParser
> %tokentype { Token' }
> %token
>       EBNF         { EbnfInput     }   
>       HAPPY        { HappyInput    }
>       YACC         { YaccInput     }
>       id_tok       { Ident' $$     }
>       cid_tok      { CIdent' $$    }
>       ";"          { SemiColon     }
>       ":"          { Colon         }
>       "::"         { DoubleColon   }
>       "%%"         { DoublePercent }
>       "%"          { Percent       }
>       "|"          { Bar           }
>       "{"          { OpenBrace     }
>       "}"          { ClosingBrace  }
>       "."          { Dot           }
>       "="          { Equal         }
>       "+"          { Plus          }
>       "["          { OpenBrack     }
>       "]"          { ClosingBrack  }
>       "("          { OpenParen     }
>       ")"          { ClosingParen  }
>       "/"          { Slash         }
>       any_string   { String' $$    }
>       any_symbol   { Symbol' $$    }
>       number       { Number' _     }

Scheint in der neuen Happy-Version nicht mehr verfügbar zu sein. In der
Dokumentation wird jedenfalls darauf hingewiesen, daß das newline-token in
zukünftigen Versionen möglicherweise abgeschafft wird.
   > %newline        { Newline       }

> %%

> ebnf2psparse    :: { [Production] }
> ebnf2psparse    :  EBNF   ebnfInput                       { $2 }
>                 |  HAPPY  happyInput                      { $2 }
>                 |  YACC   yaccInput                       { $2 }


(1) happy specification for a grammar in EBNF

> ebnfInput       :: { [Production] }
> ebnfInput       :  ebnfProductions                        { $1 }

> ebnfProductions :: { [Production] }
> ebnfProductions :  ebnfProduction                         { [$1] }
>                 |  ebnfProductions ebnfProduction         { $1 ++ [$2] }

> ebnfProduction  :: { Production }
> ebnfProduction  :  ebnfNonterminal 
>                    ebnfOptString "=" ebnfTerm ";"         { ProdProduction $1 $2 $4 }
                
> ebnfOptString   :: { [String] }
> ebnfOptString   :  any_string                             { [$1] }
>                 |                                         { [] }

> ebnfNonterminal :: { String }
> ebnfNonterminal :  id_tok                                 { $1 }

> ebnfTerm        :: { Production }
> ebnfTerm        :  ebnfFactors                            { ProdTerm $1 }     

> ebnfFactors     :: { [Production] }
> ebnfFactors     :  ebnfFactor                             { [$1] }
>                 |  ebnfFactors "|" ebnfFactor             { $1 ++ [$3] }

> ebnfFactor      :: { Production }
> ebnfFactor      :  ebnfExtAtoms                           { ProdFactor $1 }

> ebnfExtAtoms    :: { [Production] }
> ebnfExtAtoms    :  ebnfExtAtom                            { [$1] }
>                 |  ebnfExtAtom ebnfExtAtoms               { $1 : $2 }

> ebnfExtAtom     :: { Production }
> ebnfExtAtom     :  ebnfAtom                               { $1 }
>                 |  ebnfAtom "/" ebnfAtom                  { ProdRepeatWithAtom $1 $3 }
>                 |  ebnfAtom "+"                           { ProdRepeat1 $1 }

> ebnfAtom        :: { Production }
> ebnfAtom        :  id_tok                                 { ProdNonterminal $1 }
>                 |  any_string                             { ProdTerminal $1 }
>                 |  "(" ebnfTerm ")"                       { $2 }
>                 |  "[" ebnfTerm "]"                       { ProdOption $2 }   
>                 |  "{" ebnfTerm "}"                       { ProdRepeat $2 }



(2) happy specification for the happy input language

> happyInput       :: { [Production] }
> happyInput       :  happyOptCode 
>                     happyTokInfos 
>                     "%%" 
>                     happyRules 
>                     happyOptCode                          { happyPrepare $2 $4 }

> happyRules       :: { [Production] }
> happyRules       :  happyRule happyRules                  { $1 : $2 }
>                  |  happyRule                             { [$1] }

> happyRule        :: { Production }
> happyRule        :  id_tok "::" happyCode happyHRule      { ProdProduction $1 [] $4 }
>                  |  id_tok ":" happyProds                 { ProdProduction $1 [] (ProdTerm $3) }

> happyHRule       :: { Production }
> happyHRule       :  id_tok ":" happyProds                 { ProdTerm $3 }
>                  |         ":" happyProds                 { ProdTerm $2 }

> happyProds       :: { [Production] }
> happyProds       :  happyProd "|" happyProds              { $1 : $3 }
>                  |  happyProd                             { [$1] }

> happyProd        :: { Production }
> happyProd        :  happyProdItems happyCode ";"          { ProdFactor $1 }
>                  |  happyProdItems happyCode              { ProdFactor $1 }

> happyProdItems   :: { [Production] }
> happyProdItems   :  happyProdItem happyProdItems          { $1 : $2 }
>                  |                                        { [] }

> happyProdItem    :: { Production }
> happyProdItem    :  any_string                            { ProdTerminal    $1 }
>                  |  id_tok                                { ProdNonterminal $1 }

> happyTokInfos    :: { [String] }
> happyTokInfos    :  happyTokInfo happyTokInfos            { $1 ++ $2 }
>                  |  happyTokInfo                          { $1 }

> happyTokInfo     :: { [String] }
> happyTokInfo     :  "%" id_tok happyTokInfoRest           { $3 }

> happyTokInfoRest :: { [String] }
> happyTokInfoRest :  happyCode                             { [] }
>                  |  id_tok                                { [] }
>                  |  happyCode happyCode happyCode         { [] }
>                  |  happyTokenList                        { $1 }      

> happyTokenList   :: { [String] }
> happyTokenList   :  id_tok happyCode happyTokenList       { $1 : $3 }
>                  |  any_string happyCode happyTokenList   { $3 }
>                  |                                        { [] }

here goes happy optCode:

> happyOptCode     :: { () }
> happyOptCode     :  happyCode                             { () }
>                  |                                        { () }

> happyCode        :: { () }
> happyCode        :  "{" happyCodeBody "}"                 { () }

> happyCodeBody    :: { () }
> happyCodeBody    :  happyCodeItem happyCodeBody           { () }
>                  |  happyCodeItem                         { () }

> happyCodeItem    :: { () }
> happyCodeItem    :  any_string                            { () }
>                  |  id_tok                                { () }
>                  |  happyCode                             { () }
>                  |  any_symbol                            { () }
>                  |  ":"                                   { () }
>                  |  ";"                                   { () }
>                  |  "::"                                  { () }
>                  |  "|"                                   { () }
>                  |  "%%"                                  { () }
>                  |  "%"                                   { () }



(3) happy specification for the yacc input language 

> yaccInput       :: { [Production] }
> yaccInput       :  "%%" yaccRules yaccEnd                 { yaccPrepare $2 }

> yaccRules       :: { [Production] }
> yaccRules       :  yaccRhs                                { [$1] }
>                 |  yaccRules yaccRule                     { $1 ++ [$2] }

> yaccRule        :: { Production }
> yaccRule        :  yaccRhs                                { $1 }
>                 |  "|" yaccRbody yaccPrec                 { ProdFactor $2 }

> yaccRhs         :: { Production }
> yaccRhs         :  cid_tok ":" yaccRbody yaccPrec         { ProdProduction $1 [] (ProdTerm [ProdFactor $3]) }

> yaccRbody       :: { [Production] }
> yaccRbody       :  yaccRbody yaccIdent                    { $1 ++ [$2] }
>                 |  yaccRbody yaccAction                   { $1 }
>                 |                                         { [] }

> yaccPrec        :: { () }
> yaccPrec        :  "%" id_tok yaccIdent                   { () }
>                 |  "%" id_tok yaccIdent yaccAction        { () }
>                 | yaccPrec ";"                            { () }
>                 |                                         { () }

> yaccIdent       :: { Production }
> yaccIdent       :  id_tok                                 { ProdTerminal $1 }
>                 |  any_string                             { ProdTerminal $1 }

> yaccAction      :: { () }
> yaccAction      :  "{" "}"                                { () }

> yaccEnd         :: { () }
> yaccEnd         :  "%%"                                   { () }
>                 |                                         { () }




> {

alt, mit Zeilennummerangabe:
   > happyError :: Int -> [Token'] -> a
   > happyError i ts = error ("Parse error in (line " ++ show i ++ ") " ++
   >                          case ts of
   >                          [] -> " at EOF\n"
   >                          _  ->  "before\n" ++ showList (take 20 (dropWhile (==Newline) ts)) [] ++ "\n")

neu, ohne:
> happyError :: [Token'] -> a
> happyError ts = error ("Parse error in " ++
>                          case ts of
>                          [] -> " at EOF\n"
>                          _  ->  "before\n" ++ showList (take 20 (dropWhile (==Newline) ts)) [] ++ "\n")

A preprocessor for literal scripts (slow)

> unlit :: String -> String
> unlit = unlines . map p . lines
>     where p ('>':' ':cs)  = cs
>           p ('>':'\t':cs) = cs
>           p _             = [] 

A preprocessor for yacc scripts 

> yaccpreprocessor :: String -> String 
> yaccpreprocessor "" = ""
> yaccpreprocessor ('%':'%':cs) = '%':'%': yaccRules cs
> yaccpreprocessor ('\n':cs)    = '\n':yaccpreprocessor cs
> yaccpreprocessor (_:cs)       = yaccpreprocessor cs   
 
> yaccRules :: String -> String 
> yaccRules "" = ""
> yaccRules ('/':'*':cs) = yaccRules (dropCComment 0 cs)
> yaccRules ('%':'{':cs) = yaccRules  (dropCSyntax cs)
> yaccRules ('%':'%':cs) = "%%" 
> yaccRules ('\'':'{':'\'':cs) = '\'':'{':'\'': yaccRules cs
> yaccRules ('{':cs)     = '{':yaccRules (dropActions 0 cs)
> yaccRules (c:cs)       = c:yaccRules cs
 
> dropCSyntax :: String -> String
> dropCSyntax "" = ""
> dropCSyntax ('%':'}':cs) = cs
> dropCSyntax ('\n':cs) = '\n':dropCSyntax cs
> dropCSyntax (c:cs) = dropCSyntax cs
 
> dropCComment :: Int -> String -> String
> dropCComment _ "" = ""
> dropCComment n ('/':'*':cs) = dropCComment (n+1) cs
> dropCComment n ('\n':cs) = '\n':dropCComment n cs
> dropCComment n ('*':'/':cs) 
>              | n == 0 = cs
>              | otherwise = dropCComment (n-1) cs
> dropCComment n (c:cs) = dropCComment n cs

 
> dropActions :: Int -> String -> String 
> dropActions _ "" = ""
> dropActions n ('"':cs) = dropActions n css where (_,css) = lexString cs
> dropActions n ('\'':'{':'\'':cs) = dropActions n cs
> dropActions n ('\'':'}':'\'':cs) = dropActions n cs
> dropActions n ('{':cs) = dropActions (n+1) cs
> dropActions n ('\n':cs) = '\n':dropActions n cs
> dropActions n ('}':cs) 
>             | n == 0 = '}':cs
>             | otherwise = dropActions (n-1) cs
> dropActions n (c:cs) = dropActions n cs                 


A postprocessor for a grammar in EBNF and a postprocessor to make happy happy

> data Token'
>        = EbnfInput
>        | HappyInput
>        | YaccInput
>        | Newline
>        | Ident'  String 
>        | CIdent' String
>        | Symbol' String
>        | String' String
>        | Number' String
>        | Percent
>        | DoublePercent
>        | OpenBrace  
>        | ClosingBrace
>        | Bar 
>        | SemiColon 
>        | DoubleColon 
>        | Colon
>        | OpenBrack 
>        | ClosingBrack
>        | OpenParen 
>        | ClosingParen
>        | Dot 
>        | Equal 
>        | Plus 
>        | Slash
>   deriving Eq


> instance Show Token' where 
>  showsPrec n (Ident' s) = showChar '[' . showString s . showString "] "
>  showsPrec n (CIdent' s) = showChar '/' . showString s . showString "/"
>  showsPrec n (Symbol' "\n") = showChar '\n'
>  showsPrec n (Symbol' s) = showChar '<' . showString s . showString "> "
>  showsPrec n (String' s) = showChar '"' . showString s . showString "\" "     
>  showsPrec n (Number' s) = showChar ' ' . showString s . showChar ' ' 
>  showsPrec n Percent = showString "%"
>  showsPrec n DoublePercent = showString "%% "
>  showsPrec n OpenBrace = showString "{ "
>  showsPrec n ClosingBrace = showString "} "
>  showsPrec n OpenBrack = showString "[ "
>  showsPrec n ClosingBrack = showString "] "
>  showsPrec n OpenParen = showString "( "
>  showsPrec n ClosingParen = showString ") "
>  showsPrec n Bar = showString "| "
>  showsPrec n SemiColon = showString "; "
>  showsPrec n DoubleColon = showString ":: "
>  showsPrec n Colon = showString ": "
>  showsPrec n Dot = showString ". "
>  showsPrec n Equal = showString "= "
>  showsPrec n Plus = showString "+ "
>  showsPrec n Slash = showString "/ "
>  showsPrec n Newline = showString "\n"
>  showsPrec n YaccInput = showString "\n>>YACC input format<<\n"
>  showsPrec n EbnfInput  = showString "\n>>EBNF input format<<\n" 
>  showsPrec n HappyInput = showString "\n>>HAPPY input format<<\n" 
>  showList [] = id
>  showList (x:xs) = shows x . showList xs


a ebnf postlexer

> ebnf_postlexer = \s -> EbnfInput : foldr f [] s
>   where f (Symbol "\n") = id --Newline
>         f (Symbol "=")  = (Equal:)
>         f (Symbol ".")  = (Dot:)
>         f (Symbol "|")  = (Bar:)
>         f (Symbol "/")  = (Slash:)
>         f (Symbol "+")  = (Plus:)
>         f (Symbol "(")  = (OpenParen:)
>         f (Symbol "[")  = (OpenBrack:)
>         f (Symbol "{")  = (OpenBrace:)
>         f (Symbol ")")  = (ClosingParen:)
>         f (Symbol "]")  = (ClosingBrack:)
>         f (Symbol "}")  = (ClosingBrace:)
>         f (Symbol ";")  = (SemiColon:)
>         f (Symbol s)    = (Symbol' s:)
>         f (Ident s)     = (Ident'  s:)
>         f (String s)    = (String' s:)
>         f (Number n)    = (Symbol' n:)

a happy postlexer

> happy_postlexer = \s -> HappyInput : foldr f [] s
>   where f (Symbol "\n") = id --Newline
>         f (Symbol "%%") = (DoublePercent:)
>         f (Symbol "%")  = (Percent:)
>         f (Symbol "{")  = (OpenBrace:)
>         f (Symbol "}")  = (ClosingBrace:)
>         f (Symbol "::") = (DoubleColon:)
>         f (Symbol ":")  = (Colon:)
>         f (Symbol ";")  = (SemiColon:)
>         f (Symbol "|")  = (Bar:)
>         f (Symbol s)    = (Symbol' s:)
>         f (Ident s)     = (Ident'  s:)
>         f (String s)    = (String' s:)
>         f (Number n)    = (Symbol' n:)

a yacc postlexer

> yacc_postlexer s = YaccInput : f s
>   where toSkip [] = False
>         toSkip (Symbol "\n":cs') = toSkip cs'
>         toSkip (Symbol ":":_) = True
>         toSkip (c:_) = False
>         f [] = []
>         f (Symbol "\n":cs) = f cs		-- Newline
>         f (Symbol "%":cs)  = Percent : f cs
>         f (Symbol "%%":cs) = DoublePercent : f cs
>         f (Symbol "|":cs)  = Bar : f cs
>         f (Symbol "{":cs)  = OpenBrace : f cs
>         f (Symbol "}":cs)  = ClosingBrace : f cs
>         f (Symbol ";":cs)  = SemiColon : f cs
>         f (Symbol ":":cs)  = Colon : f cs
>         f (Symbol c :cs) = (Symbol' c): f cs
>         f (String c :cs) = (String' c): f cs
>         f (Number c :cs) = (Number' c): f cs
>         f (Ident c :cs) | toSkip cs = (CIdent' c): f cs
>                         | otherwise = (Ident' c): f cs


> happyPrepare terminalsyms = map (happyPrepare' terminalsyms)
> happyPrepare' ts (ProdProduction s1 s2 prod)  = ProdProduction s1 s2 (happyPrepare' ts prod)
> happyPrepare' ts (ProdFactor prods) = ProdFactor (map (happyPrepare' ts) prods)
> happyPrepare' ts (ProdTerminal s) = ProdTerminal s
> happyPrepare' ts (ProdOption prod) = ProdOption (happyPrepare' ts prod)
> happyPrepare' ts (ProdRepeat prod) = ProdRepeat (happyPrepare' ts prod)
> happyPrepare' ts (ProdRepeat1 prod) = ProdRepeat1 (happyPrepare' ts prod)
> happyPrepare' ts (ProdRepeatWithAtom p1 p2) = ProdRepeatWithAtom (happyPrepare' ts p1) (happyPrepare' ts p2)
> happyPrepare' ts (ProdPlus) = ProdPlus
> happyPrepare' ts (ProdSlash prod) = ProdSlash (happyPrepare' ts prod)
> happyPrepare' ts (ProdTerm prods) = ProdTerm (map (happyPrepare' ts) prods)
> happyPrepare' ts (ProdNonterminal s) 
>      | s `elem` ts = ProdTerminal s
>      | otherwise   = ProdNonterminal s

                
> yaccPrepare happyresult =
>   [noDup (getNt nt) | nt <- nub nonterminals]
>   where (nonterminals, prods) = transform happyresult [] []
>         getNt str = [yaccPrepare' nonterminals p | p@(ProdProduction nt _ _) <- prods, str == nt] 
>         transform [] as bs = (as,bs)
>         transform ((ProdProduction nt aliases (ProdTerm ps)):pss) as bs =
>               transform pss' (nt:as) bs'
>               where (factors, pss') = span isProdFactor pss 
>                     bs' = bs ++ [ProdProduction nt aliases (ProdTerm ps')]
>                     ps' = ps ++ factors       
>         noDup [p] = p
>         noDup (ProdProduction nt aliases (ProdTerm ps):p':ps') = 
>               ProdProduction nt aliases 
>                (ProdTerm (foldr (\ (ProdProduction _ _ (ProdTerm prods')) ps1 -> ps1++prods') ps (p':ps')))
>         isProdFactor p = case p of { ProdFactor _ -> True;  _ -> False}

> yaccPrepare' nts (ProdProduction s1 s2 prod) = ProdProduction s1 s2 (yaccPrepare' nts prod)
> yaccPrepare' nts (ProdFactor prods) = ProdFactor (map (yaccPrepare' nts) prods)
> yaccPrepare' nts (ProdTerm prods) = ProdTerm (map (yaccPrepare' nts) prods)
> yaccPrepare' nts (ProdOption prod) = ProdOption (yaccPrepare' nts prod)
> yaccPrepare' nts (ProdRepeat prod) = ProdRepeat (yaccPrepare' nts prod)
> yaccPrepare' nts (ProdRepeat1 prod) = ProdRepeat1 (yaccPrepare' nts prod)
> yaccPrepare' nts (ProdRepeatWithAtom p1 p2) = ProdRepeatWithAtom (yaccPrepare' nts p1) (yaccPrepare' nts p2)
> yaccPrepare' nts (ProdPlus) = ProdPlus
> yaccPrepare' nts (ProdSlash prod) = ProdSlash (yaccPrepare' nts prod)
> yaccPrepare' nts (ProdTerminal s) 
>     | s `elem` nts = ProdNonterminal s
>     | otherwise = ProdTerminal s


> theEbnfParser  = ebnf2psParser . ebnf_postlexer  . lexer . uncomment
> theHappyParser = ebnf2psParser . happy_postlexer . lexer . unlit
> theYaccParser  = ebnf2psParser . yacc_postlexer  . lexer . yaccpreprocessor

> }

