Ebnf2psParser.ly --- happy grammar for Ebnf2ps input
(1)  A happy specification for a grammar in EBNF
(2)  A happy specification for the happy input language
(3)  A happy specification for the yacc input language


> {
> module Ebnf2psParser (theEbnfParser, theHappyParser, theYaccParser) where
> import AbstractSyntax
> import Lexer
> import Data.List
> import ParseAux
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

> theEbnfParser  = ebnf2psParser . ebnf_postlexer  . lexer . uncomment
> theHappyParser = ebnf2psParser . happy_postlexer . lexer . unlit
> theYaccParser  = ebnf2psParser . yacc_postlexer  . lexer . yaccpreprocessor

> }
