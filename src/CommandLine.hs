module CommandLine (parse_cmds) where
import System.IO
import System.Environment

defaultArgs :: Args
defaultArgs  =  MkArgs "Times-Roman" 10 "black" "Times-Roman" 10 "black" "white" "black" "Times-Roman" 10 "black" "white" "black" "black" 500 500 30 100 200 "rgb.txt" False False False False True False False False False

usage :: IO ()
usage  =  putStr "Usage: prog [-titleFont String] [-titleScale Int] [-titleColor String] [-ntFont String] [-ntScale Int] [-ntColor String] [-ntBg String] [-ntBoxColor String] [-tFont String] [-tScale Int] [-tColor String] [-tBg String] [-tBoxColor String] [-lineColor String] [-borderDistX Int] [-borderDistY Int] [-lineWidth Int] [-fatLineWidth Int] [-arrowSize Int] [-rgbFileName String] [-happy] [-yacc] [(+|-)simplify] [(+|-)unfold] [(+|-)ps] [(+|-)fig] [(+|-)ebnf] [-help] [-verbose]\n" 

data Args  =  MkArgs String Int String String Int String String String String Int String String String String Int Int Int Int Int String Bool Bool Bool Bool Bool Bool Bool Bool Bool deriving ()
type ProgType = String -> Int -> String -> String -> Int -> String -> String -> String -> String -> Int -> String -> String -> String -> String -> Int -> Int -> Int -> Int -> Int -> String -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> [String] -> IO ()

parse_args :: ProgType -> Args -> [String] -> IO ()
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-titleFont":rest)
    =  readstring (\str -> parse_args prog (MkArgs str x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-titleScale":rest)
    =  readval reads (\val -> parse_args prog (MkArgs x1 val x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-titleColor":rest)
    =  readstring (\str -> parse_args prog (MkArgs x1 x2 str x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-ntFont":rest)
    =  readstring (\str -> parse_args prog (MkArgs x1 x2 x3 str x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-ntScale":rest)
    =  readval reads (\val -> parse_args prog (MkArgs x1 x2 x3 x4 val x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-ntColor":rest)
    =  readstring (\str -> parse_args prog (MkArgs x1 x2 x3 x4 x5 str x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-ntBg":rest)
    =  readstring (\str -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 str x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-ntBoxColor":rest)
    =  readstring (\str -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 str x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-tFont":rest)
    =  readstring (\str -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 str x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-tScale":rest)
    =  readval reads (\val -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 val x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-tColor":rest)
    =  readstring (\str -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 str x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-tBg":rest)
    =  readstring (\str -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 str x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-tBoxColor":rest)
    =  readstring (\str -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 str x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-lineColor":rest)
    =  readstring (\str -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 str x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-borderDistX":rest)
    =  readval reads (\val -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 val x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-borderDistY":rest)
    =  readval reads (\val -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 val x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-lineWidth":rest)
    =  readval reads (\val -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 val x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-fatLineWidth":rest)
    =  readval reads (\val -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 val x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-arrowSize":rest)
    =  readval reads (\val -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 val x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-rgbFileName":rest)
    =  readstring (\str -> parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 str x21 x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-happy":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 True x22 x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-yacc":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 True x23 x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-simplify":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 False x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("+simplify":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 True x24 x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-unfold":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 False x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("+unfold":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 True x25 x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-ps":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 False x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("+ps":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 True x26 x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-fig":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 False x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("+fig":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 True x27 x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-ebnf":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 False x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("+ebnf":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 True x28 x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-help":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 True x29)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) ("-verbose":rest)
    =  readbool (parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 True)) rest
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29) (('-': _) :rest)
    =  usage
parse_args prog (MkArgs x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29)  rest  =  prog x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 rest

parse_cmds :: ProgType -> IO ()
parse_cmds prog = getArgs >>= parse_args prog defaultArgs

readbool :: ([String] -> IO ()) -> [String] -> IO ()
readbool f = f

readstring :: (String -> [String] -> IO ()) -> [String] -> IO ()
readstring f (str: rest) = f str rest
readstring f []          = usage

readval :: (Read a) => ReadS a -> (a -> [String] -> IO ()) -> [String]
                       -> IO ()
readval readsfn f (str: rest)
    =  case readsfn str of
           ((val, ""):_) -> f val rest
           _             -> usage
