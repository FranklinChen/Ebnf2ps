{-# LANGUAGE CPP #-}
------------------------------------------------------------------------
-- © 1998 Peter Thiemann
-- some default settings
-- 
module Defaults (afmPathDefault, ebnfInputDefault, rgbPathDefault)
where

afmPathDefault      = [
#include "afmpath.h"
                      , "/usr/local/tex/lib/TeXPS/afm"]
ebnfInputDefault    = ["."]
rgbPathDefault	    = [
#include "rgbpath.h"
                      , "/usr/X11R6/lib/X11"]
