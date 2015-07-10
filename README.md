# Ebnf2ps

[![Build Status](https://travis-ci.org/FranklinChen/Ebnf2ps.png)](https://travis-ci.org/FranklinChen/Ebnf2ps)

Ebnf2ps generates nice looking syntax diagrams in EPS and FIG format
from EBNF specifications and from yacc, bison, and Happy input
grammars. The diagrams can be immediatedly included in TeX/LaTeX
documents and in texts created with other popular document preparation
systems.

## Usage

The latest version of the manual is available in [`doc.pdf`](https://raw.githubusercontent.com/FranklinChen/Ebnf2ps/master/doc/doc.pdf).

Two environment variables may need to be set:

- Find out where your `TeX` installation stores
  `.afm`-files (Adobe font metric files). Then set the
  environment variable `AFMPATH` when running the program. The
  `.afm`-files are absolutely necessary. If you don't find them
  complain to your system administrator or get them yourself. One
  possible source is [here](https://github.com/weiss/original-bsd/tree/master/local/transcript/lib/).
- Set the environment variable `RGBPATH` to contain the directory
  where your X installation stores its color data base (usually
  `/usr/lib/X11`). There are fallback color definitions for the 8
  digital colors (black, white, blue, green, cyan, red,
  magenta, yellow), so nothing needs be done if that's enough for you.

  Note: for Mac OS X, the default `/opt/X11/share/X11` has been
  provided and will work automatically, if you have installed X using
  [XQuartz](http://xquartz.macosforge.org/).

## Notes

With Peter Thiemann's permission, I am now maintaining this package, starting with version 1.0.12.

I first used this software in 1995 (!), so it was a nostalgia trip finding it again in 2004 and reviving it. PostScript is no longer in fashion, but one can still use this tool and then convert PostScript to PDF.
