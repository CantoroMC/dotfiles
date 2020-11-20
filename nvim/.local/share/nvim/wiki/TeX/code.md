# Code Formatting with LaTeX

## Listing Package
### Basics

```tex
\usepackage{listing}

\begin{lstlisting}[language=LanguageName,caption=captionName]
Code
\end{lstlisting}
```

The additional parameter inside brackets enables code highlighting for the
selected programming language.
Special words are in boldface and comments are italicized.

The captions can be used in the list of Listings
```tex
\lstlistoflistings
```
#### Supported languages are:

- ABAP (R/2 4.3, R/2 5.0, R/3 3.1, R/3 4.6C, R/3 6.10)
- ACSL
- Ada (2005, 83, 95)
- Algol (60, 68)
- Ant
- Assembler (Motorola68k, x86masm)
- Awk (gnu, POSIX)
- bash
- Basic (Visual)
- C (ANSI, Handel, Objective, Sharp)
- C++ (ANSI, GNU, ISO, Visual)
- Caml (light, Objective)
- CIL
- Clean
- Cobol (1974, 1985, ibm)
- Comal 80
- command.com (WinXP)
- Comsol
- csh
- Delphi
- Eiffel
- Elan
- erlang
- Euphoria
- Fortran (77, 90, 95)
- GCL
- Gnuplot
- Haskell
- HTML
- IDL (empty, CORBA)
- inform
- Java (empty, AspectJ)
- JVMIS
- ksh
- Lingo
- Lisp (empty, Auto)
- Logo
- make (empty, gnu)
- Mathematica (1.0, 3.0, 5.2)
- Matlab
- Mercury
- MetaPost
- Miranda
- Mizar
- ML
- Modula-2
- MuPAD
- NASTRAN
- Oberon-2
- OCL (decorative, OMG)
- Octave
- Oz
- Pascal (Borland6, Standard, XSC)
- Perl
- PHP
- PL/I
- Plasm
- PostScript
- POV
- Prolog
- Promela
- PSTricks
- Python
- R
- Reduce
- Rexx
- RSL
- Ruby
- S (empty, PLUS)
- SAS
- Scilab
- sh
- SHELXL
- Simula (67, CII, DEC, IBM)
- SPARQL
- SQL
- tcl (empty, tk)
- TeX (AlLaTeX, common, LaTeX, plain, primitive)
- VBScript
- Verilog
- VHDL (empty, AMS)
- VRML (97)
- XML
- XSLT

### Importing code from a file

```tex
\lstinputlisting[language=languageName,firstline=Number,lastline=Number]{fileName.ext}
```

### Code Styles and Colours

```tex
\lstdefinestyle{theStyle}{Options...}
\lstset{style=theStyle}
```

#### Style Options

- **backgroundcolor**
- **commentstyle**
- **basicstyle**: Font size, family .. for source code
- **keywordstyle**
- **numberstyle**
- **numbersep**: Distance of line-numbers from the code
- **stringstyle**
- **showspaces**: Emphasize spaces in code (true or false)
- **showstringspaces**: Emphasize spaces in strings (true/false)
- **showtabs**: emphasize tabulators in code (true/false)
- **numbers**: position of line numbers (left/right/none)
- **prebreak**: displaying mark on the end of breaking line
- **captionpos**: position of caption (t/b)
- **frame**: showing frame outside code
  (none/leftline/topline/bottomline/lines/single/shadowbox)
- **breakatwhitespace**: sets if automatic breaks should only happen at
  whitespaces (true/false)
- **breaklines**: automatic line-breaking (true/false)
- **keepspaces**: keep spaces in the code, useful for indentation (true/false)
- **tabsize**: default tab size
- **escapeinside**: specify characters to escape from source code to \LaTeX
- **rulecolor**: specify the color of the frame-box
#### Style example

```tex
\usepackage{listings}
\usepackage{xcolor}

\definecolor{codegreen}{rgb}{0,0.6,0}
\definecolor{codegray}{rgb}{0.5,0.5,0.5}
\definecolor{codepurple}{rgb}{0.58,0,0.82}
\definecolor{backcolour}{rgb}{0.95,0.95,0.92}

\lstdefinestyle{mystyle}{
  backgroundcolor=\color{backcolour},
  commentstyle=\color{codegreen},
  keywordstyle=\color{magenta},
  numberstyle=\tiny\color{codegray},
  stringstyle=\color{codepurple},
  basicstyle=\ttfamily\footnotesize,
  breakatwhitespace=false,
  breaklines=true,
  captionpos=b,
  keepspaces=true,
  numbers=left,
  numbersep=5pt,
  showspaces=false,
  showstringspaces=false,
  showtabs=false,
  tabsize=2
}
```

## Minted package

### Basics

```tex
\usepackage{minted}

\begin[Options...]{minted}{languageName}
\end{minted}
```
### Supported languages are:

- abap
- ada
- ahk
- antlr
- apacheconf
- applescript
- as
- aspectj
- autoit
- asy
- awk
- basemake
- bash
- bat
- bbcode
- befunge
- bmax
- boo
- brainfuck
- bro
- bugs
- c
- ceylon
- cfm
- cfs
- cheetah
- clj
- cmake
- cobol
- cl
- console
- control
- coq
- cpp
- croc
- csharp
- css
- cucumber
- cuda
- cyx
- d
- dg
- diff
- django
- dpatch
- duel
- dylan
- ec
- erb
- evoque
- fan
- fancy
- fortran
- gas
- genshi
- glsl
- gnuplot
- go
- gosu
- groovy
- gst
- haml
- haskell
- hxml
- html
- http
- hx
- idl
- irc
- ini
- java
- jade
- js
- json
- jsp
- kconfig
- koka
- lasso
- livescrit
- llvm
- logos
- lua
- mako
- mason
- matlab
- minid
- monkey
- moon
- mxml
- myghty
- mysql
- nasm
- newlisp
- newspeak
- numpy
- ocaml
- octave
- ooc
- perl
- php
- plpgsql
- postgresql
- postscript
- pot
- prolog
- psql
- puppet
- python
- qml
- ragel
- raw
- ruby
- rhtml
- sass
- scheme
- smalltalk
- sql
- ssp
- tcl
- tea
- tex
- text
- vala
- vgl
- vim
- xml
- xquery
- yaml

### Minted Options

- **frame**: leftline, topline, bottomline, single and lines
- **baselinestretch**: Code interlining
- **bgcolor**
- **fontsize**
- **linenos**: Enables line numbers
- **mathescape**: Enables math mode in code comments
- **rulecolor**: Changes the color of the frame
- **showspaces**: Enables a special character to make space visible

### Importing code from a file

```tex
\inputminted{LanguageName}{fileName}
```

### One-line code

```tex
\mint{LanguageName}|code|
```
where | is the code delimiter

### Colours and Style Sheets

```tex
\usemintedstyle{StyleName}
```

#### Colour Styles

Probably the default is the best one

**Light**
- _manni_
- _rrt_
- _perldoc_
- _borland_
- _colorful_
- _murphy_
- _vs_ --> Good
- _trac_
- _tango_
- _autumn_
- _emacs_
- _bw_
- _pastie_ --> Good
- _friendly_ --> Good

**Dark**
- _fruity_
- _vim_
- _native_
- _monokai_

### Captions, Labels and the List of Listings

```tex
\begin{listing}[ht]
\inputminted{LanguageName}{fileName}
\caption{CaptionText}

\label{abb:label}
\end{listing}
```

To print the list of listing use ```\lstlistoflistings```

Suggestion for a command
```tex
\renewcommand\listoflistingscaption{List of source codes}
```
