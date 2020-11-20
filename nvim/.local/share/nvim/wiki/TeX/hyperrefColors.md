# Latex

## Colors

```tex
\usepackage[dvipsnames]{xcolor}
```

### Color definitions

```tex
\definecolor{amber}{rgb}{1.0, 0.75, 0.0}
\definecolor{darkblue}{rgb}{.1,0,.2}
\definecolor{dukeblue}{rgb}{0.0, 0.0, 0.61}
\definecolor{lapislazuli}{rgb}{0.15, 0.38, 0.61}
\definecolor{Blue}{rgb}{.1,.1,.5}
\definecolor{Royal}{rgb}{.2,.2,.8}
\definecolor{Green}{rgb}{0,.4,0}
\definecolor{Purple}{rgb}{.5,0,.5}
```

## Hyperref

```tex
\usepackage{hyperref}
```

### Options

```tex
\hypersetup{
  bookmarks=true,
  colorlinks=true,
  linktoc=section,
  linkcolor=blue,
  citecolor=dukeblue,
  filecolor=magenta,
  urlcolor=cyan,
}
```

```tex
\urlstyle{same}
```

### Usage with:

```tex
\label{} and \ref{}
\pageref{}
\href{url}{text} and \url{url}
\href{run:path/to/file}{text}
\hyperlink{senteceLabel}{text} and \hypertarget{sentenceLabel}{text}
```

## TitleSec

```tex
\usepackage{titlesec}
```
#### How to use it
```tex
\titleformat{<command>}[<shape>]{<format>}{<label>}{<sep>}{<before-code>}[<after-code>]
```
| Label       | Description                                                |
| :---        | :---                                                       |
| Command     | sectioning command do be redefined (\part, \chapter, ... ) |
| Shape       | hang, block, display, runin, leftmargin, rightmargin,      |
|             | drop, wrap, frame                                          |
| Format      | to be applied to the title, labels and text                |
| Label       | sectioning label                                           |
| Sep         | horizontal separation label-title body (cannot be empty)   |
| Before-code | code preceding the title body                              |
| After-code  | code following the title body                              |

```tex
\titlespacing{<command>}{<left>}{<before-sep>}{<after-sep>}
```

| Label      | Description                                      |
| :---       | :---                                             |
| Left       | increases the left margin                        |
| Before-sep | vertical space before the title                  |
| After-sep  | separation between title and non-sectioning text |

##### Example

```tex
\titleformat{\chapter}[display]
{\bfseries\Large\itshape}{}{0.1em}
{\rule{\textwidth}{1pt}\vspace{1ex}\centering}
[\vspace{-0.5ex}\rule{\textwidth}{0.3pt}]

\titleformat{\section}[wrap]
{\normalfont\bfseries}{\thesection}{0.5em}{}

\titlespacing{\section}{12pc}{1.5ex plus .1ex minus .2ex}{1pc}
```
