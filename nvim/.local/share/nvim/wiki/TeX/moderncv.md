# ModernCv Class for typesetting curricula

## Curriculum
```tex
\cvitem[spacing]{header}{text}
```
makes a resume line with a header and a corresponding text

```tex
\cvdoubleitem[spacing]{header1}{text1}{head2}{text2}
```
makes a resume line with 2 list items

```tex
\cvlistitem[label]{item}
```
makes a resume line with a list item

```tex
\cvlistdoubleitem[label]{item1}{item2}
```
makes a resume line with 2 list items

```tex
\cventry[spacing]{years}{degree/job title}{institution/employer}{
  localization}{optional: grade/...}{optional: comment/job description}
```
makes a typical resume job / education entry

```tex
\cvitemwithcomment[spacing]{header}{text}{comment}
```
makes a resume entry with a proficiency comment

```tex
\link[optional text]{link}
```
makes a generic hyperlink

```tex
\httplink[optional text]{link}
```
makes a http hyperlink

```tex
\emaillink[optional text]{link}
```
makes an email hyperlink

```tex
\begin{cvcolumns}
\cvcolumn[width]{head}{content}
\cvcolumn[width]{head}{content}
...
\end{cvcolumns}
```
__cvcolumns__ environment, where every column is created through \cvcolumn
where "_width_" is the width as a fraction of the line length
(between 0 and 1), "_head_" is the column header and "_content_" its content

## Closing Letter

```tex
\recipient{recipientname}{recipientadress}
\date{}
\opening{letter opening}
\closing{letter closing}
\enclosure[Attached]{curriculum vit\ae{}}

\makelettertitle

Letter content

\makeletterclosing
```
