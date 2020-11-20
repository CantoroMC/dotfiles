# Fzf - Command line fuzzy finder

## Usage

Fuzzy finder will launch interactive finder, read the list from STDIN and write
the selected item to STDOUT
```sh
find * -type f | fzf >> selectedFile
```

## Search Syntax

| Token    | Match Type                 | Description                         |
| :---     | :---:                      | :---                                |
| string   | fuzzy-match                | Items that match the string         |
| 'string  | exact-match                | Items that include string           |
| ^string  | prefix-exact-match         | Items that start with string        |
| string$  | suffix-exact-match         | Items that end with string          |
| !string  | inverse-exact-match        | Items that do not include string    |
| !^string | inverse-prefix-exact-match | Items that do not start with string |
| !string$ | inverse-suffix-exact-match | Items that do not end with string   |

-e or --exact option to use exact match by default

fzf doen't support globbing but you can insert multiple search terms with a |
in between them

## Environmental variable

`FZF_DEFAULT_COMMAND` --> Default command to use when input is tty<br>
`FZF_DEFAULT_OPTS` --> Default options

## Options

##### Search Mode

| short opt | long option           | Description                                                               |
| :---      | :---                  | :---                                                                      |
| -x        | --extended            | Extended search mode(default)                                             |
| -e        | --exact               | Enable exact match                                                        |
| -i        |                       | Case insensitive match(default)                                           |
| +i        |                       | Case sensitive match                                                      |
|           | --literal             | Do not normalize latin script letters for matching                        |
|           | --algo=__TYPE__       | Fuzzy matching algorithm                                                  |
| -n        | --nth=__N[,..]__      | Comma separated list of filed index expressions for limiting search scope |
|           | --with-nth=__N[,..]__ | Transform the presentation of each line using field index expressions     |
| -d        | --delimiter=__STR__   | Field delimiter regex for __--nth__ and __-with-nth__ (default AWK-style) |
|           | --phony               | Do not perform search.                                                    |

##### Search result

| short opt | long option | Description |
| :--- | :--- | :---|
| | |


## Examples

** <Tab> trigger fuzzy completion
