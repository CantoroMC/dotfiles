# Some basic commands

## Logical operators

Logical operators can replace many if statements

| Operator | Description |
| :---     | :--- |
| &&       | Run the second command if the first succeded |
| \|\|     | Run the second command if the first not succed |
| ;        | Chains commands together |
| &        | Start the second command without waiting for the first command |

```sh
if [ -z "EDITOR" ]; then
    EDITOR=nano
fi
```

```sh
[ -z "$EDITOR" ] && EDITOR=nano
```

## If Statements
```sh
if [ condition ]; then
    commands
else
    commands
fi
```
| option          | Description                                              |
| :---            | :---                                                     |
| -a              | file exist                                               |
| -b              | file exist and is block special                          |
| -c              | file exist and is character special                      |
| -d              | file exist and is a directory                            |
| -e              | same as -a                                               |
| -f              | file exist and is regular                                |
| -g              | file exist and is set-group ID                           |
| -G              | file exist and is owned by the effective group ID        |
| -h              | file exist and is a simbolic link                        |
| -k              | file exist and is a sticky bit set                       |
| -L              | file exist and is a simbolic link (same as -h)           |
| -N              | file exist and was modified since last read              |
| -O              | file exist and is owned by the user executing the script |
| -p              | file exist and is a named pipe                           |
| -r              | file exist and is readable                               |
| -s              | file exist and is non empty                              |
| -S              | file exist and is a socket                               |
| -t              | file descriptor exist and refers to an open terminal     |
| -u              | file exist and is set-user ID                            |
| -w              | file exist and is writeable                              |
| -x              | file exist and is executable for the script              |
| __strings__     |                                                          |
| -n              | nonempty                                                 |
| -z              | empty                                                    |
| =~              | match regex pattern (requires [[ ]])                     |
| for             | shell options                                            |
| -o              | shell option is enabled                                  |
| !               | negate the option                                        |

| __comparisons__ | Description                                  |
| :---            | :---                                         |
| -nt             | newer than                                   |
| -ot             | older than                                   |
| -ef             | files refers to the same device/inode number |
| -a              | and between options                          |
| -o              | or between options                           |
| __numbers__     |                                              |
| -lt             |                                              |
| -gt             |                                              |
| -eq             |                                              |
| -ne             |                                              |
| -ge             |                                              |
| -le             |                                              |
| __strings__     |                                              |
| ==              |                                              |
| <               |                                              |
| >               |                                              |

## Double bracket syntax: features shell globbing
```sh
if [[ "$stringvar" == *[sS]tring* ]]; then
```

- if stringvar contains anywhere string or String
- word splitting is prevented
- you could omit placing quotes around string variables

not expanding filenames

``sh
if [ -a *.sh ]; then
``

throws an error if there is more than one file with sh extension
```sh
if [[ -a *.sh ]]; then
```
support also the && || comparisons and allow regex pattern matching using the =~ operator

## While Statements

```sh
while [ condition ]; do
    commands...
    ....
    ..
done
```

```sh
while [ condition ]; do commands; done
```

```sh
[condition] = :
```
 Infinite loop: command break to exit from the while loop or enter Ctrl+C command  continue to resume the next iteration

__Example__
```sh
x=1; while [ $x -le 5 ]; do echo "Welcome $x times" $(( x++ )); done
```

## Reading data line by line from file
```sh
FILE=$1             # read $FILE using the file descriptors
exec 3<&0
exec 0<$FILE
while read line; do # use $line variable to process line
    echo $line
done
exec 0<&3
```

## Functions  (use source to call a script)
__Definition__
function definition must be placed before any calls to the function
```sh
fun_name () {
    commands
}
```
```sh
fun_name () { commands; }
```

```sh
function fun_name {
    commands
}
```
```sh
function fun_name { commands; }
```

## Local and global variables

local variable defined with local var_name have precedence over global variables

### Value return
they cannot return real values but instead return the value of the status of the last statement executed
Return status (0-255) can be return with return StatusNumber and it is assigned to the variable `$?`
to return value we can assign it to global variable or we can send it to stdout using echo or printf

```sh
echo "$func_result"
func_result="$(fun_name)"   # command substitution
echo $func_result
```

## Passing parameters

```sh
fun_name par_name1 par_name2 ... par_nameN
```

the passed parameters are `$1`,`$2`, ... `$N`
`$0` is reserved for the function name<br>
`$#` holds the number of positional parameters/arguments passed<br>
`$*` and `$@` variables holds all positional arg/param passed to the function<br>
`"$*"` expands to a single string separated by space ("`$1` `$2` ... `$N`")<br>
`"$@"` expands to separate strings (`"$1"` `"$2"` ... `"$N"`)<br>

## Colors

`echo -e` enable the parsing of the escape sequences
`"<Esc>"[FormatCodem]`

<Esc> escape sequences: `\e`, `\033` `\x1B`

FormatCode
| Number | Set             | UnsetNumber |
| :---   | :---            | :---        |
| 1      | Bold/Bright     | 21          |
| 2      | Dim             | 22          |
| 4      | Underlined      | 24          |
| 5      | Blink           | 25          |
| 7      | Inverted colors | 27          |
| 8      | Hidden          | 28          |

0 Reset all attributes


| Foreground | Color         | Background |
| :---       | :---:         | ---:       |
| 39         | Default       | 49         |
| 30         | Black         | 40         |
| 31         | Red           | 41         |
| 32         | Green         | 42         |
| 33         | Yellow        | 43         |
| 34         | Blue          | 44         |
| 35         | Magenta       | 45         |
| 36         | Cyan          | 46         |
| 37         | Light Gray    | 47         |
| 90         | Dark Gray     | 100        |
| 91         | Light Red     | 101        |
| 92         | Light Green   | 102        |
| 93         | Light Yellow  | 103        |
| 94         | Light Blue    | 104        |
| 95         | Light Magenta | 105        |
| 96         | Light Cyan    | 106        |
| 97         | White         | 107        |

__Attributes combination__ separated by a semicolon
```sh
echo -e "\e[1;31;42m Yes it is awful \e[0m"
```
