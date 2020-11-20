# C Syntax and Command

#### `main()` Function

* The main() function is the starting point of the program: 	int main (int argc, char *argv[])
* The return type of the main() function is an integer (type int) and it is known as the return value of the program.
* As a rule of thumb, value 0 means success while non-zero means an error conditions.

#### Include Files

* The purpose of these files is to tell the compiler about the existence of external functions which the source code will make use of.

#### Preprocessor directives:

`#include "mine.h"` --- search current working directory first<br>
`#include <stdio.h>` --- search command line directory then system<br>
`#define TRUE 1` --- macro substitution, usually use capitals<br>
`#define min(a,b) (a<b)?(a):(b)` --- macro substitution with parameters<br>
`#define abs(a) (a<0)?(-(a)):(a)` --- macro substitution<br>
`#define note /* comment */` --- this comment gets inserted every time note appears */<br>
`#undef TRUE` --- undefines a previously defined macroname<br>
`#error` --- stop compiling at this point<br>
`#if expression` --- conditional compilation, start if structure<br>
`#elif expression` --- else if expression != 0 compile following code<br>
`#else` --- else compile following code<br>
`#endif` --- end of conditional compiling<br>
`#ifdef macroname` --- like #if, compiles if macroname defined<br>
`#ifndef` --- like #if, compiles if macroname undefined<br>
`#line number [filename]` --- set origin for __LINE__ and __FILE__<br>
`#pragma` --- gives the compiler commands<br>

#### 32 Reserved words

| __Word__     | __Usage__                                                          |
| :---         | :---                                                               |
| __Term__     | Description                                                        |
| __auto__     | optional local declaration                                         |
| __break__    | used to exit loop and used to exit switch                          |
| __case__     | choice in a switch                                                 |
| __char__     | basic declaration of a type character                              |
| __const__    | prefix declaration meaning variable can not be changed             |
| __continue__ | go to bottom of loop in for, while and do loops                    |
| __default__  | optional last case of a switch                                     |
| __do__       | executable statement, do-while loop                                |
| __double__   | basic declaration double precision floating point                  |
| __else__     | executable statement, part of "if" structure                       |
| __enum__     | basic declaration of enumeration type                              |
| __extern__   | prefix declaration meaning variable is defined externally          |
| __float__    | basic declaration of floating point                                |
| __for__      | executable statement, for loop                                     |
| __goto__     | jump within function to a label                                    |
| __if__       | executable statement                                               |
| __int__      | basic declaration of integer                                       |
| __long__     | prefix declaration applying to many types                          |
| __register__ | prefix declaration meaning keep variable in register               |
| __return__   | executable statement with or without a value                       |
| __short__    | prefix declaration applying to many types                          |
| __signed__   | prefix declaration applying to some types                          |
| __sizeof__   | operator applying to variables and types, gives size in bytes      |
| __static__   | prefix declaration to make local variable static                   |
| __struct__   | declaration of a structure, like a record                          |
| __switch__   | executable statement for cases                                     |
| __typedef__  | creates a new type name for an existing type                       |
| __union__    | declaration of variables that are in the same memory locations     |
| __unsigned__ | prefix declaration applying to some types                          |
| __void__     | declaration of a typeless variable                                 |
| __volatile__ | prefix declaration meaning the variable can be changed at any time |
| __while__    | executable statement, while loop or do-while loop                  |

#### Basic types

__char__ --- character type, usually one byte ( a string is array of char )<br>
__int__ --- integer type, usually 2 or 4 bytes ( default )<br>
__float__ --- floating point type, usually 4 bytes<br>
__double__ --- floating point type, usually 8 bytes<br>
__void__ --- no type, typeless<br>
__enum__ --- enumeration type ( user defines the type name )<br>

#### Type modifiers, prefix for basic types

__signed__ --- has a sign ( default )<br>
__unsigned__ --- no sign bit in variable<br>
__long__ --- longer version of type (short or long alone means short int or<br>
__short__ --- shorter version of type long int because int is the default)<br>
__const__ --- variable can not be stored into<br>

#### Storage Types

Prefix	   Description
auto	     local variable ( default )
static	   permanent when function exits, not auto
volatile   can change from outside influence
extern	   variables are defined elsewhere, externally
register   assign variable to register

Operators

  ( )	  grouping parenthesis, function call
  [ ]	  array indexing, also  [ ][ ]  etc.
  ->   	selector, structure pointer
  .	    select structure element
  !	    relational not, complement, ! a  yields true or false
  ~    	bitwise not, ones complement, ~ a
  ++  	increment, pre or post to a variable
  --   	decrement, pre or post to a variable
  -   	unary minus, - a
  +    	unary plus,  + a
  *    	indirect, the value of a pointer,  * p is value at pointer p address
  &    	the memory address, & b is the memory address of variable b
  sizeof size in bytes,   sizeof a     or  sizeof (int)
	(type) a cast, explicit type conversion,  (float) i, (*fun)(a,b), (int*)x
  *   	multiply, a * b
  /   	divide, a / b
  %    	modulo, a % b
  +    	add, a + b
  -    	subtract, a - b
  <<   	shift left,  left operand is shifted left by right operand bits
  >>   	shift right, left operand is shifted right by right operand bits
  <    	less than, result is true or false,  a %lt; b
  <=   	less than or equal, result is true or false,  a <= b
  >    	greater than, result is true or false,  a > b
  >=   	greater than or equal, result is true or false, a >= b
  ==   	equal, result is true or false,  a == b
  !=   	not equal, result is true or false,  a != b
  &    	bitwise and,  a & b
  ^   	bitwise exclusive or,  a ^ b
  |    	bitwise or,  a | b
  &&  	relational and, result is true or false,  a < b && c >= d
  ||	  relational or, result is true or false,  a < b || c >= d
  ?    	exp1 ? exp2 : exp3  result is exp2 if exp1 != 0, else result is exp3
  =    	store
  +=   	add and store
  -=   	subtract and store
  *=   	multiply and store
  /=  	divide and store
  %= 	  modulo and store
  <<=  	shift left and store
  >>=  	shift right and store
  &=   	bitwise and and store
  ^=   	bitwise exclusive or and store
  |=   	bitwise or and store
  ,    	separator as in   ( y=x,z=++x )


Operator precedence

More precedence

LR	( ) [ ] -> . x++ x--
RL	! ~ - + ++x --x * & sizeof (type)
LR	* / %
LR	+ -
LR	<< >>
LR	< <= > >=
LR	== !=
LR	&
LR	^
LR	|
LR	&&
LR	||
RL	? :
RL	= += -= *= /= %= >>= <<= &= ^= |=
LR	,

Less precedence

Conditional branching

 if ( condition ) statement ;
  else statement_2 ;            /* optional  else  clause */

Switch statement

switch ( expression )      /* constants must be unique              */
  {
      case constant_1:       /* do nothing for this case              */
         break;
      case constant_2:       /* drop through and do same as constant_3*/
      case constant_3:
         statement_sequence  /* can have but does not need  { }       */
         break;
      case constant_4:
         statement_sequence  /* does this and next */
                            		 /* statement_sequence also*/
      case constant_5:
         statement_sequence
         break;
      default:               /* default executes if no constant equals*/
         statement_sequence  /* the expression. This is optional      */
 }

Function definition

type function_name(int a, float b, const char * ch,...) { function_body }

/* only parameters passed by address can are modified*/

/* in the calling function, local copy can be modified*/

char * strcpy( char * s1, const char * s2 ) { statements }

Declarations forms

basic_type variable;

type variable[val][val]...[val]={data,data,...};  /*multidimensional array*/

struct struct_name {     /* struct_name is optional */
     type variable_1;    /* any declaration */
     …                   /* all variable names must be unique*/
} variable_1, ... ;      /* variables are optional */

struct struct_name {          /* struct_name is optional */
     type variable_1: length; /* any declaration : length in bits */
         ...					        /* type is int, unsigned or signed */
} variable_1, ... ;           /* variables are optional, they can also be arrays and pointers */


union union_name {            /* union_name is optional */
    type variable_1;          /* variable_1 overlays variable_2 */
    type variable_2;
        ...
} variable_a, ...;            /* variables are optional */

enum enum_type                /* enum_name is optional */
  { enumeration_name_1,       /* establishes enumeration literals */
    enumeration_name_2=number,/* optional number, */
      ...                     /* default is 0, 1, 2, ... */
  } variable, ...;            /* variables are optional */

 /* use dot notation to select a component of a struct or union */

