# General-Purpose Fortran Cooperative

This is a cooperatively developed repository of general purpose Fortran
modules and libraries.

This initial release is a collection of files output by personal build
utilities used to maintain the seed set of files.

Depending on interest the full set of files and utilities will be ported
to the repository.

Code additions are strongly encouraged (if this remains a personal
collection the purpose of the repository is essentially negated).

An index into the documentation is contained in the [[overview]](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/download.html)

This (mostly) Fortran repository contains modules and procedures primarily
useful for creating a command line interface (CLI) for a Fortran program. A
modern Fortran compiler is assumed. This code has been tested using GNU
Fortran (GCC) 5.4.0.

A [collection of programs](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/man_1k.html) that use the CLI
library are included. These range from useful utilities (a pre-processor, an
SCCS-style meta-data reader, regression testing utilities, ...) to simple
example programs that exercise the modules.

Again, this collection is _not_ a repository of routines solving complex
analytical problems. This is a collection of those "other" Fortran procedures
that solve the day-to-day non-numeric(mostly) issues in programming. Large
repositories of numeric algorithms written in Fortran exist at such sites as
the [ netlib ](http://www.netlib.org) repository. The Fortran Wiki [
fortranwiki.org ](http://fortranwiki.org) contains information on many Fortran
resources (If you do not see a link there to your favorites please add them.)


## Routine Categories and Descriptions

### command line arguments

[M_KRACKEN(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/BOOK_M_kracken.html): command line
arguments

The M_KRACKEN(3f) module makes cracking Unix-like arguments off the command
line easy. This version is expanded to allow use in configuration files and
interactive input as well, and incorporates additional modules. It requires
many other components from the CLI collection, but is used to generate
programs that use shell-like input as well as crack command lines.

[M_GETOPT(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/M_getopt.3.html): command line arguments

The M_GETOPT(3fm) module by Mark Gates supports cracking Unix-like arguments
off the command line in much the same way as the C getopt(3c) procedure.

[M_GETOPT_LONG(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/M_getopt_long.3.html): command line
arguments

The M_GETOPT_LONG(3fm) module by Joe Krahn supports cracking Unix-like
arguments off the command line in much the same way as the SunOS
getopt_long(3c) procedure.

### strings

[M_STRINGS(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/BOOK_M_strings.html)

convert case, change numbers to strings, strings to numbers, parse on
delimiters,edit strings, ... .

### date and time

[M_TIME(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/BOOK_M_time.html)

Calculate and display date and time values

### expression parsers

[M_CALCULATOR(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/M_calculator.3.html),
[M_CALCULATOR_PLUS(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/M_calculator_plus.3.html)

M_calculator(3fm) is a module for evaluating expressions. The primary routine
is JUCALC(3f). JUCALC(3f) evaluates simple numeric and string expressions.
This allows your input to use expressions and variable names instead of simple
values. You will have to comment out or supply some of the functions called,
depending on how f2003-compliant your compiler is.  
  
M_calculator_plus(3fm) is a supplemental module containing convenience
routines for using M_calculator(3fm).

### command line recall and editing

[M_HISTORY(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/redo.3.html): An interactive input editor
module

Acting much like a line-mode editor, the REDO(3f) procedure lets you list,
edit, save, and modify your interactively entered program input. Built-in help
and no dependence on terminal control sequences makes this a simple-to-master
and portable input history editor.

[ M_READLINE(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/M_readline.3.html): calling readline(3c)
from Fortran

The readline(3c) routine lets you edit and then execute previously entered
commands. An interface using a small C routine and the standard ISO_C_BINDING
module lets the commonly available C routine be called from Fortran. The
readline(3c) library is required.

### messages

[M_JOURNAL(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/M_journal.3.html),
[M_MESSAGES(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/M_messages.3.html)

journaling, logging, and general messaging routines that let you have a single
routine filter output to journal files; display attention-grabbing messages or
reformat messages.

### unit testing and debugging

[M_DEBUG(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/BOOK_M_debug.html)

These routines are useful for constructing unit tests for code and for adding
debug modes to routines and applications.

[Paranoid Compiler Test](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/paranoid.1.html)

The PARANOID program converted into subroutines that can be called from your
programs to verify compiler behavior.

### flow control

[M_LOGIC(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/M_logic.3.html): conditionally select input

The M_LOGIC(3f) module allows you to use if/else/elseif/endif directives in
your input; allowing conditional selection of input lines. Requires an
expression parser. It uses JUCALC(3f) by default.

### I/O

[ M_IO(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/M_io.3.html)

Input/Output

### hot keys

[GETKEY(3f)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/getkey.3.html): read a character from the
keyboard in "immediate mode"

A simple C routine for most Unix and GNU/Linux systems that immediately reads
a single character keystroke from the keyboard without waiting for a carriage
return. An interface using the ISO_C_BINDING interface allows the routine to
be called from Fortran.

### Fortran/C calls

[M_PROCESS(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/M_process.3.html)

Read and write from a process using popen(3c)

[M_SYSTEM(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/BOOK_M_system.html)

  * Environment
  * Directories
  * Error code
  * Process management
Some simple but commonly called C routines interfaced to Fortran. Makes heavy
use of the ISO_C_BINDING module introduced as part of Fortran 2003.

[M_NCURSES(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/M_ncurses.html),
[M_FIXEDFORM(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/PROGRAMS/fixeform.3.html)

## Fortran(2003) interface to the ncurses(3c) library

  
  
A Fortran module and a few related files create an interface from Fortran to
the C ncurses(3c) library; allowing Fortran programs to read function keys and
characters one at a time and to control the characters on the screen on a
character-cell by character-cell basis.

File characteristics

Input/Output

Regular expressions

Signal management

Sockets

database

encryption

internet

runtime

opengl

parallel

overloading

structures

Additional Utilities

Routine Categories  Descriptions

### numeric utilities that are aware of their accuracy

[M_COMPARE_FLOAT_NUMBERS(3fm)](math/Compare_Float_Numbers/M_Compare_Float_Numbers.HTML)

A Fortran module containing routines to perform equality and relational
comparisons on floating point numbers. That is, you can more safetly compare
real numeric values.

[Types and Kinds](types/Type_Kinds.HTML)

Fortran KIND definitions used by other parts of the basic utilities

[ACCDIG(3f)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/accdig.3.html)

compare two real numbers only up to a specified number of digits

### Graphics

[M_PIXEL(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/BOOK_M_pixel.html)

The M_pixel module is a _PRELIMINARY_ collection of routines that use 2D-
vector routines to create or modify a pixel array, which can then be written
as a GIF file.

### Color

[M_COLOR(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/M_color.3.html)

The M_color module is a collection of color-related procedures. Routines to
convert between different color models, return RGB values given common X11
color names, and other simple color-related operations.

### Sort

[ M_SORT(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/M_sort.3.html)

sorting routines encapsulated in a module

[ sort_shell(3f)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/sort_shell.3.html)

simple generic sort procedure

[ M_SWAP(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/swap.3.html)

swap two variables

### General Mathematics

[ M_MATH(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/BOOK_M_math.html)

A collection of routines for basic geometry, curve fitting and interpolation,
basic statistics, ...

### Unit Conversions

[ M_UNITS(3fm)](http://www.urbanjost.altervista.org/LIBRARY/libjust4/download/tmp/html/BOOK_M_units.html)

A collection of unit conversions and constants. Allow degrees instead of
radians in trig functions; convert between Celcius and Fahrenheit, ...

An example program that combines many of the components into a simple
calculator-like utility that parses command line arguments, evaluates Fortran-
like expressions, draws in pixel arrays, allows calls to ncurses, has
interactive command history recall and editing, and supports
if/else/elseif/endif directives can be found in
[shell.ff](EXE/SHELL/shell.ff).

* * *
