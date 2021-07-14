# General-Purpose Fortran Repository

## Shortcut

The majority of the documentation is in HTML at the 
[Documentation](https://urbanjost.github.io/general-purpose-fortran/index.html)
page.

## Intro
This repository concentrates on those "other" Fortran procedures that
solve the day-to-day non-numeric(mostly) issues in programming. Large
repositories of numeric algorithms written in Fortran exist at such
sites as the [ netlib ](http://www.netlib.org) repository.

Modern Fortran is quite capable of accessing many commonly available
C libraries or expressing many commonly desired utility procedures
directly, but the standard language does not define many of these
basic interfaces. Because of Fortran's expressiveness and power in
HPC environments ( Coarrays, MPI, OpenMP, matrix operators and the
many numeric libraries available in Fortran) I prefer we bring these
general-purpose utilities to Fortran instead of bringing Fortran to other
languages. With that in mind I placed some such modules I have here.
I hope others will find them useful (and expand upon them).

### Alternate stand-alone modules

GPF(General Purpose Fortran) is the main repository that contains man(1)
pages, utility programs, extended examples and unit tests.  As generally
useful modules mature simpler stand-alone versions are created from
this base. That is because the full GPF (General Purpose Fortran
repository) has many interdependencies. The extracated stand-alone
[individual modules](https://github.com/urbanjost?tab=repositories)
are more appropriate if only a specific module is required, and can be
found as additional repositories at the GPF site.

But if you want to work with the entire repository this is where new
development (and a lot more functionality) is concentrated!

### Cloning the repository
```bash
    git clone https://github.com/urbanjost/general-purpose-fortran
    cd general_purpose-fortran/src
    # you might have to change "-lncurses" to "-lncurses -ltinit"
    # depending on how ncurses(3c) was built on your machine, and
    # have ncurses(3c), readline(3c), sqlite3(3c), X11(3c) and lua(3c) 
    # installed for all the interfaces to build, but the most 
    # commonly used interfaces should build without problems.

    make -k
```
### for building optional GPF
To build some of the optional components you may have to install several
packages. Starting from a base MINT 20.1 platform (2021-03-07), for example:
```
   sudo apt-get install gfortran
   sudo apt-get install libx11-dev
   sudo apt-get install sqlite3 libsqlite3-dev
   sudo apt-get install libcurl4-gnutls-dev 
   sudo apt-get install libreadline libreadline-dev
```

### If interested in the graphics routines ...

you might want to edit hershey.sh to select where hershey
font files will be installed (default is /usr/share/hershey), 
and then enter ...

    bash hershey.sh

if you do not have bash(1) or a Posix shell you will have to run
the h2v program for each font, as illustrated by the simple
hershey.sh script.

## Dependencies

By default, the graphics routines require X11, the ncurses interface
assumes libncurses is installed, the SQLite3 interface needs the
libsqlite3 library, and the readline interface needs libreadline.
These libraries are commonly available on many platforms but you may
have to remove some of the files from Makefile 
if you do not want to build those interfaces.

## Overview

The GPF (General-Purpose Fortran) repository consists of  Fortran modules
and libraries (and in some cases their associated C support routines)
providing

*  __command line parsing__
*  __string operations__
*  a __date and time__ module
*  a module that supports basic integrated __unit testing__ 
*  simple __numeric expression parsing__
*  __command line history__ editing
*  The low-level __M_draw__ graphics library, and the beginnings of a
   __pixel graphics module__ -- both similar to the VOGLE graphics library interface
*  libraries for __message handling__ and __debugging__
*  a growing number of Fortran interfaces to common C routines, including
   +  many __POSIX system routines__
   +  a module for reading and writing lines to a process (a __popen(3c)__ interface)
   +  an interface to the __ncurses(3c)__ terminal screen handling library
   +  an interface to the __sqlite3(3c)__ SQL file library
   +  an interface to the __regex(3c)__ Regular Expression library
   +  routines for controlling the X11 Windows attributes of an xterm(1) terminal emulator,
      including the program __setxt__(1)

The majority of these routines come with a man(1) page in *roff and HTML format.
An index into the documentation is contained in this distribution and available on the home page
   + [[overview]](https://urbanjost.github.io/general-purpose-fortran/index.html)
   + A [collection of programs](https://urbanjost.github.io/general-purpose-fortran/man_1.html) that use the
     repository code are included. These range from useful utilities (a __pre-processor__, an
     __SCCS-style meta-data reader__, __regression testing utilities__, ...) to simple
     example programs that exercise the modules.

Code additions are strongly encouraged (This is intended to be a development of
the Fortran community).


The Fortran Wiki [fortranwiki.org ](http://fortranwiki.org) contains information on many Fortran
resources (If you do not see a link there to your favorites please add them.)

A modern Fortran compiler is assumed. This code has been tested using
gfortran GNU Fortran (GCC) 8.3.1 .

## Routine Categories and Descriptions

### command line arguments

[M\_KRACKEN(3fm)](https://urbanjost.github.io/general-purpose-fortran/BOOK_ARGUMENTS.html): command line
arguments

The M\_KRACKEN(3f) module makes cracking Unix-like arguments off the command
line easy. This version is expanded to allow use in configuration files and
interactive input as well, and incorporates additional modules. It requires
many other components from the GPF collection, and is used to generate
programs that use shell-like input as well as crack command lines.

[M\_ARGS(3fm)](https://urbanjost.github.io/general-purpose-fortran/M_args.3m_args.html) 

The M_args(3fm) module supports cracking Unix-like arguments off the
command line as a string that can be read as a NAMELIST, eliminating
the typical additional steps needed to convert the strings found on the
command line to other types.

[M\_GETOPT(3fm)](https://urbanjost.github.io/general-purpose-fortran/M_getopt.3.html): command line arguments

The M\_GETOPT(3fm) module by Mark Gates supports cracking Unix-like arguments
off the command line in much the same way as the C getopt(3c) procedure.

[M\_GETOPT\_LONG(3fm)](https://urbanjost.github.io/general-purpose-fortran/M_getopt_long.3.html): command line
arguments

The M\_GETOPT\_LONG(3fm) module by Joe Krahn supports cracking Unix-like
arguments off the command line in much the same way as the SunOS
getopt_long(3c) procedure.

### strings

[M\_STRINGS(3fm)](https://urbanjost.github.io/general-purpose-fortran/BOOK_M_strings.html)

convert case, change numbers to strings, strings to numbers, parse on
delimiters,edit strings, ... .

### date and time

[M\_TIME(3fm)](https://urbanjost.github.io/general-purpose-fortran/BOOK_M_time.html)

Calculate and display date and time values

### expression parsers

[M\_CALCULATOR(3fm)](https://urbanjost.github.io/general-purpose-fortran/M_calculator.3.html),

M\_calculator(3fm) is a module for evaluating expressions. The primary routine
is JUCALC(3f). JUCALC(3f) evaluates simple numeric and string expressions.
This allows your input to use expressions and variable names instead of simple
values. You will have to comment out or supply some of the functions called,
depending on how f2003-compliant your compiler is.

### command line recall and editing

[M\_HISTORY(3fm)](https://urbanjost.github.io/general-purpose-fortran/redo.3.html): An interactive input editor
module

Acting much like a line-mode editor, the REDO(3f) procedure lets you list,
edit, save, and modify your interactively entered program input. Built-in help
and no dependence on terminal control sequences makes this a simple-to-master
and portable input history editor.

[ M\_READLINE(3fm)](https://urbanjost.github.io/general-purpose-fortran/M_readline.3.html): calling readline(3c)
from Fortran

The readline(3c) routine lets you edit and then execute previously entered
commands. An interface using a small C routine and the standard ISO_C_BINDING
module lets the commonly available C routine be called from Fortran. The
readline(3c) library is required.

### messages

[M\_JOURNAL(3fm)](https://urbanjost.github.io/general-purpose-fortran/M_journal.3.html),
[M\_MESSAGES(3fm)](https://urbanjost.github.io/general-purpose-fortran/M_messages.3.html)

journaling, logging, and general messaging routines that let you have a single
routine filter output to journal files; display attention-grabbing messages or
reformat messages.

### unit testing and debugging

[M\_DEBUG(3fm)](https://urbanjost.github.io/general-purpose-fortran/BOOK_M_debug.html)

These routines are useful for constructing unit tests for code and for adding
debug modes to routines and applications.

[Paranoid Compiler Test](https://urbanjost.github.io/general-purpose-fortran/paranoid.1.html)

The PARANOID program converted into subroutines that can be called from your
programs to verify compiler behavior.

### flow control

[M\_LOGIC(3fm)](https://urbanjost.github.io/general-purpose-fortran/M_logic.3.html): conditionally select input

The M\_LOGIC(3f) module allows you to use if/else/elseif/endif directives in
your input; allowing conditional selection of input lines. Requires an
expression parser. It uses JUCALC(3f) by default.

### I/O

[ M\_IO(3fm)](https://urbanjost.github.io/general-purpose-fortran/M_io.3m_io.html)

Input/Output

### hot keys

[GETKEY(3f)](https://urbanjost.github.io/general-purpose-fortran/system_getkey.3.html): read a character from the
keyboard in "immediate mode"

A simple C routine for most Unix and GNU/Linux systems that immediately reads
a single character keystroke from the keyboard without waiting for a carriage
return. An interface using the ISO_C_BINDING interface allows the routine to
be called from Fortran.

### Fortran/C calls

##### Read or Write from a system process

[M\_PROCESS(3fm)](https://urbanjost.github.io/general-purpose-fortran/M_process.3.html)

Read and write from a process using popen(3c)

##### Many POSIX interface routines

[M\_SYSTEM(3fm)](https://urbanjost.github.io/general-purpose-fortran/BOOK_M_system.html)

  * Environment
  * Directories
  * File status and attributes
  * Error code
  * Process management

And other commonly called C system routines interfaced to Fortran. Makes heavy
use of the ISO_C_BINDING module introduced as part of Fortran 2003.

##### Call the POSIX Regular Expression library

[M\_REGEX(3fm)](https://urbanjost.github.io/general-purpose-fortran/M_regex.3.html)

Call the POSIX regular expression library.

#### Fortran(2008) interface to the SQLite3 library

[M\_SQLITE(3fm)](https://urbanjost.github.io/general-purpose-fortran/M_sqlite.3.html)

A basic interface to the SQLite3 library using the ISO_C_BINDING module and C/Fortran interoperability
modeled iniitially on the FLIB interface.

#### Fortran(2003) interface to the ncurses(3c) library

[M\_NCURSES(3fm)](https://urbanjost.github.io/general-purpose-fortran/M_ncurses.html),
[M\_FIXEDFORM(3fm)](https://urbanjost.github.io/general-purpose-fortran/M_fixedform.3.html)

A Fortran module and a few related files create an interface from Fortran to
the C ncurses(3c) library; allowing Fortran programs to read function keys and
characters one at a time and to control the characters on the screen on a
character-cell by character-cell basis.

File characteristics

Input/Output

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

[M\_COMPARE_FLOAT\_NUMBERS(3fm)](math/Compare_Float_Numbers/M_Compare_Float_Numbers.HTML)

A Fortran module containing routines to perform equality and relational
comparisons on floating point numbers. That is, you can more safely compare
real numeric values.

[Types and Kinds](types/Type_Kinds.HTML)

Fortran KIND definitions used by other parts of the basic utilities

[ACCDIG(3f)](https://urbanjost.github.io/general-purpose-fortran/accdig.3.html)

compare two real numbers only up to a specified number of digits

### Graphics
[M\_DRAW(3fm)](https://urbanjost.github.io/general-purpose-fortran/BOOK_M_draw.html)

The M\_draw module is an interface to a C library based on the VOGLE graphics library. It is supplemented
by higher level modules.

[M\_PIXEL(3fm)](https://urbanjost.github.io/general-purpose-fortran/BOOK_M_pixel.html)

The M\_pixel module is a _PRELIMINARY_ collection of routines that use 2D-
vector routines to create or modify a pixel array, which can then be written
as a GIF file.

### Color

[M\_COLOR(3fm)](https://urbanjost.github.io/general-purpose-fortran/M_color.3.html)

The M\_color module is a collection of color-related procedures. Routines to
convert between different color models, return RGB values given common X11
color names, and other simple color-related operations.

### xterm(1) attributes

[M\_XTERM(3fm)](https://urbanjost.github.io/general-purpose-fortran/BOOK_M_xterm.html)

The M\_xterm module is a collection of routines that use escape sequences to set and query
xterm(1) X11 Windows attributes such as window size, background and foreground color, font,
and cursor color. The program setxt(1), included in the GPF distribution, allows for easily
using the module capabilities from the command line.

### Sort

[ M\_SORT(3fm)](https://urbanjost.github.io/general-purpose-fortran/M_sort.3.html)

sorting routines encapsulated in a module

[ sort_shell(3f)](https://urbanjost.github.io/general-purpose-fortran/sort_shell.3.html)

simple generic sort procedure

[ M\_SWAP(3fm)](https://urbanjost.github.io/general-purpose-fortran/swap.3.html)

swap two variables

### General Mathematics

[ M\_MATH(3fm)](https://urbanjost.github.io/general-purpose-fortran/BOOK_M_math.html)

A collection of routines for basic geometry, curve fitting and interpolation,
basic statistics, ...

### Unit Conversions

[ M\_UNITS(3fm)](https://urbanjost.github.io/general-purpose-fortran/BOOK_M_units.html)

A collection of unit conversions and constants. Allow degrees instead of
radians in trig functions; convert between Celcius and Fahrenheit, ...

An example program that combines many of the components into a simple
calculator-like utility that parses command line arguments, evaluates Fortran-
like expressions, draws in pixel arrays, allows calls to ncurses, has
interactive command history recall and editing, and supports
if/else/elseif/endif directives can be found in
[shell.ff](EXE/SHELL/shell.ff).

* * *
## Experimental
 
 + GPF is bundled as an `fpm` package
## SUPPORTS FPM ![fpm](docs/images/fpm_logo.gif)

Alternatively, download the github repository and build it with 
fpm ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )

```bash
     git clone https://github.com/urbanjost/general-purpose-fortran.git
     cd general-purpose-fortran
     fpm test  # build and then run unit tests
```

or just list it as a dependency in your fpm.toml project file.

```toml
     [dependencies]
     general-purpose-fortran = { git = "https://github.com/urbanjost/general-purpose-fortran.git" }
```
 + A `ford` configuration file is available to build documentation
 + A `doxygen` configuration file is available to build documentation

<!--
==================================================================================================
At least recently, Fortran has centered around solving large
numerically-intensive problems in STEM-related fields; often in parallel,
particularly on large HPC platforms.  And this work was done almost
exclusively with commercial compilers. Where once Fortran was available
on nearly every platform (usually with many extensions provided on a
per-platform basis) it became something primarily available in these
somewhat elite environments.

Then came the open-source compilers G95 and subsequently gfortran, and
more recently several previously commerical-only compilers from Intel,
NAG, Nvidia and new open-source projects. And newer standards standardized
C interfaces and added back a few of the common features once found in
almost all compilers via extensions.

So Fortran is once again available almost everywhere. But as critical as
it is for many fields it is rarely taught, as instructors generally prefer
teaching the most general programming tools, as they can be useful in many
different disciplines.

But Fortran is just as capable at its core of doing general functions as
any language now, given it's standarized C interface and new modern features.

But, largely due to historical reasons, there still is no standard library
for such tasks as command-line parsing, string functions, date-and-time
manipulation and display, interfacing to the C POSIX system routines, ...
==================================================================================================
 -->
