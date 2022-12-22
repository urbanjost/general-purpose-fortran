# General-Purpose Fortran Repository![GPF](docs/images/GPF.gif)


The majority of the documentation is in HTML at the
[Documentation](https://urbanjost.github.io/general-purpose-fortran/docs/index.html)
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
   + [[overview]](https://urbanjost.github.io/general-purpose-fortran/docs/index.html)
   + A [collection of programs](https://urbanjost.github.io/general-purpose-fortran/docs/man1.html) that use the
     repository code are included. These range from useful utilities (a __pre-processor__, an
     __SCCS-style meta-data reader__, __regression testing utilities__, ...) to simple
     example programs that exercise the modules.

Code additions are strongly encouraged (This is intended to be a development of
the Fortran community).


The Fortran Wiki [fortranwiki.org ](http://fortranwiki.org) contains information on many Fortran
resources (If you do not see a link there to your favorites please add them.)

A modern Fortran compiler is assumed. This code has been tested using
gfortran GNU Fortran (GCC) 10.3.0 and ifort 2021.8.0


---
![fpm](docs/images/fpm_logo.gif)
---

## SUPPORTS FPM

GPF is bundled as an `fpm` package.  Download the github repository and
build it with fpm
( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )

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
