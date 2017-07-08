[CLI Home Page]

NAME
    M_io(3f) - [M_io]Fortran I/O module
SYNOPSIS
    use M_io, only : slurp, notopen, print_inquire, uniq use M_io, only : dirname
DESCRIPTION
    The M_io module is a collection of routines related to basic I/O (input/output) operations.
      + SLURP - read a file into a character array
      + NOTOPEN - find unused FUN/LUN file-unit-number
      + PRINT_INQUIRE - run INQUIRE(3f) on a file by name or number and print results
      + UNIQ - add numeric suffix to filename if needed till filename does not exist
      + DIRNAME - return the directory portion of a pathname
      + SPLITPATH - split Unix pathname into directory, name, basename and extension
      + ISDIR - tell if a path is an existing directory on Unix-compatible systems
      + READLINE - read line as long as programming environment line length limit

