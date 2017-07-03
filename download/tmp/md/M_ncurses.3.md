-----------------------------------------------------------------------------------------------------------------------------------

A public-domain Fortran(2003) interface to the ncurses(3c) library

The ncurses(3c) library lets you control your terminal screen on a character-cell by character-cell basis. It is often described as
a "CRT screen handling and optimization package". The ncurses(3c) library is C-based and does not supply a Fortran interface. The
following is a description of public-domain files that comprise such a Fortran/C interface for ncurses(3c).

This is a screen shot from a Fortran program running in an xterm(1) window that uses ncurses(3c):

[example]

Note that this interface does not yet support all the ncurses(3c) extensions to the curses(3c) interface, such as

  * "Wide" (ie. UNICODE) character support.
  * the forms, menus, and panels widget extensions

System Requirements

  * The ncurses(3c) library must be installed on your system
  * Your Fortran compiler must support the Fortran-2003 ISO_C_BINDING interface

The interface has recently (20150113) been tested using

  * GNU Fortran/C (ie. gfortran(1) and gcc(1)) version 4.8.3, and ncurses(3c) version 5.9
  * Intel Composer 14.0.3, and ncurses(3c) version 5.9

Download Files:

All the files, including the examples are contained in

  * ncf.tgz

Or you can browse the principal files

  * ncurses.f90 (the main Fortran module)
  * macros.c (required auxiliary C routines)
  * UNLICENSE.txt (the License file)

Other CLI-related materials can be found in the libjust4 library.

You can typically get a description of all the ncurses(3c) C routines on Linux and Unix platforms by entering

  man ncurses

or see the Wikipedia entry or main web page for ncurses(3c):

   http://invisible-island.net/ncurses/

Examples

The examples should help considerably to show the differences in the Fortran interface for those familiar with the C interface. To
get started the most significant differences are:

  * Functions must always be assigned to a variable even if the return value is unused. For example:

            move(Y,X);      /* C usage */
            ierr=move(Y,X)  ! Fortran Usage


  * The printw(3c) family of routines has limited support. Only one REAL, one CHARACTER string or one or two INTEGER values should
    be printed per call. Note that the C escape-syntax strings such as /n will not work, but that characters such as C_NEW_LINE are
    defined in the ISO_C_BINDING interface that can be used to produce the same results.
  * Strings need terminated with the ISO_C_BINDING character C_NULL_CHAR
  * Variables passed to the routines should be C-interoperable (ie. declared with a KIND that is listed in the ISO_C_BINDING
    definition) to be portable. Note that some of the examples do not strictly follow that suggestion, as INTEGER and INTEGER
    (C_INT) and REAL and REAL(C_FLOAT) are often the same on common platforms.
  * getwin(3f) and putwin(3f) take a filename instead of a C "FILE*" argument.

To try the interface start with a simple program like the "Hello World!" example below and try the following commands (or their
equivalent for your programming environment):

   # GNU compiler
   gcc -c macros.c  # build auxiliary C routines
   gfortran -J. -c ncurses.f90
   gfortran -I. -J. hello_world.f90 ncurses.o macros.o -lncurses -o hello_world
   ./hello_world

   # Intel compiler
   icc -c macros.c  # build auxiliary C routines
   ifort -I. -c ncurses.f90
   ifort -I. hello_world.f90 ncurses.o macros.o -lncurses -o hello_world
   ./hello_world

Fortran "Hello World" Example

!===============================================================================
program hello_world ! @(#) HELLO WORLD program using ncurses(3c) from Fortran
   use M_ncurses      ! load interface to ncurses(3c) C library
   !mplicit none
   integer :: ierr, ikey
   stdscr=initscr()                           ! Start curses mode
   ierr=addstr("Hello World!!!"//C_NULL_CHAR) ! Print Hello World
   ierr=refresh()                             ! update the real screen
   ikey=getch()                               ! Wait for a user keystroke
   ierr=endwin()                              ! End curses mode
end program hello_world

C "Hello World" Example

#include <ncurses.h>
int main()
{
        initscr();                      /* Start curses mode              */
        printw("Hello World !!!");      /* Print Hello World              */
        refresh();                      /* Print it on to the real screen */
        getch();                        /* Wait for user input */
        endwin();                       /* End curses mode                */
        return 0;
}

The following example programs show basic usage:

  * nc_hello_world.f90: HELLO WORLD program using ncurses(3c) from Fortran
  * nc_key_code.f90: basic example shows how to identify what keys were pressed
  * nc_acs_vars.f90: test Alternate Character Set (ACS) variable names (line box characters, ...)
  * nc_getwin.f90: read a window dump from putwin(3c) using getwin(3c)
  * nc_mouse.f90: how to get position and click information from mouse presses
  * nc_putwin.f90: use putwin(3f) to create a window dump to a file
  * nc_colo.f90: Simple color text program with more than 8 colors
  * nc_color_set.f90: setting color pairs
  * nc_colour.f90: Print out some ncurses colors.
  * nc_exp1.f90: draw a panel and fill it and show a subsection of it
  * nc_hline.f90: draw horizontal lines using mvhline(3f)
  * nc_init_func_example.f90: Capture all key strokes and identify what keys were pressed
  * nc_marquee.f90: example showing character inserts
  * nc_printw_example.f90: simple printw-family calls to place text on the screen
  * nc_scanw_example.f90: read a string from the screen
  * nc_simple_attr.f90: ncurses example program that pages a C code and highlights comments
  * nc_simple_color.f90: Simple color text program
  * nc_simple_key.f90: draw a simple menu
  * nc_temp_leave.f90: temporarily leave screen mode
  * nc_termattrs.f90: test termattrs(3f) and query which attributes this terminal type takes
  * nc_wenclose.f90: basic mouse usage
  * nc_win_border.f90: draw box that can be moved around screen
  * nc_with_chgat.f90: change attributes of text on the screen

Print your ncurses(3c) pads and windows as HTML

A simple cut and paste of a plain terminal window can suffice for monochrome output not using box characters. but I find it far
easier (especially if you want to print a "pad" window, which can be larger than your display screen) to use the following
routines:

  * nc_printhtml.f90
  * nc_errmessage.f90
  * ncp.f90

The ncp.f90 sample program reads a file generated by ncurses(3c) programs with the putwin(3c) procedure and converts them to HTML.
Note this allows windows dumped from programs written in other languages to be dumped.

As an example, nc_printhtml(3f) was used to generate the following output from selected test programs:

-----------------------------------------------------------------------------------------------------------------------------------

ACS_ULCORNER:Upper left corner    ?    ACS_GEQUAL  :Greater/Equal sign   ?
ACS_LLCORNER:Lower left corner      ?  ACS_PI      :Pi                     ?
ACS_LRCORNER:Lower right corner   ?    ACS_NEQUAL  :Not equal            ?
ACS_URCORNER:Upper right corner     ?  ACS_STERLING:UK pound sign          -L-
ACS_LTEE    :Tee pointing right   ?
ACS_RTEE    :Tee pointing left      ?
ACS_BTEE    :Tee pointing up      ?
ACS_TTEE    :Tee pointing down      ?
ACS_HLINE   :Horizontal line      ?
ACS_VLINE   :Vertical line          ?
ACS_PLUS    :Crossover            ?
ACS_S1      :Scan Line 1            ?
ACS_S3      :Scan Line 3          ?
ACS_S7      :Scan Line 7            ?
ACS_S9      :Scan Line 9          ?
ACS_DIAMOND :Diamond                ?
ACS_CKBOARD :Stipple              ?
ACS_DEGREE  :Degree Symbol          DEG
ACS_PLMINUS :Plus/Minus Symbol    +-
ACS_BULLET  :Bullet                 ?
ACS_LARROW  :Arrow Pointing Left  ?
ACS_RARROW  :Arrow Pointing Right   ?
ACS_DARROW  :Arrow Pointing Down  ?
ACS_UARROW  :Arrow Pointing Up      ?
ACS_BOARD   :Board of squares     ?
ACS_LANTERN :Lantern Symbol         ?
ACS_BLOCK   :Solid Square Block   &block;
ACS_LEQUAL  :Less/Equal sign        ?


-----------------------------------------------------------------------------------------------------------------------------------

 Color      FD  FB  FW  BG  BD    BFD BFB BFW BBG BBN

 default    FD  FB  FW  BG  BD    FD  FB  FW  BG  BD
 BLACK      FD  FB  FW  BG  BD    FD  FB  FW  BG  BD
 RED        FD  FB  FW  BG  BD    FD  FB  FW  BG  BD
 GREEN      FD  FB  FW  BG  BD    FD  FB  FW  BG  BD
 YELLOW     FD  FB  FW  BG  BD    FD  FB  FW  BG  BD
 BLUE       FD  FB  FW  BG  BD    FD  FB  FW  BG  BD
 MAGENTA    FD  FB  FW  BG  BD    FD  FB  FW  BG  BD
 CYAN       FD  FB  FW  BG  BD    FD  FB  FW  BG  BD
 WHITE      FD  FB  FW  BG  BD    FD  FB  FW  BG  BD

  FD  = Front color on default background
  FB  = Front color on black background
  FW  = Front color on white background
  BG  = Front and background color
  BD  = Background color with default front color
  B?? = As above, with A_BOLD enabled

Hit any key to exit.



-----------------------------------------------------------------------------------------------------------------------------------

                                       ??
                                      ????
                                     ??????
                                    ????????
                                   ??????????
                                  ????????????
                                 ??????????????
                                ????????????????
                               ??????????????????
                              ????????????????????
                             ??????????????????????
                            ????????????????????????
                           ??????????????????????????
                          ????????????????????????????
                         ??????????????????????????????
                        ????????????????????????????????
                       ??????????????????????????????????
                      ????????????????????????????????????
                     ??????????????????????????????????????
                    ????????????????????????????????????????
                   ??????????????????????????????????????????
                  ????????????????????????????????????????????
                 ??????????????????????????????????????????????
                ????????????????????????????????????????????????
               ??????????????????????????????????????????????????
              ????????????????????????????????????????????????????
             ??????????????????????????????????????????????????????
            ????????????????????????????????????????????????????????


-----------------------------------------------------------------------------------------------------------------------------------

Type any character to see it in bold ('q' to quit)
Note on some keyboards you hit [FN][Function Key] to press a 'function key'
The pressed key value is 330, named function  dc: delete character
The pressed key value is 262, named function  home: home key
The pressed key value is 339, named function  ppage: previous page
The pressed key value is 338, named function  npage: next page
The pressed key value is 360, named function  end: end key
The pressed key value is 360, named function  end: end key
The pressed key value is 261, named function  right: Right arrow key
The key value is 19, a normal 'non-printable' called  DC3 which prints as^S
The pressed key value is 97, the regular character a
The pressed key value is 98, the regular character b
The pressed key value is 99, the regular character c
The pressed key value is 100, the regular character d
The pressed key value is 65, the regular character A
The pressed key value is 66, the regular character B
The pressed key value is 67, the regular character C
The key value is 32, a normal 'non-printable' called  SPACE which prints as 
The key value is 27, a normal 'non-printable' called  ESC which prints as^[
The pressed key value is 96, the regular character `
The pressed key value is 113, the regular character q



-----------------------------------------------------------------------------------------------------------------------------------

This terminal is capable of the following attributes:
 AltCharSet: Yes ?bcdeDEG+-???????????????????&block;123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
      Blink: Yes abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
       Bold: Yes abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
        Dim: No  abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
 Invis(ible):Yes abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
     Normal: Yes abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
    Reverse: Yes abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
   Standout: Yes abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
  Underline: Yes abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
    Protect: No  abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
 Horizontal: No  abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
       Left: No  abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
        Low: No  abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
      Right: No  abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
        Top: No  abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
   Vertical: No  abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
   Italic:   No  abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
Window size is 28 rows, 80 columns.
This terminal has insert/delete character abilities
This terminal has insert/delete line abilities
This terminal can do colors.
This terminal can change the standard colors.
This terminal's baud rate is 38400.



-----------------------------------------------------------------------------------------------------------------------------------

Click the left mouse around the screen. Asterisks should appear where you click
unless you click in the green bar, which should cause a beep instead.
The ENTER key exits.
                                   *   *
                                                  *
                           *                        *
                                    *         *
                                          *      *

                     *        *           
                                *             *      *
                                          
                     *     *                     *
                                          
                                           *
                                       *        *   *

                      *        *
                                     *    *       *

                                               *
                               *         *






-----------------------------------------------------------------------------------------------------------------------------------
