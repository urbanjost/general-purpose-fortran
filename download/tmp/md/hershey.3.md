[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                               Manual Reference Pages  - hershey (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    hershey(3f) - [M_pixel] draw text string as Hershey software vector fonts

CONTENTS

    Synopsis
    Options
    Description
    Example

SYNOPSIS

    definition:


       subroutine hershey(x,y,height,itext,theta,ntext)
       character(len=*),intent(in)   :: itext
       real,intent(in)               :: x,y
       real,intent(in)               :: height
       real,intent(in)               :: theta
       integer,intent(in)            :: ntext



OPTIONS

             X,Y are the coordinates in inches from the current origin to the lower left corner of the 1st character to be plotted.
             If either is set to 999.0 then saved next character position is used. HEIGHT is the character height in inches

             ITEXT contains the text to be plotted

             THETA is the positive CCW angle W.R.T. the X-axis

             NTEXT is the number of characters in itext to plot

             o    If NTEXT.lt.-1 the pen is down to (X,Y) and a single special centered symbol is plotted.

             o    If NTEXT.eq.-1 the pen is up to (X,Y) and a single special centered symbol is plotted.

             o    if NTEXT=0 a single Simplex Roman character from ITEXT, left-justified, is plotted.

             o    if NTEXT.gt.0 NTEXT characters from ITEXT are decoded and NCHR characters are plotted where NCHR.le.NTEXT to
                  remove backslashes, command codes, etc.

DESCRIPTION

    FEATURES:

        1)   Four HERSHEY letter fonts--SIMPLEX,COMPLEX,ITALIC, and DUPLEX-- are provided in upper and lower case ROMAN

        2)   Two hershey letter fonts--SIMPLEX and COMPLEX--are provided in upper and lower case GREEK

        3)   47 special mathematical symbols, e.g. integral sign, del... are provided

        4)   SUPER- and SUB-scripting is possible within a character string without separate calls to HERSHEY

    Change of font is made by enclosing the name of the font in upper case in backslashes, e.g \SIMPLEX\. Three letters suffice to
    specify the font. SIMPLEX is the default font on the initial call to HERSHEY. A font remains in effect until explicitly
    changed. SUPER- or SUB-scripting is accomplished by enclosing the expression to be SUPER- or SUB-scripted in curly brackets and
    preceding it by SUP or SUB. the closing curly bracket terminates the SUPER- or SUB-scripting and returns to normal character
    plotting. Note that SUPER- and SUB-script letters are plotted with a different character size.

    GREEK letters are drawn by enclosing the ENGLISH name of the letter in backslashes, e.g. \ALPHA\. The case of the first letter
    determines the case of the GREEK letter. The closing backslash must be included.

    Any symbol may be called by enclosing the symbol number+1000 in backslashes. This is the only way to call some symbols,
    especially special mathematical symbols.

The symbol numbers are

    1-26    upper case ROMAN SIMPLEX

       27-52 lower case ROMAN SIMPLEX

       53-72 SIMPLEX numbers and symbols

       73-96 upper case GREEK SIMPLEX

       97-120 lower case GREEK SIMPLEX

       121-146 upper case ROMAN COMPLEX

       147-172 lower case ROMAN COMPLEX

       173-192 COMPLEX numbers and symbols

       193-216 upper case GREEK COMPLEX

       217-240 lower case GREEK COMPLEX

       241-266 upper case ROMAN ITALIC

       267-292 lower case ROMAN ITALIC

       293-312 ITALIC numbers and symbols

       313-338 upper case ROMAN DUPLEX

       339-364 lower case ROMAN DUPLEX

       365-384 DUPLEX numbers and symbols

       385-432 special mathematical symbols

    Additional features added Feb 1982:

    The pen may be moved back to the start point for the previous character by \BS\. This is useful, for example, in writing
    integral signs with limits above and below them.

    Symbol parameters taken from N.M.Wolcott, FORTRAN IV Enhanced Character Graphics, NBS

    A.CHAVE IGPP/UCSD Aug 1981, Modified Feb 1982 by A. Chave, R.L. Parker, and L. Shure

    programmed in FORTRAN-77

EXAMPLE

    Show all Hershey characters

       program demo_hershey
       use M_pixel
       use M_writegif_animated, only : write_animated_gif
       use M_strings, only : v2s
       integer,parameter :: isize=600
       integer,parameter :: topsym=432
       integer  :: movie(1:topsym,0:isize-1,0:isize-1)
       !! set up environment
       call prefsize(isize,isize)
       call vinit()
       call ortho2(-150.0,150.0,-150.0,150.0)


       !! draw all characters using hershey numeric strings
       do i=1,topsym
          !! draw reference circle and crosshairs
          call color(0)
          call clear()


          call color(4)
          call linewidth(100)
          call circle(0.0,0.0,75.0)
          call move2(-75.0,0.0)
          call draw2(75.0,0.0)
          call move2(0.0,-75.0)
          call draw2(0.0,75.0)


          call centertext(.true.)
          call color(7)
          call linewidth(500)
          call textang(3.0*i)
          call textang(0.0)
          call move2(0.0,0.0)
          call textsize(150.0,150.0)
          call drawstr( #146;//v2s(i+1000)// #146;)


          call centertext(.false.)
          call color(1)
          call move2(-120.0,120.0)
          call textsize(10.0,10.0)
          call linewidth(40)
          call drawstr(v2s(i+1000)//   )
          movie(i,:,:)=P_pixel
       enddo
       call vexit()
       !! write to file and display with display(1)
       call write_animated_gif( hershey.3.gif ,movie,P_colormap,delay=40)
       !call execute_command_line( display hershey.3.gif )
       end program demo_hershey



-----------------------------------------------------------------------------------------------------------------------------------

                                                            hershey (3)                                               July 02, 2017

Generated by manServer 1.08 from b15f60ad-774a-4d0c-b29e-d1a395ea16dc using man macros.
                                                             [hershey]
