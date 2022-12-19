










!>
!!##PRE-AMBLE:
!!
!!    Turn back now if you did not come to this page specifically because
!!    you were already looking for a "CALCOMP-compatible" graphics library.
!!
!!    In general, CALCOMP is not recommended for developing large new codes.
!!
!!    Because I have had need to quickly restore or interface with codes
!!    that have CALCOMP interfaces in them it is handy for me to have a
!!    CALCOMP-compatible library and documentation around.
!!
!!    If you have a need similar to mine for this CALCOMP library note that
!!    many so-called "CALCOMP-compatible" libraries differed to various
!!    degrees from the "standard" (ie. the library supplied by CALCOMP). As
!!    graphics capabilities expanded the vendors often added routines to fill
!!    polygons with patterns; to provide clipping and viewporting options;
!!    to add color and line thickness control; to select hardware fonts and
!!    so on. Although the M_draw(3f) library being called by this CALCOMP
!!    emulator supports those features, this library only emulates the
!!    original CALCOMP library capabilities with a few exceptions (NFRAME,
!!    WIDTH, NEWPEN). That is, you may have to add a few routines if you
!!    need some of those additional features; or change your code to call
!!    M_draw(3f) directly.
!!
!!    This library calls vary low graphics primitives in another graphics
!!    library -- M_draw(3f), my variant of the VOGLE graphics library.
!!
!!    Alternate driver code is present (but not compiled by default by
!!    the build script) that does not use M_draw(3f), but instead writes
!!    a MegaTek/Liant TEMPLATE PDF (Pseudo-Device File) without using
!!    TEMPLATE. PDF files were a proprietary metafile which was prepared for
!!    specific output devices using TEMPLATE routines that converted PDFs to
!!    any of the TEMPLATE output devices (which probably ran to well over a
!!    hundred formats). Note that these file have nothing to do with Adobe
!!    Acrobat PDF files. Unless you somehow still have a TEMPLATE license,
!!    this code is of historical interest only.
!!
!!    This code originated as FORTRAN 66 code circa 1969, and began with
!!    original CALCOMP code snippets (which were provided with permission
!!    for modification). "If it ain't broke don't fix it" ...
!!
!!##COMPATIBILITY ISSUES:
!!
!!    "CALCOMP" graphics libraries were a de-facto standard graphics
!!    interface around the world when graphics was done mostly on
!!    pen-plotters; but there were many variants. Because of these
!!    differences, you may very well have to make simple routines of your own
!!    that call these routines. So, to avoid conflicts with your routines
!!    and other libraries all user-callable routines in this library have
!!    had the prefix C_ added to their name.
!!
!!    Other than that, this library is called in a way that is identical
!!    to the original CALCOMP library except for a few minor changes
!!    made originally to call the commercial graphics library "TEMPLATE"
!!    (instead of generating a file of CALCOMP plotter commands), as noted
!!    in the documentation.
!!
!!    Note that the most common point of divergence between
!!    "CALCOMP-compatible" libraries was whether a string was a packed
!!    integer array of various word sizes (ie. "Hollerith") or an array of
!!    characters or a character string.
!!
!!##HISTORY
!!
!!     Originally the CALCOMP library was a simple graphics library that
!!     created files composed of instructions that drove a very popular
!!     family of pen plotters made by CALCOMP. The library was supplied
!!     by the manufacturer.  This made it easier for CALCOMP customers
!!     to create plots on these old vector-based monochrome (then color)
!!     pen plotters using Fortran 66 or 77 (which was the most popular
!!     programming language for graphics). For the most part, codes
!!     generated plots not by writing specific standardized metafiles such
!!     as PostScript or CGM or SVG or even pixmap files, but by calling
!!     the CALCOMP-supplied library.
!!
!!     Because of the initial dominance of CALCOMP in the plotter market
!!     other vendors made plotters compatible with CALCOMP plotters, or
!!     supplied a library with similar routines that would drive their
!!     plotters.
!!
!!     The CALCOMP calls thus became a de-facto standard for quite some time.
!!
!!       + Many graphics products did not even produce graphics directly but
!!         assumed "a CALCOMP-compatible" library was available for customers
!!         to hook their package to.
!!       + It was assumed a programmer was available to interface the
!!         plotter to codes and utilities that needed to use it.
!!       + Almost every plotter vendor provided or sold a
!!         "CALCOMP-compatible" library that would drive their specific device.
!!
!!     The advent of efficient and affordable raster graphics and PostScript
!!     and 3-D graphics ultimately made this simple vector-based 2-D graphics
!!     library inadequate for many graphics applications and its use faded.
!!
!!       + Created: 19920213
!!##LICENSE
!!    Public Domain
!>
!!##NAME
!!    M_calcomp(3fm) - [M_calcomp::INTRO] emulate old Calcomp graphics library
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!   THE CALCOMP GRAPHICS LIBRARY USER GUIDE
!!
!!   This is an interface that closely emulates a very early de-facto
!!   graphics standard called the "CALCOMP-compatible library" and is
!!   generally used to interface to older utilities that support CALCOMP
!!   interfaces, or to quickly resurrect codes that have CALCOMP calls
!!   in them. It is not recommended for new large code development.
!!
!!   The CALCOMP library is a simple set of FORTRAN callable graphic routines
!!   that allows users to quickly construct plots. It was historically
!!   used principally to interface to purchased vendor software that often
!!   supplied a "CALCOMP library interface", and for quick development of
!!   codes that generated XY plots (that is right -- products often could not
!!   produce graphics without being hooked up to the customer custom plotting
!!   interfaces!).
!!
!!   Consult the supplement at the end of this guide for specific guidelines
!!   on how to convert existing user and vendor CALCOMP code.
!!
!!   Revision 1.0.0: 07/01/91
!!
!!##TABLE OF CONTENTS
!!
!!  The following sections are available ....
!!
!!  INTRODUCTION
!!
!!  CALCOMP BASIC SOFTWARE
!!
!!   PLOT    - Move or draw to specified point,
!!             establish plot origin, update pen position
!!             and terminate plotting
!!   PLOTS   - Initialization, specify output file unit number
!!   FACTOR  - Adjusts the overall size of the plot
!!   WHERE   - Returns current pen location
!!   NFRAME  - Ends current frame and re-origins pen position
!!   SYMBOL  - Plots annotation (text) and special symbols
!!   NUMBER  - Plot decimal equivalent of a floating point number
!!   SCALE   - Determine starting value and scale for an array
!!             to be plotted on a graph
!!   AXIS    - Draws an annotated linear graph axis
!!   LINE    - Scale and plot a set of X,Y values
!!   NEWPEN  - Select new pen color
!!   WIDTH   - Set line thickness
!!
!!   Sample Plotting Program
!!
!!  CALCOMP GENERAL FUNCTIONAL SOFTWARE
!!
!!   CIRCL   - Draws a circle, arc, or spiral
!!   ELIPS   - Draws an ellipse or elliptical (or circular) arc
!!   DASHL   - Draws dashed line connecting a series of data points
!!   DASHP   - Draws a dashed line to a specified point
!!   FIT     - Draws a curve through three points
!!   GRID    - Draws a linear grid
!!   POLY    - Draws an equilateral polygon
!!   RECT    - Draws a rectangle
!!
!!  CALCOMP SCIENTIFIC FUNCTIONAL SOFTWARE
!!
!!   CURVX   - Draws a function of X over a given range
!!   CURVY   - Draws a function of Y over a given range
!!   FLINE   - Draws a smooth curve through a set of data points
!!   SMOOT   - Draws a smooth curve through sequential data points
!!   SCALG   - Performs scaling for logarithmic plotting
!!   LGAXS   - Plots an annotated logarithmic axis
!!   LGLIN   - Draws data in either log-log or semi-log mode
!!   POLAR   - Draws data points using polar coordinates
!!
!!  APPLICATIONS ROUTINES
!!
!!   CNTOUR  - Makes a contour plot
!!
!!  CALCOMP CODE MIGRATION SUPPLEMENT
!!
!!##INTRODUCTION
!!
!!    This user guide describes the calling sequences and arguments for the
!!    FORTRAN-callable CALCOMP software subroutines. The routines do not
!!    produce a device dependent CALCOMP file but rather call the M_draw(3f)
!!    graphics module.
!!
!!    CALCOMP divides their routines into three categories:
!!
!!     Basic, General Function, and Scientific Function.
!!
!!     Differences between the implemented routines and the standard CALCOMP
!!     routines are:
!!
!!      o All coordinate values should be greater than or equal to zero, and
!!        less than 100 inches. Values outside this range may result in
!!        unpredictable results (Negative values are possible if the
!!        frame coordinate origin is set first using the PLOT call).
!!
!!      o The metalanguage output filename is "pdf", and uses FORTRAN
!!        unit 50 unless an appropriate alternate value is specified
!!        in the PLOTS routine call. The output filename may be specified
!!        using the environment variable CALCOMP_PDF.
!!
!!      o A routine NFRAME is available for creating multiple frames for
!!        graphic devices other than pen plotters.
!!
!!      o Color is supported via the NEWPEN routine
!!
!!      o Line thickness is supported via the WIDTH routine
!!
!!      o Frames will not plot to true inches unless specific steps are
!!        taken in the generation and post-processing of the plot file.
!!
!!  Other changes may be needed in existing CALCOMP code from vendors as
!!  CALCOMP has produced several versions of CALCOMP routines that vary in
!!  such ways as use of CHARACTER variables versus Hollerith, the number
!!  of parameters on SYMBOL calls, and the current pen position after a
!!  call to SYMBOL.
!!
!!  The CALCOMP subroutines were written for use with CALCOMP pen plotters
!!  and originally worked in units of inches for the mapping of the
!!  plot directly to the output device. There are two classes of CALCOMP
!!  subroutines--those that accept user units and scale them to inches and
!!  those that require data to be directly in units of inches.
!!
!!  Table 1 lists the CALCOMP subroutines that fall into each class.
!!
!!  The main difference CALCOMP users will notice when using this CALCOMP
!!  library is that when the CALCOMP subroutines were incorporated into
!!  M_DRAW(3fm) the meaning of CALCOMP inches was altered to no longer mean
!!  a physical inch but just a unit-less measure (since M_DRAW(3fm) uses
!!  device-independent space and the graphics post processing procedures
!!  produce output for a number of graphics devices, some of which have a
!!  limited device space unlike pen plotters). THIS DIFFERENCE IS USUALLY
!!  ONLY OF SIGNIFICANCE TO USERS TRYING TO PRODUCE PLOTS USING TRUE INCHES.
!!
!!  The graphics post processing procedures use the CALCOMP inches to
!!  determine the aspect ratio of the plot, and the plot is made as large as
!!  possible for a given device while maintaining the aspect ratio specified
!!  by the user CALCOMP calls. A parameter called SIZE is included with
!!  most graphics post-processor procedures which facilitates the scaling
!!  of plots to a specific size in inches. An example program shows how to
!!  use these parameters to get consistent frames in as close as possible
!!  to true inches.
!!
!!##TABLE 1
!!
!!  Scaling versus Device units
!!
!!   >    Routines Which                    Routines Which Require
!!   >    Perform Scaling                           Inches
!!   > of User Data to Inches         (Data Must be Scaled to Inches)
!!   > ______________________          _______________________________
!!   >
!!   >         SCALE                               PLOT
!!   >         AXIS                                WHERE
!!   >         LINE                                SYMBOL
!!   >         DASHL                               NUMBER
!!   >         FLINE                               CIRCL
!!   >         SCALG                               ELIPS
!!   >         LGAXS                               DASHP
!!   >         LGLIN                               FIT
!!   >         POLAR                               GRID
!!   >                                             POLY
!!   >                                             RECT
!!   >                                             CURVX
!!   >                                             CURVY
!!   >                                             SMOOT
!!
!!##CALCOMP BASIC SOFTWARE
!!
!!     The routines included in the CALCOMP Basic Software category are
!!     PLOT, PLOTS, FACTOR, WHERE, SYMBOL, NUMBER, SCALE, AXIS, LINE,
!!     WIDTH and NEWPEN. NFRAME, an enhancement, is included here because
!!     it performs a basic function.
!!
!!     Usually when examining existing CALCOMP code you will find it breaks
!!     down into two categories - that which produces XY plots and that
!!     which does almost everything in its own high-level routines and uses
!!     CALCOMP mostly just to draw lines with the PLOT command. Therefore
!!     you are likely not to need to be familiar with many of the CALCOMP
!!     routines described here.
!!
!!     The majority of graphic applications are intended to produce an
!!     XY-plot. Usually the production of these graphs requires only a
!!     combination of the routines PLOTS (initialize), SCALE, AXIS, LINE,
!!     NFRAME and PLOT (terminate). Additional text can be added with SYMBOL,
!!     and options such as frame borders and general line drawing might be
!!     added with PLOT calls.
!!
!!     When plotting requirements cannot be satisfied by using these
!!     subroutines, the code often calls the PLOT routine almost exclusively
!!     ( which basically draws a line or moves the pen directly in units
!!     of inches). This is often done by vendors so that it is very easy
!!     for them to interface to virtually any graphics library.
!!
!!     Two other routines are often found in programs that do not call the
!!     higher level routines (such as the axis and contour plot routines):
!!     follows:
!!
!!     FACTOR   Adjusts the overall size of a plot.
!!     WHERE    Returns the current pen location.
!!
!!##EXAMPLE
!!
!!
!!  A SAMPLE PLOTTING PROGRAM
!!
!!  To illustrate the use of the CALCOMP routines, a sample program is
!!  provided which will produce the graph shown below. The only assumption
!!  made is that the 24 pairs of TIME and VOLTAGE data values are contained
!!  in a file of 24 records.
!!
!!    program sample
!!    use M_calcomp
!!    !  Reserve space for 24 data values plus two additional locations
!!    !  required by the SCALE, AXIS, and LINE subroutines.
!!    dimension xarray(26),yarray(26)
!!    !  Perform initialization.
!!    call plots(0.0,10.0,0.0,10.0)
!!    !  Read 24 pairs of TIME and VOLTAGE from an input file into two arrays
!!    !  with names XARRAY and YARRAY.
!!    read (5,25)(xarray(i),yarray(i),i=1,24)
!!    25    format(2f6.2)
!!    !  Establish a new origin one-half inch higher than the point where the
!!    !  pen was initially placed so that the annotation of the TIME axis will
!!    !  fit between the axis and the edge of the plotting surface.
!!    call plot(0.0,0.5,-3)
!!    !  Compute scale factors for use in plotting the TIME values within a
!!    !  five-inch plotting area.
!!    call scale(xarray,5.0,24,1)
!!    !  Compute scale factors for use in plotting the VOLTAGE data values
!!    !  within a six-inch plotting area (i.e., the data pairs of TIME,
!!    !  VOLTAGE will plot within a five-by-six inch area).
!!    call scale(yarray,6.0,24,1)
!!    !  Draw the TIME axis (5 inches long), using the scale factors computed
!!    !  in statement 40 to determine the milliseconds at each inch along the
!!    !  TIME axis.
!!    call axis(0.0,0.0,'time in milliseconds',-20,5.0,0.0,xarray(25),xarray(26))
!!    !  Draw the VOLTAGE axis (6 inches long) using the scale factors
!!    !  computed in statement 50 to determine the voltage at each inch along
!!    !  the VOLTAGE axis.
!!    call axis(0.0,0.0,'voltage',7,6.0,90.0,yarray(25),yarray(26))
!!    !  Plot VOLTAGE vs TIME, drawing a line between each of the 24 scaled
!!    !  points and a symbol X at every other point.
!!    call line(xarray,yarray,24,1,2,4)
!!    !  Plot the first line of the graph title.
!!    call symbol(0.5,5.6,0.21,'performance test',inteq,0.0,16)
!!    !  Plot the second line of the graph title.
!!    call symbol(0.5,5.2,0.14,'ref. no. 1623-46',inteq,0.0,16)
!!    !  Terminate the plot.
!!    call nframe()
!!    !  Close the plot file.
!!    CALL PLOT(0.0,00.0,999)
!!    !  Terminate Program execution.
!!    end program sample
!!
!!##CALCOMP GENERAL FUNCTIONAL SOFTWARE
!!
!!  The routines included in the CALCOMP General Functional software category
!!  are CIRCL, DASHL, DASHP, ELIPS, FIT, GRID, POLY and
!!  RECT. These routines call the Basic routines and should be viewed
!!  as an extension of the Basic library rather than as a separate entity.
!!
!!##CALCOMP SCIENTIFIC FUNCTIONAL SOFTWARE
!!
!!  The routines included in the CALCOMP Scientific Functional software
!!  category are CURVX, CURVY, FLINE, LGAXS, LGLIN, POLAR,
!!  SCALG, and SMOOT. These routines call the Basic routines and
!!  should be viewed as an extension of the Basic library.
!!
!!##APPLICATION ROUTINES
!!
!!  The routines included in this category draw, on a single call, complete
!!  plots of types useful to engineers. They are not part of the software
!!  from CALCOMP, but they do use the Basic CALCOMP subroutines.
!!
!!    CNTOUR
!!
!!##CALCOMP MANPAGES
!!
!!  If the manpages have been installed properly, you should be able to
!!  list all the CALCOMP-related pages by entering
!!
!!    man -s 3m_calcomp -k .
!!
!!  There should be a directory in the source for the GPF (General Purpose
!!  Fortran) collection that contains a collection of example CALCOMP
!!  programs in
!!
!!    PROGRAMS/CALCOMP
!!
!!  You can list all the manpages sorted by section using
!!
!!    #!/bin/bash
!!    export MANWIDTH=80
!!    for NAME in $(man -s 3m_calcomp -k . |sort -k 4|awk '{print $1}')
!!    do
!!       man -s 3m_calcomp $NAME |col -b
!!    done
!!
!!##CALCOMP SETUP
!!
!!  Since this version of a CALCOMP-compatible library uses the M_draw(3f)
!!  graphic primitives, the same environment variables can be used to
!!  select the type and size of output. For example:
!!
!!    # where the M_draw(3f) font files are located
!!    export M_DRAW_FONTLIB=/usr/share/hershey
!!
!!    # X11
!!    # set output to Poskanzer pixel map format at specified size
!!    export M_DRAW_DEVICE='x11'
!!    # run a program
!!    demo_general
!!
!!    # There are many output formats available (Adobe PDF, PostScript, SVG, ...)
!!
!!    # POSKANZER ASCII FILES (one of the harder ones to use in this case)
!!    # set output to Poskanzer pixel map format at specified size
!!    export M_DRAW_DEVICE='p3 850 1100'
!!    # the name of the output file
!!    export M_DRAW_OUTPUT=calcomp.p3
!!
!!    # optionally set up the virtual size in inches of the calcomp drawing surface
!!    export CALCOMP_XMIN CALCOMP_XMAX CALCOMP_YMIN CALCOMP_YMAX
!!    CALCOMP_XMIN=0
!!    CALCOMP_XMAX=8.5
!!    CALCOMP_YMIN=0
!!    CALCOMP_YMAX=11
!!
!!    # run a program
!!    demo_general
!!    # split pixmap file into individual drawings
!!     csplit -f P3. -k calcomp.p3 '%^P3%' '/^P3/' '{999}' 2>&1 >/dev/null
!!
!!##CALCOMP SUPPLEMENT
!!
!!  MOVING EXISTING CALCOMP CODE
!!
!!  The CALCOMP plot library emulates the interface originally leased
!!  from California Computer Products, Inc; and had been available in
!!  a very similar form on the old CDC 7600 Super Computers. Of course,
!!  this similarity is intentional. This library is trying to provide a
!!  consistent programming environment wherever possible.
!!
!!  All of the subroutines from the 7600 version of the CALCOMP library
!!  have been included in this version; although plots generated will
!!  not always look exactly the same as those produced on the 7600s.
!!
!!  The CALCOMP library is interfaced to locally developed routines (called
!!  primitives) which produce plots using the M_DRAW(3fm) module. This
!!  allows CALCOMP-based code to generate output which can be sent to any
!!  supported M_DRAW(3fm) output device.
!!
!!  CALCOMP is not the recommended graphics package for major new program
!!  development.
!!
!!  CALCOMP is being provided to meet certain special requirements:
!!
!!    1. To facilitate the migration of user code that already uses a
!!       CALCOMP-like package to new machines.
!!    2. To support interfaces to non-inhouse code. Such code may
!!       often already support a set of CALCOMP-like calls.
!!    3. Applications where a simple portable interface is more important
!!       than powerful graphics capabilities.
!!
!!  There are no plans to provide local enhancements to CALCOMP, and
!!  capabilities such as high-level charting routines will
!!  not be made available with CALCOMP. Those involved in program
!!  conversions and development are urged to consider long-term graphics
!!  requirements in deciding which package to use (CALCOMP or an alternative).
!!
!!  The CALCOMP software was initially developed to drive only CALCOMP
!!  plotters. In general, the calls produced plots directly in inches
!!  (A call to draw a line one unit long produced a one-inch line on
!!  the plotter).
!!
!!  With the interfacing of CALCOMP to the M_DRAW(3f) module graphics
!!  system (which provides the ability to obtain output on a wide range of
!!  devices), the meaning of units in the CALCOMP library has undergone
!!  a change. CALCOMP Inches, therefore, may not translate directly
!!  into physical inches on a pen plotter.
!!
!!  Important differences exist between this CALCOMP and "standard" CALCOMP
!!  interfaces third-party software often provides interfaces to. The format
!!  of the following primary example program can be used as a guide as to
!!  how to nullify the affects of these differences.
!!
!!   DIFFERENCES FROM 7600 CALCOMP LIBRARIES
!!
!!   1. For subroutines SYMBOL, AXIS, and LGAXS, the parameter used to
!!      specify text or title information (IBCD) has been changed to be
!!      type CHARACTER to be consistent with ANSI 77 FORTRAN. Data for these
!!      arguments should be changed to be type CHARACTER (although use of
!!      a Hollerith string or INTEGER array may currently work, their use is
!!      not recommended, and there are no plans to support this usage).
!!
!!   2. For subroutine SYMBOL on the 7600s, there is a "STANDARD" call
!!      (used to plot a text string) and a "SPECIAL" call (used to plot a
!!      single symbol). To Accommodate CHARACTER data and both versions of
!!      the call to SYMBOL, the calling sequence was modified to have 7
!!      arguments. All programs being converted from the 7600
!!      -MUST- make this change to the call to SYMBOL.
!!
!!      The new calling sequence is
!!
!!            CALL SYMBOL(XPAGE,YPAGE,HEIGHT,IBCD,INTEQ,ANGLE,NCHAR)
!!
!!      Where XPAGE, YPAGE, HEIGHT, and ANGLE are defined as on the 7600s, and the
!!      user guide can be consulted for details of their use.
!!
!!      The last parameter NCHAR is used as a flag to specify whether a text string
!!      or a single symbol is being plotted. If NCHAR is less that zero, a single
!!      symbol is plotted regardless of the contents of IBCD. If NCHAR is equal to
!!      or greater than zero the string in IBCD is used (FAILURE TO SPECIFY THE
!!      PROPER VALUE FOR NCHAR, INTEQ OR IBCD WILL CAUSE ERRONEOUS RESULTS).
!!
!!      To use SYMBOL to plot text for titles, captions, or legends--
!!
!!         IBCD--Contains the text string as CHARACTER data.
!!
!!         INTEQ--Should be set to 999 .
!!                (THE ACTUAL VALUE IS NOT USED FOR ANYTHING.)
!!
!!         NCHAR--Is the number of characters in IBCD.
!!
!!      For example, the following call to SYMBOL will result in the characters
!!      'TITLE 10' being output beginning at X and Y coordinates of 1.0 .
!!
!!          CHARACTER GRLBL*8
!!          GRLBL = 'TITLE 10'
!!          CALL SYMBOL(1.0,1.0,0.14,GRLBL,999,0.0,8)
!!
!!      To use SYMBOL to plot a single symbol or character--
!!
!!         IBCD--  A dummy CHARACTER variable or string should be used
!!                 THE ACTUAL VALUE IS NOT USED FOR ANYTHING.)
!!
!!         INTEQ--Contains the INTEGER EQUIVALENT of the desired symbol.
!!                If INTEQ has a value of 0 (zero) through 14, a centered
!!                symbol (where XPAGE and YPAGE specify the center of
!!                the symbol) is produced. The symbol table is unchanged
!!                from that on the 7600s, so the table on page 2-10 of the
!!                7600 CALCOMP guide is still applicable.
!!
!!         NCHAR--Determines whether the pen is up or down during the move
!!                to XPAGE and YPAGE. (IT MUST BE NEGATIVE.)
!!
!!                When NCHAR is--
!!
!!                 -1, the pen is UP during the move.
!!                 -2 or less, the pen is DOWN during the move.
!!
!!      For example, the following call to SYMBOL will result in special symbol
!!      number 5 being output with its center at XY coordinates of (1.0,1.0).
!!
!!         CALL SYMBOL(1.0,1.0,0.14,DUMMY,5,0.0,-1)
!!
!!   3. Because of interfacing the CALCOMP routines to the device
!!      dependent M_DRAW(3fm)-based post-processing procedures, some limit for
!!      the maximum plot size had to be established. For the CALCOMP
!!      library, a plot frame is limited to a maximum size in either the
!!      X or Y direction of 100 "CALCOMP inches". (The actual frame size
!!      on a particular output medium is dependent on the method of post-
!!      processing and the device selected.)
!!
!!      Each plot frame is usually initialized by a call to subroutine PLOT with
!!      the third argument (IPEN) equal to -2 or -3. For example,
!!
!!           CALL PLOT(0.5,1.0,-3)
!!
!!      Says to move 0.5 inches in the X-direction and 1.0 inch in the Y-direction
!!      before establishing a new origin. When establishing a new origin, all
!!      offsets are included inside the frame boundary, and therefore, they are part
!!      of the plot frame size. If any X or Y coordinate value (Plus the appropriate
!!      offset) exceeds the 100 inch limit, results are unpredictable. In programs
!!      where X and Y coordinate values exceed the scaling limit, a call to the
!!      CALCOMP routine FACTOR may be used to scale down the plot size appropriately.
!!      Each plot frame is terminated by a call to subroutine NFRAME; no additional
!!      offset is added here.
!!
!!      Knowledge of the plot frame size in the X and Y directions will be needed to
!!      scale pen plots to actual inches when the device dependent post processing
!!      procedures are available. The following example is provided to assist in
!!      understanding how the frame size is determined.
!!
!!       >   PROGRAM CALTEST
!!       >   USE M_calcomp
!!       >   CALL PLOTS()           ! perform initialization
!!       >   CALL BORDER(8.5,11.0)  ! establish a consistent frame size
!!       >!  Calls to generate first plot go here
!!       >!  where all calls stay inside area established by border
!!       >!    .
!!       >!    .
!!       >   CALL NFRAME()          ! terminate first plot
!!       >   CALL BORDER(8.5,11.0)  ! establish a consistent frame size
!!       >!  In next plot negative values up to (-1,-2) are needed
!!       >   CALL PLOT(1.0,2.0,-3) ! establish origin for second plot
!!       >!  To stay in the border no numbers greater than XBORDER-1 in X
!!       >!  or YBORDER-2 can be used
!!       >!  Calls to generate second plot go here
!!       >!    .
!!       >!    .
!!       >   CALL NFRAME()          ! terminate second plot
!!       >   CALL PLOT(0.0,0.0,999)! close the plot file
!!       >   END PROGRAM CALTEST
!!       >   SUBROUTINE BORDER(XBORDER,YBORDER)
!!       >!  Must be called with same values throughout entire program
!!       >!  or not all frames will plot to same scale.
!!       >!  Draw a box inside of which all frames can appear
!!       >   CALL PLOT(XBORDER,0.0,    2)
!!       >   CALL PLOT(XBORDER,YBORDER,2)
!!       >   CALL PLOT(0.0,    YBORDER,2)
!!       >   CALL PLOT(0.0,    0.0,    2)
!!       >   END SUBROUTINE BORDER
!!
!!   4. All coordinate values (XPAGE, YPAGE for example) should be greater
!!      than or equal to zero relative to the original frame origin.
!!      Negative values will be clipped or might cause post-processor errors.
!!      (Although this was not a requirement on the 7600s, it is necessary
!!      because metafiles must contain only positive values and it would
!!      be very inefficient to store each frame's data and then translate
!!      all the values to positive numbers once the frame was finished and
!!      the largest negative numbers in the frame could be identified.
!!
!!   6. Subroutine PLOTS must still be the first CALCOMP subroutine called.
!!      It performs various initialization functions and should be called
!!      only one time per program execution. Although some of the values
!!      are not used, they are maintained for compatibility purposes.
!!
!!   7. Subroutine CNTOUR (Which was developed at the Westinghouse Research
!!      Laboratories) is available in the CALCOMP library. The plot
!!      produced by CNTOUR will look different from that produced on the
!!      7600s since the legend is placed at the top of the plot. If more
!!      that 20 contours are used, the legend could overwrite the plot.
!!      A limit of 6.5 inches must be observed for the height parameter
!!      (HGT).
!!
!!##RECORD OF REVISIONS
!!
!!    06/24/85    Preliminary release was made for COS.
!!
!!    07/11/89    The routine NEWPEN may be used to select color.
!!
!!     07/01/91   The first release of the documentation on UNICOS.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_M_calcomp
!!    use M_calcomp
!!    ! 07/30/69
!!    real              :: x(104), y(104)
!!    character(len=40) :: msg
!!    integer,parameter :: kin = 50
!!    equivalence(x(1),xl),(y(1),yl)
!!    9007 format(7(1X,F9.3),F7.1)
!!       call make_c_qa4()   ! create datafile
!!       f = 1.0
!!       ipn = 2
!!       call plots(0.0,10.0,0.0,10.0)
!!    !-----------------------------------------------------------------------
!!       open(unit=kin,file='qa4.dat',action="read")
!!       NEXTREAD: do
!!          read(kin,9001) nrec, msg
!!          9001 format(1X,I2,7X,A40)
!!          write(*,*)'NREC=',nrec,'MSG=',trim(msg)
!!          select case(adjustl(msg))
!!    !-----------------------------------------------------------------------
!!           case('DATA')
!!             do i = 1,nrec
!!                read(kin,9007) x(1),y(1),x(2),y(2),x(3),y(3),x(4),y(4)
!!                do j = 1,4
!!                   if(x(j).eq.0)then
!!                      if(y(j).eq.0)then
!!                         ipn = 3
!!                         cycle
!!                      endif
!!                   endif
!!                   call plot(x(j),y(j),ipn)
!!                   ipn = 2
!!                enddo
!!             enddo
!!    !-----------------------------------------------------------------------
!!           case('CIRCL')
!!             do i = 1,nrec
!!                read(kin,9007) xl,yl,tho,thf,ro,rf,di
!!                call circl(xl,yl,tho,thf,ro,rf,di)
!!             enddo
!!    !-----------------------------------------------------------------------
!!           case('DASHL')
!!             do i = 1,nrec
!!                read(kin,9009) x(1),y(1),npts,inc
!!                9009 format(2(1X,F9.3),1X,I3,7X,I1)
!!                j1 = inc+1
!!                j2 = inc*npts+1-inc
!!                do j = j1,j2,inc
!!                   read(kin,9007) x(j),y(j)
!!                enddo
!!                j = j2+inc
!!                x(j) = 0.
!!                y(j) = 0.
!!                j = j+inc
!!                x(j) = 1.
!!                y(j) = 1.
!!                call dashl(x,y,npts,inc)
!!             enddo
!!    !-----------------------------------------------------------------------
!!           case('DASHP')
!!             do i = 1,nrec
!!                read(kin,9007) xl,yl,d
!!                call dashp(xl,yl,d)
!!             enddo
!!    !-----------------------------------------------------------------------
!!           case('ELIPS')
!!             do i = 1,nrec
!!                read(kin,9012) xl,yl,rma,rmi,a,th0,thf,ipen
!!                9012 format(7(1X,F9.3),1X,I1)
!!                call elips(xl,yl,rma,rmi,a,tho,thf,ipen)
!!             enddo
!!    !-----------------------------------------------------------------------
!!           case('FIT')
!!             do i = 1,nrec
!!                read(kin,9007) x(1),y(1),x(2),y(2),x(3),y(3)
!!                call fit(x(1),y(1),x(2),y(2),x(3),y(3))
!!             enddo
!!    !-----------------------------------------------------------------------
!!           case('GRID')
!!             do i = 1,nrec
!!                read(kin,9014) xl,yl,dx,dy,nx,ny
!!                9014 format(4(1X,F9.3),2(1X,I2,7X))
!!                call grid(xl,yl,dx,dy,nx,ny)
!!             enddo
!!    !-----------------------------------------------------------------------
!!           case('POLY')
!!             do i = 1,nrec
!!                read(kin,9007) xl,yl,sl,sn,a
!!                call poly(xl,yl,sl,sn,a)
!!             enddo
!!    !-----------------------------------------------------------------------
!!           case('RECT')
!!             do i = 1,nrec
!!                read(kin,9021) xl,yl,h,w,a,ipen
!!                9021 format(5(1X,F9.3),1X,I2)
!!                call rect(xl,yl,h,w,a,ipen)
!!             enddo
!!    !-----------------------------------------------------------------------
!!           case('SYMBOL')
!!             do i = 1,nrec
!!                read(kin,9016) xl,yl,h,msg,inc
!!                9016 format(3(1X,F9.3), A40,1X,I3)
!!                read(kin,9017) a,nc
!!                9017 format(1X,F9.3,1X,I2)
!!                if(inc.lt.0)cycle NEXTREAD
!!                call symbol(xl,yl,h,msg,inc,a,nc)
!!             enddo
!!    !-----------------------------------------------------------------------
!!           case('1100')
!!             do i = 1,nrec
!!                read(kin,9007) xl,yl
!!                call plot(xl,yl,-3)
!!             enddo
!!    !-----------------------------------------------------------------------
!!           case('FACTOR')
!!             do i = 1,nrec
!!                read(kin,9007) f
!!                call factor(f)
!!             enddo
!!    !-----------------------------------------------------------------------
!!           case('END')
!!             call factor(1.)
!!             call plot(20.,0.,999)
!!             exit NEXTREAD
!!    !-----------------------------------------------------------------------
!!           case default
!!             write(*,*)'unknown keyword ',trim(msg)
!!    !-----------------------------------------------------------------------
!!          end select
!!    !-----------------------------------------------------------------------
!!       enddo NEXTREAD
!!       close(unit=kin,status='delete')
!!    !-----------------------------------------------------------------------
!!    contains
!!
!!    subroutine make_c_qa4()
!!    integer,parameter :: io=40
!!    open(unit=io,file='qa4.dat',action="write")
!!    write(io,'(a)')'  1                RECT'
!!    write(io,'(a)')' 1.        1.        9.         7.       0.         3'
!!    write(io,'(a)')'  7                SYMBOL'
!!    write(io,'(a)')' 1.5       9.5       .14      SAMPLE OF GENERAL SUBROUTINES PACKAGE    999'
!!    write(io,'(a)')' 0.        37'
!!    write(io,'(a)')' 2.25      9.        .105     CIRCL                                    999'
!!    write(io,'(a)')' 0.         6'
!!    write(io,'(a)')' 5.75      9.        .105     ELIPS                                    999'
!!    write(io,'(a)')' 0.         5'
!!    write(io,'(a)')' 2.25      6.5       .105     FIT, DASHP                               999'
!!    write(io,'(a)')' 0.        11'
!!    write(io,'(a)')' 5.75      6.5       .105     POLY                                     999'
!!    write(io,'(a)')' 0.         4'
!!    write(io,'(a)')' 3.75      4.25      .105     GRID, DASHL                              999'
!!    write(io,'(a)')' 0.        12'
!!    write(io,'(a)')' 2.        1.1       .07      THE BORDER IS DRAWN WITH RECT            999'
!!    write(io,'(a)')' 0.        29'
!!    write(io,'(a)')'  3                CIRCL'
!!    write(io,'(a)')' 3.25      8.        0.        720.      .75       .25       0.'
!!    write(io,'(a)')' 3.25      8.        0.        360.      .75       .25       1.'
!!    write(io,'(a)')' 3.35      8.        0.        360.      .85       .85       0.'
!!    write(io,'(a)')'  6                ELIPS'
!!    write(io,'(a)')' 6.5       8.        .5        .7        0.        0.        360.      3'
!!    write(io,'(a)')' 6.6       8.        .6        .6        0.        0.        360.      3'
!!    write(io,'(a)')' 6.7       8.        .7        .5        0.        0.        360.      3'
!!    write(io,'(a)')' 6.8       8.        .8        .4        0.        0.        360.      3'
!!    write(io,'(a)')' 6.9       8.        .9        .3        0.        0.        360.      3'
!!    write(io,'(a)')' 7.        8.        1.        .2        0.        0.        360.      3'
!!    write(io,'(a)')'  3                DATA'
!!    write(io,'(a)')' 0.        0.        1.5       5.        1.5       5.5       2.375     6.'
!!    write(io,'(a)')' 3.5       6.125     2.625     5.5       1.5       5.5       0.        0.'
!!    write(io,'(a)')' 1.5       5.        2.625     5.        3.5       5.625     0.        0.'
!!    write(io,'(a)')'  1                  DASHP'
!!    write(io,'(a)')' 2.375     5.625     .1'
!!    write(io,'(a)')'  1                  DATA'
!!    write(io,'(a)')' 1.5       5.        1.5       5.        1.5       5.        0.       0'
!!    write(io,'(a)')'  2                  DASHP'
!!    write(io,'(a)')' 2.375     5.625     .1'
!!    write(io,'(a)')' 2.375     6.125     .1'
!!    write(io,'(a)')'  2                FIT'
!!    write(io,'(a)')' 2.625     5.        2.5       5.25      2.625     5.5'
!!    write(io,'(a)')' 3.5       5.625     3.375     5.875     3.5       6.125'
!!    write(io,'(a)')' 10                  POLY'
!!    write(io,'(a)')' 5.75      5.        .35       3.        0.'
!!    write(io,'(a)')' 5.75      5.        .35       4.        0.'
!!    write(io,'(a)')' 5.75      5.        .35       5.        0.'
!!    write(io,'(a)')' 5.75      5.        .35       6.        0.'
!!    write(io,'(a)')' 5.75      5.        .35       7.        0.'
!!    write(io,'(a)')' 5.75      5.        .35       8.        0.'
!!    write(io,'(a)')' 5.75      5.        .35       9.        0.'
!!    write(io,'(a)')' 5.75      5.        .35       10.       0.'
!!    write(io,'(a)')' 5.75      5.        .35       11.       0.'
!!    write(io,'(a)')' 5.75      5.        .35       12.       0.'
!!    write(io,'(a)')'  2                GRID'
!!    write(io,'(a)')' 1.5       2.        .25       .25       24         8'
!!    write(io,'(a)')' 1.51      1.99      1.5       1.         4         2'
!!    write(io,'(a)')'  1                DASHL'
!!    write(io,'(a)')' 1.75      2.25       11       1'
!!    write(io,'(a)')' 2.5       3.75'
!!    write(io,'(a)')' 2.75      3.25'
!!    write(io,'(a)')' 3.        3.5'
!!    write(io,'(a)')' 3.5       2.75'
!!    write(io,'(a)')' 4.        2.5'
!!    write(io,'(a)')' 4.25      3.25'
!!    write(io,'(a)')' 5.25      2.75'
!!    write(io,'(a)')' 5.5       3.75'
!!    write(io,'(a)')' 6.5       2.5'
!!    write(io,'(a)')' 7.25      3.5'
!!    write(io,'(a)')'                    END'
!!    close(unit=io)
!!    end subroutine make_c_qa4
!!
!!    end program demo_M_calcomp
!!##LICENSE
!!   Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_calcomp
implicit none
public axis
public circl
public cntour
public curvx
public curvy
public dashl
public dashp
public elips
public factor
public fit
public fit4
public fline
public grid
public lgaxs
public lglin
public line
public newpen
public width
public nframe
public number
public plot
public plots
public polar
public poly
public rect
public reflx
public scale
public scalg
public smoot
public solut
public symbol
public where

public mpset
public mset
public setpar
public move, draw, end
integer,parameter,public :: black=0
integer,parameter,public :: red=1
integer,parameter,public :: green=2
integer,parameter,public :: yellow=3
integer,parameter,public :: purple=4
integer,parameter,public :: magenta=5
integer,parameter,public :: cyan=6
integer,parameter,public :: white=7

private primitive__addrec
private primitive__merger
private primitive__newone
private primitive__pass
private primitive__recorx
private primitive__scan
private primitive__tracer
private primitive__fgetvar
private primitive__frend
private primitive__clear
private primitive__draw_line
private primitive__draw_text
private primitive__end_plotting
private primitive__start_plotting
private primitive__wpen
private primitive__width
!     FOR CONTOUR PLOTS
integer,save :: nx_q,ny_q,ii_q,ix_q,iy_q,nxm1_q,nym1_q
integer,save,target :: ntrk_q, nn_q
integer,save,target :: rec_q(2048)
integer,save :: direc_q,trk_q(255),trka_q(255),c_q
real,save    :: ratio_q,cvv_q,hx_q(255),hy_q(255)
logical,save :: edge_q,alt_q,oddpas_q
integer,save :: hxsz_q = 255
integer,save :: hysz_q = 255
integer,save :: limit_q = 2048
integer,save :: ixd_q(4)=[0,-1,0,1]
integer,save :: iyd_q(4)=[1,0,-1,0]
!
!  INITIALIZE THE SYMBOL TABLE AND OTHER CONSTANTS TO BE USED BY
!  THE SYMBOL ROUTINE
!
!  TABLE CHANGED FROM 596 TO 700 PER REQUEST OF R. LINCOLN, ORLANDO
integer table_q(700)
character(len=1) :: alpha_q(63)
real,save :: xspc_q=7.0,yspc_q=7.0
integer   :: idata

   data alpha_q/'A','B','C','D','E','F','G','H','I','J','K','L',           &
   &           'M','N','O','P','Q','R','S','T','U','V','W','X',            &
   &           'Y','Z','0','1','2','3','4','5','6','7','8','9','+',        &
   &           '-','*','/','(',')','$','=',' ',',','.',' ','''','>',       &
   &           ':',' ','?','[','<',' ','!',']',';',' ',' ',' ','\'   /
!
! ADDED > AND < IN LINES 3 AND 4 ABOVE FOR R. LINCOLN
!
   data (table_q(idata),idata=1,229)/                                    &
   & 0,5,0040,4044,4404,0400,2224,1,9,0110,1030,3041,4143,4334,          &
   & 3414,1403,0301,2224,2,4,0141,4124,2401,2224,3,2,2024,0242,          &
   & 4,2,0044,0440,5,5,0220,2042,4224,2402,2224,6,4,2024,0242,           &
   & 4224,2402,7,3,0044,4404,0440,8,4,0444,4400,0040,1232,9,3,           &
   & 2022,2204,4422,10,8,0011,1131,3140,3133,3313,1304,1311,2244,        &
   & 11,4,0044,2420,4004,0242,12,4,0040,4004,0444,4400,13,1,             &
   & 2024,14,7,0141,4124,2401,0343,4320,2003,2224,15,1,2040,16,          &
   & 3,2040,2242,2444,17,1,0007,18,4,2720,2032,3212,1220,19,4,           &
   & 2715,1535,3527,2720,20,3,2747,2444,2141,21,4,0343,4334,3432,        &
   & 3243,22,9,0605,0515,1516,1606,0146,3141,4142,4232,3231,23,3,        &
   & 0644,4402,0040,24,2,0224,2442,25,3,0242,0444,3511,26,3,             &
   & 2325,1434,1030,27,4,0343,1412,2422,3432,28,6,0006,0617,1737,        &
   & 3746,4640,0444,29,10,0007,0737,3746,4645,4534,3404,3443,4341,       &
   & 4130,3000,30,7,4637,3717,1706,0601,0110,1030,3041,31,6,0007,        &
   & 0727,2745,4543,4320,2000,32,4,4707,0700,0040,0434,33,3,0007,        &
   & 0747,0434,34,9,4637,3717,1706,0601,0110,1030,3041,4143,4313,        &
   & 35,3,0007,4047,0444/

   data (table_q(idata),idata=230,448)/                                  &
   & 36,3,0040,2027,0747,37,4,0110,1030,3041,4147,38,3,0007,             &
   & 0347,0340,39,2,0700,0040,40,4,0007,0724,2447,4740,41,3,             &
   & 0007,0740,4047,42,8,1030,3041,4146,4637,3717,1706,0601,0110,        &
   & 43,6,0007,0737,3746,4645,4534,3404,44,9,3010,1001,0106,0617,        &
   & 1737,3746,4641,4130,3140,45,7,0007,0737,3746,4645,4534,3404,        &
   & 2440,46,11,4637,3717,1706,0605,0514,1434,3443,4341,4130,3010,       &
   & 1001,47,2,0747,2720,48,5,0701,0110,1030,3041,4147,49,2,             &
   & 0720,2047,50,4,0710,1023,2330,3047,51,2,0047,0740,52,3,             &
   & 0724,2447,2420,53,4,0747,4700,0040,1434,54,8,1030,3042,4245,        &
   & 4537,3717,1705,0503,0310,55,3,1627,2720,1030,56,6,0627,2746,        &
   & 4644,4402,0200,0040,57,6,0747,4724,2443,4341,4120,2001,58,3,        &
   & 3037,3703,0343,59,8,4707,0704,0434,3443,4341,4130,3010,1001,        &
   & 60,11,4637,3717,1706,0601,0110,1030,3041,4143,4334,3414,1403,       &
   & 61,3,0607,0747,4700,62,15,3445,4546,4637,3717,1706,0605,            &
   & 0514,1434,3443,4341,4130,3010,1001,0103,0314,63,11,4433,3313,       &
   & 1304,0406,0617,1737,3746,4641,4130,3010,1001,64,2,2325,1434,        &
   & 65,1,1434/

   data (table_q(idata),idata=449,700)/                                  &
   & 66,4,2325,3513,1434,3315,67,1,0146,68,3,4725,2522,2240,69,          &
   & 3,0725,2522,2200,70,11,2027,0211,1131,3142,4243,4334,3414,          &
   & 1405,0516,1636,3645,71,2,0242,0444,72,0,73,5,1112,1222,             &
   & 2221,2111,2110,74,4,2030,3031,3121,2120,75,3,4604,0442,0040,        &
   & 76,1,2725,77,2,0644,4402,78,8,2223,2333,3332,3222,2425,             &
   & 2535,3534,3424,79,4,0112,1230,3037,3747,80,10,0617,1737,            &
   & 3746,4644,4433,3323,2322,2130,3010,1021,81,3,3707,0700,0030,        &
   & 82,2,4604,0442,83,4,1131,1333,1535,2026,84,4,2722,2130,             &
   & 3010,1021,85,3,1747,4740,4010,86,9,2223,2333,3332,3222,3221,        &
   & 2425,2535,3534,3424,87,3,0343,4326,2603,88,3,2027,0646,0141,        &
   & 89,5,0617,1726,2637,3746,2620,90,1,0740,104*0/
!
! ADDED 104 ZERO ENTRIES FOR R. LINCOLN
!
!===================================================================================================================================
real :: maxq ! zmax
!===================================================================================================================================
character(len=4),save :: cttyp_q
integer,save          :: ktsize_q

integer,parameter     :: end=999
integer,parameter     :: move=3
integer,parameter     :: draw=2
!===================================================================================================================================
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    circl(3f) - [M_calcomp:general] draws an arc or spiral
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine circl(xpage,ypage,tho,thf,ro,rf,di)
!!
!!##DESCRIPTION
!!
!!   CIRCL(3f) is a FORTRAN subroutine that draws, starting at a given point,
!!   an arc which may be extended to form a circle or spiral.
!!
!!##OPTIONS
!!
!!     XPAGE,YPAGE  are the coordinates of the starting point of the arc
!!                  in inches.
!!     THO          is the radius angle, in degrees counterclockwise from
!!                  the X-axis, for the start of the arc.
!!
!!     THF          is the radius angle, in degrees counterclockwise from
!!                  the X-axis, for the end of the arc.
!!
!!     RO           is the arc's starting radius, in inches.
!!
!!     RF           is the arc's ending radius, in inches.
!!     DI           is a code used to specify the type of line desired.
!!
!!                   If DI = 0.0, a solid arc is drawn;
!!                           0.5, a dashed arc is drawn.
!!
!!   COMMENTS
!!
!!  THO and THF may be positive or negative. If THO is less than THF, the arc is
!!  drawn in a counterclockwise direction; and if THO is greater than THF, the
!!  arc is drawn in a clockwise direction.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_circl
!!    use M_calcomp, only : plots, plot, newpen, circl
!!    implicit none
!!    character(len=:),allocatable :: lines(:)
!!    integer                      :: i
!!    integer                      :: ipen
!!    real                         :: xstart, ystart
!!    real                         :: start_angle, finish_angle
!!    real                         :: start_radius, finish_radius
!!    real                         :: dash_code
!!    integer,parameter            :: MOVE=3, DRAW=2
!!    lines=[character(len=80) :: &
!!    '#--------#--------#--------#--------#--------#--------#--------#', &
!!    '# xstart ! ystart !strt_ang! end_ang! start_r!  end_r !dashcode ', &
!!    '# BIG CIRCLES                                                   ', &
!!    '!4.50    ! 2.5    !0.0     !360.0   !2.00    !2.00    !0.0     !', &
!!    '!4.00    ! 2.5    !0.0     !360.0   !1.50    !1.50    !0.0     !', &
!!    '!3.50    ! 2.5    !0.0     !360.0   !1.00    !1.00    !0.0     !', &
!!    '# LONG SPIRAL                                                   ', &
!!    '!5.00    !-2.5    !0.0     !1440.0  !2.50    !0.25    !0.0     !', &
!!    '# SPIRAL WITH DASHED LINE                                       ', &
!!    '!-1.75   ! 2.5    !0.0     !360.0   !0.75    !0.25    !1.0     !', &
!!    '# CIRCULAR ARC                                                  ', &
!!    '!-2.50   !-2.5    !0.0     !180.0   !0.85    !0.85    !0.0     !', &
!!    '!-2.50   !-2.5    !-45.0   !-90.0   !0.85    !0.85    !0.0     !', &
!!    '#--------#--------#--------#--------#--------#--------#--------#']
!!    call plots(0.0,10.0,0.0,10.0)      ! initialize graphics
!!    call plot(5.0,5.0,-3) ! set origin
!!    ! draw a crosshair at origin point <0,0>
!!    call crosshair(0.0,0.0,2.0)
!!    ! draw some circles using center and radius
!!    call circle(-2.5,-2.5,2.5)
!!    call circle( 2.5, 2.5,2.5)
!!    call circle( 2.5,-2.5,2.5)
!!    call circle(-2.5, 2.5,2.5)
!!    ! box around 10x10 area
!!    call plot(-5.0,-5.0, MOVE)
!!    call plot( 5.0,-5.0, DRAW)
!!    call plot( 5.0, 5.0, DRAW)
!!    call plot(-5.0, 5.0, DRAW)
!!    call plot(-5.0,-5.0, DRAW)
!!    ! call the values from the table
!!    do i = 1,size(lines)
!!       write(*,'(a)')lines(i)
!!       ipen=mod(i,8)
!!       ipen=merge(5,ipen,ipen.eq.0)
!!       call newpen(ipen)
!!       if(index(lines(i),'#').ne.0)cycle
!!       read(lines(i),'(7(1x,f8.3))') xstart,ystart, &
!!          start_angle,finish_angle, &
!!          start_radius,finish_radius, &
!!          dash_code
!!       call circl(xstart,ystart, &
!!          start_angle,finish_angle, &
!!          start_radius,finish_radius, &
!!          dash_code)
!!    enddo
!!    call plot(0.0,0.0,999) ! end graphics
!!    contains
!!    subroutine crosshair(x,y,s)
!!    real,intent(in) :: x,y
!!    real,intent(in) :: s
!!        call plot(x+s,y    ,MOVE)
!!        call plot(x-s,y    ,DRAW)
!!        call plot(x    ,y+s,MOVE)
!!        call plot(x    ,y-s,DRAW)
!!    end subroutine crosshair
!!    subroutine circle(x,y,r)
!!    real,intent(in) :: x,y  ! center
!!    real,intent(in) :: r    ! radius
!!       call crosshair(x,y,0.2)
!!       call circl(x+r,y,0.0,360.0,r,r,0.0)
!!    end subroutine circle
!!    end program demo_circl
!!##LICENSE
!!   Public Domain
subroutine circl(startx,starty,start_angle,end_angle,start_radius,end_radius,dash_code)

! ident_1="@(#) M_calcomp circl(3f) draws an arc or spiral"

real,intent(in) :: startx
real,intent(in) :: starty
real,intent(in) :: start_angle, end_angle
real,intent(in) :: start_radius, end_radius
real,intent(in) :: dash_code
integer         :: i
integer         :: i2, i5
integer         :: knt
integer         :: n
real            :: dth
real            :: x, y
real            :: xcenter, ycenter
real            :: dummyx, dummyy
real            :: start_radians, end_radians
real            :: fctr
real            :: r0rrf
real            :: rn
real            :: tn
   i5 = 4.51+dash_code
   i2 = 2
   x=startx ; y=starty                                ! working point
   call where(dummyx,dummyy,fctr)                     ! get scale factor and decide on a nice angle delta
   knt = 7.0*fctr
   r0rrf = abs(start_radius)+abs(end_radius)+0.00001
   dth = 0.03/(r0rrf) / fctr

   start_radians = start_angle / 57.2958              ! convert degrees to radians
   end_radians = end_angle / 57.2958

   xcenter = x - start_radius * cos(start_radians)
   tn = (end_radians - start_radians) / dth
   if((end_radians - start_radians).lt.0)then
      tn = abs(tn)
      dth = -dth
   endif

   ycenter = y - start_radius * sin(start_radians)

   !call plot(xcenter+.2,ycenter+.2,MOVE)
   !call plot(xcenter-.2,ycenter-.2,DRAW)

   !call plot(xcenter+.2,ycenter-.2,MOVE)
   !call plot(xcenter-.2,ycenter+.2,DRAW)

   n = tn
   call plot(x,y,MOVE)                                   ! move to starting point
   if(n.gt.0)then
      tn = (end_radius-start_radius)/tn
      rn = start_radius-tn
      do i = 1,n
         start_radians = start_radians + dth
         rn = rn + tn
         x = rn * cos(start_radians) + xcenter
         y = rn * sin(start_radians) + ycenter
         if( knt .le.0)then
            i2 = i5-i2
            knt = 7.0*fctr
         endif
         knt = knt - 1
         call plot(x,y,i2)
      enddo
   endif

   x = end_radius*cos(end_radians)+xcenter
   y = end_radius*sin(end_radians)+ycenter
   call plot(x,y,i2)

end subroutine circl
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    dashl(3f) - [M_calcomp:general] draws a polyline with dashed lines
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine dashl(xarray,yarray,npts,inc)
!!
!!##DESCRIPTION
!!
!!   DASHL is a FORTRAN subroutine which draws dashed lines connecting a series of
!!   data points. Its operation is similar to that of the LINE subroutine.
!!
!!##OPTIONS
!!
!!     XARRAY   is the name of the array containing abscissas (X values) of the
!!              data points to be plotted and the scaling parameters for
!!              the X array.
!!
!!     YARRAY   is the name of the array containing ordinates (Y values) of the
!!              data points to be plotted and the scaling parameters for
!!              the Y array.
!!
!!     NPTS     is the quantity of data points to be plotted.
!!
!!     INC      is the increment between array elements to be plotted.
!!              INC is greater than 1 if the values to be plotted are in
!!              a mixed or multi-dimensioned array. (Normally INC = 1).
!!
!!
!!  COMMENTS
!!
!!  The arrays must be dimensioned with at least NPTS + 2 elements.
!!  The adjusted minimum value (FIRSTV) and the adjusted delta value
!!  (DELTAV), normally provided by the SCALE subroutine, must be stored
!!  following the data array.
!!
!!  For the X array, the adjusted minimum is stored in XARRAY(NPTS*INC+1),
!!  and the adjusted delta is in XARRAY(NPTS*INC+INC+1).
!!  Similarly, for the Y array, the minimum is in YARRAY(NPTS*INC+1),
!!  and the delta is in YARRAY(NPTS*INC+INC+1). Therefore, XARRAY
!!  and YARRAY must be dimensioned to be at least NPTS*INC+INC+1 .
!!
!!  If scaling is not required, the user must place the appropriate
!!  minimum and delta values in the specified elements of the arrays.
!!  For a one-to-one correspondence between array data and plotted data,
!!  these values should be 0.0 (minimum) and 1.0 (delta).
!!
!!  A dashed line, with dashes approximately 0.1 inch long, is drawn
!!  connecting sequential points. Coding is optimized so that plotting may
!!  either begin at the first point and progress forward or begin at the
!!  last point and progress backward.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_dashl
!!    use m_calcomp
!!    character(len=28) :: ichr1
!!    character(len=26) :: ichr2
!!    character(len=10) :: lbcd1,lbcd2
!!    dimension xarray(62),yarray(62)
!!    ichr1='PLOTTED ON A CALCOMP PLOTTER'
!!    ichr2='USING  Y = X -0.7*X +0.1*X'
!!    lbcd1='X-ABSCISSA'
!!    lbcd2='Y-ORDINATE'
!!    ! PLOT GRAPH ILLUSTRATING SCALE, AXIS, AND LINE
!!    deltax=0.04
!!    ! AXIS DRAWS LABELS AS MUCH AS 0.4 INCHES TO THE NEGATIVE OF AXIS CENTER;
!!    ! EITHER USE AN ORIGIN OFFSET OF AT LEAST THIS VALUE OR DO NOT USE AN
!!    ! ORIGIN VALUE OF LESS THAN 0.4 OR CLIPPING WILL OCCUR
!!    call plots(0.0,12.0,0.0,12.0)
!!    call width(0)
!!    call newpen(white)
!!    call rect(0.0,0.0,11.0,10.0,0.0,7)
!!    call plot(0.4,0.4,-move)
!!    deltax=2.0*deltax
!!    xarray(1)=deltax
!!    do j=1,60
!!       yarray(j)=xarray(j)**2-0.7*xarray(j)**3+0.1*xarray(j)**4
!!       xarray(j+1)=xarray(j)+deltax
!!    enddo
!!    call scale(xarray(1), 6.5,60,1)
!!    call scale(yarray(1),10.0,60,1)
!!    call axis(0.0,0.0,lbcd1,-10, 6.5, 0.0,xarray(61),xarray(62))
!!    call axis(0.0,0.0,lbcd2, 10,10.0,90.0,yarray(61),yarray(62))
!!    call width(20)
!!    !!call newpen(RED)
!!    !!linetype=-1
!!    !!inteq=4
!!    !!call line(xarray(1),yarray(1),60,1,linetype,inteq)
!!    call newpen(green)
!!    call dashl(xarray(1),yarray(1),60,1)
!!    call newpen(1)
!!    call symbol(1.3,10.,.14,ichr1,inteq,0.0,28)
!!    call symbol(1.3,9.7,.14,ichr2,inteq,0.0,26)
!!    call width(0)
!!    call number(2.98,9.8,.1,2.0,0.,-1)
!!    call number(3.96,9.8,.1,3.0,0.,-1)
!!    call number(4.94,9.8,.1,4.0,0.,-1)
!!    call plot(0.0,0.0,end)
!!    end program demo_dashl
!!##LICENSE
!!   Public Domain
SUBROUTINE DASHL  (X,Y,N,K)

! ident_2="@(#) M_calcomp dashl(3f) draws a polyline with dashed lines"

real    :: x(*)
real    :: y(*)
real    :: dds
real    :: ds
real    :: dx
real    :: dy
integer :: i
integer :: id
integer :: j
integer :: k
integer :: kk
integer :: n
integer :: nd
integer :: nm
integer :: no
integer :: np
real    :: xn
real    :: xt
real    :: xx
real    :: yn
real    :: yt
!     TEST LESS THAN TWO POINTS
      if(n-1)  90,90,80
80    continue
!     INITIALIZE POINT, MINIMUM AND DELTA INDEXES
      np=(n-1)*k+1
      nm=np+k
      nd = nm + k
      no=1
      kk=k
!     DETERMINE CURRENT PEN POSITION
      call where(xn,yn,xx)
!     FIND NEAREST END OF LINE
      dx = max(abs((x( 1)-x(nm))/x(nd)-xn), abs((y( 1)-y(nm))/y(nd)-yn))
      dy = max(abs((x(np)-x(nm))/x(nd)-xn), abs((y(np)-y(nm))/y(nd)-yn))
      if(dx-dy) 20,20,40
!     REVERSE INCREMENT AND END TEST VALUE
40    no=np
      np=1
      kk=-kk
20    i=no
!     COMPUTE DELTAS OF INDEXED LINE SEGMENT
30    j=i+kk
      dy =(y(j) - y(i) )/y(nd)
      dx =(x(j) - x(i))/x(nd)
      ds = sqrt(dx*dx+dy*dy+0.000001)
      id = 5.0 *ds
      if(id)10,10,11
!     ASSURE DIVISOR NON ZERO
10    id = 1
!     DERIVE DASH LENGTH.
11    dds = ds / float(2*id+1)
      dy = dds * dy / ds * y(nd)
      dx = dds * dx / ds * x(nd)
!     SET XT/YT TO SEGMENT START POINT
      xt = x(i)
      yt = y(i)
!     PLOT WITH PEN UP TO XT/YT
1     call plot((xt-x(nm))/x(nd),(yt-y(nm))/y(nd),3)
!     ADJUST XT/YT AND END TEST BY DASH LENGTH
      xt = xt + dx
      yt = yt + dy
      ds = ds - dds
!     TEST LINE SEGMENT END
      if(ds) 3,3,2
!     PLOT TO XT/YT WITH PEN DOWN
2     call plot((xt-x(nm))/x(nd),(yt-y(nm))/y(nd),2)
!     ADJUST XT/YT AND END TEST BY DASH LENGTH
      xt = xt + dx
      yt = yt + dy
      ds = ds - dds
!     TEST LINE SEGMENT END
      if(ds) 3,4,1
!     PLOT SEGMENT FINISH POINT \PEN DOWN'
3     call plot((x(j)-x(nm))/x(nd),(y(j)-y(nm))/y(nd),2)
!     TEST LAST LINE SEGMENT
4     if(j-np) 5,90,5
5     i=j
      goto 30
90    return
end subroutine dashl
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    dashp(3f) - [M_calcomp:general] draw from current position to new point with dashed line
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine dashp(xpage,ypage,dash)
!!
!!##DESCRIPTION
!!
!!  DASHP is a FORTRAN subroutine which draws a dashed line from the current pen
!!  position to a specified point.
!!
!!##OPTIONS
!!
!!    XPAGE,YPAGE  are the coordinates, in inches, of the point to which the
!!                 dashed line is to be drawn.
!!
!!    DASH         is the length, in inches, of each dash and of the space
!!                 between dashes.
!!
!!  COMMENTS
!!
!!  If the line length is less than double the dash length, the dash length is
!!  adjusted to half the line length.
!!##LICENSE
!!   Public Domain
SUBROUTINE DASHP  (X,Y,DL)

! ident_3="@(#) M_calcomp dashp(3f) draw from current position to new point with dashed line"

!     A DASHED LINE IS DRAWN IN INCHES FROM THE CURRENT PEN POSITION TO
!     THE SPECIFIED XPAGE, YPAGE. THE SIZE OF THE DASH WILL BE AS CALLED
!     FOR EXCEPT IF THE LINE LENGTH IS LESS THAN DOUBLE THE DASH SIZE
!     THE DASH IS ADJUSTED TO HALF THE LINE LENGTH.

! EARLIER VERSION OF THIS SUBROUTINE WAS
!     SUBROUTINE  DASHPT(X,Y,DL)
real    :: dl
real    :: ds
real    :: dx
real    :: dy
integer :: ic
real    :: s
real    :: st
real    :: x
real    :: xt
real    :: y
real    :: yt

!     DETERMINE CURRENT PEN POSITION
   call where(xt,yt,st)
!     COMPUTE DELTAX AND DELTAY
   dx = x-xt
   dy = y-yt
   ds = dl
   ic = 2
!     DERIVE LINE LENGTH
   s =   sqrt(dx*dx+dy*dy)
   if(s-0.02*st) 6,10,10
10 continue
   ds = ds/s
!     TEST IF LINE LESS THAN DOUBLE DASH LENGTH
   if(ds-0.5) 2,2,7
!     HALVE DASH LENGTH
7  continue
   ds = 0.5
!     PROPORTION THE DELTAS BY THE LENGTH/DASH RATIO
2  continue
   dx = dx*ds
   dy = dy*ds
!     SET UP ADJUSTMENT AND END OF LINE TEST FROM ABS GREATEST DELTA
   s = dx
   st = abs(dx)-abs(dy)
   if(st) 3,4,4
3  continue
   s = dy
4  continue
   st = abs( s/ds)-abs( s)
   ds = abs( s)
!     DASHED LINE LOOP
5  continue
   xt = xt+dx
   yt = yt+dy
   st = st-ds
   call plot(xt,yt,ic)
   ic = 5-ic
   if(st) 6,6,5
!     LAST SPECIFIC LINE SEGMENT CALL
6  continue
   call plot(x, y, ic)
end subroutine dashp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    elips(3f) - [M_calcomp:general] draw an elliptical arc
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine elips(xpage,ypage,rmaj,rmin,angle,tho,thf,ipen)
!!
!!##DESCRIPTION
!!
!!   ELIPS is a FORTRAN subroutine which draws an ellipse or elliptical arc.
!!
!!##OPTIONS
!!
!!     XPAGE,YPAGE     are the coordinates, in inches, of the starting point of
!!                     the ellipse or arc.
!!
!!     RMAJ,RMIN       are the lengths, in inches, of the semi-major and
!!                     semi-minor axes, respectively.
!!
!!     ANGLE           is the angle of the major axis, in degrees.
!!
!!     THO,THF         are the angles, in degrees with respect to ANGLE, of the
!!                     arc's starting and ending points.
!!     IPEN            is the code that moves the pen to the arc's starting
!!                     point. If the value of IPEN is:
!!
!!                        3, the pen is up for the move;
!!                        2, the pen is down for the move.
!!
!!   COMMENTS
!!
!!  THO and THF may be positive or negative. If THO is less than THF, the arc is
!!  drawn in a counterclockwise direction; if THO is greater than THF, the arc is
!!  drawn in a clockwise direction.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_elips
!!    use M_calcomp, only : plots, plot, newpen, factor, nframe, elips
!!    implicit none
!!    character(len=:),allocatable :: lines(:)
!!    integer                  :: i
!!    real                     :: x, y
!!    real                     :: a
!!    real                     :: major_axis_length, minor_axis_length
!!    real                     :: start_angle, finish_angle
!!    integer                  :: ipen
!!    integer,parameter        :: END=999, MOVE=3, DRAW=2
!!    lines=[character(len=80) :: &
!!    '#---------------------------------------------------------------------#-',&
!!    '#  move over in the x direction the same amount you change axis length  ',&
!!    '#---------#---------#---------#---------#---------#---------#---------#-',&
!!    '|6.5      !8.0      !0.5      !0.7      ! 0.0     ! 0.0     ! 360.0   !3',&
!!    '|6.6      !8.0      !0.6      !0.6      ! 0.0     ! 0.0     ! 360.0   !3',&
!!    '|6.7      !8.0      !0.7      !0.5      ! 0.0     ! 0.0     ! 360.0   !3',&
!!    '|6.8      !8.0      !0.8      !0.4      ! 0.0     ! 0.0     ! 360.0   !3',&
!!    '|6.9      !8.0      !0.9      !0.3      ! 0.0     ! 0.0     ! 360.0   !3',&
!!    '|7.0      !8.0      !1.0      !0.2      ! 0.0     ! 0.0     ! 360.0   !3',&
!!    '#---------#---------#---------#---------#---------#---------#---------#-',&
!!    '#  different end angles of different signs                              ',&
!!    '#---------#---------#---------#---------#---------#---------#---------#-',&
!!    '|5.0      !8.0      !1.0      !0.2      ! 0.0     ! 0.0     !  45.0   !3',&
!!    '|3.0      !8.0      !1.0      !0.2      ! 0.0     ! 0.0     ! -60.0   !3',&
!!    '#---------#---------#---------#---------#---------#---------#---------#-',&
!!    '# circles                                                               ',&
!!    '#---------#---------#---------#---------#---------#---------#---------#-',&
!!    '#---------#---------#---------#---------#---------#---------#---------#-',&
!!    '#  end of values to call ELIPS(3f) with                                 ',&
!!    '#---------------------------------------------------------------------#-']
!!       call plots(0.0,10.0,0.0,10.0)
!!       do i=1,size(lines)
!!          write(*,'(a)')lines(i)
!!          if( lines(i).eq.' ' .or. index(lines(i),'#').ne.0 )cycle
!!          read(lines(i),9012) x,y, &
!!                          & major_axis_length,minor_axis_length, &
!!                          & a, &
!!                          & start_angle,finish_angle, &
!!                          & ipen
!!          9012 format(7(1X,F9.3),1X,I1)
!!          call elips(x,y, &
!!                          & major_axis_length,minor_axis_length, &
!!                          & a, &
!!                          & start_angle,finish_angle, &
!!                          & ipen)
!!       enddo
!!       call nframe()
!!       call plot(0.0,0.0,END)
!!    end program demo_elips
!!##LICENSE
!!   Public Domain
subroutine elips(x0,y0,a,b,alpha,thet0, thetf, iv)

! ident_4="@(#) M_calcomp elips(3f) draw an elliptical arc"

real,intent(in)     :: x0, y0
real,intent(in)     :: a
real,intent(in)     :: b
real,intent(in)     :: alpha
real,intent(in)     :: thet0
real,intent(in)     :: thetf
integer,intent(in)  :: iv
real                :: dummyx,dummyy
real                :: ab
real                :: absq
real                :: alp
real                :: bsq
real                :: d
real                :: dthe
real                :: fctr
integer             :: i
integer             :: n
real                :: st
real                :: the0
real                :: thef
real                :: then
real                :: xc
real                :: xf
real                :: yc
real                :: yf

   if(abs(a)+abs(b)) 4,20,4
4  continue
   alp = alpha/57.2958
   the0 = thet0  / 57.2958
   thef = thetf  / 57.2958
   d=a*b/sqrt((a*sin(the0))**2+(b*cos(the0))**2)
   xc = x0 - d * cos(the0 + alp)
   yc = y0 - d * sin(the0 + alp)
   bsq=b*b
   absq=a*a-bsq
   ab=a*b
   call plot(x0,y0,iv)
   call where(dummyx,dummyy,fctr)
   dthe = 0.03/(abs(a)+abs(b))/fctr
   n  =int((thef - the0)/dthe)
   if(n) 6,5,7
5  continue
   n = -1
6  continue
   n = -n
   dthe = -dthe
7  continue
   then = the0 + dthe
   do i=1,n
      st=sin(then)
      d=ab/sqrt(absq*st*st+bsq)
      xf=xc+d*cos(then+alp)
      yf=yc+d*sin(then+alp)
      call plot(xf,yf,2)
      then = then + dthe
   enddo
   st=sin(thef)
   d=ab/sqrt(absq*st*st+bsq)
   xf=xc+d*cos(thef+alp)
   yf=yc+d*sin(thef+alp)
   call plot(xf,yf,2)
   return
20 continue
   call plot(x0,y0,iv)
end subroutine elips
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    fit(3f) - [M_calcomp:general] draws a semi-hyperbolic curve through three points
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine fit(xpage1,ypage1,xpage2,ypage2,xpage3,ypage3)
!!
!!##DESCRIPTION
!!
!!  FIT is a FORTRAN subroutine which draws a semi-hyperbolic curve through three
!!  points.
!!
!!##OPTIONS
!!
!!    XPAGE1,YPAGE1   are the X and Y coordinates, in inches, of the three
!!    XPAGE2,YPAGE2   points through which the curve passes.
!!    XPAGE3,YPAGE3
!!
!!##COMMENTS
!!
!!  This subroutine generates a semi-hyperbolic fit using the three given points.
!!  A set of points for which a fit is not possible is drawn with straight-line
!!  segments.
!!
!!##RESTRICTIONS
!!
!!  The curve through the three points must be multi-valued in both X and Y.
!!  That is, the middle point (XPAGE2,YPAGE2) must be between the endpoints along
!!  the X-axis or the Y-axis.
!!
!!        XPAGE1<YPAGE2<XPAGE3 or XPAGE1>XPAGE2>XPAGE3 or
!!        YPAGE1<YPAGE2<YPAGE3 or YPAGE1>YPAGE2>YPAGE3
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_fit
!!    use M_calcomp, only : plots, plot, newpen, width, fit
!!    use M_calcomp, only : black ,red ,green ,yellow
!!    use M_calcomp, only : purple ,magenta ,cyan ,white
!!    implicit none
!!    integer,parameter  :: MOVE=3, DRAW=2
!!    integer            :: i
!!    real               :: x(3)=[-3.0,1.0,4.4],y(3)=[3.2,1.0,-4.0]
!!    call plots(0.0,10.0,0.0,10.0)      ! initialize graphics
!!    call plot(5.0,5.0,-3)              ! set origin
!!    call newpen(green)
!!    call crosshair(0.0,0.0,2.0)        ! draw a crosshair at origin point <0,0>
!!    call width(30); call newpen(red)
!!    do i=1,size(x)                     ! mark the points
!!       call crosshair(x(i),y(i),0.2)
!!    enddo
!!    x=[-3.0, 1.0, 4.4]
!!    y=[ 3.2, 1.0,-4.0]
!!    call width(80); call newpen(yellow)
!!    call fit(x(2),y(2),x(3),y(3),x(1),y(1))   ! draw in wrong order
!!    call width(40);call newpen(magenta)
!!    call fit(x(2),y(2),x(1),y(1),x(3),y(3))   ! draw in wrong order
!!    call width(140); call newpen(green)
!!    call fit(x(1),y(1),x(2),y(2),x(3),y(3))   ! draw in right order to get fit
!!    call plot(0.0,0.0,999)                    ! terminate graphics and pause
!!    contains
!!    subroutine crosshair(x,y,s)
!!    real,intent(in) :: x,y,s
!!        call plot(x+s,y    ,MOVE)
!!        call plot(x-s,y    ,DRAW)
!!        call plot(x    ,y+s,MOVE)
!!        call plot(x    ,y-s,DRAW)
!!    end subroutine crosshair
!!    end program demo_fit
!!
!!##LICENSE
!!   Public Domain
subroutine fit (xa,ya,xb,yb,xc,yc)

! ident_5="@(#) m_calcomp fit(3f) draws a semi-hyperbolic curve through three points"

real      :: xa
real      :: ya
real      :: xb
real      :: yb
real      :: xc
real      :: yc
real      :: ss(8,9),theta(2)
real,save :: a=0.0
real,save :: b=0.0
integer   :: m
real      :: dy
real      :: dx
real      :: z3
integer   :: i
real      :: c
real      :: d
real      :: dz
real      :: fctr
integer   :: ktra
real      :: x
real      :: y
real      :: z
real      :: z2
      m = 2
      dy = yc - ya
      dx = xc - xa
      z3 = sqrt( dy**2 + dx**2 )
      if( z3 ) 20,20,21
21    do i = 1,2
         if(abs(dx)-abs(dy)) 1,2,2
1        theta(i) = 1.5708  - atan(abs(dx/dy))
         goto 3
2        theta (i)= atan(abs(dy/dx))
3        if(dx) 25,26,26
25       if(dy) 5,4,4
26       if(dy) 4,5,5
4        theta(i) = -theta(i)
5        if(dx) 6,7,7
6        theta(i) =  theta(i) + 3.1416
7        dx = xb - xa
         dy = yb - ya
      enddo
      z2 = sqrt(dy**2 + dx**2)  * cos(theta(2)-theta(1))
      if( z2 ) 20,20,22
22    continue
      ss(1,3) = xa - xc
      ss(2,3) = xa - xb
      ktra = 1
      goto 13
16    continue
      a = ss(1,3)
      b = ss(2,3)
      ss(1,3) = ya - yc
      ss(2,3) = ya - yb
      ktra = 2
      goto 13
17    continue
      call where(x,y,fctr)
      dz =0.01 / fctr
      z = dz
      call plot(xa,ya,3)
      c = ss(1,3)
      d= ss(2,3)
18    continue
      x = (a*z+b)*z+xa
      y = (c*z+d)*z+ya
      call plot(x,y,2)
      z = z + dz
      if(z - z3)18,19,19
19    continue
      call plot (xc,yc,2)
      return
13    continue
      ss(1,1) = z3 * z3
      ss(1,2) = z3
      ss(2,1) = z2 * z2
      ss(2,2) = z2
      call solut(ss,m)
! EARLIER VERSION USED
!     CALL SOLUTN(SS,M)
      if(m)  20,20,14
14    continue
      goto (16,17),ktra
20    continue
      call plot(xa,ya,3)
      call plot(xb,yb,2)
      goto 19
end subroutine fit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    grid(3f) - [M_calcomp:general] draws a linear grid
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine grid(xpage,ypage,deltax,deltay,nxsp,nysp)
!!
!!    real,intent(in)    :: xpage, ypage
!!    real,intent(in)    :: deltax, deltay
!!    integer,intent(in) :: nxsp, nysp
!!
!!##DESCRIPTION
!!
!!  GRID(3f) draws a linear grid.
!!
!!##OPTIONS
!!
!!    XPAGE,YPAGE  are the coordinates, in inches, of the lower left
!!                 corner of the grid
!!
!!    DELTAX       is the number of inches between grid lines in the X
!!                 direction.
!!
!!    DELTAY       is the number of inches between grid lines in the Y
!!                 direction.
!!
!!    NXSP,NYSP    are the number of grid intervals in the X and Y
!!                 directions, respectively. The number of grid lines is
!!                 one more than the number of grid intervals.
!!
!!   COMMENTS
!!
!!  GRID generates a linear grid of any size. The number of lines drawn is
!!  NXSP+1 in the X direction and NYSP+1 in the Y direction.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_grid
!!    use M_calcomp, only : plots, plot, newpen, grid
!!    use M_calcomp, only : END
!!    implicit none
!!    real              :: xmax=8.5,ymax=11.0
!!    real              :: step
!!       call plots(0.0,xmax,0.0,ymax)  ! make a 8 1/2 x 11 inch page
!!       call newpen(1)                 ! red
!!       step=0.25                      ! make 1/4 inch grid
!!       call grid(0.0,0.0,step,step,int(xmax/step)+1,int(ymax/step)+1)
!!       call newpen(2)                 ! green
!!       step=0.50                      ! make 1/2 inch grid
!!       call grid(0.0,0.0,step,step,int(xmax/step)+1,int(ymax/step)+1)
!!       call plot(0.0,0.0,END)         ! end graphics
!!    end program demo_grid
!!##LICENSE
!!   Public Domain
subroutine grid (x,y,xs,ys,m,n)

! ident_6="@(#) M_calcomp grid(3f) draws a linear grid"

real,intent(in)    :: x,y ! (x,y) is the starting position of grid
real,intent(in)    :: xs  ! xs    is the space of grid in x direction.
real,intent(in)    :: ys  ! ys    is the space of grid in y direction
integer,intent(in) :: m   ! m     is the number of division in x direction.
integer,intent(in) :: n   ! n     is the number of divisions in y direction.

real               :: y0, x0
real               :: xf
integer            :: im
integer            :: i
real               :: xt

   y0 = y
   x0 = x
   im = n + 1
   xf = x0 + xs * float(m)
   call plot(x0,y0,3)
   do i = 1,im
      call plot(x0,y0,2)
      call plot(xf,y0,2)
      y0 = y0 + ys
      xt = xf
      xf = x0
      x0 = xt
   enddo

   x0 = x
   y0 = y
   xf = y + ys * float(n)
   im = m + 1
   do i = 1,im
      call plot(x0,xf,2)
      call plot(x0,y0,2)
      x0 = x0 +xs
      xt = xf
      xf = y0
      y0 = xt
   enddo

end subroutine grid
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    poly(3f) - [M_calcomp:general] draw an equilateral polygon
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine poly(xpage,ypage,slen,sn,angle)
!!
!!##DESCRIPTION
!!
!!  POLY(3f) draws equilateral polygons.
!!
!!##OPTIONS
!!
!!    XPAGE,YPAGE  are the coordinates, in inches, of the starting
!!                 point of the polygon.
!!
!!    SLEN         is the length, in inches, of a side of the polygon.
!!
!!    SN           is the number of sides of the polygon.
!!
!!    ANGLE        is the angle, in degrees, of the first side of the
!!                 polygon.
!!
!!   COMMENTS
!!
!!    If SN is negative, a star is drawn with SN points.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_poly
!!    use M_calcomp, only : plots, poly, plot
!!    implicit none
!!    real              :: xstart, ystart
!!    real              :: side_length
!!    real              :: number_of_sides
!!    real              :: angle
!!    integer           :: i
!!       call plots(0.0,10.0,0.0,10.0)
!!       call plot(0.001,0.001,-3) ! move origin a bit so lines on edge OK
!!       call poly(0.0,0.0,10.0,4.0,0.0) ! 10 inch square
!!       side_length=2.35
!!       xstart=(10.0-side_length)/2.0
!!       ystart=0.5
!!       angle=0.0
!!       do i = 3,12
!!          number_of_sides=real(i)
!!          call poly(xstart,ystart,side_length,number_of_sides,angle)
!!       enddo
!!       call plot(0.0,0.0,999)
!!    end program demo_poly
!!##LICENSE
!!   Public Domain
subroutine poly (x,y,side_length,rn,th)

! ident_7="@(#) M_calcomp poly(3f) draw an equilateral polygon"

real,intent(in) :: x,y
real,intent(in) :: side_length
real,intent(in) :: rn
real,intent(in) :: th
integer         :: i
integer         :: n
real            :: th1
real            :: th2
real            :: tho
real            :: xn
real            :: yn
   n = rn
   xn = x
   yn = y
   tho = th * 0.01745                      ! convert angle from degrees to radians
   call plot(x,y,3)
   select case(n)
    case(:-1)                              ! start
      th1 = (-6.2832) / rn
      th2 = (-th1) * 2.0
      n=-n
      do i = 1,n
         xn = xn + side_length * cos(tho)
         yn = yn + side_length * sin(tho)
         call plot(xn,yn,2)
         tho = tho + th1
         xn = xn + side_length * cos(tho)
         yn = yn + side_length * sin(tho)
         call plot(xn,yn,2)
         tho = tho + th2
      enddo
    case(0)
    case(1:)                                ! regular polygon
      th1 = 6.2832 / rn
      do i = 1,n
         xn = xn + side_length * cos(tho)
         yn = yn + side_length * sin(tho)
         call plot(xn,yn,2)
         tho = tho + th1
      enddo
   end select
end subroutine poly
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    rect(3f) - [M_calcomp:general] draw a rectangle
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine rect(xpage,ypage,height,width,angle,ipen)
!!
!!##DESCRIPTION
!!
!!   RECT is a FORTRAN subroutine used to draw rectangles.
!!
!!##OPTIONS
!!
!!    XPAGE,YPAGE  are the coordinates, in inches, of the rectangle's lower
!!                 left corner, before rotation.
!!
!!    HEIGHT       is the rectangle's height, in inches.
!!
!!    WIDTH        is the rectangle's width, in inches. (This parameter
!!                 defines the base of the rectangle.)
!!
!!    ANGLE        is the angle, in degrees, at which the rectangle's base
!!                 is to be drawn. (Rectangle is rotated about XPAGE,YPAGE.)
!!
!!    IPEN         is the code that moves the pen to the rectangle's
!!                 starting point.
!!
!!                 If the value of IPEN is:
!!
!!                  3, the pen is up for the move;
!!                  2, the pen is down for the move.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_rect
!!    use M_calcomp, only : plots, plot, newpen, rect
!!    use M_calcomp, only : END,MOVE,DRAW
!!    implicit none
!!    real  :: xmax=8.5,ymax=7.0
!!    real  :: xstart=2.5, ystart=1.0 ! lower left corner before rotation
!!    real  :: height=3.0, wdth=5.0
!!    real  :: angle
!!       call plots(0.0,xmax,0.0,ymax)
!!       ! (make a small dot at xstart,ystart>
!!       call rect(xstart,ystart,0.04,0.04,45.0,MOVE)
!!       ! rectangle
!!       call newpen(1)
!!       angle=0.0
!!       call rect(xstart,ystart,height,wdth,angle,MOVE)
!!       ! rotated rectangle
!!       angle=45.0
!!       call newpen(2)
!!       call rect(xstart,ystart,height,wdth,angle,MOVE)
!!       ! end graphics
!!       call plot(0.0,0.0,END)
!!    end program demo_rect
!!##LICENSE
!!   Public Domain
SUBROUTINE RECT (X,Y,H,W,TH,IV)

! ident_8="@(#) M_calcomp rect(3f) draw a rectangle"

real    :: h
integer :: iv
real    :: th
real    :: theta
real    :: w
real    :: x
real    :: x1
real    :: xc
real    :: xs
real    :: y
real    :: y1
   theta = th/57.2958
   xs = sin(theta)
   xc = cos(theta)
   call plot(x,y,iv)
   x1 = x - h * xs
   y1 = y + h * xc
   call plot(x1,y1,2)
   x1 = x1 + w * xc
   y1 = y1 + w * xs
   call plot(x1,y1,2)
   x1 =  x + w * xc
   y1 = y  + w * xs
   call plot(x1,y1,2)
   call plot(x,y,2)
end subroutine rect
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine solut  (x,n)

! EARLIER VERSION OF THIS SUBROUTINE WAS
!     SUBROUTINE  SOLUTN (X,N)
real    ::  x(8,9)
integer :: n
integer :: i
integer :: in
integer :: it
integer :: j
integer :: k
integer :: l
integer :: nm1
integer :: np1
real    :: dx
real    :: ratio
real    :: xt
      nm1 = n - 1
      np1 = n + 1
      do 10 i = 1,nm1
         l = i + 1
         it = i
         do in = l,n
            if(abs(x(it,i))-abs(x(in,i))) 5,6,6
5           it = in
6        continue
         enddo
         if(x(it,i)) 8,7,8
7        n = 0
         return
8        if(it - i) 17,17,16
16       do in = i,np1
            xt = x(i,in)
            x(i,in) = x(it,in)
            x(it,in) = xt
         enddo
17       do 10 j = l,n
            ratio = x(j,i)/x(i,i)
            do 10 k = l,np1
10    x(j,k) = x(j,k)- ratio * x(i,k)
      do 40 i = 1,n
         dx = 0.0
         k = n - i + 1
         if(i-1) 40,40,20
20       continue
         do j = 2,i
            l = n + 2 - j
            dx = dx + x(k,l) * x(l,np1)
         enddo
40    x(k,np1) =(-x(k,np1) - dx) / x(k,k)
      return
end subroutine solut
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    curvx(3f) - [M_calcomp:scientific] plots a function of X over a given range
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine curvx(xo,xf,coeff1,exp1,coeff2,exp2,coeff3,exp3,coeff4,exp4)
!!
!!##DESCRIPTION
!!
!!   CURVX is a FORTRAN subroutine which plots a function of X over a given range.
!!
!!##OPTIONS
!!
!!    XO,XF  are the starting and ending values of X. (These are
!!           assumed to be inches.)
!!
!!    COEFF1,COEFF2,COEFF3,COEFF4  are the coefficients of the polynomial
!!                                 that defines the function to be plotted.
!!
!!    EXP1,EXP2,EXP3,EPX4  are the exponents of the polynomial that defines
!!                         the function to be plotted.
!!
!!   COMMENTS
!!
!!  The polynomial that defines the function to be plotted is:
!!
!!        Y=COEFF1*X**EXP1+COEFF2*X**EXP2+COEFF3*X**EXP3+COEFF4*X**EXP4
!!
!!  for values of X from XO to XF, where deltaX=0.01. Since values of X are
!!  assumed to be inches, any scaling required must be performed before calling
!!  this subroutine. Errors may be generated if X is zero or negative.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_curvx
!!    use M_calcomp
!!    implicit none
!!    ! based on concepts of CALIFORNIA COMPUTER PRODUCTS, 1968
!!    character(len=50) :: ibcd
!!    integer           :: inteq
!!    ! INITIALIZE GRAPHICS
!!       call plots(0.0,10.0,0.0,10.0)
!!    ! DRAW FRAME
!!       call plot(7.0,0.0,2)
!!       call plot(7.0,9.0,2)
!!       call plot(0.0,9.0,2)
!!       call plot(0.0,0.0,2)
!!    ! COMMENTS ARE INSERTED
!!       ibcd='SAMPLE OF SCIENTIFIC SUBROUTINES PACKAGE'
!!       call symbol(0.7,8.5,0.14,ibcd,inteq,0.0,40)
!!       ibcd='USING CURVY SUBROUTINE'
!!       call symbol(0.7,4.25,0.14,ibcd,inteq,0.0,23)
!!       ibcd='USING CURVX SUBROUTINE'
!!       call symbol(0.7,8.25,0.14,ibcd,inteq,0.0,23)
!!    ! TWO PAIRS OF AXES ARE DRAWN
!!       ibcd=''
!!       call axis(1.0,4.75,ibcd,-1,5.0,0.0,0.0,1.0)
!!       call axis(1.0,4.75,ibcd, 1,3.0,90.0,0.0,1.0)
!!       call axis(1.0,0.75,ibcd,-1,5.0,0.0,0.0,1.0)
!!       call axis(1.0,0.75,ibcd, 1,3.0,90.0,0.0,1.0)
!!    ! CURVX IS DRAWN
!!       call plot(1.0,4.75,-3)
!!       call curvx(0.1,5.0,2.40,0.0,0.75,2.0,-0.525,3.0,0.075,4.0)
!!       call plot(-1.0,-4.75,-3)
!!    ! CURVY IS DRAWN
!!       call plot(1.0,0.75,-3)
!!       call curvy(0.1,3.0,9.0,1.26,-6.0,2.52,1.0,3.78,0.0,0.0)
!!       call plot(-1.0,-0.75,-3)
!!    ! EQUATIONS ARE DRAWN
!!       ibcd='Y=0.075X**4-0.525X**3+0.75X**2+2.40'
!!       call symbol(3.0,7.75,0.09,ibcd,inteq,0.0,35)
!!       ibcd='X=Y**3.78-6Y**2.52+9Y**1.26'
!!       call symbol(3.0,3.90,0.09,ibcd,inteq,0.0,27)
!!       call nframe()
!!    !  CLOSE GRAPHICS
!!       call plot(11.0,0.0,999)
!!    end program demo_curvx
!!##LICENSE
!!   Public Domain
subroutine curvx  (x0,xf,a,e,b,f,c,g,d,h)

! ident_9="@(#) M_calcomp curvx(3f) plots a function of X over a given range"

!     CALL CURVX
!      (XO, XF, COEFF1, EXP1, COEFF2, EXP2, COEFF3, EXP3, COEFF4, EXP4)
!
!     XO, XF                           ARE THE STARTING AND ENDING
!                                      VALUES OF X IN INCHES.
!                                      POLYNOMIAL.
! EARLIER VERSION OF THIS SUBROUTINE WAS
!     SUBROUTINE CURVEX (X0,XF,A,E,B,F,C,G,D,H)
real    :: a
real    :: b
real    :: c
real    :: d
real    :: dlt
real    :: dx
real    :: e
real    :: f
real    :: g
real    :: h
integer :: i3
integer :: iv
integer :: n
real    :: x
real    :: x0
real    :: xf
real    :: xfct
real    :: y
!     INITIALIZATION -
!     GET LINE LENGTH
      dx  =  xf - x0
      i3 = 3
!     DEVELOP FACTORED DELTA
      call where(x,y,xfct)
      x = x0
      dlt = 0.01/xfct
!     CHECK LINE LENGTH (IF ZERO RETURN)
      if(dx) 10,20,15
!     IF NEGATIVE MAKE DELTA LIKEWISE
10    continue
      dlt = -dlt
!     COMPUTE NUMBER OF LINE POINTS
15    continue
      n = int(abs(dx/dlt) + 1.0)
!     CURVE FITTING PLOT LOOP
      do iv= 1,n
         y = a*x**e + b*x**f + c*x**g + d*x**h
         call plot(x,y,i3)
         x = x + dlt
         i3 = 2
      enddo
!     PLOT EXPLICIT FINAL POINT AND RETURN
      y = a*xf**e + b*xf**f + c*xf**g +d*xf**h
      call plot(xf,y,2)
20    continue
      return
end subroutine curvx
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    curvy(3f) - [M_calcomp:scientific] plots a function of Y over a given range
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine curvy(yo,yf,coeff1,exp1,coeff2,exp2,coeff3,exp3,coeff4,exp4)
!!
!!##DESCRIPTION
!!
!!  CURVY is a FORTRAN subroutine which plots a function of Y over a given range.
!!
!!##OPTIONS
!!
!!    YO,YF           are the starting and ending values of Y. (These are
!!                    assumed to be inches.)
!!
!!    COEFF1,COEFF2,COEFF3,COEFF4  are the coefficients of the polynomial that
!!                                 defines the function to be plotted.
!!
!!    EXP1,EXP2,EXP3,EXP4  are the exponents of the polynomial that defines
!!                         the function to be plotted.
!!
!!   COMMENTS
!!
!!  The polynomial that defines the function to be plotted is:
!!
!!        X=COEFF1*Y**EXP1+COEFF2*Y**EXP2+COEFF3*Y**EXP3+COEFF4*Y**EXP4
!!
!!  for values of Y from YO to YF, where deltaY=0.01. Since values
!!  of Y are assumed to be inches, any scaling required must be performed
!!  before calling this subroutine.
!!
!!  If Y is zero or negative, errors may be generated.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_curvx
!!    use M_calcomp
!!    implicit none
!!    ! based on concepts of CALIFORNIA COMPUTER PRODUCTS, 1968
!!    character(len=50) :: ibcd
!!    integer           :: inteq
!!    ! INITIALIZE GRAPHICS
!!       call plots(0.0,10.0,0.0,10.0)
!!    ! DRAW FRAME
!!       call plot(7.0,0.0,2)
!!       call plot(7.0,9.0,2)
!!       call plot(0.0,9.0,2)
!!       call plot(0.0,0.0,2)
!!    ! COMMENTS ARE INSERTED
!!       ibcd='SAMPLE OF SCIENTIFIC SUBROUTINES PACKAGE'
!!       call symbol(0.7,8.5,0.14,ibcd,inteq,0.0,40)
!!       ibcd='USING CURVY SUBROUTINE'
!!       call symbol(0.7,4.25,0.14,ibcd,inteq,0.0,23)
!!       ibcd='USING CURVX SUBROUTINE'
!!       call symbol(0.7,8.25,0.14,ibcd,inteq,0.0,23)
!!    ! TWO PAIRS OF AXES ARE DRAWN
!!       ibcd=''
!!       call axis(1.0,4.75,ibcd,-1,5.0,0.0,0.0,1.0)
!!       call axis(1.0,4.75,ibcd, 1,3.0,90.0,0.0,1.0)
!!       call axis(1.0,0.75,ibcd,-1,5.0,0.0,0.0,1.0)
!!       call axis(1.0,0.75,ibcd, 1,3.0,90.0,0.0,1.0)
!!    ! CURVX IS DRAWN
!!       call plot(1.0,4.75,-3)
!!       call curvx(0.1,5.0,2.40,0.0,0.75,2.0,-0.525,3.0,0.075,4.0)
!!       call plot(-1.0,-4.75,-3)
!!    ! CURVY IS DRAWN
!!       call plot(1.0,0.75,-3)
!!       call curvy(0.1,3.0,9.0,1.26,-6.0,2.52,1.0,3.78,0.0,0.0)
!!       call plot(-1.0,-0.75,-3)
!!    ! EQUATIONS ARE DRAWN
!!       ibcd='Y=0.075X**4-0.525X**3+0.75X**2+2.40'
!!       call symbol(3.0,7.75,0.09,ibcd,inteq,0.0,35)
!!       ibcd='X=Y**3.78-6Y**2.52+9Y**1.26'
!!       call symbol(3.0,3.90,0.09,ibcd,inteq,0.0,27)
!!       call nframe()
!!    !  CLOSE GRAPHICS
!!       call plot(11.0,0.0,999)
!!    end program demo_curvx
!!##LICENSE
!!    Public Domain
subroutine curvy  (y0,yf,a,e,b,f,c,g,d,h)

! ident_10="@(#) M_calcomp curvy(3f) plots a function of Y over a given range"

!     CALL CURVY
!      (YO, YF, COEFF1, EXP1, COEFF2, EXP2, COEFF3, EXP3, COEFF4, EXP4)
!
!     YO, YF                           ARE THE STARTING AND ENDING
!                                      VALUES OF Y IN INCHES.
!     COEFF1, COEFF2, COEFF3, COEFF4   ARE THE COEFFICIENTS OF THE
!                                      POLYNOMIAL.
!     EXP1, EXP2, EXP3, EXP4           ARE THE EXPONENTS OF THE
!                                      POLYNOMIAL.
! EARLIER VERSION OF THIS SUBROUTINE WAS
!     SUBROUTINE CURVEY (Y0,YF,A,E,B,F,C,G,D,H)
real    :: a
real    :: b
real    :: c
real    :: d
real    :: dlt
real    :: dy
real    :: e
real    :: f
real    :: g
real    :: h
integer :: i3
integer :: iv
integer :: n
real    :: x
real    :: xfct
real    :: y
real    :: y0
real    :: yf
!     INITIALIZATION -
!     GET LINE LENGTH
      dy  =  yf - y0
      i3 = 3
!     DEVELOP FACTORED DELTA
      call where(x,y,xfct)
      y = y0
      dlt = 0.01/xfct
!     CHECK LINE LENGTH (IF ZERO RETURN)
      if(dy) 10,20,15
!     IF NEGATIVE MAKE DELTA LIKEWISE
10    continue
      dlt = -dlt
!     COMPUTE NUMBER OF LINE POINTS
15    continue
      n = int(abs(dy/dlt) + 1.0)
!     CURVE FITTING PLOT LOOP
      do iv= 1,n
         x = a*y**e + b*y**f + c*y**g + d*y**h
         call plot(x,y,i3)
         y = y + dlt
         i3 = 2
      enddo
!     PLOT EXPLICIT FINAL POINT AND RETURN
      x = a*yf**e + b*yf**f + c*yf**g +d*yf**h
      call plot(x,yf,2)
20    continue
end subroutine curvy
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine fit4(px1,py1,px2,py2,vecx1,vecy1,vecx3,vecy3)

real    :: ax
real    :: ay
real    :: bx
real    :: by
real    :: d
real    :: d1
real    :: d2
real    :: dv
integer :: i
integer :: n
real    :: px1
real    :: px2
real    :: py1
real    :: py2
real    :: t
real    :: uux1
real    :: uux2
real    :: uuy1
real    :: uuy2
real    :: vecx1
real    :: vecx3
real    :: vecy1
real    :: vecy3
real    :: x
real    :: x1
real    :: y
real    :: y1

real,save    :: vx2 = 0.0
real,save    :: vy2 = 0.0
real,save    :: vx3 = 0.0
real,save    :: vy3 = 0.0
real,save    :: d3  = 0.0
real,save    :: ux1 = 0.0
real,save    :: uy1 = 0.0
real,save    :: ux2 = 0.0
real,save    :: uy2 = 0.0
      x1=px1
      y1=py1
      call where(x,y,d)
      d = 0.01/d
      if(abs(x1-x)-d) 10,2,2
10    if(abs(y1-y)-d) 11,2,2
11    if(vecx1-vx2)  5,12,5
12    if(vecy1-vy2)  5,13,5
13    if(vx3-px2+x1) 5,14,5
14    if(vy3-py2+y1) 5, 6,5
2     call plot(px1,py1,3)
5     vx3=px2-x1
      vy3=py2-y1
      vx2=vecx1
      vy2=vecy1
      d2=vx2*vx2+vy2*vy2
      t=1.0
      goto 7
6     t=0.0
      vx2=vx3
      vy2=vy3
      vx3=vecx3
      vy3=vecy3
      d2=d3
      ux1=ux2
      uy1=uy2
7     d3=vx3*vx3+vy3*vy3
      ux2=d2*vx3+d3*vx2
      uy2=d2*vy3+d3*vy2
      dv = 1.0/sqrt(ux2*ux2+uy2*uy2+0.00001)
      ux2=dv*ux2
      uy2=dv*uy2
      if(t)6,8,6
8     d=abs(ux1*vx2+uy1*vy2)
      d1=d
      uux1=d*ux1
      uuy1=d*uy1
      d=abs(ux2*vx2+uy2*vy2)
      uux2=d*ux2
      uuy2=d*uy2
      d=d+d1
      ax=uux2+uux1-vx2-vx2
      bx=vx2-uux1-ax
      ay=uuy2+uuy1-vy2-vy2
      by=vy2-uuy1-ay
      n=10.*d+1.0
      d=1.0/float (n)
      do i=1,n
         t=t+d
         x=((ax*t+bx)*t+uux1)*t+x1
         y=((ay*t+by)*t+uuy1)*t+y1
         call plot(x,y,2)
      enddo
end subroutine fit4
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    fline(3f) - [M_calcomp:scientific] plot a polyline with optional fit
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine fline(xarray,yarray,npts,inc,+-lintyp,inteq)
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!
!!     XARRAY   is the name of the array containing the values to be
!!              plotted as the abscissa and the appropriate scaling
!!              parameters.
!!
!!     YARRAY   is the name of the array containing the values to be
!!              plotted as the ordinates and the appropriate scaling
!!              parameters.
!!
!!     NPTS     is the number of data points to be plotted:
!!
!!              if NPTS >0 a straight line is drawn between the points.
!!
!!              if NPTS <0 a smooth curve, drawn using a modified
!!              spline-fitting technique, is drawn between the points.
!!
!!     INC      is the increment between elements in the array to be
!!              plotted. INC >1 if the values to be plotted are in a
!!              mixed array. (Usually INC = 1.)
!!     LINTYP   is used to control the type of graph produced:
!!
!!              if LINTYP = 0 a line is plotted between successive data
!!              points. (No symbols are plotted.)
!!
!!              if LINTYP = 1 a line plot is produced, with a symbol at
!!              each data point.
!!
!!              if LINTYP = n a line plot is produced, with a symbol at
!!              every nth data point.
!!
!!              if LINTYP = -n, connecting lines are not plotted between
!!              data points; a symbol appears at every nth data point.
!!
!!     INTEQ    is the integer equivalent used to specify the symbol to
!!              be plotted at a data point. (Refer to the description of
!!              SYMBOL for possible values of INTEQ.)
!!
!!  COMMENTS
!!
!!  The arrays must be dimensioned with at least NPTS + 2 elements. The
!!  adjusted minimum value (FIRSTV) and the adjusted delta value (DELTAV),
!!  normally provided by the SCALE subroutine, must be stored following
!!  the data array.
!!
!!  For the X array, the adjusted minimum is stored in XARRAY (NPTS*INC +
!!  1), and the adjusted delta is in XARRAY (NPTS*INC + INC + 1). Similarly,
!!  for the Y array, the minimum is in YARRAY (NPTS*INC + 1), and the delta
!!  is in YARRAY (NPTS*INC + INC + 1). Therefore, XARRAY and YARRAY must
!!  be dimensioned to be at least NPTS*INC+INC+1 .
!!
!!  If scaling is not required, the user must place the appropriate
!!  minimum and delta values in the specified elements in the arrays. For
!!  a one-to-one correspondence between array data and plotted data, these
!!  values should be 0.0 (minimum) and 1.0 (delta).
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_fline
!!    use M_calcomp
!!    implicit none
!!    ! based on concepts of CALIFORNIA COMPUTER PRODUCTS, 1968
!!    real :: xar(10)=[0.75,1.75,2.25,2.75,3.25,4.25,4.75,5.75,0.0,1.0]
!!    real :: yar(10)=[3.25,2.00,5.25,6.50,6.75,6.25,3.25,4.25,0.0,1.0]
!!    character(len=50) :: ibcd
!!    integer           :: inteq
!!       call plots(0.0,10.0,0.0,10.0)
!!    !     DRAW FRAME
!!       call plot(7.0,0.0,2)
!!       call plot(7.0,9.0,2)
!!       call plot(0.0,9.0,2)
!!       call plot(0.0,0.0,2)
!!    !     DRAW AXIS
!!       ibcd='SERVICE TIME'
!!       call axis(0.75,0.75,ibcd,-12,5.0,0.0,5.0,1.0)
!!       ibcd='FREQUENCY'
!!       call axis(0.75,0.75,ibcd, 9,7.0,90.0,0.0,100.0)
!!    !     DRAW COMMENTS
!!       ibcd='USING FLINE AND SMOOT SUBROUTINES'
!!       call symbol(0.7,8.25,0.14,ibcd,inteq,0.0,34)
!!       call plot(5.0,7.8,3)
!!       call plot(5.1,7.8,2)
!!       ibcd='SMOOT'
!!       call symbol(5.2,7.80,0.09,ibcd,inteq,0.0, 6)
!!       inteq = 1
!!       call symbol(5.0,7.60,0.10,ibcd,inteq,0.0,-1)
!!       inteq=999
!!       ibcd='FLINE'
!!       call symbol(5.2,7.60,0.09,ibcd,inteq,0.0, 5)
!!    ! SMOOTHING
!!       call smoot(0.75,3.75,0)
!!       call smoot(1.75,2.5,-2)
!!       call smoot(2.25,5.75,-2)
!!       call smoot(2.75,7.0,-2)
!!       call smoot(3.25,7.25,-2)
!!       call smoot(4.25,6.75,-2)
!!       call smoot(4.75,3.75,-2)
!!       call smoot(5.75,4.75,-24)
!!    ! FLINE IS USED
!!       call plot(0.75,3.25,3)
!!       call fline(xar, yar, -8,1,1,1)
!!       call nframe()
!!    end program demo_fline
!!##LICENSE
!!    Public Domain
subroutine fline (x,y,nn,k,j,l)

! ident_11="@(#) M_calcomp fline(3f) plot a polyline with optional fit"

!             X  IS THE NAME OF THE ARRAY OF UNSCALED ORDINATE VALUES.
!             Y  IS THE NAME OF THE ARRAY OF UNSCALED ABSCISSA VALUES.
!             N  IS THE NUMBER OF POINTS IN THE ARRAY, NEGATIVE TO FIT
!             K  IS THE REPEAT CYCLE OF A MIXED ARRAY (NORMALLY = 1).
!             J  IS THE ALTERNATE NUMBER OF DATA POINT TO PLOT A SYMBOL.
!                 J WILL = 0 FOR LINE PLOT,NEGATIVE FOR POINT PLOT,
!                 J = 1 FOR POINT FOR EVERY DATA POINT,2 FOR EVERY OTHER
!             L  IS AN INTEGER DESCRIBING SYMBOL TO BE USED, SEE SYMBOL
!                 ROUTINE FOR LIST
!
!        NOTE THIS ROUTINE EXPECTS  XMIN,DX,YMIN AND DY TO BE STORED IN
!              X(N*K+1),X(N*K+1+K),Y(N*K+1),AND Y(N*K+1+K) RESPECTIVELY.
!
      DIMENSION X(*), Y(*)
!
!  THE VARIABLE IBCD HAS BEEN DECLARED AS CHARACTER TYPE FOR USE IN
!  THE CALL TO THE 'SYMBOL' ROUTINE. SINCE 'IBCD' HAS NO MEANING IN
!  THE PARTICULAR CALL TO 'SYMBOL' FOUND IN THIS SUBROUTINE, 'IBCD'
!  IS NOT INITIALIZED.
!
!  THE VARIABLE 'INTEQ' HAS BEEN CHANGED FROM AN ARRAY DIMENSIONED 2
!  (IN THE CDC VERSION OF THE CALCOMP LIBRARY) TO A SIMPLE INTEGER
!  VARIABLE AS THE CRAY VERSION OF THE 'SYMBOL' ROUTINE EXPECTS A
!  SIMPLE INTEGER VALUE FOR THIS ARGUMENT.
!
real    :: df
real    :: dl
real    :: dx
real    :: dy
integer :: i
integer :: ic
integer :: ica
integer :: inteq
integer :: is
integer :: isa
integer :: j
integer :: k
integer :: kk
integer :: l
integer :: ldx
integer :: lmin
integer :: lp
integer :: lsw
integer :: n
integer :: na
integer :: nf
integer :: nfp
integer :: nl
integer :: nlp
integer :: nn
integer :: nt
integer :: nw
real    :: su
real    :: sv
real    :: u
real    :: u1
real    :: u2
real    :: v
real    :: v1
real    :: v2
real    :: x
real    :: x0
real    :: xmin
real    :: xn
real    :: xn1
real    :: y
real    :: y0
real    :: ymin
real    :: yn
real    :: yn1
      character(len=8) :: ibcd
      inteq = l
      n=abs(nn)
      kk=k
      lmin=n*kk+1
      ldx =lmin+kk
      nl  =lmin-kk
      xmin=x(lmin)
      ymin=y(lmin)
      dx=x(ldx)
      dy=y(ldx)
      call where(xn,yn,df)
      df= max( abs((x( 1)-xmin)/dx-xn), abs((y( 1)-ymin)/dy-yn))
      dl= max( abs((x(nl)-xmin)/dx-xn), abs((y(nl)-ymin)/dy-yn))
      ic=3
      is=-1
      nt= abs(j)
      if(j) 2,1,2
1     continue
      nt=1
2     continue
      if(df-dl) 3,3,4
3     continue
      nf=1
      na=nt
      goto 5
4     continue
      kk=-kk
      nf=nl
      nl=1
      na=((n-1)/nt)*nt+nt-n+1
5     continue
      if  (j) 6,7,8
6     continue
      ica=3
      isa=-1
      lsw=1
      goto 9
7     continue
      na=ldx
8     continue
      ica=2
      isa=-2
      lsw=0
9     continue
      xn1=(x(nf)-xmin)/dx
      yn1=(y(nf)-ymin)/dy
      nf=nf+kk
      if(nn) 10,10,25
10    continue
      x0=xn1
      y0=yn1
      lp=nl
      nlp=lp-kk
      nfp = nf-kk
      u = (x(nf)-x(nfp))/dx
      v = (y(nf)-y(nfp))/dy
      u1=(x(lp)-x(nlp))/dx
      v1=(y(lp)-y(nlp))/dy
      su=u
      sv=v
      if(x(nfp)-x(lp)) 13,12,13
12    continue
      if(y(nfp)-y(lp)) 13,25,13
13    continue
      nfp=nlp-kk
      su=(x(nlp)-x(nfp))/dx
      sv=(y(nlp)-y(nfp))/dy
      call reflx(u1,v1,su,sv)
      nfp=nf+kk
      u1=(x(nfp)-x(nf))/dx
      v1=(y(nfp)-y(nf))/dy
      call reflx(u,v,u1,v1)
25    continue
      do i=1,n
         xn=xn1
         yn=yn1
         if(n-i) 11,11,14
14       continue
         xn1=(x(nf)-xmin)/dx
         yn1=(y(nf)-ymin)/dy
11       continue
         nw=na-nt
         if(nw) 29,26,26
29       continue
         if(lsw) 17,26,17
26       continue
         if(nn) 16,24,15
15       continue
         call plot(xn,yn,ic)
         goto 20
16       continue
         if(ic-2) 15,17,15
17       continue
         if(n-i) 27,27,18
27       continue
         u2=su
         v2=sv
         goto 19
18       continue
         u2=xn1-xn
         v2=yn1-yn
19       continue
         call fit4(x0,y0,xn,yn,u1,v1,u2,v2)
         u1=u
         v1=v
         u=u2
         v=v2
         x0=xn
         y0=yn
20       continue
         na=na+1
         if(nw) 22,28,22
!
!  THE FOLLOWING CALL TO THE 'SYMBOL' ROUTINE HAS BEEN MODIFIED TO
!  CONVERT THIS SUBROUTINE TO THE CRAY. ON THE CRAY, 'SYMBOL' HAS
!  7 ARGUMENTS; ON THE CDC, 'SYMBOL' HAS 6 ARGUMENTS. THE ADDITIONAL
!  ARGUMENT IN THIS CALL IS 'IBCD'.
!
28       continue
         call symbol(xn,yn,0.08,ibcd,inteq,0.0,-1)
         na=1
22       continue
         ic=ica
         nf=nf+kk
      enddo
24    continue
      return
end subroutine fline
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    lgaxs(3f) - [M_calcomp:scientific] draw logarithmic axis
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine lgaxs(xpage,ypage,ibcd,+-nchar,axlen,angle,firstv,deltav)
!!
!!##DESCRIPTION
!!
!!  LGAXS(3f) is a FORTRAN subroutine which draws a logarithmic axis
!!  with annotation in powers of ten. LGAXS(3f) is similar in operation
!!  to AXIS(3f).
!!
!!##OPTIONS
!!
!!     XPAGE,YPAGE  are the coordinates, in inches, of the axis starting
!!                  point.
!!
!!     IBCD         is the character data to be used as the axis title (may
!!                  be one or more characters). It is centered and
!!                  positioned parallel to the axis.
!!
!!     NCHAR        is the number of characters in the axis title.
!!
!!                  if NCHAR is:
!!
!!                    o negative, annotation is placed on the clockwise
!!                      side of the axis (normally used for the X axis);
!!
!!                    o positive, annotation is placed on the
!!                      counterclockwise side of the axis (normally used
!!                      for the Y axis).
!!
!!     AXLEN        is the length of the axis, in inches.
!!
!!     ANGLE        is the angle, in degrees, at which the axis is to be
!!                  drawn. (The axis is rotated about XPAGE,YPAGE). The X
!!                  axis is at 0 degrees; the Y axis is at 90 degrees.
!!
!!     FIRSTV       is the value of annotation at the beginning of the axis.
!!
!!     DELTAV       is the number of log cycles per inch (the reciprocal of
!!                  the length of one cycle, in inches).
!!
!!                  Equivalently this is the reciprocal of
!!                  the length of one cycle.
!!
!!                  if SCALOG() is used to scale a logarithmic array, VARRAY,
!!                  then FIRSTV=VARRAY(NV*K+1), and DELTAV=VARRAY(NV*K+K+1)
!!                  where NV is number of values used and K is repeat cycle
!!                  of location of values in array, as described in SCALOG().
!!  COMMENTS
!!
!!  A tick mark is placed on the axis for each power of ten and for each
!!  of the nine integer values between.
!!
!!  Annotation is placed at the tick marks as follows:
!!
!!     o If a cycle is not less than two inches long, the integer tick marks
!!       are annotated.
!!
!!     o The power-of-ten tick marks are annotated in the form 10**N.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_lgaxs
!!    use M_calcomp
!!    implicit none
!!    ! based on concepts of CALIFORNIA COMPUTER PRODUCTS, 1968
!!    character(len=50) :: ibcd
!!    integer           :: i, k
!!    integer           :: inteq
!!    real              :: a
!!    real              :: angl
!!    real              :: angle(19)
!!    real              :: bang
!!    real              :: beta
!!    real              :: r(19)
!!    real              :: theta
!!    real              :: x
!!    real              :: xx
!!    real              :: xa,ya, xb,yb, xc,yc, xd,yd
!!    real              :: xar(8)= [ 1.00, 2.00, 3.00, 4.00, 5.00, 6.00       , 0.0, 0.0 ]
!!    real              :: yar(8)= [ 250.0, 110.0, 500.0, 900.0, 200.0, 140.0 , 0.0, 1.0 ]
!!       call plots(0.0,10.0,0.0,10.0)
!!    ! DRAW FRAME
!!       call plot(7.0,0.0,2)
!!       call plot(7.0,9.0,2)
!!       call plot(0.0,9.0,2)
!!       call plot(0.0,0.0,2)
!!    ! DRAW COMMENTS
!!       ibcd='USING SCALG, LGAXS, AND LGLIN SUBROUTINES'
!!       call symbol(0.7,8.25,0.14,ibcd,inteq,0.0,42)
!!       ibcd='USING POLAR SUBROUTINE'
!!       call symbol(0.7,3.80,0.14,ibcd,inteq,0.0,22)
!!    ! AXIS IS DRAWN
!!       ibcd='ALTITUDE'
!!       call axis(1.0,4.75,ibcd,-8,5.0,0.0,0.0,25.0)
!!       call scalg(yar,3.0,6,1)
!!       ibcd='TEMPERATURE'
!!       call lgaxs(1.0,4.75,ibcd,11,3.0,90.0,yar(7),yar(8))
!!       call scale(xar,5.0,6,1)
!!       call plot(1.0,4.75,-3)
!!       call lglin(xar,yar,6,1,0,1,1)
!!       call plot(-1.0,-4.75,-3)
!!    ! POLAR SUBROUTINE IS USED
!!       x=0.0
!!       do k=1,19
!!          theta=x*0.0174533
!!          r(k)=2.0*(1.0-cos(theta))
!!          angle(k)=theta
!!          x=x+10.0
!!       enddo
!!       call plot(5.0,0.75,-3)
!!       call polar(r,angle,19,1,0,1,0.0,1.0)
!!       angl =30.0
!!       a=1.0
!!       do  i=1,5
!!          theta=angl *0.0174533
!!          xa=cos(theta)
!!          ya=sin(theta)
!!          call plot(xa,ya,3)
!!          xb=1.1*xa
!!          yb=1.1*ya
!!          call plot(xb,yb,2)
!!          xc=xb+0.05*xa
!!          yc=yb+0.05*ya
!!          if((i-3).gt.0)then
!!             a=1.5
!!          endif
!!          beta=1.570797-theta
!!          xd=xc-0.105*a*cos(beta)
!!          yd=yc+0.105*a*sin(beta)
!!          bang=270.0+angl
!!          call number(xd,yd,0.105,angl, bang,-1)
!!          angl =angl +30.0
!!       enddo
!!       xx=0.0
!!       do i=1,19
!!          angle(i) = xx*0.0174533
!!          r(i) = 1.0
!!          xx=xx+10.0
!!       enddo
!!       call polar(r,angle,19,1,0,1,0.0,1.0)
!!       call plot(-5.0,-0.75,-3)
!!    ! AXIS IS DRAWN
!!       ibcd=''
!!       call axis(1.0,0.75,ibcd,-1,4.0,0.0,4.0,-1.0)
!!       call axis(5.0,0.75,ibcd,-1,1.0,0.0,0.0,1.0)
!!       ibcd='RADIUS=2*(1-COS(ANGLE))'
!!       call symbol(3.75,3.5,0.09,ibcd,inteq,0.0,23)
!!       call plot(11.0,0.0,999)
!!    end program demo_lgaxs
!!##LICENSE
!!    Public Domain
subroutine lgaxs(xo,yo,ibcd,n,dist,theta,vorg,delta)

! ident_12="@(#) M_calcomp lgaxs(3f) draw logarithmic axis"

! EARLIER VERSION OF THIS SUBROUTINE WAS

!     SUBROUTINE LGAXIS(XO,YO,IBCD,N,DIST,THETA,VORG,DELTA)

      DIMENSION BLOG(10)
!
!  THE VARIABLE 'IBCD' HAS BEEN CHANGED TO CHARACTER TYPE TO MAKE
!  IT COMPATIBLE WITH THE VERSION OF THE 'SYMBOL' ROUTINE AVAILABLE
!  ON THE CRAY.
!
!  THE VARIABLE 'INTEQ' IS ALSO USED IN THE CALL TO THE 'SYMBOL'
!  ROUTINE. 'INTEQ' IS THE INTEGER EQUIVALENT OF THE SYMBOL TO BE
!  DRAWN. IF 'INTEQ' HAS A VALUE BETWEEN 0 AND 91 INCLUSIVE, A
!  SYMBOL IS DRAWN EVEN IF A TITLE HAS BEEN SPECIFIED. TO PREVENT
!  THIS FROM HAPPENING IN THIS SUBROUTINE, 'INTEQ' IS INITIALIZED
!  TO THE VALUE 999 (WHICH HAS NO SIGNIFICANCE FOR 'SYMBOL').
!
real    :: ai
real    :: bcdx
real    :: bcdy
real    :: blen
real    :: blmn
real    :: blog
real    :: cist
real    :: cost
real    :: d1
real    :: d2
real    :: d3
real    :: d4
real    :: d5
real    :: d6
real    :: delta
real    :: dist
real    :: eto10
real    :: fj
real    :: fxmn
integer :: i
integer :: inteq
integer :: k
integer :: n
integer :: nc
real    :: sint
real    :: size
real    :: size1
real    :: sont
real    :: th
real    :: theta
real    :: vorg
real    :: x
real    :: x0
real    :: xo
real    :: y
real    :: y0
real    :: yo
      character(len=*) :: ibcd
      save inteq,sont,cist,d1,d2,d3,d4,d5,d6
      data inteq/999/,sont,cist,d1,d2,d3,d4,d5,d6/8*0.0/
!
!  SAVE ARGUMENTS IN X, Y, NC, SIZE.
      x = xo
      y = yo
      nc = n
      size = dist
!  CONVERT DEGREES TO RADIANS, STORING IN TH.
      th=0.01745329*theta
!  STORE LOGS OF INTEGERS 2-10
      eto10 = 0.4342945
      do i=1,9
         blog(i) = eto10*log(float(i))
      enddo
!  SET FXMN TO GREATEST INTEGER POWER OF TEN LESS THAN OR EQUAL TO LOG
!     OF XMIN.
      fxmn = int (eto10*log(vorg)+100.0001)-100
!  CALCULATE LENGTH FROM BEGINNING OF CYCLE CONTAINING VORG TO BEGINNING
!     OF AXIS, PLUS FACTOR PREVENTING ROUND-OFF ERROR, STORING IN BLMN.
      blmn = (eto10*log(vorg)-fxmn)/delta-0.0001
!  STORE SIN AND COS OF TH.
      sint=sin(th)
      cost=cos(th)
!  SET OFFSET CONSTANTS OF ANNOTATION, DEPENDING ON SIGN OF NC.
      if(nc) 20,40,30
20    d1=0.24*sint
      d2=(-0.24)*cost
      d3=0.12*sint-d2
      d4=(-0.12)*cost+d1
      d5=0.2*sint-0.03*cost
      d6=(-0.2)*cost-0.03*sint
      nc=-nc
      sont=sint
      cist=cost
      bcdx= x +(size-0.12*float(nc))/2.*cost + 0.48*sint
      bcdy= y +(size-0.12*float(nc))/2.*sint - 0.48*cost
      goto 40
30    d1=(-0.1)*sint
      d2=0.1*cost
      d3=(-0.22)*sint+0.24*cost
      sont=-sint
      cist=-cost
      d4=0.22*cost+0.24*sint
      d5=d1-0.03*cost
      d6=d2-0.03*sint
      bcdx=x +(size-0.12*float(nc))/2.*cost-0.34*sint
      bcdy=y +(size-0.12*float(nc))/2.*sint+0.34*cost
!  CALCULATE COORDINATES OF START OF CYCLE CONTAINING VORG,
!     AND STORE IN X0, Y0 .
40    x0=x -blmn*cost
      y0=y -blmn*sint
!  CALCULATE LENGTH OF AXIS PLUS LENGTH OF CYCLE PRECEDING AXIS PLUS
!     ROUND-OFF ERROR FACTOR, AND STORE IN SIZE1 .
      size1=size+blmn+0.0002
!  INITIALIZE CYCLE COUNTER FJ.
      fj = 0.0
!  MOVE PEN TO START OF AXIS.
      call plot(x,y,3)
!  LOOP THRU CYCLE.
!  AI DETERMINES HEIGHT OF TIC MARK, LARGE TIC MARK FOR 10**N AXIS VALUE
55    ai = 0.14
      do 60 i=1,9
!  CALCULATE NEW BLEN, LENGTH TO NEXT TIC MARK.
         blen = (blog(i)+fj)/delta
!  IF TIC MARK IS BEFORE START OF AXIS, GO TO NEXT TIC MARK.
         if(blen-blmn) 60,56,56
!  IF TIC MARK IS BEYOND END OF AXIS, GO TO DRAW LINE TO END OF AXIS.
56       if(blen-size1) 57,57,70
!  CALCULATE COORDINATES OF TIC MARK AND PLOT IT.
57       x=x0+blen*cost
         y=y0+blen*sint
         call plot(x,y,2)
         call plot(x+     ai*sont,y-     ai*cist,2)
         call plot(x,y,2)
60    ai = .07
!  INCREMENT FJ TO NEXT CYCLE.
      fj = fj+1.0
!  RETURN FOR NEXT CYCLE.
      goto 55
!  DRAW LINE TO END OF AXIS.
70    call plot(x0+size1*cost,y0+size1*sint,2)
!  LOOP BACKWARD THRU CYCLE FOR ANNOTATING TIC MARKS.
85    do 110 k=1,9
         i=10-k
!  CALCULATE DISTANCE FROM START OF FIRST CYCLE TO TIC MARK.
         blen = (blog(i)+fj)/delta
!  IF TIC MARK IS LOCATED BEFORE START OF AXIS, GO TO DRAW AXIS TITLE.
         if(blen-blmn) 120,86,86
!  IF TIC MARK IS BEYOND END OF AXIS, GO TO NEXT TIC MARK.
86       if(blen-size1) 87,87,110
!  IF TIC MARK IS AT INTEGER POWER OF 10, ANNOTATE WITH 10 AND EXPONENT.
87       if(i-1) 100,90,100
90       call number(x0+blen*cost+d1,y0+blen*sint+d2,0.14,10.0,theta,-1)
         call number( x0+blen*cost+d3,y0+blen*sint+d4,0.07,fxmn+fj,theta,-1)
         goto 110
!  IF CYCLE LENGTH IS LESS THAN 2 INCHES, GO TO NEXT TIC MARK.
100      if(delta-0.5) 105,105,110
!  ANNOTATE INTERMEDIATE TIC MARK.
105      call number(x0+blen*cost+d5,y0+blen*sint +d6, 0.105, float(i),theta,-1)
110   continue
!  DECREMENT CYCLE COUNTER.
      fj = fj-1.0
!  GO TO LOOP THRU NEXT CYCLE.
      goto 85
!  TEST FOR ANNOTATING AXIS TITLE.
120   if(nc) 125,130,125
!  DRAW AXIS TITLE.
!
!  THE FOLLOWING CALL TO THE 'SYMBOL' ROUTINE HAS BEEN MODIFIED TO
!  CONVERT THIS SUBROUTINE TO THE CRAY. ON THE CRAY, 'SYMBOL' HAS
!  7 ARGUMENTS; ON THE CDC, 'SYMBOL' HAS 6 ARGUMENTS. THE ADDITIONAL
!  ARGUMENT IN THIS CALL IS 'INTEQ'.
!
125   call symbol(bcdx,bcdy,0.14,ibcd,inteq,theta,nc)
130   continue
end subroutine lgaxs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    lglin(3f) - [M_calcomp:scientific] draw polyline in log-log or semi-log mode
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine lglin(xarray,yarray,npts,inc,+-lintyp,inteq,logtyp)
!!
!!##DESCRIPTION
!!
!!   LGLIN(3f) is a FORTRAN subroutine used to plot data either in log-log or semi-log
!!   mode. LGLIN(3f) is similar in operation to LINE(3f).
!!
!!##OPTIONS
!!
!!    XARRAY,YARRAY  are the arrays containing the variables to be plotted as
!!                   abscissa and ordinate, respectively; either logarithmic
!!                   or linear, depending on the value of LOGTYP. They also
!!                   contain the scaling parameters.
!!
!!    NPTS           is the number of points to be plotted.
!!
!!    INC            is the increment between elements in the array to be
!!                   plotted. INC>1 if the values to be plotted are in a
!!                   mixed array. (Usually INC = 1).
!!
!!    LINTYP         is used to control the type of graph produced:
!!
!!                   If LINTYP = 0 a line is plotted between successive data
!!                   points. No symbols are plotted.
!!
!!                   If LINTYP = 1 a line plot is produced, with a symbol at
!!                   each data point.
!!
!!                   If LINTYP = n a line plot is produced, with a symbol at
!!                   every nth data point.
!!
!!                   If LINTYP = -n, connecting lines are not plotted between
!!                   data points; a symbol appears at every nth data point.
!!
!!    INTEQ          is the integer equivalent used to specify the symbol to
!!                   be plotted at a data point. (Refer to the description of
!!                   SYMBOL for possible values of INTEQ.)
!!
!!    LOGTYP         is a code specifying the type of plot.
!!
!!                   If LOGTYP is:
!!
!!                   -1, a semi-log plot, logarithmic in X and linear in Y is
!!                   produced;
!!
!!                   0, a log-log plot is produced;
!!
!!                   +1, a semi-log plot, linear in X and logarithmic in Y is
!!                   produced.
!!
!!   COMMENTS
!!
!!  The arrays XARRAY and YARRAY must be dimensioned with at least NPTS + 2
!!  elements. The adjusted minimum values and the delta values (normally
!!  provided by the SCALG subroutine for logarithmic data and by the SCALE
!!  subroutine for linear data) must be stored in the data arrays.
!!
!!  For the X array, the adjusted minimum is stored in XARRAY (NPTS*INC + 1), and
!!  the adjusted delta is in XARRAY (NPTS*INC + INC + 1). Similarly, for the Y
!!  array, the minimum is in YARRAY (NPTS*INC + 1), and the delta is in YARRAY
!!  (NPTS*INC + INC + 1). Therefore, XARRAY and YARRAY must be dimensioned to be
!!  at least NPTS*INC+INC+1 words long.
!!
!!  If scaling is not required, the user must place the appropriate minimum and
!!  delta values in the specified locations in the arrays. For linear arrays,
!!  these values should be 0.0 (minimum) and 1.0 (delta), to ensure a one-to-one
!!  correspondence between array data and plotted data.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_lglin
!!    use M_calcomp
!!    implicit none
!!    ! based on concepts of CALIFORNIA COMPUTER PRODUCTS, 1968
!!    character(len=50) :: ibcd
!!    integer           :: i, k
!!    integer           :: inteq
!!    real              :: a
!!    real              :: angl
!!    real              :: angle(19)
!!    real              :: bang
!!    real              :: beta
!!    real              :: r(19)
!!    real              :: theta
!!    real              :: x
!!    real              :: xx
!!    real              :: xa,ya, xb,yb, xc,yc, xd,yd
!!    real              :: xar(8)= [ 1.00, 2.00, 3.00, 4.00, 5.00, 6.00       , 0.0, 0.0 ]
!!    real              :: yar(8)= [ 250.0, 110.0, 500.0, 900.0, 200.0, 140.0 , 0.0, 1.0 ]
!!       call plots(0.0,10.0,0.0,10.0)
!!    ! DRAW FRAME
!!       call plot(7.0,0.0,2)
!!       call plot(7.0,9.0,2)
!!       call plot(0.0,9.0,2)
!!       call plot(0.0,0.0,2)
!!    ! DRAW COMMENTS
!!       ibcd='USING SCALG, LGAXS, AND LGLIN SUBROUTINES'
!!       call symbol(0.7,8.25,0.14,ibcd,inteq,0.0,42)
!!       ibcd='USING POLAR SUBROUTINE'
!!       call symbol(0.7,3.80,0.14,ibcd,inteq,0.0,22)
!!    ! AXIS IS DRAWN
!!       ibcd='ALTITUDE'
!!       call axis(1.0,4.75,ibcd,-8,5.0,0.0,0.0,25.0)
!!       call scalg(yar,3.0,6,1)
!!       ibcd='TEMPERATURE'
!!       call lgaxs(1.0,4.75,ibcd,11,3.0,90.0,yar(7),yar(8))
!!       call scale(xar,5.0,6,1)
!!       call plot(1.0,4.75,-3)
!!       call lglin(xar,yar,6,1,0,1,1)
!!       call plot(-1.0,-4.75,-3)
!!    ! POLAR SUBROUTINE IS USED
!!       x=0.0
!!       do k=1,19
!!          theta=x*0.0174533
!!          r(k)=2.0*(1.0-cos(theta))
!!          angle(k)=theta
!!          x=x+10.0
!!       enddo
!!       call plot(5.0,0.75,-3)
!!       call polar(r,angle,19,1,0,1,0.0,1.0)
!!       angl =30.0
!!       a=1.0
!!       do  i=1,5
!!          theta=angl *0.0174533
!!          xa=cos(theta)
!!          ya=sin(theta)
!!          call plot(xa,ya,3)
!!          xb=1.1*xa
!!          yb=1.1*ya
!!          call plot(xb,yb,2)
!!          xc=xb+0.05*xa
!!          yc=yb+0.05*ya
!!          if((i-3).gt.0)then
!!             a=1.5
!!          endif
!!          beta=1.570797-theta
!!          xd=xc-0.105*a*cos(beta)
!!          yd=yc+0.105*a*sin(beta)
!!          bang=270.0+angl
!!          call number(xd,yd,0.105,angl, bang,-1)
!!          angl =angl +30.0
!!       enddo
!!       xx=0.0
!!       do i=1,19
!!          angle(i) = xx*0.0174533
!!          r(i) = 1.0
!!          xx=xx+10.0
!!       enddo
!!       call polar(r,angle,19,1,0,1,0.0,1.0)
!!       call plot(-5.0,-0.75,-3)
!!    ! AXIS IS DRAWN
!!       ibcd=''
!!       call axis(1.0,0.75,ibcd,-1,4.0,0.0,4.0,-1.0)
!!       call axis(5.0,0.75,ibcd,-1,1.0,0.0,0.0,1.0)
!!       ibcd='RADIUS=2*(1-COS(ANGLE))'
!!       call symbol(3.75,3.5,0.09,ibcd,inteq,0.0,23)
!!       call plot(11.0,0.0,999)
!!    end program demo_lglin
!!
!!##LICENSE
!!    Public Domain
!     subroutine lgline(xarra,yarra,nv,k,jtype,nsy,lgtyp)

!     SUBROUTINE LGLIN (XARRAY,YARRAY,NV,K,JTYPE,NSY,LOGTYP)
!     XARRAY  ARRAY CONTAINING VALUES TO BE PLOTTED AS THE ABSCISSAS,
!             EITHER LOGARITHMIC OR LINEAR.
!     YARRAY  ARRAY CONTAINING VALUES TO BE PLOTTED AS THE ORDINATES,
!             EITHER LOGARITHMIC OR LINEAR.
!             XARRAY AND YARRAY MUST CONTAIN SCALING FACTORS. MINIMUM
!             VALUES MUST BE LOCATED AT (NV*K+1) AND DELTA VALUES MUST
!             BE LOCATED AT (NV*K+K+1).
!     NV      NUMBER OF DATA POINTS TO BE PLOTTED.
!     K       REPEAT CYCLE OF LOCATION OF VALUES IN ARRAYS.
!     JTYPE   CONTROLS TYPE OF LINE PRODUCED.
!             JTYPE=0 PRODUCES A LINE PLOT ONLY.
!             JTYPE GREATER THAN ZERO PRODUCES A LINE PLOT WITH A
!             SYMBOL AT EVERY JTYPE-TH POINT THRU ARRAYS.
!             JTYPE LESS THAN ZERO PRODUCES ONLY SYMBOLS (NOT CONNECTED)
!             AT EVERY JTYPE-TH POINT THRU ARRAYS.
!     NSY     INTEGER SPECIFYING CENTERED SYMBOL OF SYMBOL TABLE TO
!             BE DRAWN AT EVERY JTYPE-TH DATA POINT.
!     LOGTYP  INTEGER SPECIFYING MODE OF PLOT, EITHER LOG-LOG
!             OR SEMI-LOG.
!             LOGTYP=0 PRODUCES A LOG-LOG PLOT.
!             LOGTYP=1 PRODUCES A SEMI-LOG PLOT LINEAR IN X.
!             LOGTYP=-1 PRODUCES A SEMI-LOG PLOT LINEAR IN Y.
subroutine lglin (xarra,yarra,nv,k,jtype,nsy,lgtyp)

! ident_13="@(#) m_calcomp lglin(3f) draw polyline in log-log or semi-log mode"

!  THE VARIABLE IBCD HAS BEEN DECLARED AS CHARACTER TYPE FOR USE IN
!  THE CALL TO THE 'SYMBOL' ROUTINE. SINCE 'IBCD' HAS NO MEANING IN
!  THE PARTICULAR CALL TO 'SYMBOL' FOUND IN THIS SUBROUTINE, 'IBCD'
!  IS NOT INITIALIZED.
!
!  THE VARIABLE 'INTEQ' HAS BEEN CHANGED FROM AN ARRAY DIMENSIONED 2
!  (IN THE CDC VERSION OF THE CALCOMP LIBRARY) TO A SIMPLE INTEGER
!  VARIABLE AS THE CRAY VERSION OF THE 'SYMBOL' ROUTINE EXPECTS A
!  SIMPLE INTEGER VALUE FOR THIS ARGUMENT.
!
real    :: xarra(*)
real    :: yarra(*)
integer :: nv
integer :: k
integer :: jtype
integer :: nsy
integer :: lgtyp
character(len=8) :: ibcd
real    :: df
real    :: dl
real    :: dx
real    :: dy
real    :: eto10
integer :: i
integer :: ic
integer :: ica
integer :: inteq
integer :: is
integer :: isa
integer :: kk
integer :: ldx
integer :: lmn
integer :: lsw
integer :: na
integer :: nf
integer :: nl
integer :: nt
real    :: x1
real    :: x2
real    :: xmin
real    :: xn
real    :: y1
real    :: y2
real    :: ymin
real    :: yn
real    :: z
      inteq = nsy
      eto10 = 0.4342945
      lmn = nv*k+1
      ldx = lmn+k
      nl = lmn-k
!  STORE SCALING FACTORS.
      xmin = xarra (lmn)
      dx = xarra (ldx)
      ymin = yarra (lmn)
      dy = yarra (ldx)
!  STORE COORDINATES OF ENDS OF LINE.
      x1 = xarra (1)
      x2 = xarra (nl)
      y1 = yarra (1)
      y2 = yarra (nl)
!  CONVERT LINEAR TO LOG, DEPENDING ON VALUE OF LOGTYP.
      if(lgtyp) 10,10,20
10    xmin = eto10*log(xmin)
      x1 = eto10*log(x1)
      x2 = eto10*log(x2)
20    if(lgtyp) 40,30,30
30    ymin = eto10*log(ymin)
      y1 = eto10*log(y1)
      y2 = eto10*log(y2)
!  LOCATE PEN.
40    call where(xn,yn,z)
!  FIND MAXIMUM OF COORDINATES OF END POINTS OF LINE.
      df = max(abs((x1-xmin)/dx-xn),abs((y1-ymin)/dy-yn))
      dl = max(abs((x2-xmin)/dx-xn),abs((y2-ymin)/dy-yn))
!  SET CONSTANTS FOR POINT PLOT, LINE PLOT, OR LINE AND SYMBOL PLOT.
!        IC  PEN UP-DOWN FOR PLOT.
!        IS  PEN UP-DOWN FOR SYMBOL.
!        NA  STEP FROM 1 TO NT.
!        NT  WHEN NA=NT, USE SYMBOL.
!        NF  SUBSCRIPT OF ARRAY VALUE TO BE PLOTTED.
!        KK  STEP NF FORWARD OR BACKWARD THRU ARRAYS.
!        ICA,ISA  VALUES OF IC AND IS AFTER FIRST POINT PLOTTED.
!        LSW  FLAG TO SKIP PLOT CALL FOR POINT PLOT ONLY.
      ic = 3
      is = -1
      nt = abs(jtype)
      if(jtype) 60,50,60
50    nt = 1
60    if(df-dl) 80,80,70
70    nf = nl
      na = ((nv-1)/nt)*nt+nt-(nv-1)
      kk = -k
      goto 90
80    nf = 1
      na = nt
      kk = k
90    if(jtype) 100,110,120
100   ica = 3
      isa = -1
      lsw = 1
      goto 130
110   na = ldx
120   ica = 2
      isa = -2
      lsw = 0
!  BEGIN DO-LOOP FOR PLOTTING.
130   do 230 i=1,nv
!  STORE COORDINATES.
         xn = xarra (nf)
         yn = yarra (nf)
!  CONVERT LINEAR TO LOG DEPENDING ON VALUE OF LOGTYP.
         if(lgtyp)140,140,150
140      xn = eto10*log(xn)
150      if(lgtyp) 170,160,160
160      yn = eto10*log(yn)
!  CALCULATE PAGE COORDINATES OF POINT.
170      xn = (xn-xmin)/dx
         yn = (yn-ymin)/dy
!  TEST FOR SYMBOL OR POSSIBLE PLOT CALL.
         if(na-nt) 180,190,200
!  TEST FOR PLOT OR NO-PLOT.
180      if(lsw) 210,200,210
!
!  THE FOLLOWING CALL TO THE 'SYMBOL' ROUTINE HAS BEEN MODIFIED TO
!  CONVERT THIS SUBROUTINE TO THE CRAY. ON THE CRAY, 'SYMBOL' HAS
!  7 ARGUMENTS; ON THE CDC, 'SYMBOL' HAS 6 ARGUMENTS. THE ADDITIONAL
!  ARGUMENT IN THIS CALL IS 'IBCD'.
!
190      call symbol(xn,yn,0.08,ibcd,inteq,0.0,is)
         na = 1
         goto 220
200      call plot(xn,yn,ic)
!  RESET CONSTANTS.
210      na = na+1
220      nf = nf+kk
         is = isa
230   ic = ica
end subroutine lglin
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    polar(3f) - [M_calcomp:scientific] plot radial values versus angular variables (as polar coordinates)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine polar(radar,angar,npts,inc,+-lintyp,inteq,rmax,dr)
!!
!!##DESCRIPTION
!!
!!  POLAR is a FORTRAN subroutine which scales and plots a radial variable of any
!!  magnitude against an angular variable (angle in radians) as polar
!!  coordinates. POLAR produces either a line plot (with lines connecting data
!!  points) or a point plot, centered at (0.0).
!!
!!##OPTIONS
!!
!!     RADAR        is the name of the array containing the radial values.
!!
!!     ANGAR        is the name of the array containing the angular values
!!                  (radians).
!!
!!     NPTS         is the number of data points to be plotted.
!!
!!     INC          is the increment between elements in the array. INC is
!!                  greater than 1 if the values to be plotted are in a mixed
!!                  array. Every INCth point in the array is plotted.
!!                  (Normally INC = 1). RADAR and ANGAR must be dimensioned
!!                  to be INC*NPTS words long.
!!
!!     LINTYP       is used to control the type of graph produced:
!!
!!                  If LINTYP = 0 a line is plotted between successive data
!!                  points. No symbols are plotted.
!!
!!                  If LINTYP = 1 a line plot is produced, with a symbol at
!!                  each data point.
!!
!!                  If LINTYP = n a line plot is produced, with symbol at
!!                  every nth data point.
!!
!!                  If LINTYP = -n, connecting lines are not plotted between
!!                  data points; a symbol appears at every nth data point.
!!
!!     INTEQ        is the integer equivalent used to specify the symbol to
!!                  be plotted at the data point. (Refer to the description
!!                  of SYMBOL for possible values of INTEQ).
!!
!!     RMAX         is the maximum radius for the plotting area, in page
!!                  inches. If RMAX<=(0), DR is used as scale factor.
!!
!!     DR           is the scale factor. If RMAX>0, DR is computed by the
!!                  POLAR subroutine; if RMAX<=(0), DR must contain the scale
!!                  factor. DR is expressed in units of data per page inch.
!!
!!  COMMENTS
!!
!!  Angles are measured in radians counterclockwise around (0.0,0.0), with zero
!!  being in the +X direction.
!!
!!  Radial values are measured from (0.0,0.0), with negative values being plotted
!!  radially opposite from positive values.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_polar
!!    use M_calcomp
!!    implicit none
!!    ! based on concepts of CALIFORNIA COMPUTER PRODUCTS, 1968
!!    character(len=50) :: ibcd
!!    integer           :: i, k
!!    integer           :: inteq
!!    real              :: a
!!    real              :: angl
!!    real              :: angle(19)
!!    real              :: bang
!!    real              :: beta
!!    real              :: r(19)
!!    real              :: theta
!!    real              :: x
!!    real              :: xx
!!    real              :: xa,ya, xb,yb, xc,yc, xd,yd
!!    real              :: xar(8)= [ 1.00, 2.00, 3.00, 4.00, 5.00, 6.00       , 0.0, 0.0 ]
!!    real              :: yar(8)= [ 250.0, 110.0, 500.0, 900.0, 200.0, 140.0 , 0.0, 1.0 ]
!!       call plots(0.0,10.0,0.0,10.0)
!!    ! DRAW FRAME
!!       call plot(7.0,0.0,2)
!!       call plot(7.0,9.0,2)
!!       call plot(0.0,9.0,2)
!!       call plot(0.0,0.0,2)
!!    ! DRAW COMMENTS
!!       ibcd='USING SCALG, LGAXS, AND LGLIN SUBROUTINES'
!!       call symbol(0.7,8.25,0.14,ibcd,inteq,0.0,42)
!!       ibcd='USING POLAR SUBROUTINE'
!!       call symbol(0.7,3.80,0.14,ibcd,inteq,0.0,22)
!!    ! AXIS IS DRAWN
!!       ibcd='ALTITUDE'
!!       call axis(1.0,4.75,ibcd,-8,5.0,0.0,0.0,25.0)
!!       call scalg(yar,3.0,6,1)
!!       ibcd='TEMPERATURE'
!!       call lgaxs(1.0,4.75,ibcd,11,3.0,90.0,yar(7),yar(8))
!!       call scale(xar,5.0,6,1)
!!       call plot(1.0,4.75,-3)
!!       call lglin(xar,yar,6,1,0,1,1)
!!       call plot(-1.0,-4.75,-3)
!!    ! POLAR SUBROUTINE IS USED
!!       x=0.0
!!       do k=1,19
!!          theta=x*0.0174533
!!          r(k)=2.0*(1.0-cos(theta))
!!          angle(k)=theta
!!          x=x+10.0
!!       enddo
!!       call plot(5.0,0.75,-3)
!!       call polar(r,angle,19,1,0,1,0.0,1.0)
!!       angl =30.0
!!       a=1.0
!!       do  i=1,5
!!          theta=angl *0.0174533
!!          xa=cos(theta)
!!          ya=sin(theta)
!!          call plot(xa,ya,3)
!!          xb=1.1*xa
!!          yb=1.1*ya
!!          call plot(xb,yb,2)
!!          xc=xb+0.05*xa
!!          yc=yb+0.05*ya
!!          if((i-3).gt.0)then
!!             a=1.5
!!          endif
!!          beta=1.570797-theta
!!          xd=xc-0.105*a*cos(beta)
!!          yd=yc+0.105*a*sin(beta)
!!          bang=270.0+angl
!!          call number(xd,yd,0.105,angl, bang,-1)
!!          angl =angl +30.0
!!       enddo
!!       xx=0.0
!!       do i=1,19
!!          angle(i) = xx*0.0174533
!!          r(i) = 1.0
!!          xx=xx+10.0
!!       enddo
!!       call polar(r,angle,19,1,0,1,0.0,1.0)
!!       call plot(-5.0,-0.75,-3)
!!    ! AXIS IS DRAWN
!!       ibcd=''
!!       call axis(1.0,0.75,ibcd,-1,4.0,0.0,4.0,-1.0)
!!       call axis(5.0,0.75,ibcd,-1,1.0,0.0,0.0,1.0)
!!       ibcd='RADIUS=2*(1-COS(ANGLE))'
!!       call symbol(3.75,3.5,0.09,ibcd,inteq,0.0,23)
!!       call plot(11.0,0.0,999)
!!    end program demo_polar
!!
!!##LICENSE
!!    Public Domain
subroutine polar(radar,angar,npts,inc,ltyp,inteq,rmax,dr)

! ident_14="@(#) M_calcomp polar(3f) plot radial values versus angular variables (as polar coordinates)"

!
!       RARRAY IS THE ARRAY CONTAINING THE RADIAL VALUES OF THE POINTS
!               TO BE PLOTTED, IN INCHES.
!
!       AARRAY  IS THE ARRAY CONTAINING THE ANGULAR VALUES OF THE POINTS
!               TO BE PLOTTED, IN RADIANS.
!
!       NPTS    IS THE NUMBER OF DATA POINTS TO BE PLOTTED.
!
!       INC     IS THE INCREMENT BETWEEN ELEMENTS IN THE ARRAY. INC IS
!               GREATER THAN 1 IF THE VALUES TO BE PLOTTED ARE IN A
!               MIXED ARRAY. NORMALLY INC=1 .
!
!       LINTYP  IS THE TYPE OF GRAPH TO BE PLOTTED. IF LINTYP EQUALS
!                 0   A LINE IS PLOTTED BETWEEN SUCCESSIVE DATA POINTS.
!                 1   A LINE PLOT IS PRODUCED, WITH A SYMBOL AT EACH
!                     DATA POINT.
!                 2   A LINE PLOT IS PRODUCED, WITH A SYMBOL AT EVERY
!                     SECOND DATA POINT.
!                 N   A LINE PLOT IS PRODUCED, WITH A SYMBOL AT EVERY
!                     NTH DATA POINT.
!                 -N  CONNECTING LINES ARE NOT PLOTTED? A SYMBOL APPEARS
!                     AT EVERY NTH DATA POINT.
!
!       INTEQ   IS THE INTEGER EQUIVALENT OF THE SYMBOL TO BE PLOTTED
!               AT EVERY NTH DATA POINT.
!
!       RMAX    IS THE MAXIMUM RADIUS FOR THE PLOTTING AREA. IF RMAX IS
!                 POSITIVE,  POLAR PERFORMS THE SCALING, AND RETURNS
!                            THE SCALE FACTOR IN DR.
!                 NEGATIVE,  DR IS USED AS THE SCALE FACTOR.
!
!       DR      IS THE SCALE FACTOR WHEN RMAX IS NEGATIVE.
!                 DR RETURNS THE SCALE FACTOR WHEN RMAX IS POSITIVE.
!
      DIMENSION RADAR(*), ANGAR(*), TEMP(4)
!
!  THE VARIABLE IBCD HAS BEEN DECLARED AS CHARACTER TYPE FOR USE IN
!  THE CALL TO THE 'SYMBOL' ROUTINE. SINCE 'IBCD' HAS NO MEANING IN
!  THE PARTICULAR CALL TO 'SYMBOL' FOUND IN THIS SUBROUTINE, 'IBCD'
!  IS NOT INITIALIZED.
!
!  THE VARIABLE 'INTEQ' HAS BEEN CHANGED FROM AN ARRAY DIMENSIONED 2
!  (IN THE CDC VERSION OF THE CALCOMP LIBRARY) TO A SIMPLE INTEGER
!  VARIABLE AS THE CRAY VERSION OF THE 'SYMBOL' ROUTINE EXPECTS A
!  SIMPLE INTEGER VALUE FOR THIS ARGUMENT.
!
character(len=8) :: ibcd
real    :: angar
real    :: df
real    :: dl
real    :: dr
integer :: i
integer :: ic
integer :: ica
integer :: inc
integer :: ind1
integer :: ind2
integer :: inte
integer :: inteq
integer :: is
integer :: isa
integer :: k
integer :: kk
integer :: lsw
integer :: ltyp
integer :: na
integer :: nf
integer :: nl
integer :: npts
integer :: nt
real    :: r1
real    :: radar
real    :: rmax
real    :: rmaxm
real    :: rminm
real    :: rn
real    :: t
real    :: temp
real    :: th1
real    :: thn
      inte = inteq
      k = inc
      ind1 = npts*k + 1
      ind2 = ind1 + k
      nl = ind1 - k
      if(rmax) 80,80,10
10    rmaxm = 0.0
      rminm = 0.0
      do 50 i = 1,nl,k
         t = radar(i)
         if(t-rmaxm) 30,50,20
20       rmaxm = t
         goto 50
30       if(rminm-t) 50,50,40
40       rminm = t
50    continue
      if(abs(rmaxm)-abs(rminm)) 60,70,70
60    rmaxm = -rminm
70    temp(1) = 0.0
      temp(2) = rmaxm
      call scale(temp,rmax,2,1)
      dr = temp(4)
80    call where(rn,thn,r1)
      t = radar(1)/dr
      th1 = angar(1)
      r1 = t*cos(th1)
      th1 = t*sin(th1)
      df = abs(r1-rn)
      r1 = abs(th1-thn)
      if(df-r1) 90,100,100
90    df = r1
100   t = radar(nl)/dr
      th1 = angar(nl)
      r1 = t*cos(th1)
      th1 = t*sin(th1)
      dl = abs(r1-rn)
      r1 = abs(th1-thn)
      if(dl-r1) 110,120,120
110   dl = r1
120   ic = 3
      is = -1
      nt = abs(ltyp)
      if(nt) 140,130,140
130   nt = 1
140   if(df-dl) 160,160,150
150   nf = nl
      na = ((npts-1)/nt)*nt + nt - npts + 1
      kk = -k
      goto 170
160   nf = 1
      na = nt
      kk = k
170   if(ltyp) 180,190,185
180   ica = 3
      isa = -1
      lsw = 1
      goto 210
185   ic = 2
      goto 200
190   na = ind2
200   ica = 2
      isa = -2
      lsw = 0
210   do 260 i = 1,npts
         th1 = angar(nf)
         t = radar(nf)/dr
         rn = t*cos(th1)
         thn = t*sin(th1)
         if(na-nt) 220,230,240
220      if(lsw) 250,240,250
!
!  THE FOLLOWING CALL TO THE 'SYMBOL' ROUTINE HAS BEEN MODIFIED TO
!  CONVERT THIS SUBROUTINE TO THE CRAY. ON THE CRAY, 'SYMBOL' HAS
!  7 ARGUMENTS; ON THE CDC, 'SYMBOL' HAS 6 ARGUMENTS. THE ADDITIONAL
!  ARGUMENT IN THIS CALL IS 'IBCD'.
!
230      call symbol(rn, thn, 0.08,ibcd, inte, 0.0, is)
         na = 1
         is = isa
         goto 260
240      call plot(rn,thn,ic)
         ic = ica
250      na = na + 1
260   nf = nf + kk
end subroutine polar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine reflx  (vx1,vy1,vx2,vy2)

! EARLIER VERSION OF THIS SUBROUTINE WAS
!     SUBROUTINE REFLEX (VX1,VY1,VX2,VY2)
real :: ds
real :: ps
real :: ss
real :: temp
real :: vx1
real :: vx2
real :: vy1
real :: vy2
   ps=vy1*vy1
   ds=vx1*vx1
   ss = ds+ps+0.00001
   ds=ds-ps
   ps=2.0*vx1*vy1
   temp=(ps*vy2+vx2*ds)/ss
   vy2=(ps*vx2-vy2*ds)/ss
   vx2=temp
end subroutine reflx
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    scalg(3f) - [M_calcomp:scientific] determine scale factors for a logarithmic scale
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!        subroutine scalg(array,axlen,npts,inc)
!!
!!##DESCRIPTION
!!
!!  SCALG is a FORTRAN subroutine used to determine scale factors of a
!!  data array to be plotted on a logarithmic scale. The scale factors
!!  are those used by subroutines LGLIN and LGAXS. SCALG is similar
!!  in operation to SCALE.
!!
!!##OPTIONS
!!
!!     ARRAY    is the array containing the data to be scaled. The
!!              FORTRAN DIMENSION statement must specify at least two
!!              elements more than the number of values being scaled, to
!!              allow room for SCALG to store the computed starting value
!!              and scaling factor at the end of the array.
!!
!!     AXLEN    is the maximum length, in inches, over which the data is
!!              to be plotted.
!!
!!     NPTS     is the number of values in ARRAY to be scaled.
!!
!!     INC      is the increment between elements of the array to be
!!              plotted. INC is greater than 1 if the values to be
!!              plotted are in a mixed or multi-dimensioned array.
!!              (Normally INC = 1).
!!
!!   COMMENTS
!!
!!  The array must be dimensioned with at least NPTS + 2 elements.
!!  The adjusted minimum value (FIRSTV) and the delta value (DELTAV) are
!!  stored by SCALG in the data array.
!!
!!  The adjusted minimum is stored in ARRAY (NPTS*INC + 1), and the
!!  adjusted delta (log cycles per inch) is in ARRAY (NPTS*INC + INC + 1) .
!!  Therefore, ARRAY must be dimensioned to be at least NPTS*INC+INC+1 .
!!
!!  Every INCth element of the array ARRAY, beginning with the first,
!!  is scanned to find the minimum and maximum values of the array.
!!  Next, the greatest value of 10**n (integer n) less than or equal
!!  to the minimum value is found and then stored in ARRAY (NPTS*INC + 1).
!!  Finally, the smallest value of 10**m (integer m) greater than or
!!  equal to the maximum value is established.
!!
!!  The delta value is the difference between the minimum and maximum
!!  powers of ten, divided by AXLEN, yielding log cycles per inch.
!!  The delta value is stored in ARRAY (NPTS*INC + INC + 1).
!!
!!##EXAMPLES
!!
!!
!!  Various examples:
!!
!!     A.  For the following array of values
!!
!!         ARRAY(1)  =  1500.0
!!         ARRAY(2)  =  3000.0
!!         ARRAY(3)  =  2500.0
!!         ARRAY(4)  =   300.0
!!
!!         and the following argument values
!!
!!         AXLEN     =  1.0
!!         NPTS      =  4
!!         INC       =  1
!!
!!         the adjusted minimum (FIRSTV) and delta value (DELTAV) stored by
!!         SCALG are:
!!
!!         FIRSTV:  ARRAY(5)   =  100.0
!!         DELTAV:  ARRAY(6)   =    2.0
!!
!!     B.  If the value of AXLEN in Example A were changed to 4.0, the resultant
!!         values stored would be:
!!
!!         FIRSTV:  ARRAY(5)   =  100.0
!!         DELTAV:  ARRAY(6)   =    0.5
!!
!!     C.  For the following array of values
!!
!!         ARRAY(1)  =    1.2*
!!         ARRAY(2)  =  100.0
!!         ARRAY(3)  =    2.3*
!!         ARRAY(4)  =   88.0
!!         ARRAY(5)  =    1.8*
!!         ARRAY(6)  =    0.0
!!         ARRAY(7)  =    0.8*
!!         ARRAY(8)  =   10.0
!!         ARRAY(9)  =    0.7*
!!         ARRAY(10) =   10.0
!!
!!         and the following argument values:
!!
!!         AXLEN     =  10.0
!!         NPTS      =  5
!!         INC       =  2
!!
!!         the adjusted minimum (FIRSTV) and delta value (DELTAV) are determined
!!         from the value of the asterisked items (1, 3, 5, 7, and 9) in the
!!         above array. The computed values are also stored two subscript
!!         elements apart.
!!
!!         FIRSTV:  ARRAY(11)  =  0.1
!!         DELTAV:  ARRAY(13)  =  0.2
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_scalg
!!    use M_calcomp
!!    implicit none
!!    ! based on concepts of CALIFORNIA COMPUTER PRODUCTS, 1968
!!    character(len=50) :: ibcd
!!    integer           :: i, k
!!    integer           :: inteq
!!    real              :: a
!!    real              :: angl
!!    real              :: angle(19)
!!    real              :: bang
!!    real              :: beta
!!    real              :: r(19)
!!    real              :: theta
!!    real              :: x
!!    real              :: xx
!!    real              :: xa,ya, xb,yb, xc,yc, xd,yd
!!    real              :: xar(8)= [ 1.00, 2.00, 3.00, 4.00, 5.00, 6.00       , 0.0, 0.0 ]
!!    real              :: yar(8)= [ 250.0, 110.0, 500.0, 900.0, 200.0, 140.0 , 0.0, 1.0 ]
!!       call plots(0.0,10.0,0.0,10.0)
!!    ! DRAW FRAME
!!       call plot(7.0,0.0,2)
!!       call plot(7.0,9.0,2)
!!       call plot(0.0,9.0,2)
!!       call plot(0.0,0.0,2)
!!    ! DRAW COMMENTS
!!       ibcd='USING SCALG, LGAXS, AND LGLIN SUBROUTINES'
!!       call symbol(0.7,8.25,0.14,ibcd,inteq,0.0,42)
!!       ibcd='USING POLAR SUBROUTINE'
!!       call symbol(0.7,3.80,0.14,ibcd,inteq,0.0,22)
!!    ! AXIS IS DRAWN
!!       ibcd='ALTITUDE'
!!       call axis(1.0,4.75,ibcd,-8,5.0,0.0,0.0,25.0)
!!       call scalg(yar,3.0,6,1)
!!       ibcd='TEMPERATURE'
!!       call lgaxs(1.0,4.75,ibcd,11,3.0,90.0,yar(7),yar(8))
!!       call scale(xar,5.0,6,1)
!!       call plot(1.0,4.75,-3)
!!       call lglin(xar,yar,6,1,0,1,1)
!!       call plot(-1.0,-4.75,-3)
!!    ! POLAR SUBROUTINE IS USED
!!       x=0.0
!!       do k=1,19
!!          theta=x*0.0174533
!!          r(k)=2.0*(1.0-cos(theta))
!!          angle(k)=theta
!!          x=x+10.0
!!       enddo
!!       call plot(5.0,0.75,-3)
!!       call polar(r,angle,19,1,0,1,0.0,1.0)
!!       angl =30.0
!!       a=1.0
!!       do  i=1,5
!!          theta=angl *0.0174533
!!          xa=cos(theta)
!!          ya=sin(theta)
!!          call plot(xa,ya,3)
!!          xb=1.1*xa
!!          yb=1.1*ya
!!          call plot(xb,yb,2)
!!          xc=xb+0.05*xa
!!          yc=yb+0.05*ya
!!          if((i-3).gt.0)then
!!             a=1.5
!!          endif
!!          beta=1.570797-theta
!!          xd=xc-0.105*a*cos(beta)
!!          yd=yc+0.105*a*sin(beta)
!!          bang=270.0+angl
!!          call number(xd,yd,0.105,angl, bang,-1)
!!          angl =angl +30.0
!!       enddo
!!       xx=0.0
!!       do i=1,19
!!          angle(i) = xx*0.0174533
!!          r(i) = 1.0
!!          xx=xx+10.0
!!       enddo
!!       call polar(r,angle,19,1,0,1,0.0,1.0)
!!       call plot(-5.0,-0.75,-3)
!!    ! AXIS IS DRAWN
!!       ibcd=''
!!       call axis(1.0,0.75,ibcd,-1,4.0,0.0,4.0,-1.0)
!!       call axis(5.0,0.75,ibcd,-1,1.0,0.0,0.0,1.0)
!!       ibcd='RADIUS=2*(1-COS(ANGLE))'
!!       call symbol(3.75,3.5,0.09,ibcd,inteq,0.0,23)
!!       call plot(11.0,0.0,999)
!!    end program demo_scalg
!!##LICENSE
!!    Public Domain
subroutine scalg(array,axlen,npts,inc)

! ident_15="@(#) M_calcomp scalg(3f) determine scale factors for a logarithmic scale"

!  IN THE CALCOMP CORP. ORIGINAL, HAS A BUG: IF ARRAY CONTAINED
!  ANY VALUES .LT. 1.0, THE LOWER BOUND OF THE AXIS WOULD BE TOO
!  HIGH BY ONE "CYCLE" (FACTOR OR 10), CAUSING DATA TO GO OUTSIDE
!  THE INTENDED AREA OF THE PLOT OR TO BE LOST.
!
integer npts, inc, j, lowint
real array(inc,*), axlen, xmin, xmax, higint, x
!
   lowint(x) =  nint(x - 0.4999)
   higint(x) = anint(x + 0.4999)
!
   xmin = array(1,1)
   xmax = xmin
   do j = 2, npts
      xmin = min(xmin, array(1,j))
      xmax = max(xmax, array(1,j))
   enddo
   j = lowint(log10(xmin))
   array(1,npts+1) = 10.0 ** j
   xmax = higint(log10(xmax))
   array(1,npts+2) = (xmax - j) / axlen

end subroutine scalg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    smoot(3f) - [M_calcomp:scientific] draw a polyline using modified spline-fitting technique
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine smoot(xpage,ypage,ipen)
!!
!!##DESCRIPTION
!!
!!  SMOOT is a FORTRAN subroutine which draws a smooth curve through
!!  a set of data points. It accomplishes this by using a modified
!!  spline-fitting technique. The subroutine receives a single coordinate
!!  pair on each call and accumulates the points until it has received a
!!  sufficient number to compute a pair of cubic parametric equations
!!  for a smooth curve. This accumulation method requires the user to
!!  specify an initial and a terminal call to the subroutine.
!!
!!  The SMOOT subroutine operates in either of two modes: Smooth Mode
!!  and Plot Mode.
!!
!!##OPTIONS
!!
!!     XPAGE,YPAGE  are the coordinates, in inches, of a single point through
!!                  which the pen moves.
!!
!!     IPEN         determines the mode and action of the SMOOT subroutine.
!!
!!##DESCRIPTION
!!
!!  The first call to SMOOT must use an IPEN value of 0 or -1 to put SMOOT in the
!!  Smooth Mode.
!!
!!     if IPEN = 0, XPAGE,YPAGE define the initial point (P(1))
!!     on the curve. The smoothing function ends at the last
!!     point (P(n)). An open curve is produced.
!!
!!     if IPEN = -1, XPAGE,YPAGE are used to define the initial
!!     point (P(1)) on the curve. The smoothing function
!!     continues from the last point (P(n)) back to the initial
!!     point (P(1)). A closed curve is produced.
!!
!!  SMOOTH MODE:
!!
!!   When SMOOT is in the Smooth Mode, IPEN performs the following functions:
!!
!!    IPEN = -2       XPAGE,YPAGE are used to define points P(2), P(3),...,
!!                    P(N-1), and a smoothed curve is drawn through the points
!!                    on the curve.
!!
!!    IPEN = -3       XPAGE,YPAGE are used to define points P(2), P(3),
!!                    ...,P(N-1), and the pen, in the up position, is moved
!!                    through these points. The smoothing function is
!!                    maintained.
!!
!!    IPEN = 2 or 3   The call is treated as a normal CALL PLOT
!!                    (XPAGE,YPAGE,IPEN), and the point is not considered a
!!                    point on the curve. The point of departure from the
!!                    curve is the next-to-last point received by SMOOT, not
!!                    the last point.
!!
!!                    When the next call to SMOOT with IPEN = -2 or -3 is
!!                    received, the pen is repositioned to the point where
!!                    it left the smooth curve.  The smooth curve is then
!!                    continued as though the calls with IPEN = 2 or 3 had
!!                    not occurred.
!!
!!    IPEN <=(-24)   is used for the terminal call while SMOOT is in the
!!                   Smooth Mode. XPAGE,YPAGE represent P(N). The curve is
!!                   finished, and the subroutine returns to the Plot Mode.
!!
!!  PLOT MODE:
!!
!!  SMOOT is in the Plot Mode after receiving a terminal call.
!!
!!      IF IPEN = +-2 or +-3, the call is treated as a normal CALL
!!      PLOT (XPAGE,YPAGE,IPEN).
!!
!!  COMMENTS
!!
!!  When SMOOT is called while it is in the Smooth Mode, the pen is not
!!  moved until three points on an open curve or four points on a closed
!!  curve have been received. For subsequent calls to SMOOT, the actual
!!  pen position is the next-to-last point received.
!!
!!  Calls to other plotting subroutines may be intermixed with calls to
!!  SMOOT. Point-of-departure restrictions are the same as noted in the
!!  Smooth Mode description above.
!!
!!  The first call to SMOOT must be with IPEN = 0 or -1 .
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_smoot
!!    use M_calcomp
!!    implicit none
!!    ! based on concepts of CALIFORNIA COMPUTER PRODUCTS, 1968
!!    real :: xar(10)=[0.75,1.75,2.25,2.75,3.25,4.25,4.75,5.75,0.0,1.0]
!!    real :: yar(10)=[3.25,2.00,5.25,6.50,6.75,6.25,3.25,4.25,0.0,1.0]
!!    character(len=50) :: ibcd
!!    integer           :: inteq
!!       call plots(0.0,10.0,0.0,10.0)
!!    !     DRAW FRAME
!!       call plot(7.0,0.0,2)
!!       call plot(7.0,9.0,2)
!!       call plot(0.0,9.0,2)
!!       call plot(0.0,0.0,2)
!!    !     DRAW AXIS
!!       ibcd='SERVICE TIME'
!!       call axis(0.75,0.75,ibcd,-12,5.0,0.0,5.0,1.0)
!!       ibcd='FREQUENCY'
!!       call axis(0.75,0.75,ibcd, 9,7.0,90.0,0.0,100.0)
!!    !     DRAW COMMENTS
!!       ibcd='USING FLINE AND SMOOT SUBROUTINES'
!!       call symbol(0.7,8.25,0.14,ibcd,inteq,0.0,34)
!!       call plot(5.0,7.8,3)
!!       call plot(5.1,7.8,2)
!!       ibcd='SMOOT'
!!       call symbol(5.2,7.80,0.09,ibcd,inteq,0.0, 6)
!!       inteq = 1
!!       call symbol(5.0,7.60,0.10,ibcd,inteq,0.0,-1)
!!       inteq=999
!!       ibcd='FLINE'
!!       call symbol(5.2,7.60,0.09,ibcd,inteq,0.0, 5)
!!    ! SMOOTHING
!!       call smoot(0.75,3.75,0)
!!       call smoot(1.75,2.5,-2)
!!       call smoot(2.25,5.75,-2)
!!       call smoot(2.75,7.0,-2)
!!       call smoot(3.25,7.25,-2)
!!       call smoot(4.25,6.75,-2)
!!       call smoot(4.75,3.75,-2)
!!       call smoot(5.75,4.75,-24)
!!    ! FLINE IS USED
!!       call plot(0.75,3.25,3)
!!       call fline(xar, yar, -8,1,1,1)
!!       call nframe()
!!    end program demo_smoot
!!
!!##LICENSE
!!    Public Domain
subroutine smoot(xn,yn,ic)

! ident_16="@(#) M_calcomp smoot(3f) draw a polyline using modified spline-fitting technique"

!     THE SMOOTH ROUTINE SIMULATES THE 'PLOT' ROUTINE WITH A NEW 'PLOT'
!     MODE (DRAWING A SMOOTH CURVE TO THE NEW POINT). THE SMOOTH MODE
!     IS INITIALIZED WITH THE UNITS DIGIT OF IC = 0 (FOR AN OPEN CURVE)
!     OR = 1 (FOR A CLOSED CURVE).
!                     THE VALUE OF IC FOR SMOOTHING IS THE NEGATIVE OF
!     THE PEN VALUES FOR PLOTTING. THERE IS, THEREFORE, NO RE-ORIGINING
!     WHILE SMOOTHING. USING POSITIVE VALUES FOR IC WHILE SMOOTHING
!     WILL BE TREATED AS A CALL TO 'PLOT'. TO END THE CURVE AND RETURN TO
!     THE PLOT MODE LET IC BE LESS THAN -23 .
! EARLIER VERSION OF THIS SUBROUTINE WAS
!     SUBROUTINE SMOOTH (XN,YN,IC)
real    :: ax
real    :: ay
real    :: bx
real    :: by
real    :: d
real    :: d1
real    :: d2
real    :: d3
real    :: dv
integer :: i
integer :: ic
integer :: ipc
integer :: irep
integer :: isw
integer :: jc
integer :: jsw
integer :: kc
integer :: lc
integer :: mc
integer :: n
integer :: nc
real    :: pxn
real    :: pyn
real    :: sx1
real    :: sx2
real    :: sx3
real    :: sy1
real    :: sy2
real    :: sy3
real    :: t
real    :: uux1
real    :: uux2
real    :: uuy1
real    :: uuy2
real    :: ux1
real    :: ux2
real    :: uy1
real    :: uy2
real    :: vx2
real    :: vx3
real    :: vy2
real    :: vy3
real    :: x
real    :: x1
real    :: x2
real    :: x3
real    :: xn
real    :: y
real    :: y1
real    :: y2
real    :: y3
real    :: yn
      !!SAVE NC,ISW,JSW,IPC
      !!SAVE X2,Y2,D3,UX1,UY1,UX2,UY2,SX1,SY1,SX2,SY2,SX3,SY3
      data nc,isw,jsw,ipc/4*0/
      data x2,y2,d3,ux1,uy1,ux2,uy2,sx1,sy1,sx2,sy2,sx3,sy3/13*0.0/
      save
      jc=ic
      kc=jc-jc/10*10
      lc=nc-ipc
      pxn=xn
      pyn=yn
      irep=0
      if(kc)1,4,14
1     continue
      if(kc+1)2,5,4
2     continue
      if(ipc)3,3,14
3     continue
      if(jc+24)10,10,17
4     continue
      isw=-1
      goto 6
5     continue
      isw=1
6     continue
      jsw=-1
      nc=(-kc)/10*10
      x3=pxn
      y3=pyn
      mc=nc+3
9     continue
      ipc=kc
      return
10    continue
      if(ipc+1)11,13,13
11    continue
      if(isw-1)12,15,14
12    continue
      if(isw+1)14,16,14
13    continue
      kc=nc+2
      ipc=1
      call plot(x3,y3,mc)
14    continue
      call plot(pxn,pyn,kc)
      return
15    continue
      irep=2
16    continue
      irep=irep+1
      kc=1
17    continue
      if(abs(jsw)-1)14,18,14
18    continue
      x1=x2
      y1=y2
      x2=x3
      y2=y3
      x3=pxn
      y3=pyn
      if(ipc+1)20,19,19
19    continue
      vx3=x3-x2
      vy3=y3-y2
      d3 = vx3*vx3+vy3*vy3
      sx1=x2
      sx2=x3
      sy1=y2
      sy2=y3
      goto 40
20    continue
      if(jsw)21,14,23
21    continue
      if(isw)22,14,24
22    continue
      vx2=x3-x2
      vy2=y3-y2
      call reflx(vx3,vy3,vx2,vy2)
      d2=vx2*vx2+vy2*vy2
      goto 26
23    continue
      jsw=1
24    continue
      vx2=vx3
      vy2=vy3
      vx3=x3-x2
      vy3=y3-y2
25    continue
      d2=d3
      ux1=ux2
      uy1=uy2
26    continue
      d3=vx3*vx3+vy3*vy3
      ux2=d2*vx3+d3*vx2
      uy2=d2*vy3+d3*vy2
      dv = 1.0/sqrt(ux2*ux2+uy2*uy2+0.000001)
      ux2=dv*ux2
      uy2=dv*uy2
      if(isw-jsw)27,27,45
27    continue
      if(jsw)23,14,28
28    continue
      t=0.0
      call where(x,y,d)
      if(abs(x1-x)-0.01*d) 29,30,30
29    continue
      if(abs(y1-y)-0.01*d) 31,30,30
30    continue
      call plot(x1,y1,mc)
31    continue
      if(ipc+3)32,40,32
32    continue
      d=abs(ux1*vx2+uy1*vy2)
      d1=d
      uux1=d*ux1
      uuy1=d*uy1
      d=abs(ux2*vx2+uy2*vy2)
      uux2=d*ux2
      uuy2=d*uy2
      d=d+d1
      ax=uux2+uux1-vx2-vx2
      bx=vx2-uux1-ax
      ay=uuy2+uuy1-vy2-vy2
      by=vy2-uuy1-ay
      n=10.0*d+1.0
      d=1./float (n)
      do 33 i=1,n
         t=t+d
         x=((ax*t+bx)*t+uux1)*t+x1
         y=((ay*t+by)*t+uuy1)*t+y1
33    continue
      call plot(x,y,lc)
40    continue
      if(irep)9,9,41
41    continue
      irep=irep-1
      if(isw)43,14,42
42    continue
      pxn=sx1
      pyn=sy1
      sx1=sx2
      sy1=sy2
      sx2=sx3
      sy2=sy3
      goto 18
43    continue
      call reflx(vx3,vy3,vx2,vy2)
      x=vx3
      y=vy3
      vx3=vx2
      vy3=vy2
      vx2=x
      vy2=y
      x1=x2
      y1=y2
      goto 25
45    continue
      jsw=1
      sx3=x3
      sy3=y3
      goto 40
end subroutine smoot
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    axis(3f) - [M_calcomp:basic] draw linear axis with numeric scale and axis label
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine axis(xpage,ypage,ibcd,+-nchar,axlen,angle,firstv,deltav)
!!
!!##DESCRIPTION
!!
!!  Draws a linear axis with numeric scale and axis label.
!!
!!  Important: Axis labels can be 0.4 inches lower than or to the left of the
!!  axis origin (XPAGE,YPAGE). Since lines drawn whose value is negative
!!  relative to the frame's ORIGINAL origin will be clipped, XPAGE and YPAGE must
!!  be greater than 0.4 or a new origin must be specified at least 0.4 units up
!!  and to the right from the original using PLOT(0.4,.4,-3) when (XPAGE,YPAGE)
!!  is (0.0,0.0).
!!
!!  Most graphs require axis lines and scales to indicate the orientation and
!!  values of the plotted data points. The most common type of scaled axis is
!!  produced by the AXIS subroutine which draws any length line at any angle,
!!  divides the line into one-inch segments, annotates the divisions with
!!  appropriate scale values and labels the axis with a centered title. When
!!  both the X and Y axes are needed, AXIS is called separately for each one.
!!
!!##OPTIONS
!!
!!    XPAGE,YPAGE  are the coordinates, in inches, of the axis line's
!!                 starting point. The entire line should be at least
!!                 one-half inch from any side to allow space for the scale
!!                 annotation and axis title. Usually, both the X and Y
!!                 axes are joined at the origin of the graph, where XPAGE
!!                 and YPAGE equal zero, but other starting points can be
!!                 used. When using the LINE subroutine to plot data on an
!!                 axis, at least one of the coordinates must be 0, i.e.,
!!                 for an X axis, XPAGE=0, and for a Y axis, YPAGE=0 .
!!
!!    IBCD         is the title, which is centered and placed parallel to
!!                 the axis line. This parameter may be a character array
!!                 or single variable. (The data should be stored as TYPE
!!                 CHARACTER.) The characters have a fixed height of 0.14
!!                 inch (about seven characters per inch).
!!    +-NCHAR      The magnitude specifies the number of characters in the
!!                 axis title, and the sign determines on which side of the
!!                 line the scale (tick) marks and labeling information
!!                 shall be placed. Since the axis line may be drawn at any
!!                 angle, the line itself is used as a reference.
!!
!!                 If the sign is positive, all annotation appears on the
!!                 positive (counterclockwise) side of the axis. This
!!                 condition is normally desired for the Y axis.
!!
!!                 If the sign is negative, all annotation appears on the
!!                 negative (clockwise) side of the axis. This condition is
!!                 normally desired for the X axis.
!!
!!    AXLEN        is the length of the axis line, in inches.
!!
!!    ANGLE        is the angle in degrees (positive or negative), at which
!!                 the axis is drawn. The value is 0 degrees for the X-axis
!!                 and 90 degrees for the Y-axis.
!!
!!    FIRSTV       is the starting value (either minimum or maximum) which
!!                 will appear at the first tick mark on the axis. This
!!                 value may either be computed by the SCALE subroutine and
!!                 stored at subscripted location ARRAY(NPTS*INC+1), or the
!!                 value may be determined by the user and stored at any
!!                 location.
!!
!!                 This number and scale value along the axis is drawn with
!!                 two decimal places. Since the digit size is 0.105 inch
!!                 (about 10 characters per inch), and since a scale value
!!                 appears every inch, no more than six digits and a sign
!!                 should appear to the left of the decimal point.
!!
!!    DELTAV       represents the number of data units per inch of axis.
!!                 This value (increment or decrement), which is added to
!!                 FIRSTV for each succeeding one-inch division along the
!!                 axis, may either be computed by SCALE and stored beyond
!!                 FIRSTV at ARRAY(NPTS*INC+INC+1), or the value may be
!!                 determined by the user and stored at any location.
!!
!!                 In order to use a standard format of two decimal places,
!!                 the size of DELTAV is adjusted to less than 100, but not
!!                 less than 0.01. As a result, the decimal point may be
!!                 shifted left or right in the scale values as drawn, and
!!                 the axis title is then followed by "*10**n", where n is
!!                 the power-of-ten adjustment factor. (See X-axis example
!!                 in Figure _____.)
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_axis
!!    use M_calcomp
!!    character(len=28) :: ichr1
!!    character(len=26) :: ichr2
!!    character(len=10) :: lbcd1,lbcd2
!!    dimension xarray(62),yarray(62)
!!    ICHR1='PLOTTED ON A CALCOMP PLOTTER'
!!    ICHR2='USING  Y = X -0.7*X +0.1*X'
!!    LBCD1='X-ABSCISSA'
!!    LBCD2='Y-ORDINATE'
!!    call plots(0.0,10.0,0.0,10.0)
!!    ! PLOT THREE GRAPHS ILLUSTRATING SCALE, AXIS, AND LINE
!!    deltax=0.04
!!    i=2
!!    ! AXIS DRAWS LABELS AS MUCH AS 0.4 INCHES TO THE NEGATIVE OF AXIS CENTER;
!!    ! EITHER USE AN ORIGIN OFFSET OF AT LEAST THIS VALUE OR DO NOT USE AN
!!    ! ORIGIN VALUE OF LESS THAN 0.4 OR CLIPPING WILL OCCUR
!!    call plot(0.4,0.4,-3)
!!    deltax=2.0*deltax
!!    xarray(1)=deltax
!!    do j=1,60
!!       yarray(j)=xarray(j)**2-0.7*xarray(j)**3+0.1*xarray(j)**4
!!       xarray(j+1)=xarray(j)+deltax
!!    enddo
!!    call scale(xarray(1), 6.5,60,1)
!!    call scale(yarray(1),10.0,60,1)
!!    call axis(0.0,0.0,lbcd1,-10, 6.5, 0.0,xarray(61),xarray(62))
!!    call axis(0.0,0.0,lbcd2, 10,10.0,90.0,yarray(61),yarray(62))
!!    call newpen(i)
!!    call line(xarray(1),yarray(1),60,1,2*(i-2),i)
!!    call newpen(1)
!!    call symbol(1.3,10.,.14,ichr1,inteq,0.0,28)
!!    call symbol(1.3,9.7,.14,ichr2,inteq,0.0,26)
!!    call number(2.98,9.8,.1,2.0,0.,-1)
!!    call number(3.96,9.8,.1,3.0,0.,-1)
!!    call number(4.94,9.8,.1,4.0,0.,-1)
!!    !call plot(10.0,0.0,-3)
!!    call nframe()
!!    call plot(0.0,0.0,999)
!!    end program demo_axis
!!
!!##LICENSE
!!    Public Domain
subroutine axis(xpage,ypage,ibcd,nchar,axlen,angle,firstv,deltav)

! ident_17="@(#) M_calcomp axis(3f) Draw linear axis with numeric scale and axis label"

!          XPAGE,YPAGE  COORDINATES OF STARTING POINT OF AXIS, IN INCHES
!          IBCD         AXIS TITLE.
!          NCHAR        NUMBER OF CHARACTERS IN TITLE. + FOR C.C-W SIDE.
!          AXLEN        FLOATING POINT AXIS LENGTH IN INCHES.
!          ANGLE        ANGLE OF AXIS FROM THE X-DIRECTION, IN DEGREES.
!          FIRSTV       SCALE VALUE AT THE FIRST TIC MARK.
!          DELTAV       CHANGE IN SCALE BETWEEN TIC MARKS ONE INCH APART
character(len=*)  ::  ibcd
character(len=3)  ::  nbcd
real              ::  a
real              ::  adx
real              ::  angle
real              ::  axlen
real              ::  cth
real              ::  deltav
real              ::  dxb
real              ::  dyb
real              ::  ex
real              ::  firstv
integer           ::  i
integer           ::  kn
integer           ::  nchar
integer           ::  nt
integer           ::  ntic
real              ::  sth
real              ::  xn
real              ::  xpage
real              ::  xt
real              ::  xval
real              ::  yn
real              ::  ypage
real              ::  yt
real              ::  z
      nbcd ='*10'
      kn=nchar
      a=1.0
      if(kn) 1,2,2
1     a=-a
      kn=-kn
!     EX IS THE EXPONENT FOR AXIS SCALING
2     ex=0.0
      adx= abs  (deltav)
      if(adx) 3,7,3
3     if(adx- 99.0) 6,4,4
4     adx=adx/10.0
      ex=ex+1.0
      goto 3
5     adx=adx*10.0
      ex=ex-1.0
6     if(adx-0.01) 5,7,7
7     xval=firstv*10.0**(-ex)
      adx= deltav*10.0**(-ex)
      sth=angle*0.0174533
      cth=cos(sth)
      sth=sin(sth)
      dxb=-0.1
      dyb=0.15*a-0.05
      xn=xpage+dxb*cth-dyb*sth
      yn=ypage+dyb*cth+dxb*sth
      ntic=axlen+1.0
      nt=ntic/2
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      do  20  i=1,ntic
         call number(xn,yn,0.105,xval,angle,2)
         xval=xval+adx
         xn=xn+cth
         yn=yn+sth
         if(nt) 20,11,20
11       z=kn
         if(ex.ne.0)z=z+7.0
         dxb=(-.07)*z+axlen*0.5
         dyb=0.325*a-0.075
         xt=xpage+dxb*cth-dyb*sth
         yt=ypage+dyb*cth+dxb*sth
         call symbol(xt,yt,0.14,ibcd,999,angle,kn)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! PUT OUT THE SCALE MULTIPLIER LABEL (*10**EX)
         if(ex.ne.0)then
            z=kn+2
            xt=xt+z*cth*0.14
            yt=yt+z*sth*0.14
            call symbol(xt,yt,0.14, nbcd,999,angle,3)
            xt=xt+(3.0*cth-0.8*sth)*0.14
            yt=yt+(3.0*sth+0.8*cth)*0.14
            call number(xt,yt,0.07,ex,angle,-1)
         endif
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
20    nt=nt-1
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      call plot(xpage+axlen*cth,ypage+axlen*sth,3)
      dxb=(-0.07)*a*sth
      dyb=(+0.07)*a*cth
      a=ntic-1
      xn=xpage+a*cth
      yn=ypage+a*sth
      do  30  i=1,ntic
         call plot(xn,yn,2)
         call plot(xn+dxb,yn+dyb,2)
         call plot(xn,yn,2)
         xn=xn-cth
         yn=yn-sth
30    continue
end subroutine axis
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    factor(3f) - [M_calcomp:basic] rescale entire plot
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine factor(fact)
!!
!!##DESCRIPTION
!!
!!  Subroutine FACTOR enables the user to enlarge or reduce the
!!  size of the entire plot by changing the ratio of the desired plot
!!  size to the normal plot size. FACTOR is often called only once in
!!  a program, immediately after the initialization call to PLOTS,
!!  to rescale all plotting to a single specific scale.
!!
!!  Because CALCOMP inches are unit-less units in PDF files, this routine
!!  is not necessary unless inch units greater than 100 are needed (100 is
!!  maximum PDF frame size) or if the program needs to remain portable to
!!  standard true-inch CALCOMP libraries and using the actual values used
!!  in the PLOT calls would produce a very small or very large plot.
!!
!!  USERS TRYING TO PRODUCE TRUE INCHES PLEASE NOTE:
!!
!!  In this CALCOMP, all frames are scaled individually to the maximum
!!  size obtainable on the output device they are produced on. This means
!!  to keep frames scaled relative to each other, you must move to the
!!  same maximum XY value IN EACH FRAME (see routine NFRAME description)
!!  with a call to PLOTS so that each frame is the same number of
!!  unit-less units in size. An example program at the end of this manual
!!  illustrates keeping frames scaled relative to each other.
!!
!!##OPTIONS
!!
!!    FACT    is the ratio of the desired plot size to the normal plot
!!            size. For example, if FACT=2.0, all subsequent pen
!!            movements will be twice their normal size. When FACT is
!!            reset to 1.0, all plotting returns to normal size.
!!            During the debugging of a plotting application program,
!!            plotting time can be saved by reducing the size of the
!!            entire plot output on certain devices such as pen
!!            plotters. This is done by calling FACTOR with a value
!!            less than 1.0 after calling PLOTS. When debugging is
!!            completed, this call statement can be removed.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_factor
!!    use M_calcomp, only : plots, plot, number, symbol, newpen
!!    use M_calcomp, only : nframe, factor, rect
!!    use M_calcomp, only : MOVE, END, DRAW
!!    call plots(0.0,10.0,0.0,10.0)
!!    call draw_car_prices()
!!    call nframe()
!!    call factor(0.5)
!!    call draw_car_prices()
!!    call plot( 5.0, 5.0,-3)
!!    call factor(0.5)
!!    call draw_car_prices()
!!    call plot(0.0,0.0,end)
!!    contains
!!    subroutine draw_car_prices()
!!       character(len=21) :: ichr6
!!       character(len=19) :: ichr7
!!       character(len=17) :: ichr8
!!       ichr6='CAR MODEL AGE (YEARS)'
!!       ichr7='CAR VALUE (DOLLARS)'
!!       ichr8='AVERAGE CAR VALUE'
!!       !     CALL TO SYMBOL USES -0.5Y, -0.8-.14  X
!!       !     (-.14 FOR CHARACTER HEIGHT)
!!       call rect(0.0,0.0,10.0,10.0,0.0,7)
!!       call plot(0.95,0.5,-MOVE)
!!       ! PLOT CAR VALUE CHART WITHOUT USING SCALE,AXIS,OR LINE
!!       x=1.0
!!       ! PLOT X-AXIS
!!       do i=1,7
!!          call plot(x-1.0,0.0,MOVE)
!!          call plot(x   , 0.0,DRAW)
!!          call plot(x   ,-0.1,DRAW)
!!          call number(x-.02,-0.25,0.1,x,0.0,-1)
!!          x=x+1.0
!!       enddo
!!       call symbol(2.0,-0.5,0.14,ichr6,inteq,0.0,21)
!!       ! PLOT Y-AXIS
!!       value=1000.0
!!       do i=1,6
!!          y=0.0015*value
!!          call plot(0.0,y-1.5,MOVE)
!!          call plot(0.0,y-.75,DRAW)
!!          call plot(-.1,y-.75,DRAW)
!!          call plot(0.0,y-.75,DRAW)
!!          call plot(0.0,y    ,DRAW)
!!          call plot(-.1,y    ,DRAW)
!!          call number(-0.7,y,0.14,value,0.0,-1)
!!          value=value+1000.0
!!       enddo
!!       call symbol(-0.8,3.1,0.14,ichr7,inteq,90.0,19)
!!       ! PLOT CURVES
!!       call newpen(2)
!!       do i=2000,6000,500
!!          value=i
!!          age=0.0
!!          call plot(age,0.0015*value,MOVE)
!!          do j=1,84
!!             value=value*0.972
!!             age=age+0.08333
!!             call plot(age,0.0015*value,DRAW)
!!          enddo
!!       enddo
!!       call newpen(3)
!!       call symbol(3.0,6.0,0.21,ichr8,inteq,0.0,17)
!!       end subroutine draw_car_prices
!!       end program demo_factor
!!
!!##LICENSE
!!    Public Domain
subroutine factor(fct)

! ident_18="@(#) M_calcomp factor(3f) rescale entire plot"

real,intent(in) :: fct
   call plot(fct,fct,1001)
end subroutine factor
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine mset(mode)

! ident_19="@(#) M_calcomp mset(3f) this is a general mode setting routine."

! FUNCTION: SET THE MODE ACCORDING TO THE CHARACTER VALUE PASSED
!           AS MODE. THE MODES ARE SET IN COMMON WMODE
!
! DATE: 3/85
!
   character(len=*),intent(in) :: mode
   character l_mode*8
!
   l_mode=mode
!
   if(l_mode(1:4) .eq. 'HARD')cttyp_q='HARD'
   if(l_mode(1:4) .eq. 'SOFT')cttyp_q='SOFT'
!
! SET DISCRETE HARDWARE CHARACTER SIZES
!
   if(l_mode(1:4) .eq. 'XLAR')call mpset('TSIZ',61.)
   if(l_mode(1:4) .eq. 'LARG')call mpset('TSIZ',56.)
   if(l_mode(1:4) .eq. 'MEDI')call mpset('TSIZ',37.)
   if(l_mode(1:4) .eq. 'SMAL')call mpset('TSIZ',20.)
!
! ROOM FOR MORE MODES IF NECESSARY
!
end subroutine mset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine mpset(mode,value)
!
! THIS ROUTINE SETS MODE SPECIFIC PARAMETER VALUES
!
! FUNCTION: EXAMINE THE MODE CHARACTER STRING AND SET THE SPECIFIED
!           VALUE IN COMMON WMODEP ACCORDINGLY.
!
character(len=*),intent(in) :: mode
character(len=8)            :: l_mode
real                        :: value
real                        :: v
!
! SET THE CHARACTER SIZE VALUE
!
   l_mode=mode
   if(l_mode(1:4) .eq. 'TSIZ')then
      v=value
      if(value .lt. 1. .or. value .gt. 64.)v=14.0
      ktsize_q=v
!
! OUTPUT THE SIZES TO THE METAFILE
!
      call setpar('TSIZ',ktsize_q)
   endif
!
end subroutine mpset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    line(3f) - [M_calcomp:basic] plot a polyline with optional rescaling
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine line(xarray,yarray,npts,inc,+-lintyp,inteq)
!!
!!##DESCRIPTION
!!
!!  Plots a series of XY points with optional rescaling.
!!
!!  The LINE subroutine produces a line plot of the pairs of data values in two
!!  arrays (XARRAY and YARRAY). LINE computes the page coordinates of each
!!  plotted point according to the data values in each array and the respective
!!  scaling parameters. The data points may be represented by centered symbols
!!  and/or connecting lines between points.
!!
!!  The scaling parameters corresponding to FIRSTV and DELTAV (see SCALE) must
!!  immediately follow each array. If these parameters have not been computed by
!!  the SCALE subroutine they must be supplied by the user. If scaling is not
!!  required, the user must place the appropriate minimum and delta values in the
!!  specified elements of the arrays. For a one-to-one correspondence between
!!  array data and plotted data, these values should be 0.0 (minimum) and 1.0
!!  (delta).
!!
!!##OPTIONS
!!
!!    XARRAY   is the name of the array containing the abscissa (X)
!!             values and the scaling parameters for the X array.
!!
!!    YARRAY   is the name of the array containing the ordinate (Y)
!!             values and the scaling parameters for the Y array.
!!
!!    NPTS     is the number of data points to be plotted in each of the
!!             two arrays just mentioned. The number does not include
!!             the extra two locations for the scaling parameters. The
!!             number of points in each array must be the same.
!!
!!    INC      is the increment that the LINE subroutine is to use in
!!             gathering data from the two arrays, as described
!!             previously for the SCALE subroutine. XARRAY and YARRAY
!!             must be dimensioned NPTS*INC+INC+1.0
!!
!!    +-LINTYP  is a control parameter which describes the type of line
!!              to be drawn through the data points. The magnitude of
!!              LINTYP determines the frequency of plotted symbols.
!!
!!             If LINTYP is zero, the points are connected by straight
!!             lines but no symbols are plotted.
!!
!!             If LINTYP=1, a line plot with a symbol at each data point
!!             is produced.
!!
!!             If LINTYP=n, a line plot connects every data point
!!             defined in the array; a symbol is drawn at every nth data
!!             point. (The pen is up when moving from its current
!!             position to the first point.) For example, if LINTYP=4,
!!             a special symbol (denoted by INTEQ) is plotted at every
!!             fourth data point.
!!             If LINTYP=-n, no connecting lines are drawn; only the
!!             symbols are plotted, at every nth data point.
!!
!!    INTEQ    is the integer equivalent of the special plotting symbol
!!             centered at each data point. This value normally can be
!!             0 through 14 (see Table 2), and has meaning only when
!!             LINTYP is not zero. Some of these symbols are as
!!             follows: box, octagon, triangle, plus, X, diamond, and
!!             asterisk.
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_line
!!    use M_calcomp
!!    call plots(0.0,30.0,0.0,30.0)
!!    call drawplot(0) ! solid line
!!    call plot( 0.0,15.0,-MOVE)
!!    call drawplot(1) ! symbol at each point and line
!!    call plot( 15.0, 0.0,-MOVE)
!!    call drawplot(-1) ! symbol at each point and no line
!!    call plot(0.0,0.0,999)
!!    contains
!!    subroutine drawplot(linetype)
!!    character(len=28) :: ichr1
!!    character(len=26) :: ichr2
!!    character(len=10) :: lbcd1,lbcd2
!!    dimension xarray(62),yarray(62)
!!    ichr1='PLOTTED ON A CALCOMP PLOTTER'
!!    ichr2='USING  Y = X -0.7*X +0.1*X'
!!    lbcd1='X-ABSCISSA'
!!    lbcd2='Y-ORDINATE'
!!    ! PLOT GRAPH ILLUSTRATING SCALE, AXIS, AND LINE
!!    deltax=0.04
!!    ! AXIS DRAWS LABELS AS MUCH AS 0.4 INCHES TO THE NEGATIVE OF AXIS CENTER;
!!    ! EITHER USE AN ORIGIN OFFSET OF AT LEAST THIS VALUE OR DO NOT USE AN
!!    ! ORIGIN VALUE OF LESS THAN 0.4 OR CLIPPING WILL OCCUR
!!    call width(0)
!!    call newpen(WHITE)
!!    call rect(0.0,0.0,10.0,10.0,0.0,7)
!!    call plot(0.4,0.4,-3)
!!    deltax=2.0*deltax
!!    xarray(1)=deltax
!!    do j=1,60
!!       yarray(j)=xarray(j)**2-0.7*xarray(j)**3+0.1*xarray(j)**4
!!       xarray(j+1)=xarray(j)+deltax
!!    enddo
!!    inteq=4
!!    call scale(xarray(1), 6.5,60,1)
!!    call scale(yarray(1),10.0,60,1)
!!    call axis(0.0,0.0,lbcd1,-10, 6.5, 0.0,xarray(61),xarray(62))
!!    call axis(0.0,0.0,lbcd2, 10,10.0,90.0,yarray(61),yarray(62))
!!    call width(20)
!!    call newpen(GREEN)
!!    call line(xarray(1),yarray(1),60,1,linetype,inteq)
!!    call newpen(1)
!!    call symbol(1.3,10.,.14,ichr1,inteq,0.0,28)
!!    call symbol(1.3,9.7,.14,ichr2,inteq,0.0,26)
!!    call number(2.98,9.8,.1,2.0,0.,-1)
!!    call number(3.96,9.8,.1,3.0,0.,-1)
!!    call number(4.94,9.8,.1,4.0,0.,-1)
!!    call plot(-0.4,-0.4,-3)
!!    end subroutine drawplot
!!    end program demo_line
!!
!!##LICENSE
!!    Public Domain
subroutine line(xarray,yarray,npts,inc,lintyp,inteq)

! ident_20="@(#) M_calcomp line(3f) Plot a polyline with optional rescaling"

!          XARRAY  NAME OF ARRAY CONTAINING ABSCISSA OR X VALUES.
!          YARRAY  NAME OF ARRAY CONTAINING ORDINATE OR Y VALUES.
!          NPTS    NUMBER OF POINTS TO BE PLOTTED.
!          INC     INCREMENT OF LOCATION OF SUCCESSIVE POINTS.
!          LINTYP  CONTROL TYPE OF LINE--SYMBOLS, LINE, OR COMBINATION.
!          INTEQ   INTEGER EQUIVALENT OF SYMBOL TO BE USED, IF ANY.
real    :: xarray(*)
real    :: yarray(*)
real    :: deltax
real    :: deltay
real    :: df
real    :: dl
real    :: firstx
real    :: firsty
integer :: i
integer :: icode
integer :: icodea
integer :: inc
integer :: inteq
integer :: ipen
integer :: ipena
integer :: kk
integer :: ldx
integer :: lintyp
integer :: lmin
integer :: lsw
integer :: na
integer :: nf
integer :: nl
integer :: npts
integer :: nt
real    :: xn
real    :: yn
   lmin = npts*inc+1
   ldx  = lmin+inc
   nl   = lmin-inc
   firstx = xarray(lmin)
   deltax = xarray(ldx)
   firsty = yarray(lmin)
   deltay = yarray(ldx)
   call where(xn,yn,df)
   df=max(abs((xarray( 1)-firstx)/deltax-xn), abs((yarray( 1)-firsty)/deltay-yn) )
   dl=max(abs((xarray(nl)-firstx)/deltax-xn), abs((yarray(nl)-firsty)/deltay-yn) )
   ipen = 3
   icode = -1
   nt =abs(lintyp)
   if(lintyp) 7,6,7
6  nt = 1
7  if(df-dl) 9,9,8
8  nf = nl
   na = ((npts-1)/nt)*nt+nt-(npts-1)
   kk = -inc
   goto 10
9  nf = 1
   na = nt
   kk = inc
10 if(lintyp) 11,12,13
11 ipena = 3
   icodea = -1
   lsw = 1
   goto 15
12 na=ldx
13 ipena = 2
   icodea = -2
   lsw=0
15 do 30 i =1,npts
      xn = (xarray(nf)-firstx)/deltax
      yn = (yarray(nf)-firsty)/deltay
      if(na-nt) 20,21,22
20    if(lsw) 23,22,23
21    call symbol(xn,yn,0.08,' ',inteq,0.0,icode)
      na = 1
      goto 25
22    call plot(xn,yn,ipen)
23    na = na + 1
25    nf = nf+kk
      icode = icodea
30 ipen = ipena
end subroutine line
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    width(3f) - [M_calcomp:basic] select pen width
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine width(iwidth)
!!
!!    integer,intent(in) :: iwidth
!!
!!##DESCRIPTION
!!
!!    Select a new pen width. Sets the current line width in units of
!!    1/10,000 of the X size of the display surface
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_width
!!       use M_calcomp
!!       implicit none
!!       integer :: i,n, ii
!!       real    :: angle, rad, xx, yy
!!       call plots(0.0,10.0,0.0,10.0)
!!       call plot(5.0,5.0,-3)
!!       angle=0.0
!!       n=30
!!       do i=1,n
!!          ii=360/n
!!          call width(i*10)
!!          rad=0.0174533*angle
!!          xx=5.0*cos (rad)
!!          yy=5.0*sin (rad)
!!          call plot( 0.0, 0.0,3)
!!          call plot(  xx,  yy,2)
!!          angle=angle+360.0/n
!!       enddo
!!       call plot(0.0,0.0,999)
!!    end program demo_width
!!
!!  COMMENTS
!!
!!  This routine was not part of the original Calcomp library.
!!
!!##LICENSE
!!    Public Domain
subroutine width(iwidth)

! ident_21="@(#) M_calcomp width(3f) select new pen width"

integer,intent(in)    :: iwidth ! (positive integer) new pen width

   if(iwidth.ge.0)then
      call primitive__width(iwidth)
   endif

end subroutine width
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    newpen(3f) - [M_calcomp:basic] select new pen color and move to origin
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine newpen(icolor)
!!
!!    integer,intent(in) :: icolor
!!
!!##DESCRIPTION
!!
!!
!!  Select a new pen color and move to origin. The number of colors available
!!  is output-device-dependent, but on almost all color devices the
!!  following values will produce the associated colors:
!!
!!    0 or BLACK
!!    1 or RED
!!    2 or GREEN
!!    3 or YELLOW
!!    4 or PURPLE
!!    5 or MAGENTA
!!    6 or CYAN
!!    7 or WHITE    (the default)
!!
!!  COMMENTS
!!
!!  This routine only produces color when CFT levels of 11531 or above
!!  are used. Before this, this routine forced the pen back to the frame
!!  origin and had no other affect.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_newpen
!!    use M_calcomp
!!    character(len= 4)  :: ICHR3='ANG='
!!    character(len= 4)  :: ICHR4=', H='
!!    character(len= 19) :: ICHR5='ANGULAR LETTER TEST'
!!    call plots(0.0,10.0,0.0,10.0)
!!    ! PLOT ANGULAR LETTER TEST
!!    call plot(4.5,5.5,-3)
!!    angle=0.0
!!    height=0.105
!!    do i=1,8
!!       call newpen(i)
!!       rad=0.0174533*angle
!!       xx=0.5*cos (rad)
!!       yy=0.5*sin (rad)
!!       call symbol( xx  , yy  ,height,ichr3,inteq,angle, 4)
!!       call number(999.0,999.0,height,angle ,angle,-1)
!!       call symbol(999.0,999.0,height,ichr4,inteq,angle, 4)
!!       call number(999.0,999.0,height,height,angle, 3)
!!       height=height+0.035
!!       angle=angle+45.0
!!    enddo
!!    call newpen(1)
!!    call symbol(-1.4,4.0,0.14,ichr5,inteq,0.0,19)
!!    call plot( 4.5, 5.0,3)
!!    call plot(-4.5, 5.0,2)
!!    call plot(-4.5,-5.5,2)
!!    call plot( 4.5,-5.5,2)
!!    call plot( 4.5, 5.0,2)
!!    !call plot( 6.5,-5.5,-3)
!!    call plot(0.0,0.0,999)
!!    end program demo_newpen
!!
!!##LICENSE
!!    Public Domain
subroutine newpen(index)

! ident_22="@(#) M_calcomp newpen(3f) select new pen color and move to origin"

integer,intent(in)    :: index ! (positive integer) new pen color

   if(index.ge.0)then
      call primitive__wpen(index)
   endif

!     ensure that the pen is moved to the current origin (0.0,0.0)
!     call plot(0.0,0.0,1004)

end subroutine newpen
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    nframe(3f) - [M_calcomp:basic] start new frame
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine nframe()
!!
!!##DESCRIPTION
!!
!!  Terminates the current frame and resets the current pen position and
!!  origin to zero (The lower left-hand corner of the new frame).
!!
!!  NFRAME is a locally-developed routine.
!!
!!  Subroutine NFRAME allows users to define the logical beginning
!!  and ending of a plot. This capability is necessary if plots are to
!!  be drawn on a graphics device with a limited display area (e.g.,
!!  a graphics terminal or microfiche/ film). A call to NFRAME enters
!!  a plot terminator in the user's metalanguage file. All plotting
!!  data generated between calls to NFRAME is treated as a single plot
!!  by the graphics post processing procedures. The absence of calls to
!!  NFRAME results in all plotting data being processed as a single
!!  plot by the post processor (e.g., all plotting data will be drawn
!!  in a single frame on microfiche).
!!
!!  The call to NFRAME should be placed before the call to subroutine
!!  PLOT that is used to move the pen to the origin of the next plot.
!!  Be aware that the area encompassed by moving the pen to
!!  establish the origin of a plot is considered part of the plot and will
!!  produce a visible bottom and left margin on plot frames if no negative
!!  values are subsequently used.
!!
!!  As mentioned previously, a plot frame is limited to a maximum size in
!!  either the X or Y direction of 100 "CALCOMP inches." Each plot frame is
!!  initialized by a call to subroutine PLOT with the third argument
!!  (IPEN) equal to -2 or -3. For example:
!!
!!        CALL PLOT(0.5,1.0,-3)
!!
!!  says to move 0.5 inches in the X-direction and 1.0 inch in the
!!  Y-direction before establishing a new origin. When establishing a
!!  new origin, all offsets are included inside the frame boundary and
!!  are therefore part of the plot frame size. If any X or Y coordinate
!!  value (plus the appropriate offset) exceeds the 100 inch limit,
!!  results are unpredictable. In programs where X and Y coordinate values
!!  exceed the scaling limit, a call to the CALCOMP routine FACTOR
!!  may be used to scale down the plot size appropriately. No additional
!!  offset is added by the call to NFRAME. Knowledge of the plot frame
!!  size in the X and Y directions will be needed to scale pen plots to
!!  actual inches with the device dependent post processing procedures.
!!  The following example is provided to assist in understanding how the
!!  frame size is determined.
!!
!!##EXAMPLE
!!
!!  Sample program
!!
!!        program demo_nframe
!!        use M_calcomp
!!        implicit none
!!        !
!!        ! Perform initialization
!!        call plots(0.0,10.0,0.0,10.0)
!!        !
!!        ! Establish origin for first plot (Negative Y values up to -0.5 are
!!        ! now permitted also)
!!        call plot( 0.0, 0.5, -3)
!!        !
!!        ! Draw a box inside of which all lines will appear
!!        ! but notice plot frame size now includes the offset plus this box size
!!        ! Plot frame size = maximum coordinate value used + offset
!!        ! Plot frame size in the X-direction is 8 inches
!!        ! Plot frame size in the Y-direction is 9.5 inches (0.5 offset in PLOT
!!        ! call above
!!        call plot( 8.0, 0.0, 2)
!!        call plot( 8.0, 9.0, 2)
!!        call plot( 0.0, 9.0, 2)
!!        call plot( 0.0, 0.0, 2)
!!        !
!!        ! Calls to generate first plot go here
!!        ! .
!!        ! .
!!        ! .
!!        ! Terminate first plot
!!        call nframe()
!!        !
!!        ! Establish origin for second plot
!!        call plot(1.0, 2.0, -3)
!!        ! Plot frame size in the X-direction is 6 inches
!!        ! Plot frame size in the Y-direction is 6 inches
!!        call plot(5.0, 0.0, 2)
!!        call plot(5.0, 4.0, 2)
!!        call plot(0.0, 4.0, 2)
!!        call plot(0.0, 0.0, 2)
!!        !
!!        ! Calls to generate second plot go here
!!        ! .
!!        ! .
!!        ! .
!!        ! Close the plot file
!!        call plot(0.0, 0.0, 999)
!!        end program demo_nframe
!!
!!  An inch drawn in frame 1 will not appear equal in length to an inch
!!  drawn in frame 2 because their unit-less frame sizes are not equal (
!!  8.5 x 9 versus 6 x 6 ) !
!!
!!  The size of each frame is determined by the maximum value reached
!!  in each frame relative to the ORIGINAL frame origin. Each frame, when
!!  plotted, is stretched without distortion to the maximum size
!!  it can obtain in the plotting area specified on post-processor calls
!!  (Usually the SIZE, and XI and YI parameters as described in the DOCLIB
!!  document GRPHDOC).
!!
!!##LICENSE
!!    Public Domain
subroutine nframe

! ident_23="@(#) M_calcomp nframe(3f) start new frame"

   call plot(0.0,0.0,1008)
end subroutine nframe
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    number(3f) - [M_calcomp:basic] plots a floating-point number
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine number(xpage,ypage,height,fpn,angle,+-ndec)
!!
!!##DESCRIPTION
!!
!!  Subroutine NUMBER plots a floating-point number; using the specified number
!!  of decimals in the mantissa (Using FORTRAN F-type format).
!!
!!  The routine is very similar to SYMBOL, with the exception that a numeric
!!  value, not a string, is to be plotted.
!!
!!##OPTIONS
!!
!!    XPAGE,YPAGE,    are the same as those arguments described for subroutine
!!    HEIGHT,ANGLE    SYMBOL. The continuation feature, where XPAGE or YPAGE
!!                    equals 999.0, may be used.
!!
!!    FPN             is the floating-point number that is to be converted and
!!                    plotted.
!!
!!    +-NDEC          controls the precision of the conversion of the number
!!                    FPN. If the value of NDEC>0, it specifies the number of
!!                    digits to the right of the decimal point that are to be
!!                    converted and plotted, after proper rounding. For
!!                    example, assume an internal value of - 0.12345678 x
!!                    10**3. If NDEC were 2, the plotted number would be
!!                    -123.46.0
!!
!!                    If NDEC=0, only the number's integer portion and a
!!                    decimal point are plotted, after rounding.
!!
!!                    If NDEC=-1, only the number's integer portion is plotted,
!!                    after rounding. (The above example would be plotted as
!!                    -123 with no decimal point).
!!                     If NDEC < -1, ABS(NDEC) -1 digits are truncated from the
!!                    integer portion, after rounding.
!!
!!                    The magnitude of NDEC should not exceed 9 .
!!##EXAMPLE
!!
!!  Sample program:
!!
!!    program demo_number
!!    use M_calcomp
!!    implicit none
!!    character(len=28),parameter :: ichr4='EXAMPLE OF NUMBER SUBROUTINE'
!!    real,parameter              :: znum(4)=[10293.84756,193.75,-204.86,-12345.6789]
!!    real                        :: y
!!    integer                     :: ia, ib
!!    integer                     :: inteq
!!       call plots(0.0,10.0,0.0,10.0)
!!       y=9.5
!!       ! the following tests the NUMBER(3f) subroutine for precision
!!       call symbol(0.5,2.5,.20,ichr4,inteq,90.0,28)
!!       y=10.0
!!       do ia=1,4
!!          do ib=1,11
!!             call number(1.0,y,0.14,znum(ia),0.0,ib-6)
!!             y=y-0.2
!!          enddo
!!          y=y-0.3
!!       enddo
!!       call nframe()
!!       call plot(0.0,0.0,999)
!!    end program demo_number
!!
!!##LICENSE
!!    Public Domain
subroutine number(xpage,ypage,height,fpn,angle,ndec)

! ident_24="@(#) M_calcomp number(3f) plots a floating-point number"

!     XPAGE,YPAGE COORDINATES OF LOWER LEFT CORNER OF NUMBER.
!     HEIGHT   HEIGHT OF PLOTTED NUMBER.
!     FPN      FLOATING POINT NUMBER TO BE PLOTTED.
!     ANGLE    ANGLE AT WHICH NUMBER IS PLOTTED, IN DEGREES.
!     NDEC     NUMBER OF DECIMAL PLACES TO BE DRAWN.
!     THIS VERSION OF NUMBER REQUIRES THE SYMBOL VERSION WITH
!     999. X, Y FEATURE, AND  NC = 0 FEATURE.
character(len=20) :: num
character(len=1)  :: minus,izero,ipoint
save minus,izero,ipoint
data minus /'-'/,izero/'0'/,ipoint/'.'/
real    :: angle
real    :: fpn
real    :: fpv
real    :: height
integer :: i
integer :: ii
integer :: ilp
integer :: inteq
integer :: j
integer :: k
integer :: kk
integer :: maxn
integer :: mn
integer :: n
integer :: ndec
real    :: xpage
real    :: ypage
!     IZERO='0'
      ii=0
      fpv = fpn
      n = ndec
      maxn = 9
! ESCC MOD ------------------------
      inteq=999
! ---------------------------------------------
      if(n - maxn) 11, 11, 10
10    n = maxn
11    if(n + maxn) 12, 20, 20
12    n = -maxn
20    if(fpv) 21, 30, 30
21    ii=ii+1
      num(ii:ii)=minus
30    mn = -n
      if(n) 31, 32, 32
31    mn = mn - 1
32    fpv = abs(fpv) + (0.5 * 10. ** mn)
      i = log10(fpv)+1.0
      ilp = i
      if(n + 1)  40, 41, 41
40    ilp = ilp + n + 1
41    if(ilp)  50, 50, 51
50    ii=ii+1
      num(ii:ii)=izero
      goto 61
51    if(ilp+n-18) 54,54,52
52    n=-1
      if(ilp-19) 54,54,53
53    ilp=19
54    do 60 j=1,ilp
         k = fpv * 10. ** (j - i)
         if(k-9) 57,57,55
55       k = 9
57       ii=ii+1
         kk = ichar(izero) + k
         num(ii:ii)=char(kk)
         fpv = fpv - (float(k) * 10. ** (i - j))
60    continue
61    if(n) 99, 70, 70
70    ii=ii+1
      num(ii:ii)=ipoint
      if(n)  99, 99, 80
80    do 90 j = 1, n
         k = fpv * 10.0
         if(k-9) 84,84,82
82       k = 9
84       ii=ii+1
         kk = ichar(izero) + k
         num(ii:ii)=char(kk)
90    fpv = fpv * 10. - float(k)
!
!  THE FOLLOWING CALL TO 'SYMBOL' WAS MODIFIED WHEN THIS SUBROUTINE WAS
!  CONVERTED TO THE CRAY. THE STATEMENT ORIGINALLY READ
!
!  99    CALL SYMBOL(XPAGE,YPAGE,HEIGHT,NUM,INTEQ,ANGLE,II+1000)
!
!  THE 'SYMBOL' SUBROUTINE DESIGNED FOR USE ON THE CRAY TREATS INPUT
!  COMING FROM THE 'NUMBER' SUBROUTINE IN THE SAME FASHION AS OTHER SYMB
!  HENCE, IT IS NOT NECESSARY TO ADD 1000 TO THE FINAL ARGUMENT IN THE
!  'SYMBOL' CALL (THE ARGUMENT WHICH CONTAINS THE NUMBER OF CHARACTERS
!  TO BE DRAWN).
!
99    call symbol(xpage,ypage,height,num,inteq,angle,ii)
end subroutine number
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plot(3f) - [M_calcomp:basic] move with pen up or down or start new origin or terminate plotting
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine plot(xpage,ypage,+-ipen)
!!
!!##DESCRIPTION
!!
!!  The PLOT subroutine is used to move the pen in a straight line
!!  to a new position, with the pen either up or down during the movement.
!!
!!  Another function of PLOT is to establish a new reference point
!!  (origin) for the current plot frame. This must be done if any draws
!!  or moves use values which are negative relative to the ORIGINAL
!!  frame origin.
!!
!!  PLOT also is used to terminate CALCOMP plotting, and must be
!!  called once and only once at the end of plotting calls in each CALCOMP
!!  application.
!!
!!##OPTIONS
!!
!!    XPAGE, YPAGE  are the X, Y coordinates in CALCOMP inches. The values
!!                  are measured relative to the current frame reference
!!                  (origin).
!!
!!                  An origin (where both X, Y equal zero) may be
!!                  established anywhere on the plotting surface by using
!!                  negative IPEN values, as explained below.
!!
!!                  Because CALCOMP routines are interfaced to write a TEMPLATE
!!                  PDF, some limits on X and Y coordinates were required.
!!                  All coordinate values (XPAGE, YPAGE) should be
!!                  greater than or equal to zero and less than 100 inches.
!!                  If negative values are necessary a new frame origin must
!!                  be set so the negative values are positive relative to
!!                  the ORIGINAL frame origin.
!!
!!                        0 < XPAGE+origin x-offset < 100
!!                        0 < YPAGE+origin y-offset < 100
!!
!!                  The values of XPAGE and YPAGE which are used to establish
!!                  a new origin must also be considered. See the discussion
!!                  under subroutine NFRAME for details.
!!
!!    +-IPEN        is a signed integer which controls pen status (up or
!!                  down), and the origin definition.
!!
!!                  If IPEN=2, the pen is down during movement, thus drawing
!!                  a visible line.
!!
!!                  If IPEN=3, the pen is up during movement.
!!
!!                  If IPEN= -2, or -3, a new origin is defined at (XPAGE,YPAGE)
!!                  after the movement is completed as if IPEN were positive.
!!
!!                  That is, the X,Y coordinates of the new pen position are
!!                  set equal to zero. This position is the reference point
!!                  for succeeding pen movements.
!!
!!                  If IPEN=999 the metalanguage file is closed. (Note this
!!                  must be the last call made by the plotting application).
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_plot
!!    use m_calcomp
!!    implicit none
!!    character(len=10),parameter :: ichr1='WIDTH (FT)'
!!    character(len=14),parameter :: ichr2='THICKNESS (IN)'
!!    character(len=14),parameter :: ichr3='PRESSURE (PSI)'
!!    character(len=5),parameter  :: ichr4='THK= '
!!    character(len=4),parameter  :: ichr5=' IN.'
!!    character(len=5),parameter  :: ichr6='WTH= '
!!    character(len=4),parameter  :: ichr7=' FT.'
!!    character(len=29),parameter :: ichr8='CRITICAL BUCKLING PRESSURE OF'
!!    character(len=32),parameter :: ichr9='HYPERBOLIC PARABOLOID SHELLS FOR'
!!    character(len=32),parameter :: ichr10='FIXED WIDTH VS VARYING THICKNESS'
!!    character(len=32),parameter :: ichr11='FIXED THICKNESS VS VARYING WIDTH'
!!    character(len=32),parameter :: ichr12='PREPARED ON A CALCOMP PLOTTER'
!!    character(len=1)            :: ibcd
!!    integer                     :: i,j
!!    integer                     :: inteq
!!    real                        :: x,y
!!    real                        :: psi
!!    real                        :: thick, wdth
!!    real                        :: tsqr, wsqr
!!    real                        :: tx, wx
!!       call plots(0.0,24.0,0.0,12.0)
!!    ! ESTABLISH AN ORIGIN SO NEGATIVE VALUES UP TO -0.5 MAY BE USED
!!       call plot(0.5,0.5,-3)
!!    ! PLOT X-AXIS FOR WIDTH
!!       x=0.0
!!       do i=1,10
!!          call plot(x,0.0,3)
!!          x=x+1.0
!!          call plot(x,0.0,2)
!!          call plot(x,-.1,2)
!!          call number(x,-0.25,0.1,5.0*x,0.0,-1)
!!       enddo
!!       call symbol(4.0,-0.40,0.12,ibcd,1,0.0,-1)
!!       call symbol(4.2,-0.45,0.14,ichr1,inteq,0.0,10)
!!       call plot(0.0,0.5,-3)
!!    ! PLOT X-AXIS FOR THICKNESS
!!       x=0.0
!!       do i=1,5
!!          call plot(x,0.0,3)
!!          x=x+1.0
!!          call plot(x,0.0,2)
!!          call plot(x,-.1,2)
!!          call plot(x,0.0,2)
!!          x=x+1.0
!!          call plot(x,0.0,2)
!!          call plot(x,-.1,2)
!!          call number(x,-0.25,0.1,x,0.0,-1)
!!       enddo
!!       call symbol(3.7,-0.40,0.12,ibcd,7,0.0,-1)
!!       call symbol(4.0,-0.45,0.14,ichr2,inteq,0.0,14)
!!    ! PLOT Y-AXIS
!!       y=0.0
!!       do i=1,9
!!          call plot(0.0,y,3)
!!          y=y+1.0
!!          call plot(0.0,y,2)
!!          call plot(-.1,y,2)
!!          call number(-.15,y-.2,0.1,1000.*y,90.0,0)
!!       enddo
!!       call symbol(-0.30,3.5,0.14,ichr3,inteq,90.0,14)
!!       thick=3.0
!!       wdth=25.0
!!       do i=1,3
!!          tsqr=thick*thick
!!          wsqr=wdth*wdth
!!          psi=100.99*tsqr
!!          call symbol(0.6,psi/1000.0,0.1,ichr4,inteq,0.0,5)
!!          call number(999.0,999.0,0.10,thick,0.0,0)
!!          call symbol(999.0,999.0,0.10,ichr5,inteq,0.0,4)
!!          call symbol( 2.0, 999.0,0.12,ibcd,1,0.0,-1)
!!          do j=10,50
!!             wx=real(j)
!!             psi=10099.0*tsqr/(wx*wx)
!!             call plot(wx/5.0,psi/1000.0,2)
!!          enddo
!!          psi=10099.0*81.0/wsqr
!!          call symbol(9.2,psi/1000.0,0.1,ichr6,inteq,0.0,5)
!!          call number(999.0,999.0,0.10,wdth,0.0,0)
!!          call symbol(999.0,999.0,0.10,ichr7,inteq,0.0,4)
!!          call symbol( 9.0, 999.0,0.12,ibcd,7,0.0,-1)
!!          do j=5,50
!!             tx=(50.0-real(j))/5.0
!!             psi=10099.0*tx*tx/wsqr
!!             call plot(tx,psi/1000.0,2)
!!          enddo
!!          thick=thick+3.0
!!          wdth=wdth-5.0
!!       enddo
!!       call symbol(3.3,8.5,.14,ichr8,inteq,0.0,29)
!!       call symbol(3.1,8.2,.14,ichr9,inteq,0.0,32)
!!       call symbol(3.1,7.9,.14,ichr10,inteq,0.0,32)
!!       call symbol(3.1,7.6,.14,ichr11,inteq,0.0,32)
!!       call symbol(3.3,7.0,.14,ichr12,inteq,0.0,29)
!!       call plot(0.0,0.0,999)
!!    end program demo_plot
!!##LICENSE
!!    Public Domain
subroutine plot(xpag, ypag, ipen)

! ident_25="@(#) M_calcomp plot(3f) move with pen up or down or start new origin or terminate plotting"

!
!  SUBROUTINE DESCRIPTION -
!
!       THIS SUBROUTINE PLOTS LINE SEGMENTS. THE LINE SEGMENT
!       BEGINS AT THE CURRENT PEN POSITION (DEFINED IN THE LAST
!       CALL TO THIS SUBROUTINE) AND ENDS AT THE POSITION
!       SPECIFIED BY THE CALLING ARGUMENTS. THE PEN MAY BE
!       EITHER "UP" OR "DOWN" DURING THE MOVE TO THE NEW
!       COORDINATES.
!
!  SUBROUTINES CALLED -
!
!       primitive__frend         GRAPHICS PRIMITIVE; TERMINATES THE CURRENT PLOT FRAME
!       primitive__draw_line     GRAPHICS PRIMITIVE; DRAWS A STRAIGHT LINE BETWEEN THE SPECIFIED POINTS
!       primitive__end_plotting  GRAPHICS PRIMITIVE; TERMINATES THE GRAPHICS METALANGUAGE FILE
!
!  FUNCTIONS CALLED -
!
!       ABS      SYSTEM FUNCTION; CALCULATES THE ABSOLUTE VALUE OF A REAL NUMBER
!
!  CALLING ARGUMENTS -
!
!       IPEN     INTEGER SPECIFYING WHETHER THE PEN IS TO
!                BE "UP" OR "DOWN" DURING THE MOVE
!                TO THE SPECIFIED COORDINATES; AN
!                ABSOLUTE VALUE OF 2 INDICATES THAT
!                THE PEN IS "DOWN"; AN ABSOLUTE VALUE
!                OF 3 INDICATES THAT THE PEN IS "UP"
!
!       XPAG     REAL VARIABLE CONTAINING THE X COORDINATE
!                TO WHICH THE PEN IS TO BE MOVED
!
!       YPAG     REAL VARIABLE CONTAINING THE Y COORDINATE
!                TO WHICH THE PEN IS TO BE MOVED
!
!
!  VARIABLE NAMES USED -
!
!       DX       SCALING FACTOR FOR X DIRECTION
!
!       DY       SCALING FACTOR FOR Y DIRECTION
!
!       IPEN     *** CALLING ARGUMENT ***
!
!       NOEND    LOGICAL VARIABLE INDICATING WHETHER
!                THE CURRENT PLOT FRAME HAS BEEN
!                TERMINATED VIA THE 'primitive__frend' PRIMITIVE
!
!       PENX     X COORDINATE TO WHICH THE PEN IS TO BE
!                MOVED
!
!       PENY     Y COORDINATE TO WHICH THE PEN IS TO BE
!                MOVED
!
!       X        X COORDINATE TO WHICH THE PEN IS TO BE
!                MOVED; SPECIFIED WITH RESPECT TO
!                THE CURRENTLY DEFINED ORIGIN FOR
!                THE FRAME; INCLUDES SCALING
!                FACTOR
!
!       XLAST    X COORDINATE OF CURRENT PEN POSITION
!
!       XOFF     OFFSET IN X DIRECTION (REQUIRED FOR
!                DIGP PROCESSING)
!
!       XORG     ABSOLUTE X COORDINATE OF CURRENTLY
!                DEFINED FRAME ORIGIN
!
!       XPAG     *** CALLING ARGUMENT ***
!
!       Y        Y COORDINATE TO WHICH THE PEN IS TO BE
!                MOVED; SPECIFIED WITH RESPECT TO
!                CURRENTLY DEFINED FRAME ORIGIN;
!                INCLUDES SCALING FACTOR
!
!       YLAST    Y COORDINATE OF CURRENT PEN POSITION
!
!       YORG     ABSOLUTE Y COORDINATE OF CURRENTLY
!                DEFINED FRAME ORIGIN
!
!       YPAG     *** CALLING ARGUMENT ***
!
!       YS       Y VALUE IF THE VALUE LIES BELOW THE
!                LOWER EDGE OF THE PLOT; RETAINED
!                FOR USE DURING NEXT CALL TO THIS
!                SUBROUTINE
!
      LOGICAL,save ::  NOEND=.false.
!
!  INITIALIZE VARIABLES TO BE USED BY SUBROUTINE 'PLOT':
!       DX    = X SCALING FACTOR = 1.0
!       DY    = Y SCALING FACTOR = 1.0
!       PENX  = NEW X COORDINATE = 0.0
!       PENY  = NEW Y COORDINATE = 0.0
!       XLAST = LAST X COORDINATE = 0.0
!       XOFF  = OFFSET IN X DIRECTION = 0.0
!       XORG  = X COORDINATE OF CURRENT (RELATIVE) ORIGIN = 0.0
!       YLAST = LAST Y COORDINATE = 0.0
!       YORG  = Y COORDINATE OF CURRENT (RELATIVE) ORIGIN = 0.0
!       YS    = OFFSET TO BE APPLIED IN Y DIRECTION DURING NEXT
!               CALL TO 'PLOT' IF CURRENT Y COORDINATE IS BELOW
!               BOTTOM EDGE OF PLOTTER = 0.0
!
!
!  DETERMINE IF THE VALUE OF 'IPEN' IS GREATER THAN 1000. (A
!  VALUE GREATER THAN 1000 INDICATES A SPECIAL CALL FROM ANOTHER
!  CALCOMP SUBROUTINE.)  IF SO, TRANSFER PROGRAM CONTROL TO THE
!  STATEMENT LABELLED 1000. OTHERWISE, SAVE THE NEW X AND Y
!  COORDINATES SPECIFIED IN THE CALL TO 'PLOT'.
!
!  THESE VARIABLES ARE NOT TO BE INITIALIZED EACH TIME THAT 'PLOT'
!  IS CALLED, BUT ONLY AT THE BEGINNING OF THE PROGRAM;
real,save    :: dx =1.0
real,save    :: dy =1.0
real,save    :: penx = 0.0
real,save    :: peny = 0.0
real,save    :: xlast = 0.0
real,save    :: xoff = 0.0
real,save    :: xorg = 0.0
real,save    :: ylast = 0.0
real,save    :: yorg = 0.0
real,save    :: ys = 0.0
integer :: ipen
real    :: x
real    :: xpag
real    :: y
real    :: ypag
!

      if(abs(ipen).ge.1000) goto 1000
      penx=xpag
      peny=ypag
!
!  CALCULATE THE COORDINATES TO WHICH THE PEN IS TO BE MOVED. THE X
!  COORDINATE IS SPECIFIED WITH RELATION TO THE CURRENTLY DEFINED
!  X ORIGIN (XORG) AND IS CONVERTED FROM ITS INPUT VALUE IN INCHES
!  TO ITS SCALED VALUE. SIMILARLY, THE Y COORDINATE IS SPECIFIED
!  WITH RELATION TO THE CURRENTLY DEFINED Y ORIGIN (YORG). HOWEVER,
!  BEFORE THE NEW Y COORDINATE IS SCALED, ANY ADJUSTMENT IN THE Y
!  DIRECTION REMAINING FROM A PREVIOUS CALL TO 'PLOT' MUST BE
!  FACTORED IN.
!
      x=penx*dx+xorg
      y=(peny+ys)*dy+yorg
!
!  IF THE NEW Y COORDINATE IS BELOW THE BOTTOM EDGE OF THE PLOTTER
!  (I.E., IF Y IS LESS THAN 0.0), SAVE THE NEW Y COORDINATE FOR
!  APPLICATION DURING THE NEXT CALL TO 'PLOT', AND SET THE CURRENT
!  Y COORDINATE TO 0.0 .
!
      if(y.lt.0.0.and.ipen.gt.0) then
         if(ipen.eq.3) ys=abs(peny)
         y=0.0
      endif
!
!  IF THE VALUE OF 'IPEN' IS LESS THAN 0 OR IS EQUAL TO 999, THE
!  RELATIVE ORIGIN OF THE PLOT IS TO BE REDEFINED TO BE THE NEW
!  X AND Y COORDINATES. RESET THE RELATIVE ORIGIN (XORG,YORG)
!  TO THE NEW X AND Y VALUES, THEN SET ANY RESIDUAL Y ADJUSTMENT
!  TO 0.0 .
!
      if((ipen.lt.0) .or. (ipen.eq.999)) then
         xorg=x
         yorg=y
         ys=0.0
      endif
!
!  IF 'IPEN' IS SPECIFIED WITH A VALUE OF 999, THE PLOT IS TO BE
!  TERMINATED. IF THE CURRENT FRAME HAS NOT BEEN ENDED, CALL THE
!  'primitive__frend' GRAPHICS PRIMITIVE. CALL THE 'primitive__end_plotting'
!  GRAPHICS PRIMITIVE TO TERMINATE PLOTTING.
!
      if(ipen.eq.999) then
         if(noend) call primitive__frend(0)
         call primitive__end_plotting
      endif
!
!  IF THE ABSOLUTE VALUE OF 'IPEN' IS 2, THE PEN IS TO BE DOWN DURING
!  THE MOVE TO THE NEW COORDINATES; THEREFORE, CALL THE 'primitive__draw_line'
!  GRAPHICS PRIMITIVE TO DRAW THE DESIRED LINE.
!
      if(abs(ipen).eq.2) call primitive__draw_line(xlast+xoff,ylast,x+xoff,y)
!
!  SAVE THE CURRENT PEN COORDINATES.
!
      xlast=x
      ylast=y
      noend=.true.
      return
!
!  CHECK FOR SPECIAL USE OF THIS SUBROUTINE BY OTHER CALCOMP
!  SUBROUTINES. ALL SPECIAL ENTRIES TO THE 'PLOT' SUBROUTINE USE
!  VALUES OF 'IPEN' GREATER THAN 1000. THE COMPUTED GO TO STATEMENT
!  BELOW INSURES THAT THE PROPER SECTION OF CODE IS USED FOR EACH
!  SPECIAL CALL.
!
1000  continue
!-----------------------------------------------------------------------------------------------------------------------------------
      !!GOTO (1001,1002,1003,1004,1005,1006,1007,1008),IPEN-1000
!-----------------------------------------------------------------------------------------------------------------------------------
      select case(ipen)
!-----------------------------------------------------------------------------------------------------------------------------------
      case(1001) !  SPECIAL ENTRY FOR CALL FROM SUBROUTINE 'FACTOR'.
                 ! 'FACTOR' SETS THE RATIO OF THE DESIRED PLOT SIZE TO THE NORMAL PLOT SIZE.
      dx=xpag
      dy=ypag
!-----------------------------------------------------------------------------------------------------------------------------------
      case(1002) !  SPECIAL ENTRY FOR CALL FROM SUBROUTINE 'WHERE'. 'WHERE' RETURNS
                 !  THE CURRENT PEN COORDINATES AND SCALING FACTOR FOR USE IN USER WRITTEN SUBROUTINES.
      xpag=penx
      ypag=peny
!-----------------------------------------------------------------------------------------------------------------------------------
      case(1003) !  SPECIAL ENTRY FOR CALL FROM SUBROUTINE 'SYMBOL'. THIS CALL RETURNS THE CURRENT SCALING FACTOR FOR THE PLOT.
      xpag=dx
      ypag=dy
!-----------------------------------------------------------------------------------------------------------------------------------
      case(1004) !  SPECIAL ENTRY FOR CALL FROM SUBROUTINE 'NEWPEN'. 'NEWPEN' INSURES THAT THE PEN IS MOVED TO THE ORIGIN (0.0,0.0)
      call primitive__draw_line(0.0+xoff,0.0,0.0+xoff,0.0)
!-----------------------------------------------------------------------------------------------------------------------------------
      case(1005) !  SPECIAL ENTRY FOR CALL FROM SUBROUTINE 'SYMBOL'. THIS CALL RETURNS THE CURRENT ORIGIN.
      xpag=xorg
      ypag=yorg
!-----------------------------------------------------------------------------------------------------------------------------------
      case(1006) !  SPECIAL ENTRY FOR CALL FROM CALCOMP VERSION OF SUBROUTINE 'SYMBOL'. THIS CALL ADVANCES THE PEN POSITION.
      xlast=xlast+xpag
      ylast=ylast+ypag
!-----------------------------------------------------------------------------------------------------------------------------------
      case(1007) !  SPECIAL ENTRY FOR CALL FROM SUBROUTINE 'PLOTS'. THIS CALL INSURES THAT THE PEN IS POSITIONED AT THE
                 ! ORIGIN (0.0,0.0) WHEN THE PLOT PACKAGE IS INITIALIZED.
      call primitive__draw_line(0.0+xoff,0.0,0.0+xoff,0.0)
!-----------------------------------------------------------------------------------------------------------------------------------
      case(1008) !  SPECIAL ENTRY FOR CALL FROM ESCC SUBROUTINE 'NFRAME'. THE PLOTTING
                 !  OF THE FINAL POINT OF THE GRAPH IS INSURED, THEN 'primitive__frend' IS CALLED
                 !  TO PLACE THE END-OF-FRAME MARK IN THE METALANGUAGE FILE. VARIABLES
                 !  USED BY THE 'PLOT' SUBROUTINE ARE RE-INITIALIZED, AND THE PEN IS
                 !  MOVED TO THE 0.0,0.0 POINT.
      call primitive__draw_line(xlast+xoff,ylast,xlast+xoff,ylast)
      call primitive__frend(1)
      xorg=0.0
      yorg=0.0
      xlast=0.0
      ylast=0.0
      penx=0.0
      peny=0.0
      ys=0.0
      noend=.false.
      call primitive__draw_line(0.0+xoff,0.0,0.0+xoff,0.0)
!-----------------------------------------------------------------------------------------------------------------------------------
      case default
      write(*,*)'*plot* ERROR: unknown action value=',ipen
!-----------------------------------------------------------------------------------------------------------------------------------
      end select
end subroutine plot
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    plots(3f) - [M_calcomp:basic] initialize the CALCOMP package
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine plots(0.0,10.0,0.0,10.0)
!!
!!##DESCRIPTION
!!
!!  Subroutine PLOTS is used to initialize the CALCOMP package. It must be
!!  called once and only once before any other CALCOMP calls are used in the
!!  application program.
!!
!!##OPTIONS
!!    XMIN,XMAX,YMIN,YMAX  The bounds of the original inch units bounding
!!                         box. Draws outside of this area are an error.
!!                         If not specified, defaults are: 0.0,10.0,0.0,10.0)
!!                         The environment variables CALCOMP_XMIN, CALCOMP_XMAX,
!!                         CALCOMP_YMIN,CALCOMP_XMIN can be used to override
!!                         the values.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_plots
!!    use M_calcomp, only : plots, plot, newpen, width, rect
!!    use M_calcomp, only : black ,red ,green ,yellow
!!    use M_calcomp, only : purple ,magenta ,cyan ,white
!!    use M_calcomp, only : MOVE, DRAW, END
!!    implicit none
!!    call plots(0.0,10.0,0.0,10.0)          ! initialize graphics
!!    call width(80)
!!    call rect(0.0,0.0,10.0,10.0,0.0,GREEN) ! outline plot area
!!    call plot(5.0,5.0,-3)                  ! set origin
!!    call newpen(RED)                       ! make X across area
!!    call plot(-5.0,-5.0, MOVE)
!!    call plot( 5.0, 5.0, DRAW)
!!    call plot(-5.0, 5.0, MOVE)
!!    call plot( 5.0,-5.0, DRAW)
!!    call plot( 0.0, 0.0, END)
!!    end program demo_plots
!!
!!##LICENSE
!!    Public Domain
subroutine plots(xmin,xmax,ymin,ymax)

! ident_26="@(#) M_calcomp plots(3f) initialize the CALCOMP package"

real,intent(in) :: xmin, xmax, ymin, ymax
!
! MODIFIED 3/85 TO ADD COMMONS WMODE AND WMODEP
! TO INITIALIZE TEXT MODE AND SIZE
!
      cttyp_q='SOFT'
      ktsize_q=37
!
! 'primitive__start_plotting' INITIALIZES THE METALANGUAGE ELEMENTS.
      call primitive__start_plotting(xmin,xmax,ymin,ymax)
      call plot(0.0,0.0,1007)
      call mpset('TSIZ',float(ktsize_q))

end subroutine plots
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    scale(3f) - [M_calcomp:basic] calculate scaling factors for producing XY plots with LINE(3f) and AXIS(3f) routines
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine scale(array,axlen,npts,+-inc)
!!
!!##DESCRIPTION
!!
!!  Calculate scaling factors and a starting value for an array of X or Y values
!!  for use in producing XY plots with the LINE and AXIS routines, for example.
!!
!!  Typically, the user's program will accumulate plotting data in two arrays:
!!
!!    o An array of independent variables, X(i)
!!    o An array of dependent variables, Y(i)=f(X(i))
!!
!!  Typically these values should not be drawn directly in units of inches, but
!!  should be rescaled (A temperature of 3000 should not require the generation
!!  of a 3000 inch plot!).
!!
!!  For some problems the range of data is predictable. The programmer can
!!  predetermine suitable conversion factors for use in drawing the axis scale
!!  values and plot the data points on the graph directly in units of inches
!!  using the PLOT routine. Usually, however, these factors are not known in
!!  advance.
!!
!!  Therefore, the SCALE subroutine can examine the data values in an array and
!!  determine a starting value (minimum or maximum) and a scaling factor
!!  (positive or negative) such that:
!!
!!      1. The scale numbers drawn by the AXIS subroutine at each
!!         division will properly represent the range of real data
!!         values in the array.
!!
!!      2. The data points, when plotted by the LINE subroutine,
!!         will fit in a given plotting area (Generally the bounds of the
!!         plot axis drawn with AXIS).
!!
!!  These values are computed and stored by SCALE at the END OF THE INPUT VALUE
!!  ARRAY.
!!
!!  The computed scaling factor (DELTAV) represents the number of data units per
!!  inch of axis, adjusting DELTAV so that it is always an interval of 1, 2, 4,
!!  5, or 8 x 10**n (where n is an exponent consistent with the original
!!  unadjusted scaling factor). Thus, an array may have a range of values from
!!  301 to 912, to be plotted over an axis of 10 inches. The unadjusted scaling
!!  factor is (912-301)/10=61.1 units/inch. The adjusted DELTAV would be 8 x
!!  10**1 = 80. This will allow the production of 'nice' axes, that start and
!!  end on rounded units and are divided into increments people can easily
!!  interpolate between.
!!
!!  The starting value (FIRSTV) is intended to be used as the first numeric label
!!  on the axis, is computed as a multiple of DELTAV that is equal to or outside
!!  the limits of the data in the array. For the example given above, if a
!!  minimum is wanted for FIRSTV, 240 would be chosen as the best value. If a
!!  maximum is desired instead, 960 would be selected (The nearest multiple of
!!  80=DELTAV that is below or above the minimum and maximum data values 301 and
!!  912).
!!
!!##OPTIONS
!!
!!    ARRAY    is the first element of the array of data points to be
!!             examined.
!!
!!    AXLEN    is the length of the axis, in inches, to which the data
!!             is to be scaled. Its value must be greater than 1.0 inch,
!!             and less than 100 inches.
!!
!!    NPTS     is the number of data values to be scanned in the array.
!!             The FORTRAN DIMENSION statement must specify at least two
!!             elements more than the number of values being scaled, to
!!             allow room for SCALE to store the computed starting value
!!             and scaling factor at the end of the array.
!!
!!    +-INC    is an integer whose magnitude is used by SCALE as the
!!             increment with which to select the data values to be
!!             scaled in the array. Normally INC=1; if it is 2, every
!!             other value is examined.
!!
!!             If INC is positive, the selected starting value (FIRSTV)
!!             approximates a minimum, and the scale factor (DELTAV) is
!!             positive.
!!
!!             If INC is negative, the selected starting value (FIRSTV)
!!             approximates a maximum, and the scaling factor (DELTAV)
!!             is negative.
!!
!!             WARNING
!!
!!               If INC= +-1, the array must be dimensioned at least two
!!               elements larger than the actual number of data values it
!!               contains. If the magnitude of INC > 1, the computed
!!               values are stored at (INC) elements and (2*INC) elements
!!               beyond the last data point. The subscripted element for
!!               FIRSTV is ARRAY(NPTS*INC+1); for DELTAV it is ARRAY
!!               (NPTS*INC+INC+1). Therefore, ARRAY must always be
!!               dimensioned at least NPTS*INC+INC+1 .
!!
!!               Generally, SCALE is called to examine each array to be
!!               plotted. If the user knows the range of his data values,
!!               he does not have to call SCALE for that array so long as
!!               he supplies an appropriate FIRSTV and DELTAV when AXIS
!!               and LINE are called.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_scale
!!    use M_calcomp
!!
!!    character * 28 ichr1
!!    character * 26 ichr2
!!    character * 10 lbcd1,lbcd2
!!    dimension xarray(62),yarray(62)
!!    ICHR1='PLOTTED ON A CALCOMP PLOTTER'
!!    ICHR2='USING  Y = X -0.7*X +0.1*X'
!!    LBCD1='X-ABSCISSA'
!!    LBCD2='Y-ORDINATE'
!!    call plots(0.0,10.0,0.0,10.0)
!!    ! PLOT THREE GRAPHS ILLUSTRATING SCALE, AXIS, AND LINE
!!    deltax=0.04
!!    i=1
!!    ! AXIS DRAWS LABELS AS MUCH AS 0.4 INCHES TO THE NEGATIVE OF AXIS CENTER;
!!    ! EITHER USE AN ORIGIN OFFSET OF AT LEAST THIS VALUE OR DO NOT USE AN
!!    ! ORIGIN VALUE OF LESS THAN 0.4 OR CLIPPING WILL OCCUR
!!    call plot(0.4,0.4,-3)
!!    deltax=2.0*deltax
!!    xarray(1)=deltax
!!    do j=1,60
!!       yarray(j)=xarray(j)**2-0.7*xarray(j)**3+0.1*xarray(j)**4
!!       xarray(j+1)=xarray(j)+deltax
!!    enddo
!!    call scale(xarray(1), 6.5,60,1)
!!    call scale(yarray(1),10.0,60,1)
!!    call axis(0.0,0.0,lbcd1,-10, 6.5, 0.0,xarray(61),xarray(62))
!!    call axis(0.0,0.0,lbcd2, 10,10.0,90.0,yarray(61),yarray(62))
!!    call newpen(i)
!!    call line(xarray(1),yarray(1),60,1,2*(i-2),i)
!!    call newpen(1)
!!    call symbol(1.3,10.,.14,ichr1,inteq,0.0,28)
!!    call symbol(1.3,9.7,.14,ichr2,inteq,0.0,26)
!!    call number(2.98,9.8,.1,2.0,0.,-1)
!!    call number(3.96,9.8,.1,3.0,0.,-1)
!!    call number(4.94,9.8,.1,4.0,0.,-1)
!!
!!    !call plot(10.0,0.0,-3)
!!    call plot(0.0,0.0,999)
!!    end program demo_scale
!!
!!##LICENSE
!!    Public Domain
subroutine scale(array,axlen,npts,inc)

! ident_27="@(#) M_calcomp scale(3f) calculate scaling factors for producing XY plots with LINE(3f) and AXIS(3f) routines"

!     ARRAY   NAME OF ARRAY CONTAINING VALUES TO BE SCALED.
!     AXLEN   LENGTH IN IN./CM. OVER WHICH ARRAY IS TO BE SCALED.
!     NPTS    NUMBER OF POINTS TO BE SCALED.
!     INC     INCREMENT OF LOCATION OF SUCCESSIVE POINTS.
real    :: array(*),save(7)
real    :: axlen
integer :: npts
integer :: inc
integer :: i
integer :: is
integer :: k
integer :: n
real    :: deltav
real    :: fad
real    :: firstv
real    :: p
real    :: t
real    :: y0
real    :: yn
real    :: ys
   save(1)=1.0
   save(2)=2.0
   save(3)=4.0
   save(4)=5.0
   save(5)=8.0
   save(6)=10.0
   save(7)=20.0
   fad=0.01
   k=abs(inc)
   n=npts*k
   y0=array(1)
   yn=y0
   do  25  i=1,n,k
      ys=array(i)
      if  (y0-ys)  22,22,21
21    y0=ys
      go  to  25
22    if  (ys-yn)  25,25,24
24    yn=ys
25 continue
   firstv=y0
   if  (y0)  34,35,35
34 fad=fad-1.0
35 deltav=(yn-firstv)/axlen
   if(deltav) 70,70,40
40 i=log10(deltav)+1000.0
   p=10.0**(i-1000)
   deltav=deltav/p-0.01
   do  45  i=1,6
      is=i
      if  (save(i)-deltav)  45,50,50
45 continue
50 deltav=save(is)*p
   firstv=deltav*aint(y0/deltav+fad)
   t=firstv+(axlen+0.01)*deltav
   if(t-yn)  55,57,57
55 firstv=p*aint(y0/p+fad)
   t=firstv+(axlen+.01)*deltav
   if(t-yn) 56,57,57
56 is=is+1
   go  to  50
57 firstv=firstv-aint((axlen+(firstv-yn)/deltav)/2.0)*deltav
   if(y0*firstv) 58,58,59
58 firstv=0.0
59 if  (inc) 61,61,65
61 firstv=firstv+aint(axlen+.5)*deltav
   deltav=-deltav
65 n=n+1
   array(n)=firstv
   n=n+k
   array(n)=deltav
   return
70 deltav=2.0*firstv
   deltav=abs(deltav/axlen)+1.0
   goto 40
end subroutine scale
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    symbol(3f) - [M_calcomp:basic] draw text string or marker
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  The "standard" call which produces a line of text is:
!!
!!        subroutine symbol(xpage,ypage,height,ibcd,inteq,angle,+nchar)
!!
!!##DESCRIPTION
!!
!!  The SYMBOL subroutine produces plot annotation at any angle
!!  and in practically any size. There are two SYMBOL call formats:
!!
!!      1) the "standard" call, which can be used to draw text such
!!         as titles, captions, and legends.
!!
!!      2) the "special" call, which is used to draw special
!!         centered symbols such as a box, octagon, triangle, etc., for
!!         denoting data points.
!!
!!  Both forms of the SYMBOL calling sequence have seven arguments.
!!  Which form is being used is determined by the value of the NCHAR
!!  parameter.
!!
!!  The standard characters that are drawn by SYMBOL include the
!!  letters A-Z, digits 0-9, and certain special characters. See Table 2
!!  for a description of CALCOMP's symbol table.
!!
!!  The parameter NCHAR is used to specify whether a text string
!!  or a single symbol is being plotted. If NCHAR is >= 0, the text
!!  string in IBCD is used. If NCHAR= -1 or -2, a single symbol is
!!  produced using the value of INTEQ (Which MUST then be between 0 and
!!  90, inclusive).
!!
!!  It is recommended that 999 be used for INTEQ when NCHAR is greater
!!  than or equal to zero, and either a dummy character variable (e.g.
!!  CHARACTER(len=1) :: DUMMY) or a literal character string (e.g. ' ') be used
!!  for IBCD when NCHAR is less than zero (not just a plain word as a
!!  dummy variable)!
!!
!!##OPTIONS FOR STRINGS
!!
!!     XPAGE, YPAGE  are the coordinates, in inches, of the lower left-hand
!!                   corner (before character rotation) of the first character
!!                   to be produced. The pen is up while moving to this point.
!!
!!                   Annotation may be continued from the position following
!!                   that at which the last annotation ended. Continuation
!!                   occurs when XPAGE and/or YPAGE equals 999.0, and may be
!!                   applied to X or Y independently. (Calling WHERE to
!!                   obtain the current pen position and using RXPAGE, RYPAGE
!!                   in another call to SYMBOL would not give the same results
!!                   as using 999.)
!!
!!     HEIGHT        is the height, in inches, of the character(s) to be
!!                   plotted. For best results, it should be a multiple of
!!                   seven times the standard CALCOMP increment size of 0.01
!!                   (e.g., 0.07, 0.14, 0.21), but other values are
!!                   acceptable. The width of a character, including spacing,
!!                   is normally the same as the height (e.g., a string of 10
!!                   characters 0.14 inch high is 1.4 inches wide).
!!
!!     IBCD          is the text to be used as annotation. The character(s)
!!                   must be in a character array or single variable. (The
!!                   data should be stored as TYPE CHARACTER.)
!!
!!     INTEQ         Ignored (Assuming NCHAR is positive!)
!!
!!     ANGLE         is the angle, in degrees from the X axis, at which the
!!                   annotation is to be plotted. If ANGLE=0.0, the
!!                   character(s) will be plotted right side up and parallel
!!                   to the X axis. The absolute magnitude of ANGLE can not
!!                   exceed 1800 degrees.
!!
!!     +NCHAR        is the number of characters to be plotted from IBCD.
!!                   If NCHAR=0, one alphanumeric character is produced, using
!!                   a single character which is the first element of IBCD.
!!
!!  For example, the following call to SYMBOL will result in the characters TITLE
!!  10 being output beginning at the X and Y coordinates of 1.0 .
!!
!!     character grlbl*8
!!     grlbl='title 10'
!!     call symbol(1.0,1.0,0.14,grlbl,999,0.0,8)
!!
!!##OPTIONS FOR SYMBOLS
!!
!!  A second form is the "special" call, which produces only a single symbol
!!  based on the value of INTEQ - not on the ASCII representation of a character.
!!
!!  The "special" call is:
!!
!!        call symbol(xpage,ypage,height,ibcd,inteq,angle,-nchar)
!!
!!     XPAGE, YPAGE,   are the same as described for the "standard" call. If
!!     and ANGLE       the symbol to be produced is one of the centered symbols
!!                     (e.g., if INTEQ is less than 14), XPAGE, YPAGE represent
!!                     the geometric center of the character produced.
!!
!!     HEIGHT          is the height (and width), in inches, of the centered
!!                     symbol to be drawn. Preferably, it should be a multiple
!!                     of four times the CALCOMP 0.01 increment size.
!!
!!     IBCD            Ignored (assuming NCHAR is negative!)
!!
!!     INTEQ           is the integer equivalent of the desired symbol. Valid
!!                     integers and their symbols are listed in the Symbol Table
!!                     (Table 2). If INTEQ is 0 through 14, a centered symbol
!!                     is produced. INTEQ -MUST- be greater than or equal to zero
!!                     and less than or equal to 90 .
!!
!!     NCHAR           is negative and determines whether the pen is up or down
!!                     during the move to XPAGE, YPAGE.
!!
!!                     When NCHAR is:
!!
!!                     -1, the pen is up during the move, after which a single
!!                     symbol is produced.
!!
!!                     -2, or less, the pen is down during the move, after which
!!                     a single symbol is produced.
!!
!!  For example, the following call to SYMBOL will result in special symbol
!!  number 5 being output with its center at X and Y coordinates of 1.0 .
!!
!!        CALL SYMBOL(1.0,1.0,0.16,' ',5,0.0,-1)
!!
!!  Table 2 shows the current symbols available and the integer equivalents for
!!  each symbol which are used in the "special" call. When a "standard" call to
!!  SYMBOL is made, the host computer's internal characters are translated to the
!!  appropriate characters from this table.
!!
!!##EXAMPLES
!!
!!   Sample program
!!
!!    program demo_symbol
!!    use M_calcomp
!!
!!    ! produce a symbol table which shows the characters
!!    ! available in the symbol(3f) routine.
!!    !
!!    character(len= 38),parameter :: ichr1='CHARACTERS AVAILABLE IN SYMBOL ROUTINE'
!!    character(len= 38),parameter :: ichr2='  FOR CALCOMP ON THE CRAY COMPUTER    '
!!    character(len= 60),parameter :: ichr3='INTEGER FOR USE IN SYMBOL CALL SHOWN TO LEFT OF EACH SYMBOL'
!!    character(len= 1 )           :: ibcd
!!    integer                      :: ia,ib
!!    integer                      :: m
!!    real                         :: z, xs, ys, x, y
!!       call plots(0.0,10.0,0.0,10.0)
!!       call plot(0.8,0.8,1001)
!!       call plot(0.0,11.0,2)
!!       call plot(8.5,11.0,2)
!!       call plot(8.5,0.0,2)
!!       call plot(0.0,0.0,2)
!!       call symbol(0.4,10.50,.2,ichr1,inteq,0.0,38)
!!       call symbol(0.4,10.25,.2,ichr2,inteq,0.0,38)
!!       call plot(8.1,10.0,3)
!!       call plot(0.4,10.0,2)
!!       call plot(0.4, 0.5,2)
!!       call plot(8.1, 0.5,2)
!!       z=0.0
!!       m=0
!!       xs=0.85
!!       ys=0.25
!!       x=0.4
!!       y=9.5
!!       do ia=1,6
!!          do ib=1,15
!!             call number(x+0.10,y+0.18,0.14,z,0.0,-1)
!!             call symbol(x+xs,y+ys, 0.4, ibcd,m,0.0,-1)
!!             Z=Z+1.0
!!             M=M+1
!!             Y=Y-0.6
!!          enddo
!!          if(ia.eq.6) call number(x+0.10,y+0.18,0.14,z,0.0,-1)
!!          if(ia.eq.6) call symbol(x+xs,y+ys,0.4,ibcd,m,0.0,-1)
!!          x=x+1.283
!!          call plot(x,0.5,3)
!!          call plot(x,10.0,2)
!!          y=9.5
!!          xs=0.65
!!          ys=0.05
!!       enddo
!!       call symbol(0.6,0.25,0.12,ichr3,inteq,0.0,60)
!!       call nframe()
!!       call plot(0.0,0.0,999)
!!    end program demo_symbol
!!
!!##LICENSE
!!    Public Domain
subroutine symbol(xpage,ypage,height,string,inteq,angle,nchar)

! ident_28="@(#) M_calcomp symbol(3f) draw text string or marker"

!
!  PROCEDURE DESCRIPTION -
!
!       THIS SUBROUTINE DRAWS THE SYMBOLS DEFINED FOR THE CALCOMP
!       PACKAGE.
!
!       EACH SYMBOL IS DEFINED AS A SERIES OF STRAIGHT LINE SEGMENTS
!       ON A 7 X 7 GRID. THE HEIGHT OF THE SYMBOL AS DEFINED BY THE
!       USER IS, IN REALITY, THE LENGTH OF A SIDE OF THIS DEFINING
!       GRID. THEREFORE, EACH LINE SEGMENT IN THE SYMBOL IS SCALED
!       TO CORRESPOND TO THE SYMBOL SIZE REQUESTED BY THE USER.
!
!       FOR EASE OF DEFINITION, THE SYMBOLS WERE ORIGINALLY
!       CONSTRUCTED IN TERMS OF RECTANGULAR COORDINATES. HOWEVER,
!       THE USER MAY ALSO SPECIFY AN ANGLE OF ORIENTATION FOR THE
!       SYMBOL. TO ACCOMPLISH THIS, EACH SEGMENT IS TRANSFORMED
!       FROM CARTESIAN TO POLAR COORDINATES AND IS THEN ROTATED
!       THROUGH THE SPECIFIED ANGLE. THE TRANSFORMATION FROM
!       RECTANGULAR TO POLAR COORDINATES IS PERFORMED EACH TIME
!       A SYMBOL IS DRAWN, EVEN IF THE DEFAULT ANGLE OF ORIENTATION
!       (0 DEGREES) IS TO BE USED. THIS WAS DONE TO PRESERVE
!       CONSISTENCY FOR THE CONSTRUCTION OF ALL SYMBOLS.
!
!       THE SYMBOL TABLE, CONTAINED IN THE ARRAY 'TABLE', CONTAINS
!       THE DEFINITION OF EACH SYMBOL IN RECTANGULAR COORDINATES.
!       'TABLE' IS CONSTRUCTED IN THE FOLLOWING MANNER:
!
!            THE FIRST ENTRY FOR ANY SYMBOL IS THE INTEGER
!            NUMBER TO WHICH THE SYMBOL IS EQUATED (THIS
!            EQUIVALENCE IS DEFINED IN THE ORIGINAL CALCOMP
!            PACKAGE). THE NEXT ENTRY IS THE NUMBER OF
!            LINE SEGMENTS THAT ARE NEEDED TO DRAW THE SYMBOL.
!            THIS IS FOLLOWED BY 'N' ENTRIES (WHERE 'N' IS
!            THE NUMBER OF SEGMENTS), CONTAINING THE ACTUAL
!            COORDINATE SPECIFICATIONS FOR EACH LINE SEGMENT.
!
!            EXAMPLE:  THE FOLLOWING SHOWS THE DEFINITION OF
!            THE FIRST TWO SYMBOLS IN THE 'TABLE' ARRAY.
!
!            TABLE_Q(1)=O             SYMBOL INTEGER EQUIVALENT
!            TABLE_Q(2)=5             5 LINE SEGMENTS ARE NEEDED
!                                      TO DRAW THIS SYMBOL
!            TABLE_Q(3)=0040          LINE SEGMENT BETWEEN (0,0)
!                                      AND (4,0)
!            TABLE_Q(4)=4044          LINE SEGMENT BETWEEN (4,0)
!                                      AND (4,4)
!            TABLE_Q(5)=4404          LINE SEGMENT BETWEEN (4,4)
!                                      AND (0,4)
!            TABLE_Q(6)=0400          LINE SEGMENT BETWEEN (0,4)
!                                      AND (0,0)
!            TABLE_Q(7)=2224          LINE SEGMENT BETWEEN (2,2)
!                                      AND (2,4)
!            TABLE_Q(8)=1             SYMBOL INTEGER EQUIVALENT
!            TABLE_Q(9)=9             9 LINE SEGMENTS ARE NEEDED
!                                      TO DRAW THIS SYMBOL
!            TABLE_Q(10)=0110         LINE SEGMENT BETWEEN (0,1)
!                                      AND (1,0)
!            TABLE_Q(11)=1030         LINE SEGMENT BETWEEN (1,0)
!                                      AND (3,0)
!            TABLE_Q(12)=3041         LINE SEGMENT BETWEEN (3,0)
!                                      AND (4,1)
!            TABLE_Q(13)=4143         LINE SEGMENT BETWEEN (4,1)
!                                      AND (4,3)
!            TABLE_Q(14)=4334         LINE SEGMENT BETWEEN (4,3)
!                                      AND (3,4)
!            TABLE_Q(15)=3414         LINE SEGMENT BETWEEN (3,4)
!                                      AND (1,4)
!            TABLE_Q(16)=1403         LINE SEGMENT BETWEEN (1,4)
!                                      AND (0,3)
!            TABLE_Q(17)=0301         LINE SEGMENT BETWEEN (0,3)
!                                      AND (0,1)
!            TABLE_Q(18)=2224         LINE SEGMENT BETWEEN (2,2)
!                                      AND (2,4)
!
!       THE POSITION SPECIFIED FOR THE SYMBOL BY THE CALLING
!       ARGUMENTS IS THE LOWER LEFT CORNER OF THIS SYMBOL DEFINITION
!       GRID.
!
!
!  SUBROUTINES CALLED -
!
!       PLOT           CALCOMP ROUTINE; RETURNS THE CURRENTLY DEFINED
!                           ORIGIN FOR THE FRAME
!
!       primitive__draw_line        GRAPHICS PRIMITIVE; DRAWS A STRAIGHT LINE
!                           BETWEEN 2 SPECIFIED POINTS
!
!
!  FUNCTIONS USED -
!
!       ABS            SYSTEM FUNCTION; CALCULATES THE ABSOLUTE
!                           VALUE OF A REAL NUMBER
!
!       ATAN           SYSTEM FUNCTION; CALCULATES INVERSE TANGENT
!
!       COS            SYSTEM FUNCTION; CALCULATES COSINE
!
!       SIN            SYSTEM FUNCTION; CALCULATES SINE
!
!       SQRT           SYSTEM FUNCTION; CALCULATES SQUARE ROOT
!
!
!  CALLING ARGUMENTS -
!
!       ANGLE          ORIENTATION ANGLE FOR SYMBOL IN DEGREES
!
!       HEIGHT         SIZE (HEIGHT) OF SYMBOL TO BE DRAWN
!
!       IBCD           CHARACTER STRING TO BE DRAWN
!
!       INTEQ          INTEGER EQUIVALENT OF SYMBOL TO BE DRAWN
!
!       NCHAR          IF A CHARACTER STRING IS TO BE DRAWN, THE
!                      NUMBER OF CHARACTERS IN THE STRING;
!                      IF THE INTEGER EQUIVALENT OF A SYMBOL
!                      IS SPECIFIED, FLAG TO INDICATE IF THE
!                      PEN IS TO BE "UP" (NCHAR=-1) OR "DOWN"
!                      (NCHAR LESS THAN OR EQUAL -2) WHEN
!                      MOVING TO THE STARTING POINT OF THE
!                      SYMBOL
!
!       XPAGE          X COORDINATE FOR LOWER LEFT CORNER OF
!                      SYMBOL DEFINITION GRID
!
!       YPAGE          Y COORDINATE FOR LOWER LEFT CORNER OF
!                      SYMBOL DEFINITION GRID
!
!
!  VARIABLE NAMES USED -
!
!       ALPHA_Q    ARRAY   CHARACTER EQUIVALENTS USED TO PARSE THE
!                             CHARACTER STRING TO BE DRAWN
!
!       ANGL1            ANGLE FOR POLAR EQUIVALENT OF FIRST
!                             COORDINATE OF LINE SEGMENT
!
!       ANGL2            ANGLE FOR POLAR EQUIVALENT OF SECOND
!                             COORDINATE OF LINE SEGMENT
!
!       ANGLE            *** CALLING ARGUMENT ***
!
!       CENTRX           SYMBOL CENTERING FACTOR (X DIRECTION)
!
!       CENTRY           SYMBOL CENTERING FACTOR (Y DIRECTION)
!
!       HEIGHT           *** CALLING ARGUMENT ***
!
!       I                INTEGER POINTER FOR SYMBOL ARRAYS
!
!       IBCD             *** CALLING ARGUMENT ***
!
!       ICNT             INTEGER COUNTER OF THE NUMBER OF
!                             CHARACTERS DRAWN
!
!       INTEQ            *** CALLING ARGUMENT ***
!
!       ISEG             TEMPORARY STORAGE FOR ELEMENT READ
!                             FROM 'TABLE' ARRAY
!
!       ISYM             INTEGER EQUIVALENT OF SYMBOL TO BE
!                             DRAWN; USED TO LOCATE THE
!                             SYMBOL DEFINITION IN THE SYMBOL
!                             TABLE ARRAY
!
!       J                INTEGER DO LOOP INDEX
!
!       NCHAR            *** CALLING ARGUMENT ***
!
!       NUMCHR           NUMBER OF CHARACTERS TO BE DRAWN;
!                             INCLUDES ADJUSTMENT FOR THE CASE
!                             WHERE 'NCHAR' IS ZERO
!
!       NUMSEG           NUMBER OF SEGMENTS NEEDED TO DRAW THE
!                             SYMBOL; EXTRACTED FROM 'TABLE'
!
!       PI               MATHEMATICAL CONSTANT; DEFINED TO THREE
!                             SIGNIFICANT FIGURES
!
!       RAD              RADIUS FOR TRANSFORMATION FROM
!                             RECTANGULAR TO POLAR COORDINATES
!
!       TABLE    ARRAY   COMPLETE SYMBOL TABLE DEFINITION
!
!       THETA            ANGLE OF SYMBOL ORIENTATION SPECIFIED
!                             BY USER; CONVERTED TO RADIANS
!
!       X1               X COORDINATE OF STARTING POINT OF LINE
!                             SEGMENT
!
!       X2               X COORDINATE OF ENDING POINT OF LINE
!                             SEGMENT
!
!       XFAC             SCALING FACTOR IN X DIRECTION
!
!       XORG             X COORDINATE OF CURRENTLY DEFINED FRAME
!                             ORIGIN
!
!       XPAGE            *** CALLING ARGUMENT ***
!
!       XRLORG           X COORDINATE OF LOWER LEFT CORNER OF
!                             SYMBOL DEFINITION GRID FOR NEXT
!                             SYMBOL WITH RESPECT TO THE CURRENT
!                             SYMBOL
!
!       XSPC_Q             SYMBOL SPACING FOR X COORDINATE
!                        (FEATURE ADDED FOR R. LINCOLN, ORLANDO)
!
!       Y1               Y COORDINATE OF STARTING POINT OF LINE
!                             SEGMENT
!
!       Y2               Y COORDINATE OF ENDING POINT OF LINE
!                             SEGMENT
!
!       YFAC             SCALING FACTOR IN Y DIRECTION
!
!       YORG             Y COORDINATE OF CURRENTLY DEFINED FRAME
!                             ORIGIN
!
!       YPAGE            *** CALLING ARGUMENT ***
!
!       YRLORG           Y COORDINATE OF LOWER LEFT CORNER OF
!                             SYMBOL DEFINITION GRID FOR NEXT
!                             SYMBOL WITH RESPECT TO THE CURRENT
!                             SYMBOL
!
!     YSPC_Q              SYMBOL SPACING FOR Y COORDINATE
!                       (FEATURE ADDED FOR R. LINCOLN, ORLANDO)
!
      character(len=*),intent(in)  :: string
      character(len=:),allocatable :: ibcd
!
! MODE FUNCTIONS ADDED 3/85
!
!  THE ARRAY 'ALPHA_Q' CONTAINS THE CHARACTERS WHICH MAY APPEAR IN THE
!  CHARACTER STRING 'IBCD'.
!
!  THE ARRAY 'TABLE' CONTAINS THE DEFINITION
!  OF THE SYMBOL TABLE IN TERMS OF THE GRID COORDINATES
!  NEEDED TO CONSTRUCT EACH SYMBOL.
!
!  THE VARIABLE 'PI' CONTAINS THE APPROXIMATE VALUE OF THE MATHEMATICAL CONSTANT PI
!
!  INITIALIZE THE POINTER INTO THE CHARACTER STRING (THE VARIABLE
!  'ICNT') TO ZERO. INITIALIZE THE VARIABLE 'ISYM', WHICH CONTAINS
!  THE INTEGER NUMBER ASSIGNED TO THE CURRENT SYMBOL, TO -1 (A
!  NON-EXISTENT SYMBOL). DETERMINE THE NUMBER OF CHARACTERS CONTAINED
!  IN THE ARRAY 'IBCD'; IF THE NUMBER OF CHARACTERS IS ZERO, SET THE
!  NUMBER OF CHARACTERS TO BE PRINTED ('NUMCHR') TO 1 .
!
real    :: angl1
real    :: angl2
real    :: angle
real    :: centrx
real    :: centry
real    :: height
integer :: i
integer :: icnt
integer :: ii
integer :: inteq
integer :: iseg
integer :: isym
integer :: j
integer :: nchar
integer :: numchr
integer :: numseg
real,parameter    :: pi=3.14159265
real    :: rad
real    :: theta
real    :: x1
real    :: x2
real    :: xfac
real    :: xorg
real    :: xpage
real,save    :: xrlorg = 0.0
real    :: y1
real    :: y2
real    :: yfac
real    :: yorg
real    :: ypage
real,save    :: yrlorg = 0.0
      ibcd=upper(string)
      icnt=0
      isym=-1
      numchr=nchar
      call plot(xfac,yfac,1003)
      if(nchar.eq.0) numchr=1
!
!  IF EITHER OF THE CALLING ARGUMENT 'XPAGE' OR 'YPAGE' HAS A
!  VALUE OF 999, THE NEXT SYMBOL IS TO BE APPENDED IN THE SPECIFIED
!  DIRECTION TO THE LAST SYMBOL DRAWN. THE DIRECTION IS SPECIFIED
!  BY WHICH ARGUMENT (EITHER 'XPAGE' OR 'YPAGE') IS SET TO 999. IT
!  IS POSSIBLE TO APPEND IN BOTH DIRECTIONS SIMULTANEOUSLY.
!
!  DETERMINE IF EITHER 'XPAGE' OR 'YPAGE' HAS A VALUE OF 999. IF
!  NOT, REDEFINE THE APPROPRIATE RELATIVE ORIGIN FOR THE SYMBOL
!  TO THE VALUE SPECIFIED.
!
      if(abs(xpage-999.0).gt.0.0001) xrlorg=xpage*xfac
      if(abs(ypage-999.0).gt.0.0001) yrlorg=ypage*yfac
!
! ADDED FUNCTION TO SUPPORT HARDWARE CHARACTERS
!
      if( cttyp_q .eq. 'HARD' .and. nchar .ge. 1)then
         call plot(xorg,yorg,1005)
         call primitive__draw_text(xorg+xrlorg,yorg+yrlorg,ibcd,angle)
         return
      endif
!
!  CALCULATE THE SCALING FACTORS (IN THE X AND Y DIRECTIONS) FOR
!  THE SYMBOL TO BE DRAWN. THE SCALING FACTOR IS BASED ON THE
!  SYMBOL HEIGHT SPECIFIED BY THE USER IN THE CALL TO THE 'SYMBOL'
!  SUBROUTINE AND THE GENERAL SCALING FACTOR CURRENTLY SPECIFIED
!  FOR THE PLOT. THE HEIGHT ENTERED BY THE USER IS DIVIDED BY
!  7.0 TO ACCOUNT FOR THE FACT THAT THE SPECIFIED HEIGHT APPLIES
!  TO THE ENTIRE 7 X 7 GRID FOR WHICH EACH SYMBOL IS DEFINED.
!
      xfac=height/7.0*xfac
      yfac=height/7.0*yfac
!
!  MOVE THE PEN TO THE STARTING POINT FOR THE NEXT SYMBOL. IF THE
!  NUMBER OF CHARACTERS TO BE DRAWN IS LESS THAN OR EQUAL TO -2, A
!  LINE IS TO BE DRAWN FROM THE FINAL POINT OF THE LAST SYMBOL TO THE
!  INITIAL POINT OF THE NEW SYMBOL. THEREFORE, IF NCHAR IS LESS
!  THAN OR EQUAL TO -2, CALL THE 'PLOT' SUBROUTINE WITH THE FINAL
!  ARGUMENT SET TO 2 TO FORCE THE REQUESTED LINE TO BE DRAWN.
!  OTHERWISE, CALL THE 'PLOT' SUBROUTINE WITH THE FINAL ARGUMENT SET
!  TO 3 TO ELEVATE THE PEN AND MOVE TO THE STARTING POINT OF THE NEW
!  SYMBOL. (THE 'PLOT' SUBROUTINE APPLIES ANY REQUIRED
!  SCALING FACTORS.)
!
      if(nchar.le.-2) then
         call plot(xpage,ypage,2)
      else
         call plot(xpage,ypage,3)
      endif
!
!  DETERMINE THE ANGLE AT WHICH THE SYMBOL IS TO BE DRAWN. IF THE
!  ABSOLUTE VALUE OF THE ANGLE IS LESS THAN OR EQUAL TO 1800.0 DEGREES,
!  THE ANGLE IS CONSIDERED TO BE LEGITIMATE. OTHERWISE, THE ANGLE IS
!  SET TO 0 DEGREES. THIS RESTRICTION WAS IMPLEMENTED TO ACCOMMODATE TH
!  FACT THAT THE 'ANGLE' ARGUMENT MAY BE UNINITIALIZED WHEN THE
!  'SYMBOL' SUBROUTINE IS CALLED. THE RESTRICTION WAS CONSTRUCTED
!  TO EXCLUDE BOTH LARGE POSITIVE NUMBERS AND LARGE NEGATIVE
!  NUMBERS SINCE DIFFERENT OPERATING SYSTEMS EMPLOY VARYING
!  METHODS TO INITIALIZE CENTRAL MEMORY.
!
      if(abs(angle).le.1800.0) then
         theta=angle*pi/180.0
      else
         theta=0.0
      endif
!
!  EACH SYMBOL IN THE SYMBOL TABLE HAS BEEN ASSIGNED A UNIQUE
!  INTEGER VALUE BY WHICH THAT SYMBOL MAY BE IDENTIFIED.
!
!  IF NCHAR IS .LT. 0 A SYMBOL IS TO BE PRINTED
!  IF NCHAR IS .GE. 0 THEN PRINT A STRING
      if(nchar.lt.0)then
!     INTEGER EQUIVALENT ARGUMENT ('INTEQ') MUST LIE BETWEEN 0 AND 91 .
!   ? DETERMINE IF THE INTEGER EQUIVALENT ARGUMENT INTEQ HAS A LEGITIMATE VALUE
!
!        SET THE SYMBOL INDICATOR (ISYM) TO THE SYMBOL INTEGER EQUIVALENT
         isym=inteq
!        TRANSFER CONTROL TO THE PORTION OF THE SUBROUTINE WHICH EXTRACTS THE
!        INSTRUCTIONS FOR DRAWING THE SYMBOL
         goto 20
      endif
!
!  IF THIS PORTION OF THE SUBROUTINE IS REACHED, THE SYMBOL TO BE
!  DRAWN HAS BEEN SPECIFIED AS A CHARACTER STRING. INCREMENT THE
!  CHARACTER COUNTER, AND DETERMINE IF THE NUMBER OF CHARACTERS DRAWN
!  HAS EXCEEDED THE NUMBER OF CHARACTERS SPECIFIED WHEN 'SYMBOL' WAS
!  CALLED. IF SO, PREPARE TO EXIT THIS SUBROUTINE. OTHERWISE, COMPARE
!  THE NEXT ELEMENT IN THE CHARACTER STRING WITH THE 'ALPHA_Q' ARRAY TO
!  DETERMINE THE INTEGER EQUIVALENT OF THE NEXT CHARACTER TO BE DRAWN.
!  (NOTE - THE FIRST 27 SYMBOLS IN THE SYMBOL TABLE ARE SPECIAL
!  SYMBOLS. HENCE, THE INTEGER EQUIVALENT OF ANY CHARACTER IS
!  OFFSET FROM ITS POSITION IN 'ALPHA_Q' BY 27.)
!
5     icnt=icnt+1
      if(icnt.gt.numchr) goto 100
! MODIFIED MAX DO INDEX FROM 96 FOR ORLANDO
      do 10 i=1,63
         if(ibcd(icnt:icnt).eq.alpha_q(i)) then
            isym=i+27
            goto 20
         endif
10    continue
      write (6,15)ibcd(icnt:icnt)
      write (6,'(63A1)')(alpha_q(ii),ii=1,63)
15    format(' A MATCH COULD NOT BE FOUND FOR CHARACTER ',a1)
      goto 60
!
!  SEARCH THE 'TABLE' ARRAY FOR THE INTEGER EQUIVALENT OF THE SYMBOL
!  TO BE DRAWN. 'TABLE' IS ARRANGED SUCH THAT THE ENTRY FOLLOWING
!  THE INTEGER EQUIVALENT OF THE SYMBOL IS THE NUMBER OF STRAIGHT
!  LINE SEGMENTS WHICH MAKE UP THE DRAWING. THEREFORE, IF A MATCH
!  LINE SEGMENTS WHICH COMPRISE THE DRAWING. THIS IS FOLLOWED BY
!  A SERIES OF ENTRIES (ONE FOR EACH STRAIGHT LINE SEGMENT) WHICH
!  DESCRIBE THE SYMBOL. HENCE, IF THE CURRENT 'TABLE' ENTRY DOES
!  NOT MATCH THE INTEGER EQUIVALENT OF THE DESIRED SYMBOL, THE
!  POINTER MUST BE INCREMENTED BY THE NUMBER OF SEGMENTS (TABLE_Q(I+1))
!  PLUS 2 (ONE ENTRY FOR THE SYMBOL INTEGER EQUIVALENT AND ONE ENTRY
!  FOR THE DEFINITION OF THE NUMBER OF SEGMENTS).
!
!  WHEN A MATCH FOR THE INTEGER EQUIVALENT OF THE SYMBOL HAS BEEN
!  FOUND, IDENTIFY THE NUMBER OF STRAIGHT LINE SEGMENTS USED TO
!  DRAW THE SYMBOL.
!
20    i=1
30    continue
      if(isym.ne.table_q(i)) then
         i=i+table_q(i+1)+2
! CHANGED 782 TO 699
         if(i.le.699) goto 30
         write(6, '('' HAVE EXHAUSTED SYMBOL TABLE- PROCEEDING TO NEXT SYMBOL'')')
         goto 60
      endif
      numseg=table_q(i+1)
      if(numseg .eq. 0)goto 60
      do 50 j=1,numseg
!
!  DECODE THE NEXT 'TABLE' ENTRY TO DETERMINE THE RELATIVE CARTESIAN
!  COORDINATES (THE END POINTS) FOR THE NEXT LINE OF THE SYMBOL.
!
         iseg=table_q(i+1+j)
         x1=iseg/1000
         y1=(iseg-iseg/1000*1000)/100
         x2=(iseg-iseg/100*100)/10
         y2=iseg-iseg/10*10
!
!  TRANSLATE THE RELATIVE CARTESIAN COORDINATES FOR THE SEGMENT INTO
!  RELATIVE POLAR COORDINATES.
!
         if(abs(x1).lt.0.0001) then
            angl1=pi/2.00
         else
            angl1=atan(y1/x1)
         endif
         if(abs(x2).lt.0.0001) then
            angl2=pi/2.0
         else
            angl2=atan(y2/x2)
         endif
         rad=sqrt(x1*x1+y1*y1)
         x1=rad*xfac*cos(angl1+theta)
         y1=rad*yfac*sin(angl1+theta)
         rad=sqrt(x2*x2+y2*y2)
         x2=rad*xfac*cos(angl2+theta)
         y2=rad*yfac*sin(angl2+theta)
!
!  SET THE X AND Y FACTORS FOR SYMBOL CENTERING TO 0.0. DETERMINE
!  IF THE SYMBOL IS TO BE CENTERED. (ONLY SYMBOLS WITH NUMERICAL
!  EQUIVALENTS BETWEEN 0 AND 14 INCLUSIVE ARE TO BE CENTERED.)  IF
!  THE SYMBOL IS TO BE CENTERED, CALCULATE THE POSITION OF THE
!  ORIGIN OF THE SYMBOL DEFINITION GRID WITH RESPECT TO THE DEFINED
!  CENTER OF THE SYMBOL.
!
         if((inteq.ge.0) .and. (inteq.le.14)) then
            if(nchar.lt.0)then
               centrx=(-2.0)*xfac
               centry=(-2.0)*yfac
            else
               centrx=0.0
               centry=0.0
            endif
         else
            centrx=0.0
            centry=0.0
         endif
!
!  CALL THE 'PLOT' SUBROUTINE TO FIND THE CURRENTLY DEFINED ORIGIN
!  FOR THE ENTIRE FRAME. THEN, CALL THE 'primitive__draw_line' GRAPHICS PRIMITIVE
!  ROUTINE TO DRAW THE LINE SEGMENT. THE ARGUMENTS TO 'primitive__draw_line'
!  MUST INCORPORATE THE CURRENTLY DEFINED ORIGIN FOR THE FRAME  THE
!  (XORG,YORG), THE ORIGIN POINT FOR THE SYMBOL DEFINITION GRID
!  (XRLORG,YRLORG) AND THE SYMBOL CENTERING FACTOR (CENTRX,CENTRY).
!
         call plot(xorg,yorg,1005)
         call primitive__draw_line(x1+xorg+xrlorg+centrx,y1+yorg+yrlorg+centry, x2+xorg+xrlorg+centrx,y2+yorg+yrlorg+centry)
50    continue
!
!  CALCULATE THE STARTING POINT FOR THE NEXT SYMBOL. THE VALUE OF XSPC_Q
!   AND YSPC_Q IN THESE EQUATIONS IS THE WIDTH OF THE GRID ON WHICH EACH
!  SYMBOL IS DEFINED. IT IS NECESSARY TO MOVE THE ENTIRE WIDTH
!  OF THE GRID EACH TIME A NEW SYMBOL IS DRAWN. THE 0.00 IN THESE
!  EQUATIONS IS THE DEFAULT ANGLE OF ORIENTATION FOR DRAWING
!  SYMBOLS. BY DEFAULT, ALL SYMBOLS ARE DRAWN ORIENTED PARALLEL
!  TO THE POSITIVE X AXIS. WHILE THE VALUE OF 0.00 IS NOT REQUIRED
!  MATHEMATICALLY, IT IS INCLUDED HERE FOR CONSISTENCY. (THESE
!  EQUATIONS ARE BASED ON THE PRINCIPLE AS THE EQUATIONS USED ABOVE
!  TO TRANSLATE FROM CARTESIAN TO POLAR COORDINATES. THE INCLUSION
!  OF THE 0.00 FOR THE ANGLE INSURES THAT THE SAME FORMAT IS USED
!  FOR BOTH APPLICATIONS OF THIS TRANSLATION.)
!
! XSPC_Q, YSPC_Q USED INSTEAD OF 7.0 TO ALLOW CHANGE TO SPACING OF
! CHARACTERS. MODIFICATION FROM ORLANDO
!
!
60    xrlorg=xrlorg+(xspc_q*xfac*cos(0.00+theta))
      yrlorg=yrlorg+(yspc_q*yfac*sin(0.00+theta))
!
!  IF 'INTEQ' DOES NOT CONTAIN A VALUE CORRESPONDING TO ONE OF
!  THE DEFINED SYMBOLS, ASSUME THAT A CHARACTER STRING IS BEING
!  DRAWN, AND TRANSFER CONTROL TO INTERPRET THE NEXT ELEMENT OF
!  THE STRING.
!
      if(nchar.gt.0) goto 5
!
!  RESET THE SYMBOL SIZE FACTORS TO UNITY PRIOR TO LEAVING THE
!  SUBROUTINE, AND RETURNS THE PEN TO THE ORIGIN OF THE
!  CHARACTER. (THE 'PLOT' SUBROUTINE APPLIES ANY REQUIRED
!  SCALING FACTORS.)
!
100   xfac=1.0
      yfac=1.0
      call plot(xpage,ypage,3)
end subroutine symbol
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function upper(str,begin,end) result (string)

! ident_29="@(#) M_strings upper(3f) Changes a string to uppercase"

character(*), intent(in)      :: str                 ! inpout string to convert to all uppercase
integer, intent(in), optional :: begin,end
character(len(str))           :: string              ! output string that contains no miniscule letters
integer                       :: i                   ! loop counter
integer                       :: ibegin,iend
integer,parameter             :: diff = iachar('A')-iachar('a')
   string = str                                      ! initialize output string to input string
   ibegin=1
   iend=len_trim(str)

   if (present(begin))then
      ibegin = min(max(ibegin,begin),iend)
   endif

   if (present(end))then
      iend= max(1,min(iend,end))
   endif

   do concurrent (i = ibegin:iend)                   ! step thru each letter in the string in specified range
       select case (str(i:i))
       case ('a':'z')                                ! located miniscule letter
          string(i:i) = char(iachar(str(i:i))+diff)  ! change miniscule letter to uppercase
       end select
   enddo

end function upper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    where(3f) - [M_calcomp:basic] return current position and current plot-scaling factor
!!            (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine where(rxpage,rypage,rfact)
!!
!!##DESCRIPTION
!!
!!  Subroutine WHERE returns the current position coordinates (that are in use by
!!  the PLOT subroutine) and the plot-scaling factor. This permits user-written
!!  subroutines to know the current location for optimizing pen movement or for
!!  determining coordinates for future calls.
!!
!!##OPTIONS
!!
!!    RXPAGE, RYPAGE  are variables that will be set to the current position
!!                    coordinates resulting from the previous call to PLOT
!!                    (which may have been called internally by SYMBOL, NUMBER,
!!                    AXIS, LINE or most other CALCOMP routines).
!!
!!    RFACT           is set to the current plot-sizing factor, i.e., the value
!!                    last supplied by a call to FACTOR or 1.0 if FACTOR has
!!                    not been called.
!!
!!  A call to WHERE made after a call to SYMBOL returns the coordinates of the
!!  location of the last point actually plotted.
!!
!!  Please note that different versions of CALCOMP may return different values to
!!  this routine. For example, CALCOMP has produced CALCOMP libraries that will
!!  return the following to WHERE after a call to SYMBOL has been made:
!!
!!         a) The beginning coordinate of the string produced by SYMBOL
!!         b) The end point of the string produced
!!         c) The position where the next character will be drawn if SYMBOL
!!            is called with the 999 value flags (as described in the SYMBOL
!!            routine description).
!!
!!##LICENSE
!!    Public Domain
subroutine where(xpag,ypag,fct)

! ident_30="@(#) M_calcomp where(3f) return current position and current plot-scaling factor"

real,intent(out) :: xpag
real,intent(out) :: ypag
real,intent(out) :: fct
real             :: x,y
   call plot(x,y,1002)
   xpag=x
   ypag=y
   call plot(x,y,1003)
   fct=x
end subroutine where
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
! set of routines to make simple contour plots
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    cntour(3f) - [M_calcomp:scientific] draw a contour plot
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine cntour(a, NX_Q, NY_Q, x, y, hgt, cv, ncv, legend, ndima)
!!
!!##DESCRIPTION
!!
!!  A contour map is a graphical representation of a three-dimensional surface or
!!  a function of two variables. A contour is defined as the intersection of the
!!  surface or function with a specified plane parallel to the reference plane.
!!  If the surface is denoted by z = f(x,y) where x and y are the coordinate
!!  values in the (x,y)-plane (reference plane), then z equals a constant defines
!!  the plane of intersection.
!!
!!  A contour map consists of a set of contours, usually generated for equally
!!  spaced values of z. In a region where the surface changes rapidly, the
!!  individual contours are close together, and where the surface changes
!!  gradually, they are far apart. Thus, a contour map provides a means of
!!  observing topological behavior of a surface as well as locating regions where
!!  the function z has specific values.
!!
!!  The CNTOUR subroutine and its supporting routines provide the
!!  user with a general-purpose package for preparing contour maps.
!!  The package was developed at the Westinghouse Research Laboratories
!!  and released at the Westinghouse Symposium for general use.
!!
!!##OPTIONS
!!
!!    A         input double-subscripted array containing the discrete
!!              values of the function.
!!    NX_Q        index of the last row of data in A (INTEGER).
!!
!!    NY_Q        index of the last column of data in A (INTEGER).
!!
!!    X         single-scripted array containing values of x
!!              corresponding to row positions of A.
!!
!!    Y         single-subscripted array containing values of y
!!              corresponding to column positions of A.
!!
!!    HGT       height of plot restricted to less than or equal to 6.5
!!              inches (REAL). The width is established accordingly.
!!
!!    CV        single-scripted array of contour values to be plotted.
!!
!!    NCV       number of contour values (INTEGER).
!!
!!    LEGEND    Logical option. If .TRUE., the legend relating contour
!!              values and their identification numbers is printed. If
!!              .FALSE., this legend will not be printed.
!!
!!    NDIMA     the dimensional number of rows for A.
!!
!!   COMMENTS
!!
!!  The CALCOMP initialization (PLOTS(0,0,0)) and termination (PLOT(X,Y,999))
!!  calls must be supplied by the user, external to CNTOUR. These calls were
!!  left out of CNTOUR to allow the user to create multiple plots in a single
!!  program.
!!
!!##LICENSE
!!    Public Domain
subroutine cntour (am,xx,yy,totx,toty,hgt,cv,cvn,tab,ndimyy)

! ident_31="@(#) m_calcomp cntour(3f) draw a contour plot"

integer xx,yy,cvn
character encxde*9
integer :: ndimyy
real am(ndimyy,*),totx(*),toty(*),cv(*),hgt
logical tab
! CONCOM
integer i,it
real rt,tem,dis,scale
real :: bjklx
real :: bjkly
real :: dissav
integer :: inteq
integer :: ipjb
real :: rtx
real :: rty
real :: xdisp
!  *        *NOTE* VARIABLE LIMIT_Q CONTAINS VALUE OF THE SIZE OF VECTOR
!           REC_Q. VALUE IS USED IN SUBROUTINE primitive__NEWONE.
!         HXSZ_Q IS THE VALUE OF THE SIZE OF VECTORS HX_Q, HY_Q, TRK_Q AND TRKA_Q.
!           SIZE EXCEEDED TEST USING HXSZ_Q IS MADE IN SUBROUTINE primitive__ADDREC.
   nx_q=xx
   nxm1_q=nx_q-1
   ny_q=yy
   nym1_q=ny_q-1
   do i=1,nxm1_q
      hx_q(i)=totx(i+1)-totx(i)
   enddo
   do i=1,nym1_q
      hy_q(i)=toty(i+1)-toty(i)
   enddo
   rt=totx(1)
   rtx=rt
   if(rt.eq.0.0) goto 40
   do i=1,nx_q
      totx(i)=totx(i)-rt
   enddo
40 rt=toty(1)
   rty=rt
   if(rt.eq.0.0) goto 60
   do i=1,ny_q
      toty(i)=toty(i)-rt
   enddo
60 if(hgt.gt. 6.5) hgt= 6.5
   if(tab) goto 150
70 ratio_q=hgt/toty(ny_q)
   do ii_q=1,cvn
      cvv_q=cv(ii_q)
      call primitive__scan(am,totx,toty,ndimyy)
   enddo
   call plot(0.0,0.0,-3)
   do i=2,nxm1_q
      bjklx=totx(i)*ratio_q
      call plot(bjklx,0.0,2)
      call plot(bjklx,-0.1,2)
      call plot(bjklx,0.0,2)
   enddo
   bjklx=totx(nx_q)*ratio_q
   call plot(bjklx,0.0,2)
   do i=2,nym1_q
      bjkly=toty(i)*ratio_q
      call plot(bjklx,bjkly,2)
      call plot(bjklx+0.1,bjkly,2)
      call plot(bjklx,bjkly,2)
   enddo
   bjkly=toty(ny_q)*ratio_q
   call plot(bjklx,bjkly,2)
   do ipjb=2,nxm1_q
      i=nxm1_q+2-ipjb
      bjklx=totx(i)*ratio_q
      call plot(bjklx,bjkly,2)
      call plot(bjklx,bjkly+0.1,2)
      call plot(bjklx,bjkly,2)
   enddo
   call plot(0.0,bjkly,2)
   do ipjb=2,nym1_q
      i=nym1_q+2-ipjb
      bjkly=toty(i)*ratio_q
      call plot(0.0,bjkly,2)
      call plot(-0.1,bjkly,2)
      call plot(0.0,bjkly,2)
   enddo
   call plot(0.0,0.0,2)
   do i=1,nx_q
      totx(i)=totx(i)+rtx
   enddo
   do i=1,ny_q
      toty(i)=toty(i)+rty
   enddo
   return
!
!  THE FOLLOWING STATEMENT WAS MODIFIED WHEN THIS SUBROUTINE WAS
!  CONVERTED TO THE CRAY. THE STATEMENT ORIGINALLY READ
!
!     150 DIS=HGT-0.1
!
!  'DIS' WAS SET TO A VALUE OF 8.5 TO FORCE THE LEGEND OF THE
!  PLOT TO THE UPPER LEFT CORNER OF THE DISPLAY AREA.
!
150 dis=8.5
   rt=cv(1)
   do i=2,cvn
      if(abs(rt).lt.abs(cv(i))) rt=cv(i)
   enddo
   it=0
   scale=1.0
170 if(abs(rt).lt.999999.95) goto 180
   scale=scale/10.0
   rt=rt/10.0
   it=it-1
   goto 170
180 if(it.eq.0) goto 190
!
!  THE FOLLOWING CALL TO THE 'SYMBOL' ROUTINE HAS BEEN MODIFIED TO
!  CONVERT THIS SUBROUTINE TO THE CRAY. ON THE CRAY, 'SYMBOL' HAS
!  7 ARGUMENTS; ON THE CDC, 'SYMBOL' HAS SIX ARGUMENTS. THE ADDITIONAL
!  ARGUMENT IN THIS CALL IS 'INTEQ'.
!
!  'INTEQ' IS THE INTEGER EQUIVALENT OF THE SYMBOL TO BE DRAWN.
!  IF 'INTEQ' HAS A VALUE BETWEEN 0 AND 90 INCLUSIVE, A SYMBOL IS DRAWN
!  EVEN IF A TITLE HAS BEEN SPECIFIED. TO PREVENT THIS FROM HAPPENING
!  IN THIS SUBROUTINE, 'INTEQ' IS SET TO THE VALUE 999 (WHICH HAS NO
!  SIGNIFICANCE FOR 'SYMBOL').
!
   inteq=999
   call symbol(0.26,dis,0.14,'SCALE = 10',inteq,0.0,10)
   call number(1.66,dis+0.07,0.07,float(it),0.0,-1)
   dis=dis-0.25
!
!  THE FOLLOWING CALL TO THE 'SYMBOL' ROUTINE HAS BEEN MODIFIED TO
!  CONVERT THIS SUBROUTINE TO THE CRAY. ON THE CRAY, 'SYMBOL' HAS
!  7 ARGUMENTS; ON THE CDC, 'SYMBOL' HAS SIX ARGUMENTS. THE ADDITIONAL
!  ARGUMENT IN THIS CALL IS 'INTEQ'.
!
!  'INTEQ' IS THE INTEGER EQUIVALENT OF THE SYMBOL TO BE DRAWN.
!  IF 'INTEQ' HAS A VALUE BETWEEN 0 AND 90 INCLUSIVE, A SYMBOL IS DRAWN
!  EVEN IF A TITLE HAS BEEN SPECIFIED. TO PREVENT THIS FROM HAPPENING
!  IN THIS SUBROUTINE, 'INTEQ' IS SET TO THE VALUE 999 (WHICH HAS NO
!  SIGNIFICANCE FOR 'SYMBOL').
!
!  A STATEMENT HAS ALSO BEEN INCLUDED TO GENERATE A SECOND
!  COLUMN OF CONTOUR IDENTIFIERS IN THE PLOT LEGEND IF MORE
!  THAN 10 CONTOUR LINES ARE REQUESTED.
!
190 inteq=999
   call symbol(0.0,dis,0.14,'DIGIT      CONTOUR',inteq,0.0,18)
   if(cvn.gt.10) call symbol(2.0,dis,0.14,'DIGIT      CONTOUR',inteq,0.0,18)
   dis = dis - 0.25
!
!  SAVE THE Y COORDINATE OF THE TITLE LINE OF THE LEGEND; IT WILL
!  BE USED TO POSITION THE SECOND COLUMN OF THE LEGENDIF MORE
!  THAN 10 CONTOUR LINES ARE REQUESTED.
!
   dissav=dis
   do 220 i=1,cvn
      rt=1.0
      if(i/10.ne.0) rt=2.0
!
!  SET THE DISPLACEMENT IN THE X DIRECTION FOR THE CONTOUR
!  IDENTIFIERS. BY DEFAULT, THE DISPLACEMENT IS SET FOR
!  THE LEFT COLUMN OF THE LEGEND.
!
      xdisp=1.0
      if(cvn.gt.10) then
         xdisp=3.0
         dis=dissav
      endif
!
!  THE SCALING OF THE FOLLOWING STATEMENT WAS MODIFIED WHEN THIS
!  SUBROUTINE WAS CONVERTED TO THE CRAY. THE STATEMENT ORIGINALLY
!  READ
!
!     CALL NUMBER(0.42-RT,DIS,0.14,FLOAT(I),0.0,-1)
!
!  THE CHARACTER SIZE WAS ADJUSTED TO PERMIT 10 LINES OF INFORMATION
!  TO APPEAR IN THE LEGEND. THE INFLUENCE OF THE DISPLACEMENT
!  FACTOR HAS ALSO BEEN INCLUDED.
!
      call number(0.42-rt+xdisp, dis, 0.09, float(i), 0.0, -1)
      tem=cv(i)*scale
      it=tem+00.5
      rt=3.0
200   it=it/10
      if(it.eq.0) goto 210
      rt=rt+1.0
      goto 200
210   continue
!
!  THE FOLLOWING CALL TO THE 'SYMBOL' ROUTINE HAS BEEN MODIFIED TO
!  CONVERT THIS SUBROUTINE TO THE CRAY. ON THE CRAY, 'SYMBOL' HAS
!  7 ARGUMENTS; ON THE CDC, 'SYMBOL' HAS SIX ARGUMENTS. THE ADDITIONAL
!  ARGUMENT IN THIS CALL IS 'INTEQ'.
!
!  'INTEQ' IS THE INTEGER EQUIVALENT OF THE SYMBOL TO BE DRAWN.
!  IF 'INTEQ' HAS A VALUE BETWEEN 0 AND 90 INCLUSIVE, A SYMBOL IS DRAWN
!  EVEN IF A TITLE HAS BEEN SPECIFIED. TO PREVENT THIS FROM HAPPENING
!  IN THIS SUBROUTINE, 'INTEQ' IS SET TO THE VALUE 999 (WHICH HAS NO
!  SIGNIFICANCE FOR 'SYMBOL').
!
!  THE SCALING IN THE FOLLOWING STATEMENTS WAS ALSO MODIFIED WHEN THIS
!  SUBROUTINE WAS CONVERTED TO THE CRAY. THE STATEMENTS ORIGINALLY
!  READ  (WHERE ENCXDE WAS A TWO-WORD INTEGER ARRAY)
!
!     ENCODE(9,1,ENCXDE) CV(I)
!     CALL SYMBOL(0.9,DIS,0.12,ENCXDE,0.0,9)
!   1 FORMAT(E9.3)
! 220 DIS=DIS-0.25
!
!  THE SIZE WAS ADJUSTED TO PERMIT 10 LINES OF INFORMATION TO APPEAR
!  IN THE LEGEND.
!
      inteq=999
      write(encxde,'(E9.3)')cv(i)
      call symbol(0.7+xdisp, dis, 0.09, encxde,inteq, 0.0, 9)
220 dis = dis - 0.20
!
!  THE FOLLOWING STATEMENT WAS ORIGINALLY USED TO POSITION THE
!  CONTOUR PLOT BESIDE THE LEGEND. IT HAS BEEN "COMMENTED OUT"
!  SO THAT THE CONTOUR PLOT MAY BE POSITIONED BELOW THE LEGEND.
!  THIS ARRANGEMENTS PERMITS LARGER CONTOUR PLOTS.
!
!     CALL PLOT(3.5,0.0,-3)
   goto 70
!
end subroutine cntour
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
logical function primitive__newone (val)
integer :: val
! CONCOM
integer l,m,u,d
   if(nn_q.eq.0) goto 70
   primitive__newone=.false.
   if(alt_q) goto 10
   l=1
   u=nn_q
   goto 20
10 l=nn_q
   u=limit_q
20 d=u-l
   if(val.eq.rec_q(l)) return
   if(d.eq.0) goto 70
   if(val.eq.rec_q(u)) return
   if(d.eq.1) goto 70
30 m=l+d/2
   if(val-rec_q(m)) 40,80,50
40 u=m
   goto 60
50 l=m
60 d=u-l
   if(d.ne.1) goto 30
70 primitive__newone=.true.
80 continue
end function primitive__newone
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine primitive__pass (x,z)
!           MERGE PAIRS OF STRINGS OF LENGTH C_Q FROM X INTO Z
!           WHERE X HAS NTRK_Q ELEMENTS
!           X AND Z ARE PARAMS FOR SWITCHING ARRAYS
   ! CONCOM
integer x(*),z(*)
integer k,p,q,r,s,t
!           K IS INDEX INTO Z
!           S, T ARE INDICES INTO X@  P.LE.S.LT.Q.LE.T.LT.R
!           WITH C_Q.EQ.Q-P AND C_Q.EQ.R-Q
   k=0
   r=1
! STEP THRU X PICKING OFF PAIRS OF C_Q-LENGTH STRINGS
10 p=r
   if(p.gt.ntrk_q) goto 70
!                            ALL DONE
   s=p
   q=p+c_q
   if(q.gt.ntrk_q) goto 60
!                            NO MERGE, JUST TRANSFER THE STRING
   r=q+c_q
   if(r.gt.ntrk_q) r=ntrk_q+1
!                            SHORT STRING TO MERGE
   t=q
! SELECT ELEMENT TO PASS TO Z
20 k=k+1
   if(x(s).gt.x(t)) goto 30
   z(k)=x(s)
   s=s+1
   if(s-q) 20,40,40
30 z(k)=x(t)
   t=t+1
   if(t-r) 20,50,50
! FIRST STRING EMPTY, FLUSH THE SECOND
40 k=k+1
   z(k)=x(t)
   t=t+1
   if(t-r) 40,10,10
! SECOND STRING EMPTY, FLUSH THE FIRST
50 k=k+1
   z(k)=x(s)
   s=s+1
   if(s-q) 50,10,10
! FLUSH END OF X, NO MERGE NEEDED
60 k=k+1
   z(k)=x(s)
   s=s+1
   if(s-ntrk_q) 60,60,70
! ALL DONE
70 return
!
end subroutine primitive__pass
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine primitive__recorx
   ! CONCOM
   oddpas_q=.false.
   c_q=1
10 if(c_q.ge.ntrk_q) goto 20
   call primitive__pass(trk_q,trka_q)
   oddpas_q=.true.
   c_q=c_q+c_q
   if(c_q.ge.ntrk_q) goto 20
   call primitive__pass(trka_q,trk_q)
   oddpas_q=.false.
   c_q=c_q+c_q
   goto 10
20 if(oddpas_q) goto 30
   call primitive__merger(trk_q)
   return
30 call primitive__merger(trka_q)
!
end subroutine primitive__recorx
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine primitive__scan (am,totx,toty,nydim)
integer :: nydim
real    :: am(nydim,*),totx(*),toty(*)
! CONCOM
real    :: op
integer :: jx
integer :: jy
   alt_q=.false.
   edge_q=.true.
   nn_q=0
   ix_q=1
   op=am(1,1)
   do 10 iy_q=2,ny_q
      if(am(1,iy_q).lt.cvv_q.or.op.ge.cvv_q) goto 10
      direc_q=3
      call primitive__tracer(am,totx,toty,nydim)
10 op=am(1,iy_q)
   ix_q=nx_q
   op=am(nx_q,ny_q)
   direc_q=1
   iy_q=ny_q
   do 20 jy=1,nym1_q
      iy_q=iy_q-1
      if(am(nx_q,iy_q).lt.cvv_q.or.op.ge.cvv_q) goto 20
      call primitive__tracer(am,totx,toty,nydim)
20 op=am(nx_q,iy_q)
   iy_q=1
   op=am(nx_q,1)
   direc_q=4
   ix_q=nx_q
   do 30 jx=1,nxm1_q
      ix_q=ix_q-1
      if(am(ix_q,1).lt.cvv_q.or.op.ge.cvv_q) goto 30
      call primitive__tracer(am,totx,toty,nydim)
30 op=am(ix_q,1)
   iy_q=ny_q
   op=am(1,ny_q)
   direc_q=2
   do 40 ix_q=2,nx_q
      if(am(ix_q,ny_q).lt.cvv_q.or.op.ge.cvv_q) goto 40
      call primitive__tracer(am,totx,toty,nydim)
40 op=am(ix_q,ny_q)
   edge_q=.false.
   iy_q=ny_q
   do 50 jy=2,nym1_q
      iy_q=iy_q-1
      op=am(1,iy_q)
      do 50 ix_q=2,nx_q
         if(am(ix_q,iy_q).lt.cvv_q.or.op.ge.cvv_q) goto 50
         if(primitive__newone(ix_q+(iy_q-1)*nx_q))then
            call primitive__tracer(am,totx,toty,nydim)
         endif
50 op=am(ix_q,iy_q)
!
end subroutine primitive__scan
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine primitive__tracer (am,totx,toty,nydim)
integer :: nydim
real    :: am(nydim,*),totx(*),toty(*)
! CONCOM
integer :: z
real    :: tem,plotx,ploty,fplotx,fploty,amc,amw,amz,amq,ams,amd,amp
logical :: first
integer :: s,tix,tiy,cix,ciy,dix,diy,sub,pix,piy,swsd,swmq
real    :: amm
   first=.true.
   ntrk_q=0
   tix=ix_q
   tiy=iy_q
   s=direc_q
   dix=ixd_q(s)
   diy=iyd_q(s)
   cix=tix+dix
   ciy=tiy+diy
10 continue
   tem=am(tix,tiy)
   amc=am(cix,ciy)
   if(dix.ne.0) goto 20
   sub=tiy
   if(diy.lt.0) sub=sub-1
   plotx=totx(tix)
   ploty=toty(tiy)+diy*(tem-cvv_q)*hy_q(sub)/(tem-amc)
   goto 30
20 continue
   sub=tix
   if(dix.lt.0) sub=sub-1
   ploty=toty(tiy)
   plotx=totx(tix)+dix*(tem-cvv_q)*hx_q(sub)/(tem-amc)
30 continue
   if(first) goto 40
   call plot(plotx*ratio_q,ploty*ratio_q,2)
   goto 50
40 continue
   fplotx=plotx*ratio_q
   fploty=ploty*ratio_q
   call number(fplotx+.02, fploty-.15, 0.14, float(ii_q), 0.0, -1)
   call plot(fplotx,fploty,3)
   first=.false.
50 continue
   if(.not.(edge_q)) goto 60
   if(tiy.eq.ny_q.and.s.eq.4.or.tix.eq.1.and.s.eq.1.or.tix.eq.nx_q.and.s .eq.3.or.tiy.eq.1.and.s.eq.2) goto 260
60 continue
   amm=(tem+amc)/2.0
   z=s+1
   if(z.gt.4) z=z-4
   pix=ixd_q(z)
   piy=iyd_q(z)
   amw=am(tix+pix,tiy+piy)
   amz=am(cix+pix,ciy+piy)
   amq=(amw+amz)/2.0
   ams=(tem+amw)/2.0
   amd=(amc+amz)/2.0
   amp=(amm+amq)/2.0
   if(amm.ge.cvv_q) goto 110
   if(ams.lt.cvv_q) goto 260
   if(amp.ge.cvv_q) goto 90
   assign 70 to swsd
   goto 190
70 continue
   if(amw.lt.cvv_q) goto 260
   if(amq.lt.cvv_q) goto 220
   assign 80 to swmq
   goto 160
80 continue
   if(amz.lt.cvv_q) goto 220
   goto 240
90 continue
   assign 100 to swmq
   goto 160
100 continue
   if(amd.ge.cvv_q) goto 240
   assign 80 to swsd
   goto 190
110 continue
   if(amp.ge.cvv_q) goto 140
   assign 120 to swmq
   goto 160
120 continue
   if(ams.lt.cvv_q) goto 260
   assign 130 to swsd
   goto 190
130 continue
   if(amw.lt.cvv_q) goto 260
   goto 220
140 continue
   if(amd.ge.cvv_q) goto 240
   assign 150 to swsd
   goto 190
150 continue
   if(amq.ge.cvv_q) goto 80
   assign 130 to swmq
!      INTERPOLATE MQ
160 continue
   if(pix.ne.0) goto 170
   sub=tiy
   if(piy.lt.0) sub=sub-1
   plotx=(totx(tix)+totx(cix))/2.0
   ploty=toty(tiy)+piy*(amm-cvv_q)*hy_q(sub)/(amm-amq)
   goto 180
170 continue
   sub=tix
   if(pix.lt.0) sub=sub-1
   ploty=(toty(tiy)+toty(ciy))/2.0
   plotx=totx(tix)+pix*(amm-cvv_q)*hx_q(sub)/(amm-amq)
180 continue
   call plot(plotx*ratio_q,ploty*ratio_q,2)
   goto swmq, (80,100,120,130)
!      INTERPOLATE SD
190 continue
   if(dix.ne.0) goto 200
   sub=tiy
   if(diy.lt.0) sub=sub-1
   plotx=(totx(tix)+totx(tix+pix))/2.0
   ploty=toty(tiy)+diy*(ams-cvv_q)*hy_q(sub)/(ams-amd)
   goto 210
200 continue
   sub=tix
   if(dix.lt.0) sub=sub-1
   ploty=(toty(tiy)+toty(tiy+piy))/2.0
   plotx=totx(tix)+dix*(ams-cvv_q)*hx_q(sub)/(ams-amd)
210 continue
   call plot(plotx*ratio_q,ploty*ratio_q,2)
   goto swsd (70,80,130,150)
!      TRANSFER TO W
220 continue
   if(tix.eq.1) goto 230
   if(am(tix-1,tiy).lt.cvv_q) call primitive__addrec(tix+(tiy-1)*nx_q)
230 continue
   tix=tix+pix
   tiy=tiy+piy
   goto 280
!      TRANSFER TO Z
240 continue
   if(tix.eq.1) goto 250
   if(am(tix-1,tiy).lt.cvv_q) call primitive__addrec(tix+(tiy-1)*nx_q)
250 continue
   s=s+3
   tix=cix+pix
   tiy=ciy+piy
   goto 270
!      ROTATE
260 continue
   s=s+1
270 continue
   if(s.gt.4) s=s-4
   dix=ixd_q(s)
   diy=iyd_q(s)
280 continue
   cix=tix+dix
   ciy=tiy+diy
   if(edge_q) goto 290
   if((tix.ne.ix_q).or.(tiy.ne.iy_q).or.(direc_q.ne.s)) goto 300
   call plot(fplotx,fploty,2)
   goto 330
290 continue
   if((cix.lt.1).or.(ciy.lt.1).or.(cix.gt.nx_q).or.(ciy.gt.ny_q)) goto 320
300 continue
   if(am(cix,ciy).lt.cvv_q) goto 10
   if(tix.eq.1) goto 310
   if(am(tix-1,tiy).lt.cvv_q) call primitive__addrec(tix+(tiy-1)*nx_q)
310 continue
   s=s+3
   tix=cix
   tiy=ciy
   goto 270
320 continue
   if(tix.eq.1) goto 330
   if(am(tix-1,tiy).lt.cvv_q) call primitive__addrec(tix+(tiy-1)*nx_q)
330 continue
   call primitive__recorx
!
end subroutine primitive__tracer
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine primitive__addrec (val)
integer,intent(in) ::  val
! CONCOM
   ntrk_q=ntrk_q+1
   if(ntrk_q.gt.hxsz_q) stop 7117
   trk_q(ntrk_q)=val
!
end subroutine primitive__addrec
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine primitive__merger (x)
integer x(*)
! CONCOM
integer :: k,i,j
integer dup
   if(ntrk_q.eq.0) goto 180
   if(alt_q) goto 80
   i=ntrk_q
   k=limit_q+1
   if(nn_q.eq.0) goto 60
   j=nn_q
10 if(x(i)-rec_q(j)) 20,15,30
15 continue
   i=i-1
   if(i) 50,50,20
20 k=k-1
   rec_q(k)=rec_q(j)
   j=j-1
   if(j) 60,60,10
30 k=k-1
   if(k.le.j) stop 7227
   dup=x(i)
   rec_q(k)=dup
40 i=i-1
   if(i.le.0) goto 50
   if(x(i)-dup) 10,40,10
50 k=k-1
   rec_q(k)=rec_q(j)
   j=j-1
   if(j) 170,170,50
60 k=k-1
   if(k.lt.1) stop 7227
   dup=x(i)
   rec_q(k)=dup
70 i=i-1
   if(i.le.0) goto 170
   if(x(i)-dup) 60,70,60
80 i=1
   k=0
   j=nn_q
90 if(x(i)-rec_q(j)) 120,100,110
100 i=i+1
    if(i-ntrk_q) 110,110,140
110 k=k+1
    rec_q(k)=rec_q(j)
    j=j+1
    if(j-limit_q) 90,90,150
120 k=k+1
    if(k.ge.j) stop 7227
    dup=x(i)
    rec_q(k)=dup
130 i=i+1
    if(i.gt.ntrk_q) goto 140
    if(x(i)-dup) 90,130,90
140 k=k+1
    rec_q(k)=rec_q(j)
    j=j+1
    if(j-limit_q) 140,140,170
150 k=k+1
    if(k.gt.limit_q) stop 7227
    dup=x(i)
    rec_q(k)=dup
160 i=i+1
    if(i.gt.ntrk_q) goto 170
    if(x(i)-dup) 150,160,150
170 alt_q=.not.alt_q
    nn_q=k
180 continue
!
end subroutine primitive__merger
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!     This collection of routines takes the WCCS calcomp
!     library and replaces the PDF driver with hooks to call the
!     VOGLE graphics library.
!
!     This is draft version 1.0, John S. Urban
!
!     To do this, it must emulate a subset of the routines
!     primitive__frend   # end a frame, called from plot.f
!     setpar     # here, used to set hardware character size, hard.f
!     primitive__draw_text  # print hardware text, from symbol.f
!     primitive__draw_line  # draw a line segment, plot.f, symbol.f
!     primitive__wpen    # change pen color, newpen.f
!     primitive__width   # line thickness, from width.f
!     primitive__end_plotting  # terminate graphics, plot.f
!     primitive__start_plotting  # initialize graphics, plots.f
!
!     both a single and double precision version were requested.
!
!     The primary platform of interest is SunOS.
!
!     The primary impetus is that Sunnyvale,CA does not have TEMPLATE
!     so the PDF driver is not of much value put they need to run a
!     lot of CALCOMP-based programs from Orlando. Running over the network
!     from Orlando is not acceptable because of TEMPLATE license restrictions
!     and because of network quality issues (speed, reliability, cost).
!     No other library is as easily or cheaply available as the vogle
!     library, which is being maintained for USH and other codes anyway.
!     Development of this library makes it possible for us to quickly
!     resurrect any legacy codes that used CALCOMP on the 6600,7600,COS
!     platforms. Maintenance should be very low, and development time is
!     under a manweek. Sunnyvale does not have any other libraries at
!     this time that would be able to be ported to, and it is likely that
!     a good deal of other programs that use CALCOMP will need run, and
!     this will make porting much easier. A good deal of freedom is
!     available to port to other machines (The vogle library has been run
!     on SunOS, Solaris, ULTRIX, HP-UX and other platforms and is free
!     of external licensing issues).
!
!     Collaboration was obtained from Plimley(CA), Rowe(FL), and Butler
!     at STC. Details will be provided under protest:>.
!
!     so that no call differences are needed between the old 7600 and
!     COS libraries and the SunOS PDF version and this new version,
!     several environment variables must be set.
!
!     CALCOMP_{XMIN,XMAX,YMIN,YMAX} defines the window area to be drawn.
!     M_DRAW_DEVICE selects the output device to usee {X11,psm,psc,hpglland,...}
!     M_DRAW_FONTLIB points to where the "hardware" fonts are.
!     X11 resources can be used to set up the window geometry. The vogle
!     library does not handle window size change events so the window
!     size must not change once the program starts.
!
!     both PDF and this driver could support line thickness,
!     some polygon fill, multiple fonts, more colors and other
!     enhancements put the primary use of this library is to allow the
!     many Westinghouse CALCOMP programs to be easily resurrected and
!     ported.
!
!     major usage difference is that there must be a page size
!     specified with this driver, the pdf driver stuffs the
!     page size back at the beginning by using the maximum point used.
!
!     A major enhancement that could be easily added is the use of backbuffer
!     to do smooth flicker free graphics in X11 .
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine primitive__draw_text(x,y,text,angle)
use M_draw
real,intent(in)             :: x,y      ! REAL4/REAL8
character(len=*),intent(in) :: text
real,intent(in)             :: angle    ! REAL4/REAL8
! intended for hardware text. Simulate for now if angle is not zero
   call move2(real(x),real(y))
   call textang(real(angle))
   call drawstr(text)
end subroutine primitive__draw_text
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine primitive__end_plotting()       ! terminate graphics
use M_draw
   call vexit()                            ! bugger on out of here, terminating graphics
end subroutine primitive__end_plotting
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine primitive__draw_line(x1,y1,x2,y2)     ! draw line from <x1,y1> to <x2,y2>
use M_draw, only : move2, draw2
real,intent(in) :: x1,y1,x2,y2    ! REAL4/REAL8
   call move2(real(x1),real(y1))
   call draw2(real(x2),real(y2))
end subroutine primitive__draw_line
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine primitive__width(iwidth8)
use M_draw
integer :: iwidth8  ! INTEGER4/INTEGER8
   if(iwidth8.ge.0)then
      call linewidth(int(iwidth8))
   endif
end subroutine primitive__width
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine primitive__wpen(ipen8)
use M_draw
integer :: ipen8  ! INTEGER4/INTEGER8
! if negative, set to last color used.
! if positive, set to new color.
   if(ipen8.ge.0)then
      call color(int(ipen8))
   endif
end subroutine primitive__wpen
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine primitive__frend(next8) ! end graphics frame
use M_draw
integer          :: next8  ! INTEGER4/INTEGER8
integer          :: next
character(len=1) :: cjunk
integer          :: ivalue
   next=next8
   call vflush()             ! flush graphics buffers
   ivalue=getkey()           ! wait till a keypress is read in graphics window
   ivalue=max(ivalue,32)     ! convert non-printable characters to a space (including carriage return in particular)
   cjunk=char(ivalue)
   if(next.eq.1)then
      call primitive__clear()
      ! should probably reset to last color used
      ! what is reset? line style, width, text character size, conversion or scaling factors?
   endif
end subroutine primitive__frend
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine primitive__start_plotting(xmin,xmax,ymin,ymax)
use M_strings,  only : string_to_value, v2s
use M_draw
real,intent(in),optional :: xmin, xmax, ymin, ymax
real                     :: xmin_local, xmax_local, ymin_local, ymax_local
character(len=80)        :: string
character(len=20)        :: device
real                     :: wdth
integer                  :: iend
integer                  :: ierr

   if(present(xmin))then; xmin_local=xmin; else; xmin_local= 0.0; endif
   if(present(xmax))then; xmax_local=xmax; else; xmax_local= 8.5; endif
   if(present(ymin))then; ymin_local=ymin; else; ymin_local= 0.0; endif
   if(present(ymax))then; ymax_local=ymax; else; ymin_local=11.0; endif

   call string_to_value(primitive__fgetvar('CALCOMP_XMIN',v2s(xmin_local)),xmin_local,ierr)
   call string_to_value(primitive__fgetvar('CALCOMP_XMAX',v2s(xmax_local)),xmax_local,ierr)
   call string_to_value(primitive__fgetvar('CALCOMP_YMIN',v2s(ymin_local)),ymin_local,ierr)
   call string_to_value(primitive__fgetvar('CALCOMP_YMAX',v2s(ymax_local)),ymax_local,ierr)

   device=primitive__fgetvar('M_DRAW_DEVICE',' ')
   if(device(:3).ne.'X11'.and.device(:3).ne.'tek')then   ! do not open the file for an interactive device
      iend=len_trim(device)                              ! find the last non-blank character in the device name
      string='calcomp.'//device(:iend)                   ! make up an output file name
      string=primitive__fgetvar('M_DRAW_OUTPUT',string)
      call voutput(trim(string))                         ! open output file for a batch output device run
   endif
   call vinit(' ') ! initialize device

   call page(xmin_local,xmax_local,ymin_local,ymax_local)
   maxq=max(abs(ymax_local-ymin_local),abs(xmax_local-xmin_local))
   call fixedwidth(.true.)             ! text is fixed space
   call centertext(.false.)            ! do not center text
   call vsetflush(.false.)             ! turn off automatic flushing
   call vflush()                       ! forces a flush
   call primitive__clear()                        ! as recommended, we clear after initializing graphics
   call font('futura.l')               ! set preferred default font
   call vflush()                       ! forces a flush
   call move(0.0,0.0,0.0)
!   call textsize(xx,yy)
!   call rasters(1)
   call string_to_value(primitive__fgetvar('CALCOMP_WIDTH','20'),wdth,ierr)
   call linewidth(int(wdth))
end subroutine primitive__start_plotting
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine setpar(option,kvalue8)
use m_draw
character(len=4) :: option
integer          :: kvalue8  ! INTEGER4/INTEGER8

integer          :: kvalue
real             :: rasters
real             :: tall
real             :: pctval
   kvalue=kvalue8
   if(option(1:4) .eq. 'TSIZ')then           ! CALCULATE THE SIZE VALUES
      rasters=16.0+kvalue/64.0*(350-16)
      tall=maxq*rasters/16384.0
      call textsize(tall*0.65,tall)
   elseif(option(1:4) .eq. 'INTE')then      ! SET INTENSITY VALUE. RANGE 0 TO 100 PERCENT
      pctval=float(kvalue)/100.0
      if(kvalue .ge. 0 .and. kvalue .le. 100)pctval=3
   endif
end subroutine setpar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    primitive__fgetvar(3fp) - [M_calcomp] return value of an environment variable or a default value
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function primitive__fgetvar(varname,default)
!!
!!    character(len=128) :: primitive__fgetvar
!!    character(len=*),intent(in) :: varname
!!    character(len=*),intent(in) :: default
!!
!!##DESCRIPTION
!!    The primitive__fgetvar(3fp) function returns the value of an environment variable
!!    or a default value if the environment variable is not defined.
!!
!!##OPTIONS
!!     varname     environmental variable name, blank or null terminated.
!!                 string must be left justified (no leading white space).
!!
!!     default     default value for returned value. If varname is not an
!!                 environmental variable name or is blank, this alternate
!!                 string is returned instead.
!!
!!##RESULT
!!     primitive__fgetvar  returned value.
!!                 a character variable of 1 to 128 characters.
!!
!!##EXAMPLE
!!
!!##LICENSE
!!    Public Domain
character(len=80) function primitive__fgetvar(varname,default)
character(len=*),intent(in) :: varname
character(len=*),intent(in) :: default
   call get_environment_variable(varname,primitive__fgetvar)
   if(primitive__fgetvar.eq.' ')primitive__fgetvar=default
end function primitive__fgetvar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine primitive__clear() ! clear graphics area and ensure in graphics mode
use M_draw
   call color(D_BLACK)
   call clear()
   call color(D_WHITE)
   call vflush()
end subroutine primitive__clear
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_calcomp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
