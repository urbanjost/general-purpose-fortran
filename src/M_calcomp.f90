










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
public MOVE, DRAW, END
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
INTEGER,save :: NX_Q,NY_Q,II_Q,IX_Q,IY_Q,NXM1_Q,NYM1_Q
integer,save,target :: NTRK_Q, NN_Q
integer,save,target :: REC_Q(2048)
integer,save :: DIREC_Q,TRK_Q(255),TRKA_Q(255),C_Q
REAL,save    :: RATIO_Q,CVV_Q,HX_Q(255),HY_Q(255)
LOGICAL,save :: EDGE_Q,ALT_Q,ODDPAS_Q
INTEGER,save :: HXSZ_Q = 255
INTEGER,save :: HYSZ_Q = 255
integer,save :: limit_q = 2048
integer,save :: ixd_q(4)=[0,-1,0,1]
integer,save :: iyd_q(4)=[1,0,-1,0]
!
!  INITIALIZE THE SYMBOL TABLE AND OTHER CONSTANTS TO BE USED BY
!  THE SYMBOL ROUTINE
!
!  TABLE CHANGED FROM 596 TO 700 PER REQUEST OF R. LINCOLN, ORLANDO
INTEGER TABLE_Q(700)
CHARACTER(len=1) :: ALPHA_Q(63)
REAL,SAVE :: XSPC_Q=7.0,YSPC_Q=7.0
integer   :: idata

   DATA ALPHA_Q/'A','B','C','D','E','F','G','H','I','J','K','L',           &
   &           'M','N','O','P','Q','R','S','T','U','V','W','X',            &
   &           'Y','Z','0','1','2','3','4','5','6','7','8','9','+',        &
   &           '-','*','/','(',')','$','=',' ',',','.',' ','''','>',       &
   &           ':',' ','?','[','<',' ','!',']',';',' ',' ',' ','\'   /
!
! ADDED > AND < IN LINES 3 AND 4 ABOVE FOR R. LINCOLN
!
   DATA (TABLE_Q(IDATA),IDATA=1,229)/                                    &
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

   DATA (TABLE_Q(IDATA),IDATA=230,448)/                                  &
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

   DATA (TABLE_Q(IDATA),IDATA=449,700)/                                  &
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
CHARACTER(len=4),save :: CTTYP_Q
INTEGER,SAVE          :: KTSIZE_Q

INTEGER,PARAMETER     :: END=999
INTEGER,PARAMETER     :: MOVE=3
INTEGER,PARAMETER     :: DRAW=2
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
implicit none

! ident_1="@(#)M_calcomp::circl(3f): draws an arc or spiral"

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
!!    use M_calcomp
!!    character(len=28) :: ichr1
!!    character(len=26) :: ichr2
!!    character(len=10) :: lbcd1,lbcd2
!!    dimension xarray(62),yarray(62)
!!    ICHR1='PLOTTED ON A CALCOMP PLOTTER'
!!    ICHR2='USING  Y = X -0.7*X +0.1*X'
!!    LBCD1='X-ABSCISSA'
!!    LBCD2='Y-ORDINATE'
!!    ! PLOT GRAPH ILLUSTRATING SCALE, AXIS, AND LINE
!!    deltax=0.04
!!    ! AXIS DRAWS LABELS AS MUCH AS 0.4 INCHES TO THE NEGATIVE OF AXIS CENTER;
!!    ! EITHER USE AN ORIGIN OFFSET OF AT LEAST THIS VALUE OR DO NOT USE AN
!!    ! ORIGIN VALUE OF LESS THAN 0.4 OR CLIPPING WILL OCCUR
!!    call plots(0.0,12.0,0.0,12.0)
!!    call width(0)
!!    call newpen(WHITE)
!!    call rect(0.0,0.0,11.0,10.0,0.0,7)
!!    call plot(0.4,0.4,-MOVE)
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
!!    call newpen(GREEN)
!!    call dashl(xarray(1),yarray(1),60,1)
!!    call newpen(1)
!!    call symbol(1.3,10.,.14,ichr1,inteq,0.0,28)
!!    call symbol(1.3,9.7,.14,ichr2,inteq,0.0,26)
!!    call width(0)
!!    call number(2.98,9.8,.1,2.0,0.,-1)
!!    call number(3.96,9.8,.1,3.0,0.,-1)
!!    call number(4.94,9.8,.1,4.0,0.,-1)
!!    call plot(0.0,0.0,END)
!!    end program demo_dashl
!!##LICENSE
!!   Public Domain
SUBROUTINE DASHL  (X,Y,N,K)
implicit none

! ident_2="@(#)M_calcomp::dashl(3f): draws a polyline with dashed lines"

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
      if(N-1)  90,90,80
80    continue
!     INITIALIZE POINT, MINIMUM AND DELTA INDEXES
      NP=(N-1)*K+1
      NM=NP+K
      ND = NM + K
      NO=1
      KK=K
!     DETERMINE CURRENT PEN POSITION
      CALL WHERE(XN,YN,XX)
!     FIND NEAREST END OF LINE
      DX = MAX(ABS((X( 1)-X(NM))/X(ND)-XN), ABS((Y( 1)-Y(NM))/Y(ND)-YN))
      DY = MAX(ABS((X(NP)-X(NM))/X(ND)-XN), ABS((Y(NP)-Y(NM))/Y(ND)-YN))
      if(DX-DY) 20,20,40
!     REVERSE INCREMENT AND END TEST VALUE
40    NO=NP
      NP=1
      KK=-KK
20    I=NO
!     COMPUTE DELTAS OF INDEXED LINE SEGMENT
30    J=I+KK
      DY =(Y(J) - Y(I) )/Y(ND)
      DX =(X(J) - X(I))/X(ND)
      DS = SQRT(DX*DX+DY*DY+0.000001)
      ID = 5.0 *DS
      if(ID)10,10,11
!     ASSURE DIVISOR NON ZERO
10    ID = 1
!     DERIVE DASH LENGTH.
11    DDS = DS / FLOAT(2*ID+1)
      DY = DDS * DY / DS * Y(ND)
      DX = DDS * DX / DS * X(ND)
!     SET XT/YT TO SEGMENT START POINT
      XT = X(I)
      YT = Y(I)
!     PLOT WITH PEN UP TO XT/YT
1     CALL PLOT((XT-X(NM))/X(ND),(YT-Y(NM))/Y(ND),3)
!     ADJUST XT/YT AND END TEST BY DASH LENGTH
      XT = XT + DX
      YT = YT + DY
      DS = DS - DDS
!     TEST LINE SEGMENT END
      if(DS) 3,3,2
!     PLOT TO XT/YT WITH PEN DOWN
2     CALL PLOT((XT-X(NM))/X(ND),(YT-Y(NM))/Y(ND),2)
!     ADJUST XT/YT AND END TEST BY DASH LENGTH
      XT = XT + DX
      YT = YT + DY
      DS = DS - DDS
!     TEST LINE SEGMENT END
      if(DS) 3,4,1
!     PLOT SEGMENT FINISH POINT \PEN DOWN'
3     CALL PLOT((X(J)-X(NM))/X(ND),(Y(J)-Y(NM))/Y(ND),2)
!     TEST LAST LINE SEGMENT
4     IF(J-NP) 5,90,5
5     I=J
      GOTO 30
90    RETURN
END SUBROUTINE DASHL
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
implicit none

! ident_3="@(#)M_calcomp::dashp(3f): draw from current position to new point with dashed line"

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
   CALL WHERE(XT,YT,ST)
!     COMPUTE DELTAX AND DELTAY
   DX = X-XT
   DY = Y-YT
   DS = DL
   IC = 2
!     DERIVE LINE LENGTH
   S =   SQRT(DX*DX+DY*DY)
   if(S-0.02*ST) 6,10,10
10 continue
   DS = DS/S
!     TEST IF LINE LESS THAN DOUBLE DASH LENGTH
   if(DS-0.5) 2,2,7
!     HALVE DASH LENGTH
7  continue
   DS = 0.5
!     PROPORTION THE DELTAS BY THE LENGTH/DASH RATIO
2  continue
   DX = DX*DS
   DY = DY*DS
!     SET UP ADJUSTMENT AND END OF LINE TEST FROM ABS GREATEST DELTA
   S = DX
   ST = ABS(DX)-ABS(DY)
   if(ST) 3,4,4
3  continue
   S = DY
4  continue
   ST = ABS( S/DS)-ABS( S)
   DS = ABS( S)
!     DASHED LINE LOOP
5  continue
   XT = XT+DX
   YT = YT+DY
   ST = ST-DS
   CALL PLOT(XT,YT,IC)
   IC = 5-IC
   if(ST) 6,6,5
!     LAST SPECIFIC LINE SEGMENT CALL
6  continue
   CALL PLOT(X, Y, IC)
END SUBROUTINE DASHP
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
implicit none

! ident_4="@(#)M_calcomp::elips(3f): draw an elliptical arc"

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
SUBROUTINE FIT (XA,YA,XB,YB,XC,YC)
implicit none

! ident_5="@(#)M_calcomp::fit(3f): draws a semi-hyperbolic curve through three points"

real      :: XA
real      :: YA
real      :: XB
real      :: YB
real      :: XC
real      :: YC
real      :: SS(8,9),THETA(2)
real,save :: A=0.0
real,save :: B=0.0
integer   :: M
real      :: DY
real      :: DX
real      :: Z3
integer   :: I
real      :: c
real      :: d
real      :: dz
real      :: fctr
integer   :: ktra
real      :: x
real      :: y
real      :: z
real      :: z2
      M = 2
      DY = YC - YA
      DX = XC - XA
      Z3 = SQRT( DY**2 + DX**2 )
      if( Z3 ) 20,20,21
21    DO I = 1,2
         IF(ABS(DX)-ABS(DY)) 1,2,2
1        THETA(I) = 1.5708  - ATAN(ABS(DX/DY))
         GOTO 3
2        THETA (I)= ATAN(ABS(DY/DX))
3        if(DX) 25,26,26
25       if(DY) 5,4,4
26       if(DY) 4,5,5
4        THETA(I) = -THETA(I)
5        if(DX) 6,7,7
6        THETA(I) =  THETA(I) + 3.1416
7        DX = XB - XA
         DY = YB - YA
      enddo
      Z2 = SQRT(DY**2 + DX**2)  * COS(THETA(2)-THETA(1))
      if( Z2 ) 20,20,22
22    continue
      SS(1,3) = XA - XC
      SS(2,3) = XA - XB
      KTRA = 1
      GOTO 13
16    continue
      A = SS(1,3)
      B = SS(2,3)
      SS(1,3) = YA - YC
      SS(2,3) = YA - YB
      KTRA = 2
      GOTO 13
17    continue
      CALL WHERE(X,Y,FCTR)
      DZ =0.01 / FCTR
      Z = DZ
      CALL PLOT(XA,YA,3)
      C = SS(1,3)
      D= SS(2,3)
18    continue
      X = (A*Z+B)*Z+XA
      Y = (C*Z+D)*Z+YA
      CALL PLOT(X,Y,2)
      Z = Z + DZ
      if(Z - Z3)18,19,19
19    continue
      CALL PLOT (XC,YC,2)
      RETURN
13    continue
      SS(1,1) = Z3 * Z3
      SS(1,2) = Z3
      SS(2,1) = Z2 * Z2
      SS(2,2) = Z2
      CALL SOLUT(SS,M)
! EARLIER VERSION USED
!     CALL SOLUTN(SS,M)
      if(M)  20,20,14
14    continue
      GOTO (16,17),KTRA
20    continue
      CALL PLOT(XA,YA,3)
      CALL PLOT(XB,YB,2)
      GOTO 19
END SUBROUTINE FIT
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
implicit none

! ident_6="@(#)M_calcomp::grid(3f): draws a linear grid"

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
implicit none

! ident_7="@(#)M_calcomp::poly(3f): draw an equilateral polygon"

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
implicit none

! ident_8="@(#)M_calcomp::rect(3f): draw a rectangle"

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
   THETA = TH/57.2958
   XS = SIN(THETA)
   XC = COS(THETA)
   CALL PLOT(X,Y,IV)
   X1 = X - H * XS
   Y1 = Y + H * XC
   CALL PLOT(X1,Y1,2)
   X1 = X1 + W * XC
   Y1 = Y1 + W * XS
   CALL PLOT(X1,Y1,2)
   X1 =  X + W * XC
   Y1 = Y  + W * XS
   CALL PLOT(X1,Y1,2)
   CALL PLOT(X,Y,2)
END SUBROUTINE RECT
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE SOLUT  (X,N)
implicit none

! EARLIER VERSION OF THIS SUBROUTINE WAS
!     SUBROUTINE  SOLUTN (X,N)
real    ::  X(8,9)
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
      NM1 = N - 1
      NP1 = N + 1
      DO 10 I = 1,NM1
         L = I + 1
         IT = I
         DO IN = L,N
            if(ABS(X(IT,I))-ABS(X(IN,I))) 5,6,6
5           IT = IN
6        CONTINUE
         enddo
         if(X(IT,I)) 8,7,8
7        N = 0
         RETURN
8        if(IT - I) 17,17,16
16       DO IN = I,NP1
            XT = X(I,IN)
            X(I,IN) = X(IT,IN)
            X(IT,IN) = XT
         enddo
17       DO 10 J = L,N
            RATIO = X(J,I)/X(I,I)
            DO 10 K = L,NP1
10    X(J,K) = X(J,K)- RATIO * X(I,K)
      DO 40 I = 1,N
         DX = 0.0
         K = N - I + 1
         if(I-1) 40,40,20
20       continue
         DO J = 2,I
            L = N + 2 - J
            DX = DX + X(K,L) * X(L,NP1)
         enddo
40    X(K,NP1) =(-X(K,NP1) - DX) / X(K,K)
      RETURN
END SUBROUTINE SOLUT
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
SUBROUTINE CURVX  (X0,XF,A,E,B,F,C,G,D,H)
implicit none

! ident_9="@(#)M_calcomp::curvx(3f): plots a function of X over a given range"

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
      DX  =  XF - X0
      I3 = 3
!     DEVELOP FACTORED DELTA
      CALL WHERE(X,Y,XFCT)
      X = X0
      DLT = 0.01/XFCT
!     CHECK LINE LENGTH (IF ZERO RETURN)
      if(DX) 10,20,15
!     IF NEGATIVE MAKE DELTA LIKEWISE
10    continue
      DLT = -DLT
!     COMPUTE NUMBER OF LINE POINTS
15    continue
      N = INT(ABS(DX/DLT) + 1.0)
!     CURVE FITTING PLOT LOOP
      DO IV= 1,N
         Y = A*X**E + B*X**F + C*X**G + D*X**H
         CALL PLOT(X,Y,I3)
         X = X + DLT
         I3 = 2
      enddo
!     PLOT EXPLICIT FINAL POINT AND RETURN
      Y = A*XF**E + B*XF**F + C*XF**G +D*XF**H
      CALL PLOT(XF,Y,2)
20    continue
      RETURN
END SUBROUTINE CURVX
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
SUBROUTINE CURVY  (Y0,YF,A,E,B,F,C,G,D,H)
implicit none

! ident_10="@(#)M_calcomp::curvy(3f): plots a function of Y over a given range"

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
      DY  =  YF - Y0
      I3 = 3
!     DEVELOP FACTORED DELTA
      CALL WHERE(X,Y,XFCT)
      Y = Y0
      DLT = 0.01/XFCT
!     CHECK LINE LENGTH (IF ZERO RETURN)
      if(DY) 10,20,15
!     IF NEGATIVE MAKE DELTA LIKEWISE
10    continue
      DLT = -DLT
!     COMPUTE NUMBER OF LINE POINTS
15    continue
      N = INT(ABS(DY/DLT) + 1.0)
!     CURVE FITTING PLOT LOOP
      DO IV= 1,N
         X = A*Y**E + B*Y**F + C*Y**G + D*Y**H
         CALL PLOT(X,Y,I3)
         Y = Y + DLT
         I3 = 2
      enddo
!     PLOT EXPLICIT FINAL POINT AND RETURN
      X = A*YF**E + B*YF**F + C*YF**G +D*YF**H
      CALL PLOT(X,YF,2)
20    continue
END SUBROUTINE CURVY
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE FIT4(PX1,PY1,PX2,PY2,VECX1,VECY1,VECX3,VECY3)
implicit none

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
      X1=PX1
      Y1=PY1
      CALL WHERE(X,Y,D)
      D = 0.01/D
      if(ABS(X1-X)-D) 10,2,2
10    if(ABS(Y1-Y)-D) 11,2,2
11    if(VECX1-VX2)  5,12,5
12    if(VECY1-VY2)  5,13,5
13    if(VX3-PX2+X1) 5,14,5
14    if(VY3-PY2+Y1) 5, 6,5
2     CALL PLOT(PX1,PY1,3)
5     VX3=PX2-X1
      VY3=PY2-Y1
      VX2=VECX1
      VY2=VECY1
      D2=VX2*VX2+VY2*VY2
      T=1.0
      GOTO 7
6     T=0.0
      VX2=VX3
      VY2=VY3
      VX3=VECX3
      VY3=VECY3
      D2=D3
      UX1=UX2
      UY1=UY2
7     D3=VX3*VX3+VY3*VY3
      UX2=D2*VX3+D3*VX2
      UY2=D2*VY3+D3*VY2
      DV = 1.0/SQRT(UX2*UX2+UY2*UY2+0.00001)
      UX2=DV*UX2
      UY2=DV*UY2
      IF(T)6,8,6
8     D=ABS(UX1*VX2+UY1*VY2)
      D1=D
      UUX1=D*UX1
      UUY1=D*UY1
      D=ABS(UX2*VX2+UY2*VY2)
      UUX2=D*UX2
      UUY2=D*UY2
      D=D+D1
      AX=UUX2+UUX1-VX2-VX2
      BX=VX2-UUX1-AX
      AY=UUY2+UUY1-VY2-VY2
      BY=VY2-UUY1-AY
      N=10.*D+1.0
      D=1.0/FLOAT (N)
      DO I=1,N
         T=T+D
         X=((AX*T+BX)*T+UUX1)*T+X1
         Y=((AY*T+BY)*T+UUY1)*T+Y1
         CALL PLOT(X,Y,2)
      enddo
END SUBROUTINE FIT4
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
SUBROUTINE FLINE (X,Y,NN,K,J,L)
implicit none

! ident_11="@(#)M_calcomp::fline(3f): plot a polyline with optional fit"

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
      CHARACTER(len=8) :: IBCD
      INTEQ = L
      N=ABS(NN)
      KK=K
      LMIN=N*KK+1
      LDX =LMIN+KK
      NL  =LMIN-KK
      XMIN=X(LMIN)
      YMIN=Y(LMIN)
      DX=X(LDX)
      DY=Y(LDX)
      CALL WHERE(XN,YN,DF)
      DF= MAX( ABS((X( 1)-XMIN)/DX-XN), ABS((Y( 1)-YMIN)/DY-YN))
      DL= MAX( ABS((X(NL)-XMIN)/DX-XN), ABS((Y(NL)-YMIN)/DY-YN))
      IC=3
      IS=-1
      NT= ABS(J)
      if(J) 2,1,2
1     continue
      NT=1
2     continue
      if(DF-DL) 3,3,4
3     continue
      NF=1
      NA=NT
      GOTO 5
4     continue
      KK=-KK
      NF=NL
      NL=1
      NA=((N-1)/NT)*NT+NT-N+1
5     continue
      IF  (J) 6,7,8
6     continue
      ICA=3
      ISA=-1
      LSW=1
      GOTO 9
7     continue
      NA=LDX
8     continue
      ICA=2
      ISA=-2
      LSW=0
9     continue
      XN1=(X(NF)-XMIN)/DX
      YN1=(Y(NF)-YMIN)/DY
      NF=NF+KK
      if(NN) 10,10,25
10    continue
      X0=XN1
      Y0=YN1
      LP=NL
      NLP=LP-KK
      NFP = NF-KK
      U = (X(NF)-X(NFP))/DX
      V = (Y(NF)-Y(NFP))/DY
      U1=(X(LP)-X(NLP))/DX
      V1=(Y(LP)-Y(NLP))/DY
      SU=U
      SV=V
      if(X(NFP)-X(LP)) 13,12,13
12    continue
      if(Y(NFP)-Y(LP)) 13,25,13
13    continue
      NFP=NLP-KK
      SU=(X(NLP)-X(NFP))/DX
      SV=(Y(NLP)-Y(NFP))/DY
      CALL REFLX(U1,V1,SU,SV)
      NFP=NF+KK
      U1=(X(NFP)-X(NF))/DX
      V1=(Y(NFP)-Y(NF))/DY
      CALL REFLX(U,V,U1,V1)
25    continue
      DO I=1,N
         XN=XN1
         YN=YN1
         if(N-I) 11,11,14
14       continue
         XN1=(X(NF)-XMIN)/DX
         YN1=(Y(NF)-YMIN)/DY
11       continue
         NW=NA-NT
         if(NW) 29,26,26
29       continue
         if(LSW) 17,26,17
26       continue
         if(NN) 16,24,15
15       continue
         CALL PLOT(XN,YN,IC)
         GOTO 20
16       continue
         if(IC-2) 15,17,15
17       continue
         if(N-I) 27,27,18
27       continue
         U2=SU
         V2=SV
         GOTO 19
18       continue
         U2=XN1-XN
         V2=YN1-YN
19       continue
         CALL FIT4(X0,Y0,XN,YN,U1,V1,U2,V2)
         U1=U
         V1=V
         U=U2
         V=V2
         X0=XN
         Y0=YN
20       continue
         NA=NA+1
         if(NW) 22,28,22
!
!  THE FOLLOWING CALL TO THE 'SYMBOL' ROUTINE HAS BEEN MODIFIED TO
!  CONVERT THIS SUBROUTINE TO THE CRAY. ON THE CRAY, 'SYMBOL' HAS
!  7 ARGUMENTS; ON THE CDC, 'SYMBOL' HAS 6 ARGUMENTS. THE ADDITIONAL
!  ARGUMENT IN THIS CALL IS 'IBCD'.
!
28       continue
         CALL SYMBOL(XN,YN,0.08,IBCD,INTEQ,0.0,-1)
         NA=1
22       continue
         IC=ICA
         NF=NF+KK
      enddo
24    continue
      RETURN
END SUBROUTINE FLINE
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
SUBROUTINE LGAXS(XO,YO,IBCD,N,DIST,THETA,VORG,DELTA)
implicit none

! ident_12="@(#)M_calcomp::lgaxs(3f): draw logarithmic axis"

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
      CHARACTER(len=*) :: IBCD
      SAVE INTEQ,SONT,CIST,D1,D2,D3,D4,D5,D6
      DATA INTEQ/999/,SONT,CIST,D1,D2,D3,D4,D5,D6/8*0.0/
!
!  SAVE ARGUMENTS IN X, Y, NC, SIZE.
      X = XO
      Y = YO
      NC = N
      SIZE = DIST
!  CONVERT DEGREES TO RADIANS, STORING IN TH.
      TH=0.01745329*THETA
!  STORE LOGS OF INTEGERS 2-10
      ETO10 = 0.4342945
      DO I=1,9
         BLOG(I) = ETO10*LOG(FLOAT(I))
      ENDDO
!  SET FXMN TO GREATEST INTEGER POWER OF TEN LESS THAN OR EQUAL TO LOG
!     OF XMIN.
      FXMN = INT (ETO10*LOG(VORG)+100.0001)-100
!  CALCULATE LENGTH FROM BEGINNING OF CYCLE CONTAINING VORG TO BEGINNING
!     OF AXIS, PLUS FACTOR PREVENTING ROUND-OFF ERROR, STORING IN BLMN.
      BLMN = (ETO10*LOG(VORG)-FXMN)/DELTA-0.0001
!  STORE SIN AND COS OF TH.
      SINT=SIN(TH)
      COST=COS(TH)
!  SET OFFSET CONSTANTS OF ANNOTATION, DEPENDING ON SIGN OF NC.
      IF(NC) 20,40,30
20    D1=0.24*SINT
      D2=(-0.24)*COST
      D3=0.12*SINT-D2
      D4=(-0.12)*COST+D1
      D5=0.2*SINT-0.03*COST
      D6=(-0.2)*COST-0.03*SINT
      NC=-NC
      SONT=SINT
      CIST=COST
      BCDX= X +(SIZE-0.12*FLOAT(NC))/2.*COST + 0.48*SINT
      BCDY= Y +(SIZE-0.12*FLOAT(NC))/2.*SINT - 0.48*COST
      GOTO 40
30    D1=(-0.1)*SINT
      D2=0.1*COST
      D3=(-0.22)*SINT+0.24*COST
      SONT=-SINT
      CIST=-COST
      D4=0.22*COST+0.24*SINT
      D5=D1-0.03*COST
      D6=D2-0.03*SINT
      BCDX=X +(SIZE-0.12*FLOAT(NC))/2.*COST-0.34*SINT
      BCDY=Y +(SIZE-0.12*FLOAT(NC))/2.*SINT+0.34*COST
!  CALCULATE COORDINATES OF START OF CYCLE CONTAINING VORG,
!     AND STORE IN X0, Y0 .
40    X0=X -BLMN*COST
      Y0=Y -BLMN*SINT
!  CALCULATE LENGTH OF AXIS PLUS LENGTH OF CYCLE PRECEDING AXIS PLUS
!     ROUND-OFF ERROR FACTOR, AND STORE IN SIZE1 .
      SIZE1=SIZE+BLMN+0.0002
!  INITIALIZE CYCLE COUNTER FJ.
      FJ = 0.0
!  MOVE PEN TO START OF AXIS.
      CALL PLOT(X,Y,3)
!  LOOP THRU CYCLE.
!  AI DETERMINES HEIGHT OF TIC MARK, LARGE TIC MARK FOR 10**N AXIS VALUE
55    AI = 0.14
      DO 60 I=1,9
!  CALCULATE NEW BLEN, LENGTH TO NEXT TIC MARK.
         BLEN = (BLOG(I)+FJ)/DELTA
!  IF TIC MARK IS BEFORE START OF AXIS, GO TO NEXT TIC MARK.
         if(BLEN-BLMN) 60,56,56
!  IF TIC MARK IS BEYOND END OF AXIS, GO TO DRAW LINE TO END OF AXIS.
56       if(BLEN-SIZE1) 57,57,70
!  CALCULATE COORDINATES OF TIC MARK AND PLOT IT.
57       X=X0+BLEN*COST
         Y=Y0+BLEN*SINT
         CALL PLOT(X,Y,2)
         CALL PLOT(X+     AI*SONT,Y-     AI*CIST,2)
         CALL PLOT(X,Y,2)
60    AI = .07
!  INCREMENT FJ TO NEXT CYCLE.
      FJ = FJ+1.0
!  RETURN FOR NEXT CYCLE.
      GOTO 55
!  DRAW LINE TO END OF AXIS.
70    CALL PLOT(X0+SIZE1*COST,Y0+SIZE1*SINT,2)
!  LOOP BACKWARD THRU CYCLE FOR ANNOTATING TIC MARKS.
85    DO 110 K=1,9
         I=10-K
!  CALCULATE DISTANCE FROM START OF FIRST CYCLE TO TIC MARK.
         BLEN = (BLOG(I)+FJ)/DELTA
!  IF TIC MARK IS LOCATED BEFORE START OF AXIS, GO TO DRAW AXIS TITLE.
         if(BLEN-BLMN) 120,86,86
!  IF TIC MARK IS BEYOND END OF AXIS, GO TO NEXT TIC MARK.
86       if(BLEN-SIZE1) 87,87,110
!  IF TIC MARK IS AT INTEGER POWER OF 10, ANNOTATE WITH 10 AND EXPONENT.
87       IF(I-1) 100,90,100
90       CALL NUMBER(X0+BLEN*COST+D1,Y0+BLEN*SINT+D2,0.14,10.0,THETA,-1)
         CALL NUMBER( X0+BLEN*COST+D3,Y0+BLEN*SINT+D4,0.07,FXMN+FJ,THETA,-1)
         GOTO 110
!  IF CYCLE LENGTH IS LESS THAN 2 INCHES, GO TO NEXT TIC MARK.
100      if(DELTA-0.5) 105,105,110
!  ANNOTATE INTERMEDIATE TIC MARK.
105      CALL NUMBER(X0+BLEN*COST+D5,Y0+BLEN*SINT +D6, 0.105, FLOAT(I),THETA,-1)
110   CONTINUE
!  DECREMENT CYCLE COUNTER.
      FJ = FJ-1.0
!  GO TO LOOP THRU NEXT CYCLE.
      GOTO 85
!  TEST FOR ANNOTATING AXIS TITLE.
120   if(NC) 125,130,125
!  DRAW AXIS TITLE.
!
!  THE FOLLOWING CALL TO THE 'SYMBOL' ROUTINE HAS BEEN MODIFIED TO
!  CONVERT THIS SUBROUTINE TO THE CRAY. ON THE CRAY, 'SYMBOL' HAS
!  7 ARGUMENTS; ON THE CDC, 'SYMBOL' HAS 6 ARGUMENTS. THE ADDITIONAL
!  ARGUMENT IN THIS CALL IS 'INTEQ'.
!
125   CALL SYMBOL(BCDX,BCDY,0.14,IBCD,INTEQ,THETA,NC)
130   CONTINUE
END SUBROUTINE LGAXS
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
!     SUBROUTINE LGLINE(XARRA,YARRA,NV,K,JTYPE,NSY,LGTYP)

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
SUBROUTINE LGLIN (XARRA,YARRA,NV,K,JTYPE,NSY,LGTYP)
implicit none

! ident_13="@(#)M_calcomp::lglin(3f): draw polyline in log-log or semi-log mode"

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
CHARACTER(len=8) :: IBCD
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
      INTEQ = NSY
      ETO10 = 0.4342945
      LMN = NV*K+1
      LDX = LMN+K
      NL = LMN-K
!  STORE SCALING FACTORS.
      XMIN = XARRA (LMN)
      DX = XARRA (LDX)
      YMIN = YARRA (LMN)
      DY = YARRA (LDX)
!  STORE COORDINATES OF ENDS OF LINE.
      X1 = XARRA (1)
      X2 = XARRA (NL)
      Y1 = YARRA (1)
      Y2 = YARRA (NL)
!  CONVERT LINEAR TO LOG, DEPENDING ON VALUE OF LOGTYP.
      if(LGTYP) 10,10,20
10    XMIN = ETO10*LOG(XMIN)
      X1 = ETO10*LOG(X1)
      X2 = ETO10*LOG(X2)
20    if(LGTYP) 40,30,30
30    YMIN = ETO10*LOG(YMIN)
      Y1 = ETO10*LOG(Y1)
      Y2 = ETO10*LOG(Y2)
!  LOCATE PEN.
40    CALL WHERE(XN,YN,Z)
!  FIND MAXIMUM OF COORDINATES OF END POINTS OF LINE.
      DF = MAX(ABS((X1-XMIN)/DX-XN),ABS((Y1-YMIN)/DY-YN))
      DL = MAX(ABS((X2-XMIN)/DX-XN),ABS((Y2-YMIN)/DY-YN))
!  SET CONSTANTS FOR POINT PLOT, LINE PLOT, OR LINE AND SYMBOL PLOT.
!        IC  PEN UP-DOWN FOR PLOT.
!        IS  PEN UP-DOWN FOR SYMBOL.
!        NA  STEP FROM 1 TO NT.
!        NT  WHEN NA=NT, USE SYMBOL.
!        NF  SUBSCRIPT OF ARRAY VALUE TO BE PLOTTED.
!        KK  STEP NF FORWARD OR BACKWARD THRU ARRAYS.
!        ICA,ISA  VALUES OF IC AND IS AFTER FIRST POINT PLOTTED.
!        LSW  FLAG TO SKIP PLOT CALL FOR POINT PLOT ONLY.
      IC = 3
      IS = -1
      NT = ABS(JTYPE)
      if(JTYPE) 60,50,60
50    NT = 1
60    if(DF-DL) 80,80,70
70    NF = NL
      NA = ((NV-1)/NT)*NT+NT-(NV-1)
      KK = -K
      GOTO 90
80    NF = 1
      NA = NT
      KK = K
90    if(JTYPE) 100,110,120
100   ICA = 3
      ISA = -1
      LSW = 1
      GOTO 130
110   NA = LDX
120   ICA = 2
      ISA = -2
      LSW = 0
!  BEGIN DO-LOOP FOR PLOTTING.
130   DO 230 I=1,NV
!  STORE COORDINATES.
         XN = XARRA (NF)
         YN = YARRA (NF)
!  CONVERT LINEAR TO LOG DEPENDING ON VALUE OF LOGTYP.
         if(LGTYP)140,140,150
140      XN = ETO10*LOG(XN)
150      if(LGTYP) 170,160,160
160      YN = ETO10*LOG(YN)
!  CALCULATE PAGE COORDINATES OF POINT.
170      XN = (XN-XMIN)/DX
         YN = (YN-YMIN)/DY
!  TEST FOR SYMBOL OR POSSIBLE PLOT CALL.
         if(NA-NT) 180,190,200
!  TEST FOR PLOT OR NO-PLOT.
180      if(LSW) 210,200,210
!
!  THE FOLLOWING CALL TO THE 'SYMBOL' ROUTINE HAS BEEN MODIFIED TO
!  CONVERT THIS SUBROUTINE TO THE CRAY. ON THE CRAY, 'SYMBOL' HAS
!  7 ARGUMENTS; ON THE CDC, 'SYMBOL' HAS 6 ARGUMENTS. THE ADDITIONAL
!  ARGUMENT IN THIS CALL IS 'IBCD'.
!
190      CALL SYMBOL(XN,YN,0.08,IBCD,INTEQ,0.0,IS)
         NA = 1
         GOTO 220
200      CALL PLOT(XN,YN,IC)
!  RESET CONSTANTS.
210      NA = NA+1
220      NF = NF+KK
         IS = ISA
230   IC = ICA
END SUBROUTINE LGLIN
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
SUBROUTINE POLAR(RADAR,ANGAR,NPTS,INC,LTYP,INTEQ,RMAX,DR)
implicit none

! ident_14="@(#)M_calcomp::polar(3f): plot radial values versus angular variables (as polar coordinates)"

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
character(len=8) :: IBCD
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
      INTE = INTEQ
      K = INC
      IND1 = NPTS*K + 1
      IND2 = IND1 + K
      NL = IND1 - K
      if(RMAX) 80,80,10
10    RMAXM = 0.0
      RMINM = 0.0
      DO 50 I = 1,NL,K
         T = RADAR(I)
         if(T-RMAXM) 30,50,20
20       RMAXM = T
         GOTO 50
30       if(RMINM-T) 50,50,40
40       RMINM = T
50    CONTINUE
      if(ABS(RMAXM)-ABS(RMINM)) 60,70,70
60    RMAXM = -RMINM
70    TEMP(1) = 0.0
      TEMP(2) = RMAXM
      CALL SCALE(TEMP,RMAX,2,1)
      DR = TEMP(4)
80    CALL WHERE(RN,THN,R1)
      T = RADAR(1)/DR
      TH1 = ANGAR(1)
      R1 = T*COS(TH1)
      TH1 = T*SIN(TH1)
      DF = ABS(R1-RN)
      R1 = ABS(TH1-THN)
      if(DF-R1) 90,100,100
90    DF = R1
100   T = RADAR(NL)/DR
      TH1 = ANGAR(NL)
      R1 = T*COS(TH1)
      TH1 = T*SIN(TH1)
      DL = ABS(R1-RN)
      R1 = ABS(TH1-THN)
      if(DL-R1) 110,120,120
110   DL = R1
120   IC = 3
      IS = -1
      NT = ABS(LTYP)
      if(NT) 140,130,140
130   NT = 1
140   if(DF-DL) 160,160,150
150   NF = NL
      NA = ((NPTS-1)/NT)*NT + NT - NPTS + 1
      KK = -K
      GOTO 170
160   NF = 1
      NA = NT
      KK = K
170   if(LTYP) 180,190,185
180   ICA = 3
      ISA = -1
      LSW = 1
      GOTO 210
185   IC = 2
      GOTO 200
190   NA = IND2
200   ICA = 2
      ISA = -2
      LSW = 0
210   DO 260 I = 1,NPTS
         TH1 = ANGAR(NF)
         T = RADAR(NF)/DR
         RN = T*COS(TH1)
         THN = T*SIN(TH1)
         if(NA-NT) 220,230,240
220      if(LSW) 250,240,250
!
!  THE FOLLOWING CALL TO THE 'SYMBOL' ROUTINE HAS BEEN MODIFIED TO
!  CONVERT THIS SUBROUTINE TO THE CRAY. ON THE CRAY, 'SYMBOL' HAS
!  7 ARGUMENTS; ON THE CDC, 'SYMBOL' HAS 6 ARGUMENTS. THE ADDITIONAL
!  ARGUMENT IN THIS CALL IS 'IBCD'.
!
230      CALL SYMBOL(RN, THN, 0.08,IBCD, INTE, 0.0, IS)
         NA = 1
         IS = ISA
         GOTO 260
240      CALL PLOT(RN,THN,IC)
         IC = ICA
250      NA = NA + 1
260   NF = NF + KK
END SUBROUTINE POLAR
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE REFLX  (VX1,VY1,VX2,VY2)
implicit none

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
   PS=VY1*VY1
   DS=VX1*VX1
   SS = DS+PS+0.00001
   DS=DS-PS
   PS=2.0*VX1*VY1
   TEMP=(PS*VY2+VX2*DS)/SS
   VY2=(PS*VX2-VY2*DS)/SS
   VX2=TEMP
END SUBROUTINE REFLX
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
implicit none

! ident_15="@(#)M_calcomp::scalg(3f): determine scale factors for a logarithmic scale"

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
implicit none

! ident_16="@(#)M_calcomp::smoot(3f): draw a polyline using modified spline-fitting technique"

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
      DATA NC,ISW,JSW,IPC/4*0/
      DATA X2,Y2,D3,UX1,UY1,UX2,UY2,SX1,SY1,SX2,SY2,SX3,SY3/13*0.0/
      save
      JC=IC
      KC=JC-JC/10*10
      LC=NC-IPC
      PXN=XN
      PYN=YN
      IREP=0
      IF(KC)1,4,14
1     continue
      IF(KC+1)2,5,4
2     continue
      IF(IPC)3,3,14
3     continue
      IF(JC+24)10,10,17
4     continue
      ISW=-1
      GOTO 6
5     continue
      ISW=1
6     continue
      JSW=-1
      NC=(-KC)/10*10
      X3=PXN
      Y3=PYN
      MC=NC+3
9     continue
      IPC=KC
      RETURN
10    continue
      IF(IPC+1)11,13,13
11    continue
      IF(ISW-1)12,15,14
12    continue
      IF(ISW+1)14,16,14
13    continue
      KC=NC+2
      IPC=1
      CALL PLOT(X3,Y3,MC)
14    continue
      CALL PLOT(PXN,PYN,KC)
      RETURN
15    continue
      IREP=2
16    continue
      IREP=IREP+1
      KC=1
17    continue
      IF(ABS(JSW)-1)14,18,14
18    continue
      X1=X2
      Y1=Y2
      X2=X3
      Y2=Y3
      X3=PXN
      Y3=PYN
      IF(IPC+1)20,19,19
19    continue
      VX3=X3-X2
      VY3=Y3-Y2
      D3 = VX3*VX3+VY3*VY3
      SX1=X2
      SX2=X3
      SY1=Y2
      SY2=Y3
      GOTO 40
20    continue
      IF(JSW)21,14,23
21    continue
      IF(ISW)22,14,24
22    continue
      VX2=X3-X2
      VY2=Y3-Y2
      CALL REFLX(VX3,VY3,VX2,VY2)
      D2=VX2*VX2+VY2*VY2
      GOTO 26
23    continue
      JSW=1
24    continue
      VX2=VX3
      VY2=VY3
      VX3=X3-X2
      VY3=Y3-Y2
25    continue
      D2=D3
      UX1=UX2
      UY1=UY2
26    continue
      D3=VX3*VX3+VY3*VY3
      UX2=D2*VX3+D3*VX2
      UY2=D2*VY3+D3*VY2
      DV = 1.0/SQRT(UX2*UX2+UY2*UY2+0.000001)
      UX2=DV*UX2
      UY2=DV*UY2
      IF(ISW-JSW)27,27,45
27    continue
      IF(JSW)23,14,28
28    continue
      T=0.0
      CALL WHERE(X,Y,D)
      if(ABS(X1-X)-0.01*D) 29,30,30
29    continue
      if(ABS(Y1-Y)-0.01*D) 31,30,30
30    continue
      CALL PLOT(X1,Y1,MC)
31    continue
      IF(IPC+3)32,40,32
32    continue
      D=ABS(UX1*VX2+UY1*VY2)
      D1=D
      UUX1=D*UX1
      UUY1=D*UY1
      D=ABS(UX2*VX2+UY2*VY2)
      UUX2=D*UX2
      UUY2=D*UY2
      D=D+D1
      AX=UUX2+UUX1-VX2-VX2
      BX=VX2-UUX1-AX
      AY=UUY2+UUY1-VY2-VY2
      BY=VY2-UUY1-AY
      N=10.0*D+1.0
      D=1./FLOAT (N)
      DO 33 I=1,N
         T=T+D
         X=((AX*T+BX)*T+UUX1)*T+X1
         Y=((AY*T+BY)*T+UUY1)*T+Y1
33    continue
      CALL PLOT(X,Y,LC)
40    continue
      IF(IREP)9,9,41
41    continue
      IREP=IREP-1
      IF(ISW)43,14,42
42    continue
      PXN=SX1
      PYN=SY1
      SX1=SX2
      SY1=SY2
      SX2=SX3
      SY2=SY3
      GOTO 18
43    continue
      CALL REFLX(VX3,VY3,VX2,VY2)
      X=VX3
      Y=VY3
      VX3=VX2
      VY3=VY2
      VX2=X
      VY2=Y
      X1=X2
      Y1=Y2
      GOTO 25
45    continue
      JSW=1
      SX3=X3
      SY3=Y3
      GOTO 40
END SUBROUTINE SMOOT
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
SUBROUTINE AXIS(XPAGE,YPAGE,IBCD,NCHAR,AXLEN,ANGLE,FIRSTV,DELTAV)
implicit none

! ident_17="@(#)M_calcomp::axis(3f): Draw linear axis with numeric scale and axis label"

!          XPAGE,YPAGE  COORDINATES OF STARTING POINT OF AXIS, IN INCHES
!          IBCD         AXIS TITLE.
!          NCHAR        NUMBER OF CHARACTERS IN TITLE. + FOR C.C-W SIDE.
!          AXLEN        FLOATING POINT AXIS LENGTH IN INCHES.
!          ANGLE        ANGLE OF AXIS FROM THE X-DIRECTION, IN DEGREES.
!          FIRSTV       SCALE VALUE AT THE FIRST TIC MARK.
!          DELTAV       CHANGE IN SCALE BETWEEN TIC MARKS ONE INCH APART
CHARACTER(len=*)  ::  IBCD
CHARACTER(len=3)  ::  NBCD
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
      NBCD ='*10'
      KN=NCHAR
      A=1.0
      if(KN) 1,2,2
1     A=-A
      KN=-KN
!     EX IS THE EXPONENT FOR AXIS SCALING
2     EX=0.0
      ADX= ABS  (DELTAV)
      if(ADX) 3,7,3
3     if(ADX- 99.0) 6,4,4
4     ADX=ADX/10.0
      EX=EX+1.0
      GOTO 3
5     ADX=ADX*10.0
      EX=EX-1.0
6     if(ADX-0.01) 5,7,7
7     XVAL=FIRSTV*10.0**(-EX)
      ADX= DELTAV*10.0**(-EX)
      STH=ANGLE*0.0174533
      CTH=COS(STH)
      STH=SIN(STH)
      DXB=-0.1
      DYB=0.15*A-0.05
      XN=XPAGE+DXB*CTH-DYB*STH
      YN=YPAGE+DYB*CTH+DXB*STH
      NTIC=AXLEN+1.0
      NT=NTIC/2
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      DO  20  I=1,NTIC
         CALL NUMBER(XN,YN,0.105,XVAL,ANGLE,2)
         XVAL=XVAL+ADX
         XN=XN+CTH
         YN=YN+STH
         if(NT) 20,11,20
11       Z=KN
         if(EX.NE.0)Z=Z+7.0
         DXB=(-.07)*Z+AXLEN*0.5
         DYB=0.325*A-0.075
         XT=XPAGE+DXB*CTH-DYB*STH
         YT=YPAGE+DYB*CTH+DXB*STH
         CALL SYMBOL(XT,YT,0.14,IBCD,999,ANGLE,KN)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! PUT OUT THE SCALE MULTIPLIER LABEL (*10**EX)
         IF(EX.NE.0)THEN
            Z=KN+2
            XT=XT+Z*CTH*0.14
            YT=YT+Z*STH*0.14
            CALL SYMBOL(XT,YT,0.14, NBCD,999,ANGLE,3)
            XT=XT+(3.0*CTH-0.8*STH)*0.14
            YT=YT+(3.0*STH+0.8*CTH)*0.14
            CALL NUMBER(XT,YT,0.07,EX,ANGLE,-1)
         ENDIF
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
20    NT=NT-1
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      CALL PLOT(XPAGE+AXLEN*CTH,YPAGE+AXLEN*STH,3)
      DXB=(-0.07)*A*STH
      DYB=(+0.07)*A*CTH
      A=NTIC-1
      XN=XPAGE+A*CTH
      YN=YPAGE+A*STH
      DO  30  I=1,NTIC
         CALL PLOT(XN,YN,2)
         CALL PLOT(XN+DXB,YN+DYB,2)
         CALL PLOT(XN,YN,2)
         XN=XN-CTH
         YN=YN-STH
30    CONTINUE
END SUBROUTINE AXIS
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
implicit none

! ident_18="@(#)M_calcomp::factor(3f): rescale entire plot"

real,intent(in) :: fct
   call plot(fct,fct,1001)
end subroutine factor
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE mset(MODE)
implicit none

! ident_19="@(#)M_calcomp::mset(3f): this is a general mode setting routine."

! FUNCTION: SET THE MODE ACCORDING TO THE CHARACTER VALUE PASSED
!           AS MODE. THE MODES ARE SET IN COMMON WMODE
!
! DATE: 3/85
!
   CHARACTER(LEN=*),INTENT(IN) :: MODE
   CHARACTER L_MODE*8
!
   L_MODE=MODE
!
   IF(L_MODE(1:4) .EQ. 'HARD')CTTYP_Q='HARD'
   IF(L_MODE(1:4) .EQ. 'SOFT')CTTYP_Q='SOFT'
!
! SET DISCRETE HARDWARE CHARACTER SIZES
!
   IF(L_MODE(1:4) .EQ. 'XLAR')CALL mpset('TSIZ',61.)
   IF(L_MODE(1:4) .EQ. 'LARG')CALL mpset('TSIZ',56.)
   IF(L_MODE(1:4) .EQ. 'MEDI')CALL mpset('TSIZ',37.)
   IF(L_MODE(1:4) .EQ. 'SMAL')CALL mpset('TSIZ',20.)
!
! ROOM FOR MORE MODES IF NECESSARY
!
END SUBROUTINE mset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE mpset(MODE,VALUE)
implicit none
!
! THIS ROUTINE SETS MODE SPECIFIC PARAMETER VALUES
!
! FUNCTION: EXAMINE THE MODE CHARACTER STRING AND SET THE SPECIFIED
!           VALUE IN COMMON WMODEP ACCORDINGLY.
!
CHARACTER(LEN=*),INTENT(IN) :: MODE
character(len=8)            :: L_MODE
REAL                        :: VALUE
real                        :: V
!
! SET THE CHARACTER SIZE VALUE
!
   L_MODE=MODE
   IF(L_MODE(1:4) .EQ. 'TSIZ')THEN
      V=VALUE
      IF(VALUE .LT. 1. .OR. VALUE .GT. 64.)V=14.0
      KTSIZE_Q=V
!
! OUTPUT THE SIZES TO THE METAFILE
!
      CALL setpar('TSIZ',KTSIZE_Q)
   ENDIF
!
END SUBROUTINE mpset
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
!!    ICHR1='PLOTTED ON A CALCOMP PLOTTER'
!!    ICHR2='USING  Y = X -0.7*X +0.1*X'
!!    LBCD1='X-ABSCISSA'
!!    LBCD2='Y-ORDINATE'
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
SUBROUTINE LINE(XARRAY,YARRAY,NPTS,INC,LINTYP,INTEQ)
implicit none

! ident_20="@(#)M_calcomp::line(3f): Plot a polyline with optional rescaling"

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
   LMIN = NPTS*INC+1
   LDX  = LMIN+INC
   NL   = LMIN-INC
   FIRSTX = XARRAY(LMIN)
   DELTAX = XARRAY(LDX)
   FIRSTY = YARRAY(LMIN)
   DELTAY = YARRAY(LDX)
   CALL WHERE(XN,YN,DF)
   DF=MAX(ABS((XARRAY( 1)-FIRSTX)/DELTAX-XN), ABS((YARRAY( 1)-FIRSTY)/DELTAY-YN) )
   DL=MAX(ABS((XARRAY(NL)-FIRSTX)/DELTAX-XN), ABS((YARRAY(NL)-FIRSTY)/DELTAY-YN) )
   IPEN = 3
   ICODE = -1
   NT =ABS(LINTYP)
   if(LINTYP) 7,6,7
6  NT = 1
7  if(DF-DL) 9,9,8
8  NF = NL
   NA = ((NPTS-1)/NT)*NT+NT-(NPTS-1)
   KK = -INC
   GOTO 10
9  NF = 1
   NA = NT
   KK = INC
10 if(LINTYP) 11,12,13
11 IPENA = 3
   ICODEA = -1
   LSW = 1
   GOTO 15
12 NA=LDX
13 IPENA = 2
   ICODEA = -2
   LSW=0
15 DO 30 I =1,NPTS
      XN = (XARRAY(NF)-FIRSTX)/DELTAX
      YN = (YARRAY(NF)-FIRSTY)/DELTAY
      if(NA-NT) 20,21,22
20    if(LSW) 23,22,23
21    CALL SYMBOL(XN,YN,0.08,' ',INTEQ,0.0,ICODE)
      NA = 1
      GOTO 25
22    CALL PLOT(XN,YN,IPEN)
23    NA = NA + 1
25    NF = NF+KK
      ICODE = ICODEA
30 IPEN = IPENA
END SUBROUTINE LINE
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
implicit none

! ident_21="@(#)M_calcomp::width(3f): select new pen width"

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
implicit none

! ident_22="@(#)M_calcomp::newpen(3f): select new pen color and move to origin"

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
implicit none

! ident_23="@(#)M_calcomp::nframe(3f): start new frame"

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
SUBROUTINE NUMBER(XPAGE,YPAGE,HEIGHT,FPN,ANGLE,NDEC)
implicit none

! ident_24="@(#)M_calcomp::number(3f): plots a floating-point number"

!     XPAGE,YPAGE COORDINATES OF LOWER LEFT CORNER OF NUMBER.
!     HEIGHT   HEIGHT OF PLOTTED NUMBER.
!     FPN      FLOATING POINT NUMBER TO BE PLOTTED.
!     ANGLE    ANGLE AT WHICH NUMBER IS PLOTTED, IN DEGREES.
!     NDEC     NUMBER OF DECIMAL PLACES TO BE DRAWN.
!     THIS VERSION OF NUMBER REQUIRES THE SYMBOL VERSION WITH
!     999. X, Y FEATURE, AND  NC = 0 FEATURE.
CHARACTER(len=20) :: NUM
CHARACTER(len=1)  :: MINUS,IZERO,IPOINT
SAVE MINUS,IZERO,IPOINT
DATA MINUS /'-'/,IZERO/'0'/,IPOINT/'.'/
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
      II=0
      FPV = FPN
      N = NDEC
      MAXN = 9
! ESCC MOD ------------------------
      INTEQ=999
! ---------------------------------------------
      if(N - MAXN) 11, 11, 10
10    N = MAXN
11    if(N + MAXN) 12, 20, 20
12    N = -MAXN
20    if(FPV) 21, 30, 30
21    II=II+1
      NUM(II:II)=MINUS
30    MN = -N
      if(N) 31, 32, 32
31    MN = MN - 1
32    FPV = ABS(FPV) + (0.5 * 10. ** MN)
      I = LOG10(FPV)+1.0
      ILP = I
      if(N + 1)  40, 41, 41
40    ILP = ILP + N + 1
41    if(ILP)  50, 50, 51
50    II=II+1
      NUM(II:II)=IZERO
      GOTO 61
51    if(ILP+N-18) 54,54,52
52    N=-1
      if(ILP-19) 54,54,53
53    ILP=19
54    DO 60 J=1,ILP
         K = FPV * 10. ** (J - I)
         if(K-9) 57,57,55
55       K = 9
57       II=II+1
         KK = ICHAR(IZERO) + K
         NUM(II:II)=CHAR(KK)
         FPV = FPV - (FLOAT(K) * 10. ** (I - J))
60    CONTINUE
61    if(N) 99, 70, 70
70    II=II+1
      NUM(II:II)=IPOINT
      if(N)  99, 99, 80
80    DO 90 J = 1, N
         K = FPV * 10.0
         if(K-9) 84,84,82
82       K = 9
84       II=II+1
         KK = ICHAR(IZERO) + K
         NUM(II:II)=CHAR(KK)
90    FPV = FPV * 10. - FLOAT(K)
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
99    CALL SYMBOL(XPAGE,YPAGE,HEIGHT,NUM,INTEQ,ANGLE,II)
END SUBROUTINE NUMBER
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
SUBROUTINE PLOT(XPAG, YPAG, IPEN)
implicit none

! ident_25="@(#)M_calcomp::plot(3f): move with pen up or down or start new origin or terminate plotting"

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

      if(ABS(IPEN).GE.1000) GOTO 1000
      PENX=XPAG
      PENY=YPAG
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
      X=PENX*DX+XORG
      Y=(PENY+YS)*DY+YORG
!
!  IF THE NEW Y COORDINATE IS BELOW THE BOTTOM EDGE OF THE PLOTTER
!  (I.E., IF Y IS LESS THAN 0.0), SAVE THE NEW Y COORDINATE FOR
!  APPLICATION DURING THE NEXT CALL TO 'PLOT', AND SET THE CURRENT
!  Y COORDINATE TO 0.0 .
!
      if(Y.LT.0.0.and.ipen.gt.0) THEN
         if(IPEN.EQ.3) YS=ABS(PENY)
         Y=0.0
      ENDIF
!
!  IF THE VALUE OF 'IPEN' IS LESS THAN 0 OR IS EQUAL TO 999, THE
!  RELATIVE ORIGIN OF THE PLOT IS TO BE REDEFINED TO BE THE NEW
!  X AND Y COORDINATES. RESET THE RELATIVE ORIGIN (XORG,YORG)
!  TO THE NEW X AND Y VALUES, THEN SET ANY RESIDUAL Y ADJUSTMENT
!  TO 0.0 .
!
      if((IPEN.LT.0) .OR. (IPEN.EQ.999)) THEN
         XORG=X
         YORG=Y
         YS=0.0
      ENDIF
!
!  IF 'IPEN' IS SPECIFIED WITH A VALUE OF 999, THE PLOT IS TO BE
!  TERMINATED. IF THE CURRENT FRAME HAS NOT BEEN ENDED, CALL THE
!  'primitive__frend' GRAPHICS PRIMITIVE. CALL THE 'primitive__end_plotting'
!  GRAPHICS PRIMITIVE TO TERMINATE PLOTTING.
!
      if(IPEN.EQ.999) THEN
         if(NOEND) CALL primitive__frend(0)
         CALL primitive__end_plotting
      ENDIF
!
!  IF THE ABSOLUTE VALUE OF 'IPEN' IS 2, THE PEN IS TO BE DOWN DURING
!  THE MOVE TO THE NEW COORDINATES; THEREFORE, CALL THE 'primitive__draw_line'
!  GRAPHICS PRIMITIVE TO DRAW THE DESIRED LINE.
!
      if(ABS(IPEN).EQ.2) CALL primitive__draw_line(XLAST+XOFF,YLAST,X+XOFF,Y)
!
!  SAVE THE CURRENT PEN COORDINATES.
!
      XLAST=X
      YLAST=Y
      NOEND=.TRUE.
      RETURN
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
      DX=XPAG
      DY=YPAG
!-----------------------------------------------------------------------------------------------------------------------------------
      case(1002) !  SPECIAL ENTRY FOR CALL FROM SUBROUTINE 'WHERE'. 'WHERE' RETURNS
                 !  THE CURRENT PEN COORDINATES AND SCALING FACTOR FOR USE IN USER WRITTEN SUBROUTINES.
      XPAG=PENX
      YPAG=PENY
!-----------------------------------------------------------------------------------------------------------------------------------
      case(1003) !  SPECIAL ENTRY FOR CALL FROM SUBROUTINE 'SYMBOL'. THIS CALL RETURNS THE CURRENT SCALING FACTOR FOR THE PLOT.
      XPAG=DX
      YPAG=DY
!-----------------------------------------------------------------------------------------------------------------------------------
      case(1004) !  SPECIAL ENTRY FOR CALL FROM SUBROUTINE 'NEWPEN'. 'NEWPEN' INSURES THAT THE PEN IS MOVED TO THE ORIGIN (0.0,0.0)
      CALL primitive__draw_line(0.0+XOFF,0.0,0.0+XOFF,0.0)
!-----------------------------------------------------------------------------------------------------------------------------------
      case(1005) !  SPECIAL ENTRY FOR CALL FROM SUBROUTINE 'SYMBOL'. THIS CALL RETURNS THE CURRENT ORIGIN.
      XPAG=XORG
      YPAG=YORG
!-----------------------------------------------------------------------------------------------------------------------------------
      case(1006) !  SPECIAL ENTRY FOR CALL FROM CALCOMP VERSION OF SUBROUTINE 'SYMBOL'. THIS CALL ADVANCES THE PEN POSITION.
      XLAST=XLAST+XPAG
      YLAST=YLAST+YPAG
!-----------------------------------------------------------------------------------------------------------------------------------
      case(1007) !  SPECIAL ENTRY FOR CALL FROM SUBROUTINE 'PLOTS'. THIS CALL INSURES THAT THE PEN IS POSITIONED AT THE
                 ! ORIGIN (0.0,0.0) WHEN THE PLOT PACKAGE IS INITIALIZED.
      CALL primitive__draw_line(0.0+XOFF,0.0,0.0+XOFF,0.0)
!-----------------------------------------------------------------------------------------------------------------------------------
      case(1008) !  SPECIAL ENTRY FOR CALL FROM ESCC SUBROUTINE 'NFRAME'. THE PLOTTING
                 !  OF THE FINAL POINT OF THE GRAPH IS INSURED, THEN 'primitive__frend' IS CALLED
                 !  TO PLACE THE END-OF-FRAME MARK IN THE METALANGUAGE FILE. VARIABLES
                 !  USED BY THE 'PLOT' SUBROUTINE ARE RE-INITIALIZED, AND THE PEN IS
                 !  MOVED TO THE 0.0,0.0 POINT.
      CALL primitive__draw_line(XLAST+XOFF,YLAST,XLAST+XOFF,YLAST)
      CALL primitive__frend(1)
      XORG=0.0
      YORG=0.0
      XLAST=0.0
      YLAST=0.0
      PENX=0.0
      PENY=0.0
      YS=0.0
      NOEND=.FALSE.
      CALL primitive__draw_line(0.0+XOFF,0.0,0.0+XOFF,0.0)
!-----------------------------------------------------------------------------------------------------------------------------------
      case default
      write(*,*)'*plot* ERROR: unknown action value=',ipen
!-----------------------------------------------------------------------------------------------------------------------------------
      end select
END SUBROUTINE PLOT
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
SUBROUTINE PLOTS(xmin,xmax,ymin,ymax)
implicit none

! ident_26="@(#)M_calcomp::plots(3f): initialize the CALCOMP package"

real,intent(in) :: xmin, xmax, ymin, ymax
!
! MODIFIED 3/85 TO ADD COMMONS WMODE AND WMODEP
! TO INITIALIZE TEXT MODE AND SIZE
!
      CTTYP_Q='SOFT'
      KTSIZE_Q=37
!
! 'primitive__start_plotting' INITIALIZES THE METALANGUAGE ELEMENTS.
      CALL primitive__start_plotting(xmin,xmax,ymin,ymax)
      CALL PLOT(0.0,0.0,1007)
      CALL mpset('TSIZ',FLOAT(KTSIZE_Q))

END SUBROUTINE PLOTS
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
SUBROUTINE SCALE(ARRAY,AXLEN,NPTS,INC)
implicit none

! ident_27="@(#)M_calcomp::scale(3f): calculate scaling factors for producing XY plots with LINE(3f) and AXIS(3f) routines"

!     ARRAY   NAME OF ARRAY CONTAINING VALUES TO BE SCALED.
!     AXLEN   LENGTH IN IN./CM. OVER WHICH ARRAY IS TO BE SCALED.
!     NPTS    NUMBER OF POINTS TO BE SCALED.
!     INC     INCREMENT OF LOCATION OF SUCCESSIVE POINTS.
real    :: ARRAY(*),SAVE(7)
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
   SAVE(1)=1.0
   SAVE(2)=2.0
   SAVE(3)=4.0
   SAVE(4)=5.0
   SAVE(5)=8.0
   SAVE(6)=10.0
   SAVE(7)=20.0
   FAD=0.01
   K=ABS(INC)
   N=NPTS*K
   Y0=ARRAY(1)
   YN=Y0
   DO  25  I=1,N,K
      YS=ARRAY(I)
      IF  (Y0-YS)  22,22,21
21    Y0=YS
      GO  TO  25
22    IF  (YS-YN)  25,25,24
24    YN=YS
25 CONTINUE
   FIRSTV=Y0
   IF  (Y0)  34,35,35
34 FAD=FAD-1.0
35 DELTAV=(YN-FIRSTV)/AXLEN
   if(DELTAV) 70,70,40
40 I=LOG10(DELTAV)+1000.0
   P=10.0**(I-1000)
   DELTAV=DELTAV/P-0.01
   DO  45  I=1,6
      IS=I
      IF  (SAVE(I)-DELTAV)  45,50,50
45 CONTINUE
50 DELTAV=SAVE(IS)*P
   FIRSTV=DELTAV*AINT(Y0/DELTAV+FAD)
   T=FIRSTV+(AXLEN+0.01)*DELTAV
   if(T-YN)  55,57,57
55 FIRSTV=P*AINT(Y0/P+FAD)
   T=FIRSTV+(AXLEN+.01)*DELTAV
   if(T-YN) 56,57,57
56 IS=IS+1
   GO  TO  50
57 FIRSTV=FIRSTV-AINT((AXLEN+(FIRSTV-YN)/DELTAV)/2.0)*DELTAV
   if(Y0*FIRSTV) 58,58,59
58 FIRSTV=0.0
59 IF  (INC) 61,61,65
61 FIRSTV=FIRSTV+AINT(AXLEN+.5)*DELTAV
   DELTAV=-DELTAV
65 N=N+1
   ARRAY(N)=FIRSTV
   N=N+K
   ARRAY(N)=DELTAV
   RETURN
70 DELTAV=2.0*FIRSTV
   DELTAV=ABS(DELTAV/AXLEN)+1.0
   GOTO 40
END SUBROUTINE SCALE
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
SUBROUTINE SYMBOL(XPAGE,YPAGE,HEIGHT,STRING,INTEQ,ANGLE,NCHAR)
use M_strings, only : upper
implicit none

! ident_28="@(#)M_calcomp::symbol(3f): draw text string or marker"

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
      IBCD=upper(STRING)
      ICNT=0
      ISYM=-1
      NUMCHR=NCHAR
      CALL PLOT(XFAC,YFAC,1003)
      if(NCHAR.EQ.0) NUMCHR=1
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
      if(ABS(XPAGE-999.0).GT.0.0001) XRLORG=XPAGE*XFAC
      if(ABS(YPAGE-999.0).GT.0.0001) YRLORG=YPAGE*YFAC
!
! ADDED FUNCTION TO SUPPORT HARDWARE CHARACTERS
!
      IF( CTTYP_Q .EQ. 'HARD' .AND. NCHAR .GE. 1)THEN
         CALL PLOT(XORG,YORG,1005)
         CALL primitive__draw_text(XORG+XRLORG,YORG+YRLORG,IBCD,ANGLE)
         RETURN
      ENDIF
!
!  CALCULATE THE SCALING FACTORS (IN THE X AND Y DIRECTIONS) FOR
!  THE SYMBOL TO BE DRAWN. THE SCALING FACTOR IS BASED ON THE
!  SYMBOL HEIGHT SPECIFIED BY THE USER IN THE CALL TO THE 'SYMBOL'
!  SUBROUTINE AND THE GENERAL SCALING FACTOR CURRENTLY SPECIFIED
!  FOR THE PLOT. THE HEIGHT ENTERED BY THE USER IS DIVIDED BY
!  7.0 TO ACCOUNT FOR THE FACT THAT THE SPECIFIED HEIGHT APPLIES
!  TO THE ENTIRE 7 X 7 GRID FOR WHICH EACH SYMBOL IS DEFINED.
!
      XFAC=HEIGHT/7.0*XFAC
      YFAC=HEIGHT/7.0*YFAC
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
      if(NCHAR.LE.-2) THEN
         CALL PLOT(XPAGE,YPAGE,2)
      ELSE
         CALL PLOT(XPAGE,YPAGE,3)
      ENDIF
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
      if(ABS(ANGLE).LE.1800.0) THEN
         THETA=ANGLE*PI/180.0
      ELSE
         THETA=0.0
      ENDIF
!
!  EACH SYMBOL IN THE SYMBOL TABLE HAS BEEN ASSIGNED A UNIQUE
!  INTEGER VALUE BY WHICH THAT SYMBOL MAY BE IDENTIFIED.
!
!  IF NCHAR IS .LT. 0 A SYMBOL IS TO BE PRINTED
!  IF NCHAR IS .GE. 0 THEN PRINT A STRING
      IF(NCHAR.LT.0)THEN
!     INTEGER EQUIVALENT ARGUMENT ('INTEQ') MUST LIE BETWEEN 0 AND 91 .
!   ? DETERMINE IF THE INTEGER EQUIVALENT ARGUMENT INTEQ HAS A LEGITIMATE VALUE
!
!        SET THE SYMBOL INDICATOR (ISYM) TO THE SYMBOL INTEGER EQUIVALENT
         ISYM=INTEQ
!        TRANSFER CONTROL TO THE PORTION OF THE SUBROUTINE WHICH EXTRACTS THE
!        INSTRUCTIONS FOR DRAWING THE SYMBOL
         GOTO 20
      ENDIF
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
5     ICNT=ICNT+1
      if(ICNT.GT.NUMCHR) GOTO 100
! MODIFIED MAX DO INDEX FROM 96 FOR ORLANDO
      DO 10 I=1,63
         if(IBCD(ICNT:ICNT).EQ.ALPHA_Q(I)) THEN
            ISYM=I+27
            GOTO 20
         ENDIF
10    CONTINUE
      WRITE (6,15)IBCD(ICNT:ICNT)
      WRITE (6,'(63A1)')(ALPHA_Q(II),II=1,63)
15    FORMAT(' A MATCH COULD NOT BE FOUND FOR CHARACTER ',A1)
      GOTO 60
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
20    I=1
30    CONTINUE
      if(ISYM.NE.TABLE_Q(I)) THEN
         I=I+TABLE_Q(I+1)+2
! CHANGED 782 TO 699
         if(I.LE.699) GOTO 30
         WRITE(6, '('' HAVE EXHAUSTED SYMBOL TABLE- PROCEEDING TO NEXT SYMBOL'')')
         GOTO 60
      ENDIF
      NUMSEG=TABLE_Q(I+1)
      IF(NUMSEG .EQ. 0)GOTO 60
      DO 50 J=1,NUMSEG
!
!  DECODE THE NEXT 'TABLE' ENTRY TO DETERMINE THE RELATIVE CARTESIAN
!  COORDINATES (THE END POINTS) FOR THE NEXT LINE OF THE SYMBOL.
!
         ISEG=TABLE_Q(I+1+J)
         X1=ISEG/1000
         Y1=(ISEG-ISEG/1000*1000)/100
         X2=(ISEG-ISEG/100*100)/10
         Y2=ISEG-ISEG/10*10
!
!  TRANSLATE THE RELATIVE CARTESIAN COORDINATES FOR THE SEGMENT INTO
!  RELATIVE POLAR COORDINATES.
!
         if(ABS(X1).LT.0.0001) THEN
            ANGL1=PI/2.00
         ELSE
            ANGL1=ATAN(Y1/X1)
         ENDIF
         if(ABS(X2).LT.0.0001) THEN
            ANGL2=PI/2.0
         ELSE
            ANGL2=ATAN(Y2/X2)
         ENDIF
         RAD=SQRT(X1*X1+Y1*Y1)
         X1=RAD*XFAC*COS(ANGL1+THETA)
         Y1=RAD*YFAC*SIN(ANGL1+THETA)
         RAD=SQRT(X2*X2+Y2*Y2)
         X2=RAD*XFAC*COS(ANGL2+THETA)
         Y2=RAD*YFAC*SIN(ANGL2+THETA)
!
!  SET THE X AND Y FACTORS FOR SYMBOL CENTERING TO 0.0. DETERMINE
!  IF THE SYMBOL IS TO BE CENTERED. (ONLY SYMBOLS WITH NUMERICAL
!  EQUIVALENTS BETWEEN 0 AND 14 INCLUSIVE ARE TO BE CENTERED.)  IF
!  THE SYMBOL IS TO BE CENTERED, CALCULATE THE POSITION OF THE
!  ORIGIN OF THE SYMBOL DEFINITION GRID WITH RESPECT TO THE DEFINED
!  CENTER OF THE SYMBOL.
!
         if((INTEQ.GE.0) .AND. (INTEQ.LE.14)) THEN
            IF(NCHAR.LT.0)THEN
               CENTRX=(-2.0)*XFAC
               CENTRY=(-2.0)*YFAC
            ELSE
               CENTRX=0.0
               CENTRY=0.0
            ENDIF
         ELSE
            CENTRX=0.0
            CENTRY=0.0
         ENDIF
!
!  CALL THE 'PLOT' SUBROUTINE TO FIND THE CURRENTLY DEFINED ORIGIN
!  FOR THE ENTIRE FRAME. THEN, CALL THE 'primitive__draw_line' GRAPHICS PRIMITIVE
!  ROUTINE TO DRAW THE LINE SEGMENT. THE ARGUMENTS TO 'primitive__draw_line'
!  MUST INCORPORATE THE CURRENTLY DEFINED ORIGIN FOR THE FRAME  THE
!  (XORG,YORG), THE ORIGIN POINT FOR THE SYMBOL DEFINITION GRID
!  (XRLORG,YRLORG) AND THE SYMBOL CENTERING FACTOR (CENTRX,CENTRY).
!
         CALL PLOT(XORG,YORG,1005)
         CALL primitive__draw_line(X1+XORG+XRLORG+CENTRX,Y1+YORG+YRLORG+CENTRY, X2+XORG+XRLORG+CENTRX,Y2+YORG+YRLORG+CENTRY)
50    CONTINUE
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
60    XRLORG=XRLORG+(XSPC_Q*XFAC*COS(0.00+THETA))
      YRLORG=YRLORG+(YSPC_Q*YFAC*SIN(0.00+THETA))
!
!  IF 'INTEQ' DOES NOT CONTAIN A VALUE CORRESPONDING TO ONE OF
!  THE DEFINED SYMBOLS, ASSUME THAT A CHARACTER STRING IS BEING
!  DRAWN, AND TRANSFER CONTROL TO INTERPRET THE NEXT ELEMENT OF
!  THE STRING.
!
      if(NCHAR.GT.0) GOTO 5
!
!  RESET THE SYMBOL SIZE FACTORS TO UNITY PRIOR TO LEAVING THE
!  SUBROUTINE, AND RETURNS THE PEN TO THE ORIGIN OF THE
!  CHARACTER. (THE 'PLOT' SUBROUTINE APPLIES ANY REQUIRED
!  SCALING FACTORS.)
!
100   XFAC=1.0
      YFAC=1.0
      CALL PLOT(XPAGE,YPAGE,3)
END SUBROUTINE SYMBOL
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
implicit none

! ident_29="@(#)M_calcomp::where(3f): return current position and current plot-scaling factor"

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
LOGICAL FUNCTION primitive__NEWONE (VAL)
implicit none
INTEGER :: VAL
! CONCOM
INTEGER L,M,U,D
   if(NN_Q.EQ.0) GOTO 70
   primitive__NEWONE=.FALSE.
   if(ALT_Q) GOTO 10
   L=1
   U=NN_Q
   GOTO 20
10 L=NN_Q
   U=LIMIT_Q
20 D=U-L
   if(VAL.EQ.REC_Q(L)) RETURN
   if(D.EQ.0) GOTO 70
   if(VAL.EQ.REC_Q(U)) RETURN
   if(D.EQ.1) GOTO 70
30 M=L+D/2
   if(VAL-REC_Q(M)) 40,80,50
40 U=M
   GOTO 60
50 L=M
60 D=U-L
   if(D.NE.1) GOTO 30
70 primitive__NEWONE=.TRUE.
80 continue
END FUNCTION primitive__NEWONE
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE primitive__PASS (X,Z)
implicit none !! jsujsu
!           MERGE PAIRS OF STRINGS OF LENGTH C_Q FROM X INTO Z
!           WHERE X HAS NTRK_Q ELEMENTS
!           X AND Z ARE PARAMS FOR SWITCHING ARRAYS
   ! CONCOM
INTEGER X(*),Z(*)
INTEGER K,P,Q,R,S,T
!           K IS INDEX INTO Z
!           S, T ARE INDICES INTO X@  P.LE.S.LT.Q.LE.T.LT.R
!           WITH C_Q.EQ.Q-P AND C_Q.EQ.R-Q
   K=0
   R=1
! STEP THRU X PICKING OFF PAIRS OF C_Q-LENGTH STRINGS
10 P=R
   if(P.GT.ntrk_q) GOTO 70
!                            ALL DONE
   S=P
   Q=P+C_Q
   if(Q.GT.ntrk_q) GOTO 60
!                            NO MERGE, JUST TRANSFER THE STRING
   R=Q+C_Q
   if(R.GT.ntrk_q) R=ntrk_q+1
!                            SHORT STRING TO MERGE
   T=Q
! SELECT ELEMENT TO PASS TO Z
20 K=K+1
   if(X(S).GT.X(T)) GOTO 30
   Z(K)=X(S)
   S=S+1
   if(S-Q) 20,40,40
30 Z(K)=X(T)
   T=T+1
   if(T-R) 20,50,50
! FIRST STRING EMPTY, FLUSH THE SECOND
40 K=K+1
   Z(K)=X(T)
   T=T+1
   if(T-R) 40,10,10
! SECOND STRING EMPTY, FLUSH THE FIRST
50 K=K+1
   Z(K)=X(S)
   S=S+1
   if(S-Q) 50,10,10
! FLUSH END OF X, NO MERGE NEEDED
60 K=K+1
   Z(K)=X(S)
   S=S+1
   if(S-ntrk_q) 60,60,70
! ALL DONE
70 RETURN
!
END SUBROUTINE primitive__PASS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE primitive__RECORX
implicit none
   ! CONCOM
   ODDPAS_Q=.FALSE.
   C_Q=1
10 if(C_Q.GE.NTRK_Q) GOTO 20
   CALL primitive__PASS(TRK_Q,TRKA_Q)
   ODDPAS_Q=.TRUE.
   C_Q=C_Q+C_Q
   if(C_Q.GE.NTRK_Q) GOTO 20
   CALL primitive__PASS(TRKA_Q,TRK_Q)
   ODDPAS_Q=.FALSE.
   C_Q=C_Q+C_Q
   GOTO 10
20 if(ODDPAS_Q) GOTO 30
   CALL primitive__MERGER(TRK_Q)
   RETURN
30 CALL primitive__MERGER(TRKA_Q)
!
END SUBROUTINE primitive__RECORX
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE primitive__SCAN (AM,TOTX,TOTY,NYDIM)
implicit none
integer :: nydim
REAL    :: AM(NYDIM,*),TOTX(*),TOTY(*)
! CONCOM
REAL    :: OP
integer :: jx
integer :: jy
   ALT_Q=.FALSE.
   EDGE_Q=.TRUE.
   NN_Q=0
   IX_Q=1
   OP=AM(1,1)
   DO 10 IY_Q=2,NY_Q
      if(AM(1,IY_Q).LT.CVV_Q.OR.OP.GE.CVV_Q) GOTO 10
      DIREC_Q=3
      CALL primitive__TRACER(AM,TOTX,TOTY,NYDIM)
10 OP=AM(1,IY_Q)
   IX_Q=NX_Q
   OP=AM(NX_Q,NY_Q)
   DIREC_Q=1
   IY_Q=NY_Q
   DO 20 JY=1,NYM1_Q
      IY_Q=IY_Q-1
      if(AM(NX_Q,IY_Q).LT.CVV_Q.OR.OP.GE.CVV_Q) GOTO 20
      CALL primitive__TRACER(AM,TOTX,TOTY,NYDIM)
20 OP=AM(NX_Q,IY_Q)
   IY_Q=1
   OP=AM(NX_Q,1)
   DIREC_Q=4
   IX_Q=NX_Q
   DO 30 JX=1,NXM1_Q
      IX_Q=IX_Q-1
      if(AM(IX_Q,1).LT.CVV_Q.OR.OP.GE.CVV_Q) GOTO 30
      CALL primitive__TRACER(AM,TOTX,TOTY,NYDIM)
30 OP=AM(IX_Q,1)
   IY_Q=NY_Q
   OP=AM(1,NY_Q)
   DIREC_Q=2
   DO 40 IX_Q=2,NX_Q
      if(AM(IX_Q,NY_Q).LT.CVV_Q.OR.OP.GE.CVV_Q) GOTO 40
      CALL primitive__TRACER(AM,TOTX,TOTY,NYDIM)
40 OP=AM(IX_Q,NY_Q)
   EDGE_Q=.FALSE.
   IY_Q=NY_Q
   DO 50 JY=2,NYM1_Q
      IY_Q=IY_Q-1
      OP=AM(1,IY_Q)
      DO 50 IX_Q=2,NX_Q
         if(AM(IX_Q,IY_Q).LT.CVV_Q.OR.OP.GE.CVV_Q) GOTO 50
         if(primitive__NEWONE(IX_Q+(IY_Q-1)*NX_Q))THEN
            CALL primitive__TRACER(AM,TOTX,TOTY,NYDIM)
         ENDIF
50 OP=AM(IX_Q,IY_Q)
!
END SUBROUTINE primitive__SCAN
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE primitive__TRACER (AM,TOTX,TOTY,NYDIM)
implicit none
integer :: nydim
REAL    :: AM(NYDIM,*),TOTX(*),TOTY(*)
! CONCOM
INTEGER :: Z
REAL    :: TEM,PLOTX,PLOTY,FPLOTX,FPLOTY,AMC,AMW,AMZ,AMQ,AMS,AMD,AMP
LOGICAL :: FIRST
INTEGER :: S,TIX,TIY,CIX,CIY,DIX,DIY,SUB,PIX,PIY,SWSD,SWMQ
real    :: amm
   FIRST=.TRUE.
   NTRK_Q=0
   TIX=IX_Q
   TIY=IY_Q
   S=DIREC_Q
   DIX=IXD_Q(S)
   DIY=IYD_Q(S)
   CIX=TIX+DIX
   CIY=TIY+DIY
10 continue
   TEM=AM(TIX,TIY)
   AMC=AM(CIX,CIY)
   if(DIX.NE.0) GOTO 20
   SUB=TIY
   if(DIY.LT.0) SUB=SUB-1
   PLOTX=TOTX(TIX)
   PLOTY=TOTY(TIY)+DIY*(TEM-CVV_Q)*HY_Q(SUB)/(TEM-AMC)
   GOTO 30
20 continue
   SUB=TIX
   if(DIX.LT.0) SUB=SUB-1
   PLOTY=TOTY(TIY)
   PLOTX=TOTX(TIX)+DIX*(TEM-CVV_Q)*HX_Q(SUB)/(TEM-AMC)
30 continue
   if(FIRST) GOTO 40
   CALL PLOT(PLOTX*RATIO_Q,PLOTY*RATIO_Q,2)
   GOTO 50
40 continue
   FPLOTX=PLOTX*RATIO_Q
   FPLOTY=PLOTY*RATIO_Q
   CALL NUMBER(FPLOTX+.02, FPLOTY-.15, 0.14, FLOAT(II_Q), 0.0, -1)
   CALL PLOT(FPLOTX,FPLOTY,3)
   FIRST=.FALSE.
50 continue
   if(.NOT.(EDGE_Q)) GOTO 60
   if(TIY.EQ.NY_Q.AND.S.EQ.4.OR.TIX.EQ.1.AND.S.EQ.1.OR.TIX.EQ.NX_Q.AND.S .EQ.3.OR.TIY.EQ.1.AND.S.EQ.2) GOTO 260
60 continue
   AMM=(TEM+AMC)/2.0
   Z=S+1
   if(Z.GT.4) Z=Z-4
   PIX=IXD_Q(Z)
   PIY=IYD_Q(Z)
   AMW=AM(TIX+PIX,TIY+PIY)
   AMZ=AM(CIX+PIX,CIY+PIY)
   AMQ=(AMW+AMZ)/2.0
   AMS=(TEM+AMW)/2.0
   AMD=(AMC+AMZ)/2.0
   AMP=(AMM+AMQ)/2.0
   if(AMM.GE.CVV_Q) GOTO 110
   if(AMS.LT.CVV_Q) GOTO 260
   if(AMP.GE.CVV_Q) GOTO 90
   ASSIGN 70 TO SWSD
   GOTO 190
70 continue
   if(AMW.LT.CVV_Q) GOTO 260
   if(AMQ.LT.CVV_Q) GOTO 220
   ASSIGN 80 TO SWMQ
   GOTO 160
80 continue
   if(AMZ.LT.CVV_Q) GOTO 220
   GOTO 240
90 continue
   ASSIGN 100 TO SWMQ
   GOTO 160
100 continue
   if(AMD.GE.CVV_Q) GOTO 240
   ASSIGN 80 TO SWSD
   GOTO 190
110 continue
   if(AMP.GE.CVV_Q) GOTO 140
   ASSIGN 120 TO SWMQ
   GOTO 160
120 continue
   if(AMS.LT.CVV_Q) GOTO 260
   ASSIGN 130 TO SWSD
   GOTO 190
130 continue
   if(AMW.LT.CVV_Q) GOTO 260
   GOTO 220
140 continue
   if(AMD.GE.CVV_Q) GOTO 240
   ASSIGN 150 TO SWSD
   GOTO 190
150 continue
   if(AMQ.GE.CVV_Q) GOTO 80
   ASSIGN 130 TO SWMQ
!      INTERPOLATE MQ
160 continue
   if(PIX.NE.0) GOTO 170
   SUB=TIY
   if(PIY.LT.0) SUB=SUB-1
   PLOTX=(TOTX(TIX)+TOTX(CIX))/2.0
   PLOTY=TOTY(TIY)+PIY*(AMM-CVV_Q)*HY_Q(SUB)/(AMM-AMQ)
   GOTO 180
170 continue
   SUB=TIX
   if(PIX.LT.0) SUB=SUB-1
   PLOTY=(TOTY(TIY)+TOTY(CIY))/2.0
   PLOTX=TOTX(TIX)+PIX*(AMM-CVV_Q)*HX_Q(SUB)/(AMM-AMQ)
180 continue
   CALL PLOT(PLOTX*RATIO_Q,PLOTY*RATIO_Q,2)
   GOTO SWMQ, (80,100,120,130)
!      INTERPOLATE SD
190 continue
   if(DIX.NE.0) GOTO 200
   SUB=TIY
   if(DIY.LT.0) SUB=SUB-1
   PLOTX=(TOTX(TIX)+TOTX(TIX+PIX))/2.0
   PLOTY=TOTY(TIY)+DIY*(AMS-CVV_Q)*HY_Q(SUB)/(AMS-AMD)
   GOTO 210
200 continue
   SUB=TIX
   if(DIX.LT.0) SUB=SUB-1
   PLOTY=(TOTY(TIY)+TOTY(TIY+PIY))/2.0
   PLOTX=TOTX(TIX)+DIX*(AMS-CVV_Q)*HX_Q(SUB)/(AMS-AMD)
210 continue
   CALL PLOT(PLOTX*RATIO_Q,PLOTY*RATIO_Q,2)
   GOTO SWSD (70,80,130,150)
!      TRANSFER TO W
220 continue
   if(TIX.EQ.1) GOTO 230
   if(AM(TIX-1,TIY).LT.CVV_Q) CALL primitive__ADDREC(TIX+(TIY-1)*NX_Q)
230 continue
   TIX=TIX+PIX
   TIY=TIY+PIY
   GOTO 280
!      TRANSFER TO Z
240 continue
   if(TIX.EQ.1) GOTO 250
   if(AM(TIX-1,TIY).LT.CVV_Q) CALL primitive__ADDREC(TIX+(TIY-1)*NX_Q)
250 continue
   S=S+3
   TIX=CIX+PIX
   TIY=CIY+PIY
   GOTO 270
!      ROTATE
260 continue
   S=S+1
270 continue
   if(S.GT.4) S=S-4
   DIX=IXD_Q(S)
   DIY=IYD_Q(S)
280 continue
   CIX=TIX+DIX
   CIY=TIY+DIY
   if(EDGE_Q) GOTO 290
   if((TIX.NE.IX_Q).OR.(TIY.NE.IY_Q).OR.(DIREC_Q.NE.S)) GOTO 300
   CALL PLOT(FPLOTX,FPLOTY,2)
   GOTO 330
290 continue
   if((CIX.LT.1).OR.(CIY.LT.1).OR.(CIX.GT.NX_Q).OR.(CIY.GT.NY_Q)) GOTO 320
300 continue
   if(AM(CIX,CIY).LT.CVV_Q) GOTO 10
   if(TIX.EQ.1) GOTO 310
   if(AM(TIX-1,TIY).LT.CVV_Q) CALL primitive__ADDREC(TIX+(TIY-1)*NX_Q)
310 continue
   S=S+3
   TIX=CIX
   TIY=CIY
   GOTO 270
320 continue
   if(TIX.EQ.1) GOTO 330
   if(AM(TIX-1,TIY).LT.CVV_Q) CALL primitive__ADDREC(TIX+(TIY-1)*NX_Q)
330 continue
   CALL primitive__RECORX
!
END SUBROUTINE primitive__TRACER
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
SUBROUTINE CNTOUR (AM,XX,YY,TOTX,TOTY,HGT,CV,CVN,TAB,NDIMYY)
implicit none

! ident_30="@(#)M_calcomp::cntour(3f): draw a contour plot"

INTEGER XX,YY,CVN
CHARACTER ENCXDE*9
integer :: ndimyy
REAL AM(NDIMYY,*),TOTX(*),TOTY(*),CV(*),HGT
LOGICAL TAB
! CONCOM
INTEGER I,IT
REAL RT,TEM,DIS,SCALE
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
   NX_Q=XX
   NXM1_Q=NX_Q-1
   NY_Q=YY
   NYM1_Q=NY_Q-1
   DO I=1,NXM1_Q
      HX_Q(I)=TOTX(I+1)-TOTX(I)
   enddo
   DO I=1,NYM1_Q
      HY_Q(I)=TOTY(I+1)-TOTY(I)
   enddo
   RT=TOTX(1)
   RTX=RT
   if(RT.EQ.0.0) GOTO 40
   DO I=1,NX_Q
      TOTX(I)=TOTX(I)-RT
   enddo
40 RT=TOTY(1)
   RTY=RT
   if(RT.EQ.0.0) GOTO 60
   DO I=1,NY_Q
      TOTY(I)=TOTY(I)-RT
   enddo
60 if(HGT.GT. 6.5) HGT= 6.5
   if(TAB) GOTO 150
70 RATIO_Q=HGT/TOTY(NY_Q)
   DO II_Q=1,CVN
      CVV_Q=CV(II_Q)
      CALL primitive__SCAN(AM,TOTX,TOTY,NDIMYY)
   enddo
   CALL PLOT(0.0,0.0,-3)
   DO I=2,NXM1_Q
      BJKLX=TOTX(I)*RATIO_Q
      CALL PLOT(BJKLX,0.0,2)
      CALL PLOT(BJKLX,-0.1,2)
      CALL PLOT(BJKLX,0.0,2)
   enddo
   BJKLX=TOTX(NX_Q)*RATIO_Q
   CALL PLOT(BJKLX,0.0,2)
   DO I=2,NYM1_Q
      BJKLY=TOTY(I)*RATIO_Q
      CALL PLOT(BJKLX,BJKLY,2)
      CALL PLOT(BJKLX+0.1,BJKLY,2)
      CALL PLOT(BJKLX,BJKLY,2)
   enddo
   BJKLY=TOTY(NY_Q)*RATIO_Q
   CALL PLOT(BJKLX,BJKLY,2)
   DO IPJB=2,NXM1_Q
      I=NXM1_Q+2-IPJB
      BJKLX=TOTX(I)*RATIO_Q
      CALL PLOT(BJKLX,BJKLY,2)
      CALL PLOT(BJKLX,BJKLY+0.1,2)
      CALL PLOT(BJKLX,BJKLY,2)
   enddo
   CALL PLOT(0.0,BJKLY,2)
   DO IPJB=2,NYM1_Q
      I=NYM1_Q+2-IPJB
      BJKLY=TOTY(I)*RATIO_Q
      CALL PLOT(0.0,BJKLY,2)
      CALL PLOT(-0.1,BJKLY,2)
      CALL PLOT(0.0,BJKLY,2)
   enddo
   CALL PLOT(0.0,0.0,2)
   DO I=1,NX_Q
      TOTX(I)=TOTX(I)+RTX
   enddo
   DO I=1,NY_Q
      TOTY(I)=TOTY(I)+RTY
   enddo
   RETURN
!
!  THE FOLLOWING STATEMENT WAS MODIFIED WHEN THIS SUBROUTINE WAS
!  CONVERTED TO THE CRAY. THE STATEMENT ORIGINALLY READ
!
!     150 DIS=HGT-0.1
!
!  'DIS' WAS SET TO A VALUE OF 8.5 TO FORCE THE LEGEND OF THE
!  PLOT TO THE UPPER LEFT CORNER OF THE DISPLAY AREA.
!
150 DIS=8.5
   RT=CV(1)
   DO I=2,CVN
      if(ABS(RT).LT.ABS(CV(I))) RT=CV(I)
   enddo
   IT=0
   SCALE=1.0
170 if(ABS(RT).LT.999999.95) GOTO 180
   SCALE=SCALE/10.0
   RT=RT/10.0
   IT=IT-1
   GOTO 170
180 if(IT.EQ.0) GOTO 190
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
   INTEQ=999
   CALL SYMBOL(0.26,DIS,0.14,'SCALE = 10',INTEQ,0.0,10)
   CALL NUMBER(1.66,DIS+0.07,0.07,FLOAT(IT),0.0,-1)
   DIS=DIS-0.25
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
190 INTEQ=999
   CALL SYMBOL(0.0,DIS,0.14,'DIGIT      CONTOUR',INTEQ,0.0,18)
   if(CVN.GT.10) CALL SYMBOL(2.0,DIS,0.14,'DIGIT      CONTOUR',INTEQ,0.0,18)
   DIS = DIS - 0.25
!
!  SAVE THE Y COORDINATE OF THE TITLE LINE OF THE LEGEND; IT WILL
!  BE USED TO POSITION THE SECOND COLUMN OF THE LEGENDIF MORE
!  THAN 10 CONTOUR LINES ARE REQUESTED.
!
   DISSAV=DIS
   DO 220 I=1,CVN
      RT=1.0
      if(I/10.NE.0) RT=2.0
!
!  SET THE DISPLACEMENT IN THE X DIRECTION FOR THE CONTOUR
!  IDENTIFIERS. BY DEFAULT, THE DISPLACEMENT IS SET FOR
!  THE LEFT COLUMN OF THE LEGEND.
!
      XDISP=1.0
      if(CVN.GT.10) THEN
         XDISP=3.0
         DIS=DISSAV
      ENDIF
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
      CALL NUMBER(0.42-RT+XDISP, DIS, 0.09, FLOAT(I), 0.0, -1)
      TEM=CV(I)*SCALE
      IT=TEM+00.5
      RT=3.0
200   IT=IT/10
      if(IT.EQ.0) GOTO 210
      RT=RT+1.0
      GOTO 200
210   CONTINUE
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
      INTEQ=999
      WRITE(ENCXDE,'(E9.3)')CV(I)
      CALL SYMBOL(0.7+XDISP, DIS, 0.09, ENCXDE,INTEQ, 0.0, 9)
220 DIS = DIS - 0.20
!
!  THE FOLLOWING STATEMENT WAS ORIGINALLY USED TO POSITION THE
!  CONTOUR PLOT BESIDE THE LEGEND. IT HAS BEEN "COMMENTED OUT"
!  SO THAT THE CONTOUR PLOT MAY BE POSITIONED BELOW THE LEGEND.
!  THIS ARRANGEMENTS PERMITS LARGER CONTOUR PLOTS.
!
!     CALL PLOT(3.5,0.0,-3)
   GOTO 70
!
END SUBROUTINE CNTOUR
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE primitive__ADDREC (VAL)
implicit none
INTEGER,INTENT(IN) ::  VAL
! CONCOM
   NTRK_Q=NTRK_Q+1
   if(NTRK_Q.GT.HXSZ_Q) STOP 7117
   TRK_Q(NTRK_Q)=VAL
!
END SUBROUTINE primitive__ADDREC
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE primitive__MERGER (X)
implicit none !! jsujsu
INTEGER X(*)
! CONCOM
INTEGER :: K,I,J
INTEGER DUP
   if(ntrk_q.EQ.0) GOTO 180
   if(ALT_Q) GOTO 80
   I=ntrk_q
   K=LIMIT_Q+1
   if(NN_Q.EQ.0) GOTO 60
   J=NN_Q
10 if(X(I)-rec_q(J)) 20,15,30
15 CONTINUE
   I=I-1
   if(I) 50,50,20
20 K=K-1
   rec_q(K)=rec_q(J)
   J=J-1
   if(J) 60,60,10
30 K=K-1
   if(K.LE.J) STOP 7227
   DUP=X(I)
   rec_q(K)=DUP
40 I=I-1
   if(I.LE.0) GOTO 50
   if(X(I)-DUP) 10,40,10
50 K=K-1
   rec_q(K)=rec_q(J)
   J=J-1
   if(J) 170,170,50
60 K=K-1
   if(K.LT.1) STOP 7227
   DUP=X(I)
   rec_q(K)=DUP
70 I=I-1
   if(I.LE.0) GOTO 170
   if(X(I)-DUP) 60,70,60
80 I=1
   K=0
   J=NN_Q
90 if(X(I)-rec_q(J)) 120,100,110
100 I=I+1
    if(I-ntrk_q) 110,110,140
110 K=K+1
    rec_q(K)=rec_q(J)
    J=J+1
    if(J-LIMIT_Q) 90,90,150
120 K=K+1
    if(K.GE.J) STOP 7227
    DUP=X(I)
    rec_q(K)=DUP
130 I=I+1
    if(I.GT.ntrk_q) GOTO 140
    if(X(I)-DUP) 90,130,90
140 K=K+1
    rec_q(K)=rec_q(J)
    J=J+1
    if(J-LIMIT_Q) 140,140,170
150 K=K+1
    if(K.GT.LIMIT_Q) STOP 7227
    DUP=X(I)
    rec_q(K)=DUP
160 I=I+1
    if(I.GT.ntrk_q) GOTO 170
    if(X(I)-DUP) 150,160,150
170 ALT_Q=.NOT.ALT_Q
    NN_Q=K
180 continue
!
END SUBROUTINE primitive__MERGER
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
implicit none
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
implicit none
   call vexit()                            ! bugger on out of here, terminating graphics
end subroutine primitive__end_plotting
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine primitive__draw_line(x1,y1,x2,y2)     ! draw line from <x1,y1> to <x2,y2>
use M_draw, only : move2, draw2
implicit none
real,intent(in) :: x1,y1,x2,y2    ! REAL4/REAL8
   call move2(real(x1),real(y1))
   call draw2(real(x2),real(y2))
end subroutine primitive__draw_line
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine primitive__width(iwidth8)
use M_draw
implicit none
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
implicit none
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
implicit none
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
implicit none
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
implicit none
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
implicit none
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
implicit none
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
