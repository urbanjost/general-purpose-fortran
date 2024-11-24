










!>
!!##NAME
!!    M_draw(3fm) - [M_draw::INTRO] The M_draw graphics library
!!    (LICENSE:PD)
!!
!!##DESCRIPTION
!!
!!    M_draw is a portable public-domain device-independent graphics library
!!    intended for being called from Fortran that is based on VOGLE (from
!!    the University of Melbourne) that is loosely based on the Silicon
!!    Graphics Iris GL library. It was also partly inspired by the DIGS
!!    library developed at the U.S. Naval Academy under the guidance of
!!    Prof David Rogers.
!!
!!    Many output devices are available:
!!
!!      * FrameMaker MIF 3.0 (Maker Interchange File) driver.
!!      * Adobe PDF driver.
!!      * HTML5 Canvas driver.
!!      * SVG driver.
!!      * A PCL5/HPGL2 driver that supports prefsize() calls.
!!      * Monochrome PBM (Poskanzer bitmap P1 and P4 formats) and X11
!!        bitmap driver.
!!      * Color PBM (Poskanzer pixmap P3 and P6 formats). If you have the
!!        pbmplus package you can use it to make M_draw appear to write
!!        any format pbmplus writes (assuming your system supports the
!!        popen(3c) function).
!!      * A clear-text CGM (Computer Graphics Metafile) driver.
!!      * A different (color) PostScript driver.
!!      * A driver for Microsoft VML (Vector Markup Language)
!!
!!    M_draw is intended to produce simple graphics composed of line
!!    drawings and polygon fills in two and three dimensions. It handles
!!    circles, curves, arcs, patches, polygons, and software text in a device
!!    independent fashion. Simple hidden line removal is also available via
!!    polygon backfacing. Access to hardware text and double buffering of
!!    drawings depends on the driver.
!!
!!    M_draw is callable from C and Fortran, and Pascal; but M_draw is only
!!    supported in Fortran (the C components are being converted to Fortran).
!!
!!    The original VOGLE source's ownership statement
!!
!!       This software is public domain and may be used for any purpose
!!       commercial or otherwise. It is offered without any guarantee
!!       as to its suitability for any purpose or as to the sanity of
!!       its writers. The authors do ask that the source is passed on to
!!       anyone that requests a copy, and that people who get copies don't
!!       go round claiming they wrote it. Use at your own risk.
!!
!!##LIBRARY FUNCTION DESCRIPTIONS
!!
!!    DEVICE ROUTINES
!!    vinit(device)                    Initialise device
!!    vexit()                          Reset window/terminal (must be last routine called)
!!    voutput(path)                    Redirect output from *next* vinit to file
!!    vnewdev(device)                  Reinitialize to use new device without changing
!!    vgetdev(device)                  Get name of current device
!!    pushdev(device)                  push current device onto a stack
!!    popdev(device)                   pop device from stack created by pushdev.
!!    getdepth()                       Return number of bit planes (color planes)
!!
!!    ROUTINES FOR SETTING UP WINDOWS
!!    prefposition(x, y)               Specify preferred position of window
!!    prefsize(width, height)          Specify preferred width and height of window
!!
!!    Some devices are basically window oriented - like sunview and X11. You
!!    can give M_draw some information on the window that it will use with these
!!    routines. These can make your code very device independent. Both routines
!!    take arguments which are in device space. (0, 0) is the top left hand
!!    corner in device space. To have any effect these routines must be called
!!    before vinit. For the X11 device, an entry may be made in your .Xdefaults
!!    file or loaded in with the xrdb(1) command:
!!
!!       xrdb <<\end_of_file
!!       ! X11 Windows fonts to use for "small" and "large" fonts
!!       M_draw*smallfont: fixed
!!       M_draw*largefont: 9x15
!!       ! title on decoration bar for the window
!!       M_draw*title: My M_draw program
!!       ! window geometry and position,
!!       ! overridden by prefsize(3c) and prefposition(3c)
!!       M_draw.Geometry: =500x500-10+20
!!       end_of_file
!!
!!    (where you specify your geometry as you please).
!!
!!    CLIPPING ROUTINES
!!    clipping(onoff)                  Turn clipping on or off
!!
!!    COLOR ROUTINES
!!    clear()                          Clears screen to current color
!!    color(col)                       Set current color
!!    mapcolor(indx, red, green, blue) Set color map index
!!
!!    INTERACTIVE ROUTINES
!!    getkey()                         Return ASCII ordinal of next key typed
!!    checkkey()                       Returns zero if no key is pressed or ASCII ordinal
!!    getstring(bcol, string)          Read in a string, echoing it in current font
!!    locator(xaddr, yaddr)            Find out where cursor is
!!    slocator(xaddr, yaddr)           Find out where cursor is in screen coordinates
!!
!!    FLUSHING
!!    vsetflush(yesno)                 Set global flushing status
!!    vflush()                         Call device flush or syncronisation routine
!!
!!    On some devices (particularly X11) considerable speedups in display
!!    can be achieved by not flushing each graphics primitive call to the
!!    actual display until necessary. M_draw automatically delays flushing in
!!    the following cases:
!!
!!      * Within a callobj() call.
!!      * Within curves and patches.
!!      * Within Hershey software text.
!!      * When double buffering (the flush is only done within swapbuffers).
!!
!!    There are two user routines that can be used to control flushing.
!!
!!    VIEWPORT ROUTINES
!!    viewport(left, right, bottom, top)     Specify which part of screen to draw in
!!    pushviewport()                         Save current viewport
!!    popviewport()                          Retrieve last viewport
!!    getviewport(left, right, bottom,top)   Returns limits of current viewport in screen coordinates
!!    expandviewport()                       use the entire device viewport
!!    unexpandviewport()                     undo expandviewport(3f)
!!
!!    Viewpoint routines alter the current transformation matrix.
!!
!!    GETTING THE ASPECT DETAILS
!!    getaspect()                      Returns the ratio height over width of the display device.
!!    getfactors(wfact, hfact)         Returns width over min(width of device, height of device) and height over min(width of
!!                                     device, height of device).
!!    getdisplaysize(w, h)             Returns width and height of device in device units
!!
!!    Often the screen is not perfectly square and it would be nice to use
!!    the extra space without having to turn clipping off. The following
!!    routines are provided to get the values needed to adjust the calls
!!    to viewport, etc as needed.
!!
!!    ATTRIBUTE STACK ROUTINES
!!    pushattributes()                 Save the current attributes on the attribute stack.
!!    popattributes()                  Restore attributes to what they were at last pushattributes().
!!
!!    The attribute stack contains details such as current color, filling,
!!    hatching, centered, fixedwidth, text height, text width, and the
!!    current font. If you need to prevent object calls from changing these,
!!    use pushattributes before the call and popattributes after.
!!
!!    PROJECTION ROUTINES
!!    ortho(left, right, bottom, top,near,far)    Define x,y,z clipping planes.
!!    ortho2(left, right, bottom, top)            Define x and y clipping planes.
!!    perspective(fov, aspect, near, far)         Specify perspective viewing pyramid
!!    window(left, right, bot, top, near,far)     Specify a perspective viewing pyramid
!!
!!    PROJECTION AND VIEWPORT ROUTINES
!!    subroutine page (left, right, bottom, top)  create a window of the specified size and then
!!                                                set the viewport to the largest viewport with
!!                                                that aspect so that output acts much like a
!!                                                page of paper of the specified size without
!!                                                distortion.
!!
!!    All the projection routines define a new transformation matrix, and
!!    consequently the world units. Parallel projections are defined by
!!    ortho or ortho2. Perspective projections can be defined by perspective
!!    and window.
!!
!!    MATRIX STACK ROUTINES
!!    pushmatrix()                     Save the current transformation matrix on the matrix stack.
!!    popmatrix()                      Reinstall the last matrix pushed
!!
!!    VIEWPOINT ROUTINES
!!    polarview(dist, azim, inc, twist)      Specify the viewer's position in polar coordinates
!!    up(x, y, z)                            Specify the world up.
!!    lookat(vx, vy, vz, px, py, pz,twist)   Specify the viewer's position
!!
!!    MOVE ROUTINES
!!    move(x, y, z)                    Move current graphics position to (x, y, z)
!!    rmove(deltax, deltay, deltaz)    Relative move
!!    move2(x, y)                      Move graphics position to point (x, y)
!!    rmove2(deltax, deltay)           Relative move in world units.
!!    smove2(x, y)                     Move current graphics position in screen coordinates (-1.0 to 1.0).
!!    rsmove2(deltax, deltay)          Relative move in screen units (-1.0 to 1.0).
!!
!!    LINESTYLE ROUTINES
!!    linewidth()                      set line width in rasters
!!    dashcode()                       set dash pattern length
!!    linestyle()                      set the line dash pattern
!!
!!    Linestyles are specified by giving a nominal length of a single
!!    dash and a character string consisting of 1's and 0's (zeros) that
!!    specify when to draw a dash and when not to draw a dash. Linestyles
!!    will follow curves and "go around" corners. If a linestyle is set or
!!    reset, the accumulated information as to where on a curve (or line)
!!    a dash is to be draw is also reset.
!!
!!    For EXAMPLE, with a nominal view of -1 to 1, setting the dash length
!!    to 0.5, and the linestyle to '11010' would draw a line(or curve) with
!!    a 1.0 unit solid part, followed by a 0.5 unit blank part followed by
!!    a 0.5 unit solid part followed by a 0.5 unit blank part. The linestyle
!!    would then repeat itself.
!!
!!    The dash sizes are affected by the current viewport/transformation
!!    scaling factors, meaning that in perspective, the dashes look smaller
!!    the farther away they are.
!!
!!    DRAW ROUTINES
!!    draw(x, y, z)                    Draw from current graphics position to (x, y, z)
!!    rdraw(deltax, deltay, deltaz)    Relative draw
!!    draw2(x, y)                      Draw from current graphics position to point (x, y)
!!    rdraw2(deltax,deltay)            Relative draw
!!    sdraw2(x, y)                     Draw in screen coordinates (-1.0 to 1.0).
!!    rsdraw2(deltax, deltay)          Relative draw in screen units (-1.0 to 1.0).
!!
!!    ARCS AND CIRCLES
!!    circleprecision(nsegs)                  Set number of line segments in a circle. Default is 32.
!!    arc(x, y, radius, startang, endang)     Draw an arc in world units.
!!    sector(x, y, radius, startang,endang)   Draw a sector. Note: sectors are polygons.
!!    circle(x, y, radius)                    Draw a circle. Note: circles are polygons.
!!
!!    When creating arcs and sectors note that angles are
!!    measured in degrees; where zero(0) is the positive X axis in a
!!    right-handed Cartesian coordinate system and positive angles sweep
!!    counterclockwise. If filling sectors or circles (As described in the
!!    section on polygons) hatch pitch is measured in world coordinates
!!    and is initially set to 0.1. The initial hatch angle is zero(0).
!!
!!    CURVE ROUTINES
!!    curvebasis(basis)           Define a basis matrix for a curve.
!!    curveprecision(nsegs)       Define number of line segments used to draw a curve.
!!    rcurve(geom)                Draw a rational curve.
!!    curve(geom)                 Draw a curve.
!!    curven(n, geom)             Draw n - 3 overlapping curve segments. Note: n must be at least 4.
!!
!!    RECTANGLES AND GENERAL POLYGON ROUTINES
!!    rect(x1, y1, x2, y2)        Draw a rectangle.
!!    polyfill(onoff)             Set the polygon fill flag
!!    polyhatch(onoff)            Set the polygon hatch flag
!!    hatchang(angle)             Set the angle of the hatch lines.
!!    hatchpitch(pitch)           Set the distance between hatch lines.
!!    poly2(n, points)            Construct an (x, y) polygon from an array of points
!!    poly(n, points)             Construct a polygon from an array of points
!!    makepoly()                  opens polygon constructed by a series of move-draws and closed by closepoly
!!    closepoly()                 Terminates a polygon opened by makepoly.
!!    backface(onoff)             Turns on culling of backfacing polygons.
!!    backfacedir(clockwise)      Sets backfacing direction to clockwise or anti-clockwise
!!
!!    A polygon is composed of a number of coplanar line segments connected
!!    end to end to form a closed shape.
!!
!!    In M_draw curves are estimated by a series of line segments, and thus
!!    may be included easily into polygons.
!!
!!    Regular    A polygon with all sides and interior angles the same. Regular
!!               polygons are always convex. See Regular Polygons
!!    Irregular  Each side may a different length, each angle may be a different
!!               measure. The opposite of a regular polygon. See Irregular Polygons
!!    Convex     All interior angles less than 180 ,and all vertices 'point
!!               outwards' away from the interior. The opposite of concave. Regular
!!               polygons are always convex. See Convex Polygons
!!    Concave    One or more interior angles greater than 180 . Some vertices
!!               push 'inwards' towards the interior of the polygon. The opposite
!!               of convex.
!!    Self-intersecting or Crossed  A polygon where one or more sides crosses back over another side,
!!                                  creating multiple smaller polygons. Most of the properties and
!!                                  theorems concerning polygons do not apply to this shape. It is
!!                                  best considered as several separate polygons. A polygon that in
!!                                  not self-intersecting in this way is called a simple polygon.
!!
!!    TEXT ROUTINES
!!    font(fontname)                   set the current font
!!    numchars()                       return number of characters in the current SOFTWARE font.
!!    textsize(width, height)          set maximum size of a character in the current SOFTWARE font.
!!    textang(ang)                     set the SOFTWARE text angle.
!!    fixedwidth(onoff)                turns fixedwidth mode on or off for SOFTWARE fonts.
!!    getcharsize(c, width, height)    get the width and height of a character.
!!    getfontdec()                     return size of maximum font descender
!!    getfontsize(width, height)       get maximum width and height of a character in a font.
!!    drawchar(c)                      draw the character c and update current position.
!!    drawstr(str)                     draw the text in string at the current position.
!!    strlength(str)                   return the length of the string s
!!    boxtext(x, y, l, h, s)           stretch and draw the SOFTWARE string s so that it fits in the imaginary box
!!    boxfit(x, y, l, h, s)            resize the SOFTWARE text size so it fits in a box
!!
!!    textjustify(val)                 general text justification
!!
!!    leftjustify()                    left justify text
!!    rightjustify()                   right justify text
!!    topjustify()                     top justify text
!!    bottomjustify()                  bottom justify text
!!
!!    centertext(onoff)                turns centertext mode on or off for SOFTWARE fonts.
!!    xcentertext()                    center text in the X direction
!!    ycentertext()                    center text in the Y direction
!!
!!    textslant()                      defines the obliqueness of the fonts.
!!    textweight()                     defines the weight of the fonts.
!!
!!    M_draw supports hardware and software fonts. The software fonts are based
!!    on the character set digitized by Dr Allen V. Hershey while working at
!!    the U. S. National Bureau of Standards. Exactly what hardware fonts are
!!    supported depends on the device, but it is guaranteed that the names
!!    "large" and "small" will result in something readable. For X11 displays
!!    the default large and small fonts used by the program can be overridden
!!    by placing the following defaults in the ~/.Xdefaults file:
!!
!!      draw.smallfont: X11-font-name
!!      draw.largefont: X11-font-name
!!
!!    It is noted here that hardware text is always assumed to be drawn
!!    parallel to the (x, y) plane, using whatever the current z coordinate
!!    is. The following software fonts are supported:
!!
!!       astrology       cursive         cyrillic        futura.l
!!       futura.m        gothic.eng      gothic.ger      gothic.ita
!!       greek           markers         math.low        math.upp
!!       meteorology     music           script          symbolic
!!       times.g         times.i         times.ib        times.r
!!       times.rb        japanese
!!
!!    A markers font "markers" is also provided for doing markers - you need
!!    to have centertext mode on for this to give sensible results when placing
!!    the markers.
!!
!!    If the environment variable "M_DRAW_FONTPATH" is set M_draw looks for the software
!!    fonts in the directory given by this value.
!!
!!    the default font is futura.l
!!
!!    TRANSFORMATION ROUTINES
!!    translate(x, y, z)          Set up a translation.
!!    scale(x, y, z)              Set up scaling factors in x, y, and z axis.
!!    rotate(angle, axis)         Set up a rotation in axis axis where axis is one of 'x','y', or 'z'.
!!
!!    All transformations are cumulative, so if you rotate something and
!!    then do a translate you are translating relative to the rotated
!!    axes. If you need to preserve the current transformation matrix use
!!    pushmatrix(), do the drawing, and then call popmatrix() to get back
!!    where you were before.
!!
!!    When doing transformations, ensure your objects remain in the viewing
!!    volume or they will be clipped. See routines such as ortho(3) for
!!    more information.
!!
!!    PATCH ROUTINES
!!    patchbasis(tbasis, ubasis)  Define the t and u basis matrices of a patch.
!!    patchprecision(tseg, useg)  Set minimum number of line segments making up curves in a patch.
!!    patchcurves(nt, nu)         Set the number of curves making up a patch.
!!    rpatch(gx, gy, gz, gw)      Draws a rational patch in the current basis, according to the geometry matrices gx, gy, gz, and gw.
!!    patch(gx, gy, gz)           Draws a patch in the current basis, according to the geometry matrices gx, gy, and gz.
!!
!!    POINT ROUTINES
!!    point(x, y, z)              Draw a point at x, y, z
!!    point2(x, y)                Draw a point at x, y.
!!
!!    points are drawn with the current color and linewidth. Points are
!!    currently device-specific and may appear as circles, squares, or
!!    not at all; as they are generated by a zero-length vector using the
!!    hardware line style.
!!
!!    OBJECT ROUTINES
!!    makeobj(n)                  Commence the object number n.
!!    closeobj()                  Close the current object.
!!    genobj()                    Returns a unique object identifier.
!!    getopenobj()                Return the number of the current object.
!!    callobj(n)                  Draw object number n.
!!    isobj(n)                    Returns non-zero if there is an object of number n.
!!    delobj(n)                   Delete the object number n.
!!    loadobj(n, filename)        Load the object in the file filename as object number n.
!!    saveobj(n, filename)        Save object number n into file filename. Does NOT save objects called inside object n.
!!
!!    invokeobj(xt,yt,zt,xs,ys,zs,xr,yr,zr,iobject)  push environment and do a transformation and then pop environment
!!
!!    Objects are graphical entities created by the drawing routines called
!!    between makeobj and closeobj. Objects may be called from within other
!!    objects. When an object is created most of the calculations required
!!    by the drawing routines called within it are done up to where the
!!    calculations involve the current transformation matrix. So if you need to
!!    draw the same thing several times on the screen but in different places
!!    it is faster to use objects than to call the appropriate drawing routines
!!    each time. Objects also have the advantage of being savable to a file,
!!    from where they can be reloaded for later reuse. Routines which draw
!!    or move in screen coordinates, or change device, cannot be included
!!    in objects.
!!
!!    DOUBLE BUFFERING
!!    backbuffer()                Draw in the backbuffer. Returns -1 if the device is not up to it.
!!    frontbuffer()               Draw in the front buffer. This will always work.
!!    swapbuffers()               Swap the front and back buffers.
!!
!!    Where possible M_draw allows for front and back buffers to enable
!!    things like animation and smooth updating of the screen. The routine
!!    backbuffer is used to initialise double buffering.
!!
!!    POSITION ROUTINES
!!    getgp(x, y, z)              Gets the current graphics position
!!    getgpt(x, y, z, w)          Gets the current transformed graphics position in world coords.
!!    getgp2(x, y)                Gets the current graphics position
!!    sgetgp2(x, y)               Gets the current screen graphics position in screen coords (-1 to 1)
!!
!!    GENERAL STACK ROUTINES
!!    subroutine pop()            pop
!!    subroutine push()           push
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_M_draw
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    integer  :: ipaws
!!    real     :: x1, y1
!!    integer  :: icolor
!!    integer  :: i,j
!!
!!       ! initialize image
!!       call prefsize(400,400)  ! set size before starting
!!       call vinit(' ')         ! start graphics using device $M_DRAW_DEVICE
!!       call textsize(10.0,10.0)
!!       call mapcolor( 0,   255,255,255 )  !white
!!       call mapcolor( 1,   255,  0,  0 )  !red
!!       call mapcolor( 2,     0,255,  0 )  !green
!!       call mapcolor( 3,   255,255,  0 )  !yellow
!!       call mapcolor( 4,     0,  0,255 )  !blue
!!       call mapcolor( 5,   255,  0,255 )  !magenta
!!       call mapcolor( 6,     0,255,255 )  !cyan
!!       call mapcolor( 7,     0,  0,  0 )  !black
!!       call mapcolor( 8,     0,155,  0 )
!!       call mapcolor( 9,   155,155,155 )
!!       call mapcolor(10,   155,255,255 )
!!       call mapcolor(11,   155,155,  0 )
!!       call mapcolor(12,     0,  0,155 )
!!       call mapcolor(13,   155,  0,155 )
!!       call mapcolor(14,     0,155,155 )
!!       call mapcolor(15,   100,100,100 )
!!       call mapcolor(16,   155,100,100 )
!!       call color(D_BLACK)
!!       call clear()            ! clear to color 0
!!       call color(D_WHITE)
!!
!!       ! map area of virtual world to specified device area
!!       ! notice Y-axis for viewport is zero at TOP
!!       ! define the virtual world area we want to work in
!!       call page(0.0,  400.0,    0.0, 400.0)
!!       ! the drawing routines use these world units
!!
!!       ! put some colored boxes into pixmap by address
!!       ! so show how the pixel map can be edited easily with
!!       ! other routines that can manipulate a pixel array.
!!       ! The P_pixel array was created when vinit(3f) was called
!!       call polyfill(.true.)
!!       icolor=1
!!       do i=0,3
!!          do j=0,3
!!             x1=j*100.0
!!             y1=i*100.0
!!             icolor=icolor+1
!!             call color(icolor)
!!             call rect(x1,y1,x1+100.0,y1+100.0)
!!          enddo
!!       enddo
!!       call polyfill(.false.)
!!
!!       ! draw polar grids
!!       call linewidth(100)
!!
!!       call linewidth(100)
!!       call color(14)
!!       call target(200.0,200.0,200.0)
!!
!!       call linewidth(75)
!!       call color(0)
!!       call target(100.0,200.0,50.0)
!!
!!       ! draw some lines
!!       call color(D_RED)
!!       call linewidth(200)
!!       call line(1.0,1.0,400.0,400.0)
!!
!!       call color(D_BLUE)
!!       call linewidth(250)
!!       call line(350.0,200.0,350.0,300.0)
!!
!!       ! print some text
!!       call color(1)
!!       call linewidth(125)
!!       call font('futura.l')
!!       call hershey(40.0, 40.0,35.0,'Hello World',0.0)
!!       call color(7)
!!       call linewidth(25)
!!       call hershey(40.0, 80.0,35.0,'Hello World',0.0)
!!       call linewidth(100)
!!       call hershey(40.0,120.0,35.0,'Hello World',30.0)
!!
!!       call hershey(  40.0,350.0,35.0,'Hello World',0.0)
!!       call font('futura.m')
!!       call hershey(  40.0,310.0,35.0,'Hello World',0.0)
!!       call font('times.r')
!!       call hershey( 350.0,400.0,35.0,'Hello World',90.0)
!!       call linewidth(50)
!!       call font('times.i')
!!       call hershey(200.0,120.0,15.0,'Hello World',20.0)
!!
!!       ipaws=getkey()
!!       call vexit()
!!
!!       contains
!!
!!       subroutine target(xc,yc,rc)
!!       real     :: xc,yc,rc
!!       integer  :: i
!!       real     :: x,y
!!          do i=0,360,10
!!             x=rc*cosd(real(i))
!!             y=rc*sind(real(i))
!!             call line(xc,yc,xc+x,yc+y)
!!          enddo
!!          do i=1,int(rc),10
!!             call circle(xc,yc,real(i))
!!          enddo
!!       end subroutine target
!!
!!       subroutine line(x1,y1,x2,y2)
!!       real,intent(in) :: x1,y1,x2,y2
!!       call move2(x1,y1)
!!       call draw2(x2,y2)
!!       end subroutine line
!!
!!       subroutine hershey(x,y,height,itext,theta)
!!       real,intent(in)               :: x,y
!!       real,intent(in)               :: height
!!       character(len=*),intent(in)   :: itext
!!       real,intent(in)               :: theta
!!       call move2(x,y)
!!       call textang(theta)
!!       call textsize(height,height)
!!       call drawstr(itext)
!!       end subroutine hershey
!!    end program demo_M_draw
!!
!!##BUGS
!!
!!    Polygon hatching will give unexpected results unless the polygon is
!!    initially defined in the X-Y plane.
!!
!!    Double buffering isn't supported on all devices.
!!
!!    We don't recommend the use of the smove/sdraw routines.
!!
!!    The yobbarays may be turned on or they may be turned off.
!!
!!    When creating an object, current position and text size are not
!!    actually changed so almost any query routine to get position or font
!!    size or whatever will not work properly.
!!
!!    If call vexit(3f) and then call vinit(3f) everything should probably
!!    be reset to initial values at program startup, such as linewidth,
!!    current font, and color. It is currently left up to the output device
!!    initialization routine. It should not be. To minimize the issue,
!!    call all the push* routines after a vinit(3f) and call all the pop*
!!    routines before vexit(3f).
!!
!!    Exactly what attributes should and should not be reset with a
!!    vnewdev(3f) is questionable.
!>
!!##NAME
!!    prefposition(3f) - [M_draw:WINDOW_SETUP] Specify preferred position of window
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine prefposition(x, y)
!!
!!     integer,intent(in) :: x, y
!!
!!##DESCRIPTION
!!    Specify the preferred position of the window opened by the *next*
!!    vinit in raster units or units of resolution, depending on the output
!!    device. The physical size of the units are generally device-specific --
!!    For X11 Windows and PPM pixmaps the values would represent rasters.
!!    For vector output devices the number of "rasters" per inch varies.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_prefposition
!!      use M_draw, only    : prefsize, vinit, ortho2, clear, getkey
!!      use M_draw, only    : prefposition, move2, draw2, vexit, color
!!      use M_draw,    only  : D_BLACK,   D_WHITE
!!      use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!      use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!      implicit none
!!      integer :: ipaws
!!
!!      call prefsize(60,40)
!!      call prefposition(100,100)
!!
!!      call vinit(' ')         ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-300.0,300.0,-200.0,200.0)
!!      call color(D_BLACK)
!!      call clear()
!!      call color(D_RED)
!!      call move2(-300.0,-200.0)
!!      call draw2(300.0,200.0)
!!      call move2(300.0,-200.0)
!!      call draw2(-300.0,200.0)
!!      ipaws=getkey()
!!      call vexit()
!!
!!      end program demo_prefposition
!>
!!##NAME
!!    prefsize(3f) - [M_draw:WINDOW_SETUP] Specify preferred width and
!!    height of window in physical units
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine prefsize(width, height)
!!         integer,intent(in) :: width, height
!!
!!##DESCRIPTION
!!
!!    Specify the preferred width and height of the device output surface
!!    opened by the *next* vinit(3f).
!!
!!##OPTIONS
!!    WIDTH   width of device to create when vinit(3f) is called
!!    HEIGHT  height of device to create when vinit(3f) is called
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_prefsize
!!      use M_draw, only: prefsize, vinit, ortho2, clear, getkey
!!      use M_draw, only: move2, draw2, vexit, color
!!      use M_draw,    only  : D_BLACK,   D_WHITE
!!      use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!      use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!      implicit none
!!      integer :: ipaws
!!         ! make first file with one size
!!         call prefsize(60*2,40*2)
!!         call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!         call picture()
!!         ipaws=getkey()
!!         call vexit()
!!
!!         ! make second file with another size
!!         call prefsize(60*3,40*3)
!!         call vinit(' ')
!!         call picture()
!!         ipaws=getkey()
!!         call vexit()
!!      contains
!!      subroutine picture
!!         call ortho2(-300.0,300.0,-200.0,200.0)
!!         call color(D_BLACK)
!!         call clear()
!!         call color(D_RED)
!!         call move2(-300.0,-200.0)
!!         call draw2(300.0,200.0)
!!         call move2(300.0,-200.0)
!!         call draw2(-300.0,200.0)
!!      end subroutine picture
!!      end program demo_prefsize
!>
!!##NAME
!!    vinit(3f) - [M_draw:DEVICE] Initialise device
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine vinit(device)
!!          character(len=*),intent(in) :: device
!!
!!##DESCRIPTION
!!    Initialise the device.
!!
!!
!!  Note 1 : Currently available devices are:
!!
!!        INTERACTIVE DEVICES
!!
!!        PC   - native MSW PC driver; only tested from CygWin
!!        X11  - X11 windows (Black background)
!!        x11  - X11 windows (White background)
!!        tek  - tektronix 4010 and compatibles
!!        xtek - X11 xterm Tektronix 4010 emulator
!!
!!        PRINTERS AND PLOTTERS
!!
!!           PostScript:
!!              [p]psm or [p]postscript - monochrome PostScript
!!              [p]psg - grayscale PostScript
!!              [p]psc - color PostScript
!!           HPGL:
!!              hpgl - HP Graphics language and compatibles
!!              [p]hpgl2 - HPGL level 2 (obeys prefsize calls)
!!           PCL:
!!              [p]pclland  - monochrome PCL5 (obeys prefsize calls)
!!              [p]pclport  - monochrome PCL5 (obeys prefsize calls)
!!              pcl5land (color PCL5 landscape)
!!              pcl5port (color PCL5 portrait)
!!
!!        PIXMAPS (COLOR) AND BITMAPS (MONOCHROME)
!!
!!        char     An ASCII file that can be displayed to most
!!                 xterm(1) terminal emulators that support
!!                 color
!!        p1/pbm   Poskanzer (pbmplus/netplus) portable
!!                 ASCII bitmap file
!!        p3/ppm   Poskanzer portable ASCII pixmap file
!!        p4       Poskanzer portable binary bitmap file
!!        p6       Poskanzer portable binary pixmap file
!!        xbm      X11 bitmap file
!!        bm       bitmap format for atobm(1)
!!
!!        PRODUCT INPUT FILES
!!
!!        mif        FrameMaker Interchange Format 3.0 (MIF) files
!!                   (16 colors)
!!        mif4       FrameMaker Interchange Format 4.0 (MIF) files
!!                   (user-definable colors, but breaks a MIF rule)
!!        xfig       X11 xfig(1) figure utility
!!
!!        METAFILES WITH POST_PROCESSORS/CONVERTERS
!!
!!        cgmt      a clear-text CGM (Computer Graphics Metafile)
!!        gnu       GNU plot(1) metafile
!!        pdf       Adobe Public Document Format
!!        unixplot  Unix plot(1) metafile
!!
!!        BROWSER FILES
!!
!!        canvas    HTML5 CANVAS graphics element file
!!        svg       Scalable Vector Graphics
!!        usemap    HTML image map
!!        vml       Microsoft Vector Markup Language
!!
!!        FILES
!!        vog       M_draw low level call record (debug)
!!
!!        OTHER
!!        fti       SGI vector-based icons
!!        null      no output
!!
!!      :- Drivers I've dropped but code is there for
!!      ---------------------------------------------
!!
!!        grwin - (minGW GRwin PC interface)
!!        decX11 - the decstation window manager
!!        dxy - roland DXY plotter language
!!        sun - Sun workstations running sunview
!!        next - NeXTStep and other NeXT platforms
!!        apollo - Apollo workstations
!!
!!      :- Drivers I've dropped but are in the original distribution
!!      ------------------------------------------------------------
!!
!!        hercules - IBM PC hercules graphics card
!!        cga - IBM PC cga graphics card
!!        ega - IBM PC ega graphics card
!!        vga - IBM PC vga graphics card
!!        sigma - IBM PC sigma graphics card.
!!        mswin - IBM PC Microsoft Windows.
!!
!!     Note 2 : If device is a NULL or a null string the value
!!          of the environment variable "M_DRAW_DEVICE" is taken as the
!!          device type to be opened. The format of the variable is
!!
!!             M_DRAW_DEVICE [ xsize [ ysize [ xoffset [ yoffset ] ] ]
!!
!!          That is, if present xsize and ysize will be used
!!          in a call to prefsize(3c), and xoffset and yoffset will
!!          be used in a call to preposition(3c).
!!
!!     Note 3 : after vinit() it is wise to explicitly clear the screen.
!!     e.g.:
!!
!!          call color(D_BLACK)
!!          call clear()
!!
!!##NOTES
!!
!! gnu
!!
!!    The GNU plotutils package includes a program called plot(1) that
!!    can read in the gnu metafile and render images on an X11 display, PNG
!!    (Portable Network Graphics) format, portable anymap format (PBM/PGM/PPM),
!!    a pseudo-GIF format that does not use LZW encoding, the new XML-based
!!    Scalable Vector Graphics format, the format used by Adobe Illustrator,
!!    Postscript or Encapsulated Postscript (EPS) that can be edited with
!!    idraw(1), CGM format (by default, confirming to the WebCGM profile),
!!    the format used by the xfig(1) drawing editor, the Hewlett-Packard PCL 5
!!    printer language, the Hewlett-Packard Graphics Language, ReGIS graphics
!!    format (which can be displayed by the dxterm(1) terminal emulator or
!!    by a VT330 or VT340 terminal), Tektronix format (which can be displayed
!!    by the xterm(1) terminal emulator), and device-independent GNU metafile
!!    format itself.
!!
!! pdf
!!
!!    Popular PDF readers are the Adobe PDF viewer, which is often callable from
!!    Web browsers; the GhostScript-based gv(1) utility; or the xpdf program.
!!
!!        The xpdf(1) software , related utilities ( pdftops(1), pdftotext(1),
!!        pdfinfo(1), pdffonts(1), pdftoppm(1), pdfimages(1), xpdfrc (5))
!!        and documentation are copyright 1996-2004 Glyph & Cog, LLC. at
!!        http://www.foolabs.com/xpdf/
!!
!!    The GhostScript-based tools can convert PDF files to PostScript as
!!    well as view the files.
!!
!! cgmt
!!
!!    The ralcgm(1) and gplot(1) packages are two very complete CGM viewers.
!!
!!    ppm,pbm (and p1,p3,p4,p6)
!!
!!      * p1/pbm - Poskanzer (pbmplus/netplus) portable ASCII bitmap file
!!      * p3/ppm - Poskanzer portable ASCII pixmap file
!!      * p4 - Poskanzer portable binary bitmap file
!!      * p6 - Poskanzer portable binary pixmap file
!!
!!    The NetPBM package is available for almost every platform and lets
!!    you convert the Poskanzer portable pixmap (PPM) files to just about
!!    any pixmap or bitmap format, including PNG, JPEG, GIF/PseudoGIF, BPM,
!!    ..... Other popular pixmap products such as ImageMagick, gv, ... can
!!    read PPM files, convert them, and often edit them.
!!
!!##HTML:
!!
!!    The vml, canvas, svg, and usemap drivers are primarily used to generate
!!    graphics for inclusion in HTML documents. Browsers such as Opera, Safari,
!!    Foxfire, and Chrome can easily incorporate graphics generated using the
!!    SVG (Scalable Vector Graphics) format or the HTML5 CANVAS element.
!!
!! usemap
!!
!!    This driver writes out the edges of any polygon in a format that can be
!!    used with an HTML image map; if the same sizes are used a plot generated
!!    with the ppm driver; you will have clickable regions in your pixmap when
!!    converted to a GIF image.
!!
!!    If the polygons overlap you need to reverse the order of the polygon
!!    definitions in the output file. The numeric field in the<AREA> titles
!!    should help.
!!
!! vml
!!
!!    The VML format can be read in by any MicroSoft Office 2000+ product and
!!    MicroSoft's web browser Internet Explorer. If the plots contain more than
!!    about 9766 vectors MicroSoft Word starts choking (still true in 2005),
!!    but otherwise this is a very nice way to generate input for MicroSoft
!!    products.
!!
!!    I generally use this on a machine running MicroSoft Windows by installing
!!    CygWin with the X11 options (and ralcgm, the GhostScript software,
!!    the GNU plotutils packages and netpbm).
!!
!! xfig
!!
!!    The xfig(1) command can be used to edit graphics generated with the
!!    M_draw graphics library; and to convert the xfig(1)-format file to
!!    many other output formats. If you are generating pixmaps with the PPM
!!    driver and want to use them as image maps in your HTML documents the
!!    usemap driver can be used.
!!
!!    If you have xfig(1) installed, you will find that calling fig2dev(1)
!!    allows you to generate many output formats from a single file,
!!    including LaTex and encapsulated PostScript.
!!
!!    xfig(1) is an X11 Windows application that can be used to interactively
!!    edit figures. The HELP utility of xfig(1) provides a description of the
!!    xfig(1) file format (as well as a user guide and many other documents).
!!
!!    Unfortunately, the manual indicates the user defined colors must
!!    be defined before any other Fig objects. By default, 16 colors are
!!    defined. If undefined colors are used they are assigned a dash pattern
!!    or a fill pattern to help distinguish them. Use of hardware dash and
!!    M_draw software dash could get confusing.
!!
!!    Also, in the current driver version all lines are drawn as a series
!!    of move-draw vectors, which can make the files relatively very large.
!!
!!    multiple pages appear to only work with the PostScript and PDF drivers
!!    of xfig(1); and even then pages must be all positive numbers from left
!!    to right and top to bottom, printing all pages in a rectangular area.
!!
!!    Alternatively, could use depth to keep up to 999 pages separate
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!       program demo_vinit
!!       use M_draw
!!       use ISO_C_BINDING
!!       implicit none
!!       integer :: ios, idum
!!       character(len=50) :: device
!!
!!       ! read in device name and start graphics mode
!!       print*,'Enter output device:'
!!       read(*,'(a)',iostat=ios)device
!!       if(ios.ne.0)device=' '
!!       call vinit(device)
!!       ! by default the drawing surface is
!!       ! a square ranging from -1 to 1 in both
!!       ! the X and Y axis
!!
!!       ! set font to large hardware font
!!       call font('large')
!!
!!       ! set current color to black
!!       call color(D_BLACK)
!!
!!       ! clear to current color
!!       call clear()
!!
!!       ! we want to draw in green
!!       call color(D_GREEN)
!!
!!       ! draw a horizontal line at y = 0
!!       call move2(-1.0, 0.0)
!!       call draw2(1.0, 0.0)
!!
!!       ! pause for some input
!!       idum=getkey()
!!
!!       ! draw a line along x = 0
!!       call move2(0.0, 0.0)
!!       call draw2(0.0, 1.0)
!!
!!       ! move to the middle of the screen
!!       call move2(0.0, 0.0)
!!
!!       ! draw 'Hello' starting at the origin
!!       call drawstr('Hello')
!!
!!       ! pause again
!!       idum=getkey()
!!
!!       !  wrap up and exit graphics mode
!!       call vexit()
!!
!!       end program demo_vinit
!>
!!##NAME
!!    vexit(3f) - [M_draw:DEVICE] Reset window/terminal and exit graphics mode. Must be last routine called.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine vexit()
!!
!!##DESCRIPTION
!!
!!    Reset the window/terminal and terminate graphics mode. Required to
!!    properly close out most output devices. Must be called before any
!!    second call to vinit(3f). Must be the last M_draw routine called.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_vexit
!!      use M_draw, only: vinit, vexit, voutput, circle, linewidth, color
!!      use M_draw,    only  : D_BLACK,   D_WHITE
!!      use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!      use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!      implicit none
!!      integer :: i
!!
!!      ! assuming you have the NetPBM package installed
!!      ! set up output to create a GIF file called one.gif
!!      call voutput('|ppmtogif >vexit.3m_draw.gif')
!!
!!      call vinit('p6') ! start graphics
!!      ! default window is -1 <= x <= 1, -1 <= y <= 1
!!      ! default viewport is left bottom justified square
!!      ! so essentially you have a square drawing surface
!!      ! with the point <0,0> at the center of the square
!!      ! with X and Y ranging from -1 to 1.
!!
!!      call color(D_RED)
!!      call linewidth(100)
!!      ! this loop draws outside the current window
!!      ! but by default clipping is on
!!      do i=1,30
!!         call circle((real(i)-1)*0.04,0.0,1.0/real(i))
!!      enddo
!!
!!      call vexit() ! exited graphics so can start again
!!
!!      ! start second graphics session with new output
!!      ! device and output file
!!      call voutput('|ppmtogif >vexit2.3m_draw.gif')
!!      call vinit('p6')
!!
!!      do i=10,1,-1
!!         call color(i)
!!         call circle(0.0,0.0,1.0/real(i))
!!      enddo
!!
!!      call vexit()
!!
!!      end program demo_vexit
!>
!!##NAME
!!    voutput(3f) - [M_draw:DEVICE] Redirect output from *next* vinit to file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine voutput(path)
!!          character*(*) path
!!##DESCRIPTION
!!
!!       Redirect output from *next* vinit() to file given by path. This routine
!!       only applies to device drivers that write to stdout e.g. PostScript and
!!       hpgl.
!!
!!       The special file names are
!!
!!         * -  is standard output
!!         * +  is standard error
!!         * |  command will create a pipe to "command"
!!
!!       If the open of the file fails, an attempt is made to append to file
!!       "M_DRAW_OUTPUT". If this fails, standard output is used.
!!
!!       When vinit() is called if voutput() has not been called then the
!!       environment variable M_DRAW_OUTPUT is checked and if it is defined and not a
!!       null string then voutput() is called with the M_DRAW_OUTPUT variable's value.
!!
!!       A common use of the |command option is to automatically call programs
!!       that convert PPM files to other common pixmap formats or converts the GNU
!!       metafile to other formats (typically via the GNU plotutils plot program).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_voutput
!!    use M_draw
!!    implicit none
!!    ! want a 400x400 raster output
!!    call prefsize(400,400)
!!    ! convert PPM to a GIF file using ppmtogif(1)
!!    call voutput('|ppmtogif >voutput.3m_draw.gif')
!!    ! start graphics using PPM device
!!    call vinit('p6')
!!    ! draw a filled circle
!!    call color(D_RED)
!!    call polyfill(.true.)
!!    call circle(0.0,0.0,1.0)
!!    !
!!    call vexit()
!!    end program demo_voutput
!>
!!##NAME
!!    vnewdev(3f) - [M_draw:DEVICE] Reinitialize to use new device without changing
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!          subroutine vnewdev(device)
!!          character *(*) device
!!##DESCRIPTION
!!
!!    Reinitialize M_draw to use a new device without changing attributes,
!!    viewport etc. (eg. window and viewport specifications)
!>
!!##NAME
!!    vgetdev(3f) - [M_draw:DEVICE] Get name of current device
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!          subroutine vgetdev(device)
!!          character *(*) device
!!##DESCRIPTION
!!
!!    Gets the name of the current M_draw device. The C version of the routine
!!    also returns a pointer to its argument.
!>
!!##NAME
!!    getdepth(3f) - [M_draw:DEVICE] Return number of bit planes (color planes)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          integer function getdepth()
!!
!!##DESCRIPTION
!!    Returns the number of bit planes (or color planes) for a particular
!!    device. The number of colors displayable by the device is then
!!    2**(nplanes); ie. if nplanes=1,then there are two colors (black and
!!    white).
!>
!!##NAME
!!    pushdev(3f) - [M_draw:DEVICE] push current device onto a stack
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine pushdev(device)
!!          character *(*) device
!!
!!##DESCRIPTION
!!    Initialize a new device without changing attributes, viewport etc,
!!    but save the previously initialised device on a stack.
!!
!!    Note, this is intended to completely change the device, it will not work
!!    if you pushdev() the same device that you are already running. (This will
!!    be fixed at a later date).
!>
!!##NAME
!!    popdev(3f) - [M_draw:DEVICE] pop device from stack created by pushdev.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!           subroutine popdev()
!!
!!##DESCRIPTION
!!
!!    Pops a device off the device stack and reinstates the previously pushed
!!    device.
!>
!!##NAME
!!    move(3f) - [M_draw:MOVE] Move current graphics position to (x, y, z)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine move(x, y, z)
!!          real x, y, z
!!##DESCRIPTION
!!
!!    Move current graphics position to (x, y, z). (x, y, z) is a point in
!!    world coordinates.
!>
!!##NAME
!!    rmove(3f) - [M_draw:MOVE] Relative move
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine rmove(deltax, deltay, deltaz)
!!          real deltax, deltay, deltaz
!!
!!##DESCRIPTION
!!
!!    Relative move. deltax, deltay, and deltaz are offsets in world units.
!>
!!##NAME
!!    move2(3f) - [M_draw:MOVE] Move graphics position to point (x, y)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine move2(x, y)
!!          real,intent(in) :: x, y
!!
!!##DESCRIPTION
!!
!!    Update current position.
!!    Move graphics position to point (x, y). (x, y) is a point in world
!!    coordinates.
!!
!!##OPTIONS
!!    X  new X position
!!    Y  new Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_move2
!!      use M_draw, only : prefsize, vinit, ortho2, clear, getkey
!!      use M_draw, only : move2, draw2, vexit, color
!!      use M_draw,    only  : D_BLACK,   D_WHITE
!!      use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!      use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!      implicit none
!!      integer :: ipaws
!!      call prefsize(60,40)
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-300.0,300.0,-200.0,200.0)
!!      call color(D_BLACK)
!!      call clear()
!!      call color(D_WHITE)
!!      call move2(-300.0,-200.0)
!!      call draw2(300.0,200.0)
!!      call move2(300.0,-200.0)
!!      call draw2(-300.0,200.0)
!!      ipaws=getkey()
!!      call vexit()
!!      end program demo_move2
!>
!!##NAME
!!    rmove2(3f) - [M_draw:MOVE] Relative move in world units.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     subroutine rmove2(deltax, deltay)
!!     real,intent(in) :: deltax, deltay
!!
!!##DESCRIPTION
!!    Update current position.
!!    Relative move2. deltax and deltay are offsets in world units.
!!
!!##OPTIONS
!!    X  new X position
!!    Y  new Y position
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_rmove2
!!      use M_draw, only: prefsize, vinit, ortho2, clear, getkey
!!      use M_draw, only: move2, rmove2, rdraw2, vexit
!!      use M_draw, only: linewidth
!!      implicit none
!!      integer :: ipaws
!!      integer :: i
!!      call prefsize(500,500)
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-110.0,110.0,-110.0,110.0)
!!      call move2(-100.0,-100.0)
!!      call linewidth(70)
!!      do i=1,20
!!         call rmove2(10.0, 0.0)
!!         call rdraw2( 0.0,10.0)
!!      enddo
!!      ipaws=getkey()
!!      call vexit()
!!      end program demo_rmove2
!>
!!##NAME
!!    smove2(3f) - [M_draw:MOVE] Move current graphics position in screen coordinates (-1.0 to 1.0).
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine smove2(x, y)
!!          real x, y
!!
!!##DESCRIPTION
!!    Move current graphics position in screen coordinates (-1.0 to 1.0).
!>
!!##NAME
!!    rsmove2(3f) - [M_draw:MOVE] Relative move in screen units (-1.0 to 1.0).
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine rsmove2(deltax, deltay)
!!          real deltax, deltay
!!
!!##DESCRIPTION
!!
!!    Relative smove2. deltax, and deltay are offsets in screen units
!!    (-1.0 to 1.0).
!>
!!##NAME
!!    draw(3f) - [M_draw:DRAW] Draw from current graphics position to (x, y, z)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine draw(x, y, z)
!!          real x, y, z
!!##DESCRIPTION
!!
!!    Draw from current graphics position to (x, y, z). (x, y, z) is a
!!    point in world coordinates.
!>
!!##NAME
!!    rdraw(3f) - [M_draw:DRAW] Relative draw
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     subroutine rdraw(deltax, deltay, deltaz)
!!     real deltax, deltay, deltaz
!!
!!##DESCRIPTION
!!    Relative draw. deltax, deltay, and deltaz are offsets in world units.
!>
!!##NAME
!!    draw2(3f) - [M_draw:DRAW] Draw from current graphics position to given point (x, y)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     subroutine draw2(x, y)
!!     real,intent(in) :: x, y
!!
!!##DESCRIPTION
!!    Draw from current position to specified point using current
!!    color and line width. Updates current position to new point.
!!    (x, y) is a point in world coordinates.
!!
!!##OPTIONS
!!    X  new X position
!!    Y  new Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_draw2
!!    use M_draw, only: prefsize, vinit, ortho2, clear, getkey
!!    use M_draw, only: move2, draw2, vexit, color, linewidth
!!    use M_draw, only: D_BLACK, D_MAGENTA! , &
!!    !        D_RED, D_GREEN, D_BLUE, D_YELLOW, D_WHITE, D_CYAN
!!    !
!!    ! The Archimedean spiral is the locus of points corresponding
!!    ! to the locations over time of a point moving away from a
!!    ! fixed point with a constant speed along a line which rotates
!!    ! with constant angular velocity.
!!    !    r=A+B*theta
!!    ! Changing the parameter A will turn the spiral,
!!    ! while B controls the distance between successive turnings.
!!    !
!!    implicit none
!!    integer        :: i
!!    real           :: x, y, radius, theta
!!    real, parameter :: rotate = 0.0, gap = 2.0
!!    integer        :: ipaws
!!
!!       call prefsize(400, 400)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call ortho2(-150.0, 150.0, -150.0, 150.0)
!!       call color(D_MAGENTA)
!!       call clear()
!!       call move2(0.0, 0.0)
!!       call color(D_BLACK)
!!       call linewidth(40)
!!       do i = 0, 360*10, 5
!!          theta = d2r(real(i))
!!          ! equation in polar coordinates
!!          radius = rotate + gap*theta
!!          ! convert polar coordinates to cartesian
!!          call polar_to_cartesian(radius, theta, x, y)
!!          ! draw from current position to end of next segment
!!          call draw2(x, y)
!!       end do
!!       ipaws = getkey()
!!       ! exit graphics mode
!!       call vexit()
!!    contains
!!    !
!!    elemental real function d2r(degrees)
!!    real, intent(in) :: degrees
!!    real, parameter  :: Deg_Per_Rad = 57.2957795130823208767981548
!!       d2r = degrees/Deg_Per_Rad
!!    end function d2r
!!    !
!!    subroutine polar_to_cartesian(radius, inclination, x, y)
!!    real, intent(in) :: radius, inclination
!!    real, intent(out)  :: x, y
!!       if (radius == 0) then
!!          x = 0.0
!!          y = 0.0
!!       else
!!          x = radius*cos(inclination)
!!          y = radius*sin(inclination)
!!       end if
!!    end subroutine polar_to_cartesian
!!    !
!!    end program demo_draw2
!>
!!##NAME
!!    rdraw2(3f) - [M_draw:DRAW] Relative draw from current position to given point
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine rdraw2(deltax, deltay)
!!          real,intent(in) :: deltax, deltay
!!
!!##DESCRIPTION
!!    Relative draw from current position to specified point using current
!!    color and line width. Updates current position to new point.
!!    (x, y) is a point in world coordinates.
!!
!!##OPTIONS
!!    deltax and deltay are offsets in world units.
!!
!!    X  new X position
!!    Y  new Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_rdraw2
!!      use M_draw, only: vinit, prefsize, ortho2,linewidth,getkey
!!      use M_draw, only: clear, move2, rdraw2, vexit,color
!!      use M_draw,    only  : D_BLACK,   D_WHITE
!!      use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!      use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!      implicit none
!!      integer :: ipaws
!!
!!      call prefsize(200,200)
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-55.0, 55.0, -55.0, 55.0)
!!      call linewidth(400)
!!      call color(D_WHITE)
!!      call clear()
!!
!!      call color(D_RED)
!!      call move2(-50.0,0.0)
!!      call square(50.0)
!!
!!      call linewidth(200)
!!      call color(D_GREEN)
!!      call move2(  0.0,-50.0)
!!      call square(50.0)
!!
!!      ipaws=getkey()
!!      call vexit()
!!
!!      contains
!!
!!      subroutine square(side)
!!      real,intent(in) :: side
!!      call rdraw2( side,   0.0)
!!      call rdraw2(  0.0,  side)
!!      call rdraw2(-side,   0.0)
!!      call rdraw2(  0.0, -side)
!!      end subroutine square
!!
!!      end program demo_rdraw2
!>
!!##NAME
!!    sdraw2(3f) - [M_draw:DRAW] Draw in screen coordinates (-1.0 to 1.0).
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine sdraw2(x, y)
!!          real x, y
!!##DESCRIPTION
!!    Draw in screen coordinates (-1.0 to 1.0).
!>
!!##NAME
!!    rsdraw2(3f) - [M_draw:DRAW] Relative draw in screen units (-1.0 to 1.0).
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine rsdraw2(deltax, deltay)
!!          real deltax, deltay
!!
!!##DESCRIPTION
!!    Relative sdraw2. delatx and deltay are in screen units (-1.0 to 1.0).
!>
!!##NAME
!!    rect(3f) - [M_draw:POLYGONS] Draw a rectangle given two corners
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine rect(x1, y1, x2, y2)
!!       real,intent(in) :: x1,y1,x2,y2
!!
!!##DESCRIPTION
!!    Draw rectangle given two opposite corners.
!!
!!    Note: rectangles are regarded as polygons, so if
!!    polyfill or polyhatch has been called with .TRUE., the rectangle will
!!    be filled or hatched accordingly.
!!
!!##OPTIONS
!!   Given
!!
!!       x1,y1 ############ x2,y1
!!             #          #
!!             #          #
!!             #          #
!!       x1,y2 ############ x2,y2
!!
!!    X1,Y1  coordinates of a corner of the rectangle
!!    X2,Y2  coordinates of corner point opposite first point
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_rect
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    integer :: ipaws
!!    real    :: b=0.2
!!
!!    !! set up graphics area
!!    call prefsize(1000,200)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!
!!    call linewidth(150)
!!    call color(D_RED)
!!    call rect(-24.0, -4.0, -12.0, 4.0)
!!    call polyfill(.true.)
!!    call color(D_GREEN)
!!    call rect(-10.0, -4.0, -2.0, 4.0)
!!    call polyhatch(.true.)
!!    call hatchpitch(0.4)
!!    call hatchang(30.0)
!!    call linewidth(20)
!!    call color(D_BLUE)
!!    call rect(0.0, -4.0, 20.0, 3.0)
!!    call linewidth(200)
!!    call color(D_BLUE)
!!    call move2(-25.0, -5.0)
!!    call draw2(-25.0, 5.0)
!!    call draw2(25.0, 5.0)
!!    call draw2(25.0, -5.0)
!!    call draw2(-25.0, -5.0)
!!
!!    !! pause
!!    call vflush()
!!    ipaws=getkey()
!!
!!    !! wrap up graphics
!!    call vexit()
!!
!!    end program demo_rect
!>
!!##NAME
!!      polyfill(3f) - [M_draw:POLYGONS] Set the polygon fill flag
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine polyfill(onoff)
!!       logical onoff
!!
!!##DESCRIPTION
!!    Set the polygon fill flag. This will always turn off hatching. A
!!    LOGICAL .true. turns polyfill on.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_polyfill
!!    ! Using polygons, hatching, and filling.
!!       use M_draw, only : vinit, color, clear, ortho, boxtext, vexit
!!       use M_draw, only : polyfill
!!       use M_draw,    only  : D_BLACK,   D_WHITE
!!       use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!       use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!       implicit none
!!       integer           :: ios
!!       character(len=50) :: device
!!       print*,'Enter output device:'
!!       read(*,'(a)',iostat=ios)device
!!       if(ios.ne.0)device=' '
!!       call vinit(device)
!!    ! clear to black
!!       call color(D_BLACK)
!!       call clear()
!!    ! world coordinates are now in the range -10 to 10
!!    ! in x, y, and z. Note that positive z is towards us.
!!       call ortho(-10.0, 10.0, -10.0, 10.0, 10.0, -10.0)
!!       call color(D_YELLOW)
!!    ! write out the string "Polygon from poly()" in the
!!    ! starting at (-8.0, -4.0) and scaled to be 4.0 units long,
!!    ! 0.5 units high.
!!       call boxtext(-8.0, -4.0, 4.0, 0.5, 'Polygon from poly()')
!!       call color(D_GREEN)
!!    ! write out a scaled string starting at (0.0, 6.0)
!!       call boxtext(0.0, 6.0, 4.0, 0.5, 'Polygon from move()/ draw()')
!!       call color(D_MAGENTA)
!!    ! write out a scaled string starting at (0.0, 6.0)
!!       call boxtext(3.5, -3.5, 1.9, 0.5, 'Sector')
!!    ! turn on polygon filling - this automatically turns off hatching
!!       call polyfill(.true.)
!!    ! draw some polygons with filling
!!       call my_drawpoly()
!!       call vexit()
!!    contains
!!    subroutine my_drawpoly()
!!       use M_draw, only : color, move, draw, closepoly, sector, getkey
!!       use M_draw, only : poly, closepoly, sector, makepoly
!!       use M_draw,    only  : D_BLACK,   D_WHITE
!!       use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!       use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!       real parray(3,4)                   ! An array of points for a polygon
!!       integer :: idum
!!       data parray/ -8.0, -8.0, 0.0,  &
!!       & -5.0, -8.0, 0.0,  &
!!       & -5.0, -5.0, 0.0,  &
!!       & -8.0, -5.0, 0.0 /
!!       call color(D_YELLOW)
!!    ! Draw a polygon using poly, parray is our array of
!!    ! points and 4 is the number of points in it.
!!       call poly(4, parray)
!!       call color(D_GREEN)
!!    ! Draw a 5 sided figure by using move, draw and closepoly.
!!       call makepoly()
!!       call move(0.0, 0.0, 0.0)
!!       call draw(3.0, 0.0, 0.0)
!!       call draw(3.0, 4.0, 0.0)
!!       call draw(-1.0, 5.0, 0.0)
!!       call draw(-2.0, 2.0, 0.0)
!!       call closepoly()
!!       call color(D_MAGENTA)
!!    ! draw a sector representing a 1/4 circle
!!       call sector(1.5, -7.0, 3.0, 0.0, 90.0)
!!       idum=getkey()
!!    end subroutine my_drawpoly
!!    end program demo_polyfill
!>
!!##NAME
!!    polyhatch(3f) - [M_draw:POLYGONS] Set the polygon hatch flag
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     subroutine polyhatch(onoff)
!!     logical onoff
!!##DESCRIPTION
!!
!!    Set the polygon hatch flag. This will always turn off fill. A
!!    LOGICAL .true. turns polyhatch on. Note that hatched polygons
!!    must initially be defined parallel to the X-Y plane.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_polyhatch
!!    use M_draw
!!    use M_draw, only: D_BLACK, D_WHITE
!!    use M_draw, only: D_RED, D_GREEN, D_BLUE
!!    use M_draw, only: D_YELLOW, D_MAGENTA, D_CYAN
!!    implicit none
!!    integer :: key
!!    real :: N = 11.0
!!       call prefsize(600*10/6, 200*10/6)
!!       call vinit(' ')
!!       call page(-15.0, 15.0, -5.0, 5.0)
!!       call linewidth(100)
!!       call color(D_BLACK)
!!       call clear()
!!       call color(D_RED)
!!       call spirograph(-10.0, 0.0, N, 1.0, N, 5.0, 1000, 0.0, 0.0, 0)
!!       call polyhatch(.true.) ! turn on polygon hatching
!!       call hatchang(45.0)
!!       call hatchpitch(0.3)
!!       call color(D_GREEN)
!!       call spirograph(10.0, 0.0, N, 1.0, N, 5.0, 1000, 0.0, 0.0, 2)
!!       call vflush()
!!       key = getkey()
!!       call vexit()
!!    contains
!!    subroutine spirograph(xc,yc,sun,planet0,offset0,rad,ilines,ang,angs,ifill)
!!    real, parameter :: PI=3.14159265358979323846264338327950288419716939937510
!!    ! center of curve
!!    real, intent(in)    :: xc, yc
!!    ! radii of sun, planet, and planet offset
!!    real, intent(in)    :: sun, planet0, offset0
!!    ! radius to fit the shape to (no fit if radius is 0)
!!    real, intent(in)    :: rad
!!    ! number of points to sample along curve
!!    integer, intent(in) :: ilines
!!    ! angle to rotate the shape by, to orientate it.
!!    real, intent(in)    :: ang
!!    ! angle to start sampling points at; ccw is +; 0 is East
!!    real, intent(in)    :: angs
!!    ! 1 make a filled polygon, 2 make a hatched polygon
!!    integer, intent(in) :: ifill
!!    real                :: ang1, con1, con2, factor, offset, planet
!!    real                :: r, sunr, u, xpoin, xpoin1, ypoin, ypoin1
!!    integer             :: i10
!!       sunr = sun
!!       offset = offset0
!!       planet = planet0
!!       if (ilines  ==  0) return
!!       if (planet  ==  0.0) return
!!       if (sunr  ==  0.0) return
!!       if (rad  /=  0 .and. sunr - planet + offset  /=  0) then
!!          factor = rad/(sunr - planet + offset)
!!          sunr = factor*sunr
!!          planet = factor*planet
!!          offset = factor*offset
!!       end if
!!       u = 0.0 + ang
!!       con1 = PI*2.*(sunr/planet)/real(ilines)
!!       con2 = (1.0 - planet/sunr)*u
!!       xpoin1 = (sunr - planet)*cos(planet*u/sunr) + offset*cos(con2)
!!       ypoin1 = (sunr - planet)*sin(planet*u/sunr) - offset*sin(con2)
!!       ang1 = atan2(ypoin1, xpoin1) + angs
!!       r = sqrt(xpoin1**2 + ypoin1**2)
!!       xpoin1 = r*cos(ang1) + xc
!!       ypoin1 = r*sin(ang1) + yc
!!       select case (ifill)
!!       case (0)
!!       case (1)
!!          call polyfill(.true.)
!!          call makepoly()
!!       case (2)
!!          call polyhatch(.true.)
!!          call makepoly()
!!       case (3:)
!!          call makepoly()
!!       case default
!!       end select
!!       call move2(xpoin1, ypoin1)
!!       do i10 = 1, ilines
!!          u = con1*i10 + ang
!!          con2 = (1.0 - planet/sunr)*u
!!          if (con2  >=  2**24) con2 = amod(con2, PI)
!!          xpoin = (sunr - planet)*cos(planet*u/sunr) + offset*cos(con2)
!!          ypoin = (sunr - planet)*sin(planet*u/sunr) - offset*sin(con2)
!!          ang1 = atan2(ypoin, xpoin) + angs
!!          r = sqrt(xpoin**2 + ypoin**2)
!!          xpoin = r*cos(ang1) + xc
!!          ypoin = r*sin(ang1) + yc
!!          call draw2(xpoin, ypoin)
!!       end do
!!       if (ifill  >  0) then
!!          call closepoly()
!!          call polyfill(.false.)
!!       end if
!!    end subroutine spirograph
!!    end program demo_polyhatch

!>
!!##NAME
!!    hatchang(3f) - [M_draw:POLYGONS] Set the angle of the hatch lines.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!       subroutine hatchang(angle)
!!       real,intent(in) :: angle
!!
!!##DESCRIPTION
!!
!!    Set the angle of the hatch lines. The angle is in degrees. Zero degrees
!!    is on the negative X axis. Positive values are counterclockwise. The
!!    value is 0 at program initialization. The last value set is retained
!!    even if hatching is not active or is turned on and off.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!       program demo_hatchang
!!       use M_draw
!!       implicit none
!!       real :: b
!!       integer :: idum
!!          call prefsize(1000,200)
!!          call vinit(' ')
!!          b = 0.4
!!          call page(-25.0 - b, 25.0 + b, -5.0 - b, 5.0 + b)
!!          call color(0)
!!          call clear()
!!          call textsize(0.6, 0.7)
!!          call font('futura.l')
!!          call centertext(.true.)
!!          call leftjustify()
!!          call linewidth(50)
!!          call polyhatch(.true.)
!!          call hatchpitch(1.0/2.0)
!!          ! draw circles with hatching
!!          call drawc(ang=  90.1, col=7, x=-20.0, label='90 degrees')
!!          call drawc(ang=  45.0, col=2, x=-10.0, label='45 degrees')
!!          call drawc(ang=   0.0, col=6, x=  0.0, label='0 degrees')
!!          call drawc(ang= -45.0, col=5, x= 10.0, label='-45 degrees')
!!          call drawc(ang= -90.0, col=4, x= 20.0, label='-90 degrees')
!!
!!          call linewidth(130)
!!          call move2(0.0, 0.0)
!!          call draw2(-5.0, 0.0)
!!          call move2(-5.0, 0.0)
!!          call draw2(-4.4, 0.3)
!!          call move2(-5.0, 0.0)
!!          call draw2(-4.4, - 0.3)
!!          call rightjustify()
!!          call linewidth(60)
!!          call move2(-5.0,0.0)
!!          call drawstr('0 degrees')
!!          idum = getkey()
!!          call vexit()
!!       contains
!!          subroutine drawc(ang,col,x,label)
!!          real,intent(in) :: ang, x
!!          integer,intent(in) :: col
!!          character(len=*),intent(in) :: label
!!          real :: y
!!          y=0.0
!!          call linewidth(90)
!!          call hatchang(ang)
!!          call color(col)
!!          call circle(X, Y, 5.0)
!!          y = -4.9
!!          call move2(X - 4.9, Y)
!!          call color(7)
!!          call linewidth(60)
!!          call drawstr(label)
!!          end subroutine drawc
!!
!!       end program demo_hatchang
!>
!!##NAME
!!    hatchpitch(3f) - [M_draw:POLYGONS] Set the distance between hatch lines.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine hatchpitch(pitch)
!!       real,intent(in) :: pitch
!!
!!##DESCRIPTION
!!    Set the distance between hatch lines. The distance is measured in
!!    window units (as opposed to viewport or device units).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!       program demo_hatchpitch
!!       use M_draw
!!       implicit none
!!       real :: b
!!       integer :: idum
!!
!!          call prefsize(1000, 200)
!!          call vinit(' ')
!!          b = 0.1
!!          call page(-25.0 - b, 25.0 + b, -5.0 - b, 5.0 + b)
!!          call color(0)
!!          call clear()
!!          call textsize(0.5, 0.6)
!!          call font('futura.l')
!!          call leftjustify()
!!          call circleprecision(3)
!!          ! draw circles with hatching
!!          call linewidth(150)
!!          call polyhatch(.true.)
!!          call hatchang(30.0)
!!
!!          call try(pitch=1.0/3.0, col=7, X=-20.0, label='1/3')
!!          call try(pitch=1.0/2.0, col=2, X=-10.0, label='1/2')
!!          call try(pitch=1.0,     col=6, X= -0.0, label='1')
!!          call try(pitch=2.0,     col=5, X= 10.0, label='2')
!!          call try(pitch=3.0,     col=4, X= 20.0, label='3')
!!          idum = getkey()
!!          call vexit()
!!       contains
!!          subroutine try(pitch, col, x, label)
!!             real, intent(in) :: pitch
!!             integer, intent(in) :: col
!!             real, intent(in) :: x
!!             character(len=*), intent(in) :: label
!!             call hatchpitch(pitch)
!!             call color(col)
!!             call circle(X, 0.0, 5.0)
!!             call move2(X - 4.9,-4.9)
!!             call color(7)
!!             call drawstr(label)
!!          end subroutine try
!!
!!       end program demo_hatchpitch
!>
!!##NAME
!!    poly2(3f) - [M_draw:POLYGONS] Construct an (x, y) polygon from an array of points
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!       subroutine poly2(n, points)
!!       integer,intent(in) :: n
!!       real,intent(in) :: points(2, n)
!!
!!##DESCRIPTION
!!    Construct an (x, y) polygon from an array of points provided by the user.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_poly2
!!      use M_draw
!!      implicit none
!!      integer :: i,j
!!      integer :: ipaws, icolor
!!      real    :: xx,yy
!!         call prefsize(512,512)
!!         call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!         call ortho2(0.0,256.0,0.0,256.0)
!!         call linewidth(1)
!!         call polyfill(.true.)
!!         ! step thru a series of rectangular cells
!!         icolor=0
!!         xx=0.0
!!         do i=1,16
!!            yy=0.0
!!            do j=1,16
!!               yy=yy+16.0
!!               icolor=icolor+1
!!               call setcolor(icolor,xx,yy)
!!            enddo
!!            xx=xx+16.0
!!         enddo
!!         ipaws=getkey()
!!         call vexit()
!!      contains
!!
!!      subroutine setcolor(iset,xx,yy)
!!      integer,intent(in) :: iset
!!      real,intent(in)    :: xx,yy
!!      real    :: points(2,100)
!!      integer :: red, green, blue
!!      if(iset.gt.255)return
!!      ! determine coordinates of next square
!!      points(1:2,1)=[xx,      yy      ]
!!      points(1:2,2)=[xx,      yy+16.0 ]
!!      points(1:2,3)=[xx+16.0, yy+16.0 ]
!!      points(1:2,4)=[xx+16.0, yy      ]
!!      points(1:2,5)=[xx,      yy      ]
!!      ! get some RGB values to try
!!      red=irand(0,255)
!!      green=irand(0,255)
!!      blue=irand(0,255)
!!      ! set a color number to the new RGB values
!!      call mapcolor(icolor, red, green, blue)
!!      ! set to the new color
!!      call color(icolor)
!!      ! fill the rectangle in that color
!!      call poly2(5,points)
!!      end subroutine setcolor
!!
!!      function irand(first,last) result(rand_int)
!!      use, intrinsic :: iso_fortran_env, only : dp=>real64
!!      integer,intent(in) :: first
!!      integer,intent(in) :: last
!!      integer              :: rand_int
!!      real(kind=dp)        :: rand_val
!!         call random_number(rand_val)
!!         rand_int = first + FLOOR((last+1-first)*rand_val)
!!      end function irand
!!
!!      end program demo_poly2
!>
!!##NAME
!!    poly(3f) - [M_draw:POLYGONS] Construct a polygon from an array of points
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine poly(n, points)
!!       integer,intent(in) :: n
!!       real,intent(in) :: points(3, n)
!!
!!##DESCRIPTION
!!
!!    Construct a polygon from an array of points provided by the user.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!    program demo_poly
!!    use M_draw
!!    implicit none
!!    ! Using polygons, hatching, and filling.
!!    integer           :: ios
!!    character(len=50) :: device
!!       print*,'Enter output device:'
!!       read(*,'(a)',iostat=ios)device
!!       if(ios.ne.0)then
!!          call prefsize(1000,1000)
!!          device=' '
!!       endif
!!       call vinit(device)
!!    ! clear to black
!!       call color(D_BLACK)
!!       call clear()
!!    ! world coordinates are now in the range -10 to 10
!!    ! in x, y, and z. Note that positive z is towards us.
!!       call ortho(-10.0, 10.0, -10.0, 10.0, 10.0, -10.0)
!!       call color(D_YELLOW)
!!    ! write out the string "Polygon from poly()" in the
!!    ! starting at (-8.0, -4.0) and scaled to be 4.0 units long,
!!    ! 0.5 units high.
!!       call boxtext(-8.0, -4.0, 4.0, 0.5, 'Polygon from poly()')
!!       call color(D_GREEN)
!!    ! write out a scaled string starting at (0.0, 6.0)
!!       call boxtext(0.0, 6.0, 4.0, 0.5, 'Polygon from move()/ draw()')
!!       call color(D_MAGENTA)
!!    ! write out a scaled string starting at (0.0, 6.0)
!!       call boxtext(3.5, -3.5, 1.9, 0.5, 'Sector')
!!    ! draw some polygons
!!       call drawpoly()
!!    ! turn on polygon hatching
!!       call polyhatch(.true.)
!!       call hatchang(45.0)
!!       call hatchpitch(0.3)
!!    !  Rotate 20 degrees around x and 30 around y
!!       call rotate(20.0, 'x')
!!       call rotate(30.0, 'y')
!!    ! draw some polygons with hatching
!!       call drawpoly()
!!    ! turn on polygon filling - this automatically turns off hatching
!!       call polyfill(.true.)
!!    !  Do another set of rotations.
!!       call rotate(20.0, 'x')
!!       call rotate(30.0, 'y')
!!    ! draw some polygons with filling
!!       call drawpoly()
!!       call vexit()
!!    contains
!!    subroutine drawpoly()
!!       integer :: idum
!!       real parray(3,4)                   ! An array of points for a polygon
!!       data parray/ -8.0, -8.0, 0.0,  &
!!                  & -5.0, -8.0, 0.0,  &
!!                  & -5.0, -5.0, 0.0,  &
!!                  & -8.0, -5.0, 0.0 /
!!       call color(D_YELLOW)
!!    ! Draw a polygon using poly, parray is our array of
!!    ! points and 4 is the number of points in it.
!!       call poly(4, parray)
!!       call color(D_GREEN)
!!    ! Draw a 5 sided figure by using move, draw and closepoly.
!!       call makepoly()
!!          call move(0.0, 0.0, 0.0)
!!          call draw(3.0, 0.0, 0.0)
!!          call draw(3.0, 4.0, 0.0)
!!          call draw(-1.0, 5.0, 0.0)
!!          call draw(-2.0, 2.0, 0.0)
!!       call closepoly()
!!       call color(D_MAGENTA)
!!    ! draw a sector representing a 1/4 circle
!!       call sector(1.5, -7.0, 3.0, 0.0, 90.0)
!!       idum=getkey()
!!    end subroutine drawpoly
!!
!!    end program demo_poly
!>
!!##NAME
!!    makepoly(3f) - [M_draw:POLYGONS] opens polygon constructed by a series of move-draws and closed by closepoly
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine makepoly()
!!
!!##DESCRIPTION
!!    makepoly(3f)  opens up a polygon which will then be constructed by a series
!!    of move-draws and closed by a closepoly.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!    program demo_makepoly
!!    use :: M_draw
!!    implicit none
!!    integer,parameter :: wide=640, tall=640
!!    integer :: rows, xoff, yoff, box_sz
!!    integer :: i20, i30, ncols, nrows, ilines
!!    real    :: bottom, left, sun_radius, planet_radius, planet_offset
!!    integer :: ipaws
!!       call prefsize(wide,tall)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call ortho2(0.0, real(wide), 0.0, real(tall) )
!!       ! really slows down pbm driver because all lines are polygons
!!       ! call linewidth(3)
!!       call color(D_WHITE)
!!       call clear()
!!       call color(D_BLACK)
!!       rows=1
!!       ! size of biggest box to use and get specified number of rows
!!       box_sz=MIN(wide,tall)/rows
!!       ! number of rows of objects to draw
!!       nrows = tall/box_sz
!!       ! number of columns of objects to draw
!!       ncols = wide/box_sz
!!       ! initial x offset to begin row at to center drawings
!!       xoff = (wide - ncols * box_sz)/2
!!       ! initial x offset to begin column at to center drawings
!!       yoff = (tall - nrows * box_sz)/2
!!       sun_radius = 148
!!       planet_radius = 1
!!       do ilines = 1, 300
!!          do i20 = 1, ncols
!!             left = (i20-1)*box_sz+xoff
!!             do i30 = 1, nrows
!!                bottom = (i30-1)*box_sz+yoff
!!                call color(D_BLACK)
!!             call makepoly()
!!                call rect(left,bottom,left+box_sz,bottom+box_sz)
!!             call closepoly()
!!                planet_offset= sun_radius
!!                   call color(mod(ilines,15)+1)
!!                   call hypoc(left + box_sz/2.0, bottom + box_sz/2.0, &
!!                & sun_radius, planet_radius, planet_offset, &
!!                & box_sz/2.0, ilines,  &
!!                & 0.0, 0.0, 1)
!!             enddo
!!          enddo
!!          ipaws=getkey()
!!       enddo
!!       call vexit()
!!    contains
!!    !
!!    !  Make shapes using hypocycloidal curves.
!!    !
!!    subroutine hypoc(xc,yc,sun,planet0,offset0,radius,ilines,ang,angs,ifill)
!!    use M_draw
!!    implicit none
!!    real,parameter  :: PI= 3.14159265358979323846264338327950288419716939937510
!!    real,intent(in) :: xc, yc      ! center of curve
!!    ! radii of sun, planet, and planet offset
!!    real,intent(in) :: sun,planet0,offset0
!!    real,intent(in)    :: radius
!!    integer,intent(in) :: ilines
!!    ! radius to fit the shape to (no fit if radius is 0)
!!    real,intent(in)    :: ang
!!    ! number of points to sample along curve
!!    real,intent(in)    :: angs
!!    ! angle to rotate the shape by, to orientate it.
!!    integer,intent(in) :: ifill
!!    ! angle to start sampling points at; ccw is +; 0 is East
!!    integer            :: i10
!!    ! 1 make a filled polygon, 2 make a hatched polygon
!!    real               :: ang1, con1, con2, factor
!!    real               :: offset, planet, r, sunr, u
!!    real               :: xpoin, xpoin1, ypoin, ypoin1
!!       sunr=sun
!!       offset=offset0
!!       planet=planet0
!!       if(ilines.eq.0.0) return
!!       if(planet.eq.0.0) return
!!       if(sunr.eq.0.0)   return
!!       if(radius.ne.0.and.sunr-planet+offset.ne.0)then
!!          factor=radius/(sunr-planet+offset)
!!          sunr=factor*sunr
!!          planet=factor*planet
!!          offset=factor*offset
!!       endif
!!       u=0.0+ang
!!       con1=PI*2.*(sunr/planet)/real(ilines)
!!       con2=(1.0-planet/sunr)*u
!!       xpoin1=(sunr-planet)*cos(planet*u/sunr)+offset*cos(con2)
!!       ypoin1=(sunr-planet)*sin(planet*u/sunr)-offset*sin(con2)
!!       ang1=atan2(ypoin1,xpoin1)+angs
!!       r=sqrt(xpoin1**2+ypoin1**2)
!!       xpoin1=r*cos(ang1)+xc
!!       ypoin1=r*sin(ang1)+yc
!!       select case(ifill)
!!       case(:0)
!!       case(1:)
!!          call makepoly()
!!       end select
!!       call move2(xpoin1,ypoin1)
!!       do i10=1,ilines
!!          u=con1*i10+ang
!!          con2=(1.0-planet/sunr)*u
!!          if(con2.ge.2**24) con2=amod(con2,PI)
!!          xpoin=(sunr-planet)*cos(planet*u/sunr)+offset*cos(con2)
!!          ypoin=(sunr-planet)*sin(planet*u/sunr)-offset*sin(con2)
!!          ang1=atan2(ypoin,xpoin)+angs
!!          r=sqrt(xpoin**2+ypoin**2)
!!          xpoin=r*cos(ang1)+xc
!!          ypoin=r*sin(ang1)+yc
!!          call draw2(xpoin,ypoin)
!!       enddo
!!       call draw2(xpoin1,ypoin1)
!!       if(ifill.gt.0)then
!!         call closepoly()
!!       endif
!!    end subroutine hypoc
!!    end program demo_makepoly
!>
!!##NAME
!!    closepoly(3f) - [M_draw:POLYGONS] Terminates a polygon opened by makepoly(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine closepoly()
!!
!!##DESCRIPTION
!!
!!    Terminates a polygon opened by makepoly(3f).
!>
!!##NAME
!!    backface(3f) - [M_draw:POLYGONS] Turns on culling of backfacing polygons.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine backface(onoff)
!!       logical onoff
!!
!!##DESCRIPTION
!!    Turns on culling of backfacing polygons. A polygon is backfacing if
!!    its orientation in *screen* coords is clockwise, unless a call
!!    to backfacedir is made.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_backface
!!    ! demonstrate double buffering and what happens
!!    ! when you hit clipping plane
!!    !
!!    ! Specifying an extra argument turns on the polygon filling.
!!    !
!!    use M_draw
!!    implicit none
!!    character(len=30) :: device
!!    character(len=1)  :: c
!!    real    r, t, dr, dt
!!    integer nplanes
!!    logical fill, back, backdir
!!    integer :: ios
!!    integer :: ipaws
!!
!!    print*,'Enter output device:'
!!    read(*,'(a)',iostat=ios)device
!!    if(ios.ne.0)device=' '
!!
!!    dr = 0.3
!!    dt = 0.2
!!    print*,'Enter delta in degrees (0.333 is typical):'
!!    read(*,*,iostat=ios) dr
!!    if(ios.ne.0)dr=0.333
!!    write(*,*)'DR=',dr
!!
!!    call prefsize(300, 300)
!!
!!    call vinit(device)
!!
!!    nplanes = getdepth()
!!
!!    fill = .true.
!!    back = .true.
!!    backdir = .true.
!!
!!    call polyfill(fill)
!!    call backface(back)
!!    call backfacedir(backdir)
!!
!!    call color(D_BLACK)
!!    call clear()
!!
!!    call window(-1.5, 1.5, -1.5, 1.5, 9.0, -5.0)
!!    call lookat(0.0, 0.0, 12.0, 0.0, 0.0, 0.0, 0.0)
!!
!!    t = 0.0
!!    r = 30.0
!!    !
!!    ! Setup drawing into the backbuffer....
!!    !
!!    if (backbuffer().lt.0) then
!!       write(*,*)'Device can''t support doublebuffering'
!!       ! draw a single view so at least the cube is displayed
!!       call frontbuffer()
!!       call color(D_BLACK)
!!       call clear()
!!       call translate(0.0, 0.0, t)
!!       call rotate(r, 'y')
!!       call rotate(r, 'z')
!!       call rotate(r, 'x')
!!       call color(D_WHITE)
!!       call polyfill(.true.)
!!       call drawcube(nplanes)
!!       call color(D_BLACK)
!!       call polyfill(.false.)
!!       call drawcube(nplanes)
!!       ipaws=getkey()
!!       call vexit()
!!       stop
!!
!!    endif
!!
!!    do
!!       if(r.ge.360) r = 0.0
!!       call color(D_BLACK)
!!       call clear()
!!
!!       call pushmatrix()
!!
!!       call translate(0.0, 0.0, t)
!!       call rotate(r, 'y')
!!       call rotate(r, 'z')
!!       call rotate(r, 'x')
!!       call color(D_WHITE)
!!
!!       call drawcube(nplanes)
!!
!!       if (nplanes .eq. 1 .and. fill) then
!!          call polyfill(.false.)
!!          call color(D_BLACK)
!!          call drawcube(nplanes)
!!          call polyfill(fill)
!!       endif
!!
!!       call popmatrix()
!!
!!       t = t + dt
!!       if (t.gt.3.0 .or. t.lt.-18.0) dt = -dt
!!
!!       call swapbuffers()
!!
!!       c = char(checkkey())
!!       if (c .eq. 'f') then
!!          fill = .not. fill
!!          call polyfill(fill)
!!       elseif (c .eq. 'b') then
!!          back = .not. back
!!          call backface(back)
!!       elseif (c .eq. 'd') then
!!          backdir = .not. backdir
!!          call backfacedir(backdir)
!!       elseif (c .ne. char(0)) then
!!          call vexit()
!!          stop
!!       endif
!!
!!       r = r + dr
!!    enddo
!!
!!    contains
!!    ! this routine draws the cube, using colours if available
!!    !
!!    subroutine drawcube(nplanes)
!!       integer nplanes
!!
!!       real carray(3, 8)
!!       data carray/            &
!!       &   -1.0,  -1.0,   1.0,  &
!!       &    1.0,  -1.0,   1.0,  &
!!       &    1.0,   1.0,   1.0,  &
!!       &   -1.0,   1.0,   1.0,  &
!!       &   -1.0,  -1.0,  -1.0,  &
!!       &    1.0,  -1.0,  -1.0,  &
!!       &    1.0,   1.0,  -1.0,  &
!!       &   -1.0,   1.0,  -1.0/
!!       save carray
!!
!!       if (nplanes.gt.1) call color(D_RED)
!!
!!       call makepoly()
!!       call move(carray(1,1), carray(2,1), carray(3,1))
!!       call draw(carray(1,2), carray(2,2), carray(3,2))
!!       call draw(carray(1,3), carray(2,3), carray(3,3))
!!       call draw(carray(1,4), carray(2,4), carray(3,4))
!!       call draw(carray(1,1), carray(2,1), carray(3,1))
!!       call closepoly()
!!
!!       if (nplanes.gt.1) call color(D_GREEN)
!!
!!       call makepoly()
!!       call move(carray(1,6), carray(2,6), carray(3,6))
!!       call draw(carray(1,5), carray(2,5), carray(3,5))
!!       call draw(carray(1,8), carray(2,8), carray(3,8))
!!       call draw(carray(1,7), carray(2,7), carray(3,7))
!!       call draw(carray(1,6), carray(2,6), carray(3,6))
!!       call closepoly()
!!
!!       if (nplanes.gt.1) call color(D_YELLOW)
!!
!!       call makepoly()
!!       call move(carray(1,2), carray(2,2), carray(3,2))
!!       call draw(carray(1,6), carray(2,6), carray(3,6))
!!       call draw(carray(1,7), carray(2,7), carray(3,7))
!!       call draw(carray(1,3), carray(2,3), carray(3,3))
!!       call draw(carray(1,2), carray(2,2), carray(3,2))
!!       call closepoly()
!!
!!       if (nplanes.gt.1) call color(D_BLUE)
!!
!!       call makepoly()
!!       call move(carray(1,1), carray(2,1), carray(3,1))
!!       call draw(carray(1,4), carray(2,4), carray(3,4))
!!       call draw(carray(1,8), carray(2,8), carray(3,8))
!!       call draw(carray(1,5), carray(2,5), carray(3,5))
!!       call draw(carray(1,1), carray(2,1), carray(3,1))
!!       call closepoly()
!!
!!       if (nplanes.gt.1) call color(D_MAGENTA)
!!
!!       call makepoly()
!!       call move(carray(1,3), carray(2,3), carray(3,3))
!!       call draw(carray(1,7), carray(2,7), carray(3,7))
!!       call draw(carray(1,8), carray(2,8), carray(3,8))
!!       call draw(carray(1,4), carray(2,4), carray(3,4))
!!       call draw(carray(1,3), carray(2,3), carray(3,3))
!!       call closepoly()
!!
!!       if (nplanes.gt.1) call color(D_CYAN)
!!
!!       call makepoly()
!!       call move(carray(1,1), carray(2,1), carray(3,1))
!!       call draw(carray(1,5), carray(2,5), carray(3,5))
!!       call draw(carray(1,6), carray(2,6), carray(3,6))
!!       call draw(carray(1,2), carray(2,2), carray(3,2))
!!       call draw(carray(1,1), carray(2,1), carray(3,1))
!!       call closepoly()
!!
!!    end subroutine drawcube
!!
!!    end program demo_backface
!!
!! !
!>
!!##NAME
!!    backfacedir(3f) - [M_draw:POLYGONS] Sets backfacing direction to clockwise or anti-clockwise
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine backfacedir(clockwise)
!!       integer,intent(in) :: clockwise
!!
!!##DESCRIPTION
!!    Sets the backfacing direction to clockwise or anti-clockwise depending
!!    on whether clockwise is 1 or 0. 1 = clockwise (in screen coords)
!!    0 = anticlockwise.
!!
!>
!!##NAME
!!     circleprecision(3f) - [M_draw:ARCS] Set number of line segments used to approximate a circle
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine circleprecision(nsegs)
!!         integer,intent(in) :: nsegs
!!
!!    Set the number of line segments making up a circle. Default is
!!    currently 32. The number of segments in an arc or sector is calculated
!!    from the variable "nsegs" according to the span of the arc or sector.
!!
!!##OPTIONS
!!    NSEGS   number of line segments making up a circle
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_circleprecision
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    real    :: b=0.5
!!    real    :: y1,y2,ym,x1,x2
!!    real    :: width=50.0/8.0,width2
!!    integer,parameter :: ivals(*)=[3,5,7,10,20,30,60,100]
!!    integer :: i
!!    integer :: ipaws
!!       !! set up long bar as plotting area
!!       call prefsize(1000,200)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!       call textsize( 2.5/2.0, 3.0/2.0)
!!       call font('futura.l')
!!       call centertext(.true.)
!!       call linewidth(30)
!!       call color(D_GREEN)
!!       y1=-5
!!       y2=5
!!       ym=0
!!       x1=-25+.05*width
!!       ! draw colored rectangle and a circle and label center of circle repeat
!!       width2=width*0.95
!!       do i=1,size(ivals)
!!          x2=x1+width2
!!          call move2((x1+x2)/2.0,ym)
!!          call circleprecision(ivals(i))
!!          call print(ivals(i))     ! convert number to string and draw it
!!          call circle((x1+x2)/2.0, ym, (x2-x1)/2.10)
!!          x1=x1+width
!!       enddo
!!       ipaws=getkey()
!!       call vexit()
!!    end program demo_circleprecision
!!
!!##IMAGE
!!    circles are drawn with various circle precision values.
!>
!!##NAME
!!     arc(3f) - [M_draw:ARCS] Draw an arc in world units.
!!     (LICENSE:PD)
!!##SYNOPSIS
!!
!!         subroutine arc(x, y, radius, startang, endang)
!!         real,intent(in) :: x
!!         real,intent(in) :: y
!!         real,intent(in) :: radius
!!         real,intent(in) :: startang
!!         real,intent(in) :: endang
!!##DESCRIPTION
!!
!!    Draw an arc. x, y, and radius are values in world units
!!    using current line width and color
!!
!!    Angles are in degrees, positive measured counterclockwise from the
!!    +X axis. The current position after the arc is drawn is at the end
!!    of the arc.
!!
!!##OPTIONS
!!    X,Y        Coordinates for the center of the circle
!!    RADIUS     Radius of the circle
!!    STARTANG   Start angle
!!    ENDANG     End angle
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_arc
!!       use M_draw
!!       use M_draw,    only  : D_BLACK,   D_WHITE
!!       use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!       use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!       implicit none
!!       integer        :: icolor
!!       real           :: b=0.5
!!       real           :: R=4.9
!!       real           :: X, Y, A, YY
!!       integer        :: key
!!       call prefsize(1000,200)
!!       call vinit(' ')
!!       call color(D_BLACK)
!!       call clear()
!!       call color(D_YELLOW)
!!       call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!       call textsize(1.0,1.4)
!!       call font("futura.l")
!!       call centertext(.true.)
!!       ! draw arcs with various start and end angles
!!
!!       X=-20.0; Y=0.0; A=0.0; B=30.0;  icolor=7; YY=4
!!       call drawit("0 to 30 deg.")
!!
!!       X=-10.0; YY=-4; A=0.0; B=-45.0; icolor=1
!!       call drawit("0 to -45 deg.")
!!
!!       X=0.0; YY=-4; A=100.0; B=200.0; icolor=2
!!       call drawit("100 to 200 deg.")
!!
!!       X=10.0; YY=-4; A=-30.0; B=30.0;icolor=5
!!       call drawit("-30 to 30 deg.")
!!
!!       YY=4; X=20.0; A=45.0; B=-45.0; icolor=6
!!       call drawit("45 to -45 deg.")
!!
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    contains
!!       subroutine drawit(label)
!!          character(len=*) :: label
!!          call linewidth(150)
!!          call color(icolor)
!!          call arc(X,Y,R,A,B)
!!          call draw2(X,Y)
!!          call move2(X,YY)
!!          call linewidth(50)
!!          call color(D_WHITE)
!!          call drawstr(label)
!!       end subroutine drawit
!!
!!    end program demo_arc
!>
!!##NAME
!!    sector(3f) - [M_draw:ARCS] Draw a sector. Note: sectors are polygons.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine sector(x, y, radius, startang, endang)
!!         REAL x, y, radius, startang, endang
!!
!!##DESCRIPTION
!!
!!    Draw a sector. x, y, and radius are values in world units. Note: sectors
!!    are regarded as polygons, so if polyfill or polyhatch has been called
!!    with 1, the sectors will be filled or hatched accordingly.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_sector
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    real           :: b=0.5
!!    real           :: R=4.9
!!    real           :: X, Y, A
!!    integer        :: key
!!       call prefsize(1000,200)
!!       call vinit(' ')
!!       call color(D_BLACK)
!!       call clear()
!!       call color(D_YELLOW)
!!       call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!       call textsize(1.0,1.4)
!!       call font("futura.l")
!!       call centertext(.true.)
!!
!!       ! draw sectors with various start and end angles
!!
!!       call linewidth(150)
!!       call color(D_WHITE)
!!       X=-20; Y=0; R=5.0; A=0; B=30
!!       call sector(X,Y,R,A,B)
!!       call move2(X, 4.0)
!!       call linewidth(50)
!!       call drawstr("0 to 30 deg.")
!!
!!       call linewidth(150)
!!       call color(D_RED)
!!       X=-10; Y=0; R=5; A=0; B=-45
!!       call sector(X,Y,R,A,B)
!!       call move2(X,-4.0)
!!       call linewidth(50)
!!       call drawstr("0 to -45 deg.")
!!
!!       call polyfill(.true.)
!!
!!       call linewidth(150)
!!       call color(D_GREEN)
!!       X=-0; Y=0; R=5; A=100; B=200
!!       call sector(X,Y,R,A,B)
!!       call move2(X,-4.0)
!!       call linewidth(50)
!!       call drawstr("100 to 200 deg.")
!!
!!       call polyhatch(.true.)
!!       call hatchpitch(1.0/2.0)
!!       call hatchang(90.0)
!!
!!       call linewidth(150)
!!       call color(D_MAGENTA)
!!       X=10; Y=0; R=5; A=-30; B=30
!!       call sector(X,Y,R,A,B)
!!       call move2(X,-4.0)
!!       call linewidth(50)
!!       call drawstr("-30 to 30 deg.")
!!
!!       call hatchang(30.0)
!!       call linewidth(150)
!!       call color(D_CYAN)
!!       X=20; Y=0; R=5; A=45; B=-45
!!       call sector(X,Y,R,A,B)
!!       call move2(X, 4.0)
!!       call linewidth(50)
!!       call drawstr(" 45 to -45 deg.")
!!
!!       key=getkey()
!!       call vexit()
!!    end program demo_sector
!>
!!##NAME
!!    circle(3f) - [M_draw:ARCS] Draw a circle.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine circle(x, y, radius)
!!         real,intent(in) :: x
!!         real,intent(in) :: y
!!         real,intent(in) :: radius
!!
!!##DESCRIPTION
!!
!!    Draw a circle. x, y, and radius are values in world units.
!!
!!    Draw a circle using current line width and color
!!
!!    NOTE
!!
!!    circles are regarded as polygons, so if polyfill or polyhatch has been
!!    called with .true., the circle will be filled or hatched accordingly.
!!
!!##OPTIONS
!!    X,Y        Coordinates for the center of the circle
!!    RADIUS     Radius of the circle
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_circle
!!       use M_draw
!!       use M_draw,    only  : D_BLACK,   D_WHITE
!!       use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!       use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!       implicit none
!!       real :: b=0.5
!!       real :: R=5
!!       integer :: ipaws
!!    ! set up drawing surface
!!       call prefsize(1000,200)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call page(-25.0-b,25.0+b,-R-b,R+b)
!!       call linewidth(200)
!!       call color(D_CYAN)
!!       call clear()
!!    ! draw circles with hatching and fill
!!       call color(D_WHITE)
!!       call circle(-20.0,0.0, R)
!!       call color(D_RED)
!!       call circle(-10.0,0.0, R)
!!       call polyfill(.true.)
!!       call color(D_GREEN)
!!       call circle(0.0,0.0,R)
!!       call linewidth(20)
!!       call polyhatch(.true.)
!!       call hatchpitch(1.0/2.0)
!!       call color(D_MAGENTA)
!!       call circle(10.0,0.0,R)
!!    ! layer calls to get a filled crosshatched circle
!!    ! first a solid colored circle
!!       call color(D_YELLOW)
!!       call polyfill(.true.)
!!       call circle(20.0,0.0,R)
!!
!!    ! draw hatch lines at 45 degrees
!!       call color(D_GREEN)
!!       call linewidth(80)
!!       call polyhatch(.true.)
!!       call hatchpitch(2.0/3.0)
!!       call hatchang(45.0)
!!       call circle(20.0,0.0,R)
!!    ! draw hatch lines at -45 degrees
!!       call hatchang(-45.0)
!!       call circle(20.0,0.0,R)
!!
!!    ! outline circle with a thick border
!!       call color(D_WHITE)
!!       call linewidth(160)
!!       call polyhatch(.false.)
!!       call circle(20.0,0.0,R)
!!
!!       ipaws=getkey()
!!    ! exit graphics mode
!!       call vexit()
!!    end program demo_circle
!>
!!##NAME
!!    point(3f) - [M_draw:POINT] Draw a point at x, y, z
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!         subroutine point(x, y, z)
!!         real,intent(in) :: x, y, z
!!##DESCRIPTION
!!    Draw a point at x, y, z. The size of the point is not affected by
!!    perspective. Draw a polygon if you want perspective to be applied.
!!    The size of the point is controlled by the current linewidth.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_point
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    integer :: i
!!    integer :: ikey
!!       call prefsize(1000,200)
!!       call vinit(' ')
!!       call color(D_BLACK)
!!       call clear()
!!       call page(-25.0, 25.0, -5.0, 5.0)
!!
!!       ! draw points using various linewidths and colors
!!       do i=1,300
!!          call randpoint()
!!       enddo
!!
!!       ikey=getkey()
!!       call vexit()
!!
!!    contains
!!       subroutine randpoint()
!!          real :: r1, r2
!!          call random_number(r1)
!!          call random_number(r2)
!!          call linewidth(int(r1*500+200))
!!          call color(nint(r2*7))
!!          call point(r1*50.0-25.0,r2*10.0-5.0,0.0)
!!       end subroutine randpoint
!!
!!    end program demo_point
!>
!!##NAME
!!    point2(3f) - [M_draw:POINT] Draw a point at x, y.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine point2(x, y)
!!         real,intent(in) :: x, y
!!
!!##DESCRIPTION
!!    Draw a point at x, y. Points are device-dependent and may not appear
!!    at all. Generally points are drawn with the current color as a circle
!!    with a diameter equal to the current linewidth.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_point2
!!    use :: M_draw
!!    implicit none
!!    integer :: i
!!    integer :: ipaws
!!    call prefsize(300,300)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call ortho2(0.0, 20.0, 0.0, 20.0)
!!    call color(D_MAGENTA)
!!    do i=1,20
!!       call linewidth(20*i)
!!       call point2(real(i),real(i))
!!    enddo
!!    ipaws=getkey()
!!    call vexit()
!!    end program demo_point2
!>
!!##NAME
!!    curvebasis(3f) - [M_draw:CURVE] Define a basis matrix for a curve.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!          subroutine curvebasis(basis)
!!          real,intent(in) :: basis(4,4)
!!##DESCRIPTION
!!    Define a basis matrix for a curve.
!>
!!##NAME
!!    curveprecision(3f) - [M_draw:CURVE] Define number of line segments used to draw a curve.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!          subroutine curveprecision(nsegs)
!!          integer nsegs
!!##DESCRIPTION
!!    Define the number of line segments used to draw a curve.
!>
!!##NAME
!!    rcurve(3f) - [M_draw:CURVE] Draw a rational curve.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine rcurve(geom)
!!          real geom(4,4)
!!
!!##DESCRIPTION
!!    Draw a rational curve.
!>
!!##NAME
!!    curve(3f) - [M_draw:CURVE] Draw a curve.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine curve(geom)
!!    real geom(3,4)
!!
!!##DESCRIPTION
!!    Draw a curve.
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_curve
!!    !
!!    !      using curves
!!    !
!!    use M_draw
!!    implicit none
!!
!!    integer i, idum, ios
!!    character(len=50) :: buf
!!    real bezier(4, 4), cardinal(4, 4), bspline(4, 4)
!!    real geom1(3, 4), geom2(3, 6)
!!    !
!!    ! curve basis types
!!    !
!!    data bezier /                                            &
!!    &          -1.0,   3.0,    -3.0,   1.0,                  &
!!    &          3.0,    -6.0,   3.0,    0.0,                  &
!!    &          -3.0,   3.0,    0.0,    0.0,                  &
!!    &          1.0,    0.0,    0.0,    0.0                   &
!!    &  /
!!
!!    data cardinal /                                          &
!!    &          -0.5,   1.5,    -1.5,   0.5,                  &
!!    &          1.0,    -2.5,   2.0,    -0.5,                 &
!!    &          -0.5,   0.0,    0.5,    0.0,                  &
!!    &          0.0,    1.0,    0.0,    0.0                   &
!!    &  /
!!
!!    data bspline /                                           &
!!    &          -0.166666,     0.5,     -0.5,     0.166666,   &
!!    &           0.5,         -1.0,      0.5,     0.0,        &
!!    &          -0.5,          0.0,      0.5,     0.0,        &
!!    &           0.166666,     0.666666, 0.166666, 0.0        &
!!    &  /
!!
!!    !
!!    ! Geometry matrix to demonstrate basic spline segments
!!    !
!!    data geom1 /                      &
!!    &           -180.0, 10.0, 0.0,    &
!!    &           -100.0, 110.0, 0.0,   &
!!    &           -100.0, -90.0, 0.0,   &
!!    &           0.0, 50.0, 0.0        &
!!    &  /
!!
!!    !
!!    ! Geometry matrix to demonstrate overlapping control points to
!!    ! produce continuous (Well, except for the bezier ones) curves
!!    ! from spline segments
!!    !
!!    data geom2 /                      &
!!    &          200.0, 480.0, 0.0,     &
!!    &          380.0, 180.0, 0.0,     &
!!    &          250.0, 430.0, 0.0,     &
!!    &          100.0, 130.0, 0.0,     &
!!    &          50.0,  280.0, 0.0,     &
!!    &          150.0, 380.0, 0.0      &
!!    &  /
!!
!!
!!    print*,'Enter output device:'
!!    read(*,'(a)',iostat=ios) buf
!!    if(ios.ne.0)buf=' '
!!
!!    call vinit(buf)
!!
!!    call ortho2(-200.0, 400.0, -100.0, 500.0)
!!
!!    call color(D_BLACK)
!!    call clear()
!!
!!    call color(D_YELLOW)
!!
!!    call textsize(10.0, 10.0)
!!    !
!!    ! label the control points in geom1
!!    !
!!    do i = 1, 4
!!       call move2(geom1(1, i), geom1(2, i))
!!       write(buf, '(i1)')i
!!       call drawstr(buf)
!!    enddo
!!    !
!!    ! label the control points in geom2
!!    !
!!    do i = 1, 6
!!       call move2(geom2(1, i), geom2(2, i))
!!       write(buf, '(i1)')i
!!       call drawstr(buf)
!!    enddo
!!    !
!!    ! scale the current font so that 30 of the largest characters
!!    ! in the current font will fit in a region 300 world units wide,
!!    ! 20 high.
!!    !
!!    call boxfit(300.0, 20.0, 30)
!!
!!    !
!!    ! set the number of line segments appearing in each curve to 20
!!    !
!!    call curveprecision(20)
!!
!!    !
!!    ! copy the bezier basis matrix into the curve basis matrix.
!!    !
!!    call curvebasis(bezier)
!!
!!    call color(D_RED)
!!
!!    !
!!    ! draw a curve using the current basis matrix (bezier in this case)
!!    ! and the control points in geom1
!!    !
!!    call curve(geom1)
!!
!!    call move2(70.0, 60.0)
!!    call drawstr('Bezier Curve Segment')
!!
!!    call move2(-190.0, 450.0)
!!    call drawstr('Three overlapping Bezier Curves')
!!
!!    !
!!    ! curven draws overlapping curve segments according to geom2, the
!!    ! number of curve segments drawn is three less than the number of
!!    ! points passed, assuming there are a least four points in the
!!    ! geometry matrix (in this case geom2). This call will draw 3
!!    ! overlapping curve segments in the current basis matrix - still
!!    ! bezier.
!!    !
!!    call curven(6, geom2)
!!
!!    idum=getkey()
!!    !
!!    ! load in the cardinal basis matrix
!!    !
!!    call curvebasis(cardinal)
!!
!!    call color(D_MAGENTA)
!!
!!    call move2(70.0, 10.0)
!!    call drawstr('Cardinal Curve Segment')
!!    !
!!    ! plot out a curve segment using the cardinal basis matrix
!!    !
!!    call curve(geom1)
!!
!!    call move2(-190.0, 400.0)
!!    call drawstr('Three overlapping Cardinal Curves')
!!    !
!!    ! now draw a bunch of them again.
!!    !
!!    call curven(6, geom2)
!!
!!    idum=getkey()
!!    !
!!    ! change the basis matrix again
!!    !
!!    call curvebasis(bspline)
!!
!!    call color(D_GREEN)
!!
!!    call move2(70.0, -40.0)
!!    call drawstr('Bspline Curve Segment')
!!    !
!!    ! now draw our curve segment in the new basis...
!!    !
!!    call curve(geom1)
!!
!!    call move2(-190.0, 350.0)
!!    call drawstr('Three overlapping Bspline Curves')
!!    !
!!    ! ...and do some overlapping ones
!!    !
!!    call curven(6, geom2)
!!
!!    idum=getkey()
!!
!!    call vexit()
!!
!!    end program demo_curve
!>
!!##NAME
!!    curven(3f) - [M_draw:CURVE] Draw n-3 overlapping curve segments. Note: n must be at least 4.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine curven(n, geom)
!!          integer n
!!          real geom(3,n)
!!
!!##DESCRIPTION
!!    Draw n-3 overlapping curve segments. Note: n must be at least 4.
!>
!!
!!##NAME
!!    font(3f) - [M_draw:TEXT] Set the current font by name
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine font(fontname)
!!         character(len=*),intent(in) :: fontname
!!
!!##DESCRIPTION
!!    Set the current font.
!!
!!    M_draw supports hardware and software fonts. The software fonts are based
!!    on the character set digitized by Dr Allen V. Hershey while working at
!!    the U. S. National Bureau of Standards. Exactly what hardware fonts are
!!    supported depends on the device, but it is guaranteed that the names
!!    "large" and "small" will result in something readable. For X11 displays
!!    the default large and small fonts used by the program can be overridden
!!    by placing the following defaults in the ~/.Xdefaults file:
!!
!!      draw.smallfont: X11-font-name
!!      draw.largefont: X11-font-name
!!
!!    It is noted here that hardware text is always assumed to be drawn
!!    parallel to the (x, y) plane, using whatever the current z coordinate
!!    is. The following software fonts are supported:
!!
!!       astrology       cursive         cyrillic        futura.l
!!       futura.m        gothic.eng      gothic.ger      gothic.ita
!!       greek           markers         math.low        math.upp
!!       meteorology     music           script          symbolic
!!       times.g         times.i         times.ib        times.r
!!       times.rb        japanese
!!
!!    A markers font "markers" is also provided for doing markers - you need
!!    to have centertext mode on for this to give sensible results when placing
!!    the markers.
!!
!!    If the environment variable "M_DRAW_FONTPATH" is set M_draw looks for the software
!!    fonts in the directory given by this value.
!!
!!    WHEN ASKED FOR NON-EXISTENT FONT NAMES, FONT(3f) STOPS THE PROGRAM.
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_font
!!    use :: M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    real    :: left
!!    real    :: baseline=80.0
!!    integer :: icolor=1
!!    integer :: ipaws
!!       !! set up drawing surface
!!       call prefsize(400, 400)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call page(-100.0, 100.0, -100.0, 100.0)
!!       call color(D_WHITE)
!!       call clear()
!!       call textsize(10.0, 10.0)
!!       !! place a vertical line along the edge
!!       call color(D_RED)
!!       call move2(-90.0, -90.0)
!!       call draw2(-90.0, 90.0)
!!       !! make a centered title at top a bit bolder and bigger
!!       call xcentertext()
!!       call textsize(13.0, 13.0)
!!       call linewidth(90)
!!       left=0
!!       call nextline('Font Samples')
!!       !! print the font samples
!!       left=-90
!!       call linewidth(40)
!!       call textsize(10.0, 10.0)
!!       call centertext(.false.)
!!       icolor=icolor-1
!!       call nextline('DEFAULT (ie. futura.l)')
!!       icolor=icolor-1
!!       call nextline('now call font(3f) ...')
!!       call nextline('A nice SIMPLEX font, futura.l')
!!       call nextline('A COMPLEX font, times.r')
!!       call nextline('ITALIC letters,  times.i')
!!       call nextline('DUPLEX is in between, futura.m')
!!       ipaws=getkey()
!!       call vexit()
!!    contains
!!    subroutine nextline(string)
!!    character(len=*) :: string
!!    !! reduce some duplicate code; very specific to this EXAMPLE
!!       integer :: iend
!!       iend=index(string,',')  ! if comma, assume font name found
!!       write(*,*)'FONT=',string(iend+1:),iend
!!       if(iend.ne.0)call font(trim(adjustl(string(iend+1:)))) ! change font
!!       icolor=icolor+1         ! set pen color
!!       call color(icolor)
!!       baseline=baseline-20    ! move down before drawing line
!!       call move2(left, baseline)
!!       call drawstr(string)    ! draw string
!!    end subroutine nextline
!!
!!    end program demo_font
!>
!!##NAME
!!    numchars(3f) - [M_draw:TEXT] Return number of characters in the current SOFTWARE font.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         integer function numchars()
!!
!!##DESCRIPTION
!!    Return the number of characters in the current font. Applicable only to
!!    software fonts.
!>
!!##NAME
!!    textsize(3f) - [M_draw:TEXT] Set text size of a character in the current SOFTWARE font in world units.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!         subroutine textsize(width, height)
!!         real,intent(in) :: width
!!         real,intent(in) :: height
!!
!!##DESCRIPTION
!!
!! Set the maximum size of a character in the current font. Width and height
!! are values in world units. This applies to software text, but may not apply
!! to hardware fonts depending upon the output device. This must
!! be done after the font being scaled is loaded. To keep text of different
!! sizes aligned along the same baseline not that you typically need to
!! subtract the descender height from the Y position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_textsize
!!    use M_draw
!!    implicit none
!!    integer :: i,ii
!!    integer :: ipaws
!!       !! set up long bar as plotting area
!!       call prefsize(900,150)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call ortho2(-30.0, 30.0, -5.0, 5.0)
!!       call font('times.r')
!!
!!       call move2(-23.0,-4.5)
!!       call color(D_WHITE)
!!       call textsize(2.0,2.0)
!!       call move2(-27.5,-3.0)
!!       call draw2( 27.5,-3.0)
!!       call move2(-27.5,-3.0)
!!
!!       do i=1,7
!!          ii=nint((i*20)*0.30)
!!          call linewidth(nint(ii*2.35))
!!          call textsize(real(i),real(i))
!!          call color(D_MAGENTA)
!!          call drawstr('aA')
!!       enddo
!!
!!       ipaws=getkey()
!!
!!       call vexit()
!!
!!    end program demo_textsize
!>
!!##NAME
!!    textang(3f) - [M_draw:TEXT] Set the SOFTWARE text angle.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!         subroutine textang(ang)
!!         real,intent(in) :: ang
!!
!!##DESCRIPTION
!!    Set the text angle. This angles strings and chars. This routine only
!!    affects software text. Angle is in degrees
!!
!!##OPTIONS
!!    ANG   The angle in degrees to draw text with when using drawstr(3f).
!!          Angles are measured counterclockwise with zero degrees at the horizontal
!!          line to the right of the original.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_textang
!!    use :: M_draw
!!    implicit none
!!    integer :: i, ipaws
!!
!!    !! set up drawing environment
!!    call prefsize(600,600)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call ortho2(-100.0,100.0,-100.0,100.0)
!!    call textsize(7.0,7.0)
!!    call linewidth(20)
!!    call color(D_BLACK)
!!    call clear()
!!
!!    do i=1,30
!!       !! draw radial lines
!!       call color(D_RED)
!!       call move2(0.0,0.0)
!!       call draw2(100.0*cosd(i*12.0),100.0*sind(i*12.0))
!!       !! draw rotated text
!!       call color(D_WHITE)
!!       call move2(30.0*cosd(i*12.0),30.0*sind(i*12.0))
!!       call textang(i*12.0)
!!       call drawstr('angled text')
!!    enddo
!!
!!    ipaws=getkey()
!!
!!    call vexit()
!!
!!    end program demo_textang
!>
!!##NAME
!!    fixedwidth(3f) - [M_draw:TEXT] Turns fixedwidth mode on or off for SOFTWARE fonts.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!         subroutine fixedwidth(onoff)
!!         logical onoff
!!##DESCRIPTION
!!
!!    Turns fixedwidth text on or off. .TRUE. causes all text to
!!    be printed with a fixed width for each character. Otherwise, the text
!!    is spaced proportionally, where each character has a unique width less
!!    than or equal to the current fixed font width. This routine only affects
!!    software text.
!!
!!    The default at program initialization is fixedwidth(.false.)
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_fixedwidth
!!       use M_draw
!!       implicit none
!!       real,parameter :: x1=0.0,  x2=40.0,  y1=0.0,  y2=4.0
!!       real,parameter :: scl=3*0.7
!!       integer :: idum
!!    ! set up display
!!       call prefsize(1000,100)
!!       call vinit(' ')
!!       call page(x1,x2,y1,y2)
!!    ! set font appearance
!!       call linewidth(200)
!!       call font("times.rb")
!!    ! draw a string using proportional and fixed spacing
!!       call move2(x1+0.3,y1+0.4)
!!       call textsize(0.8*scl,1.2*scl)
!!       call color(1)
!!       call fixedwidth(.false.)
!!       call drawstr("fixedwidth(.false.)")
!!       call textsize(0.6*scl,1.2*scl)
!!       call color(2)
!!       call fixedwidth(.true.)
!!       call drawstr(" fixedwidth(.true.)")
!!    ! wrap up
!!       idum=getkey()
!!       call vexit()
!!    end program demo_fixedwidth
!>
!!##NAME
!!    centertext(3f) - [M_draw:TEXT] Turns centertext mode on or off for SOFTWARE fonts.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine centertext(onoff)
!!         logical,intent(in) :: onoff
!!
!!##DESCRIPTION
!!
!!    Turns centertext text on or off. .TRUE. is on. This centers
!!    strings and chars. This routine only affects software text.
!!
!!##OPTIONS
!!    ONOFF  set centering mode on or off
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_centertext
!!    use :: M_draw
!!    implicit none
!!    real :: x1, y1, r, ang, xx, yy
!!    integer :: i, j, ipaws
!!    !! set up drawing environment
!!    call prefsize(600,600)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call ortho2(-300.0,300.0,-300.0,300.0)
!!    call textsize(8.0,8.0)
!!    call linewidth(30)
!!    call color(D_BLACK)
!!    call clear()
!!
!!    x1=-150
!!    y1=-150
!!    do j=1,4
!!       select case(j)
!!       case(1);  call  xcentertext();        x1=-150;  y1=-150;  r=100
!!       case(2);  call  ycentertext();        x1=+150;  y1=-150;  r= 30
!!       case(3);  call  centertext(.true.);   x1=-150;  y1=+150;  r=100
!!       case(4);  call  centertext(.false.);  x1=+150;  y1=+150;  r= 30
!!       end select
!!       !! draw radial lines
!!       call color(D_RED)
!!       do i=1,80
!!          call move2(x1,y1)
!!          call draw2(x1+150.0*cosd(i*12.0), y1+150.0*sind(i*12.0))
!!       enddo
!!
!!       !! draw rotated text
!!       call color(D_GREEN)
!!       do i=1,30
!!          ang=i*12.0
!!          xx=x1+r*cosd(ang)
!!          yy=y1+r*sind(ang)
!!          call move2(xx,yy)
!!          call textang(ang)
!!          call color(D_WHITE)
!!          call drawstr('This is angled text')
!!          call color(D_RED)
!!       enddo
!!    enddo
!!
!!    ipaws=getkey()
!!
!!    call vexit()
!!
!!    end program demo_centertext
!>
!!##NAME
!!    getcharsize(3f) - [M_draw:TEXT] Get the width and height of a character.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine getcharsize(c, width, height)
!!         character*1 c
!!         real width, height
!!
!!##DESCRIPTION
!!    Get the width and height of a character. At the moment the height
!!    returned is always that of the difference between the maximum descender
!!    and ascender.
!>
!!##NAME
!!    getfontdec(3f) - [M_draw:TEXT] Return size of maximum font descender
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         real function getfontdec()
!!##DESCRIPTION
!!    Get the descender size of a character in a font.
!>
!!##NAME
!!    getfontsize(3f) - [M_draw:TEXT] Get maximum width and height of a character in a font.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine getfontsize(width, height)
!!         real width, height
!!
!!##DESCRIPTION
!!
!!    Get the maximum width and height of a character in a font.
!>
!!##NAME
!!    drawchar(3f) - [M_draw:TEXT] Draw the character c and update current position.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine drawchar(ch)
!!         character(len=1),intent(in) :: ch
!!
!!##DESCRIPTION
!!
!!    Draw the character c at the current position. The current graphics
!!    position represents the bottom left hand corner of the character space.
!!
!!    Uses current line color and thickness and text justification mode.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_drawchar
!!    !
!!    !      display all the hershey fonts and demonstrate textang
!!    !
!!
!!    use M_draw, only: vinit, vsetflush, color, clear, font, vexit, vflush
!!    use M_draw, only: ortho2, textang, boxtext, rect, textsize, getkey
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    use M_draw
!!    implicit none
!!
!!    character(len=40)   :: str1, str2, str3, str4, fonts(22)
!!    character(len=100)  :: buf
!!    character(len=1)    :: c
!!    integer             :: i, ios
!!    data fonts/ 'astrology', 'cursive',    'futura.l',               &
!!    &      'futura.m',  'gothic.eng', 'gothic.ger',             &
!!    &      'gothic.ita','greek',      'japanese',    'markers', &
!!    &      'math.low',  'math.upp',   'meteorology', 'music',   &
!!    &      'cyrillic',  'script',     'symbolic',    'times.g', &
!!    &      'times.ib',  'times.i',    'times.r',     'times.rb' /
!!
!!    data str1/ 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /
!!    data str2/ 'abcdefghijklmnopqrstuvwxyz' /
!!    data str3/ '1234567890+-=!@#$%^&*(){}[]' /
!!    data str4/ '<>,./?~`\|_BONK,blark' /
!!    integer :: idum
!!    print*,'Enter output device:'
!!    read(*,'(a)',iostat=ios)buf
!!    if(ios.ne.0)buf=' '
!!    call prefsize(1000,1000)
!!    call vinit(buf)
!!    call vsetflush(.false.)
!!    call linewidth(20)
!!    call clear()
!!    ! define the world space
!!    call ortho2(-14.0, 14.0, -14.0, 14.0)
!!    do i = 1, 22
!!       ! do the title
!!       call textang(0.0)   ! reset text angle so title is straight
!!       call color(D_CYAN)
!!       call font('futura.m')
!!       write(buf, '(''This is Hershey font '',a)') fonts(i)
!!       !call printattribs('before')
!!       call boxtext(-11.0, 12.0, 20.0, 1.0, buf)
!!       !call printattribs('after')
!!       call rect(-11.0, 12.0, 9.0, 13.0) ! draw a box around the title
!!       call font(fonts(i))               ! grab a font from the table
!!       call color(D_BLUE)
!!       ! show the outer ring
!!       call textsize(1.5, 1.5)
!!       call ShowCircularText(11.0, str1)
!!       ! show the second ring
!!       call textsize(1.3, 1.3)
!!       call ShowCircularText(8.5, str2)
!!       ! show the third ring
!!       call textsize(1.1, 1.1)
!!       call ShowCircularText(7.0, str3)
!!       ! show the inside ring
!!       call textsize(0.9, 0.9)
!!       call ShowCircularText(5.0, str4)
!!       call vflush()
!!
!!       idum= getkey()
!!       select case(idum)
!!        case(:-1,ichar('q'),ichar('Q'))
!!          exit
!!       end select
!!
!!       call color(D_BLACK)
!!       call clear()
!!    enddo
!!    call vexit()
!!    contains
!!
!!    subroutine ShowCircularText(r, str)  ! show a ring of text
!!       use M_draw, only : move2, textang, drawchar
!!
!!       real              :: r
!!       character(len=*)  :: str
!!       real              :: i, inc, x, y, a
!!       integer           :: j, i10
!!       character(len=1)  :: c
!!       real,parameter    :: pi = 3.1415926535
!!
!!       j = 1
!!       inc = 360.0 / len_trim(str)
!!
!!       i=0.0
!!       do i10 = 1,len_trim(str)
!!          !
!!          ! calculate the next drawing position
!!          c = str(j:j)
!!          x = r * cos(i * pi / 180.0)
!!          y = r * sin(i * pi / 180.0)
!!          call move2(x, y)
!!          !
!!          ! calculate angle for next character
!!          a = 90.0 + i
!!          !
!!          ! set the orientation of the next character
!!          call textang(a)
!!          !
!!          ! draw the character
!!          call drawchar(c)
!!          j = j + 1
!!          i=i+inc
!!       enddo
!!
!!    end subroutine ShowCircularText
!!
!!    end program demo_drawchar
!>
!!##NAME
!!    drawstr(3f) - [M_draw:TEXT] Draw the text in string at the current position.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine drawstr(str)
!!         character(len=*),intent(in) :: str
!!
!!##DESCRIPTION
!!    Draw a text string at the current position. Uses current line color
!!    and thickness and text centering mode.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!       program demo_drawstr
!!       use M_draw
!!       implicit none
!!       integer :: idum
!!       call vinit('')
!!       ! by default the drawing surface is
!!       ! a square ranging from -1 to 1 in both
!!       ! the X and Y axis
!!
!!       call color(D_BLACK)    ! set current color to black
!!       call clear()           ! clear to current color
!!
!!       ! SET COMMON TEXT ATTRIBUTES
!!       call color(D_GREEN)    ! we want to draw in green
!!       call font('futura.m')  ! set font
!!       call textsize(0.1,0.1) ! font size
!!
!!       ! DRAW A STRING
!!       call move2(-1.0, 0.0)
!!       call drawstr('Hello')  ! draw string at current position
!!       ! note that current position is now at end of this string
!!
!!       ! CHANGE SOME TEXT ATTRIBUTES AGAIN
!!       call linewidth(20)     ! set line width
!!       call color(D_RED)      ! change color
!!       call textang(45.0)     ! change text angle
!!
!!       call drawstr(' World!')! draw string at current position
!!       idum=getkey()          ! pause
!!
!!       call vexit()           !  wrap up and exit graphics mode
!!
!!       end program demo_drawstr
!>
!!##NAME
!!    strlength(3f) - [M_draw:TEXT] return length of string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!       real function strlength(string)
!!       character(len=*),intent(in) :: string
!!
!!##DESCRIPTION
!!    Return the length of the string "STRING" in world units.
!!
!!##RETURNS
!!    STRLENGTH  length of string using current font size
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_strlength
!!    use :: M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    real    :: left
!!    real    :: baseline
!!    integer :: ipaws
!!    integer :: icolor=0
!!    real    :: texth=10.0
!!       !! set up drawing surface
!!       call prefsize(800, 400)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call page(-100.0, 300.0, -100.0, 100.0)
!!       call color(D_WHITE)
!!       call clear()
!!       call linewidth(40)
!!       call textsize(texth, texth)
!!       call xcentertext()
!!       call color(D_RED)
!!
!!       baseline=85.0
!!       call move2(0.0,baseline)
!!       call drawstr('If I Can Stop One Heart')
!!       baseline= baseline-texth*1.20
!!       call move2(0.0,baseline)
!!       call drawstr('by Emily Dickinson')
!!       call centertext(.false.)
!!
!!       texth=8.5
!!       baseline=baseline-texth*1.50
!!       call textsize(texth, texth)
!!       left=-90.0
!!
!!       call nextline('If I can stop one heart from breaking,')
!!       call nextline('I shall not live in vain;')
!!       call nextline('If I can ease one life the aching,')
!!       call nextline('Or cool one pain,')
!!       call nextline('Or help one fainting robin')
!!       call nextline('Unto his nest again,')
!!       call nextline('I shall not live in vain.')
!!
!!       ipaws=getkey()
!!       call vexit()
!!    contains
!!    subroutine nextline(string)
!!    character(len=*) :: string
!!    real :: xx
!!    !! reduce some duplicate code; very specific to this EXAMPLE
!!       call color(icolor)
!!       baseline=baseline-texth*1.5    ! move down before drawing line
!!          call polyfill(.true.)
!!             call makepoly()
!!                xx=strlength(string)
!!                call rect(left,baseline-texth*0.3,left+xx,baseline+texth)
!!             call closepoly()
!!          call polyfill(.false.)
!!       call color(D_WHITE)
!!       call move2(left, baseline)
!!       call drawstr(string)    ! draw string
!!       icolor=icolor+1         ! set pen color
!!    end subroutine nextline
!!
!!    end program demo_strlength
!>
!!##NAME
!!    boxtext(3f) - [M_draw:TEXT] stretch and draw the SOFTWARE string s so that it fits in the imaginary box
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine boxtext(x, y, l, h, s)
!!         real x, y, l, h, s
!!
!!##DESCRIPTION
!!    Draw the string S so that it fits in the imaginary box defined with
!!    bottom left hand corner at (x, y), length l, and height h. This only
!!    applies to software text. The textsize is temporarily resized so that
!!    the string fills the specified box.
!!##OPTIONS
!!    X,Y   position of lower left corner of box in world coordinates
!!    L     length of box in world coordinates
!!    H     height of box in world coordinates
!!    S     string to stretch into box and draw
!!
!!##EXAMPLE
!!
!!   Simple program:
!!
!!     program demo_boxtext
!!     use M_draw,     only : vinit,vexit,prefsize,vgetdev,clear,page
!!     use M_draw,     only : centertext,polyfill,font,linewidth,color
!!     use M_draw,     only : getkey
!!     use M_draw,     only : color,rect,boxtext
!!     use M_draw,     only : D_BLACK,   D_WHITE
!!     use M_draw,     only : D_RED,     D_GREEN,    D_BLUE
!!     use M_draw,     only : D_YELLOW,  D_MAGENTA,  D_CYAN
!!     implicit none
!!     real              :: x1=0.0,    x2=40.0,    y1=0.0,    y2=7.0
!!     real              :: xmin=1.0,  xmax=39.0,  ymin=1.0,  ymax=6.0
!!     integer           :: idum
!!        call prefsize(int(x2-x1)*25,int(y2-y1)*25)
!!        call vinit(' ')
!!        call page(x1,x2,y1,y2)
!!        call centertext(.true.)
!!        call font("times.rb")
!!        call color(D_GREEN)
!!        call clear()
!!        call linewidth(200)
!!        call color(D_CYAN)
!!        call polyfill(.false.)
!!        call rect(xmin,ymin,xmax,ymax)
!!        call color(D_WHITE)
!!        call polyfill(.true.)
!!        call rect(xmin,ymin,xmax,ymax)
!!        call color(D_BLACK)
!!        call boxtext(xmin,ymin,xmax-xmin,ymax-ymin,"This text is in the box")
!!        idum=getkey()
!!        call vexit()
!!     end program demo_boxtext
!>
!!##NAME
!!    boxfit(3f) - [M_draw:TEXT] resize the SOFTWARE text size so it fits in a box
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine boxfit(l, h, nchars)
!!         real l, h
!!         integer nchars
!!
!!##DESCRIPTION
!!    Set scale for text so that a string of the biggest characters in
!!    the font will fit in a box l by h. l and h are real values in world
!!    dimensions. This only applies to software text.
!>
!!##NAME
!!    textjustify(3f) - [M_draw:TEXT] general text justification (C only)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine textjustify(val)
!!        character(kind=c_short) :: ival
!!        character(kind=c_char) :: val
!!
!!##DESCRIPTION
!!    General (direct) control of text justification. The value of val is made
!!    up of the logical OR of the following predefined constants in draw.h
!!    (FOR C and Fortran only). D_LEFT, D_RIGHT, D_XCENTERED, D_TOP, D_BOTTOM,
!!    D_YCENTERED. Centering takes priority, as does RIGHT and TOP justification
!!    (if you were silly enough to set it to D_LEFT|D_RIGHT for EXAMPLE that
!!    is). A value of 0 (zero) (in all languages) resets the textjustification
!!    to the default.
!!
!!        ! from Fortran, use IANY() to OR the array of options, and CHAR()
!!        ! to convert the integer result to a C_CHAR type. KIND C_CHAR is
!!        ! defined by loading the intrinsic module for C bindings
!!        ! (ie. "USE ISO_C_BINDING").
!!        ival=iany([D_XCENTERED,D_YCENTERED])
!!        val=char(ival)
!!        call textjustify(val)
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_textjustify
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    use iso_c_binding
!!    implicit none
!!    real :: x1=-20.0, x2=20.0, y1=-20.0, y2=20.0
!!       call prefsize(int(x2-x1)*30,int(y2-y1)*30)
!!       !!call voutput('|ppmtogif >textjustify.3M_draw.gif')
!!       !!call vinit('p6')
!!       call vinit(' ')
!!       call page(x1,x2,y1,y2)
!!       call clear()
!!       call textsize(0.9, 1.4)
!!       call font("times.rb")
!!       call linewidth(20)
!!       call seejustify( "right|top",           &
!!               iany([d_right,d_top]),           -10.0, -10.0 )
!!       call seejustify( "right|ycentered",     &
!!               iany([d_right,d_ycentered]),     -10.0,   0.0 )
!!       call seejustify( "right|bottom",        &
!!               iany([d_right,d_bottom]),        -10.0, +10.0 )
!!       call seejustify( "xcentered|top",       &
!!               iany([d_xcentered,d_top]),         0.0, -10.0 )
!!       call seejustify( "xcentered|ycentered", &
!!               iany([d_xcentered,d_ycentered]),   0.0,   0.0 )
!!       call seejustify( "xcentered|bottom",    &
!!               iany([d_xcentered,d_bottom]),      0.0, +10.0 )
!!       call seejustify( "left|top",            &
!!               iany([d_left,d_top]),            +10.0, -10.0 )
!!       call seejustify( "left|ycentered",      &
!!               iany([d_left,d_ycentered]),      +10.0,   0.0 )
!!       call seejustify( "left|bottom",         &
!!               iany([d_left,d_bottom]),         +10.0, +10.0 )
!!       call vexit()
!!    contains
!!       subroutine seejustify(string,justify,x,y)
!!          implicit none
!!          real                    :: x, y
!!          real                    :: height, width
!!          integer(kind=c_short)   :: justify
!!          character(len=*)        :: string
!!          character(kind=c_char)  :: byte
!!          call color(D_RED)
!!          call move2(x-1.0,y); call draw2(x+1.0,y)
!!          call move2(x,y-1.0); call draw2(x,y+1.0)
!!          call circle(x,y,5.0)
!!          call color(D_BLUE)
!!          call move2(x,y)
!!          byte=char(justify)
!!          call textjustify(byte)
!!          call drawstr(string)
!!          call color(D_WHITE)
!!          call rmove2(-strlength(string),0.0)
!!          call rdraw2(+strlength(string),0.0)
!!          call getfontsize(width, height)
!!          call rmove2(0.0,height)
!!          call rmove2(-strlength(string),0.0)
!!          call rdraw2(+strlength(string),0.0)
!!       end subroutine seejustify
!!    end program demo_textjustify
!>
!!##NAME
!!    leftjustify(3f) - [M_draw:TEXT] left justify text
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine leftjustify()
!!
!!##DESCRIPTION
!!    Left justifies text. The text string will begin at the current position
!!    and extend to the notional right. Right justification and X centering
!!    are turned off.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_leftjustify
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    real    :: x1, x2, y1, y2
!!    real    :: scl, ax, bx
!!    integer :: key
!!       call prefsize(1200,120)
!!       call vinit(' ')
!!       x1=0; x2=40; y1=0; y2=4; scl=1.9
!!       call page(x1,x2,y1,y2)
!!       call textsize(0.9*scl,1.4*scl)
!!       call font("times.rb")
!!       call linewidth(200)
!!       AX=(x1+x2)/2+1; BX=y1+1.3
!!       call move2(AX,BX)
!!       call leftjustify()
!!       call color(D_GREEN)
!!       call drawstr("leftjustify()")
!!       call color(D_RED)
!!       call move2(AX-1.0,BX)
!!       call draw2(AX+1.0,BX)
!!       call move2(AX,BX-1.0)
!!       call draw2(AX,BX+1.0)
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    end program demo_leftjustify
!>
!!##NAME
!!    rightjustify(3f) - [M_draw:TEXT] right justify text
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine rightjustify()
!!
!!##DESCRIPTION
!!
!!    Right justifies text. The text string will begin at a point to the
!!    notional left of the current position and finish at the current
!!    position. Left justification and X centering are turned off.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_rightjustify
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    real    :: x1, x2, y1, y2
!!    real    :: scl, ax, bx
!!    integer :: key
!!       call prefsize(1200,120)
!!       call vinit(' ')
!!       x1=0; x2=40; y1=0; y2=4; scl=1.9
!!       call page(x1,x2,y1,y2)
!!       call textsize(0.9*scl,1.4*scl)
!!       call font("times.rb")
!!       call linewidth(200)
!!       AX=(x1+x2)/2+6; BX=y1+1.3
!!       call move2(AX,BX)
!!       call rightjustify()
!!       call color(D_GREEN)
!!       call drawstr("rightjustify()")
!!       call color(D_RED)
!!       call move2(AX-1.0,BX)
!!       call draw2(AX+1.0,BX)
!!       call move2(AX,BX-1.0)
!!       call draw2(AX,BX+1.0)
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    end program demo_rightjustify
!>
!!##NAME
!!    xcentertext(3f) - [M_draw:TEXT] set text centering mode on in X direction
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine xcentertext()
!!
!!##DESCRIPTION
!!    Set text centering mode on in X direction. Y justification is
!!    turned off.
!!
!!    Centers text in the X direction. The text string will begin at a
!!    point to the notional left of the current position and finish at a
!!    point to the right of the current position. Left justification and
!!    Right justification are turned off.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_xcentertext
!!    use M_draw
!!    implicit none
!!    real    :: x1, x2, y1, y2
!!    real    :: scl, ax, bx
!!    integer :: key
!!       call prefsize(1200,120)
!!       call vinit(' ')
!!       x1=0; x2=40; y1=0; y2=4; scl=1.9
!!       call page(x1,x2,y1,y2)
!!       call textsize(0.9*scl,1.4*scl)
!!       call font("times.rb")
!!       call linewidth(200)
!!       AX=(x1+x2)/2+4; BX=y1+1.3
!!       call move2(AX,BX)
!!       call xcentertext()
!!       call color(D_GREEN)
!!       call drawstr("xcentertext()")
!!       call color(D_RED)
!!       call move2(AX-1.0,BX)
!!       call draw2(AX+1.0,BX)
!!       call move2(AX,BX-1.0)
!!       call draw2(AX,BX+1.0)
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    end program demo_xcentertext
!>
!!##NAME
!!    topjustify(3f) - [M_draw:TEXT] top justify text
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine topjustify()
!!
!!##DESCRIPTION
!!    Top justifies text. The text string will be drawn with its upper edge
!!    aligned with the current Y position. Bottom justification and Y centering
!!    are turned off.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_topjustify
!!    use M_draw
!!    implicit none
!!    real    :: x1, x2, y1, y2
!!    real    :: scl, ax, bx
!!    integer :: key
!!       call prefsize(1200,120)
!!       call vinit(' ')
!!       x1=0; x2=40; y1=0; y2=4; scl=1.9
!!       call page(x1,x2,y1,y2)
!!       call textsize(0.9*scl,1.4*scl)
!!       call font("times.rb")
!!       call linewidth(200)
!!       AX=(x1+x2)/2+0.3; BX=y1+3.3
!!       call move2(AX,BX)
!!       call topjustify()
!!       call color(D_BLUE)
!!       call drawstr("topjustify()")
!!       call color(D_RED)
!!       call move2(AX-1.0,BX)
!!       call draw2(AX+1.0,BX)
!!       call move2(AX,BX-1.0)
!!       call draw2(AX,BX+1.0)
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    end program demo_topjustify
!>
!!##NAME
!!    bottomjustify(3f) - [M_draw:TEXT] bottom justify text
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine bottomjustify()
!!
!!##DESCRIPTION
!!
!!    Bottom justifies text. The text string will be drawn with the lower
!!    edge aligned with the current Y position. Top justification and Y
!!    centering are turned off.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_bottomjustify
!!    use M_draw
!!    implicit none
!!    real    :: x1, x2, y1, y2
!!    real    :: scl, ax, bx
!!    integer :: key
!!       call prefsize(1200,120)
!!       call vinit(' ')
!!       x1=0; x2=40; y1=0; y2=4; scl=1.9
!!       call page(x1,x2,y1,y2)
!!       call textsize(0.9*scl,1.4*scl)
!!       call font("times.rb")
!!       call linewidth(200)
!!       AX=(x1+x2)/2+1; BX=y1+1.3
!!       call move2(AX,BX)
!!       call bottomjustify()
!!       call color(D_BLUE)
!!       call drawstr("bottomjustify()")
!!       call color(D_RED)
!!       call move2(AX-1.0,BX)
!!       call draw2(AX+1.0,BX)
!!       call move2(AX,BX-1.0)
!!       call draw2(AX,BX+1.0)
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    end program demo_bottomjustify
!>
!!##NAME
!!    ycentertext(3f) - [M_draw:TEXT] center text in the Y direction
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine ycentertext()
!!
!!##DESCRIPTION
!!    Centers text in the Y direction. The text string will so that its
!!    center line is aligned with the current y position. Top justification
!!    and Bottom justification are turned off.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_ycentertext
!!    use M_draw
!!    implicit none
!!    real    :: x1, x2, y1, y2
!!    real    :: scl, ax, bx
!!    integer :: key
!!       call prefsize(1200,120)
!!       call vinit(' ')
!!       call color(D_BLACK)
!!       call clear()
!!       x1=0; x2=40; y1=0; y2=4; scl=1.9
!!       call page(x1,x2,y1,y2)
!!       call textsize(0.9*scl,1.4*scl)
!!       call font("times.rb")
!!       call linewidth(200)
!!       AX=(x1+x2)/2.0; BX=(y1+y2)/2.0
!!       call move2(AX,BX)
!!       call ycentertext()
!!       call color(D_MAGENTA)
!!       call drawstr("ycentertext()")
!!       call color(D_CYAN)
!!       call move2(AX-1.0,BX)
!!       call draw2(AX+1.0,BX)
!!       call move2(AX,BX-1.0)
!!       call draw2(AX,BX+1.0)
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    end program demo_ycentertext
!!
!>
!!##NAME
!!    textslant(3f) - [M_draw:TEXT] Defines the obliqueness of the fonts.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine textslant(var)
!!        real var
!!
!!##DESCRIPTION
!!
!!    Defines the obliqueness of the fonts. This is a simplistic method that
!!    allows you to generate italicized versions of the software fonts. The x-
!!    values of the software font coordinates after the current textsize()
!!    values are applied are multiplied by (1+val).
!!
!!    Note that this means the same value tilts the characters less the taller
!!    the characters are relative to their width.
!!
!!    Generally, practical values are generally between -1 and 1 times the
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_textslant
!!    use M_draw
!!    implicit none
!!    real    :: x1, x2, y1, y2
!!    real    :: scl
!!    integer :: key
!!       call prefsize(1200,300)
!!       call vinit(' ')
!!       call color(D_BLACK)
!!       call clear()
!!       x1=0.0; x2=40.0; y1=0.0; y2=10.0; scl=3*0.7
!!       call page(x1,x2,y1,y2)
!!       call font("times.rb")
!!       call linewidth(180)
!!       call textsize(0.8*scl,1.2*scl)
!!       call move2( x1+.3,y1+.4)
!!       call color(D_RED)
!!       call textslant(0.0);  call drawstr("textslant(0.0); ")
!!       !
!!       call color(D_GREEN)
!!       call textslant(-1.0); call drawstr(" textslant(-1.0);")
!!       !
!!       call color(D_BLUE)
!!       call textslant(1.0);  call drawstr(" textslant(1.0);")
!!       !
!!       call textsize(0.8*scl,1.2*3*scl)
!!       call move2(x1+.3,y1+3+.4)
!!       call color(D_MAGENTA)
!!       call textslant(1.0); call drawstr(" textslant(1.0);")
!!       !
!!       call textsize(0.8*scl,1.2*scl)
!!       call color(D_CYAN)
!!       call textslant(0.3); call drawstr(" textslant(0.3);")
!!       !
!!       call color(D_WHITE)
!!       call textslant(0.5); call drawstr(" textslant(0.5);")
!!       !
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    end program demo_textslant
!>
!!##NAME
!!    textweight(3f) - [M_draw:TEXT] Defines the weight of the fonts.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        subroutine textweight(ival)
!!        integer ival
!!##DESCRIPTION
!!
!!     Defines the weight of the fonts. Currently, the predefined constants
!!     in C and Fortran are D_NORMAL and D_BOLD; which correspond to 0
!!     and 1. This is not the same as using linethickess to change the
!!     appearance of a software font. The font is redrawn multiple times
!!     with a slight offset to create the bold appearance.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_textweight
!!    use M_draw
!!    implicit none
!!    real,parameter :: w=40.0
!!    integer        :: key
!!       call prefsize(600,600)
!!       call vinit(' ')
!!       call color(D_BLACK)
!!       call clear()
!!       call color(D_YELLOW)
!!       call page(-w,w,-w,w)
!!       call font("times.rb")
!!       call linewidth(180)
!!       call textsize(15.0,15.0)
!!       call centertext(.true.)
!!       call linewidth(0);call color(D_BLUE)
!!       call move2(0.0, W/2.0)
!!
!!       call textweight(0)
!!       call drawstr('NORMAL')
!!
!!       call linewidth(0);call color(D_MAGENTA)
!!       call move2(0.0, 0.0-W/2.0)
!!
!!       call textweight(1)
!!       call drawstr('BOLD')
!!
!!       call vflush()
!!       key=getkey()
!!       call vexit()
!!    end program demo_textweight
!>
!!##NAME
!!    linewidth(3f) - [M_draw:LINESTYLE] set line width in rasters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine linewidth(iwidth)
!!         integer iwidth
!!##DESCRIPTION
!!
!!    Set the current line width in units of 1/10,000 of the X size of the
!!    display surface
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_linewidth
!!    use M_draw,     only : prefsize, vinit, clear, getkey, drawstr, page
!!    use M_draw,     only : textsize, ycentertext, rdraw2, rmove2
!!    use M_draw,     only : move2, draw2, vexit, color, linewidth, font
!!    use M_draw,     only : D_BLACK,   D_WHITE
!!    use M_draw,     only : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,     only : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    real    :: b=0.5
!!    integer :: ipaws
!!    call prefsize(1000,200)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!    call color(D_YELLOW)
!!    call clear()
!!    call textsize(2.4,3.0)
!!    call font("futura.m")
!!    call ycentertext()
!!    ! draw circles with different precisions and label center of circles
!!    call color(D_WHITE)
!!    call linewidth(0)
!!    call move2(-20.0, -3.5)
!!    call rdraw2(10.0,0.0)
!!    call rmove2(3.0,0.0)
!!    call linewidth(180)
!!    call color(D_WHITE)
!!    call drawstr("0")
!!    call color(D_RED)
!!    call linewidth(100)
!!    call move2(-20.0,0.0)
!!    call rdraw2(10.0,0.0)
!!    call rmove2(3.0,0.0)
!!    call linewidth(180)
!!    call color(D_WHITE)
!!    call drawstr("100")
!!    call color(D_GREEN)
!!    call linewidth(200)
!!    call move2(-20.0,3.5)
!!    call rdraw2(10.0,0.0)
!!    call rmove2(3.0,0.0)
!!    call linewidth(180)
!!    call color(D_WHITE)
!!    call drawstr("200")
!!    call color(D_BLUE)
!!    call linewidth(300)
!!    call move2(3.0,-3.5)
!!    call rdraw2(10.0,0.0)
!!    call rmove2(3.0,0.0)
!!    call linewidth(180)
!!    call color(D_WHITE)
!!    call drawstr("300")
!!    call color(D_MAGENTA)
!!    call linewidth(400)
!!    call move2(3.0,0.0)
!!    call rdraw2(10.0,0.0)
!!    call rmove2(3.0,0.0)
!!    call linewidth(180)
!!    call color(D_WHITE)
!!    call drawstr("400")
!!    call color(D_CYAN)
!!    call linewidth(500)
!!    call move2(3.0,3.5)
!!    call rdraw2(10.0,0.0)
!!    call rmove2(3.0,0.0)
!!    call linewidth(180)
!!    call color(D_WHITE)
!!    call drawstr("500")
!!    call linewidth(200)
!!    call color(D_BLUE)
!!    call move2(-25.0,-5.0)
!!    call draw2(-25.0,5.0)
!!    call draw2(25.0,5.0)
!!    call draw2(25.0,-5.0)
!!    call draw2(-25.0,-5.0)
!!    ipaws=getkey()
!!    ! exit graphics mode
!!    call vexit()
!!    end program demo_linewidth
!>
!!##NAME
!!    dashcode(3f) - [M_draw:LINESTYLE] set dash pattern length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine dashcode(dashlen)
!!         real dashlen
!!
!!##DESCRIPTION
!!
!!    Set the current dash length (in world units) to be dashlen.
!!
!!##IMAGE
!!
!!    The sample graphic shows a line segment being drawn using the same
!!    linestyle except the dashcode is being changed. Note that the dashcode
!!    is in world units,
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!   program demo_dashcode
!!   use M_draw
!!   implicit none
!!   integer        :: icolor
!!   integer        :: ikey
!!   real,parameter :: b=0.5
!!   real           :: x, y
!!   real           :: dcode
!!      call prefsize(1000,200)
!!      call vinit(' ')
!!      call color(D_BLACK)
!!      call clear()
!!      call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!      call textsize(2.0, 2.4)
!!      call font("futura.m")
!!      call ycentertext()
!!      icolor=7; dcode=0.010/5.0; X=-23.0; Y=3.5  ; call line()
!!      icolor=1; dcode=0.025/5.0; X=-23.0; Y=-0.0 ; call line()
!!      icolor=2; dcode=0.030/5.0; X=-23.0; Y=-3.5 ; call line()
!!      icolor=4; dcode=0.050/5.0; X=0.0;   Y=3.5  ; call line()
!!      icolor=5; dcode=0.075/5.0; X=0.0;   Y=-0.0 ; call line()
!!      icolor=6; dcode=0.100/5.0; X=0.0;   Y=-3.5 ; call line()
!!      call linewidth(200)
!!      call color(D_BLUE)
!!      call move2(-25.0, -5.0)
!!      call draw2(-25.0,  5.0)
!!      call draw2(25.0,   5.0)
!!      call draw2(25.0,  -5.0)
!!      call draw2(-25.0, -5.0)
!!      ikey=getkey()
!!      call vexit()
!!   contains
!!   subroutine line
!!   character(len=6) :: string
!!      call linestyle('11100100')
!!      call dashcode(dcode)
!!      call color(icolor)
!!      call linewidth(70)
!!      call move2(X,Y)
!!      call rdraw2(10.0, 0.0)
!!      call rmove2(3.0, 0.0)
!!      call linestyle('')
!!      call linewidth(180)
!!      call color(D_WHITE)
!!      write(string,'(f6.3)')dcode
!!      call drawstr (string)
!!   end subroutine line
!!   end program demo_dashcode
!>
!!##NAME
!!     linestyle(3f) - [M_draw:LINESTYLE] set the line dash pattern
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine linestyle(style)
!!    character *(*) style
!!
!!##DESCRIPTION
!!
!!     Set the current linestyle to style. Linestyles are specified by giving
!!     a nominal length of a single dash and a character string consisting
!!     of 1's and 0's (zeros) that specify when to draw a dash and when not
!!     to draw a dash. "1" is for a dash , "0" is for a gap. Linestyles will
!!     follow curves and "go around" corners.
!!
!!     To reset to a solid line style, enter a linestyle of " ". If a
!!     linestyle is set or reset, the accumulated information as to where
!!     on a curve (or line) a dash is to be draw is also reset.
!!
!!     The sample program shows a line segment being drawn using the same
!!     dashcode (to specify dash length) except the linestyle is being
!!     changed. Note that the dashcode is in world units.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_linestyle
!!     ! A program showing basic line styles.
!!     use M_draw
!!     implicit none
!!     character(len=40) :: device
!!     integer           :: ios, idum, i
!!
!!        print*,'Enter output device: '
!!        read(*,'(a)',iostat=ios)device
!!        if(ios.ne.0)device=' '
!!
!!        call prefsize(1000,1000)
!!        call vinit(device)
!!        call vsetflush(.true.)
!!        call up(0.0, 1.0, 0.0)
!!        call perspective(90.0, 1.0, 0.3, 3.0)
!!        call translate(0.0, 0.0, -1.3)
!!        call linewidth(30)
!!
!!        call drawscene()
!!        call rotate(-30.0, 'y')
!!        call rotate(-30.0, 'x')
!!        call drawscene()
!!
!!        call vexit()
!!        contains
!!
!!        subroutine drawscene()
!!
!!           call color(D_BLACK)
!!           call clear()
!!
!!           call color(D_GREEN)
!!           call dashcode(0.03)
!!
!!           call linestyle(' ')
!!           call xcentertext()
!!           call move2(-0.45, 0.9)
!!           call drawstr('Linestyle: "10"')
!!           call move2(-0.45, 0.7)
!!           call drawstr('Linestyle: "110"')
!!           call move2(-0.45, 0.5)
!!           call drawstr('Linestyle: "111010"')
!!           call move2(-0.45, 0.3)
!!           call drawstr('Linestyle: "0001"')
!!
!!           call linestyle('10')
!!           call move2(-0.9, 0.9)
!!           call draw2( 0.0, 0.9)
!!           call circle(0.6, 0.6, 0.4)
!!
!!           call drawbox(0.9)
!!           call drawsine(0.9)
!!
!!           call color(D_RED)
!!           call linestyle('110')
!!           call move2(-0.9, 0.7)
!!           call draw2( 0.0, 0.7)
!!           call circle(0.6, 0.6, 0.3)
!!           call drawbox(0.7)
!!           call drawsine(0.7)
!!
!!           call color(D_CYAN)
!!           call linestyle('111010')
!!           call move2(-0.9, 0.5)
!!           call draw2( 0.0, 0.5)
!!           call circle(0.6, 0.6, 0.2)
!!           call drawbox(0.5)
!!           call drawsine(0.5)
!!
!!           call color(D_YELLOW)
!!           call linestyle('0001')
!!           call move2(-0.9, 0.3)
!!           call draw2( 0.0, 0.3)
!!           call circle(0.6, 0.6, 0.1)
!!           call drawbox(0.3)
!!           call drawsine(0.3)
!!
!!           idum=getkey()
!!
!!           end subroutine drawscene
!!
!!           subroutine drawbox(scl)
!!           real :: scl
!!
!!           call pushmatrix()
!!
!!           call rotate(30.0, 'x')
!!           call rotate(60.0, 'y')
!!           call translate(-0.7, -1.2, 0.0)
!!           call scale(scl, scl, scl)
!!
!!           call move(0.0, 0.0, 0.0)
!!
!!           call draw(1.0, 0.0, 0.0)
!!           call draw(1.0, 1.0, 0.0)
!!           call draw(0.0, 1.0, 0.0)
!!           call draw(0.0, 0.0, 0.0)
!!
!!           call draw(0.0, 0.0, -1.0)
!!           call draw(1.0, 0.0, -1.0)
!!           call draw(1.0, 1.0, -1.0)
!!           call draw(0.0, 1.0, -1.0)
!!           call draw(0.0, 0.0, -1.0)
!!
!!           call move(0.0, 1.0, -1.0)
!!           call draw(0.0, 1.0, 0.0)
!!
!!           call move(1.0, 1.0, 0.0)
!!           call draw(1.0, 1.0, -1.0)
!!
!!           call move(1.0, 0.0, 0.0)
!!           call draw(1.0, 0.0, -1.0)
!!
!!           call popmatrix()
!!
!!           end subroutine drawbox
!!
!!           subroutine drawsine(s)
!!           real    s, RAD, AMP
!!           parameter(RAD = 0.5, AMP = 0.04)
!!           real    a, x, y, z
!!
!!           call pushmatrix()
!!
!!           call translate(RAD + 0.2, -0.5, 0.0)
!!           call scale(s, s, s)
!!
!!           call move(RAD, 0.0, 0.0)
!!           a=0.0
!!           do i = 0, 2*314, 2
!!              x = RAD * cos(a)
!!              z = RAD * sin(a)
!!              y = AMP * sin(a * 6.0)
!!
!!              call draw(x, y, z)
!!              a=a+0.02
!!           enddo
!!           call popmatrix()
!!           end subroutine drawsine
!!     end program demo_linestyle
!>
!!##NAME
!!    clear(3f) - [M_draw:COLOR] Clears screen to current color
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine clear()
!!
!!##DESCRIPTION
!!
!!    Clears the screen to the current color. Causes a new page to begin
!!    on file-based devices that support multiple pages (Currently, the
!!    pixmap devices do not support multiple pages).
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!      program demo_clear
!!      use M_draw, only  : prefsize, vinit, ortho2, clear, getkey
!!      use M_draw, only  : vexit, color, circle, polyfill
!!      use M_draw, only  : D_BLACK,   D_WHITE
!!      use M_draw, only  : D_RED,     D_GREEN,    D_BLUE
!!      use M_draw, only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!      implicit none
!!      integer :: ipaws
!!
!!      call prefsize(300,300)
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      ipaws=getkey()
!!      call ortho2(-100.0,100.0,-100.0,100.0)
!!
!!      call color(D_BLACK)               ! set current  color
!!      call clear()                ! clear background to current color
!!      call color(D_RED)               ! set color to draw with
!!      call circle(0.0,0.0,50.0)
!!      ipaws=getkey()              ! pause for a keypress on interactive devices
!!
!!      call color(D_GREEN)               ! make a second page
!!      call clear()
!!      call polyfill(.true.)
!!      call color(D_YELLOW)
!!      call circle(0.0,0.0,50.0)
!!      ipaws=getkey()
!!
!!      call vexit()
!!
!!      end program demo_clear
!>
!!##NAME
!!    color(3f) - [M_draw:COLOR] Set current color
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine color(col)
!!         integer,intent(in) :: col
!!
!!##DESCRIPTION
!!
!! Set the current color. The standard colors are as follows:
!!
!!       d_black  =  0  d_red      =  1  d_green  =  2  d_yellow  =  3
!!       d_blue   =  4  d_magenta  =  5  d_cyan   =  6  d_white   =  7
!!
!!##OPTION
!!     COL  A color number from 0 to 255. To define additional
!!          colors see mapcolor(3f).
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_color
!!     use M_draw
!!     implicit none
!!     real    :: b=0.5
!!     real    :: y1,y2,ym,x1,x2
!!     real    :: width=50.0/8.0,width2
!!     integer :: i
!!     integer :: ipaws
!!        !! set up long bar as plotting area
!!        call prefsize(1000,200)
!!        call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!        call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
!!        call textsize( 3.5, 4.0)
!!        call font('futura.m')
!!        call centertext(.true.)
!!        call linewidth(90)
!!        y1=-5
!!        y2=5
!!        ym=0
!!        x1=-25+.05*width
!!        ! draw colored rectangle and a circle and label center of circle
!!        ! and repeat from colors 0 to 7.
!!        width2=width*0.95
!!        call linewidth(40)
!!        do i=0,7
!!           call color(i)
!!           x2=x1+width2
!!           call polyfill(.true.)
!!           call makepoly()
!!           call rect(x1,y1,x2,y2)
!!           call closepoly()
!!           call color(mod(i+1,7)+1)
!!           call move2((x1+x2)/2.0,ym)
!!           call print(i)     ! convert number to string and draw it
!!           call polyfill(.false.)
!!           call circle((x1+x2)/2.0, ym, (x2-x1)/2.10)
!!           x1=x1+width
!!        enddo
!!        ipaws=getkey()
!!        call vexit()
!!     end program demo_color
!>
!!##NAME
!!    mapcolor(3f) - [M_draw:COLOR] set a color index using RGB values
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine mapcolor(indx, red, green, blue)
!!         integer,intent(in) :: indx, red, green, blue
!!
!!##DESCRIPTION
!!
!!    Set the color map index indx to the color represented by (red, green,
!!    blue). If the device has no color map this call does nothing.
!!
!!    rgb values are in the range of 0 to 255.
!!
!!##OPTIONS
!!    INDX    color index number, in range 0 to 255
!!    RED     red component of color being defined, in range 0 to 255
!!    GREEN   green component of color being defined, in range 0 to 255
!!    BLUE    blue component of color being defined, in range 0 to 255
!!
!!##EXAMPLE
!!
!!  Color wheel EXAMPLE:
!!
!!    program demo_mapcolor
!!    !   good program to exercise color tables, and look at differences
!!    !   when actual output device has a color table that is dynamic,
!!    !   or only has a small color table (a frame in this program takes
!!    !   at least SLICES*RINGS colors to produce accurately).
!!    use M_draw
!!    implicit none
!!    real                 :: lightstep
!!    integer              :: ii, iframe
!!    integer, parameter    :: SLICES = 30
!!    integer, parameter    :: RINGS = 8
!!    real                 :: LIGHTNESS
!!    integer, parameter    :: BOX = 500
!!    integer              :: ipaws
!!    integer              :: istart, iend
!!    character(len=20)    :: device
!!       call prefsize(BOX, BOX)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       call polyfill(.true.)
!!       call color(D_BLACK)
!!       call clear()
!!       call color(D_WHITE)
!!       call page(-110.0/2.0+5, 85.0/2.0+5, -110.0/2.0, 110.0/2.0)
!!       LIGHTNESS = 100.0
!!       lightstep = -5
!!
!!       call vgetdev(device)
!!       select case (device)
!!       case ('p6', 'p3', 'ppm') ! just do one wheel
!!          istart = 10
!!          iend = 10
!!          LIGHTNESS = 50.0
!!       case default           ! do many lightnesses
!!          istart = 1
!!          iend = 19
!!       end select
!!
!!       do ii = istart, iend
!!          iframe = ii
!!          call color(D_BLACK)
!!          call clear()
!!          call color(D_WHITE)
!!          call wheel()
!!          LIGHTNESS = LIGHTNESS + LIGHTSTEP
!!          ipaws = getkey()
!!       end do
!!       call vexit()
!!    contains
!!    !
!!    subroutine wheel() ! draw an entire wheel
!!    character(len=40) :: inline
!!    real              :: hue_val
!!    integer           :: ii
!!       call textang(0.0)
!!       call color(D_WHITE)
!!       call textsize(5.0, 6.0)
!!       call font('times.r')
!!       call move2(0.0, 103.0/2.0)
!!       call centertext(.true.)
!!       call linewidth(30)
!!       call drawstr('COLOR WHEEL')
!!       call linewidth(0)
!!       call textsize(2.5, 2.5)
!!       call font('futura.l')
!!       call move2(0.0, 90.0/2.0)
!!       write (inline, '("lightness=",f6.2)') LIGHTNESS
!!       call linewidth(30)
!!       call drawstr(inline)
!!       call linewidth(0)
!!       call textsize(1.5, 1.5)
!!       hue_val = 0
!!       do ii = SLICES, 1, -1
!!          call slice(hue_val)
!!       end do
!!       call centertext(.false.)
!!    end subroutine wheel
!!    !
!!    subroutine slice(hue_val) ! draw a slice
!!    integer           :: buffer
!!    real              :: hue_val, ang_inc
!!    character(len=40) :: inline
!!    real              :: step
!!    real              :: X1, X2, X3, X4
!!    real              :: Y1, Y2, Y3, Y4
!!    !
!!    integer           :: maxcolors, current_color
!!    integer           :: ir, ig, ib
!!    real              :: r, g, b
!!    real              :: saturation
!!    !
!!    integer           :: status
!!    integer           :: icount
!!    real              :: angle1, angle2
!!    real              :: radius1, radius2, radius3, radius4
!!    !
!!    integer, save      :: color_count = 0
!!       !
!!       buffer = 8
!!       ANG_INC = 360.0/SLICES
!!       angle1 = hue_val - ANG_INC/2
!!       angle2 = angle1 + ANG_INC
!!       saturation = 100
!!       radius1 = 32
!!       radius3 = radius1 + 4
!!       radius4 = radius1 + 7
!!       ! draw tic from wheel to start of angle label
!!       call color(D_WHITE)
!!       call linewidth(40)
!!       call move2(radius1*cosd(hue_val), radius1*sind(hue_val))
!!       call draw2(radius3*cosd(hue_val), radius3*sind(hue_val))
!!       ! draw degree label at tic
!!       call textang(hue_val)
!!       call move2(radius4*cosd(hue_val), radius4*sind(hue_val))
!!       write (inline, '(i0)') nint(hue_val)
!!       call linewidth(20)
!!       call drawstr(inline)
!!       call linewidth(0)
!!       step = radius1/real(RINGS)
!!       radius2 = radius1 - step
!!       ! draw a chunk in a slice
!!       MAXCOLORS = (256) - buffer
!!       do icount = RINGS + 1, 2, -1
!!          ! add buffer to leave base colors alone
!!          CURRENT_COLOR = MOD(color_count, MAXCOLORS) + buffer
!!          color_count = color_count + 1
!!          ! fancy mapcolor
!!          call hlsrgb(hue_val, LIGHTNESS, saturation, r, g, b, status)
!!          ir = int(r*255.0/100.0 + 0.50)
!!          ig = int(g*255.0/100.0 + 0.50)
!!          ib = int(b*255.0/100.0 + 0.50)
!!          call mapcolor(CURRENT_COLOR, ir, ig, ib)
!!          call color(CURRENT_COLOR)
!!          !
!!          X1 = cosd(angle1)*radius2
!!          Y1 = sind(angle1)*radius2
!!          X2 = cosd(angle1)*radius1
!!          Y2 = sind(angle1)*radius1
!!          !
!!          X3 = cosd(angle2)*radius2
!!          Y3 = sind(angle2)*radius2
!!          X4 = cosd(angle2)*radius1
!!          Y4 = sind(angle2)*radius1
!!          !
!!          call makepoly()
!!          call move2(X1, Y1)
!!          call draw2(X2, Y2)
!!          call draw2(X4, Y4)
!!          call draw2(X3, Y3)
!!          call closepoly()
!!          !
!!          saturation = saturation - 100.0/RINGS
!!          radius1 = radius2
!!          radius2 = radius1 - step
!!       end do
!!       hue_val = hue_val + ANG_INC
!!    end subroutine slice
!!    !
!!    subroutine hlsrgb(H, L, S, R, G, B, status)
!!    ! convert HLS(hue,lightness,saturation) values to RGB components
!!    !     given  : hue as a value of 0 to 360 degrees.
!!    !     .        lightness and saturation each as a value of 0 to 100.
!!    !     desired: r, g, and b each as a value of 0 to 100.
!!    !
!!    real, intent(in)   :: H, L, S
!!    real, intent(out)  :: R, G, B
!!    integer           :: status
!!    real              :: hue, lightness, saturation
!!    real              :: clr1, clr2
!!       ! passively report on bad input values
!!       if (h < 0.0 .or. h > 360.0) status = 1
!!       if (l < 0.0 .or. l > 100.0) status = 1
!!       if (s < 0.0 .or. s > 100.0) status = 1
!!       hue = H
!!       lightness = L/100.0
!!       saturation = S/100.0
!!       if (saturation == 0.0) then
!!          R = lightness
!!          G = lightness
!!          B = lightness
!!       end if
!!       if (lightness <= 0.50) then
!!          clr2 = lightness*(1.0 + saturation)
!!       else
!!          clr2 = lightness + saturation - lightness*saturation
!!       end if
!!       clr1 = 2.0*lightness - clr2
!!       R = rgbval(clr1, clr2, hue + 120.0)*100.0
!!       G = rgbval(clr1, clr2, hue)*100.0
!!       B = rgbval(clr1, clr2, hue - 120.0)*100.0
!!    end subroutine hlsrgb
!!    real function rgbval(clr1,clr2,h)
!!    ! rgbval(3fp): ensure a value is in the appropriate range and quadrant
!!    real    :: clr1,clr2
!!    real    :: h
!!    real    :: h2
!!       h2=h
!!       do
!!          if(h2 > 360.0 ) then
!!             h2=h2-360.0
!!             cycle
!!          endif
!!          exit
!!       enddo
!!       do
!!          if( h2  <  0.0 ) then
!!             h2=h2+360.0
!!             cycle
!!          endif
!!          exit
!!       enddo
!!       if(h2 < 60.0 ) then
!!          rgbval=clr1+(clr2-clr1)*h2/60.0
!!       else if(h2 < 180.0) then
!!          rgbval=clr2
!!       else if(h2 < 240.0) then
!!          rgbval=clr1+(clr2-clr1)*(240.0-h2)/60.0
!!       else
!!          rgbval=clr1
!!       endif
!!    end function rgbval
!!    end program demo_mapcolor
!>
!!##NAME
!!    clipping(3f) - [M_draw:CLIPPING] Turn clipping on or off
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine clipping(onoff)
!!
!!          logical onoff
!!##DESCRIPTION
!!
!! Turn clipping on or off. Note: on some devices
!! turning clipping off may not be a good idea.
!>
!!##NAME
!!    getkey(3f) - [M_draw:INTERACTIVE] Return ASCII ordinal of next key typed
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          integer function getkey()
!!
!!##DESCRIPTION
!!
!!    Return the ASCII ordinal of the next key typed at the keyboard. If the
!!    device has no keyboard getkey(3) returns -1.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_getkey
!!      use :: M_draw
!!      implicit none
!!      integer :: ichar
!!      !! set up drawing environment
!!      call prefsize(600,600)
!!      call voutput('+')
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-300.0,300.0,-300.0,300.0)
!!      call textsize(500.0,500.0)
!!      call linewidth(130)
!!      call centertext(.true.)
!!      call color(D_BLACK)
!!      call clear()
!!      write(*,*)'press any character to see it displayed in the default font'
!!      write(*,*)'Enter "q" to quit'
!!      do
!!        ichar=getkey()
!!        if(ichar.lt.0)then
!!           write(*,*)'this device does not support getkey'
!!           exit
!!        elseif(ichar.ne.0)then
!!           call color(D_BLACK)
!!           call clear()
!!           call color(D_BLUE)
!!           call move2(0.0,0.0)
!!           call drawstr(char(ichar))
!!        endif
!!        if(char(ichar).eq.'q')then
!!           write(*,*)'press any key to exit'
!!           ichar=getkey()
!!           exit
!!        endif
!!      enddo
!!      call vexit()
!!      end program demo_getkey
!>
!!##NAME
!!    checkkey(3f) - [M_draw:INTERACTIVE] Returns zero if no key is pressed or ASCII ordinal
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          integer function checkkey()
!!
!!##DESCRIPTION
!!
!!    Returns zero if no key is pressed or the ASCII ordinal of the key
!!    that was pressed.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_checkkey
!!    use :: M_draw
!!    implicit none
!!    integer :: ichar
!!    !! set up drawing environment
!!    call prefsize(600,600)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!    call ortho2(-300.0,300.0,-300.0,300.0)
!!    call textsize(500.0,500.0)
!!    call linewidth(130)
!!    call centertext(.true.)
!!    call color(D_BLACK)
!!    call clear()
!!    write(*,*)'press any character to see it displayed in the default font'
!!    write(*,*)'Enter "q" to quit'
!!    do
!!      ichar=checkkey()
!!      if(ichar.lt.0)then
!!         write(*,*)'this device does not support checkkey'
!!         exit
!!      elseif(ichar.ne.0)then
!!         call color(D_BLACK)
!!         call clear()
!!         call color(D_BLUE)
!!         call move2(0.0,0.0)
!!         call drawstr(char(ichar))
!!      endif
!!      if(char(ichar).eq.'q')then
!!         write(*,*)'press any key to exit'
!!         ichar=getkey()
!!         exit
!!      endif
!!    enddo
!!    call vexit()
!!    end program demo_checkkey
!>
!!##NAME
!!    getstring(3f) - [M_draw:INTERACTIVE] Read in a string, echoing it in current font
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          integer function getstring(bcol, string)
!!          integer bcol
!!          character *(*) string
!!##DESCRIPTION
!!
!!    Read in a string, echoing it in the current font, using the current
!!    color and the current transformation.
!!
!!    getstring(3f) interprets the Backspace key (ASCII 8) and the Del
!!    key (ASCII 127) as erasing characters. An EOT (ASCII 4) or a Carriage
!!    return (ASCII 13) will terminate input.
!!
!!##OPTIONS
!!    BCOL  is the background color which is used for erasing characters
!!          after a backspace or a delete key is received.
!!##RETURNS
!!          getstring(3f) returns the number of characters read. Getstring
!!          does not check for overflow in the input buffer string.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_getstring
!!    use M_draw
!!    implicit none
!!    ! reading a string from graphic input with getstring(3f)
!!    character(len=128) :: buf(10)
!!    character(len=20)  :: dev
!!    character(len=20)  :: fname
!!    integer            :: ios, i, n
!!    real               :: shft, tsize, y
!!
!!    print*, 'Enter device:'
!!    read (*, '(a)',iostat=ios) dev
!!    if(ios.ne.0)dev=' '
!!
!!    print*, 'Enter a font name:'
!!    read (*, '(a)',iostat=ios) fname
!!    if(ios.ne.0)fname='futura.l'
!!    if(fname.eq.'')fname='futura.l'
!!
!!    call vinit(dev)
!!
!!    call font(fname)
!!
!!    call clipping(.false.)
!!
!!    shft=0.14
!!    call window(-1.0-shft, 1.0-shft, -1.0, 1.0, 1.0, -1.0)
!!    call lookat(0.0-shft, 0.0-shft, 1.0, 0.0, 0.0, 0.0, 0.0)
!!
!!    call rotate(30.0, 'x')
!!    call rotate(30.0, 'z')
!!    call rotate(60.0, 'y')
!!
!!    call color(D_BLACK)
!!    call clear()
!!    call color(D_YELLOW)
!!
!!    tsize=0.25
!!    call textsize(tsize/2.5, tsize)
!!    call linewidth(100)
!!    call rect(-0.5, -0.5, 0.5, 0.5)
!!
!!    y=0.5
!!
!!    call linewidth(40)
!!    call move2(-0.5, y)
!!    call color(D_GREEN)
!!    call drawstr('getstring(3f) demo')
!!
!!    write(*,*)'Enter 10 lines up to 128 characters long.'
!!    write(*,*)'Program ends on blank line.'
!!
!!    do n=1,10
!!       y=y-tsize
!!       call move2(-0.5, y)
!!       i = getstring(D_BLACK, buf(n))
!!       write(*,'(/,a,i0,a,i0)',advance='no')'N=',n,' I=',i
!!       if(n.ge.1 .and. n.le.size(buf))write(*,*)' BUF=',trim(buf(n)(:i))
!!       buf(n)(min(128,i+1):)=' '
!!       if(i.le.0)exit ! exit on empty line
!!    enddo
!!
!!    call vexit()
!!
!!    do i = 1, n-1
!!       write(*, '(1x, ''Line'',i3,'' was: '', a)') i, buf(i)
!!    enddo
!!
!!    end program demo_getstring
!>
!!##NAME
!!    locator(3f) - [M_draw:INTERACTIVE] Find out where cursor is
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          integer function locator(xaddr, yaddr)
!!          real xaddr, yaddr
!!##DESCRIPTION
!!
!!    Find out where the cursor is. XADDR and YADDR are set to the current
!!    location in world coordinates. The function returns a bit pattern
!!    which indicates which buttons are being held down -- eg. if mouse
!!    buttons 1 and 3 are down locator returns binary 101 (decimal 7). The
!!    function returns -1 if the device has no locator capability. Note:
!!    if doing 3-D transformations XADDR and YADDR may not make a lot of
!!    sense. In that case use slocator.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!       program demo_locator !     track a cube with the locator
!!       use M_draw
!!       implicit none
!!       real :: trans, sc, tdir, scal, x, y
!!       integer :: idum, nplanes
!!       parameter(TRANS=20.0, SC=0.1)
!!       integer, parameter :: FACE=1, FILLED=2, OUTLINE=3
!!       character(len=10)  :: device
!!       character(len=1)   :: but
!!       logical            :: back, fill, hatch
!!       integer            :: ios
!!          write(*,*)'x,y,z = translate'
!!          write(*,*)'s     = apply scale'
!!          write(*,*)'+,-   = change scale/translate direction'
!!          write(*,*)'f,h   = fill, hatch'
!!          write(*,*)'b     = toggle backface'
!!          write(*,*)'q     = quit'
!!
!!          print*, 'Enter output device:'
!!          read(*, '(a)',iostat=ios) device
!!          if(ios.ne.0) device=' '
!!
!!          call prefposition(50, 50)
!!          call prefsize(500, 500)
!!
!!          call vinit(device)
!!
!!          call window(-800.0, 800.0, -800.0, 800.0, -800.0, 800.0)
!!          call lookat(0.0, 0.0, 1500.0, 0.0, 0.0, 0.0, 0.0)
!!
!!          tdir = TRANS
!!          scal = SC
!!
!!          !
!!          ! Start with a very ordinary filled cube like in the original demo...
!!          !
!!          call polyhatch(.false.)
!!          call hatchang(45.0)
!!          call hatchpitch(40.0)
!!          call polyfill(.true.)
!!
!!          fill = .true.
!!          hatch = .false.
!!          back = .true.
!!
!!          call makeobj(FACE)
!!          call makepoly()
!!          call rect(-200.0, -200.0, 200.0, 200.0)
!!          call closepoly()
!!          call closeobj()
!!
!!          call makecube(FILLED)
!!
!!          nplanes = getdepth()
!!          if (nplanes .eq. 1) call makecube(OUTLINE)
!!
!!          call backface(back)
!!          !
!!          ! Setup drawing into the backbuffer....
!!          !
!!          if (backbuffer().lt.0) then
!!             call vexit()
!!             write(*,*)'Device can''t support doublebuffering'
!!             stop
!!          endif
!!
!!          INFINITE: do
!!             idum = slocator(x, y)
!!             call pushmatrix()
!!             call rotate(100.0 * x, 'y')
!!             call rotate(100.0 * y, 'x')
!!             call color(D_BLACK)
!!             call clear()
!!             call callobj(FILLED)
!!             if (nplanes .eq. 1 .and. (fill .or. hatch)) call callobj(OUTLINE)
!!             call popmatrix()
!!             call swapbuffers()
!!
!!             but = char(checkkey())
!!             select case(but)
!!              case('x')
!!                call translate(tdir, 0.0, 0.0)
!!              case('y')
!!                call translate(0.0, tdir, 0.0)
!!              case('z')
!!                call translate(0.0, 0.0, tdir)
!!              case('s')
!!                call scale(scal, scal, scal)
!!              case('f')
!!                fill = .not. fill
!!                hatch = .false.
!!                call polyfill(fill)
!!              case('h')
!!                hatch = .not. hatch
!!                fill = .false.
!!                call polyhatch(hatch)
!!              case('b')
!!                back = .not. back
!!                call backface(back)
!!              case('-')
!!                tdir = -tdir
!!                if (scal .lt. 1.0) then
!!                   scal = 1.0 + SC
!!                else
!!                   scal = 1.0 - SC
!!                endif
!!              case('+')
!!                tdir = TRANS
!!              case('q',char(27))
!!                call vexit()
!!                stop
!!             end select
!!          enddo INFINITE
!!       contains
!!
!!       subroutine makecube(obj)
!!       integer obj
!!
!!          call makeobj(obj)
!!          if (obj .eq. OUTLINE) then
!!             call pushattributes()
!!             call color(D_BLACK)
!!             call polyfill(.false.)
!!             call polyhatch(.false.)
!!          endif
!!
!!          call pushmatrix()
!!          call translate(0.0, 0.0, 200.0)
!!          if (obj .eq. FILLED) call color(D_RED)
!!          call callobj(FACE)
!!          call popmatrix()
!!
!!          call pushmatrix()
!!          call translate(200.0, 0.0, 0.0)
!!          call rotate(90.0, 'y')
!!          if (obj .eq. FILLED) call color(D_GREEN)
!!          call callobj(FACE)
!!          call popmatrix()
!!
!!          call pushmatrix()
!!          call translate(0.0, 0.0, -200.0)
!!          call rotate(180.0, 'y')
!!          if (obj .eq. FILLED) call color(D_BLUE)
!!          call callobj(FACE)
!!          call popmatrix()
!!
!!          call pushmatrix()
!!          call translate(-200.0, 0.0, 0.0)
!!          call rotate(-90.0, 'y')
!!          if (obj .eq. FILLED) call color(D_CYAN)
!!          call callobj(FACE)
!!          call popmatrix()
!!
!!          call pushmatrix()
!!          call translate(0.0, 200.0, 0.0)
!!          call rotate(-90.0, 'x')
!!          if (obj .eq. FILLED) call color(D_MAGENTA)
!!          call callobj(FACE)
!!          call popmatrix()
!!
!!          call pushmatrix()
!!          call translate(0.0, -200.0, 0.0)
!!          call rotate(90.0, 'x')
!!          if (obj .eq. FILLED) call color(D_YELLOW)
!!          call callobj(FACE)
!!          call popmatrix()
!!
!!          if (obj .eq. OUTLINE) call popattributes()
!!
!!          call closeobj()
!!
!!       end subroutine makecube
!!
!!       end program demo_locator
!>
!!##NAME
!!    slocator(3f) - [M_draw:INTERACTIVE] Find out where cursor is in screen coordinates
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          integer function slocator(xaddr, yaddr)
!!          real xaddr, yaddr
!!##DESCRIPTION
!!
!!    Find out where the cursor is. xaddr and yaddr are set to the current
!!    location in screen coordinates. The return value of the function is
!!    set up in the same way as with locator. If the device has no locator
!!    device slocator returns -1.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     !
!!     !      a routine to demonstrate using locator.
!!     !
!!     program demo_slocator
!!
!!     use M_draw
!!    implicit none
!!
!!     character(len=20) :: dev
!!     integer bt
!!     real x, y, sx, sy
!!     logical act, curpnt
!!     integer :: ios
!!
!!     print*,'Enter device name:'
!!     read(*,'(a)',iostat=ios) dev
!!     if(ios.ne.0)dev=' '
!!     call vinit(dev)
!!
!!     call color(D_BLACK)
!!     call clear()
!!
!!     call color(D_BLUE)
!!     !
!!     !       draw some axes
!!     !
!!     call move2(0.0, 1.0)
!!     call draw2(0.0, -1.0)
!!
!!     call move2(1.0, 0.0)
!!     call draw2(-1.0, 0.0)
!!
!!     call color(D_GREEN)
!!
!!     act = .false.
!!     curpnt = .false.
!!     !
!!     !       locator returns whether a mouse button has been
!!     !       pressed or not. In a device such as the tektronix
!!     !       where you have to wait for a keypress to get the
!!     !       position of the crosshairs locator returns 0
!!     !       automatically on every second call. A return value
!!     !       of 2 indicates the second mouse button has been pressed.
!!     !       A return value of 1 indicates the first mouse button has
!!     !       been pressed. We wait for the locator to return zero so
!!     !       that we know the mouse button has been released.
!!     !
!!     write(*,*)' click two points to create a line segment'
!!     write(*,*)' button 2 exits'
!!
!!     INFINITE: do
!!        bt = slocator(x, y)
!!        !! write(*,*)'slocator returned ',bt,' and coordinates ',x,y
!!        if (bt .eq. -1) then
!!           call vexit()
!!           print*,'No locator device found'
!!           stop
!!        elseif (bt .eq. 2) then
!!           call vexit()
!!           stop
!!        elseif (bt .eq. 0) then
!!           act = .true.
!!        elseif (act) then
!!           act = .false.
!!           if (bt .eq. 1) then
!!              if (curpnt) then
!!                 call move2(sx, sy)
!!                 call draw2(x, y)
!!                 curpnt = .false.
!!              else
!!                 curpnt = .true.
!!              endif
!!
!!              sx = x
!!              sy = y
!!           endif
!!        endif
!!     enddo INFINITE
!!
!!     end program demo_slocator
!>
!!##NAME
!!    vsetflush(3f) - [M_draw:FLUSHING] Set global flushing status
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine vsetflush(yesno)
!!          logical yesno
!!
!!##DESCRIPTION
!!    Set global flushing status. If yesno = 0 (.false.) then don't do any
!!    flushing (except in swapbuffers(), or vflush()). If yesno = 1
!!    (.true.) then do the flushing as described above.
!>
!!##NAME
!!    vflush(3f) - [M_draw:FLUSHING] Call device flush or syncronisation routine
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine vflush()
!!
!!##DESCRIPTION
!!
!!    Call the device flush or syncronisation routine. This forces a flush.
!>
!!##NAME
!!    viewport(3f) - [M_draw:VIEWPORT] Specify which part of screen to draw in
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine viewport(left, right, bottom, top)
!!          real,intent(in) :: left, right, bottom, top
!!##DESCRIPTION
!!    Specify which part of the screen to draw in. Left, right, bottom,
!!    and top are real values in screen coordinates (0:n,0:m).
!!
!!    If a device has been declared to be 600 x 400
!!
!!         o-----> X                         (right=600,top=0)
!!         | #------------------------------------#
!!         | |                                    |
!!         | |                                    |
!!         V |                                    |
!!         Y |                                    |
!!           #------------------------------------#
!!      (left=0,bottom=400)
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_viewport
!!    !
!!    ! using non-square viewports,
!!    ! the associated distortion -- and how to fix it
!!    !
!!    use M_draw
!!    implicit none
!!
!!    character(len=50)  :: device
!!    character(len=120) :: buf
!!    real    xfact, yfact
!!    integer :: ios, idum
!!
!!    print*,'Enter output device:'
!!    read(*,'(a)',iostat=ios)device
!!    if(ios.ne.0)device=' '
!!
!!    call vinit(device)
!!
!!    call color(D_BLACK)
!!    call clear()
!!    !
!!    ! Make the viewport the same size as the screen/window.
!!    !
!!    call getfactors(xfact, yfact)
!!    call viewport(-1.0, xfact, -1.0, yfact)
!!    !
!!    ! Draw a square. (Looks like a rectangle, if the viewport
!!    ! wasn't "accidentally" square)
!!    !
!!    call color(D_RED)
!!    call rect(-0.5, -0.5, 0.5, 0.5)
!!    !
!!    ! Tell them what it is.
!!    !
!!    call move2(-1.0, 0.9)
!!    write(buf,&
!!    & '(''Distorted square (viewport(-1, '', F7.3, '', -1, '', F7.3, ''))'')')&
!!    & xfact, yfact
!!    call drawstr(buf)
!!
!!    idum=getkey()
!!    !
!!    ! Fix up the distortion (The actual formula to fix
!!    ! the distortion is (viewport.xmax * (1 + xfact) / 2.0),
!!    ! and similar for the y axis.
!!    !
!!    call ortho2(-1.0, xfact, -1.0, yfact)
!!    !
!!    ! Draw another square (Really is square this time)
!!    !
!!    call color(D_YELLOW)
!!    call rect(-0.5, -0.5, 0.5, 0.5)
!!    !
!!    ! Tell them what it is.
!!    !
!!    call move2(-1.0, -0.9)
!!    write(buf,&
!!    & '(''Fixed up square with ortho2(-1, '', F7.3, '', -1, '', F7.3, '')'')')&
!!    & xfact, yfact
!!    call drawstr(buf)
!!
!!    idum=getkey()
!!    !
!!    ! Do it with world coords going from 0 - 5, 0 - 5.
!!    ! Reset square viewport.
!!    !
!!    call color(D_BLACK)
!!    call clear()
!!
!!    call viewport(-1.0, 1.0, -1.0, 1.0)
!!    call ortho2(0.0, 5.0, 0.0, 5.0)
!!    call textsize(0.1, 0.1)
!!    !
!!    ! Square from 1 to 3. (Really is square)
!!    !
!!    call color(D_GREEN)
!!    call rect(1.0, 1.0, 3.0, 3.0)
!!
!!    call move2(0.0, 4.5)
!!    call drawstr('Square from 0 - 3, 0 - 3')
!!
!!    idum=getkey()
!!    !
!!    ! Distort it with a non-square viewport.
!!    !
!!    call viewport(-1.0, xfact, -1.0, yfact)
!!
!!    call color(D_BLUE)
!!    call rect(1.0, 1.0, 3.0, 3.0)
!!
!!    call move2(0.0, 0.5)
!!    call drawstr('Distorted square from 0 - 3, 0 - 3')
!!
!!    idum=getkey()
!!    !
!!    ! Fix the distortion.
!!    !
!!    call ortho2(0.0, 5.0 * (1.0 + xfact) / 2.0, 0.0, 5.0 * (1.0 + yfact) / 2.0)
!!
!!    call color(D_MAGENTA)
!!    call rect(1.0, 1.0, 3.0, 3.0)
!!
!!    call move2(0.0, 2.5)
!!    call drawstr('Fixed up  square from 0 - 3, 0 - 3')
!!
!!    idum=getkey()
!!
!!    call vexit()
!!
!!    end program demo_viewport
!>
!!##NAME
!!    pushviewport(3f) - [M_draw:VIEWPORT] Save current viewport
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine pushviewport()
!!##DESCRIPTION
!!
!!    Save current viewport.
!>
!!##NAME
!!    popviewport(3f) - [M_draw:VIEWPORT] Retrieve last viewport
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine popviewport()
!!
!!##DESCRIPTION
!!
!!    Retrieve last viewport.
!>
!!##NAME
!!    getviewport(3f) - [M_draw:VIEWPORT] Returns limits of current viewport in screen coordinates
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine getviewport(left, right, bottom, top)
!!          real,intent(out) :: left
!!          real,intent(out) :: right
!!          real,intent(out) :: bottom
!!          real,intent(out) :: top
!!
!!##DESCRIPTION
!!
!! Returns the left, right, bottom and top limits of the current viewport
!! in screen coordinates (-1.0 to 1.0).
!!
!!    If a device has been declared to be real :: array(600,400)
!!
!!         o-----> X                         (right=600,top=0)
!!         | #------------------------------------#
!!         | |                                    |
!!         | |                                    |
!!         V |                                    |
!!         Y |                                    |
!!           #------------------------------------#
!!      (left=0,bottom=400)
!!
!!##OPTIONS
!!    LEFT     value for left side
!!    RIGHT    value for right side
!!    BOTTOM   value for bottom side
!!    TOP      value for top side
!>
!!##NAME
!!    expandviewport(3f) - [M_draw:VIEWPORT] use the entire device viewport
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine expandviewport()
!!
!!##DESCRIPTION
!!
!!    When M_draw does viewport calculations, it will normally begin by using
!!    the largest square it can fit onto the actual display device. This
!!    call says to use the whole device... however you must then take
!!    into account any distortion that will occur due to the non-square
!!    mapping. Thus, a viewport of (-1.0, 1.0, -1.0, 1.0) will map into
!!    the whole display device.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_expandviewport
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    integer :: ipaws
!!
!!    !! set up graphics area
!!    call prefsize(1000,200)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!
!!    !! draw box that fills default world coordinate window
!!    call color(D_RED)
!!    call polyfill(.true.)
!!    call rect(-1.0, -1.0, 1.0, 1.0)
!!    call color(D_GREEN)
!!    call circle(0.0,0.0,1.0)
!!
!!    call expandviewport()
!!    !! the meaning of viewport numbers has now changed, but the
!!    !! viewport itself has not. Now <-1,-1> <1,1> defines the
!!    !! entire display area, where before it defined the largest square
!!    !! that would fit on the display
!!    call viewport(-1.0,1.0,-1.0,1.0)
!!    !! draw box that fills default world coordinate window again
!!
!!    !! instead of a square and circle, the mapping now
!!    !! produces an ellipse and rectangle unless this
!!    !! device has a square display
!!    call polyhatch(.true.)
!!    call hatchpitch(0.1)
!!    call hatchang(30.0)
!!    call linewidth(20)
!!    call color(D_CYAN)
!!    call rect(-1.0, -1.0, 1.0, 1.0)
!!    call color(D_YELLOW)
!!    call hatchang(120.0)
!!    call circle(0.0,0.0,1.0)
!!
!!    !! border
!!    call linewidth(100)
!!    call color(D_BLUE)
!!    call move2(-1.0, -1.0)
!!    call draw2(-1.0, 1.0)
!!    call draw2(1.0, 1.0)
!!    call draw2(1.0, -1.0)
!!    call draw2(-1.0, -1.0)
!!
!!    call vflush(); ipaws=getkey() !! pause
!!    call vexit()                  !! wrap up graphics
!!
!!    end program demo_expandviewport
!>
!!##NAME
!!    unexpandviewport(3f) - [M_draw:VIEWPORT] undo expandviewport(3f)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine unexpandviewport()
!!##DESCRIPTION
!!
!!    Does the reverse of expandviewport. Basically, it returns M_draw to
!!    using the largest square of the device for its viewport calculations.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_unexpandviewport
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    integer :: ipaws
!!
!!    !! set up graphics area
!!    call prefsize(1000,200)
!!    call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!
!!    !! draw circle that fills default world coordinate window
!!    call polyfill(.true.)
!!    call color(D_GREEN)
!!    call circle(0.0,0.0,1.0)
!!    ipaws=getkey() !! pause
!!
!!    !! set new scales for viewport so <-1,-1> and <1,1> are at
!!    !! corners of display instead of corners of largest square
!!    !! that can fit on display
!!    call expandviewport()
!!    call viewport(-1.0,1.0,-1.0,1.0)
!!
!!    !! draw circle that fills default world coordinate window again
!!    !! instead of a circle, the mapping now produces an ellipse unless
!!    !! this device has a square display
!!    call polyhatch(.true.)
!!    call hatchpitch(0.1)
!!    call hatchang(30.0)
!!    call linewidth(40)
!!    call color(D_CYAN)
!!    call circle(0.0,0.0,1.0)
!!    ipaws=getkey() !! pause
!!
!!    !! set new scales for viewport so <-1,-1> and <1,1> are at
!!    !! corners of largest square that fits on display
!!    call unexpandviewport()
!!    !! actually change to the new viewport
!!    call viewport(-1.0,1.0,-1.0,1.0)
!!
!!    !! now the same circle should draw where the original one did
!!    call color(D_BLACK)
!!    call hatchang(120.0)
!!    call linewidth(40)
!!    call circle(0.0,0.0,1.0)
!!    ipaws=getkey() !! pause
!!
!!    call vexit()                  !! wrap up graphics
!!
!!    end program demo_unexpandviewport
!>
!!##NAME
!!    getaspect(3f) - [M_draw:ASPECT] Returns the ratio height over width of the display device.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          real function getaspect()
!!
!!##DESCRIPTION
!!    Returns the ratio height over width of the display device.
!>
!!##NAME
!!    getfactors(3f) - [M_draw:ASPECT] Returns width over min(width of device, height of device) and height over min(width of device, height of device).
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine getfactors(w, h)
!!          real w, h
!!##DESCRIPTION
!!    Returns wfact as the width over min(width of device, height of device)
!!    and hfact as the height over min(width of device, height of
!!    device).
!>
!!##NAME
!!    getdisplaysize(3f) - [M_draw:ASPECT] Returns width and height of device in device units
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine getdisplaysize(w, h)
!!          real,intent(in) :: w, h
!!
!!##DESCRIPTION
!!    Returns the width and height of the device in device units in w and h
!!    respectively.
!>
!!##NAME
!!    pushattributes(3f) - [M_draw:ATTRIBUTE_STACK] Save the current attributes on the attribute stack.
!!    (LICENSE:PD)
!!
!!
!!##SYNOPSIS
!!
!!          subroutine pushattributes()
!!
!!##DESCRIPTION
!!
!!    Save the current attributes on the attribute stack.
!>
!!##NAME
!!    popattributes(3f) - [M_draw:ATTRIBUTE_STACK] Restore attributes to what they were at last pushattributes().
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine popattributes()
!!
!!##DESCRIPTION
!!
!!    Restore the attributes to what they were at the last pushattributes().
!>
!!##NAME
!!    ortho(3f) - [M_draw:PROJECTION] Define x,y,z clipping planes.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine ortho(left, right, bottom, top, near_d, far_d)
!!          real left, right, bottom, top, near_d, far_d
!!##DESCRIPTION
!!
!!    Define x (left, right), y (bottom, top), and z (near, far) clipping
!!    planes. The near and far clipping planes are actually specified
!!    as distances along the line of sight. These distances can also be
!!    negative. The actual location of the clipping planes is z = -near_d
!!    and z = -far_d.
!>
!!##NAME
!!    ortho2(3f) - [M_draw:PROJECTION] define the area of the virtual world coordinates to map to the viewport
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine ortho2(left, right, bottom, top)
!!          real,intent(in) :: left, right, bottom, top
!!##DESCRIPTION
!!
!!    Defines the section of the virtual world coordinates to map to
!!    the viewport. That is, Define x (left, right), and y (bottom, top)
!!    clipping planes.
!!
!!    All the projection routines define a new transformation
!!    matrix, and consequently the world units. Parallel projections are
!!    defined by ortho2.
!>
!!##NAME
!!    perspective(3f) - [M_draw:PROJECTION] Specify perspective viewing pyramid
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine perspective(fov, aspect, near, far)
!!          real fov, aspect, near, far
!!##DESCRIPTION
!!    Specify a perspective viewing pyramid in world coordinates by giving
!!    a field of view, aspect ratio and the distance from the eye of the
!!    near and far clipping plane.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_perspective
!!    !
!!    !     Shows various combinations of viewing and projection transformations
!!    use M_draw
!!    implicit none
!!    !
!!    character(len=50) :: device
!!    integer :: ios, idum
!!    !
!!       print*,'Enter output device:'
!!       read(*,'(a)',iostat=ios)device
!!       if(ios.ne.0)device=' '
!!    !
!!       call vinit(device)
!!    !
!!       call color(D_BLACK)
!!       call clear()
!!    !
!!    ! we want to draw just within the boundaries of the screen
!!    !
!!       call viewport(-0.9, 0.9, -0.9, 0.9)
!!    !
!!    ! set the world size
!!    !
!!       call ortho2(-5.0, 5.0, -5.0, 5.0)
!!    !
!!    ! draw a boundary frame
!!    !
!!       call color(D_RED)
!!       call rect(-5.0, -5.0, 5.0, 5.0)
!!    !
!!    ! set up a perspective projection with a field of view of
!!    ! 40.0 degrees, aspect ratio of 1.0, near clipping plane 0.1,
!!    ! and the far clipping plane at 1000.0.
!!    !
!!       call perspective(40.0, 1.0, 0.1, 1000.0)
!!    !
!!    ! we want the drawing to be done with our eye point at (5.0, 8.0, 5.0)
!!    ! looking towards (0.0, 0.0, 0.0). The last parameter gives a twist
!!    ! in degrees around the line of sight, in this case zero.
!!    !
!!       call lookat(5.0, 8.0, 5.0, 0.0, 0.0, 0.0, 0.0)
!!    !
!!       call drawtetra()
!!    !
!!    ! set the text size
!!    !
!!       call textsize(0.6, 0.9)
!!    !
!!       call move2(-4.5, -4.5)
!!       call drawstr('perspective/lookat')
!!    !
!!       idum=getkey()
!!    !
!!    ! window can also be used to give a perspective projection. Its
!!    ! arguments are 6 clipping planes, left, right, bottom, top, near,
!!    ! and far.
!!    !
!!       call window(-5.0, 5.0, -5.0, 5.0, -5.0, 5.0)
!!    !
!!    ! as window replaces the current transformation matrix we must
!!    ! specify our viewpoint again.
!!    !
!!       call lookat(5.0, 8.0, 5.0, 0.0, 0.0, 0.0, 0.0)
!!    !
!!       call color(D_BLACK)
!!       call clear()
!!    !
!!       call color(D_GREEN)
!!       call rect(-5.0, -5.0, 5.0, 5.0)
!!    !
!!       call drawtetra()
!!    !
!!       call textsize(0.6, 0.9)
!!       call move2(-4.5,-4.5)
!!       call drawstr('window/lookat')
!!    !
!!       idum=getkey()
!!    !
!!    ! set up our original perspective projection again.
!!    !
!!       call perspective(40.0, 1.0, 0.1, 1000.0)
!!    !
!!    ! polarview also specifies our viewpoint, but, unlike lookat, in polar
!!    ! coordinates. Its arguments are the distance from the world origin, an
!!    ! azimuthal angle in the x-y plane measured from the y axis, an
!!    ! incidence angle in the y-z plane measured from the z axis, and a
!!    ! twist around the line of sight.
!!    !
!!       call polarview(15.0, 30.0, 30.0, 30.0)
!!    !
!!       call color(D_BLACK)
!!       call clear()
!!    !
!!       call color(D_MAGENTA)
!!       call rect(-5.0, -5.0, 5.0, 5.0)
!!    !
!!       call drawtetra()
!!    !
!!       call move2(-4.5,-4.5)
!!       call textsize(0.6, 0.9)
!!       call drawstr('perspective/polarview')
!!    !
!!       idum=getkey()
!!    !
!!    ! once more with window for comparison
!!    !
!!       call window(-4.0, 4.0, -4.0, 4.0, -4.0, 4.0)
!!       call polarview(6.0, 20.0, -30.0, 70.0)
!!    !
!!       call color(D_BLACK)
!!       call clear()
!!    !
!!       call color(D_YELLOW)
!!       call rect(-5.0, -5.0, 5.0, 5.0)
!!    !
!!       call drawtetra()
!!    !
!!       call move2(-4.5,-4.5)
!!       call textsize(0.6, 0.9)
!!       call drawstr('window/polarview')
!!    !
!!       idum=getkey()
!!    !
!!       call vexit()
!!    !
!!    contains
!!    !
!!    subroutine drawtetra()
!!    !
!!    ! generate a tetrahedron as a series of move draws
!!    !
!!       call move(-0.5,  0.866, -0.5)
!!       call draw(-0.5, -0.866, -0.5)
!!       call draw( 1.0,  0.0,   -0.5)
!!       call draw(-0.5,  0.866, -0.5)
!!       call draw( 0.0,  0.0,    1.5)
!!       call draw(-0.5, -0.866, -0.5)
!!       call move( 1.0,  0.0,   -0.5)
!!       call draw( 0.0,  0.0,    1.5)
!!    !
!!    !    Label the vertices.
!!    !
!!       call color(D_WHITE)
!!       call textsize(0.3, 0.5)
!!       call move(-0.5,  0.866, -0.5)
!!       call drawchar('a')
!!       call move(-0.5, -0.866, -0.5)
!!       call drawchar('b')
!!       call move( 1.0,  0.0,   -0.5)
!!       call drawchar('c')
!!       call move( 0.0,  0.0,    1.5)
!!       call drawchar('d')
!!    !
!!    end subroutine drawtetra
!!    !
!!    end program demo_perspective
!>
!!##NAME
!!    window(3f) - [M_draw:PROJECTION] Specify a perspective viewing pyramid
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine window(left, right, bot, top, near, far)
!!          real left, right, bot, top, near, far
!!##DESCRIPTION
!!
!!    Specify a perspective viewing pyramid in world coordinates by giving
!!    the rectangle closest to the eye (ie. at the near clipping plane)
!!    and the distances to the near and far clipping planes.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_window
!!    use M_draw
!!    implicit none
!!    integer :: idum, ios
!!    integer,parameter :: &
!!      CUBE=1,       &
!!      TOPLEFT=2,    &
!!      TOPRIGHT=3,   &
!!      BOTTOMLEFT=4, &
!!      BOTTOMRIGHT=5
!!    character(len=20) :: device
!!    print*,'Enter device name:'
!!    read(*,'(A)',iostat=ios)device
!!    if(ios.ne.0)device=' '
!!    call vinit(device)
!!    call pushviewport
!!    call textsize(0.5, 0.9)
!!    call font('futura.m')
!!    call color(D_BLACK)
!!    call clear
!!    ! make an object that represents the cube
!!    call makecube
!!    ! set up an object which draws in the top left of the screen.
!!    call makeobj(TOPLEFT)
!!    call viewport(-1.0, 0.0, 0.0, 1.0)
!!    call ortho2(-5.0, 5.0, -5.0, 5.0)
!!    call color(D_RED)
!!    call rect(-5.0, -5.0, 5.0, 5.0)
!!    call perspective(40.0, 1.0, 0.1, 1000.0)
!!    call lookat(5.0, 8.0, 5.0, 0.0, 0.0, 0.0, 0.0)
!!    ! Call an object within another object
!!    call callobj(CUBE)
!!    call color(D_GREEN)
!!    call move2(-4.5, -4.5)
!!    call drawstr('perspective/lookat')
!!    call closeobj
!!    ! now set up one which draws in the top right of the screen
!!    call makeobj(TOPRIGHT)
!!    call viewport(0.0, 1.0, 0.0, 1.0)
!!    call ortho2(-5.0, 5.0, -5.0, 5.0)
!!    call color(D_GREEN)
!!    call rect(-5.0, -5.0, 5.0, 5.0)
!!    call window(-5.0, 5.0, -5.0, 5.0, -5.0, 5.0)
!!    call lookat(5.0, 8.0, 5.0, 0.0, 0.0, 0.0, 0.0)
!!    call callobj(CUBE)
!!    call color(D_RED)
!!    call move2(-4.5, -4.5)
!!    call drawstr('window/lookat')
!!    call closeobj
!!    ! try the bottom left
!!    call makeobj(BOTTOMLEFT)
!!    call viewport(-1.0, 0.0, -1.0, 0.0)
!!    call ortho2(-5.0, 5.0, -5.0, 5.0)
!!    call color(D_MAGENTA)
!!    call rect(-5.0, -5.0, 5.0, 5.0)
!!    call perspective(40.0, 1.0, 0.1, 1000.0)
!!    call polarview(15.0, 30.0, 30.0, 30.0)
!!    call callobj(CUBE)
!!    call color(D_YELLOW)
!!    call move2(-4.5, -4.5)
!!    call drawstr('perspective/polarview')
!!    call closeobj
!!    ! and the bottom right
!!    call makeobj(BOTTOMRIGHT)
!!    call viewport(0.0, 1.0, -1.0, 0.0)
!!    call ortho2(-5.0, 5.0, -5.0, 5.0)
!!    call color(D_CYAN)
!!    call rect(-5.0, -5.0, 5.0, 5.0)
!!    call window(-5.0, 5.0, -5.0, 5.0, -5.0, 5.0)
!!    call polarview(8.0, -18.0, -3.0, 18.0)
!!    call callobj(CUBE)
!!    call color(D_BLUE)
!!    call move2(-4.5, -4.5)
!!    call drawstr('window/polarview')
!!    call closeobj
!!    ! now draw them
!!    call callobj(TOPLEFT)
!!    call callobj(TOPRIGHT)
!!    call callobj(BOTTOMLEFT)
!!    call callobj(BOTTOMRIGHT)
!!    idum=getkey()
!!    call vexit
!!    !=====================================================================
!!    contains
!!    !=====================================================================
!!    subroutine makecube
!!    ! set up a cube
!!    integer CUBE
!!    parameter (CUBE = 1)
!!    call makeobj(CUBE)
!!    ! The border around the cube
!!    call rect(-5.0, -5.0, 10.0, 10.0)
!!    ! Make the cube from 4 squares
!!    call pushmatrix()
!!    call side()
!!    call rotate(90.0, 'x')
!!    call side()
!!    call rotate(90.0, 'x')
!!    call side()
!!    call rotate(90.0, 'x')
!!    call side()
!!    call popmatrix()
!!    call closeobj()
!!    end subroutine makecube
!!    !=====================================================================
!!    subroutine side
!!    ! define a face for the cube
!!    call pushmatrix
!!    call translate(0.0, 0.0, 1.0)
!!    call rect(-1.0, -1.0, 1.0, 1.0)
!!    call popmatrix
!!    end subroutine side
!!    !=====================================================================
!!    end program demo_window
!>
!!##NAME
!!    pushmatrix(3f) - [M_draw:MATRIX_STACK] Save the current transformation matrix on the matrix stack.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine pushmatrix()
!!
!!##DESCRIPTION
!!
!!    Save the current transformation matrix on the matrix stack.
!>
!!##NAME
!!    popmatrix(3f) - [M_draw:MATRIX_STACK] Reinstall the last matrix pushed
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine popmatrix()
!!
!!##DESCRIPTION
!!
!!    Retrieve the last matrix pushed and make it the current transformation matrix.
!>
!!##NAME
!!    polarview(3f) - [M_draw:VIEWPORT] Specify the viewer's position in polar coordinates
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine polarview(dist, azim, inc, twist)
!!          real dist, azim, inc, twist
!!##DESCRIPTION
!!
!! Specify the viewer's position in polar coordinates by giving the distance
!! from the viewpoint to the world origin, the azimuthal angle in the x-y
!! plane, measured from the y-axis, the incidence angle in the y-z plane,
!! measured from the z-axis, and the twist angle about the line of sight.
!>
!!##NAME
!!    up(3f) - [M_draw:VIEWPORT] Specify the world up.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine up(x, y, z)
!!          real x, y, z
!!
!!##DESCRIPTION
!! Specify the world up. This can be used to prevent lookat's sometimes
!! annoying habit of turning everything upside down due to the line of
!! sight crossing the appropriate axis.
!>
!!##NAME
!!    lookat(3f) - [M_draw:VIEWPORT] Specify the viewer's position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine lookat(vx, vy, vz, px, py, pz, twist)
!!          real vx, vy, vz, px, py, pz, twist
!!
!!##DESCRIPTION
!!    Specify the viewer's position by giving a viewpoint and a reference
!!    point in world coordinates. A twist about the line of sight may also
!!    be given.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!      program demo_lookat
!!      ! Demonstrate a rotating translating tetrahedron, and doublebuffering
!!      use M_draw
!!      implicit none
!!      !
!!      integer TETRAHEDRON
!!      parameter (TETRAHEDRON = 1)
!!      !
!!      real R, tx, tz, rotval, drotval, zeye
!!      integer i
!!      logical back, backdir, fill
!!      character(len=50) :: device
!!      integer :: ios
!!      !
!!      call prefsize(300, 300)
!!      !
!!      print*,'Enter output device:'
!!      read(*,'(a)',iostat=ios)device
!!      if(ios.ne.0)device=' '
!!      !
!!      back = .true.
!!      backdir = .true.
!!      fill = .true.
!!      !
!!      call vinit(device)
!!      !
!!      ! Make the tetrahedral object
!!      !
!!      call maketheobject()
!!      !
!!      rotval = 0.0
!!      drotval = 5.0
!!      zeye = 5.0
!!      !
!!      R = 1.6
!!      !
!!      tx = 0.0
!!      tz = R
!!      !
!!      !all polyfill(fill)
!!      call backface(back)
!!      call backfacedir(backdir)
!!      call clipping(.false.)
!!      !
!!      ! set up a perspective projection with a field of view of
!!      ! 40.0 degrees, aspect ratio of 1.0, near clipping plane 0.1,
!!      ! and the far clipping plane at 1000.0.
!!      !
!!      call perspective(40.0, 1.0, 0.001, 15.0)
!!      call lookat(0.0, 0.0, zeye, 0.0, 0.0, 0.0, 0.0)
!!      !
!!      ! Setup drawing into the backbuffer....
!!      !
!!      if (backbuffer().lt.0) then
!!         call vexit()
!!         write(*,*)'Device can''t support doublebuffering'
!!         stop
!!      endif
!!      !
!!      ! we loop back here ad-nauseam until someone hits a non-interpreted key
!!      !
!!      INFINITE: do
!!      !
!!         rotval = 0.0
!!      !
!!         do i = 0, int(359.0 / drotval)
!!      !
!!            call color(D_BLACK)
!!            call clear()
!!      !
!!      !  Rotate the whole scene...(this accumulates - hence
!!      !  drotval)
!!      !
!!            call rotate(drotval * 0.1, 'x')
!!            call rotate(drotval * 0.1, 'z')
!!      !
!!            call color(D_RED)
!!            call pushmatrix()
!!            call polyfill(.false.)
!!            call rotate(90.0, 'x')
!!            call circle(0.0, 0.0, R)
!!            call polyfill(fill)
!!            call popmatrix()
!!      !
!!            call color(D_BLUE)
!!            call move(0.0, 0.0, 0.0)
!!            call draw(tx, 0.0, tz)
!!      !
!!      ! Remember! The order of the transformations is
!!      ! the reverse of what is specified here in between
!!      ! the pushmatrix and the popmatrix. These ones don't
!!      ! accumulate because of the push and pop.
!!      !
!!            call pushmatrix()
!!               call translate(tx, 0.0, tz)
!!               call rotate(rotval, 'x')
!!               call rotate(rotval, 'y')
!!               call rotate(rotval, 'z')
!!               call scale(0.4, 0.4, 0.4)
!!               call callobj(TETRAHEDRON)
!!            call popmatrix()
!!      !
!!            tz = R * cos(rotval * 3.1415926535 / 180)
!!            tx = R * sin(rotval * 3.1415926535 / 180)
!!      !
!!            call swapbuffers()
!!      !
!!            select case(char(checkkey()))
!!            case('f')
!!                      fill = .not. fill
!!                      call polyfill(fill)
!!            case('b')
!!                      back = .not. back
!!                      call backface(back)
!!            case('d')
!!                      backdir = .not. backdir
!!                      call backfacedir(backdir)
!!            case(char(0))
!!            case default
!!                      call vexit()
!!                      stop
!!             end select
!!      !
!!            rotval = rotval + drotval
!!      !
!!            call microsleep(30000)
!!      !
!!      enddo
!!      !
!!      enddo INFINITE
!!      !
!!      contains
!!      !
!!      ! maketheobject
!!      !
!!      !       generate a tetrahedron object as a series of move draws
!!      !
!!      subroutine maketheobject()
!!
!!      integer TETRAHEDRON, NSIDES, NFACES, NPNTS
!!      parameter (TETRAHEDRON = 1, NSIDES = 3, NFACES = 4, NPNTS = 4)
!!      integer colface(NFACES)
!!      real pnts(3, NPNTS)
!!      integer faces(NSIDES, NFACES)
!!      integer i, j
!!      real x, y, z
!!
!!      data pnts/               &
!!           &  -0.5, 0.866, -0.667,     &
!!           &  -0.5, -0.866, -0.667,    &
!!           &   1.0, 0.0, -0.667,       &
!!           &   0.0, 0.0, 1.334/
!!
!!      data colface/D_GREEN, D_YELLOW, D_CYAN, D_MAGENTA/
!!
!!      data faces/   &
!!           &  3, 2, 1,      &
!!           &  1, 2, 4,      &
!!           &  2, 3, 4,      &
!!           &  3, 1, 4/
!!
!!      call makeobj(TETRAHEDRON)
!!
!!      do i = 1, NFACES
!!         call makepoly()
!!         call color(colface(i))
!!         x = pnts(1, faces(1, i))
!!         y = pnts(2, faces(1, i))
!!         z = pnts(3, faces(1, i))
!!         call move(x, y, z)
!!         do j = 2, NSIDES
!!            x = pnts(1, faces(j,i))
!!            y = pnts(2, faces(j,i))
!!            z = pnts(3, faces(j,i))
!!            call draw(x, y, z)
!!         enddo
!!         call closepoly()
!!       enddo
!!
!!       call closeobj()
!!
!!       end subroutine maketheobject
!!
!!       subroutine microsleep(waittime)
!!       use,intrinsic       :: iso_c_binding, only: c_int
!!       integer,intent(in)  :: waittime
!!       integer(kind=c_int) :: status
!!
!!       interface
!!          function c_usleep(seconds) bind (C,name="usleep")
!!             import
!!             ! should be unsigned int (not available in Fortran).
!!             ! OK until highest bit gets set.
!!             integer(c_int)       :: c_usleep
!!             integer(c_int), intent(in), VALUE :: seconds
!!          end function c_usleep
!!       end interface
!!
!!          if(waittime>0)then
!!             status=c_usleep(int(waittime,kind=c_int))
!!          endif
!!       end subroutine microsleep
!!
!!      end program demo_lookat
!!
!>
!!##NAME
!!    translate(3f) - [M_draw:TRANSFORMATION] Set up a translation.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine translate(x, y, z)
!!       real x, y, z
!!
!!##DESCRIPTION
!!
!!    Set up a translation.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_translate
!!    !
!!    !      a demonstration of objects
!!    !
!!    use M_draw
!!    implicit none
!!
!!    integer SPHERE
!!    real RADIUS
!!    parameter (RADIUS = 10.0)
!!    parameter(SPHERE = 1)
!!    character(len=50) :: device
!!    integer           :: ios, idum
!!
!!    print*,'Enter output device:'
!!    read(*,'(a)',iostat=ios) device
!!    if(ios.ne.0)device=' '
!!
!!    call vinit(device)
!!
!!    call vsetflush(.false.)
!!    !
!!    ! set up our viewing transformation
!!    !
!!    call perspective(90.0, 1.0, 0.001, 500.0)
!!    call lookat(13.0, 13.0, 8.0, 0.0, 0.0, 0.0, 0.0)
!!
!!    call color(D_BLACK)
!!    call clear()
!!    !
!!    ! Call a routine to make the sphere object
!!    !
!!    call makesphere()
!!
!!    !
!!    ! Now draw the sphere object scaled down. We use the pushmatrix
!!    ! and the popmatrix to preserve the transformation matrix so
!!    ! that only this sphere is drawn scaled. The callobj then enables
!!    ! us to draw the sphere we generated with makeobj in makesphere.
!!    !
!!    call color(D_CYAN)
!!
!!    call pushmatrix()
!!    call scale(0.5, 0.5, 0.5)
!!    call callobj(SPHERE)
!!    call popmatrix()
!!    !
!!    ! now we draw the same sphere translated, with a different
!!    ! scale and color.
!!    !
!!    call color(D_WHITE)
!!
!!    call pushmatrix()
!!    call translate(0.0, (-1.4) * RADIUS, 1.4 * RADIUS)
!!    call scale(0.3, 0.3, 0.3)
!!    call callobj(SPHERE)
!!    call popmatrix()
!!    !
!!    ! and maybe a few more times....
!!    !
!!    call color(D_RED)
!!
!!    call pushmatrix()
!!    call translate(0.0, RADIUS, 0.7 * RADIUS)
!!    call scale(0.2, 0.2, 0.2)
!!    call callobj(SPHERE)
!!    call popmatrix()
!!
!!    call color(D_GREEN)
!!
!!    call pushmatrix()
!!    call translate(0.0, 1.5 * RADIUS, -RADIUS)
!!    call scale(0.15, 0.15, 0.15)
!!    call callobj(SPHERE)
!!    call popmatrix()
!!
!!    call color(D_YELLOW)
!!
!!    call pushmatrix()
!!    call translate(0.0, -RADIUS, -RADIUS)
!!    call scale(0.12, 0.12, 0.12)
!!    call callobj(SPHERE)
!!    call popmatrix()
!!
!!    call color(D_BLUE)
!!
!!    call pushmatrix()
!!    call translate(0.0, (-2.0)*RADIUS, -RADIUS)
!!    call scale(0.3, 0.3, 0.3)
!!    call callobj(SPHERE)
!!    call popmatrix()
!!
!!    idum=getkey()
!!
!!    call vexit()
!!    contains
!!    subroutine makesphere
!!    !
!!    !        make a sphere object
!!    !
!!    integer SPHERE
!!    integer ii
!!    integer ia
!!    real i, r, z, a, RADIUS, PI
!!    parameter (PI = 3.1415926535, RADIUS = 10.0, SPHERE = 1)
!!
!!    call makeobj(SPHERE)
!!    !
!!    ! create the latitudinal rings
!!    !
!!    do ii = 0, 180, 20
!!       call pushmatrix()
!!       i=real(ii)
!!       call rotate(i, 'y')
!!       call circle(0.0, 0.0, RADIUS)
!!       call popmatrix()
!!    enddo
!!    !
!!    ! create the longitudinal rings
!!    !
!!    call pushmatrix()
!!    call rotate(90.0, 'x')
!!    do ia = -90, 90, 20
!!       a=ia
!!       r = RADIUS * cos(a * PI / 180.0)
!!       z = RADIUS * sin(a * PI / 180.0)
!!       call pushmatrix()
!!       call translate(0.0, 0.0, -z)
!!       call circle(0.0, 0.0, r)
!!       call popmatrix()
!!    enddo
!!    call popmatrix()
!!
!!    call closeobj()
!!
!!    end subroutine makesphere
!!
!!    end program demo_translate
!>
!!##NAME
!!    scale(3f) - [M_draw:TRANSFORMATION] Set up scaling factors in x, y, and z axis.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine scale(x, y, z)
!!
!!     real,intent(in) ::  x, y, z
!!##DESCRIPTION
!!    Set up scaling factors in x, y, and z axis. The scaling is applied relative
!!    to the current transformation matrix; ie. the scaling is accumulative.
!!
!!##OPTIONS
!!    x  scaling factor to apply in X direction to current transformation matrix
!!    y  scaling factor to apply in Y direction to current transformation matrix
!!    x  scaling factor to apply in Z direction to current transformation matrix
!!##EXAMPLE
!!
!!
!!  Sample program
!!
!!       program demo_scale
!!       use M_draw
!!       implicit none
!!       real :: size, x, y
!!       integer :: idum
!!       ! set up display
!!          call prefsize(300, 300)
!!          call prefposition(200, 10)
!!          call vinit('X11')
!!          SIZE = 1.2
!!          X = -0.75
!!          Y = 0.75
!!          call color(3)
!!          call clear()
!!          call color(2)
!!          call ortho2(-SIZE, SIZE, -SIZE, SIZE)
!!       ! create an object to repeatedly draw
!!          call makeobj(1)
!!            call polyfill(.true.)
!!            call color(1)
!!            call rect(0.0, 0.0, X, Y)
!!            call polyfill(.false.)
!!            call linewidth(200)
!!            call color(2)
!!            call rect(0.0, 0.0, X, Y)
!!          call closeobj()
!!       ! draw object, scaling coordinate system between instantiations
!!          call pushmatrix()
!!            call scale(1.1, 2.0, 0.0)
!!            call callobj(1)
!!            ! scaling accumulates
!!            call scale(0.5, 0.5, 0.0)
!!            call callobj(1)
!!            ! circles appear as ellipses in this coordinate system
!!            call circle(0.0, 0.0, X/3.0)
!!          ! return back to saved coordinate system
!!          call popmatrix()
!!          ! now a circle is a circle again
!!          call color(5)
!!          call circle(0.0, 0.0, X/3.0)
!!          idum = getkey()
!!          call vexit()
!!       end program demo_scale
!!
!!##SEE ALSO
!!     rotate, translate, pushmatrix, popmatrix
!>
!!##NAME
!!    rotate(3f) - [M_draw:TRANSFORMATION] Set up a rotation in axis axis where axis is one of 'x','y', or 'z'.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine rotate(angle, axis)
!!          real angle
!!          character axis
!!##DESCRIPTION
!!
!!    Set up a rotation of coordinate system along specified axis, relative
!!    to current coordinate system.
!!
!!##OPTIONS
!!    angle  Angle in degrees to rotate coordinate system, with clockwise
!!           angles being positive.
!!
!!    axis   Which axis to perform rotation about. Allowed values are 'x',
!!           'y', and 'z'.
!!
!!##EXAMPLE
!!
!!   Sample usage
!!
!!       program demo_rotate
!!       use M_draw
!!       implicit none
!!       real :: x, y, size
!!       integer :: idum
!!
!!       ! set up display
!!          call prefsize(300, 300)
!!          call prefposition(200, 10)
!!          call vinit('X11')
!!
!!          SIZE = 1.2
!!          X = -0.75
!!          Y = 0.75
!!          call ortho2(-SIZE, SIZE, -SIZE, SIZE)
!!          call color(3)
!!          call clear()
!!       ! create an object to repeatedly draw
!!          call makeobj(1)
!!            call polyfill(.true.)
!!            call color(1)
!!            call rect(0.0, 0.0, X, Y)
!!            call polyfill(.false.)
!!            call linewidth(200)
!!            call color(2)
!!            call rect(0.0, 0.0, X, Y)
!!          call closeobj()
!!       ! draw object, rotating coordinate system between instantiations
!!          call callobj(1)
!!          call rotate(45.0, 'z')
!!          call callobj(1)
!!          call rotate(45.0, 'z')
!!          call callobj(1)
!!          call circle(0.0, 0.0, X/3)
!!          idum = getkey()
!!          call vexit()
!!
!!       end program demo_rotate
!!
!!##SEE ALSO
!!     translate, pushmatrix, popmatrix, scale
!>
!!##NAME
!!    patchbasis(3f) - [M_draw:PATCH] Define the t and u basis matrices of a patch.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine patchbasis(tbasis, ubasis)
!!          real :: tbasis(4,4), ubasis(4,4)
!!##DESCRIPTION
!!
!!    Define the t and u basis matrices of a patch.
!>
!!##NAME
!!    patchprecision(3f) - [M_draw:PATCH] Set minimum number of line segments making up curves in a patch.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine patchprecision(tseg, useg)
!!          integer,intent(in) :: tseg, useg
!!
!!##DESCRIPTION
!!    Set the minimum number of line segments making up curves in a patch.
!>
!!##NAME
!!    patchcurves(3f) - [M_draw:PATCH] Set the number of curves making up a patch.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine patchcurves(nt, nu)
!!          integer,intent(in) :: nt, nu
!!
!!##DESCRIPTION
!!
!!    Set the number of curves making up a patch.
!>
!!##NAME
!!    rpatch(3f) - [M_draw:PATCH] Draws a rational patch in the current basis, according to the geometry matrices gx, gy, gz, and gw.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine rpatch(gx, gy, gz, gw)
!!          real,intent(in) :: gx(4,4), gy(4,4), gz(4,4), gw(4,4)
!!##DESCRIPTION
!!
!!    Draws a rational patch in the current basis, according to the geometry
!!    matrices gx, gy, gz, and gw.
!>
!!##NAME
!!    patch(3f) - [M_draw:PATCH] Draws a patch in the current basis, according to the geometry matrices gx, gy, and gz.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine patch(gx, gy, gz)
!!          real,intent(in) :: gx(4,4), gy(4,4), gz(4,4)
!!##DESCRIPTION
!!
!!    Draws a patch in the current basis, according to the geometry matrices
!!    gx, gy, and gz.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_patch
!!    !     Draws patches of various bases
!!    !
!!       use M_draw
!!       implicit none
!!    !
!!       character(len=20) :: dev
!!       integer           :: ios
!!       integer           :: i
!!       integer           :: idum
!!    !
!!    !  patch basis types
!!    !
!!       real bezier(4,4), cardinal(4, 4), bspline(4, 4)
!!       real power(4, 4)
!!       real x1(4, 4), y1(4, 4), z1(4, 4)
!!       real x2(4, 4), y2(4, 4), z2(4, 4)
!!      !
!!       character(len=50) :: labels(4)
!!      !
!!       real    basis(64)
!!       equivalence (basis(1), bezier(1, 1))
!!       equivalence (basis(17), cardinal(1, 1))
!!       equivalence (basis(33), bspline(1, 1))
!!       equivalence (basis(49), power(1, 1))
!!      !
!!       data bezier/                                            &
!!       &          -1.0,   3.0,    -3.0,   1.0,                 &
!!       &          3.0,    -6.0,   3.0,    0.0,                 &
!!       &          -3.0,   3.0,    0.0,    0.0,                 &
!!       &          1.0,    0.0,    0.0,    0.0/
!!      !
!!       data cardinal/                                          &
!!       &          -0.5,   1.5,    -1.5,   0.5,                 &
!!       &          1.0,    -2.5,   2.0,    -0.5,                &
!!       &          -0.5,   0.0,    0.5,    0.0,                 &
!!       &          0.0,    1.0,    0.0,    0.0/
!!      !
!!       data bspline/                                           &
!!       &          -0.166666,     0.5,     -0.5,     0.166666,  &
!!       &           0.5,         -1.0,      0.5,     0.0,       &
!!       &          -0.5,          0.0,      0.5,     0.0,       &
!!       &           0.166666,     0.666666, 0.166666, 0.0 /
!!      !
!!       data power/                                             &
!!       &          1.0, 0.0, 0.0, 0.0,                          &
!!       &          0.0, 1.0, 0.0, 0.0,                          &
!!       &          0.0, 0.0, 1.0, 0.0,                          &
!!       &          0.0, 0.0, 0.0, 1.0/
!!      !
!!       data    x1 /                                            &
!!       &          0.0,   0.2588,   0.5,   0.7071,              &
!!       &          0.0,   0.51764,  1.0,   1.4142,              &
!!       &          0.0,   0.51764,  1.0,   1.4142,              &
!!       &          0.0,   0.2588,   0.5,   0.7071/
!!      !
!!       data    y1 /                                            &
!!       &          1.0,   0.966,   0.866,  0.7071,              &
!!       &          2.0,   1.9318,  1.732,  1.4142,              &
!!       &          2.0,   1.9318,  1.732,  1.4142,              &
!!       &          1.0,   0.966,   0.866,  0.7071/
!!      !
!!       data    z1 /                                            &
!!       &          1.0,   1.0,     1.0,    1.0,                 &
!!       &          1.0,   1.0,     1.0,    1.0,                 &
!!       &          0.0,   0.0,     0.0,    0.0,                 &
!!       &          0.0,   0.0,     0.0,    0.0/
!!      !
!!       data    x2 /                                            &
!!       &          0.7071, 0.8660, 0.9660, 1.0,                 &
!!       &          1.4142, 1.7320, 1.932,  2.0,                 &
!!       &          1.4142, 1.7320, 1.932,  2.0,                 &
!!       &          0.7071, 0.8660, 0.9660, 1.0/
!!      !
!!       data    y2 /                                            &
!!       &          0.7071, 0.5,    0.2588, 0.0,                 &
!!       &          1.4142, 1.0,    0.5176, 0.0,                 &
!!       &          1.4142, 1.0,    0.5176, 0.0,                 &
!!       &          0.7071, 0.5,    0.2588, 0.0/
!!      !
!!       data    z2 /                                            &
!!       &          1.0,   1.0,     1.0,    1.0,                 &
!!       &          1.0,   1.0,     1.0,    1.0,                 &
!!       &          0.0,   0.0,     0.0,    0.0,                 &
!!       &          0.0,   0.0,     0.0,    0.0/
!!      !
!!       data labels /                                           &
!!       &          'Bezier Patch(es)',                          &
!!       &          'Cardinal Patch(es)',                        &
!!       &          'B-Spline Patch(es)',                        &
!!       &          '''Power'' Patch(es)' /
!!    !
!!    !  demonstrate patches
!!    !
!!       write (*,*)'Enter device:'
!!       read(*,'(a)',iostat=ios) dev
!!       if(ios.ne.0)dev=' '
!!    !
!!       call prefsize(1000,1000)
!!       call vinit(dev)
!!    !
!!       call vsetflush(.true.)
!!    !
!!       call color(D_BLACK)
!!       call clear()
!!    !
!!    ! Set up two viewports (They actually overlap)
!!    !
!!       call viewport(-1.0, 0.3, -1.0, 0.3)
!!       call ortho(-2.0, 5.0, -2.0, 5.0, -2.0, 5.0)
!!       call lookat(0.0, 0.0, 0.0, -3.0, 2.0, -4.0, 0.0)
!!    !
!!    !       Save it
!!    !
!!       call pushviewport()
!!       call pushmatrix()
!!    !
!!       call viewport(-0.3, 1.0, -0.3, 1.0)
!!       call ortho(-2.0, 5.0, -2.0, 5.0, -2.0, 5.0)
!!       call lookat(0.0, 0.0, 0.0, 3.0, 2.0, -4.0, 0.0)
!!    !
!!       call textsize(0.4, 0.4)
!!    !
!!    !  patchcurves provides a number of curves in the t and u
!!    !  directions. patchprecision gives the minimum number of line
!!    !  segments making up the curves in the t and u directions. The
!!    !  actual number of linesegments in t or u is equal to the closest
!!    !  integer multiple of the number of curves, > nsegs, in t or u,
!!    !  greater than or equal to the number set by patchprecision in u or
!!    !  t. eg. curves in t will be made up of 21 line segments so that we
!!    !  can match up the 7 curves in u; curves in u will have 24 as 4 by 5
!!    !  gives 20.
!!    !
!!       call patchcurves(4, 7)
!!       call patchprecision(20, 20)
!!    !
!!       do 10 i = 0, 3
!!    !
!!          call axes()
!!    !
!!    !     patchbasis sets the basis matrices for the t and u
!!    !     functions
!!    !
!!    !
!!          call patchbasis(basis(i*16 + 1), basis(i*16 + 1))
!!    !
!!    !               Draw with viewport 2
!!    !
!!          call move(0.0, 4.0, 0.0)
!!          call drawstr(labels(i+1))
!!    !
!!    !     Now draw the patches according to the geometry matrices in
!!    !     x1, y1, and z1, x2, y2, z2.
!!    !
!!          call drawhull(x1, y1, z1)
!!          call patch(x1, y1, z1)
!!    !
!!          call drawhull(x2, y2, z2)
!!          call patch(x2, y2, z2)
!!    !
!!    !               Now with viewport 1
!!    !
!!          call popviewport()
!!          call popmatrix()
!!    !
!!          call axes()
!!    !
!!          call move(0.0, 4.0, 0.0)
!!          call drawstr(labels(i + 1))
!!    !
!!    !     now draw the patches according to the geometry matrices in
!!    !     x1, y1, and z1, x2, y2, z2.
!!    !
!!          call drawhull(x1, y1, z1)
!!          call patch(x1, y1, z1)
!!    !
!!          call drawhull(x2, y2, z2)
!!          call patch(x2, y2, z2)
!!    !
!!          idum=getkey()
!!    !
!!    !     Save viewport 1 again and reset to viewport 2
!!    !
!!          call pushviewport()
!!          call pushmatrix()
!!    !
!!          call viewport(-0.3, 1.0, -0.3, 1.0)
!!          call ortho(-1.5, 5.0, -1.5, 5.0, -1.5, 5.0)
!!          call lookat(0.0, 0.0, 0.0, 3.0, 2.0, -4.0, 0.0)
!!    !
!!          call color(D_BLACK)
!!          call clear()
!!    10 continue
!!    !
!!       call vexit()
!!    !
!!    contains
!!    !
!!       subroutine drawhull(x, y, z)
!!       implicit none
!!    !
!!       real    x(4,4), y(4,4), z(4,4)
!!       integer :: i
!!       integer :: j
!!    !
!!          call color(D_MAGENTA)
!!          do i = 1,4
!!             call move(x(i,1), y(i,1), z(i,1))
!!             do j = 1,4
!!                call draw(x(i,j), y(i,j), z(i,j))
!!             enddo
!!          enddo
!!    !
!!          do i = 1,4
!!             call move(x(1,i), y(1,i), z(1,i))
!!             do j = 1,4
!!                call draw(x(j,i), y(j,i), z(j,i))
!!             enddo
!!          enddo
!!    !
!!          call color(D_GREEN)
!!       end subroutine drawhull
!!    !
!!       subroutine axes()
!!    !
!!    !       draw the axes
!!    !
!!          call color(D_BLUE)
!!          call move(0.0, 0.0, 0.0)
!!          call draw(4.0, 0.0, 0.0)
!!
!!          call move(0.0, 0.0, 0.0)
!!          call draw(0.0, 4.0, 0.0)
!!
!!          call move(0.0, 0.0, 0.0)
!!          call draw(0.0, 0.0, 4.0)
!!    !
!!       end subroutine axes
!!    !
!!    end program demo_patch
!>
!!##NAME
!!    makeobj(3f) - [M_draw:OBJECT] Commence the object number n.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!         subroutine makeobj(n)
!!         integer n
!!
!!##DESCRIPTION
!!    Commence the object number n.
!>
!!##NAME
!!    closeobj(3f) - [M_draw:OBJECT] Close the current object.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine closeobj()
!!##DESCRIPTION
!!
!!    Close the current object.
!>
!!##NAME
!!    genobj(3f) - [M_draw:OBJECT] Returns a unique object identifier.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         integer function genobj()
!!##DESCRIPTION
!!    Returns a unique object identifier.
!>
!!##NAME
!!    getopenobj(3f) - [M_draw:OBJECT] Return the number of the current object.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         integer function getopenobj()
!!
!!##DESCRIPTION
!!    Return the number of the current object.
!>
!!##NAME
!!    callobj(3f) - [M_draw:OBJECT] Draw object number n.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine callobj(n)
!!         integer n
!!##DESCRIPTION
!!    Draw object number n.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_callobj
!!       use M_draw
!!       implicit none
!!       integer :: ipaws
!!       integer :: ix, iy
!!       real    :: x, y
!!       integer :: icolor
!!
!!       ! set up graphics area
!!       call prefsize(680,680)
!!       call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!       !! Background color
!!       !call color(D_WHITE)
!!       !! Page setup
!!       !call page(0.0,2124.0,0.0,2124.0)
!!       !call color(D_BLACK)
!!
!!       call makeobj(1111)
!!       call circleprecision(300)
!!       call curveprecision(300)
!!
!!       icolor=0
!!       call mapcolor(icolor, 0, 0, 0); icolor=icolor+1
!!       call mapcolor(icolor, 0, 0, 255); icolor=icolor+1
!!       call mapcolor(icolor, 0, 255, 0); icolor=icolor+1
!!       call mapcolor(icolor, 0, 255, 255); icolor=icolor+1
!!       call mapcolor(icolor, 255, 0, 0); icolor=icolor+1
!!       call mapcolor(icolor, 255, 0, 255); icolor=icolor+1
!!       call mapcolor(icolor, 255, 255, 0); icolor=icolor+1
!!       call mapcolor(icolor, 255, 255, 255); icolor=icolor+1
!!       call mapcolor(icolor, 0, 0, 142); icolor=icolor+1
!!       call mapcolor(icolor, 0, 0, 175); icolor=icolor+1
!!       call mapcolor(icolor, 0, 0, 209); icolor=icolor+1
!!       call mapcolor(icolor, 135, 206, 255); icolor=icolor+1
!!       call mapcolor(icolor, 0, 142, 0); icolor=icolor+1
!!       call mapcolor(icolor, 0, 175, 0); icolor=icolor+1
!!       call mapcolor(icolor, 0, 209, 0); icolor=icolor+1
!!       call mapcolor(icolor, 0, 142, 142); icolor=icolor+1
!!       call mapcolor(icolor, 0, 175, 175); icolor=icolor+1
!!       call mapcolor(icolor, 0, 209, 209); icolor=icolor+1
!!       call mapcolor(icolor, 142, 0, 0); icolor=icolor+1
!!       call mapcolor(icolor, 175, 0, 0); icolor=icolor+1
!!       call mapcolor(icolor, 209, 0, 0); icolor=icolor+1
!!       call mapcolor(icolor, 142, 0, 142); icolor=icolor+1
!!       call mapcolor(icolor, 175, 0, 175); icolor=icolor+1
!!       call mapcolor(icolor, 209, 0, 209); icolor=icolor+1
!!       call mapcolor(icolor, 127, 48, 0); icolor=icolor+1
!!       call mapcolor(icolor, 160, 63, 0); icolor=icolor+1
!!       call mapcolor(icolor, 191, 96, 0); icolor=icolor+1
!!       call mapcolor(icolor, 255, 127, 127); icolor=icolor+1
!!       call mapcolor(icolor, 255, 160, 160); icolor=icolor+1
!!       call mapcolor(icolor, 255, 191, 191); icolor=icolor+1
!!       call mapcolor(icolor, 255, 224, 224); icolor=icolor+1
!!       call mapcolor(icolor, 255, 214, 0); icolor=icolor+1
!!       call mapcolor(icolor, 64, 64, 64); icolor=icolor+1
!!       call mapcolor(icolor, 128, 128, 128); icolor=icolor+1
!!       call mapcolor(icolor, 192, 192, 192); icolor=icolor+1
!!       call mapcolor(icolor, 224, 224, 224); icolor=icolor+1
!!       call mapcolor(icolor, 255, 255, 255); icolor=icolor+1
!!
!!       call polyfill(.true.)
!!
!!       ! Lower Pedestal Box
!!       call color(34-1)
!!       call rect(612.0, 537.0, 1512.0, 462.0)
!!       call polyfill(.false.)
!!       call rasters(5)
!!       call color(37-1)
!!       call rect(612.0, 537.0, 1512.0, 462.0)
!!       call polyfill(.true.)
!!       call rasters(1)
!!       ! Keyboard Surface
!!       call color(36-1)
!!       call makepoly()
!!       IX=237
!!       IY=462
!!       X=real(IX)
!!       Y=real(IY)
!!       call move2(X,Y)
!!       call pline([IX,IY,1887,462,2112,87,2112,12,12,12,12,87,IX,IY])
!!
!!       call closepoly()
!!       ! Upper Pedestal Box
!!       call color(34-1)
!!       call rect(687.0, 612.0, 1437.0, 537.0)
!!       call polyfill(.false.)
!!       call rasters(5)
!!       call color(37-1)
!!       call rect(687.0, 612.0, 1437.0, 537.0)
!!       call polyfill(.true.)
!!       call rasters(1)
!!
!!       ! Monitor Box
!!       call color(36-1)
!!       call rect(162.0, 2112.0, 1962.0, 612.0)
!!
!!       ! Main QWERTY area
!!       !edgewidth(0)
!!
!!       call color(37-1)
!!       call makepoly()
!!       call pline([312,387,162,162,1512,162,1437,387,312,387])
!!       call closepoly()
!!
!!       ! Numeric Keypad Area
!!       call makepoly()
!!       call pline([1812,387,1512,387,1587,162,1962,162,1812,387])
!!       call closepoly()
!!
!!       ! Shade Keyboard Front
!!       call color(35-1)
!!       call makepoly()
!!       call pline([12,87,12,12,2112,12,2112,87])
!!
!!       call closepoly()
!!       ! U-Shaped Edge of Keyboard Front for definition
!!       call rasters(5)
!!       call color(1-1)
!!       call pline([12,87,12,12,2112,12,2112,87])
!!
!!       ! Glass Tube/Viewing Panel
!!       call color(33-1)
!!       call rect(387.0, 1962.0, 1737.0, 762.0)
!!       ! Upper Edge of Recess
!!       call color(34-1)
!!       call makepoly()
!!       call pline([ &
!!       & 387,1887,388,1887,391,1887,396,1888,403,1888,413, &
!!       & 1889,427,1890,443,1892,463,1893,484,1895, &
!!       & 508,1896,535,1898,562,1900,592,1902,622,1904,654, &
!!       & 1906,687,1907,722,1909,759,1911,797,1912, &
!!       & 838,1913,881,1915,927,1916,975,1916,1025,1917,1077, &
!!       & 1917,1133,1917,1187,1916,1238,1915,1285,1914, &
!!       & 1330,1913,1371,1911,1410,1910,1446,1908,1481,1906, &
!!       & 1514,1904,1545,1902,1575,1900,1603,1898,1629,1896, &
!!       & 1653,1894,1674,1892,1692,1891,1707,1890,1719,1889, &
!!       & 1727,1888,1733,1887,1736,1887,1737,1887,1738,1888, &
!!       & 1741,1891,1748,1898,1760,1910,1775,1925,1789,1939, &
!!       & 1801,1951,1808,1958,1811,1961,1812,1962,1810,1962, &
!!       & 1807,1962,1800,1962,1789,1962,1773,1962,1752,1962, &
!!       & 1725,1962,1692,1962,1653,1962,1607,1962,1555,1962, &
!!       & 1498,1962,1434,1962,1366,1962,1294,1962,1218,1962, &
!!       & 1141,1962,1062,1962,983,1962,906,1962,830,1962, &
!!       & 758,1962,690,1962,626,1962,569,1962,517,1962,471, &
!!       & 1962,432,1962,399,1962,372,1962,351,1962, &
!!       & 335,1962,324,1962,317,1962,314,1962,312,1962,313, &
!!       & 1961,316,1958,323,1951,335,1939,350,1924, &
!!       & 364,1910,376,1898,383,1891,386,1888,387,1887])
!!       call closepoly()
!!
!!       ! Raised front of QWERTY area
!!       call rect(162.0, 162.0, 1512.0, 135.0)
!!
!!       ! Left Control Keys in QWERTY
!!       call color(35-1)
!!       call makepoly()
!!       call pline([284,387,162,162,387,162,418,237,366, &
!!               & 237,396,312,348,312,387,387,284,387])
!!       call closepoly()
!!
!!       ! Right Control Keys in QWERTY
!!       call makepoly()
!!       call pline([1512,162,1287,162,1287,238,1336,237,1328, &
!!               & 312,1362,312,1373,312,1358,387,1451,387,1512,162])
!!       call closepoly()
!!
!!       ! Numeric Keypad Special Keys -- Just Top?
!!       call makepoly()
!!       call pline([1962,162,1893,162,1812,342,1526,342,1511,387, &
!!               & 1843,387,1962,162])
!!       call closepoly()
!!
!!       ! Raised Front of Numeric Keypad
!!       call color(34-1)
!!       call rect(1587.0,162.0,1962.0,136.0)
!!
!!       ! Left Raised Edge of Numeric Keypad
!!       call makepoly()
!!       call pline([1587,161,1586,134,1510,365,1511,386,1587,161])
!!       call closepoly()
!!
!!       ! Right Edge of Tube Recess
!!       call color(35-1)
!!       call makepoly()
!!       call pline([ &
!!       & 1812,762,1811,763,1808,766,1801,773,1789,785, &
!!       & 1774,800,1760,814,1748,826,1741,833,1738,836, &
!!       & 1737,837,1737,839,1737,843,1737,851,1737,863, &
!!       & 1737,881,1737,905,1737,934,1737,970,1737,1012, &
!!       & 1737,1060,1737,1113,1737,1171,1737,1232,1737,1297, &
!!       & 1737,1362,1737,1427,1737,1492,1737,1553,1737,1611, &
!!       & 1737,1664,1737,1712,1737,1754,1737,1790,1737,1819, &
!!       & 1737,1843,1737,1861,1737,1873,1737,1881,1737,1885, &
!!       & 1737,1887,1738,1888,1741,1891,1748,1898,1760,1910, &
!!       & 1775,1925,1789,1939,1801,1951,1808,1958,1811,1961, &
!!       & 1812,1962,1812,1960,1812,1956,1812,1948,1812,1937, &
!!       & 1812,1919,1812,1897,1812,1868,1812,1833,1812,1791, &
!!       & 1812,1744,1812,1690,1812,1631,1812,1568,1812,1502, &
!!       & 1812,1432,1812,1362,1812,1292,1812,1222,1812,1156, &
!!       & 1812,1093,1812,1034,1812,980,1812,933,1812,891, &
!!       & 1812,856,1812,827,1812,805,1812,787,1812,776, &
!!       & 1812,768,1812,764,1812,762])
!!       call closepoly()
!!
!!       ! Left Edge of Tube Recess
!!       call makepoly()
!!       call pline([ &
!!       & 312,762,313,763,316,766,323,773,335,785, &
!!       & 350,800,364,814,376,826,383,833,386,836, &
!!       & 387,837,387,839,387,843,387,851,387,863, &
!!       & 387,881,387,905,387,934,387,970,387,1012, &
!!       & 387,1060,387,1113,387,1171,387,1232,387,1297, &
!!       & 387,1362,387,1427,387,1492,387,1553,387,1611, &
!!       & 387,1664,387,1712,387,1754,387,1790,387,1819, &
!!       & 387,1843,387,1861,387,1873,387,1881,387,1885, &
!!       & 387,1887,386,1888,383,1891,376,1898,364,1910, &
!!       & 349,1925,335,1939,323,1951,316,1958,313,1961, &
!!       & 312,1962,312,1960,312,1956,312,1948,312,1937, &
!!       & 312,1919,312,1897,312,1868,312,1833,312,1791, &
!!       & 312,1744,312,1690,312,1631,312,1568,312,1502, &
!!       & 312,1432,312,1362,312,1292,312,1222,312,1156, &
!!       & 312,1093,312,1034,312,980,312,933,312,891, &
!!       & 312,856,312,827,312,805,312,787,312,776, &
!!       & 312,768,312,764,312,762])
!!
!!       call closepoly()
!!
!!       ! Bottom Edge of Tube Recess
!!       call color(37-1)
!!       call makepoly()
!!       call pline([ &
!!       & 387,837,388,837,391,837,396,836,403,836, &
!!       & 413,835,427,834,443,832,463,831,484,829, &
!!       & 508,828,535,826,562,824,592,822,622,820, &
!!       & 654,818,687,817,722,815,759,813,797,812, &
!!       & 838,811,881,809,927,808,975,808,1025,807, &
!!       & 1077,807,1133,807,1187,808,1238,809,1285,810, &
!!       & 1330,811,1371,813,1410,814,1446,816,1481, &
!!       & 818,1514,820,1545,822,1575,824,1603,826,1629,828, &
!!       & 1653,830,1674,832,1692,833,1707,834,1719, &
!!       & 835,1727,836,1733,837,1736,837,1737,837,1738,836, &
!!       & 1741,833,1748,826,1760,814,1775,799,1789,785, &
!!       & 1801,773,1808,766,1811,763,1812,762,1810,762, &
!!       & 1807,762,1800,762,1789,762,1773,762,1752,762, &
!!       & 1725,762,1692,762,1653,762,1607,762,1555,762, &
!!       & 1498,762,1434,762,1366,762,1294,762,1218,762, &
!!       & 1141,762,1062,762,983,762,906,762,830,762, &
!!       & 758,762,690,762,626,762,569,762,517,762,471, &
!!       & 762,432,762,399,762,372,762,351,762, &
!!       & 335,762,324,762,317,762,314,762,312,762,313, &
!!       & 763,316,766,323,773,335,785,350,800, &
!!       & 364,814,376,826,383,833,386,836,387,837])
!!       call closepoly()
!!
!!       ! < of X11
!!       call color(8-1)
!!       call makepoly()
!!       call pline([1034,1233,927,1233,743,1463,987, &
!!               & 1769,1034,1769,819,1493,1034,1233])
!!       call closepoly()
!!
!!       call rasters(5)
!!       call pline([1034,1233,927,1233,743,1463,987, &
!!               & 1769,1034,1769,819,1493,1034,1233])
!!       call rasters(1)
!!
!!       ! > of X11
!!       call color(8-1)
!!       call makepoly()
!!       call pline( [483,1769,591,1769,774,1540,530, &
!!               & 1233,483,1233, 698,1509,483,1769])
!!       call closepoly()
!!
!!       call rasters(5)
!!       call pline( [483,1769,591,1769,774,1540,530, &
!!               & 1233,483,1233,698,1509,483,1769])
!!       call rasters(1)
!!
!!       ! End of Picture %
!!
!!       call closeobj()
!!
!!       call makeobj(2222)
!!       call call_obj(1111,xt=-1012.0, yt=-1012.0)
!!       call closeobj()
!!
!!       !!call call_obj(2222)
!!
!!       ! make a big page and call object with various transformations
!!       call color(D_WHITE)
!!       call page( 0.0, 20000.0, 0.0, 20000.0)
!!       call clear()
!!       call color(D_BLACK)
!!
!!       call call_obj(1111, xs=4.0, ys=4.0)
!!       call call_obj(2222, xs=2.0, ys=2.0)
!!       call call_obj(2222, xs=2.0, ys=2.0, xt=10000.0, yt=10000.0)
!!       call call_obj(2222, xs=2.0, ys=2.0, xt=4000.0, yt=10000.0, zr=45.0)
!!       call call_obj(2222, xs=2.0, ys=2.0, yt=3000.0, xt=12000.0)
!!       call call_obj(2222, xs=1.4, ys=2.0, xt=13000.0, yt=16000.0, zr=180.0)
!!       call call_obj(2222, xs=1.0, ys=1.0, xt=16000.0, yt=16000.0, zr=0.0)
!!       call call_obj(2222, xs=4.0, ys=2.0, xt=6000.0, yt=17000.0)
!!       call call_obj(2222, xs=1.0, ys=1.0, xt=16000.0, yt=4000.0, zr=0.0)
!!       call call_obj(2222, xs=1.0, ys=1.0, xt=18000.0, yt=5500.0, zr=-30.0)
!!       call call_obj(2222, xs=1.0, ys=1.0, xt=16000.0, yt=7000.0, zr=30.0)
!!       call call_obj(2222, xs=1.0, ys=1.0, xt=18000.0, yt=9500.0, zr=-50.0)
!!       call call_obj(2222, xs=1.0, ys=1.0, xt=16000.0, yt=11000.0, zr=70.0)
!!
!!       ! this does not work as expected
!!       !call call_obj(2222, xs=2.0,, ys=2.0,, xt=16000.0, yt=13000.0, &
!!       ! & xr=10.0, yr=10.0 )
!!       !call call_obj(1111, xs=2.0,, ys=2.0,, xt=16000.0, yt=13000.0, &
!!       ! & xr=0.10, yr=0.10 )
!!
!!       ! pause
!!       call vflush()
!!       ipaws=getkey()
!!
!!       ! wrap up graphics
!!       call vexit()
!!
!!       contains
!!       subroutine pline(iarr)
!!          integer,intent(in) :: iarr(:)
!!          integer            :: i
!!          ! assuming nice data in x,y pairs
!!          call move2(real(iarr(1)),real(iarr(2)))
!!          do i=3,size(iarr),2
!!             call draw2(real(iarr(i)),real(iarr(i+1)))
!!          enddo
!!       end subroutine pline
!!
!!       subroutine call_obj(iobj,xt,yt,zt,xs,ys,zs,xr,yr,zr)
!!          ! DEFAULT call_obj(iobj,0.0,0.0,0.0, 1.0,1.0,1.0, 0.0,0.0.0.0)
!!          integer,intent(in) :: iobj
!!          real,optional :: xt,yt,zt
!!          real,optional :: xs,ys,zs
!!          real,optional :: xr,yr,zr
!!          real          :: xt_l,yt_l,zt_l
!!          real          :: xs_l,ys_l,zs_l
!!          real          :: xr_l,yr_l,zr_l
!!          if(present(xt))then; xt_l=xt; else; xt_l=0.0; endif
!!          if(present(yt))then; yt_l=yt; else; yt_l=0.0; endif
!!          if(present(zt))then; zt_l=zt; else; zt_l=0.0; endif
!!
!!          if(present(xs))then; xs_l=xs; else; xs_l=1.0; endif
!!          if(present(ys))then; ys_l=ys; else; ys_l=1.0; endif
!!          if(present(zs))then; zs_l=zs; else; zs_l=1.0; endif
!!
!!          if(present(xr))then; xr_l=xr; else; xr_l=0.0; endif
!!          if(present(yr))then; yr_l=yr; else; yr_l=0.0; endif
!!          if(present(zr))then; zr_l=zr; else; zr_l=0.0; endif
!!
!!          ! call an object using specified scaling, translation, and rotation
!!          ! and then restore coordinate space
!!          call pushmatrix()
!!             call translate(xt_l,yt_l,zt_l)
!!             call scale(xs_l,ys_l,zs_l)
!!             call rotate(xr_l,"x")
!!             call rotate(yr_l,"y")
!!             call rotate(zr_l,"z")
!!             call callobj(iobj)
!!          call popmatrix()
!!       end subroutine call_obj
!!
!!       subroutine rasters(iwidth)
!!          integer,intent(in) :: iwidth
!!          call linewidth(iwidth*5)
!!       end subroutine rasters
!!    end program demo_callobj
!>
!!##NAME
!!    isobj(3f) - [M_draw:OBJECT] Returns .FALSE. if there is an object of number n.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         logical function isobj(n)
!!         integer :: n
!!
!!
!!##DESCRIPTION
!!    Returns .FALSE. if there is an object of number n.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_isobj
!!    use M_draw
!!    implicit none
!!    integer :: idum
!!       call prefsize(300, 300)
!!       call prefposition(100, 100)
!!       call vinit(' ')   ! set up device
!!       call ortho2(-5.0,5.0,-5.0,5.0)
!!       call color(D_WHITE)  ! set current color
!!       call clear()   ! clear screen to current color
!!
!!       call makeobj(3)  ! create a simple object
!!          call polyfill(.true.)
!!          call color(D_GREEN)
!!          call circle(0.0,0.0,4.0)
!!       call closeobj()
!!
!!       if(isobj(3))then
!!          write(*,*)' 3 is an object (CORRECT)'
!!          call callobj(3)
!!       else
!!          write(*,*)' 3 is not an object (ERROR)'
!!       endif
!!
!!       if(isobj(4))then
!!          write(*,*)' 4 is an object (ERROR)'
!!       else
!!          write(*,*)' 4 is not an object (CORRECT)'
!!       endif
!!
!!       call callobj(4) ! note: calling a non-existent object is a no-op
!!
!!       idum=getkey()! wait for some input
!!       call vexit()!  set the screen back to its original state
!!
!!    end program demo_isobj
!!
!!   Expected output:
!!
!!      3 is an object (CORRECT)
!!      4 is not an object (CORRECT)
!>
!!##NAME
!!    delobj(3f) - [M_draw:OBJECT] Delete the object number n.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine delobj(n)
!!         integer n
!!##DESCRIPTION
!!    Delete the object number n.
!>
!!##NAME
!!    loadobj(3f) - [M_draw:OBJECT] Load the object in the file filename as object number n.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine loadobj(n, filename)
!!         integer n
!!         character*(*) filename
!!##DESCRIPTION
!!    Load the object in the file "filename" as object number n.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_loadobj
!!    ! test some object-related procedures
!!    use M_draw
!!    implicit none
!!    integer                      :: env_len, idum
!!    character(len=:),allocatable :: env
!!       !------------------------------------------------------------
!!       ! make an object file, which would normally be
!!       ! made by a separate program
!!       call voutput('+')
!!       call vinit('nil')   ! set up device
!!       call makeobj(3)
!!          call polyfill(.true.)
!!          call color(D_GREEN)
!!          call circle(0.0,0.0,4.0)
!!       call closeobj()
!!       call saveobj(3,"circle.obj")
!!       call vexit()
!!       !------------------------------------------------------------
!!       ! CURRENTLY:
!!       ! with multiple vinit(3f) calls the environment variable is
!!       ! not used without explicitly using it
!!       call get_environment_variable('M_DRAW_OUTPUT',LENGTH=env_len)
!!       if(env_len.ne.0)then
!!          allocate(character(len=env_len) :: env)
!!          call get_environment_variable('M_DRAW_OUTPUT',env)
!!          call voutput(env)
!!       endif
!!       !------------------------------------------------------------
!!       call prefsize(300, 300)
!!       call prefposition(100, 100)
!!       call vinit(' ')   ! set up device
!!       call ortho2(-5.1,5.3,-5.2,5.4)
!!       call color(D_WHITE)  ! set current color
!!       call clear()   ! clear screen to current color
!!       call color(D_BLUE)
!!       call move2(-5.0,-5.0)
!!       call draw2(5.0,5.0)
!!       call move2(-5.0,5.0)
!!       call draw2(5.0,-5.0)
!!       call loadobj(3,"circle.obj")
!!       call callobj(3)
!!       call translate(1.0,0.0,0.0)
!!       call translate(0.0,1.0,0.0)
!!       idum=getkey() ! wait for some input
!!       call vexit()  !  set the screen back to its original state
!!       !------------------------------------------------------------
!!    end program demo_loadobj
!>
!!##NAME
!!    saveobj(3f) - [M_draw:OBJECT] Save object number n into file filename. Does NOT save objects called inside object n.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!         subroutine saveobj(n, filename)
!!         integer,intent(in) :: n
!!         character(len=*),intent(in) :: filename
!!
!!##DESCRIPTION
!!    Save the object number n into the file filename. This call does not
!!    save objects called inside object n.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_saveobj
!!    ! create object in file "circle.obj" for use with loadobj(3f)
!!    use M_draw
!!    implicit none
!!    character(len=:),allocatable :: env
!!    integer :: ipaws
!!    integer :: env_len
!!       call voutput('+')            ! ignore $M_DRAW_OUTPUT
!!       call vinit('nil')            ! start graphics
!!       call push()
!!       !
!!       call makeobj(3)              ! create an object
!!          call polyfill(.true.)
!!          call color(D_GREEN)
!!          call circle(0.0,0.0,10.0)
!!          call polyfill(.false.)
!!          call color(D_BLUE)
!!          call linewidth(100)
!!          call circle(0.0,0.0,4.0)
!!       call closeobj()
!!       !
!!       call saveobj(3,"circle.obj") ! save object to file
!!       call pop()
!!       call vexit()                 ! exit graphics
!!       !
!!       !-------  now this could be a separate program to use object
!!       !
!!       !------------------------------------------------------------
!!       ! CURRENTLY:
!!       ! with multiple vinit(3f) calls the environment variable is
!!       ! not used without explicitly using it
!!       call get_environment_variable('M_DRAW_OUTPUT',LENGTH=env_len)
!!       if(env_len.ne.0)then
!!          allocate(character(len=env_len) :: env)
!!          call get_environment_variable('M_DRAW_OUTPUT',env)
!!          call voutput(env)
!!       endif
!!       !------------------------------------------------------------
!!       call vinit(' ')  ! set up device
!!       call page(-100.0,100.0,-100.0,100.0)
!!       call loadobj(100,"circle.obj")
!!       ! translate x,y,z scale x,y,z rotate x,y,z object
!!       call invokeobj(   0.0,  0.0, 0.0,1.0,1.0,1.0,0.0,0.0,0.0, 100)
!!       call invokeobj( -20.0,-20.0, 0.0,1.0,2.0,1.0,0.0,0.0,0.0, 100)
!!       call invokeobj(  30.0, 40.0, 0.0,2.0,2.0,1.0,0.0,0.0,0.0, 100)
!!       ipaws=getkey()
!!       call vexit() ! set the screen back to its original state
!!       !
!!    end program demo_saveobj
!>
!!##NAME
!!    backbuffer(3f) - [M_draw:DOUBLE_BUFFERING] Draw in the backbuffer. Returns -1 if the device is not up to it.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          integer function backbuffer()
!!
!!##DESCRIPTION
!!    Make M_draw draw in the backbuffer. Returns -1 if the device is not up to it.
!>
!!##NAME
!!    frontbuffer(3f) - [M_draw:DOUBLE_BUFFERING] Draw in the front buffer. This will always work.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine frontbuffer()
!!
!!##DESCRIPTION
!!
!!    Make M_draw draw in the front buffer. This will always work.
!>
!!##NAME
!!    swapbuffers(3f) - [M_draw:DOUBLE_BUFFERING] Swap the front and back buffers.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine swapbuffers()
!!
!!##DESCRIPTION
!!    Swap the front and back buffers.
!>
!!##NAME
!!    getgp(3f) - [M_draw:POSITION] Gets the current graphics position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine getgp(x, y, z)
!!          real x, y, z
!!##DESCRIPTION
!!
!!    Gets the current graphics position in world coords.
!>
!!##NAME
!!    getgpt(3f) - [M_draw:POSITION] Gets the current transformed graphics position in world coords.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine getgpt(x, y, z, w)
!!          real x, y, z, w
!!##DESCRIPTION
!!
!!    Gets the current transformed graphics position in world coords.
!>
!!##NAME
!!    getgp2(3f) - [M_draw:POSITION] Gets the current graphics position in world coordinates
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!          subroutine getgp2(x, y)
!!          real,intent(out) :: x,y
!!##DESCRIPTION
!!
!!    Gets the current graphics position in world coords.
!!
!!##RETURNS
!!    X  X coordinate of current position
!!    Y  Y coordinate of current position
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_getgp2
!!      use M_draw
!!      implicit none
!!      real :: X,Y
!!      call prefsize(20,20)
!!      call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
!!      call ortho2(-100.0,100.0,-100.0,100.0)
!!      call move2(0.0,0.0)
!!      call draw2(96.5,98.333)
!!
!!      call getgp2(X,Y)
!!      write(*,*)'CURRENT POSITION (X,Y)=',X,Y
!!
!!      call vexit()
!!      end program demo_getgp2
!!
!!   Results
!!
!!    CURRENT POSITION (X,Y)=   96.5000000       98.3330002
!>
!!##NAME
!!    sgetgp2(3f) - [M_draw:POSITION] Gets the current screen graphics position in screen coords (-1 to 1)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!          subroutine sgetgp2(x, y)
!!          real x, y
!!
!!##DESCRIPTION
!!    Gets the current screen graphics position in screen coords (-1 to 1)
!>
!!##NAME
!!    example_text_justification(7) - [M_draw:EXAMPLE] example program
!!    showing text justification
!!    (LICENSE:PD)
!!
!!##DESCRIPTION
!!    Interactive example program to demonstrate text features such as
!!    justification, size, angle, ...
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_example_text_justification
!!    ! demonstrate still more features of text
!!    use M_draw
!!    implicit none
!!    character(len=20) :: dev
!!    integer           :: ios, idum
!!
!!    write(*,'(a)',advance='no')'Enter device: '
!!    read(*, '(a)',iostat=ios) dev
!!    if(ios.ne.0)dev=' '
!!    call vinit(dev)
!!    call textsize(0.03, 0.04)
!!    call ortho2(0.0, 1.0, 0.0, 1.0)
!!    call color(D_RED)
!!    call clear()
!!
!!    call drawstuff()
!!    !       Now do it all with the text rotated ....
!!    call textang(45.0); call drawstuff()
!!    call textang(160.0); call drawstuff()
!!    call textang(270.0); call drawstuff()
!!    !       Now with a single character
!!    call textjustify(achar(0))
!!
!!    call drawstuff2(0.0)
!!    call drawstuff2(90.0)
!!    call drawstuff2(160.0)
!!    call drawstuff2(270.0)
!!
!!    do
!!       idum=getkey()
!!       select case(idum)
!!       case(:-1)                  ; exit
!!       case(ichar('q'),ichar('Q')); exit
!!       end select
!!    enddo
!!    call vexit()
!!    contains
!!
!!    subroutine drawstuff()
!!    use M_draw
!!    integer :: idum
!!
!!    call color(D_BLACK)
!!    !call polyfill(1)
!!    call polyfill(.true.)
!!    !               So rect clears a bit for us
!!
!!    call rect(0.1, 0.1, 0.9, 0.9)
!!    call color(D_WHITE)
!!    call move2(0.1, 0.5)
!!    call draw2(0.9, 0.5)
!!    call move2(0.5, 0.1)
!!    call draw2(0.5, 0.9)
!!
!!    call color(D_GREEN)
!!    call move2(0.5, 0.5)
!!    call leftjustify()
!!    call drawstr('This is Left Justified text')
!!
!!    idum=getkey()
!!
!!    call color(D_BLACK)
!!    call rect(0.1, 0.1, 0.9, 0.9)
!!    call color(D_WHITE)
!!    call move2(0.1, 0.5)
!!    call draw2(0.9, 0.5)
!!    call move2(0.5, 0.1)
!!    call draw2(0.5, 0.9)
!!
!!    call color(D_YELLOW)
!!    call move2(0.5, 0.5)
!!    call centertext(.true.)
!!    call drawstr('This is Centered text')
!!    call centertext(.false.)
!!
!!    idum=getkey()
!!
!!    call color(D_BLACK)
!!    call rect(0.1, 0.1, 0.9, 0.9)
!!    call color(D_WHITE)
!!    call move2(0.1, 0.5)
!!    call draw2(0.9, 0.5)
!!    call move2(0.5, 0.1)
!!    call draw2(0.5, 0.9)
!!
!!    call color(D_MAGENTA)
!!    call move2(0.5, 0.5)
!!    call rightjustify()
!!    call drawstr('This is Right Justified text')
!!    call textjustify(achar(0))
!!
!!    idum=getkey()
!!    end subroutine drawstuff
!!
!!    subroutine drawstuff2(ang)
!!    use M_draw
!!    real :: ang
!!    integer :: idum
!!
!!    call color(D_BLACK)
!!    call rect(0.1, 0.1, 0.9, 0.9)
!!    call color(D_WHITE)
!!    call move2(0.1, 0.5)
!!    call draw2(0.9, 0.5)
!!    call move2(0.5, 0.1)
!!    call draw2(0.5, 0.9)
!!
!!
!!    call textang(ang)
!!    call color(D_GREEN)
!!    call move2(0.5, 0.5)
!!    call leftjustify()
!!    call drawchar('B')
!!
!!    call textang(0.0)
!!    call textjustify(achar(0))
!!    call boxtext(0.1, 0.1, 0.4, 0.02, 'The ''B'' should be leftjustified')
!!    call pushattributes()
!!    idum=getkey()
!!
!!    call color(D_BLACK)
!!    call rect(0.1, 0.1, 0.9, 0.9)
!!    call color(D_WHITE)
!!    call move2(0.1, 0.5)
!!    call draw2(0.9, 0.5)
!!    call move2(0.5, 0.1)
!!    call draw2(0.5, 0.9)
!!
!!    call textang(ang)
!!    call color(D_YELLOW)
!!    call move2(0.5, 0.5)
!!    call centertext(.true.)
!!    call drawchar('B')
!!    call centertext(.false.)
!!    call textang(0.0)
!!    call textjustify(achar(0))
!!    call boxtext(0.1, 0.1, 0.4, 0.02, 'The ''B'' should be centered')
!!    call pushattributes()
!!
!!    idum=getkey()
!!
!!    call color(D_BLACK)
!!    call rect(0.1, 0.1, 0.9, 0.9)
!!    call color(D_WHITE)
!!    call move2(0.1, 0.5)
!!    call draw2(0.9, 0.5)
!!    call move2(0.5, 0.1)
!!    call draw2(0.5, 0.9)
!!
!!    call textang(ang)
!!    call color(D_MAGENTA)
!!    call move2(0.5, 0.5)
!!    call rightjustify()
!!    call drawchar('B')
!!    call textang(0.0)
!!    call textjustify(achar(0))
!!    call boxtext(0.1, 0.1, 0.4, 0.02, 'The ''B'' should be rightjustified')
!!    call pushattributes()
!!
!!    idum=getkey()
!!
!!    call color(D_BLACK)
!!    call rect(0.1, 0.1, 0.9, 0.9)
!!    call color(D_WHITE)
!!    call move2(0.1, 0.5)
!!    call draw2(0.9, 0.5)
!!    call move2(0.5, 0.1)
!!    call draw2(0.5, 0.9)
!!
!!    call textang(ang)
!!    call color(D_MAGENTA)
!!    call move2(0.5, 0.5)
!!    call topjustify()
!!    call drawchar('B')
!!    call textang(0.0)
!!    call textjustify(achar(0))
!!    call boxtext(0.1, 0.1, 0.4, 0.02, 'The ''B'' should be topjustified')
!!    call pushattributes()
!!
!!    idum=getkey()
!!
!!    call color(D_BLACK)
!!    call rect(0.1, 0.1, 0.9, 0.9)
!!    call color(D_WHITE)
!!    call move2(0.1, 0.5)
!!    call draw2(0.9, 0.5)
!!    call move2(0.5, 0.1)
!!    call draw2(0.5, 0.9)
!!
!!    call textang(ang)
!!    call color(D_MAGENTA)
!!    call move2(0.5, 0.5)
!!    call topjustify()
!!    call rightjustify()
!!    call drawchar('B')
!!    call textang(0.0)
!!    call textjustify(achar(0))
!!    call boxtext(0.1, 0.1, 0.4, 0.02, 'The ''B'' should be right/topjustified')
!!
!!    idum=getkey()
!!
!!    end subroutine drawstuff2
!!
!!    end program demo_example_text_justification
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
module M_draw
!
! hide logicals from C
! trim and append null to intent(in) character strings
! logical to _Bool mapping not consistent across compilers, g95 does not even define KIND=C_BOOL; so make NAME_F routines
!
use ISO_C_BINDING
implicit none

! ident_1="@(#) M_draw M_draw(3fm) The M_draw graphics library module"

private
!-------------------------------------------------------------------------------
integer(kind=c_short),public,parameter :: D_XCENTERED=     1_C_SHORT
integer(kind=c_short),public,parameter :: D_YCENTERED=     2_C_SHORT
integer(kind=c_short),public,parameter :: D_LEFT=          4_C_SHORT  ! The default
integer(kind=c_short),public,parameter :: D_RIGHT=         8_C_SHORT
integer(kind=c_short),public,parameter :: D_TOP=          16_C_SHORT
integer(kind=c_short),public,parameter :: D_BOTTOM=       32_C_SHORT ! The default
!-------------------------------------------------------------------------------
integer(kind=c_short),public,parameter :: D_NORMAL=        0_C_SHORT ! The default
integer(kind=c_short),public,parameter :: D_BOLD=          1_C_SHORT
!-------------------------------------------------------------------------------
integer(kind=c_short),public,parameter :: D_THICK=         1_C_SHORT
integer(kind=c_short),public,parameter :: D_THIN=          0_C_SHORT ! The default
!-------------------------------------------------------------------------------
integer(kind=c_int),public,parameter   :: D_BLACK    =  0_C_INT
integer(kind=c_int),public,parameter   :: D_RED      =  1_C_INT
integer(kind=c_int),public,parameter   :: D_GREEN    =  2_C_INT
integer(kind=c_int),public,parameter   :: D_YELLOW   =  3_C_INT
integer(kind=c_int),public,parameter   :: D_BLUE     =  4_C_INT
integer(kind=c_int),public,parameter   :: D_MAGENTA  =  5_C_INT
integer(kind=c_int),public,parameter   :: D_CYAN     =  6_C_INT
integer(kind=c_int),public,parameter   :: D_WHITE    =  7_C_INT
!-------------------------------------------------------------------------------
public MATRIX
type, bind(C) :: MATRIX
   real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
end type MATRIX
!-------------------------------------------------------------------------------
 public :: arc
 public :: arcprecision
 public :: backbuffer
 public :: backface
 public :: backfacedir
 public :: bottomjustify
 public :: boxfit
 public :: boxtext
 public :: CalcW2Vcoeffs
 public :: callobj
 public :: centertext
 public :: checkkey
 public :: circle
 public :: circleprecision
 public :: clear
 public :: clipping
 public :: closeobj
 public :: closepoly
 public :: color
 public :: curve
 public :: curvebasis
 public :: curven
 public :: curveprecision
 public :: dashcode
 public :: delobj
 public :: draw
 public :: draw2
 public :: drawchar
 public :: drawstr
 public :: expandviewport
 public :: fixedwidth
 public :: font
 public :: frontbuffer
 public :: genobj
 public :: getaspect
 public :: getcharsize
 public :: getdepth
 public :: getdisplaysize
 public :: getfactors
 public :: getfontasc
 public :: getfontdec
 public :: getfontheight
 public :: getfontsize
 public :: getfontwidth
 public :: getgp
 public :: getgp2
 public :: getgpt
 public :: getkey
 public :: getmatrix
 public :: getopenobj
 public :: getplanes
 public :: getprefposandsize
 public :: getstring
 public :: getviewport
 public :: hatchang
 public :: hatchpitch
 public :: isobj
 public :: isobj_F
 public :: leftjustify
 public :: linestyle
 public :: linewidth
 public :: loadmatrix
 public :: loadobj
 public :: locator
 public :: lookat
 public :: makeobj
 public :: makepoly
 public :: mapcolor
 public :: move
 public :: move2
 public :: multmatrix
 public :: numchars
 public :: ortho
 public :: ortho2
 public :: patch
 public :: patchbasis
 public :: patchcurves
 public :: patchprecision
 public :: pdraw
 public :: perspective
 public :: pmove
 public :: point
 public :: point2
 public :: polarview
 public :: poly
 public :: poly2
 public :: polyfill
 public :: polyhatch
 public :: popattributes
 public :: popdev
 public :: popmatrix
 public :: popviewport
 public :: prefposition
 public :: prefsize
 public :: printattribs
 public :: printvdevice
 public :: pushattributes
 public :: pushdev
 public :: pushmatrix
 public :: pushviewport
 public :: rcurve
 public :: rdraw
 public :: rdraw2
 public :: rect
 public :: rightjustify
 public :: rmove
 public :: rmove2
 public :: rotate
 public :: rsdraw2
 public :: rsmove2
 public :: saveobj
 public :: scale
 public :: sdraw2
 public :: sector
 public :: sgetgp2
 public :: slocator
 public :: smove2
 public :: spoint2
 public :: srect
 public :: strlength
 public :: strlength_F
 public :: swapbuffers
 public :: textang
 public :: textjustify
 public :: textsize
 public :: textslant
 public :: textweight
 public :: topjustify
 public :: translate
 public :: unexpandviewport
 public :: up
 public :: vexit
 public :: vflush
 public :: vgetdev
 public :: viewport
 public :: vinit
 public :: vnewdev
 public :: voutput
 public :: vsetflush
 public :: VtoWxy
 public :: window
 public :: xcentertext
 public :: ycentertext
 public :: yobbarays
!public :: verror

! integer,parameter :: C_BOOL = SELECTED_INT_KIND(1) ! _Bool ! integer*1
!-------------------------------------------------------------------------------
! ==========  function definitions
!-------------------------------------------------------------------------------
! ==========  arc routines
!-------------------------------------------------------------------------------
! extern void arcprecision(int noseg);
   interface
      subroutine arcprecision(N) bind(C,NAME='draw_arcprecision')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
      end subroutine arcprecision
   end interface
!-------------------------------------------------------------------------------
! extern void arc(float x,float y, float radius, float startang, float endang);
   interface
      subroutine arc(X,Y,RADIUS,STARTANG,ENDANG) bind(C,NAME='draw_arc')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: RADIUS
         real(KIND=C_FLOAT),intent(in),value :: STARTANG
         real(KIND=C_FLOAT),intent(in),value :: ENDANG
      end subroutine arc
   end interface
!-------------------------------------------------------------------------------
! extern void circle(float x,float y,float radius);
   interface
      subroutine circle(X,Y,RADIUS) bind(C,NAME='draw_circle')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: RADIUS
      end subroutine circle
   end interface
!-------------------------------------------------------------------------------
! extern void circleprecision(int noseg);
   interface
      subroutine circleprecision(N) bind(C,NAME='draw_circleprecision')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
      end subroutine circleprecision
   end interface
!-------------------------------------------------------------------------------
! extern void sector(float x,float y,float radius,float startang,float endang);
   interface
      subroutine sector(X,Y,RADIUS,STARTANG,ENDANG) bind(C,NAME='draw_sector')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: RADIUS
         real(KIND=C_FLOAT),intent(in),value :: STARTANG
         real(KIND=C_FLOAT),intent(in),value :: ENDANG
      end subroutine sector
   end interface
!-------------------------------------------------------------------------------
! ==========  attr routines
!-------------------------------------------------------------------------------
! extern void popattributes(void);
   interface
      subroutine popattributes() bind(C,NAME='draw_popattributes')
         use ISO_C_BINDING
         implicit none
      end subroutine popattributes
   end interface
!-------------------------------------------------------------------------------
! extern void pushattributes(void);
   interface
      subroutine pushattributes() bind(C,NAME='draw_pushattributes')
         use ISO_C_BINDING
         implicit none
      end subroutine pushattributes
   end interface
!-------------------------------------------------------------------------------
! extern void printattribs(char *s);
   interface
      subroutine printattribs_F(S) bind(C,NAME='draw_printattribs')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR) :: S(*)
      end subroutine printattribs_F
   end interface
!-------------------------------------------------------------------------------
! extern void printvdevice(char *s);
   interface
      subroutine printvdevice_F(S) bind(C,NAME='draw_printvdevice')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR) :: S(*)
      end subroutine printvdevice_F
   end interface
!-------------------------------------------------------------------------------
! ==========  curve routines
!-------------------------------------------------------------------------------
! extern void curve(float geom[4][3]);
   interface
      subroutine curve(GEOM) bind(C,NAME='draw_curve')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),dimension(3,4) :: GEOM
      end subroutine curve
   end interface
!-------------------------------------------------------------------------------
! extern void rcurve_F(Matrix geom);
   interface
      subroutine rcurve_F(GEOM) bind(C,NAME='draw_rcurve')
         use ISO_C_BINDING
         implicit none
         type, bind(C) :: MATRIX
            real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
         end type MATRIX
         !real(KIND=C_FLOAT),intent(in),dimension(4,4) :: GEOM
         type(MATRIX),intent(in) :: GEOM
      end subroutine rcurve_F
   end interface
!-------------------------------------------------------------------------------
! extern void curven(int n, float geom[][3]);
   interface
      subroutine curven(N,GEOM) bind(C,NAME='draw_curven')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
         real(KIND=C_FLOAT),intent(in),dimension(3,*) :: GEOM
      end subroutine curven
   end interface
!-------------------------------------------------------------------------------
! extern void drcurve(int n, Matrix r);
!-------------------------------------------------------------------------------
! extern void curvebasis_F(Matrix basis);
   interface
      subroutine curvebasis_F(BASIS) bind(C,NAME='draw_curvebasis')
         use ISO_C_BINDING
         implicit none
         type, bind(C) :: MATRIX
            real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
         end type MATRIX
         !real(KIND=C_FLOAT),intent(in),dimension(4,4) :: BASIS
         type(MATRIX),intent(in) :: BASIS
      end subroutine curvebasis_F
   end interface
!-------------------------------------------------------------------------------
! extern void curveprecision(int nsegments);
   interface
      subroutine curveprecision(NSEGMENTS) bind(C,NAME='draw_curveprecision')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: NSEGMENTS
      end subroutine curveprecision
   end interface
!-------------------------------------------------------------------------------
! ==========  draw routines
!-------------------------------------------------------------------------------
! extern void draw(float x,float y, float z);
   interface
      subroutine draw(X,Y,Z) bind(C,NAME='draw_draw')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine draw
   end interface
!-------------------------------------------------------------------------------
! extern void draw2(float x,float y);
   interface
      subroutine draw2(X,Y) bind(C,NAME='draw_draw2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine draw2
   end interface
!-------------------------------------------------------------------------------
! extern void rdraw(float dx,float dy,float dz);
   interface
      subroutine rdraw(X,Y,Z) bind(C,NAME='draw_rdraw')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine rdraw
   end interface
!-------------------------------------------------------------------------------
! extern void rdraw2(float dx, float dy);
   interface
      subroutine rdraw2(X,Y) bind(C,NAME='draw_rdraw2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine rdraw2
   end interface
!-------------------------------------------------------------------------------
! extern void sdraw2(float xs, float ys);
   interface
      subroutine sdraw2(X,Y) bind(C,NAME='draw_sdraw2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine sdraw2
   end interface
!-------------------------------------------------------------------------------
! extern void rsdraw2(float dxs, float dys);
   interface
      subroutine rsdraw2(X,Y) bind(C,NAME='draw_rsdraw2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine rsdraw2
   end interface
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! extern void dashcode(float d);
   interface
      subroutine dashcode(D) bind(C,NAME='draw_dashcode')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_float),intent(in),value :: D
      end subroutine dashcode
   end interface
!-------------------------------------------------------------------------------
! extern void dashline(Vector p0, Vector p1);
!-------------------------------------------------------------------------------
! extern void linestyle(char *l);
   interface
      subroutine linestyle_F(L) bind(C,NAME='draw_linestyle')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in) :: L(*)
      end subroutine linestyle_F
   end interface
!-------------------------------------------------------------------------------
! extern void linewidth(int w);
   interface
      subroutine linewidth(W) bind(C,NAME='draw_linewidth')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: W
      end subroutine linewidth
   end interface
!-------------------------------------------------------------------------------
! ==========  device routines
!-------------------------------------------------------------------------------
! extern void clear(void);
   interface
      subroutine clear() bind(C,NAME='draw_clear')
         use ISO_C_BINDING
         implicit none
      end subroutine clear
   end interface
!-------------------------------------------------------------------------------
! extern void color(int i);
   interface
      subroutine color(N) bind(C,NAME='draw_color')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
      end subroutine color
   end interface
!-------------------------------------------------------------------------------
! extern int      getkey(void);
   interface
      function getkey() bind(C,NAME='draw_getkey')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: getkey
      end function getkey
   end interface
!-------------------------------------------------------------------------------
! extern int      getdepth(void);
   interface
      function getdepth() bind(C,NAME='draw_getdepth')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: getdepth
      end function getdepth
   end interface
!-------------------------------------------------------------------------------
! extern int      checkkey(void);
   interface
      function checkkey() bind(C,NAME='draw_checkkey')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: checkkey
      end function checkkey
   end interface
!-------------------------------------------------------------------------------
! extern int      getplanes(void);
   interface
      function getplanes() bind(C,NAME='draw_getplanes')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: getplanes
      end function getplanes
   end interface
!-------------------------------------------------------------------------------
! extern int      locator(float *wx,float *wy);
   interface
      function locator(WX,WY) bind(C,NAME='draw_locator')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: locator
         real(KIND=C_FLOAT),intent(out) :: WX
         real(KIND=C_FLOAT),intent(out) :: WY
      end function locator
   end interface
!-------------------------------------------------------------------------------
! extern int      slocator(float *wx,float *wy);
   interface
      function slocator(WX,WY) bind(C,NAME='draw_slocator')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: SLOCATOR
         real(KIND=C_FLOAT),intent(out) :: WX
         real(KIND=C_FLOAT),intent(out) :: WY
      end function slocator
   end interface
!-------------------------------------------------------------------------------
! extern void mapcolor(int i, short r, short g, short b);
   interface
      subroutine mapcolor(I,R,G,B) bind(C,NAME='draw_mapcolor')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: I
         integer(KIND=C_INT),intent(in),value :: R
         integer(KIND=C_INT),intent(in),value :: G
         integer(KIND=C_INT),intent(in),value :: B
      end subroutine mapcolor
   end interface
!-------------------------------------------------------------------------------
! extern void vinit(char *device);
   interface
      subroutine vinit_F(DEVICE) bind(C,NAME='draw_vinit')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in) :: DEVICE(*)
      end subroutine vinit_F
   end interface
!-------------------------------------------------------------------------------
! extern void vexit(void);
   interface
      subroutine vexit() bind(C,NAME='draw_vexit')
         use ISO_C_BINDING
         implicit none
      end subroutine vexit
   end interface
!-------------------------------------------------------------------------------
! extern void voutput(char *path);
   interface
      subroutine voutput_F(PATH) bind(C,NAME='draw_voutput')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in) :: PATH(*)
      end subroutine voutput_F
   end interface
!-------------------------------------------------------------------------------
! extern void vnewdev(char *device);
   interface
      subroutine vnewdev_F(DEVICE) bind(C,NAME='draw_vnewdev')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in) :: DEVICE(*)
      end subroutine vnewdev_F
   end interface
!-------------------------------------------------------------------------------
! extern char     *vgetdev(char *buf);
   interface
      subroutine vgetdev_F(BUF) bind(C,NAME='draw_vgetdev')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(out) :: BUF(*)
      end subroutine vgetdev_F
   end interface
!-------------------------------------------------------------------------------
! extern void pushdev(char *device);
   interface
      subroutine pushdev_F(DEVICE) bind(C,NAME='draw_pushdev')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in) :: DEVICE(*)
      end subroutine pushdev_F
   end interface
!-------------------------------------------------------------------------------
! extern void popdev(void);
   interface
      subroutine popdev() bind(C,NAME='draw_popdev')
         use ISO_C_BINDING
         implicit none
      end subroutine popdev
   end interface
!-------------------------------------------------------------------------------
! ==========  move routines
!-------------------------------------------------------------------------------
! extern void move(float x, float y, float z);
   interface
      subroutine move(X,Y,Z) bind(C,NAME='draw_move')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine move
   end interface
!-------------------------------------------------------------------------------
! extern void move2(float x, float y);
   interface
      subroutine move2(X,Y) bind(C,NAME='draw_move2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine move2
   end interface
!-------------------------------------------------------------------------------
! extern void rmove(float dx, float dy, float dz);
   interface
      subroutine rmove(X,Y,Z) bind(C,NAME='draw_rmove')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine rmove
   end interface
!-------------------------------------------------------------------------------
! extern void rmove2(float dx, float dy);
   interface
      subroutine rmove2(X,Y) bind(C,NAME='draw_rmove2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine rmove2
   end interface
!-------------------------------------------------------------------------------
! extern void smove2(float xs, float ys);
   interface
      subroutine smove2(X,Y) bind(C,NAME='draw_smove2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine smove2
   end interface
!-------------------------------------------------------------------------------
! extern void rsmove2(float dxs, float dys);
   interface
      subroutine rsmove2(X,Y) bind(C,NAME='draw_rsmove2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine rsmove2
   end interface
!-------------------------------------------------------------------------------
! ==========  object routines
!-------------------------------------------------------------------------------
! extern int    isobj(int n);
   interface
      function isobj_F(N) bind(C,NAME='draw_isobj')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: ISOBJ_F
         integer(KIND=C_INT),intent(in),value :: N
      end function isobj_F
   end interface
!-------------------------------------------------------------------------------
! extern int    genobj(void);
   interface
      function genobj() bind(C,NAME='draw_genobj')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: genobj
      end function genobj
   end interface
!-------------------------------------------------------------------------------
! extern void delobj(int n);
   interface
      subroutine delobj(N) bind(C,NAME='draw_delobj')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
      end subroutine delobj
   end interface
!-------------------------------------------------------------------------------
! extern void makeobj(int n);
   interface
      subroutine makeobj(N) bind(C,NAME='draw_makeobj')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
      end subroutine makeobj
   end interface
!-------------------------------------------------------------------------------
! extern void loadobj(int n, char *file);
   interface
      subroutine loadobj_F(N,FILE) bind(C,NAME='draw_loadobj')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
         character(KIND=C_CHAR),intent(in) :: FILE(*)
      end subroutine loadobj_F
   end interface
!-------------------------------------------------------------------------------
! extern void saveobj(int n, char *file);
   interface
      subroutine saveobj_F(N,FILE) bind(C,NAME='draw_saveobj')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
         character(KIND=C_CHAR),intent(in) :: FILE(*)
      end subroutine saveobj_F
   end interface
!-------------------------------------------------------------------------------
! extern void callobj(int n);
   interface
      subroutine callobj(N) bind(C,NAME='draw_callobj')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
      end subroutine callobj
   end interface
!-------------------------------------------------------------------------------
! extern void closeobj(void);
   interface
      subroutine closeobj() bind(C,NAME='draw_closeobj')
         use ISO_C_BINDING
         implicit none
      end subroutine closeobj
   end interface
!-------------------------------------------------------------------------------
! extern int    getopenobj(void);
   interface
      function getopenobj() bind(C,NAME='draw_getopenobj')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: getopenobj
      end function getopenobj
   end interface
!-------------------------------------------------------------------------------
! ==========  patch routines.
!-------------------------------------------------------------------------------
! extern void patch(Matrix geomx, Matrix geomy, Matrix geomz);
   interface
      subroutine patch_F(GEOMX,GEOMY,GEOMZ) bind(C,NAME='draw_patch')
         use ISO_C_BINDING
         implicit none
         type, bind(C) :: MATRIX
            real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
         end type MATRIX
         type(MATRIX),intent(inout) :: GEOMX
         type(MATRIX),intent(inout) :: GEOMY
         type(MATRIX),intent(inout) :: GEOMZ
      end subroutine patch_F
   end interface
!-------------------------------------------------------------------------------
! extern void rpatch(Matrix geomx, Matrix geomy, Matrix geomz, Matrix geomw);
   interface
      subroutine rpatch_F(GEOMX,GEOMY,GEOMZ) bind(C,NAME='draw_rpatch')
         use ISO_C_BINDING
         implicit none
         type, bind(C) :: MATRIX
            real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
         end type MATRIX
         type(MATRIX),intent(inout) :: GEOMX
         type(MATRIX),intent(inout) :: GEOMY
         type(MATRIX),intent(inout) :: GEOMZ
      end subroutine rpatch_F
   end interface
!-------------------------------------------------------------------------------
! extern void drpatch(Tensor R, int ntcurves, int nucurves, int ntsegs, int nusegs, int ntiter, int nuiter);
!-------------------------------------------------------------------------------
! extern void patchbasis(Matrix tb, Matrix ub) ;
   interface
      subroutine patchbasis_F(TB,UB) bind(C,NAME='draw_patchbasis')
         use ISO_C_BINDING
         implicit none
         type, bind(C) :: MATRIX
            real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
         end type MATRIX
         type(MATRIX),intent(inout) :: TB
         type(MATRIX),intent(inout) :: UB
      end subroutine patchbasis_F
   end interface
!-------------------------------------------------------------------------------
! extern void patchcurves(int nt, int nu);
   interface
      subroutine patchcurves(NT,NU) bind(C,NAME='draw_patchcurves')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: NT
         integer(KIND=C_INT),intent(in),value :: NU
      end subroutine patchcurves
   end interface
!-------------------------------------------------------------------------------
! extern void patchprecision(int tseg, int useg);
   interface
      subroutine patchprecision(TSEG,USEG) bind(C,NAME='draw_patchprecision')
         use ISO_C_BINDING
         implicit none
         INTEGER(KIND=C_INT),intent(in),value :: TSEG
         INTEGER(KIND=C_INT),intent(in),value :: USEG
      end subroutine patchprecision
   end interface
!-------------------------------------------------------------------------------
! extern void transformtensor(Tensor S, Matrix m);
!-------------------------------------------------------------------------------
! ==========  point routines
!-------------------------------------------------------------------------------
! extern void point(float x, float y, float z);
   interface
      subroutine point(X,Y,Z) bind(C,NAME='draw_point')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine point
   end interface
!-------------------------------------------------------------------------------
! extern void point2(float x, float y);
   interface
      subroutine point2(X,Y) bind(C,NAME='draw_point2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine point2
   end interface
!-------------------------------------------------------------------------------
! extern void spoint2(float xs, float ys);
   interface
      subroutine spoint2(X,Y) bind(C,NAME='draw_spoint2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
      end subroutine spoint2
   end interface
!-------------------------------------------------------------------------------
! ==========  polygon routines.
!-------------------------------------------------------------------------------
! extern void poly(int n, float dp[][3]);
   interface
      subroutine poly(N,DP) bind(C,NAME='draw_poly')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
         real(KIND=C_FLOAT),intent(in),dimension(3,*) :: DP
      end subroutine poly
   end interface
!-------------------------------------------------------------------------------
! extern void poly2(int n, float dp[][2]);
   interface
      subroutine poly2(N,DP) bind(C,NAME='draw_poly2')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: N
         real(KIND=C_FLOAT),intent(in),dimension(2,*) :: DP
      end subroutine poly2
   end interface
!-------------------------------------------------------------------------------
! extern void hatchang(float a);
   interface
      subroutine hatchang(A) bind(C,NAME='draw_hatchang')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: A
      end subroutine hatchang
   end interface
!-------------------------------------------------------------------------------
! extern void makepoly(void);
   interface
      subroutine makepoly() bind(C,NAME='draw_makepoly')
         use ISO_C_BINDING
         implicit none
      end subroutine makepoly
   end interface
!-------------------------------------------------------------------------------
! extern void polyfill(int onoff);
   interface
      subroutine polyfill_F(ONOFF) bind(C,NAME='draw_polyfill')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: ONOFF
      end subroutine polyfill_F
   end interface
!-------------------------------------------------------------------------------
! extern void closepoly(void);
   interface
      subroutine closepoly() bind(C,NAME='draw_closepoly')
         use ISO_C_BINDING
         implicit none
      end subroutine closepoly
   end interface
!-------------------------------------------------------------------------------
! extern void polyhatch(int onoff);
   interface
      subroutine polyhatch_F(ONOFF) bind(C,NAME='draw_polyhatch')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: ONOFF
      end subroutine polyhatch_F
   end interface
!-------------------------------------------------------------------------------
! extern void hatchpitch(float a);
   interface
      subroutine hatchpitch(A) bind(C,NAME='draw_hatchpitch')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: A
      end subroutine hatchpitch
   end interface
!-------------------------------------------------------------------------------
! extern void backfacedir(int cdir);
   interface
      subroutine backfacedir_F(cdir) bind(C,NAME='draw_backfacedir')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: cdir
      end subroutine backfacedir_F
   end interface
!-------------------------------------------------------------------------------
! extern void backface(int onoff);
   interface
      subroutine backface_F(ONOFF) bind(C,NAME='draw_backface')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: ONOFF
      end subroutine backface_F
   end interface
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! extern void pmove(float x, float y, float z);
   interface
      subroutine pmove(X,Y,Z) bind(C,NAME='draw_pmove')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine pmove
   end interface
!-------------------------------------------------------------------------------
! extern void pdraw(float x, float y, float z);
   interface
      subroutine pdraw(X,Y,Z) bind(C,NAME='draw_pdraw')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine pdraw
   end interface
!-------------------------------------------------------------------------------
! ==========  rectangle routine
!-------------------------------------------------------------------------------
! extern void rect(float x1, float y1, float x2, float y2);
   interface
      subroutine rect(X,Y,X2,Y2) bind(C,NAME='draw_rect')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: X2
         real(KIND=C_FLOAT),intent(in),value :: Y2
      end subroutine rect
   end interface
!-------------------------------------------------------------------------------
! extern void srect(float x1, float y1, float x2, float y2);
   interface
      subroutine srect(X,Y,X2,Y2) bind(C,NAME='draw_srect')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: X2
         real(KIND=C_FLOAT),intent(in),value :: Y2
      end subroutine srect
   end interface
!-------------------------------------------------------------------------------
! ==========  text routines
!-------------------------------------------------------------------------------
! extern float strlength(char *s);
   interface
      function strlength_F(S) bind(C,NAME='draw_strlength')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT) :: strlength_F
         character(KIND=C_CHAR),intent(in) :: S(*)
      end function strlength_F
   end interface
!-------------------------------------------------------------------------------
! extern void boxtext(float x, float y, float l, float h, char *s);
   interface
      subroutine boxtext_F(X,Y,L,H,S) bind(C,NAME='draw_boxtext')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: L
         real(KIND=C_FLOAT),intent(in),value :: H
         character(KIND=C_CHAR,len=1),intent(in) :: S(*)
      end subroutine boxtext_F
   end interface
!-------------------------------------------------------------------------------
! extern void boxfit(float l, float h, int nchars);
   interface
      subroutine boxfit(L,H,NCHARS) bind(C,NAME='draw_boxfit')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: L
         real(KIND=C_FLOAT),intent(in),value :: H
         integer(KIND=C_INT),intent(in),value   :: NCHARS
      end subroutine boxfit
   end interface
!-------------------------------------------------------------------------------
! extern int numchars(void);
   interface
      function numchars() bind(C,NAME='draw_numchars')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: numchars
      end function numchars
   end interface
!-------------------------------------------------------------------------------
! extern void getcharsize(char c, float *width, float *height);
   interface
      subroutine getcharsize(C,WIDTH,HEIGHT) bind(C,NAME='draw_getcharsize')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out) :: WIDTH
         real(KIND=C_FLOAT),intent(out) :: HEIGHT
         character(KIND=C_CHAR),intent(in),value :: C
      end subroutine getcharsize
   end interface
!-------------------------------------------------------------------------------
! extern void drawchar(int c);
!  there are currently issues in some ISO_C_BINDING implementations about a
!  single character being equivalent to an integer in C and pass by value
!  and such; might have to replace drawchar with a call to drawstr
   interface
      subroutine drawchar_F(C) bind(C,NAME='draw_drawchar')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in),value :: C
      end subroutine drawchar_F
   end interface
!-------------------------------------------------------------------------------
! extern void textsize(float width, float height);
   interface
      subroutine textsize(WIDTH,HEIGHT) bind(C,NAME='draw_textsize')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: WIDTH
         real(KIND=C_FLOAT),intent(in),value :: HEIGHT
      end subroutine textsize
   end interface
!-------------------------------------------------------------------------------
! extern float  getfontwidth(void);
   interface
      function getfontwidth() bind(C,NAME='draw_getfontwidth')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT) :: getfontwidth
      end function getfontwidth
   end interface
!-------------------------------------------------------------------------------
! extern float  getfontheight(void);
   interface
      function getfontheight() bind(C,NAME='draw_getfontheight')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT) :: getfontheight
      end function getfontheight
   end interface
!-------------------------------------------------------------------------------
! extern float  getfontdec(void);
   interface
      function getfontdec() bind(C,NAME='draw_getfontdec')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT) :: getfontdec
      end function getfontdec
   end interface
!-------------------------------------------------------------------------------
! extern float  getfontasc(void);
   interface
      function getfontasc() bind(C,NAME='draw_getfontasc')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT) :: getfontasc
      end function getfontasc
   end interface
!-------------------------------------------------------------------------------
! extern void getfontsize(float *cw, float *ch);
   interface
      subroutine getfontsize(CW,CH) bind(C,NAME='draw_getfontsize')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out) :: CW
         real(KIND=C_FLOAT),intent(out) :: CH
      end subroutine getfontsize
   end interface
!-------------------------------------------------------------------------------
! extern void drawstr(char *string);
   interface
      subroutine drawstr_F(STRING) bind(C,NAME='draw_drawstr')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in) :: STRING(*)
      end subroutine drawstr_F
   end interface
!-------------------------------------------------------------------------------
! extern void centertext(int onoff);
   interface
      subroutine centertext_F(ONOFF) bind(C,NAME='draw_centertext')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: ONOFF
      end subroutine centertext_F
   end interface
!-------------------------------------------------------------------------------
! extern void fixedwidth(int onoff);
   interface
      subroutine fixedwidth_F(ONOFF) bind(C,NAME='draw_fixedwidth')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: ONOFF
      end subroutine fixedwidth_F
   end interface
!-------------------------------------------------------------------------------
! extern void textang(float ang);
   interface
      subroutine textang(ANG) bind(C,NAME='draw_textang')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: ANG
      end subroutine textang
   end interface
!-------------------------------------------------------------------------------
! extern void font(char *name);
   interface
      subroutine font_F(NAME) bind(C,NAME='draw_font')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in) :: NAME(*)
      end subroutine font_F
   end interface
!-------------------------------------------------------------------------------
! extern int    getstring(int bcol, char *s);
   interface
      function getstring(BCOL,S) bind(C,NAME='draw_getstring')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT):: GETSTRING
         integer(KIND=C_INT),intent(in),value :: BCOL
         character(KIND=C_CHAR) :: S(*)
      end function getstring
   end interface
!-------------------------------------------------------------------------------
! ==========  transformation routines
!-------------------------------------------------------------------------------
! extern void scale(float x, float y, float z);
   interface
      subroutine scale(X,Y,Z) bind(C,NAME='draw_scale')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine scale
   end interface
!-------------------------------------------------------------------------------
! extern void translate(float x, float y, float z);
   interface
      subroutine translate(X,Y,Z) bind(C,NAME='draw_translate')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine translate
   end interface
!-------------------------------------------------------------------------------
! extern void rotate(float r,char axis);
   interface
      subroutine rotate(R,AXIS) bind(C,NAME='draw_rotate')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: R
         character(KIND=C_CHAR),intent(in),value :: AXIS
      end subroutine rotate
   end interface
!-------------------------------------------------------------------------------
! ==========  window definition routines
!-------------------------------------------------------------------------------
! extern void ortho(float left,float right,float bottom,float top,float hither,float yon);
   interface
      subroutine ortho(LEFT,RIGHT,BOTTOM,TOP,HITHER,YON) bind(C,NAME='draw_ortho')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: LEFT
         real(KIND=C_FLOAT),intent(in),value :: RIGHT
         real(KIND=C_FLOAT),intent(in),value :: BOTTOM
         real(KIND=C_FLOAT),intent(in),value :: TOP
         real(KIND=C_FLOAT),intent(in),value :: HITHER
         real(KIND=C_FLOAT),intent(in),value :: YON
      end subroutine ortho
   end interface
!-------------------------------------------------------------------------------
! extern void ortho2(float left,float  right,float  bottom,float  top);
   interface
      subroutine ortho2(LEFT,RIGHT,BOTTOM,TOP) bind(C,NAME='draw_ortho2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: LEFT
         real(KIND=C_FLOAT),intent(in),value :: RIGHT
         real(KIND=C_FLOAT),intent(in),value :: BOTTOM
         real(KIND=C_FLOAT),intent(in),value :: TOP
      end subroutine ortho2
   end interface
!-------------------------------------------------------------------------------
! extern void lookat(float vx,float vy,float vz,float px,float py,float pz,float twist);
   interface
      subroutine lookat(VX,VY,VZ,PX,PY,PZ,TWIST) bind(C,NAME='draw_lookat')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: VX
         real(KIND=C_FLOAT),intent(in),value :: VY
         real(KIND=C_FLOAT),intent(in),value :: VZ
         real(KIND=C_FLOAT),intent(in),value :: PX
         real(KIND=C_FLOAT),intent(in),value :: PY
         real(KIND=C_FLOAT),intent(in),value :: PZ
         real(KIND=C_FLOAT),intent(in),value :: TWIST
      end subroutine lookat
   end interface
!-------------------------------------------------------------------------------
! extern void window(float left,float right,float bottom,float top,float hither,float yon);
   interface
      subroutine window(LEFT,RIGHT,BOTTOM,TOP,HITHER,YON) bind(C,NAME='draw_window')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: LEFT
         real(KIND=C_FLOAT),intent(in),value :: RIGHT
         real(KIND=C_FLOAT),intent(in),value :: BOTTOM
         real(KIND=C_FLOAT),intent(in),value :: TOP
         real(KIND=C_FLOAT),intent(in),value :: HITHER
         real(KIND=C_FLOAT),intent(in),value :: YON
      end subroutine window
   end interface
!-------------------------------------------------------------------------------
! extern void polarview(float dist, float azim, float inc, float twist) ;
   interface
      subroutine polarview(DIST,AZIM,INC,TWIST) bind(C,NAME='draw_polarview')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: DIST
         real(KIND=C_FLOAT),intent(in),value :: AZIM
         real(KIND=C_FLOAT),intent(in),value :: INC
         real(KIND=C_FLOAT),intent(in),value :: TWIST
      end subroutine polarview
   end interface
!-------------------------------------------------------------------------------
! extern void perspective(float fov, float aspect, float hither, float yon) ;
   interface
      subroutine perspective(FOV,ASPECT,HITHER,YON) bind(C,NAME='draw_perspective')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: FOV
         real(KIND=C_FLOAT),intent(in),value :: ASPECT
         real(KIND=C_FLOAT),intent(in),value :: HITHER
         real(KIND=C_FLOAT),intent(in),value :: YON
      end subroutine perspective
   end interface
!-------------------------------------------------------------------------------
! extern void up(float x, float y, float z) ;
   interface
      subroutine up(X,Y,Z) bind(C,NAME='draw_up')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: X
         real(KIND=C_FLOAT),intent(in),value :: Y
         real(KIND=C_FLOAT),intent(in),value :: Z
      end subroutine up
   end interface
!-------------------------------------------------------------------------------
! ==========  routines for manipulating the viewport
!-------------------------------------------------------------------------------
! extern void getviewport(float *xlow, float *xhigh, float *ylow, float *yhigh);
   interface
      subroutine getviewport(XLOW,XHIGH,YLOW,YHIGH) bind(C,NAME='draw_getviewport')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out) :: XLOW
         real(KIND=C_FLOAT),intent(out) :: XHIGH
         real(KIND=C_FLOAT),intent(out) :: YLOW
         real(KIND=C_FLOAT),intent(out) :: YHIGH
      end subroutine getviewport
   end interface
!-------------------------------------------------------------------------------
! extern void viewport(float xlow, float xhigh, float ylow, float yhigh);
   interface
      subroutine viewport(XLOW,XHIGH,YLOW,YHIGH) bind(C,NAME='draw_viewport')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: XLOW
         real(KIND=C_FLOAT),intent(in),value :: XHIGH
         real(KIND=C_FLOAT),intent(in),value :: YLOW
         real(KIND=C_FLOAT),intent(in),value :: YHIGH
      end subroutine viewport
   end interface
!-------------------------------------------------------------------------------
! extern void popviewport(void);
   interface
      subroutine popviewport() bind(C,NAME='draw_popviewport')
         use ISO_C_BINDING
         implicit none
      end subroutine popviewport
   end interface
!-------------------------------------------------------------------------------
! extern void pushviewport(void);
   interface
      subroutine pushviewport() bind(C,NAME='draw_pushviewport')
         use ISO_C_BINDING
         implicit none
      end subroutine pushviewport
   end interface
!-------------------------------------------------------------------------------
! ==========  routines for retrieving the graphics position
!-------------------------------------------------------------------------------
! extern void getgp(float *x,float *y,float *z);
   interface
      subroutine getgp(X,Y,Z) bind(C,NAME='draw_getgp')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out):: X
         real(KIND=C_FLOAT),intent(out) :: Y
         real(KIND=C_FLOAT),intent(out) :: Z
      end subroutine getgp
   end interface
!-------------------------------------------------------------------------------
! extern void getgpt(float *x,float *y,float *z, float *w);
   interface
      subroutine getgpt(X,Y,Z,W) bind(C,NAME='draw_getgpt')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out) :: X
         real(KIND=C_FLOAT),intent(out) :: Y
         real(KIND=C_FLOAT),intent(out) :: Z
         real(KIND=C_FLOAT),intent(out) :: W
      end subroutine getgpt
   end interface
!-------------------------------------------------------------------------------
! extern void getgp2(float *x,float *y);
   interface
      subroutine getgp2(X,Y) bind(C,NAME='draw_getgp2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out) :: X
         real(KIND=C_FLOAT),intent(out) :: Y
      end subroutine getgp2
   end interface
!-------------------------------------------------------------------------------
! extern void sgetgp2(float *xs,float *ys);
   interface
      subroutine sgetgp2(X,Y) bind(C,NAME='draw_sgetgp2')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out) :: X
         real(KIND=C_FLOAT),intent(out) :: Y
      end subroutine sgetgp2
   end interface
!-------------------------------------------------------------------------------
! ==========  routines for retrieving the aspect details of the device
!-------------------------------------------------------------------------------
! extern float    getaspect(void);
   interface
      function getaspect() bind(C,NAME='draw_getaspect')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT) :: getaspect
      end function getaspect
   end interface
!-------------------------------------------------------------------------------
! extern void getfactors(float *x,float *y);
   interface
      subroutine getfactors(X,Y) bind(C,NAME='draw_getfactors')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out) :: X
         real(KIND=C_FLOAT),intent(out) :: Y
      end subroutine getfactors
   end interface
!-------------------------------------------------------------------------------
! extern void getdisplaysize(float *x,float *y);
   interface
      subroutine getdisplaysize(X,Y) bind(C,NAME='draw_getdisplaysize')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(out) :: X
         real(KIND=C_FLOAT),intent(out) :: Y
      end subroutine getdisplaysize
   end interface
!-------------------------------------------------------------------------------
! extern void expandviewport(void);
   interface
      subroutine expandviewport() bind(C,NAME='draw_expandviewport')
         use ISO_C_BINDING
         implicit none
      end subroutine expandviewport
   end interface
!-------------------------------------------------------------------------------
! extern void unexpandviewport(void);
   interface
      subroutine unexpandviewport() bind(C,NAME='draw_unexpandviewport')
         use ISO_C_BINDING
         implicit none
      end subroutine unexpandviewport
   end interface
!-------------------------------------------------------------------------------
! ==========  routines for handling the buffering
!-------------------------------------------------------------------------------
! extern int      backbuffer(void);
   interface
      function backbuffer() bind(C,NAME='draw_backbuffer')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT) :: backbuffer
      end function backbuffer
   end interface
!-------------------------------------------------------------------------------
! extern void frontbuffer(void);
   interface
      subroutine frontbuffer() bind(C,NAME='draw_frontbuffer')
         use ISO_C_BINDING
         implicit none
      end subroutine frontbuffer
   end interface
!-------------------------------------------------------------------------------
! extern int      swapbuffers(void);
   interface
      subroutine swapbuffers() bind(C,NAME='draw_swapbuffers')
         use ISO_C_BINDING
         implicit none
      end subroutine swapbuffers
   end interface
!-------------------------------------------------------------------------------
! ==========  routines for window sizing and positioning
!-------------------------------------------------------------------------------
! void prefposition(int x, int y);
   interface
      subroutine prefposition(X,Y) bind(C,NAME='draw_prefposition')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: X
         integer(KIND=C_INT),intent(in),value :: Y
      end subroutine prefposition
   end interface
!-------------------------------------------------------------------------------
! void prefsize(int x, int y);
   interface
      subroutine prefsize(X,Y) bind(C,NAME='draw_prefsize')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: X
         integer(KIND=C_INT),intent(in),value :: Y
      end subroutine prefsize
   end interface
!-------------------------------------------------------------------------------
! void getprefposandsize(int *x, int *y, int *xs, int *ys);
   interface
      subroutine getprefposandsize(X,Y,XS,YS) bind(C,NAME='draw_getprefposandsize')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(out) :: X
         integer(KIND=C_INT),intent(out) :: Y
         integer(KIND=C_INT),intent(out) :: XS
         integer(KIND=C_INT),intent(out) :: YS
      end subroutine getprefposandsize
   end interface
!-------------------------------------------------------------------------------
! ==========  Misc control routines
!-------------------------------------------------------------------------------
! extern void clipping(int onoff);
   interface
      subroutine clipping_F(ONOFF) bind(C,NAME='draw_clipping')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: ONOFF
      end subroutine clipping_F
   end interface
!-------------------------------------------------------------------------------
! extern void vsetflush(int yn);
   interface
      subroutine vsetflush_F(YN) bind(C,NAME='draw_vsetflush')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: YN
      end subroutine vsetflush_F
   end interface
!-------------------------------------------------------------------------------
! extern void vflush(void);
   interface
      subroutine vflush() bind(C,NAME='draw_vflush')
         use ISO_C_BINDING
         implicit none
      end subroutine vflush
   end interface
!-------------------------------------------------------------------------------
! extern void clip(Vector p0, Vector p1);
!-------------------------------------------------------------------------------
! extern void quickclip(Vector p0, Vector p1);
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! extern void yobbarays(int onoff);
   interface
      subroutine yobbarays(ONOFF) bind(C,NAME='draw_yobbarays')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: ONOFF
      end subroutine yobbarays
   end interface
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
! extern void xcentertext(void);
   interface
      subroutine xcentertext() bind(C,NAME='draw_xcentertext')
         use ISO_C_BINDING
         implicit none
      end subroutine xcentertext
   end interface
!-------------------------------------------------------------------------------
! extern void ycentertext(void);
   interface
      subroutine ycentertext() bind(C,NAME='draw_ycentertext')
         use ISO_C_BINDING
         implicit none
      end subroutine ycentertext
   end interface
!-------------------------------------------------------------------------------
! extern void topjustify(void);
   interface
      subroutine topjustify() bind(C,NAME='draw_topjustify')
         use ISO_C_BINDING
         implicit none
      end subroutine topjustify
   end interface
!-------------------------------------------------------------------------------
! extern void bottomjustify(void);
   interface
      subroutine bottomjustify() bind(C,NAME='draw_bottomjustify')
         use ISO_C_BINDING
         implicit none
      end subroutine bottomjustify
   end interface
!-------------------------------------------------------------------------------
! extern void leftjustify(void);
   interface
      subroutine leftjustify() bind(C,NAME='draw_leftjustify')
         use ISO_C_BINDING
         implicit none
      end subroutine leftjustify
   end interface
!-------------------------------------------------------------------------------
! extern void rightjustify(void);
   interface
      subroutine rightjustify() bind(C,NAME='draw_rightjustify')
         use ISO_C_BINDING
         implicit none
      end subroutine rightjustify
   end interface
!-------------------------------------------------------------------------------
! extern void textjustify(unsigned val);
! extern void textjustify(char val);
   interface
      subroutine textjustify(VAL) bind(C,NAME='draw_textjustify')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in),value :: VAL
      end subroutine textjustify
   end interface
!-------------------------------------------------------------------------------
! extern void textslant(float val);
   interface
      subroutine textslant(VAL) bind(C,NAME='draw_textslant')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: VAL
      end subroutine textslant
   end interface
!-------------------------------------------------------------------------------
! extern void textweight(int val);
   interface
      subroutine textweight(VAL) bind(C,NAME='draw_textweight')
         use ISO_C_BINDING
         implicit none
         integer(KIND=C_INT),intent(in),value :: VAL
      end subroutine textweight
   end interface
!-------------------------------------------------------------------------------
! ==========  matrix stack routines
!-------------------------------------------------------------------------------
! extern void popmatrix(void);
   interface
      subroutine popmatrix() bind(C,NAME='draw_popmatrix')
         use ISO_C_BINDING
         implicit none
      end subroutine popmatrix
   end interface
!-------------------------------------------------------------------------------
! extern void pushmatrix(void);
   interface
      subroutine pushmatrix() bind(C,NAME='draw_pushmatrix')
         use ISO_C_BINDING
         implicit none
      end subroutine pushmatrix
   end interface
!-------------------------------------------------------------------------------
! ==========  matrix stack routines
!-------------------------------------------------------------------------------
! extern void getmatrix_F(Matrix m);
   interface
      subroutine getmatrix_F(M) bind(C,NAME='draw_getmatrix')
         use ISO_C_BINDING
         implicit none
         type, bind(C) :: MATRIX
            real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
         end type MATRIX
         !real(KIND=C_FLOAT),intent(out),dimension(4,4) :: M
         type(MATRIX),intent(out) :: M
      end subroutine getmatrix_F
   end interface
!-------------------------------------------------------------------------------
! extern void loadmatrix_F(Matrix mat);
   interface
      subroutine loadmatrix_F(M) bind(C,NAME='draw_loadmatrix')
         use ISO_C_BINDING
         implicit none
         type, bind(C) :: MATRIX
            real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
         end type MATRIX
         !real(KIND=C_FLOAT),intent(in),dimension(4,4) :: M
         type(MATRIX),intent(in) :: M
      end subroutine loadmatrix_F
   end interface
!-------------------------------------------------------------------------------
! extern void multmatrix_F(Matrix mat);
   interface
      subroutine multmatrix_F(M) bind(C,NAME='draw_multmatrix')
         use ISO_C_BINDING
         implicit none
         type, bind(C) :: MATRIX
            real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
         end type MATRIX
         !real(KIND=C_FLOAT),intent(inout),dimension(4,4) :: M
         type(MATRIX),intent(inout) :: M
      end subroutine multmatrix_F
   end interface
!-------------------------------------------------------------------------------
! extern void printmat(char *s, Matrix m);
!-------------------------------------------------------------------------------
! extern void printvect(char *s, Vector v);
!-------------------------------------------------------------------------------
! ==========  object routines
!-------------------------------------------------------------------------------
! extern Token  *newtokens(int num);
!-------------------------------------------------------------------------------
! extern void polyobj(int n, Token dp[]);
!-------------------------------------------------------------------------------
! ==========  tensor routines
!-------------------------------------------------------------------------------
! extern void premulttensor(Tensor c, Matrix a,Tensor  b) ;
!-------------------------------------------------------------------------------
! extern void multtensor(Tensor c, Matrix a,Tensor  b) ;
!-------------------------------------------------------------------------------
! extern void copytensor(Tensor b, Tensor a) ;
!-------------------------------------------------------------------------------
! extern void copytensortrans(Tensor b,Tensor  a) ;
!-------------------------------------------------------------------------------
! ==========  mapping routines
!-------------------------------------------------------------------------------
! extern int WtoVx( float p[]);
!-------------------------------------------------------------------------------
! extern int WtoVy( float p[]);
!-------------------------------------------------------------------------------
! extern void VtoWxy( float xs, float ys, float  *xw, float *yw);
   interface
      subroutine VtoWxy(XS,YS,XW,YW) bind(C,NAME='draw_VtoWxy')
         use ISO_C_BINDING
         implicit none
         real(KIND=C_FLOAT),intent(in),value :: XS
         real(KIND=C_FLOAT),intent(in),value :: YS
         real(KIND=C_FLOAT),intent(out)      :: XW
         real(KIND=C_FLOAT),intent(out)      :: YW
      end subroutine VtoWxy
   end interface
!-------------------------------------------------------------------------------
! extern void CalcW2Vcoeffs(void);
   interface
      subroutine CalcW2Vcoeffs() bind(C,NAME='draw_CalcW2Vcoeffs')
         use ISO_C_BINDING
         implicit none
      end subroutine CalcW2Vcoeffs
   end interface
!-------------------------------------------------------------------------------
! ==========  general matrix and vector routines
!-------------------------------------------------------------------------------
! extern void mult4x4(register Matrix a, register Matrix b, register Matrix c);
!-------------------------------------------------------------------------------
! extern void copymatrix(register Matrix a, register Matrix b);
!-------------------------------------------------------------------------------
! extern void identmatrix(Matrix a);
!-------------------------------------------------------------------------------
! extern void copyvector(register Vector a, register Vector b);
!-------------------------------------------------------------------------------
! extern void premultvector(Vector v, Vector a, Matrix b);
!-------------------------------------------------------------------------------
! extern void copytranspose(register Matrix a, register Matrix b);
!-------------------------------------------------------------------------------
! extern void multvector(Vector v, Vector a, Matrix b);
!-------------------------------------------------------------------------------
! ==========  other internal routines
!-------------------------------------------------------------------------------
! extern void verror(char *str);
   interface
      subroutine verror_F(STR) bind(C,NAME='draw_verror')
         use ISO_C_BINDING
         implicit none
         character(KIND=C_CHAR),intent(in) :: STR(*)
      end subroutine verror_F
   end interface
!-------------------------------------------------------------------------------
! extern int    hershfont(char *fontname);
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
! extensions in Fortran

public  :: page
public  :: invokeobj
public  :: pop
public  :: push
public  :: print

interface page
   module procedure :: biggest_ortho2
   module procedure :: page_rri
end interface page
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
contains
!-------------------------------------------------------------------------------
!!call polyfill(.false.)
 subroutine polyfill(ONOFF)
   use ISO_C_BINDING
   implicit none
   logical,intent(in)  :: ONOFF
   integer(KIND=C_INT) :: IONOFF
   if(ONOFF.eqv. .true.)then
      IONOFF=1 ! ON
   else
      IONOFF=0 ! OFF
   endif
   call polyfill_F(IONOFF)
end subroutine polyfill
!-------------------------------------------------------------------------------
!!call backface(.false.)
 subroutine backface(ONOFF)
   use ISO_C_BINDING
   implicit none
   logical,intent(in)  :: ONOFF
   integer(KIND=C_INT) :: IONOFF
   if(ONOFF .eqv. .true.)then
      IONOFF=1 ! ON
   else
      IONOFF=0 ! OFF
   endif
   call backface_F(IONOFF)
end subroutine backface
!-------------------------------------------------------------------------------
!!call backfacedir(.false.)
 subroutine backfacedir(CDIR)
   use ISO_C_BINDING
   implicit none
   logical,intent(in)   :: CDIR
   integer(KIND=C_INT)  :: ICDIR
   if(CDIR .eqv. .true.)then
      ICDIR=1 ! CLOCKWISE (IN SCREEN COORDS)
   else
      ICDIR=0 ! ANTICLOCKWISE
   endif
   call backfacedir_F(ICDIR)
end subroutine backfacedir
!-------------------------------------------------------------------------------
!!call centertext(.false.)
 subroutine centertext(ONOFF)
   use ISO_C_BINDING
   implicit none
   logical,intent(in)  :: ONOFF
   integer(KIND=C_INT) :: IONOFF
   if(ONOFF .eqv. .true.)then
      IONOFF=1 ! ON
   else
      IONOFF=0 ! OFF
   endif
   call centertext_F(IONOFF)
end subroutine centertext
!-------------------------------------------------------------------------------
!!call clipping(.false.)
 subroutine clipping(ONOFF)
   use ISO_C_BINDING
   implicit none
   logical,intent(in)  :: ONOFF
   integer(KIND=C_INT) :: IONOFF
   if(ONOFF .eqv. .true.)then
      IONOFF=1 ! ON
   else
      IONOFF=0 ! OFF
   endif
   call clipping_F(IONOFF)
end subroutine clipping
!-------------------------------------------------------------------------------
!!call fixedwidth(.false.)
 subroutine fixedwidth(ONOFF)
   use ISO_C_BINDING
   implicit none
   logical,intent(in)  :: ONOFF
   integer(KIND=C_INT) :: IONOFF
   if(ONOFF .eqv. .true.)then
      IONOFF=1 ! ON
   else
      IONOFF=0 ! OFF
   endif
   call fixedwidth_F(IONOFF)
end subroutine fixedwidth
!-------------------------------------------------------------------------------
!!call polyhatch(.false.)
 subroutine polyhatch(ONOFF)
   use ISO_C_BINDING
   implicit none
   logical,intent(in)  :: ONOFF
   integer(KIND=C_INT) :: IONOFF
   if(ONOFF .eqv. .true.)then
      IONOFF=1 ! ON
   else
      IONOFF=0 ! OFF
   endif
   call polyhatch_F(IONOFF)
end subroutine polyhatch
!-------------------------------------------------------------------------------
!!vsetflush(.false.)
 subroutine vsetflush(ONOFF)
   use ISO_C_BINDING
   implicit none
   logical,intent(in)  :: ONOFF
   integer(KIND=C_INT) :: IONOFF
   if(ONOFF .eqv. .true.)then
      IONOFF=1 ! ON
   else
      IONOFF=0 ! OFF
   endif
   call vsetflush_F(IONOFF)
end subroutine vsetflush
!-------------------------------------------------------------------------------
 function isobj(N)
   use ISO_C_BINDING
   implicit none
   logical :: isobj
   integer(KIND=C_INT),intent(in) :: N
   if(isobj_F(N) == 0 )then
      isobj=.false.
   else
      isobj=.true.
   endif
end function isobj
!-------------------------------------------------------------------------------
 subroutine font(NAME)
   implicit none
   character(len=*),intent(in) :: NAME
   call font_F(s2c(NAME))
 end subroutine font
!-------------------------------------------------------------------------------
 subroutine linestyle(NAME)
   implicit none
   character(len=*),intent(in) :: NAME
   call linestyle_F(s2c(NAME))
 end subroutine linestyle
!-------------------------------------------------------------------------------
 subroutine printvdevice(S)
   implicit none
   character(len=*),intent(in) :: S
   call printvdevice_F(s2c(S))
 end subroutine printvdevice
!-------------------------------------------------------------------------------
 subroutine printattribs(S)
   implicit none
   character(len=*),intent(in) :: S
   call printattribs_F(s2c(S))
 end subroutine printattribs
!-------------------------------------------------------------------------------
 subroutine vinit(NAME)
   implicit none
   character(len=*),intent(in) :: NAME
   call vinit_F(s2c(NAME))
 end subroutine vinit
!-------------------------------------------------------------------------------
 subroutine voutput(NAME)
   implicit none
   character(len=*),intent(in) :: NAME
   call voutput_F(s2c(NAME))
 end subroutine voutput
!-------------------------------------------------------------------------------
 subroutine vnewdev(NAME)
   implicit none
   character(len=*),intent(in) :: NAME
   call vnewdev_F(s2c(NAME))
 end subroutine vnewdev
!-------------------------------------------------------------------------------
 subroutine pushdev(NAME)
   implicit none
   character(len=*),intent(in) :: NAME
   call pushdev_F(s2c(NAME))
 end subroutine pushdev
!-------------------------------------------------------------------------------
 subroutine vgetdev(NAME)
   use ISO_C_BINDING
   implicit none
   character(len=*)            :: NAME
   integer(KIND=C_INT)         :: i
   NAME=' '
   i=len(NAME)
   NAME(i:i)=char(0)
   call vgetdev_F(NAME)
   do i=1,len(NAME)  ! convert nulls to spaces in returned value
      if(NAME(i:i).eq.char(0))then
         NAME(i:i)=' '
      endif
   enddo
end subroutine vgetdev
!-------------------------------------------------------------------------------
 subroutine loadobj(N,FILE)
   use ISO_C_BINDING
   implicit none
   character(len=*),intent(in) :: FILE
   integer(KIND=C_INT)         :: N
   call loadobj_F(N,s2c(FILE))
 end subroutine loadobj
!-------------------------------------------------------------------------------
 subroutine saveobj(N,FILE)
   use ISO_C_BINDING
   implicit none
   character(len=*),intent(in)  :: FILE
   integer(KIND=C_INT) :: N
   call saveobj_F(N,s2c(FILE))
 end subroutine saveobj
!-------------------------------------------------------------------------------
 function strlength(NAME)
   use ISO_C_BINDING
   implicit none
   character(len=*),intent(in) :: NAME
   real(KIND=C_FLOAT) :: strlength
   strlength=strlength_F(s2c(NAME))
 end function strlength
!-------------------------------------------------------------------------------
 subroutine boxtext(X,Y,L,H,S)
   use ISO_C_BINDING
   implicit none
   real(KIND=C_FLOAT),intent(in) :: X
   real(KIND=C_FLOAT),intent(in) :: Y
   real(KIND=C_FLOAT),intent(in) :: L
   real(KIND=C_FLOAT),intent(in) :: H
   character(len=*),intent(in)   :: S
   call boxtext_F(X,Y,L,H,s2c(S))
end subroutine boxtext
!-------------------------------------------------------------------------------
 subroutine drawstr(NAME)
   implicit none
   character(len=*),intent(in) :: NAME
   call drawstr_F(s2c(NAME))
 end subroutine drawstr
!-------------------------------------------------------------------------------
 subroutine drawchar(NAME)
! issues between int and character and character string
! make it easier to call drawstr()
! maybe when ISO_C_BINDING has been around longer this can be eliminated
! and change drawstr_F back to drawstr
   implicit none
   character(len=1),intent(in) :: NAME
   call drawstr_F(s2c(NAME))
 end subroutine drawchar
!-------------------------------------------------------------------------------
!subroutine verror(STR)
!   implicit none
!   character(len=*),intent(in) :: STR
!   call verror_F(s2c(STR))
! end subroutine verror
!-------------------------------------------------------------------------------
! so fortran 77 does not have to have TYPE matrix
!-------------------------------------------------------------------------------
 subroutine rcurve(GEOM)
    use ISO_C_BINDING
    implicit none
    type, bind(C) :: MATRIX
       real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
    end type MATRIX
    real(KIND=C_FLOAT),intent(in),dimension(4,4) :: GEOM
    type(MATRIX) :: GEOM_matrix
    GEOM_matrix%array=geom
    call rcurve_F(geom_matrix)
 end subroutine rcurve
!-------------------------------------------------------------------------------
 subroutine curvebasis(BASIS)
    use ISO_C_BINDING
    implicit none
    type, bind(C) :: MATRIX
       real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
    end type MATRIX
    real(KIND=C_FLOAT),intent(in),dimension(4,4) :: BASIS
    type(MATRIX) :: basis_matrix
    basis_matrix%array=basis
    call curvebasis_F(basis_matrix)
 end subroutine curvebasis
!-------------------------------------------------------------------------------
 subroutine patch(GEOMX,GEOMY,GEOMZ)
    use ISO_C_BINDING
    implicit none
    real(KIND=C_FLOAT),intent(in),dimension(4,4) :: GEOMX
    real(KIND=C_FLOAT),intent(in),dimension(4,4) :: GEOMY
    real(KIND=C_FLOAT),intent(in),dimension(4,4) :: GEOMZ
    type, bind(C) :: MATRIX
       real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
    end type MATRIX
    type(MATRIX) :: GEOMX_matrix
    type(MATRIX) :: GEOMY_matrix
    type(MATRIX) :: GEOMZ_matrix
    geomx_matrix%array=geomx
    geomy_matrix%array=geomy
    geomz_matrix%array=geomz
    call patch_F(geomx_matrix,geomy_matrix,geomz_matrix)
 end subroutine patch
!-------------------------------------------------------------------------------
  subroutine patchbasis(TB,UB)
    use ISO_C_BINDING
    implicit none
    type, bind(C) :: MATRIX
       real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
    end type MATRIX
    real(KIND=C_FLOAT),intent(inout),dimension(4,4) :: TB
    real(KIND=C_FLOAT),intent(inout),dimension(4,4) :: UB
    type(MATRIX) :: TB_matrix
    type(MATRIX) :: UB_matrix
    tb_matrix%array=tb
    ub_matrix%array=ub
    call patchbasis_F(tb_matrix,ub_matrix)
 end subroutine patchbasis
!-------------------------------------------------------------------------------
 subroutine getmatrix(M)
    use ISO_C_BINDING
    implicit none
    type, bind(C) :: MATRIX
       real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
    end type MATRIX
    real(KIND=C_FLOAT),intent(out),dimension(4,4) :: M
    type(MATRIX) :: M_matrix
    call getmatrix_F(m_matrix)
    M=M_matrix%array
 end subroutine getmatrix
!-------------------------------------------------------------------------------
  subroutine loadmatrix(M)
    use ISO_C_BINDING
    implicit none
    type, bind(C) :: MATRIX
       real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
    end type MATRIX
    real(KIND=C_FLOAT),intent(in),dimension(4,4) :: M
    type(MATRIX) :: M_matrix
    M_matrix%array=M
    call loadmatrix_F(m_matrix)
 end subroutine loadmatrix
!-------------------------------------------------------------------------------
  subroutine multmatrix(M)
    use ISO_C_BINDING
    implicit none
    type, bind(C) :: MATRIX
       real(KIND=C_FLOAT),dimension(4,4) :: ARRAY
    end type MATRIX
    real(KIND=C_FLOAT),intent(inout),dimension(4,4) :: M
    type(MATRIX) :: M_matrix
    M_matrix%array=M
    call multmatrix_F(m_matrix)
 end subroutine multmatrix
!-------------------------------------------------------------------------------
pure function s2c(string)  RESULT (array)
! ident_2="@(#) s2c(3fp) copy string(1 Clen(string)) to char array with null terminator"
character(len=*),intent(in)     :: string
   character(kind=C_CHAR,len=1) :: array(len_trim(string)+1)
   integer                      :: i
   do i = 1,size(array)-1
      array(i) = string(i:i)
   enddo
   array(size(array):)=achar(0)
end function s2c
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    invokeobj(3f) - [M_draw] invoke object with specified transformations
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine invokeobj(xt,yt,zt,xs,ys,zs,xr,yr,zr,iobject)
!!
!!     real,intent(in)    :: xt,yt,zt
!!     real,intent(in)    :: xs,ys,zs
!!     real,intent(in)    :: xr,yr,zr
!!     integer,intent(in) :: iobject
!!
!!##DESCRIPTION
!!    save and restore the coordinate system while invoking an object with
!!    specified translation, rotation, and scaling.
!!
!!##OPTIONS
!!    xt,yt,zt    linear transforms
!!    xs,ys,zs    scaling
!!    xr,yr,zr    rotation in degrees
!!    iobject     object to invoke
!!##EXAMPLE
!!
!!  Sample program
!!
!!    program demo_invokeobj
!!    use M_draw
!!    implicit none
!!    real :: a, angle, step
!!    integer :: i, idum
!!    ! set window size
!!       call prefsize(700,700)
!!       call prefposition( 0, 0)
!!       call vinit ('X11')
!!       a=1.0
!!    ! make an object to draw ( a disk with an arrow on it)
!!       call makeobj(12345)
!!       call polyfill(.TRUE.)
!!       call color( 5)
!!       call circle( 0.0, 0.0, a)
!!       call color( 3)
!!       call makepoly()
!!       call move2( 0.00*a, 0.80*a)
!!       call draw2( 0.50*a, 0.30*a)
!!       call draw2( 0.20*a, 0.30*a)
!!       call draw2( 0.20*a,-0.80*a)
!!       call draw2(-0.20*a,-0.80*a)
!!       call draw2(-0.20*a, 0.30*a)
!!       call draw2(-0.50*a, 0.30*a)
!!       call draw2( 0.00*a, 0.80*a)
!!       call closepoly()
!!       call polyfill(.FALSE.)
!!       call color(7)
!!       call linewidth(20)
!!       call circleprecision(200)
!!       call circle( 0.0, 0.0, a)
!!       call vflush()
!!       call closeobj()
!!    ! draw the disk invoking different rotation
!!       ANGLE=0.0
!!       STEP=0.4
!!       idum=backbuffer()
!!       idum=-1
!!       if(idum.ne.-1)then
!!          do i=1,int(360/STEP*10)
!!             idum=backbuffer()
!!             call clear()
!!             call invokeobj(0.0,0.0,0.0,1.0,1.0,1.0,ANGLE,ANGLE,ANGLE,12345)
!!             ANGLE=ANGLE+STEP
!!             call swapbuffers()
!!          enddo
!!       else
!!          ANGLE=45.0
!!          call invokeobj(0.0,0.0,0.0,1.0,1.0,1.0,ANGLE,ANGLE,ANGLE,12345)
!!          idum=getkey()
!!       endif
!!       call vexit()
!!    end program demo_invokeobj
!!##AUTHO
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine invokeobj(xt,yt,zt,xs,ys,zs,xr,yr,zr,iobject)

! ident_3="@(#) M_draw invokeobj(3f) invoke object with specified transformation applied and then restored"

real,intent(in)    :: xt,yt,zt  ! linear transforms
real,intent(in)    :: xs,ys,zs  ! scaling
real,intent(in)    :: xr,yr,zr  ! rotation
integer,intent(in) :: iobject

   call pushmatrix()
   call translate(xt,yt,zt)
   call scale(xs,ys,zs)
   call rotate(xr,'x')
   call rotate(yr,'y')
   call rotate(zr,'z')
   call callobj(iobject)
   call popmatrix()

end subroutine invokeobj
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    page(3f) - [M_draw] set window into largest viewport available
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine page(xsmall,xlarge,ysmall,ylarge)
!!    real, intent=(in) :: xsmall
!!    real, intent=(in) :: xlarge
!!    real, intent=(in) :: ysmall
!!    real, intent=(in) :: ylarge
!!
!!    subroutine page(xsize,ysize,icolor)
!!    real, intent=(in)    :: xsize
!!    real, intent=(in)    :: ysize
!!    integer, intent=(in) :: icolor
!!
!!##DESCRIPTION
!!    FORM SUBROUTINE PAGE(XSMALL,XLARGE,YSMALL,YLARGE)
!!
!!    Set the window to the rectangle defined by the corner points
!!    <xsmall,ysmall> and <xlarge,ylarge>.
!!
!!    Also, given the window size, and assuming a one-to-one correspondence
!!    of window units (ie. an "x-unit" is as long as a "y-unit"), find the
!!    largest area on the display surface that has the same aspect ratio,
!!    and set the viewport to it.
!!
!!    FORM SUBROUTINE PAGE(XSIZE,YSIZE,ICOLOR)
!!
!!    Size the window to the rectangle defined by the corner points
!!    <0.0,0.0> and <xsize,ysize> and the viewport to the largest centered
!!    area that has the same aspect ratio, and set the background color to
!!    the value mapped to color ICOLOR.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_page
!!    use M_draw
!!    use M_draw,    only  : D_BLACK,   D_WHITE
!!    use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
!!    use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
!!    implicit none
!!    integer :: ipaws
!!    real,parameter :: radius=25.0
!!       call prefsize(600,600)
!!       call vinit(' ') ! start graphics using device $M_draw_DEVICE
!!       call page(-radius, radius, -radius, radius)
!!       call linewidth(200)
!!       call clear()
!!       call color(D_BLUE)
!!       call move2(-radius, -radius)
!!       call draw2(-radius, radius)
!!       call draw2(radius, radius)
!!       call draw2(radius, -radius)
!!       call draw2(-radius, -radius)
!!       call color(D_CYAN)
!!       call circle(0.0,0.0,radius)
!!       call vflush()
!!       ipaws=getkey()
!!       call vexit()
!!    end program demo_page
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine biggest_ortho2(xsmall,xlarge,ysmall,ylarge)

! ident_4="@(#) M_draw page(3f) given a window size find and set to largest accommodating viewport"

real,intent(in)  :: xsmall
real,intent(in)  :: xlarge
real,intent(in)  :: ysmall
real,intent(in)  :: ylarge
real             :: rps
real             :: spr
real             :: tryx
real             :: tryy
real             :: vhigh
real             :: vwide
real             :: xdelta
real             :: xmax
real             :: xmin
real             :: xsplit
real             :: ydelta
real             :: ymax
real             :: ymin
real             :: ysplit
!----------------------------------------------------------------------------------------------------------------------------------!
      call getdisplaysize(vwide,vhigh) !get screen size in terms of raster units
!----------------------------------------------------------------------------------------------------------------------------------!
!     the default viewport is in "screen units", and goes from -1,-1 to 1,1
!     all new viewports are defined in terms of this original viewport, which
!     is 2 units wide and 2 units tall.
!----------------------------------------------------------------------------------------------------------------------------------!
      rps=min(vwide,vhigh)/2.0         ! number of rasters per screen unit
      spr=2.0/min(vwide,vhigh)         ! number of screen units per raster
      tryx=vwide                       ! make as wide as display as a trial fit
      if(xlarge-xsmall.ne.0.0)then
         tryy=vwide*(ylarge-ysmall)/(xlarge-xsmall) ! calculate required height
      else                             ! ERROR: do something desperate
         call journal('*page* window has a zero X dimension')
         tryy=vhigh
      endif
      if(tryy.gt.vhigh)then ! if required height too great, fit with y maximized
         tryy=vhigh
         if(ylarge-ysmall.ne.0.0)then
            tryx=vhigh*(xlarge-xsmall)/(ylarge-ysmall)
         else                          ! ERROR: do something desperate
            call journal('*page* window has a zero Y dimension')
            tryx=vwide
         endif
      endif
!----------------------------------------------------------------------------------------------------------------------------------!
!   tryx and tryy are now the required viewport in raster units. The raster
!   units now need converted to screen units to be used in viewport procedure
!
!   some explanation of physical viewport units is required:
!   assuming maximizing the required aspect ratio in the available drawing area,
!   and that the original viewport "origin" 0,0 stays in its original position,
!   and that the original -1,1,-1,1 viewport is the largest square that can fit
!   on the display, bottom left justified.
!   the screen coordinate system is a right-handed Cartesian coordinate system
!   with positive x to the viewer's right, positive y up.
!
!   at this point,
!    vwide=width in rasters of entire display
!    vhigh=height in rasters of entire display
!   assuming a square raster
!     tryx is desired width in rasters
!     tryy is desired height in rasters
!----------------------------------------------------------------------------------------------------------------------------------!
      xdelta=tryx-2.0*rps  ! need this many more rasters in x direction from 1,1
      ydelta=tryy-2.0*rps  ! need this many more rasters in y direction from 1,1
      ! to center (to left bottom justify, make xsplit and ysplit 0)
      xsplit=(vwide-tryx)/2.0
      ysplit=(vhigh-tryy)/2.0
      xmax=1+xdelta*spr+xsplit*spr
      ymax=1+ydelta*spr+ysplit*spr
      xmin=-1+xsplit*spr
      ymin=-1+ysplit*spr
!----------------------------------------------------------------------------------------------------------------------------------!
!      write(*,*)'max. display area is', vwide, ' by ',vhigh,' rasters'
!      write(*,*)'shape is ',xsmall,xlarge,ysmall,ylarge
!      write(*,*)'attempting to get a viewport of ',tryx,' by ',tryy
!      write(*,*)'needed more rasters, ',xdelta,' by ',ydelta
!      write(*,*)'resulted in viewport ',-1,xmax,-1,ymax
!----------------------------------------------------------------------------------------------------------------------------------!
      if(xmin.ne.xmax.and.ymin.ne.ymax)then
         call viewport(xmin,xmax,ymin,ymax)
      else
         call journal('*page* window has zero dimension,no viewport set')
      endif
!     to prevent clipping lines that are right on edge of window fudge a bit
      !bugx=.001*(xlarge-xsmall)
      !bugy=.001*(ylarge-ysmall)
      !xsmall1=xsmall-bugx
      !xlarge1=xlarge+bugx
      !ysmall1=ysmall-bugy
      !ylarge1=ylarge+bugy
      !call ortho2(xsmall1,xlarge1,ysmall1,ylarge1)
      if(xsmall.ne.xlarge.and.ysmall.ne.ylarge)then
         call ortho2(xsmall,xlarge,ysmall,ylarge)
      else    ! ERROR: do something desperate
         call journal('*page* window has zero dimension, no window set')
      endif
end subroutine biggest_ortho2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!       page_rri(3fp) - [M_draw] - new world window with viewport set to largest area with same aspect ratio
!!       (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine page_rri(xsize,ysize,icolor)
!!
!!    integer,intent(in)          :: xsize
!!    integer,intent(in)          :: ysize
!!    integer,intent(in)          :: icolor
!!
!!##DESCRIPTION
!!    Given a horizontal size and vertical size and color set the window to
!!    the rectangle defined by the corner points <0.0,0.0> and <xsize,ysize>
!!    and set the viewport to the largest viewport on the display with the
!!    same aspect ratio and start a new page with the specified background
!!    color if background color is supported on the device.
!!
!!##OPTIONS
!!    XSIZE    X size of requested window
!!    YSIZE    Y size of requested window
!!    ICOLOR   Color to set background.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!      program demo_page_rri
!!      use M_draw
!!      use M_drawplus, only : polyline2
!!      implicit none
!!      integer :: ipaws
!!      call prefsize(300,300)
!!      call vinit(' ')
!!      call page(8.5,11.0,3)
!!      call color(2)
!!      call linewidth(100)
!!      call circle(8.5/0.0,11.0/0.0,8.5/2.0)
!!      call color(1)
!!      call polyline2([0.0,0,0,8.5,11.0]
!!      call polyline2([8.5,0,0,0.0,11.0]
!!      ipaws=getkey()
!!      call vexit()
!!      end
!!      program demo_page_rri
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine journal(string)
character(len=*),intent(in) :: string
write(*,'(a)')string
end subroutine journal
subroutine page_rri(xsize,ysize,icolor)
real,intent(in)             :: xsize
real,intent(in)             :: ysize
integer                     :: icolor
   call page(0.0,xsize,0.0,ysize)
   call pushattributes()
      call color(icolor)
      call clear()
   call popattributes()
end subroutine page_rri
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    pop(3f) - [M_draw] call popviewport(), popmatrix(), popattributes()
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine pop()
!!##DESCRIPTION
!!    call popviewport(), popmatrix(), popattributes()
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine pop()

! ident_5="@(#) M_draw pop(3f) call popviewport() popmatrix() popattributes()"

   call popviewport()
   call popmatrix()
   call popattributes()
end subroutine pop
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    print(3f) - [M_draw] call DRAWSTR(3f) with up to nine arbitrary intrinsic scalar values
!!
!!##SYNOPSIS
!!
!!    subroutine print(g1, g2, g3, g4, g5, g6, g7, g8, g9)
!!
!!    class(*),intent(in),optional :: g1 ,g2 ,g3 ,g4 ,g5, g6 ,g7 ,g8 ,g9
!!
!!##DESCRIPTION
!!    Call DRAWSTR(3f) with up to nine intrinsic scalar values. They will be composed into a
!!    string with spaces between the argument values. Trailing spaces of strings
!!    are trimmed unless the entire variable is blank.
!!##OPTIONS
!!    g1-g9  intrinsic scalar values to plot at the current point.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_print
!!    use M_draw
!!    implicit none
!!    real :: angle
!!    integer :: idum
!!    character(len=*),parameter :: space='       '
!!       ! set window size
!!       call prefsize(700,700)
!!       call prefposition( 0, 0)
!!       call vinit('X11')
!!       call page(-5.0,5.0,-5.0,5.0)
!!       call textsize(0.3,0.3)
!!       call color(D_BLUE)
!!       angle=0.0
!!       call turn()
!!       call print(space,'a logical such as ',.true.)
!!       call turn()
!!       call print(space,'a real value',3.1416)
!!       call turn()
!!       call print(space,'double precision',7890.123456d0)
!!       call turn()
!!       call print(space,'integer ',1234)
!!       call turn()
!!       call print(space,'lots of stuff',1234,.false.,cmplx(20.0,30.0))
!!       idum=getkey()
!!       call vexit()
!!    contains
!!       subroutine turn()
!!          call move2(-4.0,-3.5)
!!          call textang(angle)
!!          angle=angle+15.0
!!       end subroutine turn
!!    end program demo_print
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine print(g1, g2, g3, g4, g5, g6, g7, g8, g9)
class(*),intent(in),optional :: g1 ,g2 ,g3 ,g4 ,g5, g6 ,g7 ,g8 ,g9
   call drawstr(str(g1, g2, g3, g4, g5, g6, g7, g8, g9))
end subroutine print
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function str(generic0, generic1, generic2, generic3, generic4, generic5, generic6, generic7, generic8, generic9, &
                  & generica, genericb, genericc, genericd, generice, genericf, genericg, generich, generici, genericj, &
                  & sep)
implicit none

! ident_2="@(#)M_draw::str(3fp): writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional  :: generic0, generic1, generic2, generic3, generic4
class(*),intent(in),optional  :: generic5, generic6, generic7, generic8, generic9
class(*),intent(in),optional  :: generica, genericb, genericc, genericd, generice
class(*),intent(in),optional  :: genericf, genericg, generich, generici, genericj
character(len=:),allocatable  :: str
character(len=4096)           :: line
integer                       :: istart
integer                       :: increment
character(len=*),intent(in),optional :: sep
character(len=:),allocatable  :: sep_local
   if(present(sep))then
      increment=len(sep)+1
      sep_local=sep
   else
      increment=2
      sep_local=' '
   endif

   istart=1
   line=''
   if(present(generic0))call print_generic(generic0)
   if(present(generic1))call print_generic(generic1)
   if(present(generic2))call print_generic(generic2)
   if(present(generic3))call print_generic(generic3)
   if(present(generic4))call print_generic(generic4)
   if(present(generic5))call print_generic(generic5)
   if(present(generic6))call print_generic(generic6)
   if(present(generic7))call print_generic(generic7)
   if(present(generic8))call print_generic(generic8)
   if(present(generic9))call print_generic(generic9)
   if(present(generica))call print_generic(generica)
   if(present(genericb))call print_generic(genericb)
   if(present(genericc))call print_generic(genericc)
   if(present(genericd))call print_generic(genericd)
   if(present(generice))call print_generic(generice)
   if(present(genericf))call print_generic(genericf)
   if(present(genericg))call print_generic(genericg)
   if(present(generich))call print_generic(generich)
   if(present(generici))call print_generic(generici)
   if(present(genericj))call print_generic(genericj)
   str=trim(line)
contains

subroutine print_generic(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      type is (logical);                write(line(istart:),'(l1)') generic
      type is (character(len=*));       write(line(istart:),'(a)') trim(generic)
      type is (complex);                write(line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(line)+increment
   line=trim(line)//sep_local
end subroutine print_generic

end function str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!>
!!##NAME
!!    push(3f) - [M_draw] call pushviewport(), pushmatrix(), pushattributes()
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     subroutine push()
!!##DESCRIPTION
!!    call pushattributes(), pushmatrix(), pushviewport()
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine push()

! ident_6="@(#) M_draw push(3f) call pushattributes() pushmatrix() pushviewport()"

   call pushattributes()
   call pushmatrix()
   call pushviewport()
end subroutine push
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_draw
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
! drawchar -- int versus char
! textjustify -- UNSIGNED?
! pdraw and pmove in sunfort? spoint2? srect? getfontwidth? getfontheight?  getprefposandsize?
! getplanes? drawchar?
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
