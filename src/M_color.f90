!>
!! <dl>
!! <!-- ======================================================================= -->
!! <dt> <a name="M_COLOR">NAME</a></dt> <dd>
!! <!--
!! <em>M_color(3)</em>&nbsp;-&nbsp;[M_color]&nbsp;a&nbsp;Fortran&nbsp;module&nbsp;that&nbsp;lets&nbsp;you&nbsp;convert&nbsp;between&nbsp;common&nbsp;color&nbsp;models
!! -->
!! <em>M_color(3)</em> - [M_color] a Fortran module that lets you convert between common color models
!! (LICENSE:PD)
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> SYNOPSIS </dt><dd>
!! <pre>
!!
!!    use M_color, only : &amp;
!!
!!       &amp; <a href="hue.3m_color.html">hue</a>, &amp;
!!       &amp; <a href="closest_color_name.3m_color.html">closest_color_name</a>, &amp;
!!       &amp; <a href="color_name2rgb.3m_color.html">color_name2rgb</a>, &amp;
!!       &amp; <a href="rgbmono.3m_color.html">rgbmono</a>
!! </pre>
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> DESCRIPTION</dt>  <dd>
!!
!! <h1> <img src="images/swirl.gif" height="140" width="140" > Fortran color module M_color</h1>
!!
!! <p>
!!   Highly accurate color conversions are a tricky business, and color
!!   is a complex topic; but these simplified conversions between common
!!   color models work quite well for basic conversions.
!! </p>
!!
!! <p>
!!    Typically the only user routine called is
!!    <a href="hue.3m_color.html">HUE(3f)</a>.
!!    HUE(3f) is a single routine that interfaces to all the private
!!    low-level color conversion routines to convert a color's components
!!    from one color model to another. HUE(3f) converts between the
!!    following color models:
!! </p>
!!
!! <ul>
!!    <li>  RGB - Red, Green, Blue (color TV monitors)
!!    <li>  HLS - Hue, Lightness, Saturation
!!    <li>  CMY - Cyan, Magenta, Yellow (pigment-based printing devices)
!!    <li>  HSV - Hue, Saturation, Value
!!    <li>  YIQ - Broadcast TV color system
!! </ul>
!!
!! <p>
!!   In addition to the reversible color model conversions there are a few
!!   other user-callable color-related procedures:
!! </p>
!!
!! <a href="closest_color_name.3m_color.html">CLOSEST_COLOR_NAME</a>:&nbsp;&nbsp;given RGB values, try to find closest named color
!!    <br/>
!! <a href="color_name2rgb.3m_color.html">COLOR_NAME2RGB</a>:&nbsp;&nbsp;given a color name, return RGB color values in range 0 to 100
!!    <br/>
!! <a href="rgbmono.3m_color.html">RGBMONO</a>:&nbsp;&nbsp;convert RGB colors to a reasonable grayscale
!!
!! <h3> 2*N Design of the module</h3>
!!
!! <p>
!!    The rest of the library is composed of PRIVATE procedures.
!!    For each color model supported the general idea of the module is
!!    that there are two routines for each color model:
!! </p>
!!
!! <ul>
!!    <li> One converts that model to the RGB model   </li>
!!    <li> The other converts from RGB to that model   </li>
!! </ul>
!!
!! <p>
!!    This allows conversions between all color models with only 2*N
!!    routines. That is, to go from model A to model B the module would
!!    internally make two calls:
!! </p>
!!
!! <pre>
!!     call modelA2rgb(...)
!!     call rgb2modelB(...)
!! </pre>
!!
!! <p>
!!    The resulting internal routines that result are:
!! </p>
!!
!! <ul>
!!    <li><a href="#HLSRGB">HLSRGB</a> given hue, lightness, saturation calculate red, green, and blue components
!!       <ul>
!!          <li><a href="#RGBVAL">RGBVAL</a> ensure a value is in the appropriate range and quadrant
!!          </li>
!!       </ul>
!!    </li>
!!    <li><a href="#HVSRGB">HVSRGB</a> given hue, saturation, value calculate red, green, and blue components
!!    </li>
!!    <li><a href="#CMYRGB">CMYRGB</a> given cyan, magenta, and yellow components calculate red, green, and blue components
!!    </li>
!!    <li><a href="#YIQRGB">YIQRGB</a> given luma(gray scale), orange-blue chrominance, and  purple-green chrominance
!!                                     components calculate red, green, and blue components
!!    </li>
!!
!!    <li><a href="#RGBHVS">RGBHVS</a> given red, green, blue values calculate hue, value, and saturation components
!!    </li>
!!    <li><a href="#RGBHLS">RGBHLS</a> given red, green, blue values calculate hue, lightness, and saturation components
!!    </li>
!!    <li><a href="#RGBCMY">RGBCMY</a> given red, green, blue values calculate cyan, magenta, yellow components
!!    </li>
!!    <li><a href="#RGBYIQ">RGBYIQ</a> given red, green, blue values calculate luma(gray scale), orange-blue chrominance,
!!                                     and purple-green chrominance components
!!    </li>
!!
!! </ul>
!! </dd>
!! <dt>EXAMPLE</dt> <dd>
!! <p>
!!    Sample program:
!! </p>
!! <xmp>
!!       program demo_M_color
!!       use M_color, only : hue
!!       use M_color, only : closest_color_name
!!       use M_color, only : color_name2rgb
!!       use M_color, only : rgbmono
!!       implicit none
!!       character(len=100) :: string ! at least 20 characters
!!       character(len=20)  :: name
!!       character(len=20)  :: echoname
!!       real               :: red,green,blue
!!       real               :: gray
!!       integer            :: ierr
!!          ! find the names of colors given RGB values
!!          write(*,*)'Find names given values'
!!
!!          call closest_color_name( 100.0,   0.0,   0.0, string)
!!          write(*,*)trim(string)
!!
!!          call closest_color_name(   0.0, 100.0,   0.0, string)
!!          write(*,*)trim(string)
!!
!!          call closest_color_name(   0.0,   0.0, 100.0, string)
!!          write(*,*)trim(string)
!!
!!          ! list colors known to colorname2rgb(3f) & corresponding RGB values
!!          write(*,*)'given names find RGB values'
!!          ! get the RGB values and English name of the color
!!          call color_name2rgb('RED',red,green,blue,echoname)
!!          ! display the English name and RGB values for the name
!!          write(*,*)echoname,int([red,green,blue])
!!
!!          write(*,*)'Do some conversions between RGB, HLS, and HLS'
!!          write(*,*)'and check them against expected values'
!!          !               NAME        RGB(0-255)            HLS(0-100)
!!          call chk('hls','red',     [100, 0,   0  ], [0,   50,  100])
!!          call chk('hls','orange',  [100, 65,  0  ], [39,  50,  100])
!!          call chk('hls','yellow',  [100, 100, 0  ], [60,  50,  100])
!!          call chk('hls','green',   [0,   100, 0  ], [120, 50,  100])
!!          call chk('hls','cyan',    [0,   100, 100], [180, 50,  100])
!!          call chk('hls','blue',    [0,   0,   100], [240, 50,  100])
!!          call chk('hls','magenta', [100, 0,   100], [300, 50,  100])
!!          call chk('hls','black',   [0,   0,   0  ], [0,   0,   0  ])
!!          call chk('hls','white',   [100, 100, 100], [0,   100, 0  ])
!!          !               NAME        RGB(0-255)           HSV(0-100)
!!          call chk('hsv','red',     [100, 0,   0  ], [0,   100, 100])
!!          call chk('hsv','yellow',  [100, 100, 0  ], [60,  100, 100])
!!          call chk('hsv','green',   [0,   100, 0  ], [120, 100, 100])
!!          call chk('hsv','cyan',    [0,   100, 100], [180, 100, 100])
!!          call chk('hsv','blue',    [0,   0,   100], [240, 100, 100])
!!          call chk('hsv','magenta', [100, 0,   100], [300, 100, 100])
!!          call chk('hsv','white',   [100, 100, 100], [0,   0,   100])
!!          call chk('hsv','black',   [0,   0,   0  ], [0,   0,   0  ])
!!          call chk('hsv','gray50',  [50,  50,  50 ], [0,   0,   50 ])
!!          call chk('hsv','silver',  [75,  75,  75 ], [0,   0,   75 ])
!!          call chk('hsv','red4',    [55,  0,   0  ], [0,   100, 55 ])
!!          call chk('hsv','olive',   [50,  50,  0  ], [60,  100, 50 ])
!!          call chk('hsv','lime',    [0,   100, 0  ], [120, 100, 100])
!!          call chk('hsv','teal',    [0,   50,  50 ], [180, 100, 50 ])
!!          call chk('hsv','navy',    [0,   0,   50 ], [240, 100, 50 ])
!!          call chk('hsv','purple',  [63,  13,  94 ], [277, 87,  94 ])
!!          call chk('hsv','magenta4',[55,  0,   55 ], [300, 100, 55 ])
!!          call chk('hsv','maroon',  [69,  19,  38 ], [338, 73,  69 ])
!!
!!          write(*,*)'Get some grayscale values from RGB color values'
!!          call rgbmono(100.0,  0.0,  0.0,gray,ierr); write(*,*)'red     ',gray
!!          call rgbmono(  0.0,100.0,  0.0,gray,ierr); write(*,*)'green   ',gray
!!          call rgbmono(  0.0,  0.0,100.0,gray,ierr); write(*,*)'blue    ',gray
!!          call rgbmono(100.0,100.0,  0.0,gray,ierr); write(*,*)'Yellow  ',gray
!!          call rgbmono(  0.0,100.0,100.0,gray,ierr); write(*,*)'Cyan    ',gray
!!          call rgbmono(100.0,  0.0,100.0,gray,ierr); write(*,*)'Magenta ',gray
!!          call rgbmono(100.0,100.0,100.0,gray,ierr); write(*,*)'White   ',gray
!!          call rgbmono( 00.0,  0.0,  0.0,gray,ierr); write(*,*)'Black   ',gray
!!          call rgbmono( 50.0,  0.0,  0.0,gray,ierr); write(*,*)'Maroon  ',gray
!!          call rgbmono(100.0, 50.0, 50.0,gray,ierr); write(*,*)'Pink    ',gray
!!          contains
!!          subroutine chk(modelout,name,rgb,other)
!!          ! given a color convert to MODELOUT and compare to expected values
!!          character(len=*),intent(in)   :: name
!!          integer,intent(in)            :: rgb(3), other(3)
!!          character(len=*),intent(in)   :: modelout
!!          real                          :: val1,val2,val3
!!          integer                       :: status
!!             ! convert RGB values to MODELOUT values
!!             call hue('rgb',REAL(rgb(1)),REAL(rgb(2)),REAL(rgb(3)),&
!!             & modelout,val1,val2,val3,status)
!!             ! left-justify name to 10 characters or more
!!             write(*,'(a,1x)',advance='no') &
!!             & [ character(len=max(10,len_trim(name))) ::' '//trim(name)]
!!             write(*,'(a,1x,3(i3,1x))',advance='no') &
!!             & modelout//' EXPECTED',other
!!             write(*,'(a,1x,3(i3,1x))',advance='no') &
!!             & 'GOT',int([val1+0.5,val2+0.5,val3+0.5])
!!             write(*,'(a,i0)')'STATUS ',status
!!          end subroutine chk
!!       end program demo_M_color
!! </xmp>
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> SEE ALSO </dt> <dd>
!!
!! <p>
!!    A simple interactive javascript-based
!!    <a href="../../../public_html/javascript/color/iframe.html"> color selector</a>
!!    lets you interactively select colors.
!! </p>
!!
!! <p>
!!    The color wheel below was generated using a
!!    <a href="BOOK_M_draw.html"> M_draw(3f) </a>
!!    graphics library program
!!    <a href="../test/demos/huegif.f90">(huegif.f90)</a>
!!    that uses the M_color module.
!! </p>
!!
!! <center>
!!    <img alt="HLS circle" src="images/hls.gif" width="50%" height="" />
!! </center>
!!
!! </dd>
!! <!-- ======================================================================= -->
!! <dt>REFERENCES</dt><dd>
!!   The algorithms are based on chapter 17 of
!!   "Fundamentals of Interactive Computer Graphics"; J. D. Foley and A. Van Dam.
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> AUTHOR </dt><dd>
!!
!!    John S. Urban
!!
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> LICENSE </dt><dd>
!!
!!    Public Domain
!!
!! </dd>
!! <!-- ======================================================================= -->
!! </dl>
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_color
implicit none
! ident_1="@(#)M_color::color(3f): convert between common color models"
private

public hue                  ! converts a color's components from one color model to another
public closest_color_name   ! given RGB values, try to find closest named color
public color_name2rgb       ! given a color name, return rgb color values in range 0 to 100
public rgbmono              ! convert RGB colors to a reasonable grayscale
!----------------------------
private hlsrgb              ! convert HLS(hue, lightness, saturation) values to RGB (red, green, blue) components
private hvsrgb              ! given hue, saturation, value calculate red, green, & blue components
private cmyrgb              ! given cyan,magenta, and yellow calculate red,green,blue components
!----------------------------
private rgbhls              ! given red,green,blue calculate hue,lightness, and saturation components
private rgbhvs              ! given red, green, blue calculate hue, saturation and value components
private rgbcmy              ! given red,green,blue calculate cyan,magenta, and yellow components
private rgbyiq              ! given RGB calculate luma, orange-blue chrominance, and  purple-green chrominance
!----------------------------
private rgbval              ! internal routine to ensure a value is in the appropriate range and quadrant

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!! <dl>
!! <dt> <a name="HUE">NAME</a> </dt><dd>
!!    HUE(3f) - [M_color] converts a color's components from one color model
!!    to another.
!!    (LICENSE:PD)
!! </dd>
!!
!! <dt> SYNOPSIS </dt><dd>
!! <pre>
!! subroutine hue(modei,clr1i,clr2i,clr3i,modeo,clr1o,clr2o,clr3o,status)
!!
!!  character(len=*),intent(in) :: modei  ! color model of input values
!!  character(len=*),intent(in) :: modeo  ! color model of output values
!!  real,intent(in)             :: clr1i,clr2i,clr3i
!!  real,intent(out)            :: clr1o,clr2o,clr3o
!!  integer,intent(out)         :: status
!! </pre>
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> DESCRIPTION  </dt><dd>
!! </dd>
!! <dt> Basic color models ..</dt><dd>
!! <p>
!! Valid values for modei and modeo as well as the corresponding
!! meanings for clr1*, clr2*, and clr3* are:
!!
!! <table border="1">
!! <tr><th>model</th><th> clr1 </th><th> clr2      </th><th> clr3      </th></tr>
!! <tr><td>hls  </td><td> hue  </td><td> lightness </td><td> saturation</td></tr>
!! <tr><td>hsl  </td><td> hue  </td><td> saturation</td><td> lightness </td></tr>
!! <tr><td>hvs  </td><td> hue  </td><td> value     </td><td> saturation</td></tr>
!! <tr><td>hsv  </td><td> hue  </td><td> saturation</td><td> value     </td></tr>
!! <tr><td>rgb  </td><td> red  </td><td> green     </td><td> blue      </td></tr>
!! <tr><td>cmy  </td><td> cyan </td><td> magenta   </td><td> yellow    </td></tr>
!! <tr><td>yiq  </td><td> luma</br> grayscale </td><td> orange-blue</br>chrominance </td><td> purple-green</br>chrominance </td></tr>
!! </table>
!! </p>
!!
!! <ul>
!!    <li>  lightness, value, saturation, red, green, blue, cyan, magenta, and yellow range from 0 to 100,
!!    <li>  hue ranges from 0 to 360 degrees,
!!    <li>  y   ranges from   0 to 100,
!!    <li>  i   ranges from -60 to 60,
!!    <li>  q   ranges from -52 to 52
!! </ul>
!!
!! <p>
!!    The STATUS variable can signal the following conditions:
!! </p>
!!
!! <pre>
!!
!!    -1   modei = modeo, so no substantial conversion was done,
!!     1   one of the input color values was outside the allowable range,
!!     2   modei was invalid
!!     3   modeo was invalid
!! </pre>
!! <h2>EXAMPLE</h2>
!! <pre>
!!
!!   Sample program
!!
!!    program demo_hue
!!    use M_color, only : hue
!!    implicit none
!!       !               NAME        RGB(0-255)            HLS(0-100)
!!       call chk('hls','red',     [100, 0,   0  ], [0,   50,  100])
!!       call chk('hls','orange',  [100, 65,  0  ], [39,  50,  100])
!!       call chk('hls','yellow',  [100, 100, 0  ], [60,  50,  100])
!!       call chk('hls','green',   [0,   100, 0  ], [120, 50,  100])
!!       call chk('hls','cyan',    [0,   100, 100], [180, 50,  100])
!!       call chk('hls','blue',    [0,   0,   100], [240, 50,  100])
!!       call chk('hls','magenta', [100, 0,   100], [300, 50,  100])
!!       call chk('hls','black',   [0,   0,   0  ], [0,   0,   0  ])
!!       call chk('hls','white',   [100, 100, 100], [0,   100, 0  ])
!!       !               NAME        RGB(0-255)           HSV(0-100)
!!       call chk('hsv','red',     [100, 0,   0  ], [0,   100, 100])
!!       call chk('hsv','yellow',  [100, 100, 0  ], [60,  100, 100])
!!       call chk('hsv','green',   [0,   100, 0  ], [120, 100, 100])
!!       call chk('hsv','cyan',    [0,   100, 100], [180, 100, 100])
!!       call chk('hsv','blue',    [0,   0,   100], [240, 100, 100])
!!       call chk('hsv','magenta', [100, 0,   100], [300, 100, 100])
!!       call chk('hsv','black',   [0,   0,   0  ], [0,   0,   0  ])
!!       call chk('hsv','white',   [100, 100, 100], [0,   0,   100])
!!
!!       call chk('hsv','gray50',  [50,  50,  50 ], [0,   0,   50 ])
!!       call chk('hsv','silver',  [75,  75,  75 ], [0,   0,   75 ])
!!       call chk('hsv','red4',    [55,  0,   0  ], [0,   100, 55 ])
!!       call chk('hsv','olive',   [50,  50,  0  ], [60,  100, 50 ])
!!       call chk('hsv','lime',    [0,   100, 0  ], [120, 100, 100])
!!       call chk('hsv','teal',    [0,   50,  50 ], [180, 100, 50 ])
!!       call chk('hsv','navy',    [0,   0,   50 ], [240, 100, 50 ])
!!       call chk('hsv','purple',  [63,  13,  94 ], [277, 87,  94 ])
!!       call chk('hsv','magenta4',[55,  0,   55 ], [300, 100, 55 ])
!!       call chk('hsv','maroon',  [69,  19,  38 ], [338, 73,  69 ])
!!    contains
!!    subroutine chk(modelout,name,rgb,other)
!!    ! given a color convert to MODELOUT and compare to expected values
!!    character(len=*),intent(in)   :: name
!!    integer,intent(in)            :: rgb(3), other(3)
!!    character(len=*),intent(in)   :: modelout
!!       real                       :: val1,val2,val3
!!       integer                    :: status
!!       ! convert RGB values to MODELOUT values
!!       call hue('rgb',REAL(rgb(1)),REAL(rgb(2)),REAL(rgb(3)),&
!!       & modelout,val1,val2,val3,status)
!!          ! left-justify name to 10 characters or more
!!          write(*,'(a,1x)',advance='no') &
!!          & [ character(len=max(10,len_trim(name))) ::' '//trim(name)]
!!          write(*,'(a,1x,3(i3,1x))',advance='no') &
!!          & modelout//' EXPECTED',other
!!          write(*,'(a,1x,3(i3,1x))',advance='no') &
!!          & 'GOT',int([val1+0.5,val2+0.5,val3+0.5])
!!          write(*,'(a,i0)')'STATUS ',status
!!    end subroutine chk
!!    end program demo_hue
!!
!!   Results:
!!
!!     red       hls EXPECTED   0  50 100 GOT   0  50 100 STATUS 0
!!     orange    hls EXPECTED  39  50 100 GOT  39  50 100 STATUS 0
!!     yellow    hls EXPECTED  60  50 100 GOT  60  50 100 STATUS 0
!!     green     hls EXPECTED 120  50 100 GOT 120  50 100 STATUS 0
!!     cyan      hls EXPECTED 180  50 100 GOT 180  50 100 STATUS 0
!!     blue      hls EXPECTED 240  50 100 GOT 240  50 100 STATUS 0
!!     magenta   hls EXPECTED 300  50 100 GOT 300  50 100 STATUS 0
!!     black     hls EXPECTED   0   0   0 GOT   0   0   0 STATUS 0
!!     white     hls EXPECTED   0 100   0 GOT   0 100   0 STATUS 0
!!     black     hsv EXPECTED   0   0   0 GOT   0   0   0 STATUS 0
!!     gray50    hsv EXPECTED   0   0  50 GOT   0   0  50 STATUS 0
!!     silver    hsv EXPECTED   0   0  75 GOT   0   0  75 STATUS 0
!!     white     hsv EXPECTED   0   0 100 GOT   0   0 100 STATUS 0
!!     red4      hsv EXPECTED   0 100  55 GOT   0 100  55 STATUS 0
!!     red       hsv EXPECTED   0 100 100 GOT   0 100 100 STATUS 0
!!     olive     hsv EXPECTED  60 100  50 GOT  60 100  50 STATUS 0
!!     yellow    hsv EXPECTED  60 100 100 GOT  60 100 100 STATUS 0
!!     green     hsv EXPECTED 120 100 100 GOT 120 100 100 STATUS 0
!!     lime      hsv EXPECTED 120 100 100 GOT 120 100 100 STATUS 0
!!     teal      hsv EXPECTED 180 100  50 GOT 180 100  50 STATUS 0
!!     cyan      hsv EXPECTED 180 100 100 GOT 180 100 100 STATUS 0
!!     navy      hsv EXPECTED 240 100  50 GOT 240 100  50 STATUS 0
!!     blue      hsv EXPECTED 240 100 100 GOT 240 100 100 STATUS 0
!!     purple    hsv EXPECTED 277  87  94 GOT 277  86  94 STATUS 0
!!     magenta4  hsv EXPECTED 300 100  55 GOT 300 100  55 STATUS 0
!!     magenta   hsv EXPECTED 300 100 100 GOT 300 100 100 STATUS 0
!!     maroon    hsv EXPECTED 338  73  69 GOT 337  72  69 STATUS 0
!! </pre>
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> AUTHOR </dt><dd>
!!
!!    John S. Urban
!!
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> LICENSE </dt><dd>
!!
!!    Public Domain
!!
!! </dd>
!! <!-- ======================================================================= -->
!! </dl>
subroutine hue(modei,clr1i,clr2i,clr3i,modeo,clr1o,clr2o,clr3o,status)

! ident_2="@(#)M_color::hue(3f): convert color components from one color model to another"

!---- modei specifies the color model that applies to the input color components  clr1i, clr2i, & clr3i.
!---- modeo specifies the color model desired for the output color components  clr1o, clr2o, & clr3o.
!
!---- valid values for modei and modeo as well as the corresponding
!     meanings for clr1_, clr2_, and clr3_  are as shown below:
!     . | mode  | clr1             | clr2                   | clr3
!     . | ----- | ---------------- |------------------------|------------------------
!     . | 'hls' | hue              | lightness              | saturation
!     . | 'hsl' | hue              | saturation             | lightness
!     . | 'hvs' | hue              | value                  | saturation
!     . | 'hsv' | hue              | saturation             | value
!     . | 'rgb' | red              | green                  | blue
!     . | 'cmy' | cyan             | magenta                | yellow
!     . | 'yiq' | luma(gray-scale) | orange-blue chrominance| purple-green chrominance
!
!---- lightness, value, saturation, red, green, blue, cyan, magenta, yellow & y range from 0 to 100,
!     hue ranges from 0 to 360 degrees,
!     i   ranges from -60 to 60,
!     q   ranges from -52 to 52
!
!---- at a minimum, this procedure equates the output color values to the input color values.
!
!---- status signals the following conditions:
!     .  -1   modei = modeo, so no substantial conversion was done,
!     .   1   one of the input color values was outside the allowable range,
!     .   2   modei was invalid
!     .   3   modeo was invalid
!     . 999   unknown error
!
character(len=*),intent(in) :: modei
real,intent(in)             :: clr1i,clr2i,clr3i
character(len=*),intent(in) :: modeo
real,intent(out)            :: clr1o,clr2o,clr3o
integer,intent(out)         :: status

character(len=3)            :: input_color_model,output_color_model
real                        :: c1, c2, c3, r, g, b
!-----------------------------------------------------------------------------------------------------------------------------------
!-- initialize the status flag.
   status=0
!-- set the output colors equal to invalid values
   clr1o=-99999.0
   clr2o=-99999.0
   clr3o=-99999.0
!-- ensure that the input character strings are lowercase
   input_color_model=lower(modei)
   output_color_model=lower(modeo)
!-----------------------------------------------------------------------------------------------------------------------------------
!-- check for a trivial instance where the input and output model names are the same
   if(input_color_model .eq. output_color_model) then
      clr1o=clr1i
      clr2o=clr2i
      clr3o=clr3i
      status=-1
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!-- check for a transpose of terms, another trivial instance.
   SELECT CASE (input_color_model)
   CASE ('hls','hsl','hvs','hsv')
      if( input_color_model.eq.'hls' .and. output_color_model.eq.'hsl'   &
    & .or.input_color_model.eq.'hsl' .and. output_color_model.eq.'hls'   &
    & .or.input_color_model.eq.'hvs' .and. output_color_model.eq.'hsv'   &
    & .or.input_color_model.eq.'hsv' .and. output_color_model.eq.'hvs') then
         clr1o=clr1i
         clr2o=clr3i
         clr3o=clr2i
         status=-1
         return
      endif
   END SELECT
!-----------------------------------------------------------------------------------------------------------------------------------
!-- assign new variables so that the input arguments can't possibly be changed by subsequent procedures.
   c1=clr1i
   c2=clr2i
   c3=clr3i
!-----------------------------------------------------------------------------------------------------------------------------------
!-- first, convert input values to rgb values.
   SELECT CASE (input_color_model)
   CASE ('hls'); call hlsrgb(c1,c2,c3,r,g,b,status)
   CASE ('hvs'); call hvsrgb(c1,c2,c3,r,g,b,status)
   CASE ('hsl'); call hlsrgb(c1,c3,c2,r,g,b,status)
   CASE ('hsv'); call hvsrgb(c1,c3,c2,r,g,b,status)
   CASE ('cmy'); call cmyrgb(c1,c2,c3,r,g,b,status)
   CASE ('yiq'); call yiqrgb(c1,c2,c3,r,g,b,status)
   CASE ('rgb'); r=c1;g=c2;b=c3
   CASE DEFAULT ! unknown input model name
      status=2
      return
   END SELECT
!-----------------------------------------------------------------------------------------------------------------------------------
   if(status .ne. 0 )then
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!-- then convert from RGB to the desired output values
!
   SELECT CASE (output_color_model)
   CASE ('hls'); call rgbhls(r,g,b,clr1o,clr2o,clr3o,status)
   CASE ('hsl'); call rgbhls(r,g,b,clr1o,clr3o,clr2o,status)
   CASE ('hvs'); call rgbhvs(r,g,b,clr1o,clr2o,clr3o,status)
   CASE ('hsv'); call rgbhvs(r,g,b,clr1o,clr3o,clr2o,status)
   CASE ('cmy'); call rgbcmy(r,g,b,clr1o,clr2o,clr3o,status)
   CASE ('rgb'); clr1o=r; clr2o=g; clr3o=b
   CASE ('yiq'); call rgbyiq(r,g,b,clr1o,clr2o,clr3o,status)
   CASE DEFAULT ! unknown output model name
      status=3
      return
   END SELECT
!-----------------------------------------------------------------------------------------------------------------------------------
   if(status .ne. 0 )then
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine hue
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!! <dl>
!! <!-- ======================================================================= -->
!! <dt> <a name="RGBHLS">NAME</a> </dt><dd>
!!     RGBHLS(3fp) - [M_color] Given red, green, and blue color components
!!     calculates the hue, lightness, and saturation for a color
!!     (LICENSE:PD)
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> SYNOPSIS  </dt><dd>
!!    <pre>
!!
!!    subroutine rgbhls(r,g,b,h,l,s,status)
!!
!!     ! red component as a value of 0 to 100
!!     real, intent(in)  :: r
!!     ! green component as a value of 0 to 100
!!     real, intent(in)  :: g
!!     ! blue component as a value of 0 to 100
!!     real, intent(in)  :: b
!!     ! hue value in the range of 0 to 360 degrees
!!     real, intent(out) :: h
!!     ! lightness as a percent value from 0 to 100
!!     real, intent(out) :: l
!!     ! saturation as a percent from 0 to 100
!!     real, intent(out) :: s
!!     integer           :: status
!!    </pre>
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> DESCRIPTION  </dt><dd>
!!
!! RGB values are in the range 0-100; hue is 0-360 degrees; lightness
!! and saturation have a range of 0-100.
!! <br/><br/>
!!
!! <blockquote>
!! <table border="1" >
!! <tr><TH>Color</TH><TH COLSPAN="3">RGB</TH><TH COLSPAN="3">HLS</TH><TH>Sample</TH></tr>
!! <tr ALIGN="right"><td ALIGN="left">Red</td><td WIDTH="30">100.0</td><td WIDTH="30">0.0</td><td WIDTH="30">0.0</td><td WIDTH="30">0</td><td WIDTH="30">50.0</td><td WIDTH="30">100.0</td><td style="background-color:#FF0000">&nbsp;</td></tr>
!! <tr ALIGN="right"><td ALIGN="left">Yellow</td><td>100.0</td><td>100.0</td><td>0.0</td><td>60</td><td>50.0</td><td>100.0</td><td style="background-color:#FFFF00">&nbsp;</td></tr>
!! <tr ALIGN="right"><td ALIGN="left">Green</td><td>0.0</td><td>100.0</td><td>0.0</td><td>120</td><td>50.0</td><td>100.0</td><td style="background-color:#00FF00">&nbsp;</td></tr>
!! <tr ALIGN="right"><td ALIGN="left">Cyan</td><td>0.0</td><td>100.0</td><td>100.0</td><td>180</td><td>50.0</td><td>100.0</td><td style="background-color:#00FFFF">&nbsp;</td></tr>
!! <tr ALIGN="right"><td ALIGN="left">Blue</td><td>0.0</td><td>0.0</td><td>100.0</td><td>240</td><td>50.0</td><td>100.0</td><td style="background-color:#0000FF">&nbsp;</td></tr>
!! <tr ALIGN="right"><td ALIGN="left">Magenta</td><td>100.0</td><td>0.0</td><td>100.0</td><td>300</td><td>50.0</td><td>100.0</td><td style="background-color:#FF00FF">&nbsp;</td></tr>
!! <tr ALIGN="right"><td ALIGN="left">White</td><td>100.0</td><td>100.0</td><td>100.0</td><td>(any)</td><td>100.0</td><td>(any)</td><td style="background-color:#FFFFFF">&nbsp;</td></tr>
!! <tr ALIGN="right"><td ALIGN="left">Black</td><td>0.0</td><td>0.0</td><td>0.0</td><td>(any)</td><td>0.0</td><td>(any)</td><td style="background-color:#000000">&nbsp;</td></tr>
!! <tr ALIGN="right"><td ALIGN="left">Maroon</td><td>50.0</td><td>0.0</td><td>0.0</td><td>0</td><td>25.0</td><td>100.0</td><td style="background-color:#800000">&nbsp;</td></tr>
!! <tr ALIGN="right"><td ALIGN="left">Pink</td><td>100.0</td><td>50.0</td><td>50.0</td><td>0</td><td>75.0</td><td>100.0</td><td style="background-color:#FF8080">&nbsp;</td></tr>
!! </table>
!! </blockquote>
!! <!-- ======================================================================= -->
!! <dt> AUTHOR </dt><dd>
!!
!!    John S. Urban
!!
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> LICENSE </dt><dd>
!!
!!    Public Domain
!!
!! </dd>
!! <!-- ======================================================================= -->
!! </dl>
subroutine rgbhls(r0,g0,b0,h,l,s,status)

! ident_3="@(#)M_color::rgbhls(3fp): given red,green,blue values calculate hue,lightness,saturation"

!     given  : r, g, b each as a value of 0 to 100
!     desired: h as a value of 0 to 360 degrees.
!     .        l and s each as a value of 0 to 100
!
real    :: r0,g0,b0
real    :: r,g,b,h,l,s
real    :: clrmax,clrmin,clrdel,clrsum,rr,gg,bb
integer :: status
   if(r0 .lt. 0.0 .or. r0 .gt. 100.0 ) status = 1 !---- passive check for valid range of values.
   if(g0 .lt. 0.0 .or. g0 .gt. 100.0 ) status = 1 !---- passive check for valid range of values.
   if(b0 .lt. 0.0 .or. b0 .gt. 100.0 ) status = 1 !---- passive check for valid range of values.
   r=r0/100.0
   g=g0/100.0
   b=b0/100.0
   clrmax=amax1(r,g,b)
   clrmin=amin1(r,g,b)
   clrdel=clrmax-clrmin
   clrsum=clrmax+clrmin
   l=clrsum/2.0
   if(clrdel.ne.0.0 ) then
      rr=(clrmax-r)/clrdel
      gg=(clrmax-g)/clrdel
      bb=(clrmax-b)/clrdel
      if(l.le.0.5) then
         s=clrdel/clrsum
      else
         s=clrdel/(2.0-clrsum)
      endif
      if(r.eq.clrmax) then
         h=bb-gg
      else if(g.eq.clrmax) then
         h=2.0 +rr-bb
      else if(b.eq.clrmax) then
         h=4.0 +gg-rr
      endif
      h=h*60.0
      if(h.lt.0.0 ) then
         h=h+360.0
      endif
   else
      s=0.0
      h=0.0
   endif
   l=l*100.0
   s=s*100.0
   if(h .lt.   0.0 ) h = 0.0   !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   if(h .gt. 360.0 ) h = 360.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   if(l .lt.   0.0 ) l=0.0
   if(l .gt. 100.0 ) l = 100.0
   if(s .lt.   0.0 ) s=0.0
   if(s .gt. 100.0 ) s = 100.0
end subroutine rgbhls
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!! <dl>
!! <!-- ======================================================================= -->
!! <dt> <a name="RGBHVS">NAME</a> </dt><dd>
!!     RGBHVS(3fp) - [M_color] calculates the hue, value, &amp; saturation
!!     for a color given in red, green, &amp; blue components values.
!!     (LICENSE:PD)
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> SYNOPSIS  </dt><dd>
!!    <pre>
!!    subroutine rgbhvs(r,g,b,h,v,s,status)
!!
!!     ! the red component as a value of 0 to 100.
!!     real, intent(in)  :: r
!!     ! the green component as a value of 0 to 100.
!!     real, intent(in)  :: g
!!     ! the blue component as a value of 0 to 100.
!!     real, intent(in)  :: b
!!     ! the hue value in the range of 0 to 360 degrees
!!     real, intent(out) :: h
!!     ! the "value" as a percent value from 0 to 100.
!!     real, intent(out) :: v
!!     ! the saturation as a percent from 0 to 100.
!!     real, intent(out) :: s
!!     integer           :: status
!!    <pre>
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> DESCRIPTION  </dt><dd>
!!
!! RGBHVS() calculates the hue, value, &amp; saturation
!! for a color given in red, green, &amp; blue components values.
!! <br/><br/>
!!
!! <blockquote>
!! <table border="1" class="dtable" >
!! <tr>      <th>                                               Color</th>  <th>Color<br>name</th> <th>Hex    </th>  <th>(R,G,B)</th>        <th>(H,S,V)</th>                  </tr>
!! <tr>      <td              style="background-color:#000000">&nbsp;</td>  <td>Black</td>         <td>#000000</td>  <td>(0,0,0)</td>        <td>(0&ordm;,0%,0%)</td>        </tr>
!! <tr>      <td              style="background-color:#FFFFFF">&nbsp;</td>  <td>White</td>         <td>#FFFFFF</td>  <td>(100,100,100)</td>  <td>(0&ordm;,0%,100%)</td>      </tr>
!! <tr>      <td              style="background-color:#FF0000">&nbsp;</td>  <td>Red</td>           <td>#FF0000</td>  <td>(100,0,0)</td>      <td>(0&ordm;,100%,100%)</td>    </tr>
!! <tr>      <td              style="background-color:#00FF00">&nbsp;</td>  <td>Lime</td>          <td>#00FF00</td>  <td>(0,100,0)</td>      <td>(120&ordm;,100%,100%)</td>  </tr>
!! <tr>      <td              style="background-color:#0000FF">&nbsp;</td>  <td>Blue</td>          <td>#0000FF</td>  <td>(0,0,100)</td>      <td>(240&ordm;,100%,100%)</td>  </tr>
!! <tr>      <td              style="background-color:#FFFF00">&nbsp;</td>  <td>Yellow</td>        <td>#FFFF00</td>  <td>(100,100,0)</td>    <td>(60&ordm;,100%,100%)</td>   </tr>
!! <tr>      <td              style="background-color:#00FFFF">&nbsp;</td>  <td>Cyan</td>          <td>#00FFFF</td>  <td>(0,100,100)</td>    <td>(180&ordm;,100%,100%)</td>  </tr>
!! <tr>      <td              style="background-color:#FF00FF">&nbsp;</td>  <td>Magenta</td>       <td>#FF00FF</td>  <td>(100,0,100)</td>    <td>(300&ordm;,100%,100%)</td>  </tr>
!! <tr>      <td              style="background-color:#C0C0C0">&nbsp;</td>  <td>Silver</td>        <td>#C0C0C0</td>  <td>(75,75,75)</td>     <td>(0&ordm;,0%,75%)</td>       </tr>
!! <tr>      <td              style="background-color:#808080">&nbsp;</td>  <td>Gray</td>          <td>#808080</td>  <td>(50,50,50)</td>     <td>(0&ordm;,0%,50%)</td>       </tr>
!! <tr>      <td              style="background-color:#800000">&nbsp;</td>  <td>Maroon</td>        <td>#800000</td>  <td>(50,0,0)</td>       <td>(0&ordm;,100%,50%)</td>     </tr>
!! <tr>      <td              style="background-color:#808000">&nbsp;</td>  <td>Olive</td>         <td>#808000</td>  <td>(50,50,0)</td>      <td>(60&ordm;,100%,50%)</td>    </tr>
!! <tr>      <td              style="background-color:#008000">&nbsp;</td>  <td>Green</td>         <td>#008000</td>  <td>(0,50,0)</td>       <td>(120&ordm;,100%,50%)</td>   </tr>
!! <tr>      <td              style="background-color:#800080">&nbsp;</td>  <td>Purple</td>        <td>#800080</td>  <td>(50,0,50)</td>      <td>(300&ordm;,100%,50%)</td>   </tr>
!! <tr>      <td              style="background-color:#008080">&nbsp;</td>  <td>Teal</td>          <td>#008080</td>  <td>(0,50,50)</td>      <td>(180&ordm;,100%,50%)</td>   </tr>
!! <tr>      <td              style="background-color:#000080">&nbsp;</td>  <td>Navy</td>          <td>#000080</td>  <td>(0,0,50)</td>       <td>(240&ordm;,100%,50%)</td>     </tr>
!! </table>
!! </blockquote>
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> AUTHOR </dt><dd>
!!
!!    John S. Urban
!!
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> LICENSE </dt><dd>
!!
!!    Public Domain
!!
!! </dd>
!! <!-- ======================================================================= -->
!! </dl>
subroutine rgbhvs(r0,g0,b0,h,v,s,status)

! ident_4="@(#)M_color::rgbhvs(3fp): given red,green,blue calculate hue,saturation,value components"

!---- this procedure calculates a hue, saturation, value equivalent for a
!     color given in red, green, & blue components.
!     given  : r, g, b each as a value of 0 to 100.
!     desired: h as a value of 0 to 360 degrees.
!     .        s and v each as a value of 0 to 100.
!
real,intent(in)  :: r0,g0,b0
real,intent(out) :: h,v,s
integer          :: status
real             :: r,g,b
real             :: clrmax,clrmin,clrdel,rr,gg,bb
   if(r0 .lt. 0.0 .or. r0 .gt. 100.0 ) status = 1 !---- check for valid range of values.
   if(g0 .lt. 0.0 .or. g0 .gt. 100.0 ) status = 1 !---- check for valid range of values.
   if(b0 .lt. 0.0 .or. b0 .gt. 100.0 ) status = 1 !---- check for valid range of values.
   r=r0
   g=g0
   b=b0
   r=r/100.0
   g=g/100.0
   b=b/100.0
   clrmax=amax1(r,g,b)
   clrmin=amin1(r,g,b)
   clrdel=clrmax-clrmin
   v=clrmax
   if(clrmax.ne.0.0 )then
         s=clrdel/clrmax
   else
         s=0.0
   endif
   if(s.ne.0.0 )then
         rr=(clrmax-r)/clrdel
         gg=(clrmax-g)/clrdel
         bb=(clrmax-b)/clrdel
         if(r.eq.clrmax)then
            h=bb-gg
         else if(g.eq.clrmax) then
            h=2.0 +rr-bb
         else if(b.eq.clrmax) then
            h=4.0 +gg-rr
         endif
         h=h*60.0
         if(h.lt.0.0 ) then
            h=h+360.0
         endif
   endif
   v=v*100.0
   s=s*100.0
   if(h .gt. 360.0 ) h = 360.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   if(h .lt.   0.0 ) h =   0.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   if(v .gt. 100.0 ) v = 100.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   if(v .lt.   0.0 ) v =   0.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   if(s .gt. 100.0 ) s = 100.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
   if(s .lt.   0.0 ) s =   0.0 !---- Eliminate any roundoff that exceeds the limits (or hide formula bug!)
end subroutine rgbhvs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!! <dl>
!! <!-- ======================================================================= -->
!! <dt> <a name="CMYRGB">NAME</a> </dt><dd>
!!     cmyrgb(3fp) - [M_color] calculates the cyan, magenta, and yellow components
!!     given the  red, green, and blue component values.
!!     (LICENSE:PD)
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> SYNOPSIS  </dt><dd>
!!    <pre>
!!    subroutine cmyrgb(c,m,y,r,g,b,status)
!!
!!     ! cyan component as a value in the range of 0 to 100
!!     real, intent(in)  :: c
!!     ! magenta component as a value in the range of 0 to 100
!!     real, intent(in)  :: m
!!     ! yellow component as a value in the range of 0 to 100
!!     real, intent(in)  :: y
!!     ! red component as a value in the range of 0 to 100
!!     real, intent(out) :: r
!!     ! green component as a value in the range of 0 to 100
!!     real, intent(out) :: g
!!     ! blue component as a value in the range of 0 to 100
!!     real, intent(out) :: b
!!     integer           :: status
!!    </pre>
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> DESCRIPTION  </dt><dd>
!!     cmyrgb() calculates the cyan, magenta, and yellow components
!!     given the  red, green, and blue component values.
!! <!-- ======================================================================= -->
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> AUTHOR </dt><dd>
!!
!!    John S. Urban
!!
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> LICENSE </dt><dd>
!!
!!    Public Domain
!!
!! </dd>
!! <!-- ======================================================================= -->
!! </dl>
subroutine cmyrgb(c,m,y,r,g,b,status)

! ident_5="@(#)M_color::cmyrgb(3fp): given cyan,magenta,yellow calculate red,green,blue components"

! given  : r, g, b each as a value of 0 to 100
! desired: c, m, y each as a value of 0 to 100
real,intent(in)   :: c,m,y
real,intent(out)  :: r,g,b
integer           :: status
   if(c .lt. 0.0 .or. c .gt. 100.0 ) status = 1 !---- passively check for valid range of values.
   if(m .lt. 0.0 .or. m .gt. 100.0 ) status = 1 !---- passively check for valid range of values.
   if(y .lt. 0.0 .or. y .gt. 100.0 ) status = 1 !---- passively check for valid range of values.
   r= 100.0 - c
   g= 100.0 - m
   b= 100.0 - y
end subroutine cmyrgb
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!! <dl>
!! <!-- ======================================================================= -->
!! <dt> <a name="RGBCMY">NAME</a> </dt><dd>
!!     rgbcmy(3fp) - [M_color] calculates the cyan, magenta, and yellow components
!!     given the  red, green, and blue component values.
!!     (LICENSE:PD)
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> SYNOPSIS  </dt><dd>
!!    <pre>
!!    subroutine rgbcmy(r,g,b,c,m,y,status)
!!
!!     ! the red component as a value in the range of 0 to 100
!!     real, intent(in)  :: r
!!     ! the green component as a value in the range of 0 to 100
!!     real, intent(in)  :: g
!!     ! the blue component as a value in the range of 0 to 100
!!     real, intent(in)  :: b
!!     ! the cyan component as a value in the range of 0 to 100
!!     real, intent(out) :: c
!!     ! the magenta component as a value in the range of 0 to 100
!!     real, intent(out) :: m
!!     ! the yellow component as a value in the range of 0 to 100
!!     real, intent(out) :: y
!!     integer           :: status
!!    </pre>
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> DESCRIPTION  </dt><dd>
!! Table ...
!! <br/><br/>
!!
!! <blockquote>
!! <table border="1" >
!! <tr> <th>Color      </th>                                              <th>Color<p>name </th> <th>(C,M,Y)</th>        <th>(  R,  G,  B)</th>  <th>Hex</th>      </tr>
!! <tr> <td style="background-color:#000000">&nbsp;</td> <td>Black</td>   <td>(100,100,100)</td> <td>(  0,  0,  0)</td>  <td>#000000</td>  </tr>
!! <tr> <td style="background-color:#FFFFFF">&nbsp;</td> <td>White</td>   <td>(  0,  0,  0)</td> <td>(100,100,100)</td>  <td>#FFFFFF</td>  </tr>
!! <tr> <td style="background-color:#FF0000">&nbsp;</td> <td>Red</td>     <td>(  0,100,100)</td> <td>(100,  0,  0)</td>  <td>#FF0000</td>  </tr>
!! <tr> <td style="background-color:#00FF00">&nbsp;</td> <td>Green</td>   <td>(100,  0,100)</td> <td>(  0,100,  0)</td>  <td>#00FF00</td>  </tr>
!! <tr> <td style="background-color:#0000FF">&nbsp;</td> <td>Blue</td>    <td>(100,100,  0)</td> <td>(  0,  0,100)</td>  <td>#0000FF</td>  </tr>
!! <tr> <td style="background-color:#FFFF00">&nbsp;</td> <td>Yellow</td>  <td>(  0,  0,100)</td> <td>(100,100,  0)</td>  <td>#FFFF00</td>  </tr>
!! <tr> <td style="background-color:#00FFFF">&nbsp;</td> <td>Cyan</td>    <td>(100,  0,  0)</td> <td>(  0,100,100)</td>  <td>#00FFFF</td>  </tr>
!! <tr> <td style="background-color:#FF00FF">&nbsp;</td> <td>Magenta</td> <td>(  0,100,  0)</td> <td>(100,  0,100)</td>  <td>#FF00FF</td>  </tr>
!! </table>
!! </blockquote>
!! <!-- ======================================================================= -->
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> AUTHOR </dt><dd>
!!
!!    John S. Urban
!!
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> LICENSE </dt><dd>
!!
!!    Public Domain
!!
!! </dd>
!! <!-- ======================================================================= -->
!! </dl>
subroutine rgbcmy(r,g,b,c,m,y,status)

! ident_6="@(#)M_color::rgbcmy(3fp): given red,green,blue calculate cyan,magenta,yellow components"

!     given  : r, g, b each as a value of 0 to 100
!     desired: c, m, y each as a value of 0 to 100
real,intent(in)  :: r,g,b
real,intent(out) :: c,m,y
integer          :: status
   if(r .lt. 0.0 .or. r .gt. 100.0 ) status = 1 !---- check for valid range of values.
   if(g .lt. 0.0 .or. g .gt. 100.0 ) status = 1 !---- check for valid range of values.
   if(b .lt. 0.0 .or. b .gt. 100.0 ) status = 1 !---- check for valid range of values.
   c = 100.0 - r
   m = 100.0 - g
   y = 100.0 - b

end subroutine rgbcmy
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    RGBMONO(3f) - [M_color] converts RGB colors to a reasonable grayscale
!!    intensity
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine rgbmono</a>(rr,rg,rb,ri,status)
!!
!!     real, intent(in)  :: RR
!!     real, intent(in)  :: RG
!!     real, intent(in)  :: RB
!!     real, intent(out) :: RI
!!     integer           :: status
!!
!!##DESCRIPTION
!!    RGBMONO(3f) converts RGB colors to a reasonable grayscale intensity.
!!    This can be used to produce monochrome images from color images.
!!    Intensity is calculated from the specified Red, Green, Blue intensities
!!    as 0.30*R + 0.59*G + 0.11*B, as in U.S. color television systems,
!!    NTSC encoding. Note that most devices do not have an infinite range
!!    of monochrome intensities available.
!!
!!##OPTIONS
!!     RR      red component of the input color in the range 0 to 100
!!     RG      green component of the input color in the range 0 to 100
!!     RB      blue component of the input color in the range 0 to 100
!!
!!##RETURNS
!!     RI      grayscale intensity calculated in the range 0 to 100
!!     status  zero (0) if no error occurred, otherwise result is out
!!             of bounds
!!
!!##EXAMPLES
!!
!!   Sample:
!!
!!    program demo_rgbmono
!!    use M_color, only : rgbmono
!!    implicit none
!!    real    :: gray
!!    integer :: ierr
!!    call rgbmono(100.0,  0.0,  0.0,gray,ierr); write(*,*)'red     ',gray
!!    call rgbmono(  0.0,100.0,  0.0,gray,ierr); write(*,*)'green   ',gray
!!    call rgbmono(  0.0,  0.0,100.0,gray,ierr); write(*,*)'blue    ',gray
!!    call rgbmono(100.0,100.0,  0.0,gray,ierr); write(*,*)'Yellow  ',gray
!!    call rgbmono(  0.0,100.0,100.0,gray,ierr); write(*,*)'Cyan    ',gray
!!    call rgbmono(100.0,  0.0,100.0,gray,ierr); write(*,*)'Magenta ',gray
!!    call rgbmono(100.0,100.0,100.0,gray,ierr); write(*,*)'White   ',gray
!!    call rgbmono( 00.0,  0.0,  0.0,gray,ierr); write(*,*)'Black   ',gray
!!    call rgbmono( 50.0,  0.0,  0.0,gray,ierr); write(*,*)'Maroon  ',gray
!!    call rgbmono(100.0, 50.0, 50.0,gray,ierr); write(*,*)'Pink    ',gray
!!    end program demo_rgbmono
!!
!!   Results:
!!
!!     red        30.0000019
!!     green      58.9999962
!!     blue       11.0000000
!!     Yellow     89.0000000
!!     Cyan       70.0000000
!!     Magenta    41.0000000
!!     White      100.000000
!!     Black      0.00000000
!!     Maroon     15.0000010
!!     Pink       65.0000000
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine rgbmono(rr,rg,rb,ri,status)

! ident_7="@(#)M_color::rgbmono(3f): convert RGB colors to a reasonable grayscale"

! monochrome devices that support intensity can have intensity calculated from the specified Red, Green, Blue
! intensities as 0.30*R + 0.59*G + 0.11*B, as in US color television systems, NTSC encoding.
! Note that most devices do not have an infinite range of monochrome intensities available.

real,intent(in)      :: rr,rg,rb                ! red, green, blue, & intensity range from 0 to 100
real,intent(out)     :: ri
integer,intent(out)  :: status
   status=0
   if(rr .lt. 0.0 .or. rr .gt. 100.0 ) status = 1 !---- passive check for valid range of values.
   if(rg .lt. 0.0 .or. rg .gt. 100.0 ) status = 1 !---- passive check for valid range of values.
   if(rb .lt. 0.0 .or. rb .gt. 100.0 ) status = 1 !---- passive check for valid range of values.
   ri = 0.30*rr + 0.59*rg + 0.11*rb
end subroutine rgbmono
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!! <dl>
!! <!-- ======================================================================= -->
!! <dt> <a name="RGBVAL">NAME</a> </dt><dd>
!!    RGBVAL(3fp) - [M_color] is an internal private function used by hlsrgb(3fp).
!!    (LICENSE:PD)
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> SYNOPSIS</dt><dd>
!!    <pre>
!!    subroutine rgbval</a>(clr1,clr2,h)
!!
!!     integer, intent(in) :: h ! H is the hue value in degrees
!!     real, intent(in) :: clr1 !
!!     real, intent(in) :: clr2 !
!!    </pre>
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> DESCRIPTION</dt>  <dd>
!!    Function RGBVAL(3f) is an internal private function used by hlsrgb().
!!
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> AUTHOR </dt><dd>
!!
!!    John S. Urban
!!
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> LICENSE </dt><dd>
!!
!!    Public Domain
!!
!! </dd>
!! <!-- ======================================================================= -->
!! </dl>
!! <!-- ======================================================================= -->
real function rgbval(clr1,clr2,h)

! ident_8="@(#)M_color::rgbval(3fp): ensure a value is in the appropriate range and quadrant"

real    :: clr1,clr2
real    :: h
real    :: h2
   h2=h
   do
      if(h2.gt.360.0 ) then
         h2=h2-360.0
         cycle
      endif
      exit
   enddo

   do
      if( h2 .lt. 0.0 ) then
         h2=h2+360.0
         cycle
      endif
      exit
   enddo

   if(h2.lt.60.0 ) then
      rgbval=clr1+(clr2-clr1)*h2/60.0
   else if(h2.lt.180.0) then
      rgbval=clr2
   else if(h2.lt.240.0) then
      rgbval=clr1+(clr2-clr1)*(240.0-h2)/60.0
   else
      rgbval=clr1
   endif

end function rgbval
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!! <dl>
!! <!-- ======================================================================= -->
!! <dt> <a name="HLSRGB">NAME</a> </dt><dd>
!!     HLSRGB(3fp) - [M_color] calculates the red, green, &amp; blue components for a
!!     color given in hue, lightness, &amp; saturation values.
!!     (LICENSE:PD)
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> SYNOPSIS</dt>  <dd>
!!    <pre>
!!    subroutine hlsrgb</a> (h,l,s,r,g,b,status)
!!
!!     ! hue value in the range of 0 to 360 degrees
!!     real, intent(in)  :: h
!!     ! lightness as a percent value from 0 to 100.
!!     real, intent(in)  :: l
!!     ! saturation as a percent from 0 to 100.
!!     real, intent(in)  :: s
!!     ! red component as a value of 0 to 100.
!!     real, intent(out) :: r
!!     ! green component as a value of 0 to 100.
!!     real, intent(out) :: g
!!     ! blue component as a value of 0 to 100.
!!     real, intent(out) :: b
!!     integer           :: status
!!    </pre>
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> DESCRIPTION</dt>  <dd>
!!
!!     HLSRGB() calculates the red, green, &amp; blue components for a
!!      color given in hue, lightness, &amp; saturation values.
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> AUTHOR </dt><dd>
!!
!!    John S. Urban
!!
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> LICENSE </dt><dd>
!!
!!    Public Domain
!!
!! </dd>
!! <!-- ======================================================================= -->
!! </dl>
subroutine hlsrgb(H,L,S,R,G,B,status)

! ident_9="@(#)M_color::hlsrgb(3fp): convert HLS(hue,lightness,saturation) values to RGB components"

!     given  : hue as a value of 0 to 360 degrees.
!     .        lightness and saturation each as a value of 0 to 100.
!     desired: r, g, and b each as a value of 0 to 100.
!
real,intent(in)   :: H,L,S
real,intent(out)  :: R,G,B
integer           :: status
real              :: hue,lightness,saturation
real              :: clr1,clr2
   if(h .lt. 0.0 .or. h .gt.360.0 ) status = 1 ! passively report on bad input values
   if(l .lt. 0.0 .or. l .gt.100.0 ) status = 1 ! passively report on bad input values
   if(s .lt. 0.0 .or. s .gt.100.0 ) status = 1 ! passively report on bad input values
   hue =           H
   lightness =     L/100.0
   saturation =    S/100.0
   if( saturation .eq. 0.0 ) then
      R = lightness
      G = lightness
      B = lightness
   endif
   if(lightness .le. 0.50) then
      clr2= lightness*( 1.0 + saturation )
   else
      clr2= lightness + saturation - lightness * saturation
   endif
   clr1= 2.0 * lightness - clr2
   R = rgbval(clr1,clr2,hue+120.0)  *100.0
   G = rgbval(clr1,clr2,hue)        *100.0
   B = rgbval(clr1,clr2,hue-120.0)  *100.0
end subroutine hlsrgb
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!! <dl>
!! <!-- ======================================================================= -->
!! <dt> <a name="HVSRGB">NAME</a> </dt><dd>
!!     HVSRGB(3fp) - [M_color] calculates the red, green, &amp; blue components for a
!!      color given in hue, value, &amp; saturation values.
!!      (LICENSE:PD)
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> SYNOPSIS </dt><dd>
!!    <pre>
!!    subroutine hvsrgb</a>(h,v,s,r,g,b,status)
!!
!!     ! H is the hue value in the range of 0 to 360 degrees
!!     real, intent(in)  :: h
!!     ! V is the "value" as a percent value from 0 to 100.
!!     real, intent(in)  :: v
!!     ! S is the saturation as a percent from 0 to 100.
!!     real, intent(in)  :: s
!!     ! R is the red component as a value of 0 to 100.
!!     real, intent(out) :: r
!!     ! G is the green component as a value of 0 to 100.
!!     real, intent(out) :: g
!!     ! B is the blue component as a value of 0 to 100.
!!     real, intent(out) :: b
!!     integer           :: status
!!    </pre>
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> DESCRIPTION </dt><dd>
!!
!!     HVSRGB() calculates the red, green, &amp; blue components for a
!!      color given in hue, value, &amp; saturation values.
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> AUTHOR </dt><dd>
!!
!!    John S. Urban
!!
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> LICENSE </dt><dd>
!!
!!    Public Domain
!!
!! </dd>
!! <!-- ======================================================================= -->
!! </dl>
subroutine hvsrgb(h,v,s,r,g,b,status)

! ident_10="@(#)M_color::hvsrgb(3fp): given hue,saturation,value calculate red,green,blue components"

!     given  : hue as value of 0 to 360 degrees.
!     .        saturation and value each as a value of 0 to 100.
!     desired: r, g, and b as a value of 0 to 100.
real,intent(in)    :: h,v,s
real,intent(out)   :: r,g,b
integer            :: status
real               :: hue,value,saturation
integer            :: ifloor
real               :: f,p,q,t
   if(h .lt. 0.0 .or. h .gt.360.0 ) status = 1 ! passively report on bad input values
   if(v .lt. 0.0 .or. v .gt.100.0 ) status = 1 ! passively report on bad input values
   if(s .lt. 0.0 .or. s .gt.100.0 ) status = 1 ! passively report on bad input values
   hue=h
   value=v/100.0
   saturation=s/100.0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(saturation.eq.0.0) then
      r=value
      g=value
      b=value
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(hue.eq.360.0) then
      hue=0.0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   hue=hue/60.0
   ifloor=int(hue)
   f=hue-ifloor
   p=value*(1.0-saturation)
   q=value*(1.0-(saturation*f))
   t=value*(1.0-(saturation*(1-f)))
   SELECT CASE (ifloor)
   CASE (0) ;r=value; g=t; b=p
   CASE (1) ;r=q; g=value; b=p
   CASE (2) ;r=p; g=value; b=t
   CASE (3) ;r=p; g=q; b=value
   CASE (4) ;r=t; g=p; b=value
   CASE (5) ;r=value; g=p; b=q
   CASE DEFAULT
   END SELECT
   r=r*100.0
   g=g*100.0
   b=b*100.0
end subroutine hvsrgb
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!! <dl>
!! <!-- ======================================================================= -->
!! <dt> <a name="YIQRGB">NAME</a> </dt><dd>
!!    YIQRGB(3fp) - [M_color] Convert luma, orange-blue chrominance, and  purple-green chrominance
!!    to RGB values.
!!    (LICENSE:PD)
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> SYNOPSIS  </dt><dd>
!!    <pre>
!!    subroutine yiqrgb(y,i,q,r,g,b,status)
!!
!!     real,intent(in)  :: y,i,q
!!     real,intent(out) :: r,g,b
!!     integer          :: status
!!    </pre>
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> DESCRIPTION  </dt><dd>
!!
!!    Convert luma, orange-blue chrominance, and  purple-green chrominance
!!    to RGB values.
!!
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> AUTHOR </dt><dd>
!!
!!    John S. Urban
!!
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> LICENSE </dt><dd>
!!
!!    Public Domain
!!
!! </dd>
!! <!-- ======================================================================= -->
!! </dl>
subroutine yiqrgb(y,i,q,r,g,b,status)

! ident_11="@(#)M_color::yiqrgb(3fp): convert luma,orange-blue chrominance,purple-green chrominance to RGB"

real,intent(in)  :: y,i,q
real,intent(out) :: r,g,b
integer          :: status
!
!----    i don't believe that this is an exhaustive test of value ranges
!        for yiq.  for example yiq=(100.0,60.0,52.0) when converted to
!        rgb produces values greater than 100!?
!
      if(i .lt. -60.0 .or. i .gt.  60.0) status = 1
      if(q .lt. -53.0 .or. q .gt.  53.0) status = 1

      r = 1.0 * y + 0.956 * i + 0.621 * q
      g = 1.0 * y - 0.272 * i - 0.647 * q
      b = 1.0 * y - 1.106 * i + 1.703 * q
      !r= 1.0 *y + 0.94826224*i + 0.62401264*q
      !g= 1.0 *y - 0.27606635*i - 0.63981043*q
      !b= 1.0 *y - 1.1054502 *i + 1.7298578 *q
!
!-- If outside the valid range of values, truncate to allow for reasonable roundoff and then retest.
!   This should pass values essentially 0 or 100, but fail others.
!   The above formula for rgb from yiq can give answers slightly less than 0 and slightly greater than 100.0
!   The truncation should fix this.
!   The retest should then catch the instances such as yiq=(100.0,60.0,52.0) as mentioned earlier.

   r=min(100.0,max(0.0,r))
   g=min(100.0,max(0.0,g))
   b=min(100.0,max(0.0,b))

end subroutine yiqrgb
!=============================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!! <dl>
!! <!-- ======================================================================= -->
!! <dt> <a name="RGBYIQ">NAME</a> </dt><dd>
!!    RGBYIQ(3fp) - [M_color] Convert RGB values to luma, orange-blue chrominance, and  purple-green chrominance.
!!    (LICENSE:PD)
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> SYNOPSIS  </dt><dd>
!!    <pre>
!!    subroutine rgbyiq(r,g,b,y,i,q,status)
!!
!!     real,intent(in)  :: r,g,b
!!     real,intent(out) :: y,i,q
!!     integer          :: status
!!    </pre>
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> DESCRIPTION  </dt><dd>
!!    Convert RGB values to luma, orange-blue chrominance, and  purple-green chrominance.
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> AUTHOR </dt><dd>
!!
!!    John S. Urban
!!
!! </dd>
!! <!-- ======================================================================= -->
!! <dt> LICENSE </dt><dd>
!!
!!    Public Domain
!!
!! </dd>
!! <!-- ======================================================================= -->
!! </dl>
subroutine rgbyiq(r,g,b,y,i,q,status)

! ident_12="@(#)M_color::rgbyiq(3fp): convert RGB to luma,orange-blue chrominance,purple-green chrominance"

real,intent(in)  :: r,g,b
real,intent(out) :: y,i,q
integer          :: status
   if(r.lt.0.0 .or. r.gt.100.0) status=1
   if(g.lt.0.0 .or. g.gt.100.0) status=1
   if(b.lt.0.0 .or. b.gt.100.0) status=1

   y= 0.299 * r + 0.587 * g + 0.114 * b
   i= 0.596 * r - 0.274 * g - 0.322 * b
   q= 0.211 * r - 0.523 * g + 0.312 * b

!-- Eliminate any roundoff that exceeds the limits.
   if(i .lt. -59.57 ) i = -59.57
   if(i .gt.  59.57 ) i =  59.57
   if(q .lt. -52.26 ) q = -52.26
   if(q .gt.  52.26 ) q =  52.26
end subroutine rgbyiq
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    closest_color_name(3f) - [M_color] returns the closest name for the
!!    given RGB values.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine closest_color_name(r,g,b,closestname)
!!
!!     real,intent(in)               :: r,g,b
!!     character(len=20),intent(out) :: closestname
!!
!!##DESCRIPTION
!!    closest_color_name() returns the closest name for the given RGB values.
!!    Most X11 Windows color names are supported.
!!
!!##OPTIONS
!!    R   red component, range of 0 to 100
!!    G   green component, range of 0 to 100
!!    B   blue component, range of 0 to 100
!!
!!##RETURNS
!!    CLOSESTNAME   name of color found closest to given RGB value</li>
!!
!!##EXAMPLE
!!
!!
!!   Sample program
!!
!!    program demo_closest_color_name
!!    use M_color, only : closest_color_name
!!    character(len=100) :: string ! at least 20 characters
!!       string=' '
!!
!!       call closest_color_name(100.0,  0.0,  0.0,string)
!!       write(*,*)trim(string)
!!
!!       call closest_color_name(  0.0,100.0,  0.0,string)
!!       write(*,*)trim(string)
!!
!!       call closest_color_name(  0.0,  0.0,100.0,string)
!!       write(*,*)trim(string)
!!
!!    end program demo_closest_color_name
!!
!!   Results:
!!
!!    red
!!    green
!!    blue
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
SUBROUTINE closest_color_name(r,g,b,closestname)

! ident_13="@(#)M_color::closest_color_name(3f): given RGB values, try to find closest named color"

real,intent(in)               :: r,g,b
character(len=*),intent(out) :: closestname
real                          :: rn,gn,bn
real                          :: distance, minimum_distance
character(len=20)             :: echoname
integer                       :: i
character(len=20)             :: string
!-----------------------------------------------------------------------------------------------------------------------------------
   minimum_distance=1000.0
   closestname='Unknown'
   INFINITE: do i=1,1000
      write(string,'(i0)')i
      call color_name2rgb(string,rn,gn,bn,echoname)       ! get next color
      if(echoname.eq.'Unknown') exit INFINITE
      distance=sqrt( (r-rn)**2 + (g-gn)**2 + (b-bn)**2 )
      if(distance.lt.minimum_distance)then
         closestname=echoname
         minimum_distance=min(minimum_distance,distance)
      endif
   enddo INFINITE
end SUBROUTINE closest_color_name
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    COLOR_NAME2RGB(3f) - [M_color] returns the RGB values in the range 0 to
!!    100 for a given known color name.
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine color_name2rgb(name,r,g,b,echoname)
!!
!!     character(len=20),intent(in)   :: name
!!     real,intent(out)               :: r,g,b
!!     character(len=20),intent(out)  :: echoname
!!
!!##DESCRIPTION
!!    COLOR_NAME2RGB() returns the RGB values in the range 0 to 100
!!    for a given known color name. Most X11 Windows color names are
!!    supported. If the name is not found, ECHONAME is set to "Unknown".
!!
!!##EXAMPLE
!!
!!    A sample program:
!!
!!     program demo_color_name2rgb
!!     use M_color, only : hue, color_name2rgb
!!     implicit none
!!     !
!!     ! list colors known to colorname2rgb(3f) & corresponding RGB values
!!     !
!!     character(len=20) :: name
!!     character(len=20) :: echoname
!!     real              :: red,green,blue
!!     integer           :: i
!!     TRYALL: do i=1,10000
!!        ! weird little thing where the color names have aliases
!!        ! that are numeric strings
!!        write(name,'(i0)')i
!!        ! get the RGB values and English name of the color
!!        call color_name2rgb(name,red,green,blue,echoname)
!!        ! the last color name is "Unknown" so the loop should exit
!!        if(echoname.eq.'Unknown')exit TRYALL
!!        ! display the English name and RGB values for the name
!!        write(*,*)echoname,int([red,green,blue])
!!     enddo TRYALL
!!     !write(*,*)'Number of colors found is ',i-1
!!     end program demo_color_name2rgb
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine color_name2rgb(name,r,g,b,echoname)

! ident_14="@(#)M_color::color_name2rgb(3f): given a color name, return rgb color values in range 0 to 100"

character(len=*),intent(in)            :: name
real,intent(out)                       :: r,g,b
character(len=*),intent(out),optional  :: echoname
character(len=20)                      :: newname
!-----------------------------------------------------------------------------------------------------------------------------------
! returns name in ECHONAME; which is usually not useful unless NAME represents an integer string.
! Note that an integer converted to a string can be used to go sequentially thru the names until NEWNAME="Unknown"
! Color names can generally be listed using showrgb(1) in GNU/Linux and Unix environments that support X11 Windows:

! A structure would normally be used for the data; but a large SELECT is easy to maintain.
! a numeric name is an alias for each color to facilitate going thru them sequentially since they are not an array.
   SELECT case(TRIM(lower(name)))

   CASE("1",   "snow")                  ;  newname="snow"                  ;  r=255  ;  g=250  ;  b=250
   CASE("2",   "ghostwhite")            ;  newname="ghostwhite"            ;  r=248  ;  g=248  ;  b=255
   CASE("3",   "whitesmoke")            ;  newname="whitesmoke"            ;  r=245  ;  g=245  ;  b=245
   CASE("4",   "gainsboro")             ;  newname="gainsboro"             ;  r=220  ;  g=220  ;  b=220
   CASE("5",   "floralwhite")           ;  newname="floralwhite"           ;  r=255  ;  g=250  ;  b=240
   CASE("6",   "oldlace")               ;  newname="oldlace"               ;  r=253  ;  g=245  ;  b=230
   CASE("7",   "linen")                 ;  newname="linen"                 ;  r=250  ;  g=240  ;  b=230
   CASE("8",   "antiquewhite")          ;  newname="antiquewhite"          ;  r=250  ;  g=235  ;  b=215
   CASE("9",   "papayawhip")            ;  newname="papayawhip"            ;  r=255  ;  g=239  ;  b=213
   CASE("10",  "blanchedalmond")        ;  newname="blanchedalmond"        ;  r=255  ;  g=235  ;  b=205
   CASE("11",  "bisque")                ;  newname="bisque"                ;  r=255  ;  g=228  ;  b=196
   CASE("12",  "peachpuff")             ;  newname="peachpuff"             ;  r=255  ;  g=218  ;  b=185
   CASE("13",  "navajowhite")           ;  newname="navajowhite"           ;  r=255  ;  g=222  ;  b=173
   CASE("14",  "moccasin")              ;  newname="moccasin"              ;  r=255  ;  g=228  ;  b=181
   CASE("15",  "cornsilk")              ;  newname="cornsilk"              ;  r=255  ;  g=248  ;  b=220
   CASE("16",  "ivory")                 ;  newname="ivory"                 ;  r=255  ;  g=255  ;  b=240
   CASE("17",  "lemonchiffon")          ;  newname="lemonchiffon"          ;  r=255  ;  g=250  ;  b=205
   CASE("18",  "seashell")              ;  newname="seashell"              ;  r=255  ;  g=245  ;  b=238
   CASE("19",  "honeydew")              ;  newname="honeydew"              ;  r=240  ;  g=255  ;  b=240
   CASE("20",  "mintcream")             ;  newname="mintcream"             ;  r=245  ;  g=255  ;  b=250
   CASE("21",  "azure")                 ;  newname="azure"                 ;  r=240  ;  g=255  ;  b=255
   CASE("22",  "aliceblue")             ;  newname="aliceblue"             ;  r=240  ;  g=248  ;  b=255
   CASE("23",  "lavender")              ;  newname="lavender"              ;  r=230  ;  g=230  ;  b=250
   CASE("24",  "lavenderblush")         ;  newname="lavenderblush"         ;  r=255  ;  g=240  ;  b=245
   CASE("25",  "mistyrose")             ;  newname="mistyrose"             ;  r=255  ;  g=228  ;  b=225
   CASE("26",  "white")                 ;  newname="white"                 ;  r=255  ;  g=255  ;  b=255
   CASE("27",  "black")                 ;  newname="black"                 ;  r=0    ;  g=0    ;  b=0
   CASE("28",  "darkslategray")         ;  newname="darkslategray"         ;  r=47   ;  g=79   ;  b=79
   CASE("29",  "dimgray")               ;  newname="dimgray"               ;  r=105  ;  g=105  ;  b=105
   CASE("30",  "slategray")             ;  newname="slategray"             ;  r=112  ;  g=128  ;  b=144
   CASE("31",  "lightslategray")        ;  newname="lightslategray"        ;  r=119  ;  g=136  ;  b=153
   CASE("32",  "gray")                  ;  newname="gray"                  ;  r=190  ;  g=190  ;  b=190
   CASE("33",  "lightgray")             ;  newname="lightgray"             ;  r=211  ;  g=211  ;  b=211
   CASE("34",  "midnightblue")          ;  newname="midnightblue"          ;  r=25   ;  g=25   ;  b=112
   CASE("35",  "navy")                  ;  newname="navy"                  ;  r=0    ;  g=0    ;  b=128
   CASE("36",  "navyblue")              ;  newname="navyblue"              ;  r=0    ;  g=0    ;  b=128
   CASE("37",  "cornflowerblue")        ;  newname="cornflowerblue"        ;  r=100  ;  g=149  ;  b=237
   CASE("38",  "darkslateblue")         ;  newname="darkslateblue"         ;  r=72   ;  g=61   ;  b=139
   CASE("39",  "slateblue")             ;  newname="slateblue"             ;  r=106  ;  g=90   ;  b=205
   CASE("40",  "mediumslateblue")       ;  newname="mediumslateblue"       ;  r=123  ;  g=104  ;  b=238
   CASE("41",  "lightslateblue")        ;  newname="lightslateblue"        ;  r=132  ;  g=112  ;  b=255
   CASE("42",  "mediumblue")            ;  newname="mediumblue"            ;  r=0    ;  g=0    ;  b=205
   CASE("43",  "royalblue")             ;  newname="royalblue"             ;  r=65   ;  g=105  ;  b=225
   CASE("44",  "blue")                  ;  newname="blue"                  ;  r=0    ;  g=0    ;  b=255
   CASE("45",  "dodgerblue")            ;  newname="dodgerblue"            ;  r=30   ;  g=144  ;  b=255
   CASE("46",  "deepskyblue")           ;  newname="deepskyblue"           ;  r=0    ;  g=191  ;  b=255
   CASE("47",  "skyblue")               ;  newname="skyblue"               ;  r=135  ;  g=206  ;  b=235
   CASE("48",  "lightskyblue")          ;  newname="lightskyblue"          ;  r=135  ;  g=206  ;  b=250
   CASE("49",  "steelblue")             ;  newname="steelblue"             ;  r=70   ;  g=130  ;  b=180
   CASE("50",  "lightsteelblue")        ;  newname="lightsteelblue"        ;  r=176  ;  g=196  ;  b=222
   CASE("51",  "lightblue")             ;  newname="lightblue"             ;  r=173  ;  g=216  ;  b=230
   CASE("52",  "powderblue")            ;  newname="powderblue"            ;  r=176  ;  g=224  ;  b=230
   CASE("53",  "paleturquoise")         ;  newname="paleturquoise"         ;  r=175  ;  g=238  ;  b=238
   CASE("54",  "darkturquoise")         ;  newname="darkturquoise"         ;  r=0    ;  g=206  ;  b=209
   CASE("55",  "mediumturquoise")       ;  newname="mediumturquoise"       ;  r=72   ;  g=209  ;  b=204
   CASE("56",  "turquoise")             ;  newname="turquoise"             ;  r=64   ;  g=224  ;  b=208
   CASE("57",  "cyan")                  ;  newname="cyan"                  ;  r=0    ;  g=255  ;  b=255
   CASE("58",  "lightcyan")             ;  newname="lightcyan"             ;  r=224  ;  g=255  ;  b=255
   CASE("59",  "cadetblue")             ;  newname="cadetblue"             ;  r=95   ;  g=158  ;  b=160
   CASE("60",  "mediumaquamarine")      ;  newname="mediumaquamarine"      ;  r=102  ;  g=205  ;  b=170
   CASE("61",  "aquamarine")            ;  newname="aquamarine"            ;  r=127  ;  g=255  ;  b=212
   CASE("62",  "darkgreen")             ;  newname="darkgreen"             ;  r=0    ;  g=100  ;  b=0
   CASE("63",  "darkolivegreen")        ;  newname="darkolivegreen"        ;  r=85   ;  g=107  ;  b=47
   CASE("64",  "darkseagreen")          ;  newname="darkseagreen"          ;  r=143  ;  g=188  ;  b=143
   CASE("65",  "seagreen")              ;  newname="seagreen"              ;  r=46   ;  g=139  ;  b=87
   CASE("66",  "mediumseagreen")        ;  newname="mediumseagreen"        ;  r=60   ;  g=179  ;  b=113
   CASE("67",  "lightseagreen")         ;  newname="lightseagreen"         ;  r=32   ;  g=178  ;  b=170
   CASE("68",  "palegreen")             ;  newname="palegreen"             ;  r=152  ;  g=251  ;  b=152
   CASE("69",  "springgreen")           ;  newname="springgreen"           ;  r=0    ;  g=255  ;  b=127
   CASE("70",  "lawngreen")             ;  newname="lawngreen"             ;  r=124  ;  g=252  ;  b=0
   CASE("71",  "green")                 ;  newname="green"                 ;  r=0    ;  g=255  ;  b=0
   CASE("72",  "chartreuse")            ;  newname="chartreuse"            ;  r=127  ;  g=255  ;  b=0
   CASE("73",  "mediumspringgreen")     ;  newname="mediumspringgreen"     ;  r=0    ;  g=250  ;  b=154
   CASE("74",  "greenyellow")           ;  newname="greenyellow"           ;  r=173  ;  g=255  ;  b=47
   CASE("75",  "limegreen")             ;  newname="limegreen"             ;  r=50   ;  g=205  ;  b=50
   CASE("76",  "yellowgreen")           ;  newname="yellowgreen"           ;  r=154  ;  g=205  ;  b=50
   CASE("77",  "forestgreen")           ;  newname="forestgreen"           ;  r=34   ;  g=139  ;  b=34
   CASE("78",  "olivedrab")             ;  newname="olivedrab"             ;  r=107  ;  g=142  ;  b=35
   CASE("79",  "darkkhaki")             ;  newname="darkkhaki"             ;  r=189  ;  g=183  ;  b=107
   CASE("80",  "khaki")                 ;  newname="khaki"                 ;  r=240  ;  g=230  ;  b=140
   CASE("81",  "palegoldenrod")         ;  newname="palegoldenrod"         ;  r=238  ;  g=232  ;  b=170
   CASE("82",  "lightgoldenrodyellow")  ;  newname="lightgoldenrodyellow"  ;  r=250  ;  g=250  ;  b=210
   CASE("83",  "lightyellow")           ;  newname="lightyellow"           ;  r=255  ;  g=255  ;  b=224
   CASE("84",  "yellow")                ;  newname="yellow"                ;  r=255  ;  g=255  ;  b=0
   CASE("85",  "gold")                  ;  newname="gold"                  ;  r=255  ;  g=215  ;  b=0
   CASE("86",  "lightgoldenrod")        ;  newname="lightgoldenrod"        ;  r=238  ;  g=221  ;  b=130
   CASE("87",  "goldenrod")             ;  newname="goldenrod"             ;  r=218  ;  g=165  ;  b=32
   CASE("88",  "darkgoldenrod")         ;  newname="darkgoldenrod"         ;  r=184  ;  g=134  ;  b=11
   CASE("89",  "rosybrown")             ;  newname="rosybrown"             ;  r=188  ;  g=143  ;  b=143
   CASE("90",  "indianred")             ;  newname="indianred"             ;  r=205  ;  g=92   ;  b=92
   CASE("91",  "saddlebrown")           ;  newname="saddlebrown"           ;  r=139  ;  g=69   ;  b=19
   CASE("92",  "sienna")                ;  newname="sienna"                ;  r=160  ;  g=82   ;  b=45
   CASE("93",  "peru")                  ;  newname="peru"                  ;  r=205  ;  g=133  ;  b=63
   CASE("94",  "burlywood")             ;  newname="burlywood"             ;  r=222  ;  g=184  ;  b=135
   CASE("95",  "beige")                 ;  newname="beige"                 ;  r=245  ;  g=245  ;  b=220
   CASE("96",  "wheat")                 ;  newname="wheat"                 ;  r=245  ;  g=222  ;  b=179
   CASE("97",  "sandybrown")            ;  newname="sandybrown"            ;  r=244  ;  g=164  ;  b=96
   CASE("98",  "tan")                   ;  newname="tan"                   ;  r=210  ;  g=180  ;  b=140
   CASE("99",  "chocolate")             ;  newname="chocolate"             ;  r=210  ;  g=105  ;  b=30
   CASE("100", "firebrick")             ;  newname="firebrick"             ;  r=178  ;  g=34   ;  b=34
   CASE("101", "brown")                 ;  newname="brown"                 ;  r=165  ;  g=42   ;  b=42
   CASE("102", "darksalmon")            ;  newname="darksalmon"            ;  r=233  ;  g=150  ;  b=122
   CASE("103", "salmon")                ;  newname="salmon"                ;  r=250  ;  g=128  ;  b=114
   CASE("104", "lightsalmon")           ;  newname="lightsalmon"           ;  r=255  ;  g=160  ;  b=122
   CASE("105", "orange")                ;  newname="orange"                ;  r=255  ;  g=165  ;  b=0
   CASE("106", "darkorange")            ;  newname="darkorange"            ;  r=255  ;  g=140  ;  b=0
   CASE("107", "coral")                 ;  newname="coral"                 ;  r=255  ;  g=127  ;  b=80
   CASE("108", "lightcoral")            ;  newname="lightcoral"            ;  r=240  ;  g=128  ;  b=128
   CASE("109", "tomato")                ;  newname="tomato"                ;  r=255  ;  g=99   ;  b=71
   CASE("110", "orangered")             ;  newname="orangered"             ;  r=255  ;  g=69   ;  b=0
   CASE("111", "red")                   ;  newname="red"                   ;  r=255  ;  g=0    ;  b=0
   CASE("116", "palevioletred")         ;  newname="palevioletred"         ;  r=219  ;  g=112  ;  b=147
   CASE("117", "maroon")                ;  newname="maroon"                ;  r=176  ;  g=48   ;  b=96
   CASE("118", "mediumvioletred")       ;  newname="mediumvioletred"       ;  r=199  ;  g=21   ;  b=133
   CASE("119", "violetred")             ;  newname="violetred"             ;  r=208  ;  g=32   ;  b=144
   CASE("120", "magenta")               ;  newname="magenta"               ;  r=255  ;  g=0    ;  b=255
   CASE("121", "violet")                ;  newname="violet"                ;  r=238  ;  g=130  ;  b=238
   CASE("122", "plum")                  ;  newname="plum"                  ;  r=221  ;  g=160  ;  b=221
   CASE("123", "orchid")                ;  newname="orchid"                ;  r=218  ;  g=112  ;  b=214
   CASE("124", "mediumorchid")          ;  newname="mediumorchid"          ;  r=186  ;  g=85   ;  b=211
   CASE("125", "darkorchid")            ;  newname="darkorchid"            ;  r=153  ;  g=50   ;  b=204
   CASE("126", "darkviolet")            ;  newname="darkviolet"            ;  r=148  ;  g=0    ;  b=211
   CASE("127", "blueviolet")            ;  newname="blueviolet"            ;  r=138  ;  g=43   ;  b=226
   CASE("128", "purple")                ;  newname="purple"                ;  r=160  ;  g=32   ;  b=240
   CASE("129", "mediumpurple")          ;  newname="mediumpurple"          ;  r=147  ;  g=112  ;  b=219
   CASE("130", "thistle")               ;  newname="thistle"               ;  r=216  ;  g=191  ;  b=216
   CASE("131", "snow1")                 ;  newname="snow1"                 ;  r=255  ;  g=250  ;  b=250
   CASE("132", "snow2")                 ;  newname="snow2"                 ;  r=238  ;  g=233  ;  b=233
   CASE("133", "snow3")                 ;  newname="snow3"                 ;  r=205  ;  g=201  ;  b=201
   CASE("134", "snow4")                 ;  newname="snow4"                 ;  r=139  ;  g=137  ;  b=137
   CASE("135", "seashell1")             ;  newname="seashell1"             ;  r=255  ;  g=245  ;  b=238
   CASE("136", "seashell2")             ;  newname="seashell2"             ;  r=238  ;  g=229  ;  b=222
   CASE("137", "seashell3")             ;  newname="seashell3"             ;  r=205  ;  g=197  ;  b=191
   CASE("138", "seashell4")             ;  newname="seashell4"             ;  r=139  ;  g=134  ;  b=130
   CASE("139", "antiquewhite1")         ;  newname="antiquewhite1"         ;  r=255  ;  g=239  ;  b=219
   CASE("140", "antiquewhite2")         ;  newname="antiquewhite2"         ;  r=238  ;  g=223  ;  b=204
   CASE("141", "antiquewhite3")         ;  newname="antiquewhite3"         ;  r=205  ;  g=192  ;  b=176
   CASE("142", "antiquewhite4")         ;  newname="antiquewhite4"         ;  r=139  ;  g=131  ;  b=120
   CASE("143", "bisque1")               ;  newname="bisque1"               ;  r=255  ;  g=228  ;  b=196
   CASE("144", "bisque2")               ;  newname="bisque2"               ;  r=238  ;  g=213  ;  b=183
   CASE("145", "bisque3")               ;  newname="bisque3"               ;  r=205  ;  g=183  ;  b=158
   CASE("146", "bisque4")               ;  newname="bisque4"               ;  r=139  ;  g=125  ;  b=107
   CASE("147", "peachpuff1")            ;  newname="peachpuff1"            ;  r=255  ;  g=218  ;  b=185
   CASE("148", "peachpuff2")            ;  newname="peachpuff2"            ;  r=238  ;  g=203  ;  b=173
   CASE("149", "peachpuff3")            ;  newname="peachpuff3"            ;  r=205  ;  g=175  ;  b=149
   CASE("150", "peachpuff4")            ;  newname="peachpuff4"            ;  r=139  ;  g=119  ;  b=101
   CASE("151", "navajowhite1")          ;  newname="navajowhite1"          ;  r=255  ;  g=222  ;  b=173
   CASE("152", "navajowhite2")          ;  newname="navajowhite2"          ;  r=238  ;  g=207  ;  b=161
   CASE("153", "navajowhite3")          ;  newname="navajowhite3"          ;  r=205  ;  g=179  ;  b=139
   CASE("154", "navajowhite4")          ;  newname="navajowhite4"          ;  r=139  ;  g=121  ;  b=94
   CASE("155", "lemonchiffon1")         ;  newname="lemonchiffon1"         ;  r=255  ;  g=250  ;  b=205
   CASE("156", "lemonchiffon2")         ;  newname="lemonchiffon2"         ;  r=238  ;  g=233  ;  b=191
   CASE("157", "lemonchiffon3")         ;  newname="lemonchiffon3"         ;  r=205  ;  g=201  ;  b=165
   CASE("158", "lemonchiffon4")         ;  newname="lemonchiffon4"         ;  r=139  ;  g=137  ;  b=112
   CASE("159", "cornsilk1")             ;  newname="cornsilk1"             ;  r=255  ;  g=248  ;  b=220
   CASE("160", "cornsilk2")             ;  newname="cornsilk2"             ;  r=238  ;  g=232  ;  b=205
   CASE("161", "cornsilk3")             ;  newname="cornsilk3"             ;  r=205  ;  g=200  ;  b=177
   CASE("162", "cornsilk4")             ;  newname="cornsilk4"             ;  r=139  ;  g=136  ;  b=120
   CASE("163", "ivory1")                ;  newname="ivory1"                ;  r=255  ;  g=255  ;  b=240
   CASE("164", "ivory2")                ;  newname="ivory2"                ;  r=238  ;  g=238  ;  b=224
   CASE("165", "ivory3")                ;  newname="ivory3"                ;  r=205  ;  g=205  ;  b=193
   CASE("166", "ivory4")                ;  newname="ivory4"                ;  r=139  ;  g=139  ;  b=131
   CASE("167", "honeydew1")             ;  newname="honeydew1"             ;  r=240  ;  g=255  ;  b=240
   CASE("168", "honeydew2")             ;  newname="honeydew2"             ;  r=224  ;  g=238  ;  b=224
   CASE("169", "honeydew3")             ;  newname="honeydew3"             ;  r=193  ;  g=205  ;  b=193
   CASE("170", "honeydew4")             ;  newname="honeydew4"             ;  r=131  ;  g=139  ;  b=131
   CASE("171", "lavenderblush1")        ;  newname="lavenderblush1"        ;  r=255  ;  g=240  ;  b=245
   CASE("172", "lavenderblush2")        ;  newname="lavenderblush2"        ;  r=238  ;  g=224  ;  b=229
   CASE("173", "lavenderblush3")        ;  newname="lavenderblush3"        ;  r=205  ;  g=193  ;  b=197
   CASE("174", "lavenderblush4")        ;  newname="lavenderblush4"        ;  r=139  ;  g=131  ;  b=134
   CASE("175", "mistyrose1")            ;  newname="mistyrose1"            ;  r=255  ;  g=228  ;  b=225
   CASE("176", "mistyrose2")            ;  newname="mistyrose2"            ;  r=238  ;  g=213  ;  b=210
   CASE("177", "mistyrose3")            ;  newname="mistyrose3"            ;  r=205  ;  g=183  ;  b=181
   CASE("178", "mistyrose4")            ;  newname="mistyrose4"            ;  r=139  ;  g=125  ;  b=123
   CASE("179", "azure1")                ;  newname="azure1"                ;  r=240  ;  g=255  ;  b=255
   CASE("180", "azure2")                ;  newname="azure2"                ;  r=224  ;  g=238  ;  b=238
   CASE("181", "azure3")                ;  newname="azure3"                ;  r=193  ;  g=205  ;  b=205
   CASE("182", "azure4")                ;  newname="azure4"                ;  r=131  ;  g=139  ;  b=139
   CASE("183", "slateblue1")            ;  newname="slateblue1"            ;  r=131  ;  g=111  ;  b=255
   CASE("184", "slateblue2")            ;  newname="slateblue2"            ;  r=122  ;  g=103  ;  b=238
   CASE("185", "slateblue3")            ;  newname="slateblue3"            ;  r=105  ;  g=89   ;  b=205
   CASE("186", "slateblue4")            ;  newname="slateblue4"            ;  r=71   ;  g=60   ;  b=139
   CASE("187", "royalblue1")            ;  newname="royalblue1"            ;  r=72   ;  g=118  ;  b=255
   CASE("188", "royalblue2")            ;  newname="royalblue2"            ;  r=67   ;  g=110  ;  b=238
   CASE("189", "royalblue3")            ;  newname="royalblue3"            ;  r=58   ;  g=95   ;  b=205
   CASE("190", "royalblue4")            ;  newname="royalblue4"            ;  r=39   ;  g=64   ;  b=139
   CASE("191", "blue1")                 ;  newname="blue1"                 ;  r=0    ;  g=0    ;  b=255
   CASE("192", "blue2")                 ;  newname="blue2"                 ;  r=0    ;  g=0    ;  b=238
   CASE("193", "blue3")                 ;  newname="blue3"                 ;  r=0    ;  g=0    ;  b=205
   CASE("194", "blue4")                 ;  newname="blue4"                 ;  r=0    ;  g=0    ;  b=139
   CASE("195", "dodgerblue1")           ;  newname="dodgerblue1"           ;  r=30   ;  g=144  ;  b=255
   CASE("196", "dodgerblue2")           ;  newname="dodgerblue2"           ;  r=28   ;  g=134  ;  b=238
   CASE("197", "dodgerblue3")           ;  newname="dodgerblue3"           ;  r=24   ;  g=116  ;  b=205
   CASE("198", "dodgerblue4")           ;  newname="dodgerblue4"           ;  r=16   ;  g=78   ;  b=139
   CASE("199", "steelblue1")            ;  newname="steelblue1"            ;  r=99   ;  g=184  ;  b=255
   CASE("200", "steelblue2")            ;  newname="steelblue2"            ;  r=92   ;  g=172  ;  b=238
   CASE("201", "steelblue3")            ;  newname="steelblue3"            ;  r=79   ;  g=148  ;  b=205
   CASE("202", "steelblue4")            ;  newname="steelblue4"            ;  r=54   ;  g=100  ;  b=139
   CASE("203", "deepskyblue1")          ;  newname="deepskyblue1"          ;  r=0    ;  g=191  ;  b=255
   CASE("204", "deepskyblue2")          ;  newname="deepskyblue2"          ;  r=0    ;  g=178  ;  b=238
   CASE("205", "deepskyblue3")          ;  newname="deepskyblue3"          ;  r=0    ;  g=154  ;  b=205
   CASE("206", "deepskyblue4")          ;  newname="deepskyblue4"          ;  r=0    ;  g=104  ;  b=139
   CASE("207", "skyblue1")              ;  newname="skyblue1"              ;  r=135  ;  g=206  ;  b=255
   CASE("208", "skyblue2")              ;  newname="skyblue2"              ;  r=126  ;  g=192  ;  b=238
   CASE("209", "skyblue3")              ;  newname="skyblue3"              ;  r=108  ;  g=166  ;  b=205
   CASE("210", "skyblue4")              ;  newname="skyblue4"              ;  r=74   ;  g=112  ;  b=139
   CASE("211", "lightskyblue1")         ;  newname="lightskyblue1"         ;  r=176  ;  g=226  ;  b=255
   CASE("212", "lightskyblue2")         ;  newname="lightskyblue2"         ;  r=164  ;  g=211  ;  b=238
   CASE("213", "lightskyblue3")         ;  newname="lightskyblue3"         ;  r=141  ;  g=182  ;  b=205
   CASE("214", "lightskyblue4")         ;  newname="lightskyblue4"         ;  r=96   ;  g=123  ;  b=139
   CASE("215", "slategray1")            ;  newname="slategray1"            ;  r=198  ;  g=226  ;  b=255
   CASE("216", "slategray2")            ;  newname="slategray2"            ;  r=185  ;  g=211  ;  b=238
   CASE("217", "slategray3")            ;  newname="slategray3"            ;  r=159  ;  g=182  ;  b=205
   CASE("218", "slategray4")            ;  newname="slategray4"            ;  r=108  ;  g=123  ;  b=139
   CASE("219", "lightsteelblue1")       ;  newname="lightsteelblue1"       ;  r=202  ;  g=225  ;  b=255
   CASE("220", "lightsteelblue2")       ;  newname="lightsteelblue2"       ;  r=188  ;  g=210  ;  b=238
   CASE("221", "lightsteelblue3")       ;  newname="lightsteelblue3"       ;  r=162  ;  g=181  ;  b=205
   CASE("222", "lightsteelblue4")       ;  newname="lightsteelblue4"       ;  r=110  ;  g=123  ;  b=139
   CASE("223", "lightblue1")            ;  newname="lightblue1"            ;  r=191  ;  g=239  ;  b=255
   CASE("224", "lightblue2")            ;  newname="lightblue2"            ;  r=178  ;  g=223  ;  b=238
   CASE("225", "lightblue3")            ;  newname="lightblue3"            ;  r=154  ;  g=192  ;  b=205
   CASE("226", "lightblue4")            ;  newname="lightblue4"            ;  r=104  ;  g=131  ;  b=139
   CASE("227", "lightcyan1")            ;  newname="lightcyan1"            ;  r=224  ;  g=255  ;  b=255
   CASE("228", "lightcyan2")            ;  newname="lightcyan2"            ;  r=209  ;  g=238  ;  b=238
   CASE("229", "lightcyan3")            ;  newname="lightcyan3"            ;  r=180  ;  g=205  ;  b=205
   CASE("230", "lightcyan4")            ;  newname="lightcyan4"            ;  r=122  ;  g=139  ;  b=139
   CASE("231", "paleturquoise1")        ;  newname="paleturquoise1"        ;  r=187  ;  g=255  ;  b=255
   CASE("232", "paleturquoise2")        ;  newname="paleturquoise2"        ;  r=174  ;  g=238  ;  b=238
   CASE("233", "paleturquoise3")        ;  newname="paleturquoise3"        ;  r=150  ;  g=205  ;  b=205
   CASE("234", "paleturquoise4")        ;  newname="paleturquoise4"        ;  r=102  ;  g=139  ;  b=139
   CASE("235", "cadetblue1")            ;  newname="cadetblue1"            ;  r=152  ;  g=245  ;  b=255
   CASE("236", "cadetblue2")            ;  newname="cadetblue2"            ;  r=142  ;  g=229  ;  b=238
   CASE("237", "cadetblue3")            ;  newname="cadetblue3"            ;  r=122  ;  g=197  ;  b=205
   CASE("238", "cadetblue4")            ;  newname="cadetblue4"            ;  r=83   ;  g=134  ;  b=139
   CASE("239", "turquoise1")            ;  newname="turquoise1"            ;  r=0    ;  g=245  ;  b=255
   CASE("240", "turquoise2")            ;  newname="turquoise2"            ;  r=0    ;  g=229  ;  b=238
   CASE("241", "turquoise3")            ;  newname="turquoise3"            ;  r=0    ;  g=197  ;  b=205
   CASE("242", "turquoise4")            ;  newname="turquoise4"            ;  r=0    ;  g=134  ;  b=139
   CASE("243", "cyan1")                 ;  newname="cyan1"                 ;  r=0    ;  g=255  ;  b=255
   CASE("244", "cyan2")                 ;  newname="cyan2"                 ;  r=0    ;  g=238  ;  b=238
   CASE("245", "cyan3")                 ;  newname="cyan3"                 ;  r=0    ;  g=205  ;  b=205
   CASE("246", "cyan4")                 ;  newname="cyan4"                 ;  r=0    ;  g=139  ;  b=139
   CASE("247", "darkslategray1")        ;  newname="darkslategray1"        ;  r=151  ;  g=255  ;  b=255
   CASE("248", "darkslategray2")        ;  newname="darkslategray2"        ;  r=141  ;  g=238  ;  b=238
   CASE("249", "darkslategray3")        ;  newname="darkslategray3"        ;  r=121  ;  g=205  ;  b=205
   CASE("250", "darkslategray4")        ;  newname="darkslategray4"        ;  r=82   ;  g=139  ;  b=139
   CASE("251", "aquamarine1")           ;  newname="aquamarine1"           ;  r=127  ;  g=255  ;  b=212
   CASE("252", "aquamarine2")           ;  newname="aquamarine2"           ;  r=118  ;  g=238  ;  b=198
   CASE("253", "aquamarine3")           ;  newname="aquamarine3"           ;  r=102  ;  g=205  ;  b=170
   CASE("254", "aquamarine4")           ;  newname="aquamarine4"           ;  r=69   ;  g=139  ;  b=116
   CASE("255", "darkseagreen1")         ;  newname="darkseagreen1"         ;  r=193  ;  g=255  ;  b=193
   CASE("256", "darkseagreen2")         ;  newname="darkseagreen2"         ;  r=180  ;  g=238  ;  b=180
   CASE("257", "darkseagreen3")         ;  newname="darkseagreen3"         ;  r=155  ;  g=205  ;  b=155
   CASE("258", "darkseagreen4")         ;  newname="darkseagreen4"         ;  r=105  ;  g=139  ;  b=105
   CASE("259", "seagreen1")             ;  newname="seagreen1"             ;  r=84   ;  g=255  ;  b=159
   CASE("260", "seagreen2")             ;  newname="seagreen2"             ;  r=78   ;  g=238  ;  b=148
   CASE("261", "seagreen3")             ;  newname="seagreen3"             ;  r=67   ;  g=205  ;  b=128
   CASE("262", "seagreen4")             ;  newname="seagreen4"             ;  r=46   ;  g=139  ;  b=87
   CASE("263", "palegreen1")            ;  newname="palegreen1"            ;  r=154  ;  g=255  ;  b=154
   CASE("264", "palegreen2")            ;  newname="palegreen2"            ;  r=144  ;  g=238  ;  b=144
   CASE("265", "palegreen3")            ;  newname="palegreen3"            ;  r=124  ;  g=205  ;  b=124
   CASE("266", "palegreen4")            ;  newname="palegreen4"            ;  r=84   ;  g=139  ;  b=84
   CASE("267", "springgreen1")          ;  newname="springgreen1"          ;  r=0    ;  g=255  ;  b=127
   CASE("268", "springgreen2")          ;  newname="springgreen2"          ;  r=0    ;  g=238  ;  b=118
   CASE("269", "springgreen3")          ;  newname="springgreen3"          ;  r=0    ;  g=205  ;  b=102
   CASE("270", "springgreen4")          ;  newname="springgreen4"          ;  r=0    ;  g=139  ;  b=69
   CASE("271", "green1")                ;  newname="green1"                ;  r=0    ;  g=255  ;  b=0
   CASE("272", "green2")                ;  newname="green2"                ;  r=0    ;  g=238  ;  b=0
   CASE("273", "green3")                ;  newname="green3"                ;  r=0    ;  g=205  ;  b=0
   CASE("274", "green4")                ;  newname="green4"                ;  r=0    ;  g=139  ;  b=0
   CASE("275", "chartreuse1")           ;  newname="chartreuse1"           ;  r=127  ;  g=255  ;  b=0
   CASE("276", "chartreuse2")           ;  newname="chartreuse2"           ;  r=118  ;  g=238  ;  b=0
   CASE("277", "chartreuse3")           ;  newname="chartreuse3"           ;  r=102  ;  g=205  ;  b=0
   CASE("278", "chartreuse4")           ;  newname="chartreuse4"           ;  r=69   ;  g=139  ;  b=0
   CASE("279", "olivedrab1")            ;  newname="olivedrab1"            ;  r=192  ;  g=255  ;  b=62
   CASE("280", "olivedrab2")            ;  newname="olivedrab2"            ;  r=179  ;  g=238  ;  b=58
   CASE("281", "olivedrab3")            ;  newname="olivedrab3"            ;  r=154  ;  g=205  ;  b=50
   CASE("282", "olivedrab4")            ;  newname="olivedrab4"            ;  r=105  ;  g=139  ;  b=34
   CASE("283", "darkolivegreen1")       ;  newname="darkolivegreen1"       ;  r=202  ;  g=255  ;  b=112
   CASE("284", "darkolivegreen2")       ;  newname="darkolivegreen2"       ;  r=188  ;  g=238  ;  b=104
   CASE("285", "darkolivegreen3")       ;  newname="darkolivegreen3"       ;  r=162  ;  g=205  ;  b=90
   CASE("286", "darkolivegreen4")       ;  newname="darkolivegreen4"       ;  r=110  ;  g=139  ;  b=61
   CASE("287", "khaki1")                ;  newname="khaki1"                ;  r=255  ;  g=246  ;  b=143
   CASE("288", "khaki2")                ;  newname="khaki2"                ;  r=238  ;  g=230  ;  b=133
   CASE("289", "khaki3")                ;  newname="khaki3"                ;  r=205  ;  g=198  ;  b=115
   CASE("290", "khaki4")                ;  newname="khaki4"                ;  r=139  ;  g=134  ;  b=78
   CASE("291", "lightgoldenrod1")       ;  newname="lightgoldenrod1"       ;  r=255  ;  g=236  ;  b=139
   CASE("292", "lightgoldenrod2")       ;  newname="lightgoldenrod2"       ;  r=238  ;  g=220  ;  b=130
   CASE("293", "lightgoldenrod3")       ;  newname="lightgoldenrod3"       ;  r=205  ;  g=190  ;  b=112
   CASE("294", "lightgoldenrod4")       ;  newname="lightgoldenrod4"       ;  r=139  ;  g=129  ;  b=76
   CASE("295", "lightyellow1")          ;  newname="lightyellow1"          ;  r=255  ;  g=255  ;  b=224
   CASE("296", "lightyellow2")          ;  newname="lightyellow2"          ;  r=238  ;  g=238  ;  b=209
   CASE("297", "lightyellow3")          ;  newname="lightyellow3"          ;  r=205  ;  g=205  ;  b=180
   CASE("298", "lightyellow4")          ;  newname="lightyellow4"          ;  r=139  ;  g=139  ;  b=122
   CASE("299", "yellow1")               ;  newname="yellow1"               ;  r=255  ;  g=255  ;  b=0
   CASE("300", "yellow2")               ;  newname="yellow2"               ;  r=238  ;  g=238  ;  b=0
   CASE("301", "yellow3")               ;  newname="yellow3"               ;  r=205  ;  g=205  ;  b=0
   CASE("302", "yellow4")               ;  newname="yellow4"               ;  r=139  ;  g=139  ;  b=0
   CASE("303", "gold1")                 ;  newname="gold1"                 ;  r=255  ;  g=215  ;  b=0
   CASE("304", "gold2")                 ;  newname="gold2"                 ;  r=238  ;  g=201  ;  b=0
   CASE("305", "gold3")                 ;  newname="gold3"                 ;  r=205  ;  g=173  ;  b=0
   CASE("306", "gold4")                 ;  newname="gold4"                 ;  r=139  ;  g=117  ;  b=0
   CASE("307", "goldenrod1")            ;  newname="goldenrod1"            ;  r=255  ;  g=193  ;  b=37
   CASE("308", "goldenrod2")            ;  newname="goldenrod2"            ;  r=238  ;  g=180  ;  b=34
   CASE("309", "goldenrod3")            ;  newname="goldenrod3"            ;  r=205  ;  g=155  ;  b=29
   CASE("310", "goldenrod4")            ;  newname="goldenrod4"            ;  r=139  ;  g=105  ;  b=20
   CASE("311", "darkgoldenrod1")        ;  newname="darkgoldenrod1"        ;  r=255  ;  g=185  ;  b=15
   CASE("312", "darkgoldenrod2")        ;  newname="darkgoldenrod2"        ;  r=238  ;  g=173  ;  b=14
   CASE("313", "darkgoldenrod3")        ;  newname="darkgoldenrod3"        ;  r=205  ;  g=149  ;  b=12
   CASE("314", "darkgoldenrod4")        ;  newname="darkgoldenrod4"        ;  r=139  ;  g=101  ;  b=8
   CASE("315", "rosybrown1")            ;  newname="rosybrown1"            ;  r=255  ;  g=193  ;  b=193
   CASE("316", "rosybrown2")            ;  newname="rosybrown2"            ;  r=238  ;  g=180  ;  b=180
   CASE("317", "rosybrown3")            ;  newname="rosybrown3"            ;  r=205  ;  g=155  ;  b=155
   CASE("318", "rosybrown4")            ;  newname="rosybrown4"            ;  r=139  ;  g=105  ;  b=105
   CASE("319", "indianred1")            ;  newname="indianred1"            ;  r=255  ;  g=106  ;  b=106
   CASE("320", "indianred2")            ;  newname="indianred2"            ;  r=238  ;  g=99   ;  b=99
   CASE("321", "indianred3")            ;  newname="indianred3"            ;  r=205  ;  g=85   ;  b=85
   CASE("322", "indianred4")            ;  newname="indianred4"            ;  r=139  ;  g=58   ;  b=58
   CASE("323", "sienna1")               ;  newname="sienna1"               ;  r=255  ;  g=130  ;  b=71
   CASE("324", "sienna2")               ;  newname="sienna2"               ;  r=238  ;  g=121  ;  b=66
   CASE("325", "sienna3")               ;  newname="sienna3"               ;  r=205  ;  g=104  ;  b=57
   CASE("326", "sienna4")               ;  newname="sienna4"               ;  r=139  ;  g=71   ;  b=38
   CASE("327", "burlywood1")            ;  newname="burlywood1"            ;  r=255  ;  g=211  ;  b=155
   CASE("328", "burlywood2")            ;  newname="burlywood2"            ;  r=238  ;  g=197  ;  b=145
   CASE("329", "burlywood3")            ;  newname="burlywood3"            ;  r=205  ;  g=170  ;  b=125
   CASE("330", "burlywood4")            ;  newname="burlywood4"            ;  r=139  ;  g=115  ;  b=85
   CASE("331", "wheat1")                ;  newname="wheat1"                ;  r=255  ;  g=231  ;  b=186
   CASE("332", "wheat2")                ;  newname="wheat2"                ;  r=238  ;  g=216  ;  b=174
   CASE("333", "wheat3")                ;  newname="wheat3"                ;  r=205  ;  g=186  ;  b=150
   CASE("334", "wheat4")                ;  newname="wheat4"                ;  r=139  ;  g=126  ;  b=102
   CASE("335", "tan1")                  ;  newname="tan1"                  ;  r=255  ;  g=165  ;  b=79
   CASE("336", "tan2")                  ;  newname="tan2"                  ;  r=238  ;  g=154  ;  b=73
   CASE("337", "tan3")                  ;  newname="tan3"                  ;  r=205  ;  g=133  ;  b=63
   CASE("338", "tan4")                  ;  newname="tan4"                  ;  r=139  ;  g=90   ;  b=43
   CASE("339", "chocolate1")            ;  newname="chocolate1"            ;  r=255  ;  g=127  ;  b=36
   CASE("340", "chocolate2")            ;  newname="chocolate2"            ;  r=238  ;  g=118  ;  b=33
   CASE("341", "chocolate3")            ;  newname="chocolate3"            ;  r=205  ;  g=102  ;  b=29
   CASE("342", "chocolate4")            ;  newname="chocolate4"            ;  r=139  ;  g=69   ;  b=19
   CASE("343", "firebrick1")            ;  newname="firebrick1"            ;  r=255  ;  g=48   ;  b=48
   CASE("344", "firebrick2")            ;  newname="firebrick2"            ;  r=238  ;  g=44   ;  b=44
   CASE("345", "firebrick3")            ;  newname="firebrick3"            ;  r=205  ;  g=38   ;  b=38
   CASE("346", "firebrick4")            ;  newname="firebrick4"            ;  r=139  ;  g=26   ;  b=26
   CASE("347", "brown1")                ;  newname="brown1"                ;  r=255  ;  g=64   ;  b=64
   CASE("348", "brown2")                ;  newname="brown2"                ;  r=238  ;  g=59   ;  b=59
   CASE("349", "brown3")                ;  newname="brown3"                ;  r=205  ;  g=51   ;  b=51
   CASE("350", "brown4")                ;  newname="brown4"                ;  r=139  ;  g=35   ;  b=35
   CASE("351", "salmon1")               ;  newname="salmon1"               ;  r=255  ;  g=140  ;  b=105
   CASE("352", "salmon2")               ;  newname="salmon2"               ;  r=238  ;  g=130  ;  b=98
   CASE("353", "salmon3")               ;  newname="salmon3"               ;  r=205  ;  g=112  ;  b=84
   CASE("354", "salmon4")               ;  newname="salmon4"               ;  r=139  ;  g=76   ;  b=57
   CASE("355", "lightsalmon1")          ;  newname="lightsalmon1"          ;  r=255  ;  g=160  ;  b=122
   CASE("356", "lightsalmon2")          ;  newname="lightsalmon2"          ;  r=238  ;  g=149  ;  b=114
   CASE("357", "lightsalmon3")          ;  newname="lightsalmon3"          ;  r=205  ;  g=129  ;  b=98
   CASE("358", "lightsalmon4")          ;  newname="lightsalmon4"          ;  r=139  ;  g=87   ;  b=66
   CASE("359", "orange1")               ;  newname="orange1"               ;  r=255  ;  g=165  ;  b=0
   CASE("360", "orange2")               ;  newname="orange2"               ;  r=238  ;  g=154  ;  b=0
   CASE("361", "orange3")               ;  newname="orange3"               ;  r=205  ;  g=133  ;  b=0
   CASE("362", "orange4")               ;  newname="orange4"               ;  r=139  ;  g=90   ;  b=0
   CASE("363", "darkorange1")           ;  newname="darkorange1"           ;  r=255  ;  g=127  ;  b=0
   CASE("364", "darkorange2")           ;  newname="darkorange2"           ;  r=238  ;  g=118  ;  b=0
   CASE("365", "darkorange3")           ;  newname="darkorange3"           ;  r=205  ;  g=102  ;  b=0
   CASE("366", "darkorange4")           ;  newname="darkorange4"           ;  r=139  ;  g=69   ;  b=0
   CASE("367", "coral1")                ;  newname="coral1"                ;  r=255  ;  g=114  ;  b=86
   CASE("368", "coral2")                ;  newname="coral2"                ;  r=238  ;  g=106  ;  b=80
   CASE("369", "coral3")                ;  newname="coral3"                ;  r=205  ;  g=91   ;  b=69
   CASE("370", "coral4")                ;  newname="coral4"                ;  r=139  ;  g=62   ;  b=47
   CASE("371", "tomato1")               ;  newname="tomato1"               ;  r=255  ;  g=99   ;  b=71
   CASE("372", "tomato2")               ;  newname="tomato2"               ;  r=238  ;  g=92   ;  b=66
   CASE("373", "tomato3")               ;  newname="tomato3"               ;  r=205  ;  g=79   ;  b=57
   CASE("374", "tomato4")               ;  newname="tomato4"               ;  r=139  ;  g=54   ;  b=38
   CASE("375", "orangered1")            ;  newname="orangered1"            ;  r=255  ;  g=69   ;  b=0
   CASE("376", "orangered2")            ;  newname="orangered2"            ;  r=238  ;  g=64   ;  b=0
   CASE("377", "orangered3")            ;  newname="orangered3"            ;  r=205  ;  g=55   ;  b=0
   CASE("378", "orangered4")            ;  newname="orangered4"            ;  r=139  ;  g=37   ;  b=0
   CASE("379", "red1")                  ;  newname="red1"                  ;  r=255  ;  g=0    ;  b=0
   CASE("380", "red2")                  ;  newname="red2"                  ;  r=238  ;  g=0    ;  b=0
   CASE("381", "red3")                  ;  newname="red3"                  ;  r=205  ;  g=0    ;  b=0
   CASE("382", "red4")                  ;  newname="red4"                  ;  r=139  ;  g=0    ;  b=0
   CASE("112", "hotpink")               ;  newname="hotpink"               ;  r=255  ;  g=105  ;  b=180
   CASE("113", "deeppink")              ;  newname="deeppink"              ;  r=255  ;  g=20   ;  b=147
   CASE("115", "lightpink")             ;  newname="lightpink"             ;  r=255  ;  g=182  ;  b=193
   CASE("383", "deeppink1")             ;  newname="deeppink1"             ;  r=255  ;  g=20   ;  b=147
   CASE("384", "deeppink2")             ;  newname="deeppink2"             ;  r=238  ;  g=18   ;  b=137
   CASE("385", "deeppink3")             ;  newname="deeppink3"             ;  r=205  ;  g=16   ;  b=118
   CASE("386", "deeppink4")             ;  newname="deeppink4"             ;  r=139  ;  g=10   ;  b=80
   CASE("387", "hotpink1")              ;  newname="hotpink1"              ;  r=255  ;  g=110  ;  b=180
   CASE("388", "hotpink2")              ;  newname="hotpink2"              ;  r=238  ;  g=106  ;  b=167
   CASE("389", "hotpink3")              ;  newname="hotpink3"              ;  r=205  ;  g=96   ;  b=144
   CASE("390", "hotpink4")              ;  newname="hotpink4"              ;  r=139  ;  g=58   ;  b=98
   CASE("114", "pink")                  ;  newname="pink"                  ;  r=255  ;  g=192  ;  b=203
   CASE("391", "pink1")                 ;  newname="pink1"                 ;  r=255  ;  g=181  ;  b=197
   CASE("392", "pink2")                 ;  newname="pink2"                 ;  r=238  ;  g=169  ;  b=184
   CASE("393", "pink3")                 ;  newname="pink3"                 ;  r=205  ;  g=145  ;  b=158
   CASE("394", "pink4")                 ;  newname="pink4"                 ;  r=139  ;  g=99   ;  b=108
   CASE("395", "lightpink1")            ;  newname="lightpink1"            ;  r=255  ;  g=174  ;  b=185
   CASE("396", "lightpink2")            ;  newname="lightpink2"            ;  r=238  ;  g=162  ;  b=173
   CASE("397", "lightpink3")            ;  newname="lightpink3"            ;  r=205  ;  g=140  ;  b=149
   CASE("398", "lightpink4")            ;  newname="lightpink4"            ;  r=139  ;  g=95   ;  b=101
   CASE("399", "palevioletred1")        ;  newname="palevioletred1"        ;  r=255  ;  g=130  ;  b=171
   CASE("400", "palevioletred2")        ;  newname="palevioletred2"        ;  r=238  ;  g=121  ;  b=159
   CASE("401", "palevioletred3")        ;  newname="palevioletred3"        ;  r=205  ;  g=104  ;  b=137
   CASE("402", "palevioletred4")        ;  newname="palevioletred4"        ;  r=139  ;  g=71   ;  b=93
   CASE("403", "maroon1")               ;  newname="maroon1"               ;  r=255  ;  g=52   ;  b=179
   CASE("404", "maroon2")               ;  newname="maroon2"               ;  r=238  ;  g=48   ;  b=167
   CASE("405", "maroon3")               ;  newname="maroon3"               ;  r=205  ;  g=41   ;  b=144
   CASE("406", "maroon4")               ;  newname="maroon4"               ;  r=139  ;  g=28   ;  b=98
   CASE("407", "violetred1")            ;  newname="violetred1"            ;  r=255  ;  g=62   ;  b=150
   CASE("408", "violetred2")            ;  newname="violetred2"            ;  r=238  ;  g=58   ;  b=140
   CASE("409", "violetred3")            ;  newname="violetred3"            ;  r=205  ;  g=50   ;  b=120
   CASE("410", "violetred4")            ;  newname="violetred4"            ;  r=139  ;  g=34   ;  b=82
   CASE("411", "magenta1")              ;  newname="magenta1"              ;  r=255  ;  g=0    ;  b=255
   CASE("412", "magenta2")              ;  newname="magenta2"              ;  r=238  ;  g=0    ;  b=238
   CASE("413", "magenta3")              ;  newname="magenta3"              ;  r=205  ;  g=0    ;  b=205
   CASE("414", "magenta4")              ;  newname="magenta4"              ;  r=139  ;  g=0    ;  b=139
   CASE("415", "orchid1")               ;  newname="orchid1"               ;  r=255  ;  g=131  ;  b=250
   CASE("416", "orchid2")               ;  newname="orchid2"               ;  r=238  ;  g=122  ;  b=233
   CASE("417", "orchid3")               ;  newname="orchid3"               ;  r=205  ;  g=105  ;  b=201
   CASE("418", "orchid4")               ;  newname="orchid4"               ;  r=139  ;  g=71   ;  b=137
   CASE("419", "plum1")                 ;  newname="plum1"                 ;  r=255  ;  g=187  ;  b=255
   CASE("420", "plum2")                 ;  newname="plum2"                 ;  r=238  ;  g=174  ;  b=238
   CASE("421", "plum3")                 ;  newname="plum3"                 ;  r=205  ;  g=150  ;  b=205
   CASE("422", "plum4")                 ;  newname="plum4"                 ;  r=139  ;  g=102  ;  b=139
   CASE("423", "mediumorchid1")         ;  newname="mediumorchid1"         ;  r=224  ;  g=102  ;  b=255
   CASE("424", "mediumorchid2")         ;  newname="mediumorchid2"         ;  r=209  ;  g=95   ;  b=238
   CASE("425", "mediumorchid3")         ;  newname="mediumorchid3"         ;  r=180  ;  g=82   ;  b=205
   CASE("426", "mediumorchid4")         ;  newname="mediumorchid4"         ;  r=122  ;  g=55   ;  b=139
   CASE("427", "darkorchid1")           ;  newname="darkorchid1"           ;  r=191  ;  g=62   ;  b=255
   CASE("428", "darkorchid2")           ;  newname="darkorchid2"           ;  r=178  ;  g=58   ;  b=238
   CASE("429", "darkorchid3")           ;  newname="darkorchid3"           ;  r=154  ;  g=50   ;  b=205
   CASE("430", "darkorchid4")           ;  newname="darkorchid4"           ;  r=104  ;  g=34   ;  b=139
   CASE("431", "purple1")               ;  newname="purple1"               ;  r=155  ;  g=48   ;  b=255
   CASE("432", "purple2")               ;  newname="purple2"               ;  r=145  ;  g=44   ;  b=238
   CASE("433", "purple3")               ;  newname="purple3"               ;  r=125  ;  g=38   ;  b=205
   CASE("434", "purple4")               ;  newname="purple4"               ;  r=85   ;  g=26   ;  b=139
   CASE("435", "mediumpurple1")         ;  newname="mediumpurple1"         ;  r=171  ;  g=130  ;  b=255
   CASE("436", "mediumpurple2")         ;  newname="mediumpurple2"         ;  r=159  ;  g=121  ;  b=238
   CASE("437", "mediumpurple3")         ;  newname="mediumpurple3"         ;  r=137  ;  g=104  ;  b=205
   CASE("438", "mediumpurple4")         ;  newname="mediumpurple4"         ;  r=93   ;  g=71   ;  b=139
   CASE("439", "thistle1")              ;  newname="thistle1"              ;  r=255  ;  g=225  ;  b=255
   CASE("440", "thistle2")              ;  newname="thistle2"              ;  r=238  ;  g=210  ;  b=238
   CASE("441", "thistle3")              ;  newname="thistle3"              ;  r=205  ;  g=181  ;  b=205
   CASE("442", "thistle4")              ;  newname="thistle4"              ;  r=139  ;  g=123  ;  b=139
   CASE("443", "gray0")                 ;  newname="gray0"                 ;  r=0    ;  g=0    ;  b=0
   CASE("444", "gray1")                 ;  newname="gray1"                 ;  r=3    ;  g=3    ;  b=3
   CASE("445", "gray2")                 ;  newname="gray2"                 ;  r=5    ;  g=5    ;  b=5
   CASE("446", "gray3")                 ;  newname="gray3"                 ;  r=8    ;  g=8    ;  b=8
   CASE("447", "gray4")                 ;  newname="gray4"                 ;  r=10   ;  g=10   ;  b=10
   CASE("448", "gray5")                 ;  newname="gray5"                 ;  r=13   ;  g=13   ;  b=13
   CASE("449", "gray6")                 ;  newname="gray6"                 ;  r=15   ;  g=15   ;  b=15
   CASE("450", "gray7")                 ;  newname="gray7"                 ;  r=18   ;  g=18   ;  b=18
   CASE("451", "gray8")                 ;  newname="gray8"                 ;  r=20   ;  g=20   ;  b=20
   CASE("452", "gray9")                 ;  newname="gray9"                 ;  r=23   ;  g=23   ;  b=23
   CASE("453", "gray10")                ;  newname="gray10"                ;  r=26   ;  g=26   ;  b=26
   CASE("454", "gray11")                ;  newname="gray11"                ;  r=28   ;  g=28   ;  b=28
   CASE("455", "gray12")                ;  newname="gray12"                ;  r=31   ;  g=31   ;  b=31
   CASE("456", "gray13")                ;  newname="gray13"                ;  r=33   ;  g=33   ;  b=33
   CASE("457", "gray14")                ;  newname="gray14"                ;  r=36   ;  g=36   ;  b=36
   CASE("458", "gray15")                ;  newname="gray15"                ;  r=38   ;  g=38   ;  b=38
   CASE("459", "gray16")                ;  newname="gray16"                ;  r=41   ;  g=41   ;  b=41
   CASE("460", "gray17")                ;  newname="gray17"                ;  r=43   ;  g=43   ;  b=43
   CASE("461", "gray18")                ;  newname="gray18"                ;  r=46   ;  g=46   ;  b=46
   CASE("462", "gray19")                ;  newname="gray19"                ;  r=48   ;  g=48   ;  b=48
   CASE("463", "gray20")                ;  newname="gray20"                ;  r=51   ;  g=51   ;  b=51
   CASE("464", "gray21")                ;  newname="gray21"                ;  r=54   ;  g=54   ;  b=54
   CASE("465", "gray22")                ;  newname="gray22"                ;  r=56   ;  g=56   ;  b=56
   CASE("466", "gray23")                ;  newname="gray23"                ;  r=59   ;  g=59   ;  b=59
   CASE("467", "gray24")                ;  newname="gray24"                ;  r=61   ;  g=61   ;  b=61
   CASE("468", "gray25")                ;  newname="gray25"                ;  r=64   ;  g=64   ;  b=64
   CASE("469", "gray26")                ;  newname="gray26"                ;  r=66   ;  g=66   ;  b=66
   CASE("470", "gray27")                ;  newname="gray27"                ;  r=69   ;  g=69   ;  b=69
   CASE("471", "gray28")                ;  newname="gray28"                ;  r=71   ;  g=71   ;  b=71
   CASE("472", "gray29")                ;  newname="gray29"                ;  r=74   ;  g=74   ;  b=74
   CASE("473", "gray30")                ;  newname="gray30"                ;  r=77   ;  g=77   ;  b=77
   CASE("474", "gray31")                ;  newname="gray31"                ;  r=79   ;  g=79   ;  b=79
   CASE("475", "gray32")                ;  newname="gray32"                ;  r=82   ;  g=82   ;  b=82
   CASE("476", "gray33")                ;  newname="gray33"                ;  r=84   ;  g=84   ;  b=84
   CASE("477", "gray34")                ;  newname="gray34"                ;  r=87   ;  g=87   ;  b=87
   CASE("478", "gray35")                ;  newname="gray35"                ;  r=89   ;  g=89   ;  b=89
   CASE("479", "gray36")                ;  newname="gray36"                ;  r=92   ;  g=92   ;  b=92
   CASE("480", "gray37")                ;  newname="gray37"                ;  r=94   ;  g=94   ;  b=94
   CASE("481", "gray38")                ;  newname="gray38"                ;  r=97   ;  g=97   ;  b=97
   CASE("482", "gray39")                ;  newname="gray39"                ;  r=99   ;  g=99   ;  b=99
   CASE("483", "gray40")                ;  newname="gray40"                ;  r=102  ;  g=102  ;  b=102
   CASE("484", "gray41")                ;  newname="gray41"                ;  r=105  ;  g=105  ;  b=105
   CASE("485", "gray42")                ;  newname="gray42"                ;  r=107  ;  g=107  ;  b=107
   CASE("486", "gray43")                ;  newname="gray43"                ;  r=110  ;  g=110  ;  b=110
   CASE("487", "gray44")                ;  newname="gray44"                ;  r=112  ;  g=112  ;  b=112
   CASE("488", "gray45")                ;  newname="gray45"                ;  r=115  ;  g=115  ;  b=115
   CASE("489", "gray46")                ;  newname="gray46"                ;  r=117  ;  g=117  ;  b=117
   CASE("490", "gray47")                ;  newname="gray47"                ;  r=120  ;  g=120  ;  b=120
   CASE("491", "gray48")                ;  newname="gray48"                ;  r=122  ;  g=122  ;  b=122
   CASE("492", "gray49")                ;  newname="gray49"                ;  r=125  ;  g=125  ;  b=125
   CASE("493", "gray50")                ;  newname="gray50"                ;  r=127  ;  g=127  ;  b=127
   CASE("494", "gray51")                ;  newname="gray51"                ;  r=130  ;  g=130  ;  b=130
   CASE("495", "gray52")                ;  newname="gray52"                ;  r=133  ;  g=133  ;  b=133
   CASE("496", "gray53")                ;  newname="gray53"                ;  r=135  ;  g=135  ;  b=135
   CASE("497", "gray54")                ;  newname="gray54"                ;  r=138  ;  g=138  ;  b=138
   CASE("498", "gray55")                ;  newname="gray55"                ;  r=140  ;  g=140  ;  b=140
   CASE("499", "gray56")                ;  newname="gray56"                ;  r=143  ;  g=143  ;  b=143
   CASE("500", "gray57")                ;  newname="gray57"                ;  r=145  ;  g=145  ;  b=145
   CASE("501", "gray58")                ;  newname="gray58"                ;  r=148  ;  g=148  ;  b=148
   CASE("502", "gray59")                ;  newname="gray59"                ;  r=150  ;  g=150  ;  b=150
   CASE("503", "gray60")                ;  newname="gray60"                ;  r=153  ;  g=153  ;  b=153
   CASE("504", "gray61")                ;  newname="gray61"                ;  r=156  ;  g=156  ;  b=156
   CASE("505", "gray62")                ;  newname="gray62"                ;  r=158  ;  g=158  ;  b=158
   CASE("506", "gray63")                ;  newname="gray63"                ;  r=161  ;  g=161  ;  b=161
   CASE("507", "gray64")                ;  newname="gray64"                ;  r=163  ;  g=163  ;  b=163
   CASE("508", "gray65")                ;  newname="gray65"                ;  r=166  ;  g=166  ;  b=166
   CASE("509", "gray66")                ;  newname="gray66"                ;  r=168  ;  g=168  ;  b=168
   CASE("510", "gray67")                ;  newname="gray67"                ;  r=171  ;  g=171  ;  b=171
   CASE("511", "gray68")                ;  newname="gray68"                ;  r=173  ;  g=173  ;  b=173
   CASE("512", "gray69")                ;  newname="gray69"                ;  r=176  ;  g=176  ;  b=176
   CASE("513", "gray70")                ;  newname="gray70"                ;  r=179  ;  g=179  ;  b=179
   CASE("514", "gray71")                ;  newname="gray71"                ;  r=181  ;  g=181  ;  b=181
   CASE("515", "gray72")                ;  newname="gray72"                ;  r=184  ;  g=184  ;  b=184
   CASE("516", "gray73")                ;  newname="gray73"                ;  r=186  ;  g=186  ;  b=186
   CASE("517", "gray74")                ;  newname="gray74"                ;  r=189  ;  g=189  ;  b=189
   CASE("518", "gray75")                ;  newname="gray75"                ;  r=191  ;  g=191  ;  b=191
   CASE("519", "gray76")                ;  newname="gray76"                ;  r=194  ;  g=194  ;  b=194
   CASE("520", "gray77")                ;  newname="gray77"                ;  r=196  ;  g=196  ;  b=196
   CASE("521", "gray78")                ;  newname="gray78"                ;  r=199  ;  g=199  ;  b=199
   CASE("522", "gray79")                ;  newname="gray79"                ;  r=201  ;  g=201  ;  b=201
   CASE("523", "gray80")                ;  newname="gray80"                ;  r=204  ;  g=204  ;  b=204
   CASE("524", "gray81")                ;  newname="gray81"                ;  r=207  ;  g=207  ;  b=207
   CASE("525", "gray82")                ;  newname="gray82"                ;  r=209  ;  g=209  ;  b=209
   CASE("526", "gray83")                ;  newname="gray83"                ;  r=212  ;  g=212  ;  b=212
   CASE("527", "gray84")                ;  newname="gray84"                ;  r=214  ;  g=214  ;  b=214
   CASE("528", "gray85")                ;  newname="gray85"                ;  r=217  ;  g=217  ;  b=217
   CASE("529", "gray86")                ;  newname="gray86"                ;  r=219  ;  g=219  ;  b=219
   CASE("530", "gray87")                ;  newname="gray87"                ;  r=222  ;  g=222  ;  b=222
   CASE("531", "gray88")                ;  newname="gray88"                ;  r=224  ;  g=224  ;  b=224
   CASE("532", "gray89")                ;  newname="gray89"                ;  r=227  ;  g=227  ;  b=227
   CASE("533", "gray90")                ;  newname="gray90"                ;  r=229  ;  g=229  ;  b=229
   CASE("534", "gray91")                ;  newname="gray91"                ;  r=232  ;  g=232  ;  b=232
   CASE("535", "gray92")                ;  newname="gray92"                ;  r=235  ;  g=235  ;  b=235
   CASE("536", "gray93")                ;  newname="gray93"                ;  r=237  ;  g=237  ;  b=237
   CASE("537", "gray94")                ;  newname="gray94"                ;  r=240  ;  g=240  ;  b=240
   CASE("538", "gray95")                ;  newname="gray95"                ;  r=242  ;  g=242  ;  b=242
   CASE("539", "gray96")                ;  newname="gray96"                ;  r=245  ;  g=245  ;  b=245
   CASE("540", "gray97")                ;  newname="gray97"                ;  r=247  ;  g=247  ;  b=247
   CASE("541", "gray98")                ;  newname="gray98"                ;  r=250  ;  g=250  ;  b=250
   CASE("542", "gray99")                ;  newname="gray99"                ;  r=252  ;  g=252  ;  b=252
   CASE("543", "gray100")               ;  newname="gray100"               ;  r=255  ;  g=255  ;  b=255
   CASE("544", "darkgray")              ;  newname="darkgray"              ;  r=169  ;  g=169  ;  b=169
   CASE("545", "darkblue")              ;  newname="darkblue"              ;  r=0    ;  g=0    ;  b=139
   CASE("546", "darkcyan")              ;  newname="darkcyan"              ;  r=0    ;  g=139  ;  b=139
   CASE("547", "darkmagenta")           ;  newname="darkmagenta"           ;  r=139  ;  g=0    ;  b=139
   CASE("548", "darkred")               ;  newname="darkred"               ;  r=139  ;  g=0    ;  b=0
   CASE("549", "lightgreen")            ;  newname="lightgreen"            ;  r=144  ;  g=238  ;  b=144
   CASE("550", "silver")                ;  newname="silver"                ;  r=192  ;  g=192  ;  b=192
   CASE("551", "teal")                  ;  newname="teal"                  ;  r=0    ;  g=128  ;  b=128
   CASE("552", "olive")                 ;  newname="olive"                 ;  r=128  ;  g=128  ;  b=0
   CASE("553", "lime")                  ;  newname="lime"                  ;  r=0    ;  g=255  ;  b=0
   CASE("554", "aqua")                  ;  newname="aqua"                  ;  r=0    ;  g=255  ;  b=255
   CASE("555", "fuchsia")               ;  newname="fuchsia"               ;  r=255  ;  g=0    ;  b=255

   case default                         ;  newname="Unknown"               ;  r=255  ;  g=255  ;  b=255 ! unknown color name

   END SELECT

   IF(PRESENT(echoname)) THEN
      echoname = newname
   ENDIF
   r=r/2.55; g=g/2.55; b=b/2.55 ! take values from range of 0 to 255 to 0 to 100
END SUBROUTINE color_name2rgb
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
elemental pure function lower(str) result (string)

! ident_15="@(#)M_strings::lower(3f): Changes a string to lowercase over specified range"

character(*), intent(In)     :: str
character(len(str))          :: string
integer                      :: i
   string = str
   do i = 1, len_trim(str) ! step thru each letter in the string
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32) ! change letter to miniscule
      case default
      end select
   end do
end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_color
