[CLI Home Page]

[swirl] Fortran color module M_color

DOWNLOAD:just4.tgz

The download file includes an example utility program colors.f90 with a man(1) page colors.1 that exercises the module.

NAME
    M_COLOR(3f) - [M_color]a Fortran module that lets you convert between common color models
SYNOPSIS

       use M_color, only : &

          & hue, &
          & closest_color_name, &
          & color_name2rgb, &
          & rgbmono

DESCRIPTION

    Highly accurate color conversions are a tricky business, and color is a complex topic; but these simplified conversions between
    common color models work quite well for most basic needs.

    For most uses the only user routine called is HUE(3f) HUE(3f) is a single routine that interfaces to all the private low-level
    color conversion routines to convert a color's components from one color model to another. HUE(3f) converts between the
    following color models:

      + RGB - Red, Green, Blue (color TV monitors)
      + HLS - Hue, Lightness, Saturation
      + CMY - Cyan, Magenta, Yellow (pigment-based printing devices)
      + HSV - Hue, Saturation, Value
      + YIQ - Broadcast TV color system

    In addition to the reversible color model conversions there are a few other user-callable color-related procedures:

       CLOSEST_COLOR_NAME:  given RGB values, try to find closest named color
       COLOR_NAME2RGB:  given a color name, return RGB color values in range 0 to 100
       RGBMONO:  convert RGB colors to a reasonable grayscale

    2*N Design of the module

    The rest of the library is composed of PRIVATE procedures. For each color model supported the general idea of the module is
    that there are two routines for each color model:

      + One converts that model to the RGB model
      + The other converts from RGB to that model

    This allows conversions between all color models with only 2*N routines required to go from any model to any other. That is, to
    go from model A to model B the intent is that the module would make two calls:

        call modelA2rgb(...)
        call rgb2modelB(...)

    The resulting internal routines that result are:

      + HLSRGB given hue, lightness, saturation calculate red, green, and blue components
          o RGBVAL ensure a value is in the appropriate range and quadrant
      + HVSRGB given hue, saturation, value calculate red, green, and blue components
      + CMYRGB given cyan, magenta, and yellow components calculate red, green, and blue components
      + YIQRGB given luma(gray scale), orange-blue chrominance, and purple-green chrominance components calculate red, green, and
        blue components
      + RGBHVS given red, green, blue values calculate hue, value, and saturation components
      + RGBHLS given red, green, blue values calculate hue, lightness, and saturation components
      + RGBCMY given red, green, blue values calculate cyan, magenta, yellow components
      + RGBYIQ given red, green, blue values calculate luma(gray scale), orange-blue chrominance, and purple-green chrominance
        components
SEE ALSO

    A simple interactive javascript-based color selector lets you interactively select colors.

    The color wheel below was generated using a VOGLE graphics library program ((huef.f90) that uses the M_color module.

                                                              HLS circle
REFERENCES
    The algorithms are heavily based on chapter 17 of "Fundamentals of Interactive Computer Graphics"; J. D. Foley and A. Van Dam.
AUTHOR
    John S. Urban

Details on the internal procedures ...

NAME
    RGBHLS(3fp) - [M_color]Given red, green, and blue color components calculates the hue, lightness, and saturation for a color
SYNOPSIS

       subroutine rgbhls(r,g,b,h,l,s,status)

        real, intent(in)  :: r ! the red component as a value of 0 to 100
        real, intent(in)  :: g ! the green component as a value of 0 to 100
        real, intent(in)  :: b ! the blue component as a value of 0 to 100
        real, intent(out) :: h ! the hue value in the range of 0 to 360 degrees
        real, intent(out) :: l ! the lightness as a percent value from 0 to 100
        real, intent(out) :: s ! the saturation as a percent from 0 to 100
        integer           :: status


DESCRIPTION
    RGB values are in the range 0-100; hue is 0-360 degrees; lightness and saturation have a range of 0-100.


        +---------------------------------------------------+
        | Color  |       RGB       |       HLS       |Sample|
        |--------+-----------------+-----------------+------|
        |Red     |100.0|  0.0|  0.0|    0| 50.0|100.0|      |
        |--------+-----+-----+-----+-----+-----+-----+------|
        |Yellow  |100.0|100.0|  0.0|   60| 50.0|100.0|      |
        |--------+-----+-----+-----+-----+-----+-----+------|
        |Green   |  0.0|100.0|  0.0|  120| 50.0|100.0|      |
        |--------+-----+-----+-----+-----+-----+-----+------|
        |Cyan    |  0.0|100.0|100.0|  180| 50.0|100.0|      |
        |--------+-----+-----+-----+-----+-----+-----+------|
        |Blue    |  0.0|  0.0|100.0|  240| 50.0|100.0|      |
        |--------+-----+-----+-----+-----+-----+-----+------|
        |Magenta |100.0|  0.0|100.0|  300| 50.0|100.0|      |
        |--------+-----+-----+-----+-----+-----+-----+------|
        |White   |100.0|100.0|100.0|(any)|100.0|(any)|      |
        |--------+-----+-----+-----+-----+-----+-----+------|
        |Black   |  0.0|  0.0|  0.0|(any)|  0.0|(any)|      |
        |--------+-----+-----+-----+-----+-----+-----+------|
        |Maroon  | 50.0|  0.0|  0.0|    0| 25.0|100.0|      |
        |--------+-----+-----+-----+-----+-----+-----+------|
        |Pink    |100.0| 50.0| 50.0|    0| 75.0|100.0|      |
        +---------------------------------------------------+

NAME
    RGBHVS(3fp) - [M_color]calculates the hue, value, & saturation for a color given in red, green, & blue components values.
SYNOPSIS

       subroutine rgbhvs(r,g,b,h,v,s,status)

        real, intent(in)  :: r ! the red component as a value of 0 to 100.
        real, intent(in)  :: g ! the green component as a value of 0 to 100.
        real, intent(in)  :: b ! the blue component as a value of 0 to 100.
        real, intent(out) :: h ! the hue value in the range of 0 to 360 degrees
        real, intent(out) :: v ! the "value" as a percent value from 0 to 100.
        real, intent(out) :: s ! the saturation as a percent from 0 to 100.
        integer           :: status




 DESCRIPTION  


    RGBHVS() calculates the hue, value, & saturation
    for a color given in red, green, & blue components values.





        +------------------------------------------------------+
        |Color| Color |  Hex  |   (R,G,B)   |     (H,S,V)      |
        |     | name  |       |             |                  |
        |-----+-------+-------+-------------+------------------|
        |     |Black  |#000000|(0,0,0)      |(0-o,0%,0%)       |
        |-----+-------+-------+-------------+------------------|
        |     |White  |#FFFFFF|(100,100,100)|(0-o,0%,100%)     |
        |-----+-------+-------+-------------+------------------|
        |     |Red    |#FF0000|(100,0,0)    |(0-o,100%,100%)   |
        |-----+-------+-------+-------------+------------------|
        |     |Lime   |#00FF00|(0,100,0)    |(120-o,100%,100%) |
        |-----+-------+-------+-------------+------------------|
        |     |Blue   |#0000FF|(0,0,100)    |(240-o,100%,100%) |
        |-----+-------+-------+-------------+------------------|
        |     |Yellow |#FFFF00|(100,100,0)  |(60-o,100%,100%)  |
        |-----+-------+-------+-------------+------------------|
        |     |Cyan   |#00FFFF|(0,100,100)  |(180-o,100%,100%) |
        |-----+-------+-------+-------------+------------------|
        |     |Magenta|#FF00FF|(100,0,100)  |(300-o,100%,100%) |
        |-----+-------+-------+-------------+------------------|
        |     |Silver |#C0C0C0|(75,75,75)   |(0-o,0%,75%)      |
        |-----+-------+-------+-------------+------------------|
        |     |Gray   |#808080|(50,50,50)   |(0-o,0%,50%)      |
        |-----+-------+-------+-------------+------------------|
        |     |Maroon |#800000|(50,0,0)     |(0-o,100%,50%)    |
        |-----+-------+-------+-------------+------------------|
        |     |Olive  |#808000|(50,50,0)    |(60-o,100%,50%)   |
        |-----+-------+-------+-------------+------------------|
        |     |Green  |#008000|(0,50,0)     |(120-o,100%,50%)  |
        |-----+-------+-------+-------------+------------------|
        |     |Purple |#800080|(50,0,50)    |(300-o,100%,50%)  |
        |-----+-------+-------+-------------+------------------|
        |     |Teal   |#008080|(0,50,50)    |(180-o,100%,50%)  |
        |-----+-------+-------+-------------+------------------|
        |     |Navy   |#000080|(0,0,50)     |(240-o,100%,50%)  |
        +------------------------------------------------------+

NAME
    cmyrgb(3fp) - [M_color]calculates the cyan, magenta, and yellow components given the red, green, and blue component values.
SYNOPSIS

       subroutine cmyrgb(c,m,y,r,g,b,status)

        real, intent(in)  :: c ! the cyan component as a value in the range of 0 to 100
        real, intent(in)  :: m ! the magenta component as a value in the range of 0 to 100
        real, intent(in)  :: y ! the yellow component as a value in the range of 0 to 100
        real, intent(out) :: r ! the red component as a value in the range of 0 to 100
        real, intent(out) :: g ! the green component as a value in the range of 0 to 100
        real, intent(out) :: b ! the blue component as a value in the range of 0 to 100
        integer           :: status


DESCRIPTION
    cmyrgb() calculates the cyan, magenta, and yellow components given the red, green, and blue component values.

NAME
    rgbcmy(3fp) - [M_color]calculates the cyan, magenta, and yellow components given the red, green, and blue component values.
SYNOPSIS

       subroutine rgbcmy(r,g,b,c,m,y,status)

        real, intent(in)  :: r ! the red component as a value in the range of 0 to 100
        real, intent(in)  :: g ! the green component as a value in the range of 0 to 100
        real, intent(in)  :: b ! the blue component as a value in the range of 0 to 100
        real, intent(out) :: c ! the cyan component as a value in the range of 0 to 100
        real, intent(out) :: m ! the magenta component as a value in the range of 0 to 100
        real, intent(out) :: y ! the yellow component as a value in the range of 0 to 100
        integer           :: status


DESCRIPTION
    Table ...


        +-------------------------------------------------+
        |     | Color |             |             |       |
        |Color|       |   (C,M,Y)   | ( R, G, B)  |  Hex  |
        |     | name  |             |             |       |
        |-----+-------+-------------+-------------+-------|
        |     |Black  |(100,100,100)|( 0, 0, 0)   |#000000|
        |-----+-------+-------------+-------------+-------|
        |     |White  |( 0, 0, 0)   |(100,100,100)|#FFFFFF|
        |-----+-------+-------------+-------------+-------|
        |     |Red    |( 0,100,100) |(100, 0, 0)  |#FF0000|
        |-----+-------+-------------+-------------+-------|
        |     |Green  |(100, 0,100) |( 0,100, 0)  |#00FF00|
        |-----+-------+-------------+-------------+-------|
        |     |Blue   |(100,100, 0) |( 0, 0,100)  |#0000FF|
        |-----+-------+-------------+-------------+-------|
        |     |Yellow |( 0, 0,100)  |(100,100, 0) |#FFFF00|
        |-----+-------+-------------+-------------+-------|
        |     |Cyan   |(100, 0, 0)  |( 0,100,100) |#00FFFF|
        |-----+-------+-------------+-------------+-------|
        |     |Magenta|( 0,100, 0)  |(100, 0,100) |#FF00FF|
        +-------------------------------------------------+

NAME
    RGBVAL(3fp) - [M_color]is an internal private function used by hlsrgb(3fp).
SYNOPSIS

       subroutine rgbval(clr1,clr2,h)

        integer, intent(in) :: h ! H is the hue value in degrees
        real, intent(in) :: clr1 !
        real, intent(in) :: clr2 !


DESCRIPTION
    Function RGBVAL(3f) is an internal private function used by hlsrgb().

NAME
    HLSRGB(3fp) - [M_color]calculates the red, green, & blue components for a color given in hue, lightness, & saturation values.
SYNOPSIS

       subroutine hlsrgb (h,l,s,r,g,b,status)

        real, intent(in)  :: h ! hue value in the range of 0 to 360 degrees
        real, intent(in)  :: l ! lightness as a percent value from 0 to 100.
        real, intent(in)  :: s ! saturation as a percent from 0 to 100.
        real, intent(out) :: r ! red component as a value of 0 to 100.
        real, intent(out) :: g ! green component as a value of 0 to 100.
        real, intent(out) :: b ! blue component as a value of 0 to 100.
        integer           :: status


DESCRIPTION
    HLSRGB() calculates the red, green, & blue components for a color given in hue, lightness, & saturation values.

NAME
    HVSRGB(3fp) - [M_color]calculates the red, green, & blue components for a color given in hue, value, & saturation values.
SYNOPSIS

       subroutine hvsrgb(h,v,s,r,g,b,status)

        real, intent(in)  :: h ! H is the hue value in the range of 0 to 360 degrees
        real, intent(in)  :: v ! V is the "value" as a percent value from 0 to 100.
        real, intent(in)  :: s ! S is the saturation as a percent from 0 to 100.
        real, intent(out) :: r ! R is the red component as a value of 0 to 100.
        real, intent(out) :: g ! G is the green component as a value of 0 to 100.
        real, intent(out) :: b ! B is the blue component as a value of 0 to 100.
        integer           :: status


DESCRIPTION
    HVSRGB() calculates the red, green, & blue components for a color given in hue, value, & saturation values.

NAME
    YIQRGB(3fp) - [M_color]Convert luma, orange-blue chrominance, and purple-green chrominance to RGB values.
SYNOPSIS

       subroutine yiqrgb(y,i,q,r,g,b,status)

        real,intent(in)  :: y,i,q
        real,intent(out) :: r,g,b
        integer          :: status


DESCRIPTION
    Convert luma, orange-blue chrominance, and purple-green chrominance to RGB values.

NAME
    RGBYIQ(3fp) - [M_color]Convert RGB values to luma, orange-blue chrominance, and purple-green chrominance.
SYNOPSIS

       subroutine rgbyiq(r,g,b,y,i,q,status)

        real,intent(in)  :: r,g,b
        real,intent(out) :: y,i,q
        integer          :: status


DESCRIPTION
    Convert RGB values to luma, orange-blue chrominance, and purple-green chrominance.

