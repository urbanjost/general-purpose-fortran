[CLI Home Page]

NAME
    HUE(3f) - [M_color]converts a color's components from one color model to another.
SYNOPSIS

       subroutine hue(modei,clr1i,clr2i,clr3i,modeo,clr1o,clr2o,clr3o,status)

        character(len=*),intent(in) :: modei  ! name of color model of input values
        character(len=*),intent(in) :: modeo  ! name of color model of output values
        real,intent(in)             :: clr1i,clr2i,clr3i
        real,intent(out)            :: clr1o,clr2o,clr3o
        integer,intent(out)         :: status


DESCRIPTION
    Basic color models ...


          valid values for modei and modeo as well as the corresponding
                    meanings for clr1*, clr2*, and clr3* are:
        +------------------------------------------------------------------------+
        |model|      clr1      |         clr2          |          clr3           |
        |-----+----------------+-----------------------+-------------------------|
        |hls  |hue             |lightness              |saturation               |
        |-----+----------------+-----------------------+-------------------------|
        |hsl  |hue             |saturation             |lightness                |
        |-----+----------------+-----------------------+-------------------------|
        |hvs  |hue             |value                  |saturation               |
        |-----+----------------+-----------------------+-------------------------|
        |hsv  |hue             |saturation             |value                    |
        |-----+----------------+-----------------------+-------------------------|
        |rgb  |red             |green                  |blue                     |
        |-----+----------------+-----------------------+-------------------------|
        |cmy  |cyan            |magenta                |yellow                   |
        |-----+----------------+-----------------------+-------------------------|
        |yiq  |luma(gray scale)|orange-blue chrominance|purple-green chrominance |
        +------------------------------------------------------------------------+

      + lightness, value, saturation, red, green, blue, cyan, magenta, and yellow range from 0 to 100,
      + hue ranges from 0 to 360 degrees,
      + y ranges from 0 to 100,
      + i ranges from -60 to 60,
      + q ranges from -52 to 52

    The STATUS variable can signal the following conditions:

       -1   modei = modeo, so no substantial conversion was done,
        1   one of the input color values was outside the allowable range,
        2   modei was invalid
        3   modeo was invalid

