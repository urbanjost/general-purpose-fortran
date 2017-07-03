subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   colors - list colors and their values using common color models              ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   colors [color_name | R G B | model_name_A V1 V2 V3 model_name_B ]            ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Display colors using common color models; or convert color values            ',&
'   to a different color model                                                   ',&
'                                                                                ',&
'    # list known color names and their RGB values                               ',&
'    colors                                                                      ',&
'    # show values for a known named color                                       ',&
'    colors COLOR_NAME                                                           ',&
'    # find closest named color                                                  ',&
'    colors R G B                                                                ',&
'    #convert color between models                                               ',&
'    colors INPUT_MODEL_NAME VALUE1 VALUE2 VALUE3 OUTPUT_MODEL_NAME              ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'    Model Names(case sensitive):                                                ',&
'                                                                                ',&
'    rgb   color TV monitors (RGB values in range 0 to 100)                      ',&
'    hls   Hue (0 to 360 degrees), Lightness (0 to 100), Saturation (0 to 100)   ',&
'    cmy   Cyan, Magenta, Yellow : pigment-based printing devices                ',&
'          ( values in range 0 to 100 )                                          ',&
'    hsv   Hue (0 to 360 degrees), Saturation (0 to 100), Value (0 to 100)       ',&
'    yiq   Broadcast TV color system (y ranges from 0 to 100,                    ',&
'          i ranges from -60 to 60, q ranges from -52 to 52)                     ',&
'                                                                                ',&
'    --help      display this help and exit                                      ',&
'                                                                                ',&
'    --version   output version information and exit                             ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'    Common forms of use:                                                        ',&
'                                                                                ',&
'     # list named colors                                                        ',&
'     colors                                                                     ',&
'     # convert RGB values to HLS value                                          ',&
'     colors rgb 0 100 0 hls                                                     ',&
'     # display RGB values for named color                                       ',&
'     colors green                                                               ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    colors - list colors and their values using common color models
!!
!!##SYNOPSIS
!!
!!    colors [color_name | R G B | model_name_A V1 V2 V3 model_name_B ]
!!
!!##DESCRIPTION
!!    Display colors using common color models; or convert color values
!!    to a different color model
!!
!!     # list known color names and their RGB values
!!     colors
!!     # show values for a known named color
!!     colors COLOR_NAME
!!     # find closest named color
!!     colors R G B
!!     #convert color between models
!!     colors INPUT_MODEL_NAME VALUE1 VALUE2 VALUE3 OUTPUT_MODEL_NAME
!!
!!##OPTIONS
!!     Model Names(case sensitive):
!!
!!     rgb   color TV monitors (RGB values in range 0 to 100)
!!     hls   Hue (0 to 360 degrees), Lightness (0 to 100), Saturation (0 to 100)
!!     cmy   Cyan, Magenta, Yellow : pigment-based printing devices
!!           ( values in range 0 to 100 )
!!     hsv   Hue (0 to 360 degrees), Saturation (0 to 100), Value (0 to 100)
!!     yiq   Broadcast TV color system (y ranges from 0 to 100,
!!           i ranges from -60 to 60, q ranges from -52 to 52)
!!
!!     --help      display this help and exit
!!
!!     --version   output version information and exit
!!
!!##EXAMPLE
!!
!!     Common forms of use:
!!
!!      # list named colors
!!      colors
!!      # convert RGB values to HLS value
!!      colors rgb 0 100 0 hls
!!      # display RGB values for named color
!!      colors green
!===================================================================================================================================
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        CLI library utilities and examples>',&
'@(#)PROGRAM:        colors(1f)>',&
'@(#)DESCRIPTION:    display color names and values>',&
'@(#)VERSION:        1.0 20151015>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:00:50 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program colors
use m_color, only : hue, color_name2rgb , closest_color_name
use M_kracken, only: kracken, sget, IPvalue, lget, rget
use M_strings, only: split,s2v,v2s
use M_debug,   only: stderr
implicit none
character(len=*),parameter    :: ident="@(#)ccall_M_color(3f): call HUE(3f) by providing parameters on command line"
character(len=IPvalue)        :: list
character(len=80),allocatable :: token(:)          ! output array of tokens
character(len=20)             :: closestname
real                          :: valo1,valo2,valo3
integer                       :: status
integer                       :: token_count
character(len=20)             :: echoname
character(len=:),allocatable  :: fmt
real                          :: red,green,blue
real                          :: hu,li,sa,cc,mm,vv,yy,ii,qq,hh,rr,gg,bb
integer                       :: i
real                          :: denominator
real                          :: multiplier
   ! define command arguments, default values and crack command line
   call kracken('color','-oo -m 1.0 -d 1.0 -help .f. -version .f.')
   call help_usage(lget('color_help'))
   call help_version(lget('color_version'))
   list = sget('color_oo')                                   ! get values from command line
   call split(list,token)                                    ! split options into tokens
   token_count=size(token)
   if(token_count.eq.0)then
      TRYALL: do i=1,10000
         ! weird little thing where the color names have aliases that are numeric strings
         call color_name2rgb(v2s(i),red,green,blue,echoname) ! get the RGB values and English name of the color
         if(echoname.eq.'Unknown')exit TRYALL                ! the last color name is "Unknown" so the loop should exit
         fmt='(a,3(i3,"%",1x),"#",3(z2.2),1x,3(i3,1x))'
         ! display the English name and RGB values for the name
         write(*,fmt)echoname,int([red,green,blue]/1.00), int([red,green,blue]*2.55+0.5) ,int([red,green,blue]*2.55+0.5)
      enddo TRYALL
   elseif(token_count.eq.1)then
      call color_name2rgb(token(1),rr,gg,bb,echoname)        ! get the RGB values for the English color name
      if(echoname.ne.'Unknown')then
         ! display the English name and RGB values for the name

            write(*,*)trim(token(1)),' RGB(0:100, 0:100, 0:100):',int(([rr,gg,bb]+0.5)/1.00)
         call hue('rgb',rr,gg,bb,'hls',hu,li,sa,status)
            write(*,*)trim(token(1)),' HLS(0:360, 0:100, 0:100):',int(([hu,li,sa]+0.5)/1.00)
         call hue('rgb',rr,gg,bb,'yiq',yy,ii,qq,status)
            write(*,*)trim(token(1)),' YIQ(0:100,-60:60,-52:52):',int(([yy,ii,qq]+0.5)/1.00)
         call hue('rgb',rr,gg,bb,'cmy',cc,mm,yy,status)
            write(*,*)trim(token(1)),' CMY(0:100, 0:100, 0:100):',int(([cc,mm,yy]+0.5)/1.00)
         call hue('rgb',rr,gg,bb,'hsv',hh,sa,vv,status)
            write(*,*)trim(token(1)),' HSV(0:360, 0:100, 0:100):',int(([hh,sa,vv]+0.5)/1.00)
      else
         call stderr('*M_color: error: unknown name '//trim(token(1)) )
      endif
   elseif(token_count.eq.3)then
      multiplier=rget('color_m')
      denominator=rget('color_d')
      red=real(s2v(token(1)))*multiplier/denominator
      green=real(s2v(token(2)))*multiplier/denominator
      blue=real(s2v(token(3)))*multiplier/denominator
      call closest_color_name(red,green,blue,closestname)
      write(*,*)'closest name for ', red, green, blue, ' is ', closestname
   elseif(token_count.eq.5)then
      ! call color conversion routine
      call hue(token(1), real(s2v(token(2))), real(s2v(token(3))), real(s2v(token(4))),token(5), valo1, valo2, valo3, status)
      ! write out results of calling hue(3f)
      write(*,'(5(a,1x),3(i0,1x))') &
      & trim(token(1)), trim(token(2)), trim(token(3)), trim(token(4)), &
      & trim(token(5)), int(valo1),     int(valo2),     int(valo3)
      ! report on error
      select case (status)
      case(  0)  ; stop! no error occurred
      case( -1)  ; call stderr('*M_color*: error: input_model_name = output_model_name, so no substantial conversion was done')
      case(  1)  ; call stderr('*M_color*: error: one of the input color values was outside the allowable range')
      case(  2)  ; call stderr('*M_color*: error: input model name  was invalid: '//trim(token(1)))
      case(  3)  ; call stderr('*M_color*: error: output model name  was invalid: '//trim(token(5)))
      case default;call stderr('*M_color*: error: unknown status value: '//v2s(status))
      end select
      stop 2
   else
      call stderr('*M_strings*:error: unsupported number of parameters provided: '//v2s(size(token)))
      stop 1
   endif
end program colors
