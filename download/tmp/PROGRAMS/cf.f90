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
'   cf - Convert between Fahrenheit and Celsius temperature values               ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   cf [-C values] [-F values] [--help] [--version]                              ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'                                                                                ',&
'   -C values  Display the given Celsius values as both Celsius and              ',&
'              Fahrenheit values                                                 ',&
'   -F values  Display the given Fahrenheit values as both Celsius and           ',&
'              Fahrenheit values. If no values are given a small table of        ',&
'              common temperatures is displayed.                                 ',&
'   --help     display this help and exit                                        ',&
'   --version  output version information and exit                               ',&
'                                                                                ',&
'   At the physically impossible-to-reach temperature of zero Kelvin,            ',&
'   or minus 459.67 degrees Fahrenheit (minus 273.15 degrees Celsius),           ',&
'   atoms would stop moving. As such, nothing can be colder than absolute        ',&
'   zero on the Kelvin scale.                                                    ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'                                                                                ',&
' Sample program runs:                                                           ',&
'                                                                                ',&
'   %cf -C -40 0 37 100                                                          ',&
'    Celsius      Fahrenheit                                                     ',&
'     -40.00C      -40.00F                                                       ',&
'       0.00C       32.00F                                                       ',&
'      37.00C       98.60F                                                       ',&
'     100.00C      212.00F                                                       ',&
'                                                                                ',&
'   %cf -F -459.67 32 98.60 212                                                  ',&
'    Fahrenheit   Celsius                                                        ',&
'    -459.67F     -273.15C                                                       ',&
'      32.00F        0.00C                                                       ',&
'      98.60F       37.00C                                                       ',&
'     212.00F      100.00C                                                       ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    cf - Convert between Fahrenheit and Celsius temperature values
!!
!!##SYNOPSIS
!!
!!    cf [-C values] [-F values] [--help] [--version]
!!
!!##DESCRIPTION
!!
!!    -C values  Display the given Celsius values as both Celsius and
!!               Fahrenheit values
!!    -F values  Display the given Fahrenheit values as both Celsius and
!!               Fahrenheit values. If no values are given a small table of
!!               common temperatures is displayed.
!!    --help     display this help and exit
!!    --version  output version information and exit
!!
!!    At the physically impossible-to-reach temperature of zero Kelvin,
!!    or minus 459.67 degrees Fahrenheit (minus 273.15 degrees Celsius),
!!    atoms would stop moving. As such, nothing can be colder than absolute
!!    zero on the Kelvin scale.
!!
!!##EXAMPLE
!!
!!
!!  Sample program runs:
!!
!!    %cf -C -40 0 37 100
!!     Celsius      Fahrenheit
!!      -40.00C      -40.00F
!!        0.00C       32.00F
!!       37.00C       98.60F
!!      100.00C      212.00F
!!
!!    %cf -F -459.67 32 98.60 212
!!     Fahrenheit   Celsius
!!     -459.67F     -273.15C
!!       32.00F        0.00C
!!       98.60F       37.00C
!!      212.00F      100.00C
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
'@(#)PROGRAM:        cf(1f)>',&
'@(#)DESCRIPTION:    convert multiple values between Celsius and Fahrenheit>',&
'@(#)VERSION:        1.0, 2016-04-09>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:00:33 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program cf
use M_kracken, only: kracken, rgets, lget
implicit none
character(len=*),parameter :: ident="@(#)cf(1f): convert multiple values between Celsius and Fahrenheit"
real,allocatable                :: val(:)
integer                         :: i, isum=0
  call kracken('cf','-F -C -help .F. -version .F.' )
  call help_usage(lget('cf_help'))                                    ! display help information and stop  if true
  call help_version(lget('cf_version'))                               ! display version information and stop if true
  isum=0                                                              ! number of values found on -C and -F options
  val=rgets('cf_C')                                                   ! get any values specified on -C option

  if(size(val).gt.0)then                                              ! have something to print in C ==> F table
     isum=isum+size(val)
     write(*,'(a,t14,a)')'Celsius','Fahrenheit'
     write(*,'(f8.2,"C",t14,f8.2,"F")')( val(i),(val(i)+40.0)*9.0/5.0 - 40.0,i=1,size(val))    ! print the requested values
  endif

  val=rgets('cf_F')                                                   ! check for values on -F

  if(size(val).gt.0)then
     isum=isum+size(val)
     write(*,'(a,t14,a)') 'Fahrenheit', 'Celsius'
     write(*,'(f8.2,"F",t14,f8.2,"C")')(val(i),(val(i)+40.0)*5.0/9.0 - 40.0,i=1,size(val))
  endif

  if(isum.eq.0)then                                                   ! if no values given on -C and -F switches show default table
    val=[ &
       &-459.67,                               &
       & -20.0,  -15.0,  -10.0,   -5.0,   0.0, &
       &   5.0,   10.0,   15.0,   20.0,  25.0, &
       &  30.0,   32.0,   35.0,   40.0,  45.0, &
       &  50.0,   55.0,   60.0,   65.0,  70.0, &
       &  75.0,   80.0,   85.0,   90.0,  95.0, &
       &  98.6,  100.0,  105.0,  110.0, 115.0  ]
     write(*,'(a,t14,a)') 'Fahrenheit', 'Celsius'
     write(*,'(f8.2,"F",t14,f8.2,"C")')(val(i),(val(i)+40.0)*5.0/9.0 - 40.0,i=1,size(val))
  endif

end program cf
!-----------------------------------------------------------------------------------------------------------------------------------
