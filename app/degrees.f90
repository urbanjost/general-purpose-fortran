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
'   degrees(1f) - [CONVERT] Convert between Fahrenheit and Celsius temperature values',&
'   (LICENSE:PD)                                                                 ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   degrees [value_unit ...] [ -C values] [ -F values] [ --help] [ --version]    ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'                                                                                ',&
'   value_unit  numeric values followed by either a C or F to specify a unit.    ',&
'   -C values   Display the given Celsius values as both Celsius and             ',&
'               Fahrenheit values                                                ',&
'   -F values   Display the given Fahrenheit values as both Celsius and          ',&
'               Fahrenheit values. If no values are given a small table of       ',&
'               common temperatures is displayed.                                ',&
'   --help      display this help and exit                                       ',&
'   --version   output version information and exit                              ',&
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
'   %degrees -C -40 0 37 100                                                     ',&
'    Celsius      Fahrenheit                                                     ',&
'     -40.00C      -40.00F                                                       ',&
'       0.00C       32.00F                                                       ',&
'      37.00C       98.60F                                                       ',&
'     100.00C      212.00F                                                       ',&
'                                                                                ',&
'   %degrees -F -459.67 32 98.60 212                                             ',&
'    Fahrenheit   Celsius                                                        ',&
'    -459.67F     -273.15C                                                       ',&
'      32.00F        0.00C                                                       ',&
'      98.60F       37.00C                                                       ',&
'     212.00F      100.00C                                                       ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!    degrees(1f) - [CONVERT] Convert between Fahrenheit and Celsius temperature values
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    degrees [value_unit ...] [ -C values] [ -F values] [ --help] [ --version]
!!
!!##DESCRIPTION
!!
!!    value_unit  numeric values followed by either a C or F to specify a unit.
!!    -C values   Display the given Celsius values as both Celsius and
!!                Fahrenheit values
!!    -F values   Display the given Fahrenheit values as both Celsius and
!!                Fahrenheit values. If no values are given a small table of
!!                common temperatures is displayed.
!!    --help      display this help and exit
!!    --version   output version information and exit
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
!!    %degrees -C -40 0 37 100
!!     Celsius      Fahrenheit
!!      -40.00C      -40.00F
!!        0.00C       32.00F
!!       37.00C       98.60F
!!      100.00C      212.00F
!!
!!    %degrees -F -459.67 32 98.60 212
!!     Fahrenheit   Celsius
!!     -459.67F     -273.15C
!!       32.00F        0.00C
!!       98.60F       37.00C
!!      212.00F      100.00C
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
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
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        degrees(1f)>',&
'@(#)DESCRIPTION:    convert multiple values between Celsius and Fahrenheit>',&
'@(#)VERSION:        1.0, 2016-04-09>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain>',&
'@(#)COMPILED:       2021-06-26 18:31:08 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
program degrees
use M_kracken, only : kracken, rgets, lget, sget
use M_strings, only : upper, compact, replace, split, s2v
implicit none
character(len=*),parameter::ident_1="@(#)degrees(1f): convert multiple values between Celsius and Fahrenheit"
real,allocatable              :: val(:)
integer                       :: i, isum=0, ivals, last
real                          :: sval
character(len=:),allocatable  :: values
character(len=:),allocatable  :: values_split(:)
  call kracken('degrees','-F -C -K -help .F. -version .F.' )
  call help_usage(lget('degrees_help'))                                  ! display help information and stop  if true
  call help_version(lget('degrees_version'))                             ! display version information and stop if true
  isum=0                                                                 ! number of values found on -oo and -C and -F options

  values=sget('degrees_oo')            ! look for plain values with a unit suffix
  if(values.ne.'')then
     values=upper(compact(values))     ! reduce whitespace regions to single characters and convert to uppercase
     values=replace(values,' F','F')   ! remove space between values and suffix
     values=replace(values,' C','C')
     call split(values,values_split)   ! split into array of strings of form nnnn[CF]
     ivals=size(values_split)
     if(ivals.ne.0)then
        write(*,'(a,t14,a)')'Celsius','Fahrenheit'
        isum=ivals
        do i=1,ivals
           last=len(trim(values_split(i)))              ! find suffix letter
           if(last.lt.2)then
              write(*,*)'*degrees* syntax error: numeric value and F or C suffix is required for '//trim(values_split(i))
              !!call help_usage(.true.)                                          ! display help information and stop
              stop 1
           endif
           sval=s2v(values_split(i)(:last-1))           ! convert numeric section to a number
           select case(values_split(i)(last:last))
           case ('F')
              write(*,'(f8.2,"C",t14,f8.2,"F")')(sval+40.0)*5.0/9.0 - 40.0,sval
           case ('C')
              write(*,'(f8.2,"C",t14,f8.2,"F")')sval,(sval+40.0)*9.0/5.0 - 40.0
           case default
              write(*,*)'*degrees* syntax error: F or C suffix is required for '//trim(values_split(i))
              !!call help_usage(.true.)                                          ! display help information and stop
              stop 2
           end select
        enddo
     endif
  endif
  val=rgets('degrees_C')                                              ! get any values specified on -C option

  if(size(val).gt.0)then                                              ! have something to print in C ==> F table
     isum=isum+size(val)
     write(*,'(a,t14,a)')'Celsius','Fahrenheit'
     write(*,'(f8.2,"C",t14,f8.2,"F")')(val(i),(val(i)+40.0)*9.0/5.0 - 40.0,i=1,size(val))    ! print the requested values
  endif

  val=rgets('degrees_F')                                                 ! check for values on -F

  if(size(val).gt.0)then
     isum=isum+size(val)
     write(*,'(a,t14,a)') 'Fahrenheit', 'Celsius'
     write(*,'(f8.2,"F",t14,f8.2,"C")')(val(i),(val(i)+40.0)*5.0/9.0 - 40.0,i=1,size(val))
  endif

  if(isum.eq.0)then                                                   ! if no values given on -C and -F switches show default table
    val=[ &
       &-459.67,                               &
       &-400.0, -300.0, -200.0, -150.0,-128.6, -125.0, &
       &-100.0,  -75.0,  -50.0,  -40.0, -30.0, &
       & -20.0,  -15.0,  -10.0,   -5.0,   0.0, &
       &   5.0,   10.0,   15.0,   20.0,  25.0, &
       &  30.0,   32.0,   35.0,   40.0,  45.0, &
       &  50.0,   55.0,   60.0,   65.0,  70.0, &
       &  75.0,   80.0,   85.0,   90.0,  95.0, &
       &  98.6,  100.0,  105.0,  110.0, 115.0, &
       & 120.0,  125.0,  130.0,  134.0, 135.0, &
       & 140.0,  145.0,  150.0,  160.0, 170.0, &
       & 180.0,  190.0,  200.0,  210.0, 212.0  ]
     write(*,'(a,t14,a)') 'Fahrenheit', 'Celsius'
     write(*,'(f8.2,"F",t14,f8.2,"C")')(val(i),(val(i)+40.0)*5.0/9.0 - 40.0,i=1,size(val))
  endif

end program degrees
!-----------------------------------------------------------------------------------------------------------------------------------
