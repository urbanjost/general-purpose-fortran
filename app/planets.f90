module M_ephemeris
contains
!>
!!##NAME
!!    ephemeris(3f) - [M_time] ephemeris position of planets for adjusting an equatorial telescope
!!    (LICENSE:???)
!!
!!##SYNOPSIS
!!
!!    subroutine ephemeris(itime,planet,declination_d,declination_m,declination_compass,ascent_hours,ascent_minutes)
!!
!!     integer,intent(in)           :: itime(8)
!!     class(*),intent(in)          :: planet
!!      !!integer                   :: planet
!!      !!character(len=*)          :: planet
!!
!!     integer,intent(out)          :: declination_d, declination_m
!!     character(len=1),intent(out) :: declination_compass
!!     integer,intent(out)          :: ascent_hours,ascent_minutes
!!
!!##DESCRIPTION
!!    ephemeris(3f) calculates the ephemeris of a planet in our solar system
!!    in order to adjust an equatorial telescope.
!!
!!     Ref.: "Mathematiques par l'informatique individuelle
!!           - Programmes en BASIC, MASSON,
!!           Paris, 1982, p 100 - 105" [BIBLI 06].
!!
!!##OPTIONS
!!      itime   provide the date and time using the same eight values used
!!              by the DATE_AND_TIME(3f) intrinsic.
!!
!!              1. The year
!!              2. The month
!!              3. The day of the month
!!              4. Time difference with UTC in minutes
!!              5. The hour of the day
!!              6. The minutes of the hour
!!              7. The seconds of the minute
!!              8. The milliseconds of the second
!!
!!      planet  Planet number  1 to 8  or planet name (Mercury:1 Venus:2
!!              Mars:4 Jupiter:5 Saturn:6 Uranus:7 Neptune:8)
!!
!!##RESULTS
!!    declination_d,        :
!!    declination_m,        :
!!    declination_compass   Declination in degrees and minutes (-90 to 90 North or South)
!!    ascent_hours,         :
!!    ascent_minutes        Ascent in hours (0 to 24) and minutes
!!
!!##EXAMPLE
!!
!!    Find ascent and declination of planet Mars on March 10th, 1982 at 6h UT
!!
!!      program demo_ephemeris
!!      use M_time, only : ephemeris, fmtdate
!!      implicit none
!!      integer            :: itime(8)
!!      integer            :: planet
!!      integer            :: declination_d, declination_m
!!      character(len=1)   :: declination_compass
!!      integer            :: ascent_hours, ascent_minutes
!!
!!      planet=4
!!      itime=[1982,3,10,0,6,0,0,0]
!!      call ephemeris(itime, planet,                    &
!!      declination_d,declination_m,declination_compass, &
!!      ascent_hours,ascent_minutes)
!!
!!      write(*, '(" For: ",a)')fmtdate(itime)
!!      write(*, "(' Planet: ',I1,1X)",advance='no')                       &
!!              planet
!!      write(*, "(' Ascent: ',I2,' H ',I2,' MN',1X)",advance='no')        &
!!              ascent_hours, ascent_minutes
!!      write(*, "(' Declination: ',I2,' D ',I2,' MN ',A1)",advance='yes') &
!!              declination_d, declination_m, declination_compass
!!
!!      end program demo_ephemeris
!!
!!    Expected output:
!!
!!      For: Wednesday, March 10th, 1982 6:00:00 AM UTC+00:00
!!      Planet: 4  Ascent: 13 H  8 MN  Declination:  3 D 45 MN S
!!
!!##AUTHOR
!!    o F90 Version By J-P Moreau, Paris. (www.jpmoreau.fr)
!!    o Revised By John S. Urban, 20170910
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine ephemeris(itime,planet,declination_d,declination_m,declination_compass,ascent_hours,ascent_minutes)
use M_strings, only : lower
implicit none

character(len=*),parameter::ident_1="&
&@(#)M_time::ephemeris(3f): ephemeris position of planets for adjusting an equatorial telescope"

integer,parameter            :: dp=kind(0.0d0)
integer,intent(in)           :: itime(8)
class(*),intent(in)          :: planet

integer,intent(out)          :: declination_d, declination_m
character(len=1),intent(out) :: declination_compass
integer,intent(out)          :: ascent_hours,ascent_minutes

! initialize calendar constants
real(kind=dp),parameter  :: B(12)=[0.0d0, 31.0d0, 59.0d0, 25.0d0, 90.0d0, 25.0d0, 120.0d0, 25.0d0, 151.0d0, 25.0d0, 181.0d0, 25.0d0]
real(kind=dp)            :: declination,PI,t,x,y,z
real(kind=dp)            :: ascent,gg
real(kind=dp)            :: ascent_hours8
integer                  :: year, month, day
real(kind=dp)            :: hours
integer                  :: planet_number
real(kind=dp)            :: dtime(8)
!-----------------------------------------------------------------------------------------------------------------------------------
   select type(planet)
   type is (integer)
      planet_number=planet
   type is (character(len=*))
      select case(lower(planet))
      case('mercury');       planet_number=1
      case('venus');         planet_number=2
      case('earth','terra'); planet_number=3
      case('mars');          planet_number=4
      case('jupiter');       planet_number=5
      case('saturn');        planet_number=6
      case('uranus');        planet_number=7
      case('neptune');       planet_number=8
      case default;          planet_number=0
         write(*,*)'*ephemeris* ERROR: unknown planet name '//trim(planet)
      end select
   end select
   if(planet_number.lt.1.or.planet_number.gt.8)then
      write(*,*)'*ephemeris* ERROR: unknown planet number ',planet_number
      planet_number=3
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
! calculate time t
year=itime(1)
month=itime(2)
day=itime(3)
dtime=real(itime,kind=dp)
hours=dtime(5)-dtime(4)/60.0_dp+dtime(6)/60.0_dp+dtime(7)/3600.0_dp
t=365.25_dp*(year-1901)+B(month)+day
t=INT(t) + hours/24.0_dp
!-----------------------------------------------------------------------------------------------------------------------------------
                                      ! calculate earth coordinates
call planet_coordinates(3,t,x,y,z)    ! planet #3 coordinates
gg=x
ascent_hours8=y                       ! save earth coordinates

call planet_coordinates(planet_number,t,x,y,z)    ! calculate coordinates of planet #n
!-----------------------------------------------------------------------------------------------------------------------------------
                                      ! calculate geocentric equatorial coordinates
x=x-gg
y=y-ascent_hours8
t=y*.917484_dp-z*.397772_dp
z=y*.397772_dp+z*.917484_dp
y=t
!-----------------------------------------------------------------------------------------------------------------------------------
! calculate ascent and declination
ascent=DATAN2(y,x)
declination=DATAN2(z,DSQRT(x*x+y*y))
!-----------------------------------------------------------------------------------------------------------------------------------
! conversion
PI=4.d0*DATAN(1.d0)
ascent=ascent*12.0_dp/PI
if (ascent<0.d0) then
   ascent=24.d0+ascent
endif
ascent_hours=INT(ascent)
ascent_minutes=INT(60*(ascent-ascent_hours))
!-----------------------------------------------------------------------------------------------------------------------------------
! conversion
declination=declination*180.d0/PI
if (declination<0.d0) then
   declination_compass='S'
else
   declination_compass='N'
endif
declination=ABS(declination)
declination_d=INT(declination)
declination_m=INT(60*(declination-declination_d))
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine planet_coordinates(planet_number,t,x,y,z)
implicit none
!import   none
integer,parameter         :: dp=kind(0.0d0)
integer,intent(in)        :: planet_number
real(kind=dp),intent(in)  :: t
real(kind=dp),intent(out) :: x,y,z
real(kind=dp),save        :: A(9,8)
real(kind=dp)             :: xl,xm,o,p,q,e,xi,xa,xj,r,u,v
integer                   :: i
!initialize planetary constants (fill table A by columns)
data (a(i,1),i=1,9)/&
     4.01166d0, 0.071425454d0,  1.32493d0,  0.000000742289d0, 0.823045d0, 0.000000566185d0, 0.205615d0, 0.122225d0,   0.387099d0 /
data (a(i,2),i=1,9)/&
     3.60861d0, 0.027963119d0,  2.271616d0, 0.00000065572d0,  1.32291d0,  0.000000436681d0, 0.006816d0, 0.0592301d0,  0.723332d0 /
data (a(i,3),i=1,9)/&
     1.72727d0, 0.0172028d0,    1.76688d0,  0.000000818559d0, 0.0d0,      0.0d0,            0.016751d0, 0.0d0,        1.0d0      /
data (a(i,4),i=1,9)/&
     2.17756d0, 0.0091467658d0, 5.83378d0,  0.000000879297d0, 0.851616d0, 0.000000371232d0, 0.093309d0, 0.0322939d0,  1.5236d0   /
data (a(i,5),i=1,9)/&
     4.68279d0, 0.00145099d0,   0.2289d0,   0.000000857d0,    1.73578d0,  0.000000482933d0, 0.048376d0, 0.0228418d0,  5.202799d0 /
data (a(i,6),i=1,9)/&
     4.8567d0,  0.00058484d0,   1.5974d0,   0.000000412d0,    1.96856d0,  0.000000417308d0, 0.054311d0, 0.0435026d0,  9.552098d0 /
data (a(i,7),i=1,9)/&
     4.3224d0,  0.000205424d0,  2.9523d0,   0.000000762d0,    1.2825d0,   0.000000238237d0, 0.047319d0, 0.013482d0,  19.21694d0  /
data (a(i,8),i=1,9)/&
     1.5223d0,  0.000105061d0,  0.7637d0,   0.000000393d0,    2.28102d0,  0.00000052517d0,  0.008262d0, 0.0310536d0, 30.12912d0  /

!calculate planetary constants
   XL=A(1,planet_number)+A(2,planet_number)*t
   O=A(3,planet_number)+A(4,planet_number)*t
   P=A(5,planet_number)+A(6,planet_number)*t
   E=A(7,planet_number)
   XI=A(8,planet_number)
   XA=A(9,planet_number)
!solve Kepler's equation
   xm=XL-O
   u=xm

   do i=1, 10
     u=xm+E*dsin(u)
   end do

!formula (3) of reference book
   r=XA*(cos(u)-E)
   v=XA*DSQRT(1.d0-E*E)*dsin(u)
   O=O-P
   xm=dsin(O)
   O=dcos(O)
   q=dsin(P)
   P=dcos(P)
   xj=dsin(XI)
   XI=dcos(XI)
   x=(P*O-XI*q*xm)*r+(-P*xm-XI*q*O)*v
   y=(q*O+XI*P*xm)*r+(-q*xm+XI*P*O)*v
   z=xj*xm*r+xj*O*v

end subroutine planet_coordinates
!-----------------------------------------------------------------------------------------------------------------------------------
END subroutine ephemeris
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ephemeris()
use M_verify, only: unit_check,unit_check_good,unit_check_bad,unit_check_done,unit_check_start,unit_check_msg,unit_check_level
!!use M_time, only : ephemeris, fmtdate
implicit none
integer,parameter  :: itime(8)=[1982,3,10,0,6,0,0,0] ! For: Wednesday, March 10th, 1982 6:00:00 AM UTC+00:00
integer            :: planet
integer            :: D_d, D_m, A_h, A_m
character(len=1)   :: D_c

block
integer :: io=6
if(unit_check_level.gt.0)then
write(io,'(a)')'   Find ascent and declination of planets on March 10th, 1982 at 6h UT'
write(io,'(a)')''
write(io,'(a)')'      Planet     Ascent     Declination'
write(io,'(a)')'      Mercury    21 H 51    14 ° 45 mn S'
write(io,'(a)')'      Venus      20 H 26    14 ° 57 mn S'
write(io,'(a)')'      Mars       13 H 08     3 ° 45 mn S'
write(io,'(a)')'      Jupiter    14 H 32    13 ° 30 mn S'
write(io,'(a)')'      Saturn     13 H 22     5 ° 42 mn S'
write(io,'(a)')''
endif
end block

call unit_check_start('ephemeris')

     do planet=1,8
        if(planet.eq.3)cycle
        call ephemeris(itime, planet, D_d,D_m,D_c, A_h,A_m)
        call unit_check('ephemeris',D_c.eq.'S','Compass direction S')
        select case(planet)
        case(1); call unit_check('ephemeris',all([A_h,A_m,D_d,D_m].eq.[21,51,14,45]),'Mercury')
        case(2); call unit_check('ephemeris',all([A_h,A_m,D_d,D_m].eq.[20,26,14,57]),'Venus')
        case(3);
        case(4); call unit_check('ephemeris',all([A_h,A_m,D_d,D_m].eq.[13,08,03,45]),'Mars')
        case(5); call unit_check('ephemeris',all([A_h,A_m,D_d,D_m].eq.[14,32,13,30]),'Jupiter')
        case(6); call unit_check('ephemeris',all([A_h,A_m,D_d,D_m].eq.[13,22,05,42]),'Saturn')
        case(7); call unit_check('ephemeris',all([A_h,A_m,D_d,D_m].eq.[16,11,20,54]),'Uranus')
        case(8); call unit_check('ephemeris',all([A_h,A_m,D_d,D_m].eq.[17,46,22,07]),'Nepture')
        end select

     enddo

call unit_check_done('ephemeris')

end subroutine test_ephemeris
end module M_ephemeris
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
'                                                                                ',&
'   planets(1f) - [FUNIX] ephemeris position of planets for adjusting an equatorial telescope',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'                                                                                ',&
'   planets yyyy mm dd utc hh mm ss [ -planet [N|name] ]                         ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'                                                                                ',&
'   planets(1) calculates the ephemeris of a planet in our solar system          ',&
'   in order to  adjust an equatorial telescope. See ephemeris(3f) for           ',&
'   more details. The outputs are                                                ',&
'                                                                                ',&
'     o Ascent in hours (0 to 24) and minutes (0 to 60)                          ',&
'     o Declination in degrees and minutes (-90 to 90 North or South)            ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'                                                                                ',&
'     date    provide the same eight values used by the DATE_AND_TIME(3f)        ',&
'             intrinsic.                                                         ',&
'                                                                                ',&
'             value(1)  The year                                                 ',&
'             value(2)  The month                                                ',&
'             value(3)  The day of the month                                     ',&
'             value(4)  Time difference with UTC in minutes                      ',&
'             value(5)  The hour of the day                                      ',&
'             value(6)  The minutes of the hour                                  ',&
'             value(7)  The seconds of the minute                                ',&
'             value(8)  The milliseconds of the second                           ',&
'                                                                                ',&
'     N|Name  Planet numbers in range 1 to 8 (Mercury:1 Venus:2 Mars:4           ',&
'             Jupiter:5 Saturn:6 Uranus:7 Neptune:8). If not specified           ',&
'             the default is "1 2 4 5 6 7 8".                                    ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'   (Find ascent and declination of planet Mars on March 10th, 1982 at 6h UT)    ',&
'                                                                                ',&
'    planets 1982 03 10 00 06 00 00 00  -planet  4                               ',&
'                                                                                ',&
'     Wednesday, March 10th, 1982 6:00:00 AM UTC+00:00                           ',&
'     Planet: 4  Ascent: 13 H  8 MN  Declination:  3 D 45 MN S                   ',&
'                                                                                ',&
'    no planet number(s) specified:                                              ',&
'                                                                                ',&
'    planets 1982 03 10 00 06 00 00 00                                           ',&
'                                                                                ',&
'     Wednesday, March 10th, 1982 6:00:00 AM UTC+00:00                           ',&
'     Planet: 1  Ascent: 21 H 51 MN  Declination: 14 D 45 MN S                   ',&
'     Planet: 2  Ascent: 20 H 26 MN  Declination: 14 D 57 MN S                   ',&
'     Planet: 4  Ascent: 13 H  8 MN  Declination:  3 D 45 MN S                   ',&
'     Planet: 5  Ascent: 14 H 32 MN  Declination: 13 D 30 MN S                   ',&
'     Planet: 6  Ascent: 13 H 22 MN  Declination:  5 D 42 MN S                   ',&
'     Planet: 7  Ascent: 16 H 11 MN  Declination: 20 D 54 MN S                   ',&
'     Planet: 8  Ascent: 17 H 46 MN  Declination: 22 D  7 MN S                   ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!
!!    planets(1f) - [FUNIX] ephemeris position of planets for adjusting an equatorial telescope
!!
!!##SYNOPSIS
!!
!!
!!    planets yyyy mm dd utc hh mm ss [ -planet [N|name] ]
!!
!!##DESCRIPTION
!!
!!    planets(1) calculates the ephemeris of a planet in our solar system
!!    in order to  adjust an equatorial telescope. See ephemeris(3f) for
!!    more details. The outputs are
!!
!!      o Ascent in hours (0 to 24) and minutes (0 to 60)
!!      o Declination in degrees and minutes (-90 to 90 North or South)
!!
!!##OPTIONS
!!
!!      date    provide the same eight values used by the DATE_AND_TIME(3f)
!!              intrinsic.
!!
!!              value(1)  The year
!!              value(2)  The month
!!              value(3)  The day of the month
!!              value(4)  Time difference with UTC in minutes
!!              value(5)  The hour of the day
!!              value(6)  The minutes of the hour
!!              value(7)  The seconds of the minute
!!              value(8)  The milliseconds of the second
!!
!!      N|Name  Planet numbers in range 1 to 8 (Mercury:1 Venus:2 Mars:4
!!              Jupiter:5 Saturn:6 Uranus:7 Neptune:8). If not specified
!!              the default is "1 2 4 5 6 7 8".
!!
!!##EXAMPLE
!!
!!    (Find ascent and declination of planet Mars on March 10th, 1982 at 6h UT)
!!
!!     planets 1982 03 10 00 06 00 00 00  -planet  4
!!
!!      Wednesday, March 10th, 1982 6:00:00 AM UTC+00:00
!!      Planet: 4  Ascent: 13 H  8 MN  Declination:  3 D 45 MN S
!!
!!     no planet number(s) specified:
!!
!!     planets 1982 03 10 00 06 00 00 00
!!
!!      Wednesday, March 10th, 1982 6:00:00 AM UTC+00:00
!!      Planet: 1  Ascent: 21 H 51 MN  Declination: 14 D 45 MN S
!!      Planet: 2  Ascent: 20 H 26 MN  Declination: 14 D 57 MN S
!!      Planet: 4  Ascent: 13 H  8 MN  Declination:  3 D 45 MN S
!!      Planet: 5  Ascent: 14 H 32 MN  Declination: 13 D 30 MN S
!!      Planet: 6  Ascent: 13 H 22 MN  Declination:  5 D 42 MN S
!!      Planet: 7  Ascent: 16 H 11 MN  Declination: 20 D 54 MN S
!!      Planet: 8  Ascent: 17 H 46 MN  Declination: 22 D  7 MN S
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
'@(#)PROGRAM:        planets(1)>',&
'@(#)DESCRIPTION:    ephemeris position of planets for adjusting an equitorial telescope>',&
'@(#)VERSION:        1.0, 20170910>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       2021-07-01 09:04:59 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
program planets
use M_kracken, only   : kracken, lget, sget, iget, igets
use M_time, only      : guessdate, fmtdate!, ephemeris
use M_ephemeris, only : ephemeris
implicit none
integer                      :: declination_d,declination_m
integer                      :: ascent_hours, ascent_minutes
character(len=1)             :: declination_compass
integer                      :: itime(8)
integer,allocatable          :: gettime(:)
integer,allocatable          :: planet_numbers(:)
character(len=:),allocatable :: datetime
integer                      :: i
integer                      :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
call kracken('planets',' -date -planet -version .F. -help .F.')
call help_usage(lget('planets_help'))                    ! if -help option is present, display help text and exit
call help_version(lget('planets_version'))               ! if -version option is present, display version text and exit

call date_and_time(values=itime)
datetime=sget('planets_date')
if(datetime.ne.'')then
   call guessdate(datetime,itime)
else
   gettime=igets('planets_oo')
   itime(:size(gettime))=gettime
endif
write(*,'(1x,"For: ",a)')fmtdate(itime)
!-----------------------------------------------------------------------------------------------------------------------------------
planet_numbers=igets('planets_planet',ier)     ! planet number : Mercury:1 Venus:2 Mars:4 Jupiter:5 Saturn:6 Uranus:7 Neptune:8
if(ier.ne.0)stop 1
if ( size(planet_numbers).eq.0)then
   planet_numbers=[1,2,4,5,6,7,8]
endif
!-----------------------------------------------------------------------------------------------------------------------------------
do i=1,size(planet_numbers)
   call ephemeris(itime,planet_numbers(i),declination_d,declination_m,declination_compass,ascent_hours,ascent_minutes)
   write(*, "(' Planet: ',I1,1X)",advance='no')                       planet_numbers(i)
   write(*, "(' Ascent: ',I2,' H ',I2,' MN',1X)",advance='no')        ascent_hours, ascent_minutes
   write(*, "(' Declination: ',I2,' D ',I2,' MN ',A1)",advance='yes') declination_d, declination_m, declination_compass
enddo
!-----------------------------------------------------------------------------------------------------------------------------------
contains
end program planets
