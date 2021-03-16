










!===================================================================================================================================
!
!     XX                                             X
!      X            X                       X
!      X            X                       X
!  XXXXX   XXXX    XXXX    XXXXX           XXXX    XXX    XXX X    XXXXX
! X    X       X    X     X     X           X        X     X X X  X     X
! X    X   XXXXX    X     XXXXXXX           X        X     X X X  XXXXXXX
! X    X  X    X    X     X                 X        X     X X X  X
! X    X  X    X    X  X  X     X           X  X     X     X X X  X     X
!  XXXXXX  XXXX X    XX    XXXXX             XX    XXXXX  XX X XX  XXXXX
!
!                                XXXXXXXX
!
!>
!!##NAME
!!    M_time_oop(3fm) - [M_time::OBJECT_ORIENTED] OOP interface for M_time(3fm)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   use M_time_oop, only : date_time
!!
!!    use M_time_oop,only : operator(+),operator(-),operator(>),operator(<)
!!    use M_time_oop,only : operator(<=),operator(>=),operator(==),operator(/=)
!!
!!    TYPE(date_time) :: mydate
!!
!!       mydate%year
!!       mydate%month
!!       mydate%day
!!       mydate%tz
!!       mydate%hour
!!       mydate%minute
!!       mydate%second
!!       mydate%millisecond
!!
!!       call mydate%init()
!!
!!       mydate%format('')
!!       mydate%ordinal()
!!       mydate%weekday()
!!       mydate%epoch()
!!       mydate%julian()
!!       dat=mydate%datout()
!!       mydate%delta(year=NN, month=NN, day=NN, tz=NN, hour=NN, minute=NN,
!!       second=NN, millisecond=NN, week=NN, duration='DD-HH:MM:SS.XX')
!!
!!##DESCRIPTION
!!    An object-oriented interface to the M_time module. The following
!!    example program demonstrates and documents the interface
!!
!!##EXAMPLE
!!
!!
!!  sample program
!!
!!     program demo_M_time_oop
!!     !
!!     ! This is an example using the object-oriented class/type model
!!     ! This is essentially the same functionality as the procedures
!!     ! in the procedural module M_time(3fm), but allows for Object Oriented syntax:
!!     !
!!     use M_time_oop,only : date_time
!!     !!use M_time_oop,only : operator(+),operator(-),operator(>),operator(<)
!!     !!use M_time_oop,only : operator(<=),operator(>=),operator(==),operator(/=)
!!     implicit none
!!     integer         :: dat(8)
!!     TYPE(date_time) :: event
!!     TYPE(date_time) :: otherdate
!!     TYPE(date_time) :: answer
!!
!!     character(len=*),parameter :: iso_fmt='%Y-%M-%DT%h:%m:%s.%x%z'
!!        ! DIFFERENT INITIALIZATION STYLES
!!        ! (Still debating on how best to do this)
!!        write(*,*)
!!        write(*,'(a)')'Various initialization styes'
!!
!!        ! DEFINE type(date_time) WITH CONSTRUCTOR
!!        otherdate=date_time()
!!        print *,'DEFAULT CONSTRUCTOR %format()               ',&
!!        & otherdate%format()
!!        print *,'DEFAULT CONSTRUCTOR %format("")             ',&
!!        & otherdate%format("")
!!        print *,'DEFAULT CONSTRUCTOR %format(user-specified) ',&
!!        & otherdate%format(iso_fmt)
!!        print *,'DEFAULT CONSTRUCTOR %format("USA")          ',&
!!        & otherdate%format("USA")
!!
!!        otherdate=date_time(1492,10,12,0,0,0,0,0)
!!        print *,'DEFAULT CONSTRUCTOR setting values          ',&
!!        & otherdate%format()
!!
!!        otherdate=date_time(2016,6,11)
!!        print *,'DEFAULT CONSTRUCTOR with partial values     ',&
!!        & otherdate%format()
!!
!!        otherdate=date_time(year=2016,month=6,day=11,tz=-240,&
!!        & hour=21,minute=09,second=11,millisecond=500)
!!        print *,'DEFAULT CONSTRUCTOR with values by name     ',&
!!        & otherdate%format()
!!
!!        otherdate=date_time([1776,7,4,0,0,0,0,0])
!!        print *,'CONSTRUCTOR with a dat array                ',&
!!        & otherdate%format()
!!
!!        otherdate=date_time([1776,7,4])
!!        print *,'CONSTRUCTOR with a partial dat array        ',&
!!        & otherdate%format()
!!
!!        ! the init() method supports several methods
!!        ! initialize to current time using INIT
!!        call otherdate%init()
!!        ! initialize to current time using INIT
!!        call otherdate%init(type="now")
!!
!!        ! initialize to beginning of Unix Epoch Time
!!        call otherdate%init(type="epoch")
!!        ! Note
!!        ! currently, DATE_TIME DATE array is set to Unix Epoch
!!        ! start USING LOCAL TIMEZONE
!!        ! whereas default constructor is using default of Unix Epoch
!!        ! start using Z time (GMT or UTC time)
!!
!!        ! initialize with a DAT array using INIT,
!!        ! compatible with DATE_AND_TIME VALUES(8)
!!        call otherdate%init(dat=[1970,1,1,0,0,0,0,0])
!!        ! using INIT with ordered values
!!        call otherdate%init(2016,6,11,-300,23,1,0,0)
!!        ! using INIT with names
!!        call otherdate%init(year=2016,month=6,day=11,&
!!        & tz=-300,hour=23,minute=1,second=0,millisecond=0)
!!        !
!!        ! take current date and exercise the OOP interface
!!        ! initialize to current time using INIT
!!        call event%init()
!!        write(*,*)
!!        write(*,*)'Print members of type(DATE_TIME)'
!!        ! show derived type
!!        write(*,404)'EVENT=',event
!!        404 format(1x,a,i0,*(",",i0:))
!!
!!        ! MEMBERS ( basic time values are all integers)
!!        ! print members of type
!!        write(*,101)'%year        Year................... ',event%year
!!        write(*,101)'%month       Month.................. ',event%month
!!        write(*,101)'%day         Day.................... ',event%day
!!        write(*,101)'%tz          Timezone............... ',event%tz
!!        write(*,101)'%hour        Hour................... ',event%hour
!!        write(*,101)'%minute      Minute................. ',event%minute
!!        write(*,101)'%second      Second................. ',event%second
!!        write(*,101)'%millisecond Millisecond............ ',event%millisecond
!!
!!        ! PRINT METHODS OF TYPE
!!        write(*,*)'Print methods of type(DATE_TIME)'
!!        write(*,101)'%ordinal     Ordinal day of year.... ',  event%ordinal()
!!        write(*,101)'%weekday     Weekday................ ',  event%weekday()
!!        101 format(1x,a,i0)
!!        ! DOUBLE PRECISION VALUES EASILY MANIPULATED MATHEMATICALLY
!!        write(*,202)'%epoch      Unix epoch time........ ',  event%epoch()
!!        write(*,202)'%julian     Julian date............ ',  event%julian()
!!        202 format(1x,a,g0)
!!
!!        ! FORMATTED STRINGS (many strings possible.
!!        ! Takes the same format string as fmtdate(3f))
!!        write(*,*)
!!        write(*,*)'Formatted Strings (%format("STRING") &
!!        & -- see fmtdate(3f) for format descriptions'
!!        ! abbreviated month name             %l  Dec
!!        write(*,303)'Short month............ ',&
!!        & event%format("%l")
!!        !
!!        ! full month name                    %L  December
!!        write(*,303)'Month.................. ',&
!!        & event%format("%L")
!!        !
!!        ! first three characters of weekday  %w  Sat
!!        write(*,303)'Short week............. ',&
!!        & event%format("%w")
!!        !
!!        ! weekday name                       %W  Saturday
!!        write(*,303)'Week .................. ',&
!!        & event%format("%W")
!!        !
!!        ! with no percent (%) characters
!!        write(*,303)'Calendar Time ......... ',&
!!        & event%format("Y-M-D h:m:s.x z")
!!        !
!!        ! keywords with no percent (%) characters
!!        write(*,303)'Calendar Time ......... ',&
!!        & event%format('"year-month-day &
!!        !
!!        &hour:minute:second.millisecond timezone"')
!!        write(*,*)event%format('Longer format.......... &
!!        &"%W, %L %d, %Y %H:%m:%s %N"') ! a nice friendly format
!!        !
!!        303 format(1x,a,'"',a,'"')
!!
!!        ! convert date_time to integer array
!!        ! (maybe to use with module M_TIME base procedures)
!!        dat=event%datout()
!!        write(*,*)
!!        write(*,404)'DAT=',dat
!!
!!        ! OVERLOADED OPERATORS (add and subtract)
!!        ! a date_time object can have seconds added
!!        answer=event+1*86400.0d0
!!        !
!!        ! a nice friendly format
!!        write(*,*)answer%format('TOMORROW="%W, %L %d, %Y %H:%m:%s %N"')
!!        !
!!        ! a date_time object can have seconds subtracted
!!        answer=event-1*86400.0d0
!!        ! a nice friendly format
!!        write(*,*)answer%format('YESTERDAY="%W, %L %d, %Y %H:%m:%s %N"')
!!        !
!!        ! if both operands are DATE_TIME objects a subtraction
!!        ! finds the time in seconds between the two dates
!!        write(*,*)'DIFFERENCE (subtracting one date_time from another)=',&
!!        & answer-event
!!
!!        ! OVERLOADED OPERATORS (logical comparisons)
!!        ! NOTE COMPARISONS ARE PERFORMED BY
!!        ! CONVERTING TIMES TO INTEGER SECONDS
!!        write(*,*)'> ',event.eq.event   ,event.lt.event   ,event.gt.event &
!!        & ,event.le.event   ,event.ge.event   ,event.ne.event
!!        !
!!        write(*,*)'> ',event.eq.answer  ,event.lt.answer  ,event.gt.answer  &
!!        & ,event.le.answer  ,event.ge.answer  ,event.ne.answer
!!        !
!!        write(*,*)'> ',answer.eq.event  ,answer.lt.event  ,answer.gt.event  &
!!        & ,answer.le.event  ,answer.ge.event  ,answer.ne.event
!!
!!        ! %DELTA easily lets you change dates by common increments
!!        write(*,*)
!!        write(*,404)'%DELTA tests starting with date ',event%delta()
!!        !
!!        write(*,*) event%format("                             &
!!        &%W, %L %d, %Y %H:%m:%s %N")
!!
!!        write(*,*)'Remember years and months are not constant units'
!!
!!        answer=event%delta(year=1)
!!        write(*,*)answer%format(&
!!        & "FOR %%DELTA(YEAR=+1)            %W, %L %d, %Y %H:%m:%s %N")
!!        answer=event%delta(year=-1)
!!        write(*,*)answer%format(&
!!        & "FOR %%DELTA(YEAR=-1)            %W, %L %d, %Y %H:%m:%s %N")
!!
!!        answer=event%delta(month=24)
!!        write(*,*)answer%format(&
!!        & "FOR %%DELTA(MONTH=+24)          %W, %L %d, %Y %H:%m:%s %N")
!!        !
!!        answer=event%delta(month=-24)
!!        write(*,*)answer%format(&
!!        & "FOR %%DELTA(MONTH=-24)          %W, %L %d, %Y %H:%m:%s %N")
!!
!!        answer=event%delta(week=1)
!!        write(*,*)answer%format(&
!!        & "FOR %%DELTA(WEEK=+1)            %W, %L %d, %Y %H:%m:%s %N")
!!        !
!!        answer=event%delta(week=-1)
!!        write(*,*)answer%format(&
!!        & "FOR %%DELTA(WEEK=-1)            %W, %L %d, %Y %H:%m:%s %N")
!!
!!        answer=event%delta(day=1)
!!        write(*,*)answer%format(&
!!        & "FOR %%DELTA(DAY=+1)             %W, %L %d, %Y %H:%m:%s %N")
!!        !
!!        answer=event%delta(day=-1)
!!        write(*,*)answer%format(&
!!        & "FOR %%DELTA(DAY=-1)             %W, %L %d, %Y %H:%m:%s %N")
!!
!!        answer=event%delta(hour=4)
!!        write(*,*)answer%format(&
!!        !
!!        & "FOR %%DELTA(HOUR=+4)            %W, %L %d, %Y %H:%m:%s %N")
!!        answer=event%delta(hour=-4)
!!        write(*,*)answer%format(&
!!        & "FOR %%DELTA(HOUR=-4)            %W, %L %d, %Y %H:%m:%s %N")
!!
!!        answer=event%delta(minute=180)
!!        write(*,*)answer%format(&
!!        & "FOR %%DELTA(MINUTE=+180)        %W, %L %d, %Y %H:%m:%s %N")
!!        !
!!        answer=event%delta(minute=-180)
!!        write(*,*)answer%format(&
!!        & "FOR %%DELTA(MINUTE=-180)        %W, %L %d, %Y %H:%m:%s %N")
!!
!!        answer=event%delta(second=1800)
!!        write(*,*)answer%format(&
!!        & "FOR %%DELTA(SECOND=+1800)       %W, %L %d, %Y %H:%m:%s %N")
!!        !
!!        answer=event%delta(second=-1800)
!!        write(*,*)answer%format(&
!!        & "FOR %%DELTA(SECOND=-1800)       %W, %L %d, %Y %H:%m:%s %N")
!!
!!        answer=event%delta(millisecond=10000)
!!        write(*,*)answer%format(&
!!        & "FOR %%DELTA(MILLISECOND=+10000) %W, %L %d, %Y %H:%m:%s %N")
!!        !
!!        answer=event%delta(millisecond=-10000)
!!        write(*,*)answer%format(&
!!        & "FOR %%DELTA(MILLISECOND=-10000) %W, %L %d, %Y %H:%m:%s %N")
!!
!!        answer=event%delta(year=3,month=2,day=100,hour=200,&
!!        & week=-1,minute=300,second=1000,millisecond=-10000)
!!        write(*,*)answer%format(&
!!        !
!!        &"FOR %%DELTA(year=3,month=2,day=100,hour=200,&
!!        &week=-1,minute=300,second=1000,millisecond=100000)&
!!        & %W, %L %d, %Y %H:%m:%s %N")
!!
!!        answer=event%delta(duration="1-20:30:40.50")
!!        write(*,*)answer%format(&
!!        & "FOR %%DELTA(DURATION='1-20:30:40.50')&
!!        & %W, %L %d, %Y %H:%m:%s %N")
!!
!!     end program demo_M_time_oop
!!
!! Sample output:
!!
!!  Various initialization styes
!!   DEFAULT CONSTRUCTOR %format()               1970-01-01T00:00:00.000+00:00
!!   DEFAULT CONSTRUCTOR %format("")             1970-01-01T00:00:00.000+00:00
!!   DEFAULT CONSTRUCTOR %format(user-specified) 1970-01-01T00:00:00.000+00:00
!!   DEFAULT CONSTRUCTOR %format("USA")          Thursday, January 1st, 1970 12:00:00 AM
!!   DEFAULT CONSTRUCTOR setting values          1492-10-12T00:00:00.000+00:00
!!   DEFAULT CONSTRUCTOR with partial values     2016-06-11T00:00:00.000+00:00
!!   DEFAULT CONSTRUCTOR with values by name     2016-06-11T21:09:11.500-04:00
!!   CONSTRUCTOR with a dat array                1776-07-04T00:00:00.000+00:00
!!   CONSTRUCTOR with a partial dat array        1776-07-04T20:00:00.000-04:00
!!
!!   Print members of type(DATE_TIME)
!!   EVENT=2020,10,24,-240,21,49,54,105
!!   %year        Year................... 2020
!!   %month       Month.................. 10
!!   %day         Day.................... 24
!!   %tz          Timezone............... -240
!!   %hour        Hour................... 21
!!   %minute      Minute................. 49
!!   %second      Second................. 54
!!   %millisecond Millisecond............ 105
!!   Print methods of type(DATE_TIME)
!!   %ordinal     Ordinal day of year.... 298
!!   %weekday     Weekday................ 6
!!   %epoch      Unix epoch time........ 1603590594.1049695
!!   %julian     Julian date............ 2459147.5763206594
!!
!!   Formatted Strings (%format("STRING")  -- see fmtdate(3f) for format descriptions
!!   Short month............ "Oct"
!!   Month.................. "October"
!!   Short week............. "Sat"
!!   Week .................. "Saturday"
!!   Calendar Time ......... "2020-10-24 21:49:54.105 -04:00"
!!   Calendar Time ......... ""2020-10-24 21:49:54.105 -0400""
!!   Longer format.......... "Saturday, October 24th, 2020 9:49:54 PM"
!!
!!   DAT=2020,10,24,-240,21,49,54,105
!!   TOMORROW="Sunday, October 25th, 2020 9:49:54 PM"
!!   YESTERDAY="Friday, October 23rd, 2020 9:49:54 PM"
!!   DIFFERENCE (subtracting one date_time from another)=  -86400.000000000000
!!   >  T F F T T F
!!   >  F F T F T T
!!   >  F T F T F T
!!
!!   %DELTA tests starting with date 2020,10,24,-240,21,49,54,105
!!                                Saturday, October 24th, 2020 9:49:54 PM
!!   Remember years and months are not constant units
!!   FOR %DELTA(YEAR=+1)            Sunday, October 24th, 2021 9:49:54 PM
!!   FOR %DELTA(YEAR=-1)            Thursday, October 24th, 2019 9:49:54 PM
!!   FOR %DELTA(MONTH=+24)          Wednesday, October 26th, 2022 9:49:54 PM
!!   FOR %DELTA(MONTH=-24)          Wednesday, October 24th, 2018 9:49:54 PM
!!   FOR %DELTA(WEEK=+1)            Saturday, October 31st, 2020 9:49:54 PM
!!   FOR %DELTA(WEEK=-1)            Saturday, October 17th, 2020 9:49:54 PM
!!   FOR %DELTA(DAY=+1)             Sunday, October 25th, 2020 9:49:54 PM
!!   FOR %DELTA(DAY=-1)             Friday, October 23rd, 2020 9:49:54 PM
!!   FOR %DELTA(HOUR=+4)            Sunday, October 25th, 2020 1:49:54 AM
!!   FOR %DELTA(HOUR=-4)            Saturday, October 24th, 2020 5:49:54 PM
!!   FOR %DELTA(MINUTE=+180)        Sunday, October 25th, 2020 12:49:54 AM
!!   FOR %DELTA(MINUTE=-180)        Saturday, October 24th, 2020 6:49:54 PM
!!   FOR %DELTA(SECOND=+1800)       Saturday, October 24th, 2020 10:19:54 PM
!!   FOR %DELTA(SECOND=-1800)       Saturday, October 24th, 2020 9:19:54 PM
!!   FOR %DELTA(MILLISECOND=+10000) Saturday, October 24th, 2020 9:50:04 PM
!!   FOR %DELTA(MILLISECOND=-10000) Saturday, October 24th, 2020 9:49:44 PM
!!   FOR %DELTA(year=3,month=2,day=100,hour=200,week=-1,minute=300,
!!   second=1000,millisecond=100000) Thursday, April 4th, 2024 11:06:24 AM
!!   FOR %DELTA(DURATION='1-20:30:40.50') Monday, October 26th, 2020 6:20:34 PM
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! submodule is not supported by the compiler used to develop this yet or it would be worth a try
!!submodule (M_time) M_time_oop
!!end submodule M_time_oop
!
module M_time_oop
!
! Define an OOP (Object-Oriented Programming) interface for the M_time module.
!
! Leveraging the existing procedural functions in module M_TIME to do the calculations allows
! this to simply be a definition of a derived type ( TYPE(DATE_TIME) ) and the
! methods it supports and overloading of operators to support the new data type.
!
use M_time, only : d2u, u2d, fmtdate, d2o, dow, fmtdate_usage, days2sec, realtime
use M_time, only : j2d, d2j
use M_strings, only : upper
implicit none
private
private upper
!-----------------------------------------------------------------------------------------------------------------------------------
   public date_time

   private dt2d_
   private epoch_
   private julian_
   private ordinal
   private weekday
   private format
   private delta
   private init_dt
!-----------------------------------------------------------------------------------------------------------------------------------
!DERIVED TYPE DATE_TIME
!
type date_time
   ! COMPONENTS:
   ! Eight integer element integer elements containing year, month, day, Time zone difference
   ! from UTC in minutes, hour, minutes, seconds, and milliseconds of the second in one-to-one
   ! correspondence with the elements of the array generated by the intrinsic function DATE_AND_TIME()...
   integer :: year=1970
   integer :: month=1
   integer :: day=1
   integer :: tz=0
   integer :: hour=0
   integer :: minute=0
   integer :: second=0
   integer :: millisecond=0
contains
   ! METHODS:
   procedure         :: datout => dt2d_
   procedure         :: epoch  => epoch_
   procedure         :: julian => julian_
   procedure         :: ordinal
   procedure         :: weekday
   procedure         :: format
   procedure         :: delta
   procedure         :: init => init_dt
   !DECLARATION OF OVERLOADED OPERATORS FOR TYPE(DATE_TIME)

   procedure,private :: plus_seconds
   generic           :: operator(+) => plus_seconds

   procedure,private :: eq
   generic           :: operator(==) => eq

   procedure,private :: lt
   generic           :: operator(<)  => lt

   procedure,private :: gt
   generic           :: operator(>)  => gt

   procedure,private :: ge
   generic           :: operator(>=) => ge

   procedure,private :: le
   generic           :: operator(<=) => le

   procedure,private :: ne
   generic           :: operator(/=) => ne

!-! procedure         :: construct_from_dat
!-! generic           :: assignment(=)  => construct_from_dat

   procedure,private :: minus_seconds                     ! subtracts seconds, returns a new date_time
   procedure,private :: minus_date_time                   ! returns seconds
   generic           :: operator(-)  => minus_seconds
   generic           :: operator(-)  => minus_date_time
end type
!===================================================================================================================================
! User-defined constructors are created by defining a generic interface
! with the same name as the derived type they're supposed to construct.
interface date_time
   module procedure construct_from_dat
   !-!module procedure construct_from_jed
   !-!module procedure construct_from_uet
end interface date_time
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! this function is used internally in the module, but is also declared to be a constructor for creating TYPE(DATE_TYPE) structures
!
function construct_from_dat(dat)

! ident_1="@(#)M_time::construct_from_dat(3f): construct TYPE(DATE_TIME) with DAT date-time array"

integer,intent(in)          :: dat(:)                       ! (maybe partial) date time array
integer                     :: datlocal(8)                  ! date time array similar to that returned by DATE_AND_TIME
type(date_time)             :: construct_from_dat

   datlocal=u2d(0.0d0)                                      ! initialize to start of Unix Epoch Time using local time zone
   if(size(dat).gt.0)then                                   ! allow for partial DAT arrays
      datlocal(:size(dat))=dat
   endif
   construct_from_dat%year=datlocal(1)
   construct_from_dat%month=datlocal(2)
   construct_from_dat%day=datlocal(3)
   construct_from_dat%tz=datlocal(4)
   construct_from_dat%hour=datlocal(5)
   construct_from_dat%minute=datlocal(6)
   construct_from_dat%second=datlocal(7)
   construct_from_dat%millisecond=datlocal(8)
end function construct_from_dat
!===================================================================================================================================
function construct_from_jed(jed)

! ident_2="@(#)M_time::construct_from_jed(3f): construct TYPE(DATE_TIME) with REAL Julian JED date-time value"

real(kind=realtime),intent(in)   :: jed
type(date_time)                 :: construct_from_jed
   construct_from_jed=construct_from_dat(j2d(jed))
end function construct_from_jed
!===================================================================================================================================
function construct_from_uet(uet)

! ident_3="@(#)M_time::construct_from_uet(3f): construct TYPE(DATE_TIME) with INTEGER Unix UET date-time value"

integer,intent(in)   :: uet
type(date_time)                 :: construct_from_uet
   construct_from_uet=construct_from_dat(u2d(real(uet,kind=realtime)))
end function construct_from_uet
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! DEFINE THE METHODS FOR THE TYPE
! These functions are privately used to define the methods that TYPE(DATE_TIME) will support
!===================================================================================================================================
function dt2d_(self) result (dat)

! ident_4="@(#)M_time::dt2d_(3f): convert derived type date_time to DAT date-time array"

class(date_time),intent(in) :: self
integer                     :: dat(8)                  ! date time array similar to that returned by DATE_AND_TIME
   dat=[self%year, self%month, self%day, self%tz, self%hour, self%minute, self%second, self%millisecond]
end function dt2d_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function epoch_(self) result (epoch_seconds)

! ident_5="@(#)M_time::epoch_(3f): convert derived type date_time to unix epoch seconds"

class(date_time),intent(in) :: self
real(kind=realtime)         :: epoch_seconds
   epoch_seconds=d2u(dt2d_(self))
end function epoch_
!===================================================================================================================================
function format(self,fmt) result (string)

! ident_6="@(#)M_time::format(3f): convert derived type date_time to formatted string"

class(date_time),intent(in)           :: self
character(len=*),intent(in),optional  :: fmt
character(len=:),allocatable          :: fmtlocal
character(len=:),allocatable          :: string
character(len=*),parameter            :: iso_fmt='%Y-%M-%DT%h:%m:%s.%x%z'
character(len=*),parameter            :: usa_fmt='%W, %L %d, %Y %H:%m:%s %N'
character(len=*),parameter            :: ymd_fmt='%Y-%M-%D %h:%m:%s.%x%z'
character(len=*),parameter            :: mdy_fmt='%M/%D/%Y %h:%m:%s.%x%z'
   if(present(fmt))then
      fmtlocal=fmt
   else
      fmtlocal=iso_fmt
   endif
   if(index(fmtlocal,'%').eq.0)then       ! if a percent(%) in string assume it is a string to be passed to fmtdate
      select case(upper(fmtlocal))
      case("ISO","ISO-8601",""); fmtlocal=iso_fmt
      case("USA");               fmtlocal=usa_fmt
      case("YMD");               fmtlocal=ymd_fmt
      case("MDY");               fmtlocal=mdy_fmt
      case default
         !! usually used in a WRITE(3f) or PRINT(3f) so should not write output
         !write(*,*)'date_time%format: unknown format name'
         !write(*,*)'use predefined names ("iso","usa","ymd","mdy")'
         !write(*,*)'or a user-supplied format including the following macros:'
         !call fmtdate_usage()
         !fmtlocal='date_time%format: unknown format name '//trim(fmtlocal)//'(not "iso","usa","ymd","mdy" or %macros)'//iso_fmt
      end select
   endif
   string=fmtdate(dt2d_(self),fmtlocal)
end function format
!===================================================================================================================================
function julian_(self) result (julian_days)

! ident_7="@(#)M_time::julian_(3f): convert derived type date_time to julian date"

class(date_time),intent(in) :: self
real(kind=realtime)         :: julian_days
    julian_days=d2j(dt2d_(self))
end function julian_
!===================================================================================================================================
function ordinal(self) result (ordinal_days)

! ident_8="@(#)M_time::ordinal(3f): convert derived type date_time to ordinal date"

class(date_time),intent(in) :: self
integer                     :: ordinal_days
    ordinal_days=d2o(dt2d_(self))
end function ordinal
!===================================================================================================================================
function weekday(self) result (iday)

! ident_9="@(#)M_time::weekday(3f): convert derived type date_time to weekday (1=Monday,7=Sunday)"

class(date_time),intent(in)   :: self
integer                       :: iday
integer                       :: ierr      ! Error return,0=correct,-1=invalid Julian Date,-2=neither day nor weekday specified
   call dow(dt2d_(self),weekday=iday,ierr=ierr)
end function weekday
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function delta(self,year,month,day,tz,hour,minute,second,millisecond,week,duration)
!
! allow keyword addition and subtraction from each member of a date_time object
! even though there is no specific size for month and year, this is conventionally what is meant by "a year from now"
! or "a month from now". Once the arbitrary values are used to change the original date_time value convert it to
! Epoch time and back to make sure you get a valid date.

! ident_10="@(#)M_time::delta(3f): add times to a type(date_time)"

class(date_time),intent(in)           :: self
integer,intent(in),optional           :: year, month, day, tz, hour, minute, second, millisecond, week
character(len=*),intent(in),optional  :: duration
type(date_time)                       :: delta

   delta=self
   if(present(year))then;        delta%year=self%year               + year        ;else; delta%year=self%year               ; endif
   if(present(month))then;       delta%month=self%month             + month       ;else; delta%month=self%month             ; endif
   if(present(day))then;         delta%day=self%day                 + day         ;else; delta%day=self%day                 ; endif
   if(present(tz))then;          delta%tz=self%tz                   + tz          ;else; delta%tz=self%tz                   ; endif
   if(present(hour))then;        delta%hour=self%hour               + hour        ;else; delta%hour=self%hour               ; endif
   if(present(minute))then;      delta%minute=self%minute           + minute      ;else; delta%minute=self%minute           ; endif
   if(present(second))then;      delta%second=self%second           + second      ;else; delta%second=self%second           ; endif
   if(present(millisecond))then; delta%millisecond=self%millisecond + millisecond ;else; delta%millisecond=self%millisecond ; endif

   if(present(week))then;        delta%day=delta%day                + week*7                  ; endif
   if(present(duration))then;    delta%second=delta%second          + int(days2sec(duration)) ; endif

   delta=construct_from_dat(u2d(d2u(dt2d_(delta)))) ! to get a valid date after arbitrary math convert to Epoch and back

end function delta
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine init_dt(self,year,month,day,tz,hour,minute,second,millisecond,type,dat)
!
! allow for date_time object to be initialized. Default is current time
! If the 8-element dat array is present use it to initialize the date_time object
! If not, initialize to the current time or start of epoch depending on TYPE=["now"|"epoch"]
! Then, apply specific values, typically specified by keyword value

! ident_11="@(#)M_time::init_dt(3f): initialize TYPE(DATE_TIME)"

class(date_time)                     :: self
type(date_time)                      :: holddt
integer,intent(in),optional          :: year, month, day, tz, hour, minute, second, millisecond
character(len=*),intent(in),optional :: type
integer,intent(in),optional          :: dat(8)
character(len=10)                    :: typelocal
integer                              :: datlocal(8)

   if(present(dat))then

      holddt=construct_from_dat(dat)

   else

      if(present(type))then
         typelocal=type
      else
         typelocal="now"
      endif

      select case(typelocal)
      case("now","NOW")
         call date_and_time(values=datlocal)             ! current time is placed in array using standard procedure
         holddt=construct_from_dat(datlocal)             ! convert date array to date_time type
      case("epoch","EPOCH")
         holddt=construct_from_dat([1970,1,1,0,0,0,0,0])
      case default
         call date_and_time(values=datlocal)             ! current time is placed in array using standard procedure
         holddt=construct_from_dat(datlocal)             ! convert date array to date_time type
      end select

   endif

   if(present(year))         holddt%year=year
   if(present(month))        holddt%month=month
   if(present(day))          holddt%day=day
   if(present(tz))           holddt%tz=tz
   if(present(hour))         holddt%hour=hour
   if(present(minute))       holddt%minute=minute
   if(present(second))       holddt%second=second
   if(present(millisecond))  holddt%millisecond=millisecond

   holddt=construct_from_dat(u2d(d2u(dt2d_(holddt)))) ! to get a valid date after arbitrary values convert to Epoch and back

   self%year=holddt%year
   self%month=holddt%month
   self%day=holddt%day
   self%tz=holddt%tz
   self%hour=holddt%hour
   self%minute=holddt%minute
   self%second=holddt%second
   self%millisecond=holddt%millisecond

end subroutine init_dt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! FUNCTIONS FOR DEFINING OVERLOADED OPERATORS
!===================================================================================================================================
function plus_seconds(self,seconds) result (dattim)

! ident_12="@(#)M_time::plus_seconds(3f): add derived type date_time object and seconds"

class(date_time),intent(in)    :: self
real(kind=realtime),intent(in) :: seconds
type(date_time)                :: dattim
   ! convert TYPE(DATE_TIME) to a DAT array for input into d2u (DAT to UNIX time) and add seconds
   ! take the seconds (ie. the Unix Epoch time) and convert it back into a DAT array and call the
   ! construct_from_dat() function to convert the DAT array back to a TYPE(DATE_TIME)
   dattim=construct_from_dat(u2d(d2u(dt2d_(self))+seconds))
end function plus_seconds
!===================================================================================================================================
function minus_seconds(self,seconds) result (dattim)

! ident_13="@(#)M_time::minus_seconds(3f): subtract seconds from derived type date_time object"

class(date_time),intent(in)    :: self
real(kind=realtime),intent(in) :: seconds
type(date_time)                :: dattim
   dattim=construct_from_dat(u2d(d2u(dt2d_(self))-seconds))
end function minus_seconds
!===================================================================================================================================
function minus_date_time(self,other) result (seconds)

! ident_14="@(#)M_time::minus_date_time(3f): add derived type date_time object and seconds"

class(date_time),intent(in)   :: self
type(date_time),intent(in)    :: other
real(kind=realtime)           :: seconds
   seconds= d2u(dt2d_(self))- d2u(dt2d_(other))
end function minus_date_time
!===================================================================================================================================
logical function eq(self,other)

! ident_15="@(#)M_time::eq(3f): compare derived type date_time objects (eq,lt,gt,le,ge,ne)"

class(date_time),intent(in)   :: self
type(date_time),intent(in)    :: other
   eq= int(d2u(dt2d_(self))) .eq. int(d2u(dt2d_(other)))
end function eq

logical function lt(self,other)
class(date_time),intent(in)   :: self
type(date_time),intent(in)    :: other
   lt= int(d2u(dt2d_(self))) .lt. int(d2u(dt2d_(other)))
end function lt

logical function gt(self,other)
class(date_time),intent(in)   :: self
type(date_time),intent(in)    :: other
   gt= int(d2u(dt2d_(self))) .gt. int(d2u(dt2d_(other)))
end function gt

logical function le(self,other)
class(date_time),intent(in)   :: self
type(date_time),intent(in)    :: other
   le= int(d2u(dt2d_(self))) .le. int(d2u(dt2d_(other)))
end function le

logical function ge(self,other)
class(date_time),intent(in)   :: self
type(date_time),intent(in)    :: other
   ge= int(d2u(dt2d_(self))) .ge. int(d2u(dt2d_(other)))
end function ge

logical function ne(self,other)
class(date_time),intent(in)   :: self
type(date_time),intent(in)    :: other
   ne= int(d2u(dt2d_(self))) .ne. int(d2u(dt2d_(other)))
end function ne
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_time_oop
