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
'       compute(1f) - [NUMBERS]evaluate a calculator expression                  ',&
'SYNOPSIS                                                                        ',&
'       compute [STRING] [-verbose]| [-help| -version]                           ',&
'DESCRIPTION                                                                     ',&
'       Given any expression call the JUCALC(3f) calculator function             ',&
'       and evaluate it. If no expression is present, read expressions           ',&
'       from stdin until a line composed of a period(".") or end of data         ',&
'       is encountered.                                                          ',&
'                                                                                ',&
'       Expressions are similar to Fortran77 syntax except powers are            ',&
'       processed from left to right.                                            ',&
'OPTIONS                                                                         ',&
'       STRING     calculator expression to evaluate                             ',&
'       --help     display this help and exit                                    ',&
'       --version  output version information and exit                           ',&
'EXAMPLES                                                                        ',&
'       Sample commands:                                                         ',&
'                                                                                ',&
'        $ compute ''(sin(30.33333)*2)**2+40.0/2.3-1.23e3''                      ',&
'                                                                                ',&
'        $ compute funcs # list available functions                              ',&
'                                                                                ',&
'SEE ALSO                                                                        ',&
'       M_calculator(3fm)                                                        ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        compute(1f) - [NUMBERS]evaluate a calculator expression
!!##SYNOPSIS
!!
!!        compute [STRING] [-verbose]| [-help| -version]
!!##DESCRIPTION
!!        Given any expression call the JUCALC(3f) calculator function
!!        and evaluate it. If no expression is present, read expressions
!!        from stdin until a line composed of a period(".") or end of data
!!        is encountered.
!!
!!        Expressions are similar to Fortran77 syntax except powers are
!!        processed from left to right.
!!##OPTIONS
!!        STRING     calculator expression to evaluate
!!        --help     display this help and exit
!!        --version  output version information and exit
!!##EXAMPLES
!!
!!        Sample commands:
!!
!!         $ compute '(sin(30.33333)*2)**2+40.0/2.3-1.23e3'
!!
!!         $ compute funcs # list available functions
!!
!!##SEE ALSO
!!        M_calculator(3fm)
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
'@(#)PROGRAM:        compute(1f)>',&
'@(#)DESCRIPTION:    line mode calculator program (that calls jucalc(3f))>',&
'@(#)VERSION:        23.1 20160618>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:01:24 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program compute
use M_kracken, only : kracken, sget, lget
use m_calculator, only : jucalc,iclen_calc
!use M_noown, only     : juown1, c
implicit real(kind=selected_real_kind(15,300)) (a-h, o-z)
character(len=iclen_calc) :: line
character(len=iclen_calc) :: outlin
character(len=iclen_calc) :: event
doubleprecision           :: rvalue
integer                   :: ios
integer                   :: ierr=0
logical                   :: verbose
   call jucalc('ownmode(1)',outlin,event,rvalue,ierr)  ! specify user-supplied juown1 routine contains user routines for calculator
   call kracken('compute',' -oo -help .f. -version .f. -verbose .f.') ! define and crack command line arguments
   call help_usage(lget('compute_help'))               ! process -help switch
   call help_version(lget('compute_version'))          ! process -version switch
   verbose=lget('compute_verbose')                     ! test if -verbose switch is present on command line
   line=sget('compute_oo')                             ! get any expressions from command line to evaluate
   if(line.ne.'')then                                  ! if expressions on command line evaluate them
      call processit()
   else
      INFINITE: do                                     ! no expressions on command line to read expressions from stdin
         read(*,'(a)',iostat=ios)line
         if(ios.ne.0)   exit INFINITE
         if(line.eq.'.')exit INFINITE                  ! quit if user enters command "."
         call processit()
      enddo INFINITE
   endif
contains
   subroutine processit
      call jucalc(line,outlin,event,rvalue,ierr)
      select case(ierr)                                                       ! several different meanings to the error flag
      case(-1);     write(*,'(a,a)')'error===>',event(:len_trim(event))       ! an error has occurred
      case(0)
             if(verbose)then
                write(*,'(a,a,a)')trim(outlin),' = ',trim(line)               ! a numeric value was returned without error
             else
                write(*,'(a)')trim(outlin)                                    ! a numeric value was returned without error
             endif
      case(1);      write(*,'(a,a)')'message===>',event(:len_trim(event))     ! request for a message (from DUMP or FUNC)
      case(2);      write(*,'(a)')event(:int(rvalue))                         ! a string value was returned without error
      case default; write(*,'(a,i10)')'*compute* unexpected ierr value ',ierr ! this should not occur
      end select
   end subroutine processit
end program compute
