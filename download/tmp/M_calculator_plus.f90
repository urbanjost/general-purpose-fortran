module M_calculator_plus
use m_calculator, only : iclen_calc,jucalc
implicit none
private
public :: inum0   ! resolve a calculator string into a whole integer number
public :: rnum0   ! resolve a calculator string into a real number (return 0 on errors)
public :: dnum0   ! resolve a calculator string into a doubleprecision number (return 0 on error s)
public :: snum0   ! resolve a calculator expression into a string(return blank on errors)
public :: jucalcx ! call jucalc() calculator and display messages
public :: strgarr ! read a string into an array USING CALCULATOR
public :: strgar2 ! read a string into an array USING CALCULATOR
contains
!>
!!##NAME
!!      inum0(3f) - [M_calculator_plus]return integer value from calculator expression
!!##SYNOPSIS
!!
!!   integer function inum0(inline,ierr)
!!
!!    character(len=*),intent(in)  :: inline
!!    integer,optional,intent(out) :: ierr
!!
!!##SYNOPSIS
!!
!!    INUM0() evaluates a CHARACTER argument as a FORTRAN-like
!!    calculator expression and returns an integer.
!!
!!     o INUM0() uses the calculator routine jucalc(3f)
!!     o Remember that the calculator treats all values as DOUBLEPRECISION.
!!
!!    Values returned are assumed to be very close to being whole integer
!!    values.  A small value (0.01) is added to the result before it is
!!    returned to reduce roundoff error problems. This could introduce
!!    errors if INUM0 is misused and is not being used to calculate
!!    essentially integer results.
!!##DESCRIPTION
!!
!!    inline  INLINE is a CHARACTER variable up to 255 characters long that is
!!            similar to a FORTRAN 77 numeric expression. Keep it less than 80
!!            characters when possible.
!!    ierr    zero (0) if no error occurs
!!
!!##DEPENDENCIES
!!       o jucalcx
!!       o User-supplied routines:
!!
!!         All programs that call the calculator routine must supply
!!         their own JUOWN1 and C procedures. See the
!!         ../html/Example.html">example program for samples.
!!           o juown1
!!           o c
!!##EXAMPLES
!!
!!    Sample program:
!!
!!       program demo_inum0
!!       ! NOTE: user must supply the JUOWN1 and C procedures.
!!       i=inum0('20/3.4')
!!       j=inum0('CI = 13 * 3.1')
!!       k=inum0('CI')
!!       write(*,*)'Answers are ',I,J,K
!!       end program demo_inum0
!!
!!##SEE ALSO
!!       The syntax of an expression is as described in
!!       the main document of  the  Calculator  Library.
!!   See
!!       JUCALC(),
!!       RNUM0(),
!!       DNUM0(),
!!       SNUM0(),
!!       STRGARR(),
!!       STRGAR2(),
!!       JUCALCX()
!!##REFERENCES
!!##NONE.
!===================================================================================================================================
!>
!! AUTHOR:  John S. Urban
!!##VERSION: 19971123
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
integer function inum0(inline,ierr)
use M_journal, only : journal
character(len=*),parameter :: ident="@(#)M_calculator_plus::inum0(3f):resolve a calculator string into a whole integer number"
!  The special string '*' returns -99999, otherwise return 0 on errors
character(len=*),intent(in)  :: inline
integer,optional,intent(out) :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
integer,parameter            :: IBIG=2147483647               ! overflow value (2**31-1)
integer                      :: iend
real,parameter               :: SMALL=0.0001                  ! and epsilon value
doubleprecision              :: dnum1
character(len=iclen_calc)    :: cdum20
integer                      :: ierr_local
integer                      :: ilen
!-----------------------------------------------------------------------------------------------------------------------------------
ierr_local=0
if(inline.eq.' ')then                                      ! return 0 for a blank string
   dnum1=0.0d0
elseif(inline.eq.'*')then                                  ! return -99999 on special string "*"
   dnum1=-99999d0
else                                                       ! parse string using calculator function
   iend=len(inline)
   call jucalcx(inline(:iend),dnum1,cdum20,ierr_local,ilen)
   if(ierr_local.ne.0)then
      dnum1=0.0d0
   endif
endif
if(present(ierr))then
   ierr=ierr_local
endif
!-----------------------------------------------------------------------------------------------------------------------------------
! on most machines int() would catch the overflow, but this is safer
if(dnum1.gt.IBIG)then
   call journal('sc','*inum0* integer overflow 2**31-1 <',dnum1)
   inum0=IBIG
elseif(dnum1.gt.0)then
   inum0=int(dnum1+SMALL)
else
   inum0=int(dnum1-SMALL)
endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function inum0
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!       rnum0(3f) - [M_calculator_plus]returns real number from string expression using JUCALC(3f)
!!##SYNOPSIS
!!
!! real function rnum0(inline)
!!     character(len=*), intent=(in) :: inline
!!     integer,intent(out),optional  :: ierr
!!
!!##DESCRIPTION
!!     RNUM0() is used to return a REAL value from a CHARACTER string representing
!!     a numeric expression. It uses the M_calculator(3fp) module.
!!
!!     inline  INLINE is a CHARACTER variable up to (iclen_calc=512) characters long
!!             that is similar to a FORTRAN 77 numeric expression.
!!     ierr    error code. If zero, no error occurred
!!
!!##DEPENDENCIES
!!       o jucalcx
!!       o User-supplied routines:
!!         All programs that call the calculator routine must supply their own
!!         JUOWN1 and C procedures. See the example program for samples.
!!           o juown1
!!           o c
!!##EXAMPLES
!!
!!   Sample program
!!
!!     program demo_rnum0
!!     ! NOTE: user must supply the JUOWN1 and C procedures.
!!     x=rnum0('20/3.4')
!!     y=rnum0('CI = 10 * sin(3.1416/4)')
!!     z=rnum0('CI')
!!     write(*,*)x,y,z
!!     end program demo_rnum0
!!
!!##SEE ALSO
!!
!!       o The syntax of an expression is as described in the main documentation
!!         of the Calculator Library.
!!       o See JUCALCX(3f), JUCALC(3f), STRGAR2(3f), INUM0(3f), DNUM0(3f), SNUM0(3f).
!!
!!##REFERENCES
!!       o NONE.
!===================================================================================================================================
!>
!! AUTHOR    John S. Urban
!!##VERSION   1.0,19971123
!===================================================================================================================================
real function rnum0(inline,ierr)
character(len=*),parameter :: ident="@(#)M_calculator_plus::rnum0(3f):resolve a calculator string into a real number"
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: inline
integer,optional,intent(out) :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=iclen_calc)    :: cdum20
doubleprecision              :: d_answer
integer                      :: ierr_local
integer                      :: ilen
integer                      :: iend
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr_local=0
   if(inline.eq.' ')then
      d_answer=0.0d0
   elseif(inline.eq.'*')then                            !  the special string '*' returns -99999.0
      d_answer=-99999.0d0
   else
      iend=len(inline)
      call jucalcx(inline(:iend),d_answer,cdum20,ierr_local,ilen)
      if(ierr_local.ne.0)then
         d_answer=0.0d0
      endif
   endif
   if(present(ierr))then
      ierr=ierr_local
   endif
   rnum0=real(d_answer)
!-----------------------------------------------------------------------------------------------------------------------------------
end function rnum0
!>
!!##NAME
!!      dnum0(3f) - [M_calculator_plus]return double precision value from string expression using JUCALC
!!##SYNOPSIS
!!
!!   doubleprecision function dnum0(inline,ierr)
!!
!!    character(len=*),intent(in) :: inline
!!    integer,optional,intent(out) :: ierr
!!
!!##DESCRIPTION
!!     DNUM0() is used to return a DOUBLEPRECISION value from a CHARACTER string
!!     representing a numeric expression.
!!       o If an error occurs in evaluating the expression INUM() returns zero(0).
!!       o DNUM0() ultimately uses the calculator routine jucalc.f .
!!
!!      inline  INLINE is a CHARACTER variable up to (iclen_calc=255) characters long
!!              that is similar to a FORTRAN 77 numeric expression.
!!      ierr    error code. If zero, no error occurred
!!
!!##DEPENDENCIES
!!       o jucalcx
!!       o User-supplied routines:
!!         All programs that call the calculator routine must supply their own
!!         JUOWN1 and C procedures. See the example program for samples.
!!           o juown1
!!           o c
!!##EXAMPLES
!!
!!   Sample Program
!!
!!     program demo_dnum0
!!     doubleprecision x,y,z
!!     ! NOTE: user must supply the JUOWN1 and C procedures.
!!     X=DNUM0('20/3.4')
!!     Y=DNUM0('CI = 10 * sin(3.1416/4)')
!!     Z=DNUM0('CI')
!!     write(*,*)x,y,z
!!     end program demo_dnum0
!!
!!##SEE ALSO
!!
!!       o The syntax of an expression is as described in the main documentation of the Calculator Library.
!!       o See JUCALCX(), JUCALC(), STRGAR2(), RNUM0(), SNUM0().
!!
!!##REFERENCES
!!       o NONE.
!===================================================================================================================================
!>
!! AUTHOR + John S. Urban
!!##VERSION 1.0, 19971123
!===================================================================================================================================
doubleprecision function dnum0(inline,ierr)
character(len=*),parameter :: ident="@(#)M_calculator_plus::dnum0(3f):resolve a calculator string into a doubleprecision number"
character(len=*),intent(in) :: inline
integer,optional,intent(out) :: ierr
character(len=iclen_calc)           :: cdum20
doubleprecision             :: dnum1
integer                     :: iend
integer                     :: ierr_local
integer                     :: ilen
   ierr_local=0
   if(inline.eq.' ')then
      dnum1=0.0d0
   elseif(inline.eq.'*')then    !  the special string '*' returns -99999.0
      dnum1=-99999.0d0
   else
      iend=len(inline)
      call jucalcx(inline(:iend),dnum1,cdum20,ierr_local,ilen)
      if(ierr_local.ne.0)then
         dnum1=0.0d0
      endif
   endif
   dnum0=dnum1
   if(present(ierr))then
      ierr=ierr_local
   endif
end function dnum0
!>
!!##NAME
!!     snum0(3f) - [M_calculator_plus]resolve a calculator expression into a string(return blank on errors)
!!
!!##SYNOPSIS
!!
!!   function snum0(inline0,ierr)
!!
!!    character(len=:),allocatable :: snum0(inline0)
!!    character(len=*),intent(in)  :: inline0                           ! input string
!!    integer,optional,intent(out) :: ierr
!!
!!##DESCRIPTION
!!     SNUM0() is used to return a string value up to (iclen_calc=512) characters
!!     long from a string expression.
!!     SNUM0() uses the calculator routine jucalc.f .
!!
!!     inline0  INLINE0 is a CHARACTER variable up to (iclen_calc=512) characters long that
!!              is similar to a FORTRAN 77 expression.
!!     ierr     error code. If zero, no error occurred
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     !     NOTE: user must supply the JUOWN1 and C procedures.
!!     program DEMO_SNUM0
!!     use m_calculator_plus, only: iclen_calc, rnum0, snum0
!!     CHARACTER(len=80)  :: IC,JC,KC
!!
!!     RDUM=RNUM0('A=83/2') ! set a variable in the calculator
!!     KC=SNUM0('$MYTITLE="This is my title variable"')
!!
!!     IC=SNUM0('$STR("VALUE IS [",A,"]")')
!!     JC=SNUM0('$MYTITLE')
!!
!!     WRITE(*,*)'IC=',TRIM(IC)
!!     WRITE(*,*)'JC=',TRIM(JC)
!!     WRITE(*,*)'KC=',TRIM(KC)
!!
!!     end program DEMO_SNUM0
!!     $endif
!!
!!    The output should look like
!!
!!      IC=VALUE IS [41.5]
!!      JC=This is my title variable
!!      KC=This is my title variable
!!
!!##DEPENDENCIES
!!       o jucalcx
!!       o User-supplied routines:
!!         All programs that call the calculator routine must supply their own
!!         JUOWN1 and C procedures. See the example program for samples.
!!           o juown1
!!           o c
!!
!!##SEE ALSO
!!       o The syntax of an expression is described in the main document of the
!!         Calculator Library.
!!       o See JUCALC(), RNUM0(), SNUM0(), STRGAR2(), JUCALCX().
!!
!!##REFERENCES
!!       o NONE.
!===================================================================================================================================
!>
!! AUTHOR    John S. Urban
!!##VERSION   1.0, 19971123
!===================================================================================================================================
!===================================================================================================================================
function snum0(inline0,ierr)
use m_calculator, only: iclen_calc
character(len=*),parameter :: ident="@(#)M_calculator_plus::snum0(3f):resolve a calculator expression into a string"
!  a few odd things are done because some compilers did not work as expected
character(len=:),allocatable :: snum0
character(len=*),intent(in)  :: inline0                           ! input string
integer,optional,intent(out) :: ierr
character(len=iclen_calc)    :: lcopy                             ! working string
character(len=iclen_calc)    :: inline                            ! working copy of input string
integer                      :: ierr_local
integer                      :: iend                              ! size of input string
integer                      :: ilen
doubleprecision              :: dnum1

   inline=inline0                                                 ! some compilers need a copy of known length to work as expected
   ierr_local=0
   if(inline.eq.' ')then                                          ! what to do for a blank string
      snum0=' '                                                   ! return a blank string
   else                                                           ! non-blank input expression
      iend=len(inline)                                            ! size of working string
      lcopy=' '                                                   ! initialize trimmed string
      lcopy=adjustl(inline(:iend))                                ! trim leading spaces
      if(lcopy(1:1).eq.'$'.or.lcopy(1:1).eq.'"')then              ! this is a string that needs evaluated
         dnum1=0.0d0
         call jucalcx(inline(:iend),dnum1,lcopy,ierr_local,ilen)
         if(ierr_local.ne.2)then                                  ! check if expression was evaluated to a string successfully
            snum0=' '                                             ! input string was not resolved to a string
         endif
         snum0=lcopy(:max(1,ilen))                                ! return whatever jucalcx() returned
      else                                                        ! input was just a string, not an expression so just copy it
         snum0=inline(:iend)                                      ! copy input string to output
      endif
   endif
   if(present(ierr))then
      ierr=ierr_local
   endif
end function snum0
!===================================================================================================================================
!>
!!##NAME
!!     jucalcx(3f) - [M_calculator_plus]return value from a string expression processing messages to simplify call to JUCALC(3f)
!!##SYNOPSIS
!!
!!    subroutine jucalcx(inlin0,outval,outlin0,ierr,ilen)
!!
!!     character(len=*), intent=(in)  :: inlin0
!!     doubleprecision, intent=(out)  :: outval
!!     character(len=*), intent=(out) :: outlin0
!!     integer, intent=(out)          :: ierr
!!     integer, intent=(out)          :: ilen
!!
!!##DESCRIPTION
!!     JUCALCX() takes a string containing a FORTRAN-like expression and evaluates
!!     it and returns a numeric or string value as appropriate.
!!     The main purpose of JUCALCX() is to assume the burden of displaying the
!!     calculator messages for codes that make multiple calls to JUCALC(). JUCALC
!!     () does not display error messages directly.
!!       o JUCALCX() calls the calculator routine jucalc.f to evaluate the
!!         expressions.
!!       o Messages beginning with a # are considered comments and are not passed
!!         on to the calculator.
!!
!!     inlin0  INLIN0 is a string containing a numeric expression. The expression can
!!             be up to (iclen_calc=512) characters long. The syntax of an expression
!!             is as described in the main document of the Calc library. For example:
!!
!!               'A=sin(3.1416/5)'
!!               '# this is a comment'
!!               '$STR("The value is ",40/3)'
!!
!!     outval  OUTVAL is a numeric value calculated from the expression in INLIN0
!!             (when IERR returns 0).
!!             When a string value is returned (IERR=2) then OUTVAL is the length of
!!             the output string.
!!     outlin0  OUTLIN0 contains a string representation of the number returned in
!!              OUTVAL up to 20 characters long when INLIN0 is a numeric expression. It
!!              contains a string up to (iclen_calc=512) characters long when INLIN0 is
!!              a string expression.
!!     ierr    IERR returns
!!
!!             o -1 if an error occurred
!!             o 0 if a numeric value is returned (value is in OUTVAL, string
!!               representation of the value is in OUTLIN2).
!!             o 1 if no value was returned but a message was displayed (If a 'dump'
!!               or 'funcs' command was passed to the calculator).
!!             o 2 if the expression evaluated to a string value instead of a
!!               numeric value (value is in OUTLIN0).
!!     ilen    ILEN returns the length of the input string minus trailing blanks.
!!
!!##DEPENDENCIES
!!       o jucalc
!!       o pdec
!!       o User-supplied routines:
!!
!!         All programs that call the calculator routine must supply their own
!!         JUOWN1 and C procedures. See the example program for samples.
!!           o juown1
!!           o c
!!##EXAMPLES
!!
!!    Sample program:
!!
!!     program TEST_JUCALCX
!!     !     NOTE: user must supply the JUOWN1 and C procedures.
!!     use m_calculator, only: iclen_calc
!!     character(len=iclen_calc) ::  outlin0
!!     doubleprecision :: outval
!!     call jucalcx('A=3.4**5    ',outval,outlin0,ierr,ilen)
!!     write(*,*)'value of expression is ',outval
!!     write(*,*)'string representation of value is ',outlin0
!!     write(*,*)'error flag value is ',ierr
!!     write(*,*)'length of expression is ',ilen
!!     end program TEST_JUCALCX
!!
!!##SEE ALSO
!!     See also: STRGAR(),RNUM0(),JUCALC(),INUM0(),SNUM0()
!!##REFERENCES
!!     NONE.
!===================================================================================================================================
!>
!! AUTHOR   John S. Urban
!!##VERSION  V1.0, 19971123
!===================================================================================================================================
subroutine jucalcx(inlin0,outval,outlin0,ierr,ilen)
use M_journal, only : journal
character(len=*),parameter :: ident="@(#)M_calculator_plus::jucalcx(3f):call jucalc() calculator and display messages"
!
! evaluate a FORTRAN-like string expression and return a numeric
! value and it's character equivalent or a string value as appropriate
character(len=*),intent(in) :: inlin0
doubleprecision             :: outval
character(len=*)            :: outlin0
integer,intent(out)         :: ierr
integer,intent(out)         :: ilen

character(len=iclen_calc)   :: line
character(len=iclen_calc)   :: outlin
doubleprecision,save        :: rvalue=0.0d0
intrinsic                   :: len
integer                     :: imaxi
character(len=iclen_calc)   :: event
!#----------------------------------------------------------------------------------------------------------------------------------
   ! copy INLIN0 to working copy LINE and find position of last non-blank character
   ! in the string
   line=''
   line=inlin0
   ! if the line is blank set imaxi to 1, else set it to the least of the length of the input string or (iclen_calc)
   ! NOTE: not checking if input expression is longer than (iclen_calc) characters!!
   imaxi=max(min(len(line),len(inlin0)),1)
   ilen=len_trim(line(1:imaxi))
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ilen.eq.0)then                                            ! command was totally blank
      ierr=-1
      call journal('sc','*jucalcx* warning===> blank expression')
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(line(:1).eq.'#')then                                  ! line was a comment
!-----------------------------------------------------------------------------------------------------------------------------------
   else
      ierr=0
      call jucalc(line(:ilen),outlin,event,rvalue,ierr)         ! evaluate the expression
!-----------------------------------------------------------------------------------------------------------------------------------
      select case(ierr)
      case(-1)                                    ! trapped error, display error message
        call journal('sc','*jucalcx* error===>'//trim(event))
        !call pdec(line(:ilen))                   ! echo input string as is and in ASCII decimal
      case(1)                                     ! general message, display message
        call journal('sc','*jucalcx* message===>'//trim(event))
      case(0)                                     ! numeric output
         outlin0=outlin
      case(2)                                     ! string output
         outlin0=event                            ! assumes outlin is long enough to return the string into
         ilen=int(rvalue)                         ! in special mode where a string is returned, rvalue is the length of the string
      case default
        call journal('sc','*jucalcx* warning===> unexpected ierr value=',ierr)
      end select
!-----------------------------------------------------------------------------------------------------------------------------------
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   outval=rvalue                            ! return normal sized real value
end subroutine jucalcx
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!       strgarr(3f) - [M_calculator_plus]read a string into an array using jucalc(3f) calculator
!!##SYNOPSIS
!!
!!   subroutine strgarr(line,ivals,vals,ifound,delims,ierr)
!!
!!     character(len=*), intent=(in) :: line
!!     integer, intent=(in)          :: ivals
!!     real, intent=(out)            :: vals(ivals)
!!     integer, intent=(out)         :: ifound
!!     character(len=*), intent=(in) :: delims
!!     integer, intent=(out)         :: ierr
!!
!!##DESCRIPTION
!!     strgarr() returns an array of real values from a string containing numeric
!!     expressions. Use strgar2() if you are going to permit string expressions
!!     with " delimiters.
!!
!!       o strgarr() parses the string at the specified delimiters and calls the
!!         calculator routine JUCALCX(3f) to evaluate the expressions.
!!       o It counts the number of values found.
!!       o Once the maximum allowable number of values have been found strgarr()
!!         returns, ignoring the rest of the line.
!!       o If an error occurs the error flag returns the column number where the
!!         expression that failed begins.
!!
!!     line          LINE is a string of numeric expressions. Each expression can be up to
!!                  (iclen_calc=255) characters long. The syntax of an expression is as
!!                  described in the main document of the Calculator Library. Assuming the
!!                  delimiters include a space character an example would be:
!!
!!                   'A=10 100 300E2/42.6  sin(3.1416/5)'
!!
!!                  Only numeric expressions are expected; so no use of the delimiter
!!                  characters is allowed except as a delimiter, even in quoted strings.
!!     ivals        IVALS is the maximum number of values to return.
!!     vals(ivals)  VALS is an array filled with the numeric values calculated from the
!!                  expressions in LINE.
!!     ifound       IFOUND is the number of values successfully returned in VALS
!!     delims       DELIMS is a character to use as an expression delimiter. It is commonly
!!                  set to a space and semi-colon(' ;').
!!     ierr         IERR returns 0 if no error occurred. If an error did occur, it returns
!!                  the column number the expression started at that could not be
!!                  evaluated.
!!
!!##DEPENDENCIES
!!       o jucalcx
!!       o User-supplied routines:
!!         All programs that call the calculator routine must supply their own
!!         JUOWN1 and C procedures. See the example program for samples.
!!           o juown1
!!           o c
!!##EXAMPLES
!!
!!    Sample program:
!!
!!       program T_strgarr
!!       use M_kracken, only: sget, kracken, lget
!!       use M_calculator_plus, only : strgarr
!!       real vals(41)
!!       character(len=80) :: line=' '
!!       character(len=10) :: delims=' ;'
!!       !  define command arguments, default values and crack command line
!!       call kracken('cmd','-d " ;" -test .false. -help .false. -begin -end')
!!       !----------------------------------------------------------
!!       write(*,*)'SGET',trim(sget('cmd_test'))
!!       write(*,*)'LGET',lget('cmd_test')
!!       if(lget('cmd_test'))then   ! cursory test
!!          call strgarr("10;2/3;sin(4.314)",41,vals,ifound,' ;',ierr)
!!          write(*,*)'values are',(vals(i),i=1,ifound)
!!          sumtarget= 9.74497986
!!          tol=       0.00000001
!!          sumup=sum(vals(:ifound))
!!          ipass=0
!!          if(ifound.ne.3) ipass=ipass+1
!!          if(ierr.ne.0)   ipass=ipass+2
!!          if( sumup >= (sumtarget-tol) .and. sumup <= (sumtarget+tol) ) then
!!          else
!!             ipass=ipass+4
!!          endif
!!          if(ipass.eq.0)then
!!             write(*,*)'sum is ',sumup
!!             write(*,*)'number of values is',ifound
!!             write(*,*)'error flag is',ierr
!!             write(*,*)'STRGARR*: PASSED'
!!             stop(0)
!!          else
!!             write(*,*)'IFOUND:',ifound
!!             write(*,*)'IERR  :',ierr
!!             write(*,*)'SUM   :',sumup
!!             write(*,*)'STRGARR*: FAILED',ipass
!!             stop(-1)
!!          endif
!!       endif
!!       !----------------------------------------------------------
!!       delims=sget('cmd_d')
!!       write(*,*)'DELIMS=[',trim(delims),']'
!!       !----------------------------------------------------------
!!       line=sget('cmd_begin')
!!       write(*,*)'BEGIN:',trim(line)
!!       if(line.ne.' ')then
!!          call strgarr(line,41,vals,ifound,delims,ierr)
!!       endif
!!       !----------------------------------------------------------
!!       line=sget('cmd_oo')
!!       write(*,*)'LINE:',trim(line)
!!       if(line.ne.' ')then
!!          call strgarr(line,41,vals,ifound,delims,ierr)
!!          write(*,*)(VALS(I),I=1,IFOUND)
!!       else
!!          INFINITE: do
!!             read(*,'(a)',iostat=ios)line
!!             if(ios.ne.0)then
!!                exit INFINITE
!!             endif
!!             call strgarr(line,41,vals,ifound,delims,ierr)
!!             write(*,*)IERR,IFOUND,':',(VALS(I),I=1,IFOUND)
!!          enddo INFINITE
!!       endif
!!       !----------------------------------------------------------
!!       line=sget('cmd_end')
!!       write(*,*)'END',trim(line)
!!       if(line.ne.' ')then
!!          call strgarr(line,41,vals,ifound,delims,ierr)
!!          write(*,*)'END:',(VALS(I),I=1,IFOUND)
!!       endif
!!       !----------------------------------------------------------
!!       end
!!
!!       ! NOTE: user must supply the JUOWN1 and C procedures.
!!
!!##SEE ALSO
!!    To parse a list of numbers instead of expressions see STRGAR().
!!    If there is only one expression see RNUM0(), JUCALCX(), JUCALC().
!!
!!##REFERENCES
!!    none.
!===================================================================================================================================
!>
!! AUTHOR  John S. Urban
!!##VERSION 1.0, 19971123
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE strgarr(line,ivals,vals,ifound,delims0,ierr)
USE M_JOURNAL, ONLY : journal
CHARACTER(LEN=*),PARAMETER :: ident="@(#)M_calculator_plus::strgarr(3f):read numeric expressions into an real array"
!-----------------------------------------------------------------------------------------------------------------------------------
!     1989 John S. Urban
!      line=input string
!      ivals=maximum number of values to try to read into vals
!      vals=real array to be filled with values
!      ifound=number of values read (before error occurs if one does)
!      ierr==0 if no error, column number error string starts at
!
!     given a line of structure string , string , string evaluate each
!     string and store into an array. delims0 contains the legal
!     delimiters. no checking for more than can fit in vals.
!     quits if encounters any errors in read.
!-----------------------------------------------------------------------------------------------------------------------------------
      CHARACTER(LEN=*),INTENT(IN)  :: line
      INTEGER,INTENT(IN)           :: ivals
      INTEGER,INTENT(OUT)          :: ifound
      CHARACTER(LEN=*),INTENT(IN)  :: delims0
      INTEGER,INTENT(OUT)          :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
      REAL                         :: vals(ivals)
      CHARACTER(len=iclen_calc)    :: outlin
      INTEGER                      :: ilen,id
      INTEGER                      :: i10,i20,i40
      INTEGER                      :: icol,istart,iend
      DOUBLEPRECISION              :: dval
      INTEGER                      :: ier
      INTEGER                      :: ilendm
      CHARACTER(LEN=256)           :: delims
!-----------------------------------------------------------------------------------------------------------------------------------
      id=LEN(delims0)
      IF(id.EQ.0)THEN
         delims=' '
         id=1
      ELSE
         delims=delims0
      ENDIF
!-----------------------------------------------------------------------------------------------------------------------------------
      ierr=0
      iend=0
      ifound=0
      ilen=0
      DO i20=LEn(line),1,-1
         IF(INDEX(delims(:id),line(i20:i20)).EQ.0)THEN      ! see if current character is a delimiter
            ilen=i20                                        ! record position of last non-delimiter
            EXIT                                            ! found non-delimiter
         ENDIF
      ENDDO
      IF(ilen.EQ.0)THEN                                     ! command was totally composed of delimiters
         CALL journal('sc','*strgarr* blank line passed as a list of numbers')
         RETURN
      ENDIF
!-----------------------------------------------------------------------------------------------------------------------------------
!     there is at least one non-delimiter in the string
!     ilen is the column position of the last non-delimiter character
!     find next non-delimiter
      icol=1                                                ! the pointer into the line being processed
      DO i10=1,ivals,1                                      ! only find enough values to store into vals(1:ivals)
         INFINITE: DO                                       ! FIND NEXT SUBSTRING AND STORE IT
            IF(INDEX(delims(:id),line(icol:icol)).EQ.0)THEN ! character is not a delimiter so starts substring
               istart=icol                                  ! set start of substring
               iend=0                                       ! FIND END OF SUBSTRING
               DO i40=istart,ilen                           ! look at each character starting at left
                 IF(INDEX(delims(:id),line(i40:i40)).NE.0)THEN   ! determine if character is a delimiter
                    iend=i40                                ! found a delimiter. record where it was found
                    EXIT                                    ! found end of substring so leave loop
                 ENDIF
               ENDDO
               IF(iend.EQ.0)iend=ilen+1                     ! no delimiters found, so this substring goes to end of line
               CALL jucalcx(line(istart:iend-1),dval,outlin,ier,ilendm)    ! parse substring minus delimiter
               IF(ier.EQ.0)THEN
                    vals(i10)=real(dval)
                    ifound=ifound+1
               ELSE                                         ! could have option to keep going or ignore some columns
                    ierr=istart
                    RETURN
               ENDIF
               icol=iend+1
               EXIT INFINITE                                ! go look for next substring
            ELSE
               icol=icol+1                                  ! skip delimiters while looking for start of string
            ENDIF
            IF(icol.GT.ilen) THEN                           ! last string
              RETURN
            ENDIF
         ENDDO INFINITE
      ENDDO
      ierr=iend+1                                           ! error: more than ivals numbers were in the line.
END SUBROUTINE strgarr
!>
!!##NAME
!!       strgar2(3f) - [M_calculator_plus]read a string into a real array USING CALCULATOR, allowing quoted strings in arguments,
!!
!!##SYNOPSIS
!!
!!   subroutine strgar2(line,ivals,vals,ifound,delims,ierr)
!!
!!     character(len=*), intent=(in) :: line
!!     integer, intent=(in) :: ivals
!!     real, intent=(out) :: vals(ivals)
!!     integer, intent=(out) :: ifound
!!     character(len=*), intent=(in) :: delims
!!     integer, intent=(out) :: ierr
!!
!!##DESCRIPTION
!!     STRGAR2() returns an array of real values from a string containing numeric
!!     and string expressions.
!!       o STRGAR2() parses the string at the specified delimiters and calls the
!!         calculator routine jucalc.f to evaluate the expressions.
!!       o It counts the number of values found.
!!       o Once the maximum allowable number of values have been found STRGAR2()
!!         returns, ignoring the rest of the line.
!!       o If an error occurs the error flag returns the column number where the
!!         expression that failed begins.
!!       o If the string is '*', the value -99999.0 is returned.
!!
!!     line         LINE is a string of numeric expressions. Each expression can be up to
!!                  (iclen_calc=512) characters long. The syntax of an expression is as
!!                  described in the main document of the Calculator Library. (Assuming the
!!                  delimiters include a space character) an example would be:
!!
!!                       'A=10 100 300E2/42.6  sin(3.1416/5)'
!!
!!     ivals        IVALS is the maximum number of values to return.
!!     vals(ivals)  VALS is an array filled with the numeric values calculated from the
!!                  expressions in LINE.
!!     ifound       IFOUND is the number of values successfully returned in VALS
!!     delims       DELIMS is a character(s) to use as an expression delimiter. It is
!!                  commonly set to a space (' '). If more than one character is specified,
!!                  the space must not be last.
!!     ierr         IERR returns 0 if no error occurred. If an error did occur, it returns
!!                  the column number the expression started at that could not be
!!                  evaluated.
!!
!!##DEPENDENCIES
!!       o jucalcx
!!       o User-supplied routines:
!!         All programs that call the calculator routine must supply their own
!!         JUOWN1 and C procedures. See the example program for samples.
!!           o juown1
!!           o c
!!##EXAMPLES
!!
!!
!!  Sample program:
!!
!!     program T_STRGAR2
!!     use M_calculator_plus, only : strgar2
!!     integer             :: ios
!!     integer             :: i
!!     integer             :: ifound
!!     integer             :: ierr
!!     real                :: vals(1000)
!!     character(len=4096) :: line
!!     ! NOTE: user must supply the JUOWN1 and C procedures.
!!
!!     write(*,'(80("-"))')
!!     call strgar2('10;2/3;sin(4.314)',4,vals,ifound,' ;',ierr)
!!     write(*,*)'should find three values in 10;2/3;sin(4.314)'
!!     write(*,*)'ifound=',ifound
!!     write(*,*)'values are',(vals(i),i=1,ifound)
!!
!!     write(*,'(80("-"))')
!!     write(*,*)'should find three values in 10;2/3;sin(4.314)'
!!     write(*,*)'ifound=',ifound
!!     call strgar2('10;2/3;sin(4.314) ',3,vals,ifound,' ;',ierr)
!!     write(*,*)'ifound=',ifound
!!     write(*,*)'values are',(vals(i),i=1,ifound)
!!
!!     write(*,'(80("-"))')
!!     write(*,*)'should stop at two values in 10;2/3;sin(4.314)'
!!     call strgar2('10;2/3;sin(4.314)',2,vals,ifound,' ;',ierr)
!!     write(*,*)'ifound=',ifound
!!     write(*,*)'values are',(vals(i),i=1,ifound)
!!
!!     write(*,'(80("-"))')
!!     write(*,*)'should stop at one values in 10;2/3;sin(4.314)'
!!     call strgar2('10;2/3;sin(4.314)',1,vals,ifound,' ;',ierr)
!!     write(*,*)'ifound=',ifound
!!     write(*,*)'values are',(vals(i),i=1,ifound)
!!
!!     write(*,'(80("-"))')
!!     write(*,*)'should find three values in 10;2/3;sin(4.314) ; ; ;   ;; '
!!     call strgar2('10;2/3;sin(4.314) ; ; ;   ;; ',1000,vals,ifound,' ;',ierr)
!!     write(*,*)'ifound=',ifound
!!     write(*,*)'values are',(vals(i),i=1,ifound)
!!
!!     write(*,'(80("-"))')
!!     write(*,*)'should find an error in  values in 10;20/3O;sin(4.314) ; ; ;   ;; '
!!     call strgar2('10;20/3O;sin(4.314) ; ; ;   ;; ',1000,vals,ifound,' ;',ierr)
!!     write(*,*)'ifound=',ifound,' error=',ierr
!!     write(*,*)'values are',(vals(i),i=1,ifound)
!!
!!     write(*,'(80("-"))')
!!     write(*,*)'Enter strings delimited by spaces or semicolons'
!!        do
!!           read(*,'(a)',iostat=ios)line
!!           if(ios.ne.0)then
!!              stop
!!           endif
!!           call strgar2(line,1000,vals,ifound,' ;',ierr)
!!           write(*,*)'ifound=',ifound
!!           write(*,*)'values are',(vals(i),i=1,ifound)
!!        enddo
!!     end program T_STRGAR2
!!
!!##SEE ALSO
!!
!!     To parse a list of numbers instead of expressions see STRGAR().
!!     If there is only one expression see RNUM0(), JUCALCX(), JUCALC().
!!
!!##REFERENCES
!!     NONE.
!===================================================================================================================================
!>
!! AUTHOR   John S. Urban
!!##VERSION  1.0 19971123
!===================================================================================================================================
subroutine strgar2(line,iread,numbrs,inums,delims0,ierr)
use M_journal, only : journal
use m_calculator, only: iclen_calc
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter :: ident=&
&"@(#)M_calculator_plus::strgar2(3f):read numeric and string calculator expressions into an array USING CALCULATOR"
!  1989 John S. Urban
!  given a line of structure 'string,string,string' process each string and store into an array.
!  no checking for more than can fit in numbrs.
!  quits if any errors are encountered in reading the input string.
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in)           :: line            ! line=input string
   integer,intent(in)                    :: iread           ! iread=maximum number of values to try to read into numbrs
   real,intent(out)                      :: numbrs(iread)   ! numbrs=real array to be filled with values
   integer,intent(out)                   :: inums           ! inums=number of values read (before error occurs if one does)
   character(len=*),intent(in)           :: delims0         ! delimiters at which to break input into expressions
   integer,intent(out)                   :: ierr            ! ierr==0 if no error, column number error string starts at
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=iclen_calc) :: outlin
   character(len=256)        :: delims                      ! maleable copy of delimiters at which to break input into expressions
   character(len=1)          :: ch
   integer                   :: iend
   integer                   :: ilen
   integer                   :: idels
   logical                   :: instring                    ! flag that not inside a quoted string
   doubleprecision           :: dval
   integer                   :: iprev
   integer                   :: itwasd                      ! previous character was a delimiter not in a quoted region or not
   integer                   :: istart
   integer                   :: istarto
   integer                   :: ierr_calc
   integer                   :: ilendm
   integer                   :: i10
   integer                   :: i20
!-----------------------------------------------------------------------------------------------------------------------------------
   delims=delims0                                           ! need a mutable copy of the delimiter list
   if(delims.eq.'')then                                     ! if delimiter list is null or all spaces make it a space
      delims=' '                                            ! delimiter is a single space
      idels=1                                               ! length of delimiter list
   else
      idels=len_trim(delims0)                               ! length of variable WITH TRAILING WHITESPACE TRIMMED
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
   ierr_calc=0
   inums=0
   if( index(delims(:idels),'"').ne.0 )then                 ! MAKING THE ASSUMPTION THAT " IS NOT AN ALLOWED FIELD DELIMITER
      call journal('sc','*strgar2* bad delimiter requested')       ! the double quote is reserved as a string delimiter
      goto 999
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=0
   FINDONE: do i20=len(line),1,-1                                   ! find last non-delimiter character by starting at right
      if(index(delims(:idels),line(i20:i20)).eq.0)then              ! current character is not a delimiter
         ilen=i20                                                   ! this is where the last non-delimiter is
         exit FINDONE
      endif
   enddo FINDONE
   if(ilen.eq.0)then                                                !  command was totally composed of delimiters
!     call journal('sc','*strgar2* blank line passed as a list of numbers')
      goto 999
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  there is at least one non-blank sub-string
!  ilen is the column position of the last non-blank character
   iprev=-1
   itwasd=0                                                 ! previous character was a delimiter not in a quoted region
   instring=.false.                                         ! flag that not inside a quoted string
   istart=1
   istarto=1
   iend=1
!-----------------------------------------------------------------------------------------------------------------------------------
!  find next non-delimiter
   NEXT: do i10=1,ilen+1
      if(i10.eq.ilen+1)then                    ! finishing string
         ch=delims(1:1)                        ! make the imaginary last character a delimiter
         if(instring)then                      ! if unmatched quote encountered put the unclaimed characters into one last parameter
            istart=istarto
            if(iend-istart.ge.0)then           ! process what is left over when an unmatched parenthesis is encountered
               if(line(istart:iend).eq.'*')then
                  dval=-99999.0d0
               else
                  call jucalcx(line(istart:iend),dval,outlin,ierr_calc,ilendm)
               endif
               if(ierr_calc.eq.0)then
                  inums=inums+1
                  if(inums.gt.iread)then
                     call journal('sc','*strgar2* max parameters allowed is ',iread)
                     goto 999
                  endif
                  numbrs(inums)=dval
               elseif(ierr_calc.eq.2)then
                  !call journal('sc','*strgar2* could not turn string into number')
                  !call journal('sc',line(istart:iend)
               else
                  call journal('sc','*strgar2* could not turn string into number['//line(istart:iend)//']')
                  call journal('sc','*strgar2* error is ',ierr_calc)
                  ierr=istart
                  goto 999                                        ! keep going to others or not?
               endif
            endif
            goto 999
         endif
      else                                                        ! regular string
         ch=line(i10:i10)
      endif
      if(ch.eq.'"'.and.(.not.instring))then                       ! starting quote
         istarto=istart
         if(iprev.ne.i10-1.and.itwasd.eq.0)then
            istart=i10                                            ! start new string
            iend=i10-1                                            ! in case this string is not ended, do not ignore what went before
         endif
         instring=.true.                                          ! flag that in a string
      elseif(ch.eq.'"')then                                       ! closing quote or internal quote
         instring=.false.
         iprev=i10
      elseif((.not.instring).and.(index(delims(:idels),ch).ne.0))then  ! delimiter not in quoted string, last char is always delims
         iend=i10-1
         if(iend-istart.ge.0)then
            if(line(istart:iend).eq.'*')then
               dval=-99999.0d0
            else
               call jucalcx(line(istart:iend),dval,outlin,ierr_calc,ilendm)
            endif
            if(ierr_calc.eq.0)then                                               ! returned number
              inums=inums+1
              if(inums.gt.iread)then
                 inums=inums-1
                 call journal('sc','*strgar2* too many values. max values allowed stored is ',iread)
                 goto 999
              endif
              numbrs(inums)=dval
            elseif(ierr_calc.eq.2)then
              !call journal('sc','*strgar2* could not turn string into number')
              !call journal('sc',line(istart:iend)
            else
              call journal('sc','*strgar2* could not turn string into number:'//line(istart:iend)//':')
              call journal('sc','*strgar2* error is ',ierr_calc)
              ierr=istart
              goto 999                                                     ! keep going to others or not?
            endif
         endif
         istart=i10+1                                                      ! start new string
         itwasd=0
      else
         iend=i10
         itwasd=1
      endif
   enddo NEXT
!-----------------------------------------------------------------------------------------------------------------------------------
999   continue
end subroutine strgar2
!-----------------------------------------------------------------------------------------------------------------------------------
end module M_calculator_plus
