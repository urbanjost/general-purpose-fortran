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
'       compute(1f) - [MATH] evaluate a calculator expression                    ',&
'       (LICENSE:PD)                                                             ',&
'SYNOPSIS                                                                        ',&
'       compute [STRING] [ -verbose]| [ -help| -version]                         ',&
'DESCRIPTION                                                                     ',&
'       Given any expression call the CALCULATOR(3f) calculator function and     ',&
'       evaluate it. If no expression is present on the command line, read       ',&
'       expressions from stdin until a line composed of a period(".") or         ',&
'       end of data is encountered.                                              ',&
'                                                                                ',&
'       Expressions are similar to Fortran77 syntax except powers are            ',&
'       processed from left to right, and string variable names start            ',&
'       with a dollar-sign, and all numeric values are assumed to be             ',&
'       DOUBLEPRECISION.                                                         ',&
'OPTIONS                                                                         ',&
'       STRING            calculator expression to evaluate                      ',&
'       --verbose         echo the input as well as the computed values          ',&
'       --help            display this help and exit                             ',&
'       --version         output version information and exit                    ',&
'       --trail FILENAME  record actions on a trail file.                        ',&
'EXAMPLES                                                                        ',&
'       Sample commands:                                                         ',&
'                                                                                ',&
'        $ compute ''(sin(30.33333)*2)**2+40.0/2.3-1.23e3''                      ',&
'        $ compute funcs|more                                                    ',&
'                                                                                ',&
'        $ compute --trail record.txt                                            ',&
'        a=10                                                                    ',&
'        # The redo(3f) command is used for command recall and history           ',&
'        # by entering an exclamation on a line by itself or by following        ',&
'        # the exclamation by a space and an optional initial command for        ',&
'        # redo(3f). Enter "?" at the redo(3f) prompt for help using the         ',&
'        # command line history editor.                                          ',&
'                                                                                ',&
'        # enter a long calculation                                              ',&
'        b=sin(a)**2+3.4e2+100-11*2/55.6                                         ',&
'        # use redo(3f) to change "sin" to "cos". Enter return to execute        ',&
'        # the changed line                                                      ',&
'        ! c/sin/cos                                                             ',&
'                                                                                ',&
'        # system commands can be called if prefixed by exclamation              ',&
'        !ls                                                                     ',&
'                                                                                ',&
'        # exit using a period on a line by itself                               ',&
'        .                                                                       ',&
'                                                                                ',&
'        # a simple non-nesting alternate input file can be read                 ',&
'        <myfile                                                                 ',&
'                                                                                ',&
'        funcs # list available functions                                        ',&
'        dump  # list current variables                                          ',&
'                                                                                ',&
'SEE  ALSO                                                                       ',&
'       See the man(1) page for M_calculator(3fm) for a more detailed            ',&
'       description of the CALCULATOR(3f) routine.                               ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public License                                                               ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!        compute(1f) - [MATH] evaluate a calculator expression
!!        (LICENSE:PD)
!!##SYNOPSIS
!!
!!        compute [STRING] [ -verbose]| [ -help| -version]
!!##DESCRIPTION
!!        Given any expression call the CALCULATOR(3f) calculator function and
!!        evaluate it. If no expression is present on the command line, read
!!        expressions from stdin until a line composed of a period(".") or
!!        end of data is encountered.
!!
!!        Expressions are similar to Fortran77 syntax except powers are
!!        processed from left to right, and string variable names start
!!        with a dollar-sign, and all numeric values are assumed to be
!!        DOUBLEPRECISION.
!!##OPTIONS
!!        STRING            calculator expression to evaluate
!!        --verbose         echo the input as well as the computed values
!!        --help            display this help and exit
!!        --version         output version information and exit
!!        --trail FILENAME  record actions on a trail file.
!!##EXAMPLES
!!
!!        Sample commands:
!!
!!         $ compute '(sin(30.33333)*2)**2+40.0/2.3-1.23e3'
!!         $ compute funcs|more
!!
!!         $ compute --trail record.txt
!!         a=10
!!         # The redo(3f) command is used for command recall and history
!!         # by entering an exclamation on a line by itself or by following
!!         # the exclamation by a space and an optional initial command for
!!         # redo(3f). Enter "?" at the redo(3f) prompt for help using the
!!         # command line history editor.
!!
!!         # enter a long calculation
!!         b=sin(a)**2+3.4e2+100-11*2/55.6
!!         # use redo(3f) to change "sin" to "cos". Enter return to execute
!!         # the changed line
!!         ! c/sin/cos
!!
!!         # system commands can be called if prefixed by exclamation
!!         !ls
!!
!!         # exit using a period on a line by itself
!!         .
!!
!!         # a simple non-nesting alternate input file can be read
!!         <myfile
!!
!!         funcs # list available functions
!!         dump  # list current variables
!!
!!##SEE  ALSO
!!        See the man(1) page for M_calculator(3fm) for a more detailed
!!        description of the CALCULATOR(3f) routine.
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public License
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
'@(#)PROGRAM:        compute(1f)>',&
'@(#)DESCRIPTION:    line mode calculator program (that calls CALCULATOR(3f))>',&
'@(#)VERSION:        23.1 20160618>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2024-06-29 21:49:06 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
program compute
use M_kracken,    only : kracken, sget, lget
use M_calculator, only : calculator,iclen_calc
use M_history,    only : redo
use M_io,         only : read_line
use M_journal,    only : journal
!use M_noown, only     : juown1, c
!!implicit real(kind=selected_real_kind(15,300)) (a-h, o-z)
implicit none
integer,parameter            :: dp=selected_real_kind(15,300)
character(len=iclen_calc)    :: line
character(len=iclen_calc)    :: outlin
character(len=iclen_calc)    :: event
character(len=:),allocatable :: readin
character(len=:),allocatable :: trailfile
character(len=256)           :: csys
character(len=256)           :: mssge
integer                      :: isys, esys
real(kind=dp)                :: rvalue
integer                      :: ierr=0
logical                      :: verbose
integer                      :: in=5
integer                      :: ios
   call calculator('ownmode(1)',outlin,event,rvalue,ierr)     ! specify user-supplied routine contains extra routines for calculator
   call kracken('compute',' -oo -help .f. -version .f. -verbose .f. -trail') ! define and crack command line arguments
   call help_usage(lget('compute_help'))                      ! process -help switch
   call help_version(lget('compute_version'))                 ! process -version switch

   verbose=lget('compute_verbose')                     ! test if -verbose switch is present on command line
   trailfile=sget('compute_trail')
   if(trailfile.ne.' ')then
      call journal('O',trailfile)
   endif
   line=sget('compute_oo')                             ! get any expressions from command line to evaluate

   if(line.ne.'')then                                  ! if expressions on command line evaluate them
      call processit()
   else
      READ: do
         INFINITE: do while (read_line(readin,in)==0)  ! no expressions on command line to read expressions from stdin
            readin=adjustl(readin)//'  '               ! make at least two characters long and left-adjusted to avoid overindexing
            if(readin.eq.'.')then                      ! exit command is "."
               if(in.eq.5)then
                  exit INFINITE                        ! quit if user enters command "." on stdin
               else
                  close(in,iostat=ios)
                  in=5
                  cycle INFINITE
               endif
            elseif(readin(1:1).eq.'<')then             ! read from alternate input file assuming have "<filename"
               open(newunit=in,iostat=ios,file=readin(2:),iomsg=mssge)
               if(ios.ne.0)then                        ! file open failed, revert back to stdin
                  close(in,iostat=ios)
                  in=5
                  call journal('sc','error:',mssge)
               endif
               cycle INFINITE
            endif
            line=readin
            call redo(line,r='!')                         ! command history
            call journal('t',line)
            if(line(1:1).eq.'!')then                      ! system command
               call execute_command_line(line(2:),exitstat=esys,cmdstat=isys,cmdmsg=csys)   ! try as system command
            elseif(adjustl(line(1:1)).eq.'#')then         ! comment
            else                                          ! expression
               call processit()
            endif
         enddo INFINITE
         if(in.eq.5)exit READ
         close(in,iostat=ios)
         call journal('sc','exiting altername input file')
         in=5
      enddo READ
   endif
contains
   subroutine processit
      call calculator(line,outlin,event,rvalue,ierr)
      select case(ierr)                                                             ! several different meanings to the error flag
       case(-1);     call journal('sc','error===>',event)                           ! an error has occurred
       case(0)
         if(verbose.or.(in.ne.5))then
            call journal('sc',line,"=>",outlin)                                     ! a numeric value was returned without error
         else
            call journal('sc',outlin)                                               ! a numeric value was returned without error
         endif
       case(1);      call journal('sc','message===>',event)                         ! request for a message (from DUMP or FUNC)
       case(2);      call journal('sc',event(:int(rvalue)))                         ! a string value was returned without error
       case default; call journal('sc','*compute* unexpected ierr value ',ierr)     ! this should not occur
      end select
   end subroutine processit
end program compute
