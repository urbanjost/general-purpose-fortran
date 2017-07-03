!>
!!##NAME
!!   jucalc - [M_calculator]parse calculator expression and return numeric or string value
!!##SYNOPSIS
!!
!!   subroutine jucalc(inline,outlin,mssg,slast,ierr)
!!
!!    character(len=*),intent=(in)           :: inline
!!    character(len=iclen_calc),intent=(out) :: outlin
!!    character(len=iclen_calc),intent=(out) :: mssg
!!    doubleprecision, intent=(out)          :: slast
!!    integer, intent=(out)                  :: ierr
!!
!!##DESCRIPTION
!!    JUCALC() evaluates FORTRAN-like expressions. It can be used to  add
!!    calculator-like abilities to your program.
!!
!!     inline  INLINE is a string expression up to (iclen_calc=512) characters long.
!!             The syntax of an expression is described in
!!             the main document of  the  Calculator  Library.
!!     outlin  Returned numeric value as a string when IERR=0.
!!     mssg    MSSG is a string that can serve several purposes
!!             o Returned string value when IERR=2
!!             o Error message string when IERR=-1
!!             o Message from 'funcs' or 'dump' command when IERR=1
!!     slast   SLAST has different meanings depending on whether a string or number
!!             is being returned
!!             o REAL value set to last successfully calculated value when IERR=0
!!             o Number of characters in returned string variable when IERR=2
!!     ierr    status flag.
!!             o  -1 ==> An error occurred
!!             o  0 ==> A numeric value was returned
!!             o  1 ==> A message was returned
!!             o  2 ==> A string value was returned
!!##DEPENDENCIES
!!        o ceiling
!!        o floor
!!        o change
!!        o modif
!!        o rand
!!        o len_trim
!!        o  User-supplied routines: juown1, c
!!##EXAMPLES
!!
!!   Example calculator program
!!
!!       program compute
!!       !@(#)compute(1f): line mode calculator program (that calls jucalc(3f))
!!       !     requires:
!!       !     c()
!!       use m_calculator, only: jucalc,iclen_calc
!!       ! iclen_calc : max length of expression or variable value as a string
!!       implicit none
!!       integer,parameter         :: dp=kind(0.0d0)
!!       character(len=iclen_calc) :: line
!!       character(len=iclen_calc) :: outlin
!!       character(len=iclen_calc) :: event
!!       real(kind=dp)             :: rvalue
!!       integer                   :: ierr
!!       ierr=0
!!       call jucalc('ownmode(1)',outlin,event,rvalue,ierr)
!!       ! activate user-defined function interface
!!       INFINITE: do
!!          read(*,'(a)',end=999)line
!!          if(line.eq.'.')stop
!!          call jucalc(line,outlin,event,rvalue,ierr)
!!          select case (ierr)
!!          ! several different meanings to the error flag returned by calculator
!!          case(0)
!!          ! a numeric value was returned without error
!!            write(*,'(a,a,a)')trim(outlin),' = ',trim(line)
!!          case(2)
!!          ! a string value was returned without error
!!            write(*,'(a)')trim(event(:int(rvalue)))
!!          case(1)
!!          ! a request for a message has been returned (from DUMP or FUNC)
!!            write(*,'(a,a)')'message===>',trim(event(:len_trim(event)))
!!          case(-1)
!!          ! an error has occurred
!!            write(*,'(a,a)')'error===>',trim(event(:len_trim(event)))
!!          case default
!!          ! this should not occur
!!            WRITE(6,'(A,i10)')'*JUCALC* UNEXPECTED IERR VALUE ',IERR
!!          end select
!!       enddo INFINITE
!!       999 continue
!!       end program compute
!!
!!##SEE ALSO
!!     see INUM0(),RNUM0(),SNUM0(),STRGAR2(),JUCALCX().
!!##REFERENCES
!!    NONE.
!===================================================================================================================================
!>
!! AUTHOR   John S. Urbn
!!##VERSION  1.0 19971123,20161218
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
module m_calculator

   use M_journal, only : journal
   use M_IO, only : print_inquire
   use M_strings, only : upper, lower
   implicit doubleprecision (a-h,o-z)
   integer,parameter                      :: dp=kind(0.0d0)
   private

   integer,parameter                      :: ic_calc=25000               ! number of variable names allowed

   integer,parameter,public               :: iclen_calc=512              ! max length of expression or variable value as a string
   integer,parameter,public               :: ixy_calc=55555              ! number of variables in X() and Y() array
   integer,parameter,public               :: icname_calc=20              ! max length of a variable name
   real(kind=dp),save,public              :: x(ixy_calc)=0.0_dp          ! x array for procedure jufuns
   real(kind=dp),save,public              :: y(ixy_calc)=0.0_dp          ! y array for procedure jufuns
   integer,save,public                    :: valuer(ic_calc)=0           ! lengths of the string variable values
   character(len=iclen_calc),save,public  :: values(ic_calc)=' '         ! string variable values

   public :: jucalc
   public :: getvalue
   public :: igetvalue
   public :: rgetvalue
   public :: stuff
   public :: stuffa

   integer,parameter                      :: ixyc_calc=50                ! number of variables in $X() and $(Y) array
   integer,parameter                      :: icbuf_calc=20*(iclen_calc/2+1) ! buffer for string as it is expanded


!  no check on whether line expansion ever causes line length to
!  exceed allowable number of characters.
!  number of characters to prevent over-expansion would currently be
!  20 digits per number max*(input number of characters/2+1).
!  input=80 --> 820 character buffer
!  input=256 ==> 2580

   character(len=iclen_calc) :: mssge   ! for error message/messages /returning string value

   character(len=iclen_calc),save  :: xc(ixyc_calc)=' '        ! $x array for procedure jufuns
   character(len=iclen_calc),save  :: yc(ixyc_calc)=' '        ! $y array for procedure jufuns
   character(len=iclen_calc),save  :: nc(ixyc_calc)=' '        ! $n array for procedure jufuns

   character(len=icname_calc),save :: ix2(ic_calc)=' '         ! contains the names of string variables

   character(len=icname_calc),save :: ix(ic_calc)=' '          ! contains the names of numeric variables
   real(kind=dp),save              :: value(ic_calc)=0.0_dp    ! numeric variable values

   character(len=iclen_calc),save  :: last='0.0'               ! string containing last answer (i.e. current value)
   logical,save                    :: ownon=.false.            ! flag for whether to look for juown1

   integer,save                    :: ktoken                   ! count of number of token strings assembled
!
! requires
!  change, rand, date_to_unix, unix_to_date, fmtdate, fmtdate_usage
!
   private :: juator                       ! returns a real value rval from a numeric character string chars.
   private :: given_name_get_stringvalue
   private :: jurtoa

   private :: jupars
   private :: jufuns
   private :: stufftok
   private :: juargs
   private :: jucals
   private :: jupows
   private :: jufacs
   private :: jusqes
   private :: jubous
   private :: juaddr
   private :: juadds

!>
!!##NAME
!!    stuff(3f) - [M_calculator]directly store value into calculator directory for efficiency
!!
!!##SYNOPSIS
!!
!!   subroutine stuff(varnam0,value,ioflag)
!!
!!    character(len=*),intent(in)             :: varnam0
!!    integer|real|doubleprecision,intent(in) :: value
!!    character(len=*),intent(in),optional    :: ioflag
!!
!!##DEFINITION
!!
!!    Normally values are stored or defined in the calculator module
!!    M_calculator(3fm) using the jucalc(3f) routine or the convenience
!!    routines in the module M_calculator_plus(3fm).  For efficiency when
!!    large numbers of values require being stored the stuff(3f) procedure
!!    can be used to store numeric values by name in the calculator
!!    dictionary.
!!
!!    breaking the rule of only accessing the calculator thru jucalc:
!!
!!    stuff(3f) is assumed to only be used when needed for efficiency and to
!!    avoid problems with recursion if a routine called by the calculator
!!    in JUOWN1(3f) wants to store something back into the calculator
!!    variable table.
!!
!!##OPTIONS
!!    varnam0   name of calculator variable to define or replace
!!    value     numeric value to associate with the name varnam0. May be integer, real, or doubleprecison.
!!    ioflag    optional flag to use journal logging. This string is passed directly to M_journal::journal(3f)
!!              as the first parameter. The default is to not log the definitions to the journal(3f) command.
!!
!!##EXAMPLE
!!
!===================================================================================================================================
   INTERFACE STUFF
   module procedure integer_stuff,real_stuff,double_stuff
   END INTERFACE STUFF

contains
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine jucalc(inline,outlin,mssg,slast,ierr)
!
!     The goal is to create a procedure easily utilized from other
!     programs that takes a standard Fortran value statement and reduces
!     it down to a value, efficiently and using standard Fortran
!     standards where ever feasible.
!
!  Version 2.0: 03/13/87
!  Version 3.0: 07/11/2013
!  Version 5.0: 07/16/2013
!
!  o  adjacent powers are done left to right, not right to left
!  o  code does not prevent - and + beside an other operator.
!  o  no check on whether user input more characters than allowed.
!     no check on whether line expansion ever causes line length to
!     exceed allowable number of characters.
!     number of characters to prevent over-expansion would currently be
!     20 digits per number max*(input number of characters/2+1).
!  o  allowing for ixy_calc arguments in max and min seems too high. if reducing
!     array size helps significantly in costs, do so.
!  o  parentheses are required on a function call.
!  o  square brackets [] are equivalent to parenthesis ().
!===========================================================================--------------------------------------------------------
!  2. need a generic help function to list commands and functions
!  3. allow multiple expressions per line with a semi-colon between them
!     (like the parse functions).
!  4. make a function to fill x and y arrays, or to read values into them
!     from a file; and make some statistical functions that work on the
!     arrays.
!  6. allow user-written functions to be called from jufuns routine.
!  7. allow for user-defined arrays and array operations.
!===========================================================================--------------------------------------------------------
!  12/07/87 --- put in an implicit real (a-h,o-z) statement in each
!              procedure so that it could quickly be changed to
!              implicit real*8 (a-h,o-z) for a vax. be careful of
!              type mismatch between external functions and the
!              real variables.
!              use following xedit commands where periods denote
!              spaces
!              c/implicit real../implicit real*8./ *
! 12/11/87  --- changed ifix calls to int calls as ifix on vax does
!              not allow real*8 in ifix calls
! 12/11/87  --- moving all prints out of column 1 so it is not picked
!              out by vax as carriage control.
! 12/28/87  --- put bn format specifier into juator routine because
!              vax assumes zero fill
! 06/23/88  --- making a first cut at allowing string variables.
!               1. string variable names must start with a dollar-sign
!               2. strings can only be up to (iclen_calc) characters long
!               3. they will be returned in the message string to
!                  the calling program
!               4. input strings must be delimited with double quotes.
!                  to place a double quote into the string, put two
!                  double quotes adjacent to each other.
!               5. a flag value for ier to distinguish between string
!                  and numeric output?
!#----------------------------------------------------------------------------------------------------------------------------------
!subroutine jucalc(inline,outlin,mssg,slast,ierr)
implicit doubleprecision (a-h,o-z)
character(len=*),parameter :: ident=&
&"@(#)M_calculator::jucalc(3f): The procedure JUCALC acts like a calculator"
integer,parameter                      :: dp=kind(0.0d0)
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)            :: inline
character(len=iclen_calc),intent(out)  :: outlin
character(len=iclen_calc),intent(out)  :: mssg
doubleprecision,intent(out)            :: slast
integer,intent(out)                    :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=icbuf_calc)              :: line
character(len=iclen_calc)              :: varnam
character(len=iclen_calc)              :: junout
real(kind=dp),save                     :: rlast=0.0_dp
!-----------------------------------------------------------------------------------------------------------------------------------
      line=inline                                      ! set working string to initial input line
      imax=len(inline)                                 ! determine the length of the input line
      mssg=' '                                         ! set returned message/error/string value string to a blank
      outlin=' '
      BIG: do                                          ! for $A=numeric and A=string
         ierr=1                                           ! set status flag to message mode
         mssge=' '                                        ! set message/error/string value in GLOBAL to a blank
         call jusqes(line,imax,nchard,varnam,nchar2,ierr) ! preprocess the string: remove blanks and process special characters
                                                          ! also remove all quoted strings and replace them with a token
   !--------------------------------------------------------------------------------------------------------------------------------
         if(ierr.eq.-1)then                ! if an error occurred during preprocessing of the string, set returned message and quit
            slast=rlast                    ! set returned real value to last good calculated value
            mssg=mssge                     ! place internal message from GLOBAL into message returned to user
            return
         elseif(nchard.eq.0)then  ! if a blank input string was entered report it as an error and quit
            ierr=-1
            mssg='*jucalc* input line was empty'
         elseif(line(1:nchard).eq.'dump')then ! process dump command
            call journal(line(1:nchard))
            call journal('current value= '//last)
            call journal(' variable name       variable value     ')
            do i10=1,ic_calc
               if(ix(i10).ne.' ')then
                  write(junout,'('' '',2a,g20.13e3)')ix(i10),' ',value(i10)
                  call journal(trim(junout))
               endif
            enddo
            do i20=1,ic_calc
               i20x=max(1,valuer(i20))
               if(ix2(i20).ne.' ')then
                  write(junout,'('' '',3a)')ix2(i20),' ',values(i20)(:i20x)
                  call journal(trim(junout))
               endif
            enddo
            mssg='*jucalc* variable listing complete'
         elseif(line(1:nchard).eq.'funcs') then     ! process funcs command
            call help_funcs()
   !--------------------------------------------------------------------------------------------------------------------------------
         else                                               ! this is an input line to process
            call jupars(line,nchard,ierr)                   ! process the command
            if(ierr.eq.0)then                 ! if no errors occurred set output string, store the value as last, store any variable
                                              ! numeric value with no errors, assume nchard is 20 or less
               outlin=line(1:nchard)                        ! set string output value
               last=line(1:nchard)                          ! store last value (for use with question-mark token)
               call juator(last(1:nchard),rlast,idum)       ! set real number output value
               if(nchar2.ne.0.and.varnam(1:1).ne.'$')then   ! if the statement defines a variable make sure variable name is stored
                  call jubous(varnam,index,ix,ierr)         ! determine storage placement of the variable and whether it is new
                  if(ierr.eq.-1)then
                     slast=rlast                            !set returned real value to last good calculated value
                     mssg=mssge                             !place internal message from GLOBAL into message returned to user
                     return
                  endif
                  if(index.le.0)then                        ! if the variable needs added, add it
                     call juaddr(varnam,nchar2,index,ierr)  ! adding the new variable name to the variable name array
                     if(ierr.eq.-1)then
                        slast=rlast                         ! set returned real value to last good calculated value
                        mssg=mssge                          ! place internal message from GLOBAL into message returned to user
                        return                              ! report error
                     endif
                  endif
                  call juator(last(1:nchard),value(iabs(index)),ierr)  ! store a defined variable's value
               elseif(nchar2.ne.0)then                      ! numeric value to string
                  line(:)=' '
                  line=varnam(1:nchar2)//'="'//last(1:nchard)//'"'
                  imax=len_trim(line)                       ! determine the length of the input line
                  cycle BIG
               endif
            elseif(ierr.eq.2)then ! returned output is not numeric, but alphanumeric (it is a string)
   !!!!!!!  could return string values directly instead of thru message field
   !!!!!!!  make sure normal output values are not left indeterminate
               mssg=mssge                                   ! set returned string value to returned string value
               if(nchar2.ne.0.and.varnam(1:1).eq.'$')then   ! if the statement defines a variable make sure variable name is stored
                  call jubous(varnam,index,ix2,ierr)        ! determine storage placement of the variable and whether it is new
                  if(ierr.eq.-1)then
                     slast=rlast                            ! set returned real value to last good calculated value
                     mssg=mssge                             ! place internal message from GLOBAL into message returned to user
                     return
                  endif
                  if(index.le.0)then                        ! if the variable needs added, add it
                     call juadds(varnam,nchar2,index,ierr)  ! adding the new variable name to the variable name array
                     if(ierr.eq.-1)then
                        slast=rlast                         ! set returned real value to last good calculated value
                        mssg=mssge                          ! place internal message from GLOBAL into message returned to user
                        return
                     endif
                  endif
                  values(iabs(index))=mssg
                  valuer(iabs(index))=len_trim(mssg)
                  rlast=dble(valuer(iabs(index)))           ! returned value is length of string when string is returned
               elseif(nchar2.ne.0)then                      ! string but being stored to numeric variable
                   line=varnam(1:nchar2)//'='//mssg
                   imax=len_trim(line)                      ! determine the length of the input line
                   cycle BIG
               else                                         ! a string function with an assignment to it (for example "Hello"
                  rlast=len_trim(mssg)                      ! probably should pass message length up from someplace
               endif
            endif
            mssg=mssge
         endif
         exit BIG
      enddo BIG
      slast=rlast                                           ! set returned value to last successfully calculated real value
end subroutine jucalc
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine help_funcs()
implicit none
character(len=*),parameter    :: ident=&
&"@(#)M_calculator::help_funcs(3fp): prints help for calculator functions"
character(len=80),allocatable :: help_text(:)
integer                       :: i
help_text=[ &
&'--------------------------------------------------------------------------------',&
&'standard functions available:                                                   ',&
&'--------------------------------------------------------------------------------',&
&'I/O: DEVELOPMENTAL                                                              ',&
&' open(unit,$filename)                                                           ',&
&' write(unit,values)                                                             ',&
&' $read(unit,name)                                                               ',&
&' $inquire(                                                                      ',&
&' rewind(unit)                                                                   ',&
&' flush(                                                                         ',&
&' close(                                                                         ',&
&'--------------------------------------------------------------------------------',&
&' c(                   : user-defined function                                   ',&
&' ownmode(             : call user-defined procedures                            ',&
&'--------------------------------------------------------------------------------',&
&' len_trim($value)     : number of characters trimming trailing spaces           ',&
&' index($value,$match) : return position $match occurs in $value or zero         ',&
&' sign(val1,val2)      : magnitude of val1 with the sign of val2                 ',&
&' real(value)          : conversion to real type                                 ',&
&' matchw($string,$pattern): wildcard match;*=any string, ?=any character         ',&
&' str($str|expr,....)  :append as strings and then convert to number             ',&
&' $str($str|expr,....) :append as strings                                        ',&
&' round(value,digits)  :                                                         ',&
&' ichar($value)        : return ASCII Decimal Equivalent of character            ',&
&' $char(value)         : return character given ASCII Decimal Equivalent         ',&
&' $f(format,value)     : using FORMAT to create it convert number to string      ',&
&' $if(expr,$val1,$val2): if expr==0 return $val1, else return $val2              ',&
&' if(expr,val1,val2)   : if expr==0 return val1, else return val2                ',&
&' hypot(x,y)           : Euclidean distance function                             ',&
&'--------------------------------------------------------------------------------',&
&'WHOLE NUMBERS:                                                                  ',&
&' aint(value) : truncation toward zero to a whole number                         ',&
&' anint(value): nearest whole number                                             ',&
&' int(value)  : conversion to integer type                                       ',&
&' nint(value) : nearest integer                                                  ',&
&' floor(A)    : greatest integer less than or equal to A                         ',&
&' ceiling(A)  : least integer greater than or equal to A                         ',&
&'--------------------------------------------------------------------------------',&
&'MISCELLANEOUS:                                                                  ',&
&' abs(value)  : absolute value                                                   ',&
&' max(v1,v2,v3,...v50)  : maximum value of list                                  ',&
&' min(v1,v2,v3,...v50)  : minimum value of list                                  ',&
&' log(v1)     : logarithm of value to base e                                     ',&
&' log10(v1)   : logarithm of value to base 10                                    ',&
&' exp(value)  : exponenent of value                                              ',&
&' sqrt(value) : return square root of value                                      ',&
&' dim(x,y)    : maximum of X-Y and zero                                          ',&
&' frac(A)     : fractional part of A (A - INT(A))                                ',&
&' mod(A,P)    : remainder function                                               ',&
&'--------------------------------------------------------------------------------',&
&'RANDOM NUMBERS:                                                                 ',&
&' srand(seed_value) : set seed value for rand()                                  ',&
&' rand()            : random number                                              ',&
&'--------------------------------------------------------------------------------',&
&'ARRAY STORAGE:                                                                  ',&
&' $nstore(start_index,$value1,$value2,$value3,....)|$n(index)                    ',&
&' $xstore(start_index,$value1,$value2,$value3,....)|$x(index)                    ',&
&' $ystore(start_index,$value1,$value2,$value3,....)|$y(index)                    ',&
&' xstore(start_index,value1,value2,value3,....)    |x(index)                     ',&
&' ystore(start_index,value1,value2,value3,....)    |y(index)                     ',&
&'--------------------------------------------------------------------------------',&
&'STRING MODIFICATION:                                                            ',&
&' $change($input_string,"c/old_substring/new_substring")                         ',&
&' $modif($input_string,"modification_directive &=blank#=delete^=insert"          ',&
&' $l($input_string) : convert string to lowercase                                ',&
&' $u($input_string) : convert string to uppercase                                ',&
&' $substr($input_string,start_column,end_column)                                 ',&
&' $str($a|e,$a|e,$a|e,....):append string and value expressions into string      ',&
&'--------------------------------------------------------------------------------',&
&'CALENDAR:                                                                       ',&
&'|ye() : current year   |ho() : current hour   |$dw([n]): day of week            ',&
&'|mo() : current month  |mi() : current minute |$mo([n]): name of month          ',&
&'|da() : current day    |se() : current second |dw()    : day of week            ',&
&'|ju() : day of year    |$now(format)          |$fmtdate(dat(8),format)          ',&
&'--------------------------------------------------------------------------------',&
&'TRIGONOMETRIC:                                                                  ',&
&'|cos(radians):cosine   |acos(x/r)             |cosh()                           ',&
&'|sin(radians):sine     |asin(y/r)             |sinh()                           ',&
&'|tan(radians):tangent  |atan(y/x)             |tanh()                           ',&
&'|                      |atan2(x,y)            |                                 ',&
&'--------------------------------------------------------------------------------',&
&'UNIT CONVERSION:                                                                ',&
&'|c2f(c) : centigrade to Fahrenheit |f2c(f) : Fahrenheit to centigrade           ',&
&'|d2r(d) : degrees to radians       |r2d(r) : radians to degrees                 ',&
&'--------------------------------------------------------------------------------',&
&'LOGICAL:                                                                        ',&
&'|ge(a,b) : greater than or equal to                                             ',&
&'|le(a,b) : A less than or equal to B                                            ',&
&'|gt(a,b) : A greater than B                                                     ',&
&'|lt(a,b) : A less than B                                                        ',&
&'|eq(a,b) : A equal to B                                                         ',&
&'|ne(a,b) : A not equal to B                                                     ',&
&'|lge($a,$b): lexically greater than or equal to                                 ',&
&'|lle($a,$b): lexically A less than or equal to B                                ',&
&'|lgt($a,$b): lexically A greater than B                                         ',&
&'|llt($a,$b): lexically A less than B                                            ',&
&'|leq($a,$b): lexically A equal to B                                             ',&
&'|lne($a,$b): lexically A not equal to B                                         ',&
&'|in(lower_bound,test_value,upper_bound) : test if value in given range          ',&
&'--------------------------------------------------------------------------------',&
&'                                                                                ']
   WRITE(*,'(a)')(help_text(i),i=1,size(help_text))
end subroutine help_funcs
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine jupars(string,nchar,ier)
!  sets and returns ier
!   0=good numeric return
!   2=good alphameric return
!  -1=error occurred, message is in mssge
implicit doubleprecision (a-h,o-z)
character(len=*),parameter :: ident=&
&"@(#)M_calculator::jupars(3fp): crack out the parenthesis and solve"
character(len=*)             :: string
integer,intent(inout)        :: nchar
integer,intent(out)          :: ier

character(len=icbuf_calc) :: wstrng
character(len=icbuf_calc) :: dummy
integer                   :: imax
integer                   :: ileft
integer                   :: iright
integer                   :: i
integer                   :: iz
integer                   :: iwnchr
!#----------------------------------------------------------------------------------------------------------------------------------
   imax=nchar
   ier=0
   INFINITE: do
!#----------------------------------------------------------------------------------------------------------------------------------
   ileft=0                                    ! where rightmost left paren was found
   do i=imax,1,-1                             ! find rightmost left paren
      if(string(i:i).eq.'(')then
         ileft=i
         exit
      endif
   enddo
!#----------------------------------------------------------------------------------------------------------------------------------
      if(ileft.eq.0)then                          ! no left parenthesis was found; finish up
         if(index(string(:nchar),')').ne.0) then  ! if here there are no left paren. check for an (unmatched) right paren
            ier=-1
            mssge='*jupars* extraneous right parenthesis found'
         else
   !        no parenthesis left, reduce possible expression to a single value primitive and quit
   !        a potential problem is that a blank string or () would end up here too.
            call jucals(string,nchar,rdum,ier)
         endif
         return
      endif
!#----------------------------------------------------------------------------------------------------------------------------------
      iright=index(string(ileft:nchar),')') ! left parenthesis was found; find matching right paren
      if(iright.eq.0) then
         ier=-1
         mssge='*jupars* right parenthesis missing'
         return
      endif
!#----------------------------------------------------------------------------------------------------------------------------------
      iright=iright+ileft-1  !there was a matched set of paren remaining in the string
      iz=1  ! check now to see if this is a function call. search for an operator
!     if ileft is 1, then last set of parenthesis,(and for an expression)
      if(ileft.ne.1)then
         do i=ileft-1,1,-1
            iz=i
            if(index('#=*/(,',string(i:i)).ne.0)then
               iz=iz+1
               goto 11
            endif
         enddo
!        if here, a function call begins the string, as iz=1 but ileft doesn't
      endif
!=======================================================================------------------------------------------------------------
!     iz=position beginning current primitive's string
!     ileft=position of opening parenthesis for this primitive
!     iright=position of end and right parenthesis for this string
11    continue
      if(iz.eq.ileft)then  ! if ileft eq iz then a parenthesized expression, not a function call
         wstrng=string(ileft+1:iright-1)
         iwnchr=iright-1-(ileft+1)+1
         call jucals(wstrng,iwnchr,rdum,ier)
      else
         wstrng=string(iz:iright)
         iwnchr=iright-iz+1
         call jufuns(wstrng,iwnchr,ier)
      endif
      if(ier.eq.-1)return !     if an error occurred in jucals or jufuns, then return
      ! restring the evaluated primitive back into the main string
      ! remember that if an expression, iz=ileft
      ! last set of -matched- parentheses, and entire string was evaluated
      if(iz.eq.1.and.iright.eq.nchar)then
         dummy=wstrng(:iwnchr)
         nchar=iwnchr
!        last set of -matched- parentheses, but other characters still to right
      elseif(iz.eq.1)then
         dummy=wstrng(:iwnchr)//string(iright+1:nchar)
         nchar=iwnchr+nchar-iright
      elseif(iright.eq.nchar)then
!        last expression evaluated was at end of string
         dummy=string(:iz-1)//wstrng(:iwnchr)
         nchar=iz-1+iwnchr
      else
!        last expression evaluated was in middle of string
         dummy=string(:iz-1)//wstrng(:iwnchr)//string(iright+1:nchar)
         nchar=iz-1+iwnchr+nchar-iright
      endif
!     set last place to look for a left parenthesis to one to the left
!     of the beginning of the primitive just reduced, or to a 1 so that
!     the loop looking for the left parenthesis doesn't look for a
!     parenthesis at position 0:0
      imax=max(iz-1,1)
      string=dummy
   enddo INFINITE
end subroutine jupars
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine jufuns(wstrng,nchars,ier)
use M_time,    only : date_to_unix , unix_to_date, fmtdate, now, fmtdate_usage, realtime
use M_STRINGS, only : matchw, change, modif, delim
use M_TIME,    only : date_to_julian,day_of_week=>dow, d2o, now
use M_math,    only : round, dp_accdig
implicit doubleprecision (a-h,o-z)
character(len=*),parameter :: ident=&
&"@(#)M_calculator::jufuns(3fp):reduce name(p1,p2,...) (p(i) are non-parenthesized expressions)"
!  and call the procedure "name" with those values passed as the parameters.
   external juown1
   real,external                       :: c
   doubleprecision                     :: fval
   real(kind=realtime)                 :: uepoch
   integer          :: ierr
   integer          :: idat(8)
   integer          :: iweekday
   character(len=9) :: day

   integer,save                        :: ikeepran=22
   doubleprecision,external            :: ran_mod

   intrinsic                           :: acos,asin,atan,cos,cosh,sin,sinh,tan,tanh
   intrinsic                           :: abs,aint,anint,exp,nint,int,log,log10
   intrinsic                           :: sqrt,atan2,dim,mod,sign,max,min
   character(len=icname_calc)          :: wstrng2
   character(len=iclen_calc)           :: ctmp
   character(len=iclen_calc)           :: ctmp2
   character(len=*)                    :: wstrng
   character(len=icname_calc)          :: cnum
   character(len=iclen_calc)           :: junout
   integer,parameter                   :: iargs=100
   integer                             :: ifail
   integer                             :: itime(8)
   doubleprecision,target              :: args(iargs)
   integer                             :: iargs_type(iargs)
   integer                             :: ibegin(ixyc_calc),iterm(ixyc_calc)
   integer                             :: idarray(8)
   integer                             :: itype
   character(len=10),save              :: months(12)
   character(len=10),save              :: days(7)
!-----------------------------------------------------------------------------------------------------------------------------------
   data months/'January','February','March','April','May','June','July','August','September','October','November','December'/
   data days/'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'/
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc------------------------------------------------------------
!  non-ANSI:
!     x(i) ---- the x-array values
!     y(i) ---- the y-array values
!     xstore(start,value1,value2,value3,....valuen)
!     ystore(start,value1,value2,value3,....valuen)
!     $x(i) ---- the $x-array values
!     $y(i) ---- the $y-array values
!     $xstore(start,value1,value2,value3,....valuen)
!     $ystore(start,value1,value2,value3,....valuen)
!     d2r    - degrees to radians
!     r2d    - radians to degrees
!     date_to_unix - date values to Unix Epoch Time
!     unix_to_date - Unix Epoch Time to date values in X array
!     ownmode
!     $str(), str()
!     ye(), mo(), da(), ho(), mi(), se(), dw(), $dw(), $mo(), $now(), $fmtdate()
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc------------------------------------------------------------
   TRUE=0.0d0
   FALSE=1.0d0
   ier=0
   iright=nchars-1
   ileft=index(wstrng(1:nchars),'(')+1
   iend=ileft-2
   iflen=iright-ileft+1
!  n=number of parameters found
   if(iright-ileft.lt.0)then ! if call such as fx() expression string is null
      n=0
   else ! take string of expressions separated by commas and place values into an array and return how many values were found
      call juargs(wstrng(ileft:iright),iflen,args,iargs_type,n,ier,100)
      if(ier.eq.-1)then
         goto 999
      else
         ier=0 ! ier could be 2 from juargs()
      endif
   endif
   wstrng2=' '
   wstrng2(:iend)=lower(wstrng(:iend))
   fval=0.0d0
   if(ier.eq.-1)then
      goto 999
   endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
select case (wstrng2(:iend))
case("abs","aint","anint","ceil","ceiling","floor","frac","int","nint",&
    &"d2r","r2d",&
    &"c2f","f2c",&
    &"gamma","log_gamma",&
    &"log","log10","exp",&
    &"bessel_j0","bessel_j1","bessel_y0","bessel_y1",&
    &"erf","erfc","erfc_scaled",&
    &"sin","cos","tan",&
    &"sind","cosd","tand",&
    &"sinh","cosh","tanh",&
    &"asin","acos","atan",&
    &"asinh","acosh","atanh",&
!   &"cpu_time",&
    &"exponent","fraction",&
    &"real","sqrt")
      if(n.ne.1)then                                                    ! check number of parameters
        mssge='*jufuns* incorrect number of parameters in '//wstrng2(:iend)
        ier=-1
      elseif(iargs_type(1).ne.0)then                                    ! check type of parameters
        mssge='*jufuns* parameter not numeric in '//wstrng2(:iend)
        ier=-1
      else                                                              ! single numeric argument
         select case (wstrng2(:iend))
!=======================================================================------------------------------------------------------------
         case("acos");

         if(args(1).gt.1.or.args(1).lt.-1)then
           mssge='*acos* parameter not in range -1 >= value <=1'
           ier=-1
         else
            fval= acos(args(1))
         endif
         case("atan");   fval= atan(args(1))
         case("asin");   fval= asin(args(1))

         !case("cpu_time");   fval= cpu_time(args(1))
         case("fraction");   fval= fraction(args(1))
         case("exponent");   fval= exponent(args(1))
         case("gamma");      fval= gamma(args(1))
         case("log_gamma");  fval= log_gamma(args(1))

         case("cos");    fval= cos(args(1))
         case("sin");    fval= sin(args(1))
         case("tan");    fval= tan(args(1))

         case("acosh");    fval= acosh(args(1))
         case("asinh");    fval= asinh(args(1))
         case("atanh");    fval= atanh(args(1))

         case("cosd");   fval= cos(args(1)*acos(-1.0d0)/180.d0)
         case("sind");   fval= sin(args(1)*acos(-1.0d0)/180.d0)
         case("tand");   fval= tan(args(1)*acos(-1.0d0)/180.d0)

         case("cosh");   fval= cosh(args(1))
         case("sinh");   fval= sinh(args(1))
         case("tanh");   fval= tanh(args(1))

         case("erf");         fval= erf(args(1))
         case("erfc");        fval= erfc(args(1))
         case("erfc_scaled"); fval= erfc_scaled(args(1))

         case("d2r");    fval= args(1)*acos(-1.0d0)/180.d0
         case("r2d");    fval= args(1)*180.d0/acos(-1.0d0)

         case("c2f");    fval= (args(1)+40.0d0)*9.0d0/5.0d0 - 40.0d0
         case("f2c");    fval= (args(1)+40.0d0)*5.0d0/9.0d0 - 40.0d0

         case("bessel_j0");    fval= bessel_j0(args(1))
         case("bessel_j1");    fval= bessel_j1(args(1))
         case("bessel_y0");    fval= bessel_y0(args(1))
         case("bessel_y1");    fval= bessel_y1(args(1))

         case("abs");    fval= abs(args(1))
         case("aint");   fval= aint(args(1))
         case("anint");  fval= anint(args(1))
         case("ceil","ceiling");   fval=ceiling(real(args(1)))
         case("exp");    fval= exp(args(1))
         case("floor");  fval= floor(real(args(1)))
         case("frac");   fval= args(1)-int(args(1))
         case("int");    fval= int(args(1))
         case("nint");   fval= nint(args(1))
         case("real");   fval= real(args(1))
         case("sqrt");   fval= sqrt(args(1))
!=======================================================================------------------------------------------------------------
         case("log")
            if(args(1).le.0.0d0)then                                          ! check for appropriate value range for function
               call journal('sc','*log* ERROR: cannot take log of ',real(args(1)))
            else                                                              ! call function with one positive numeric parameter
               fval= log(args(1))
            endif
!=======================================================================------------------------------------------------------------
         case("log10")
            if(args(1).le.0.0d0)then                                          ! check for appropriate value range for function
               call journal('sc','*log10* ERROR: cannot take log of ',real(args(1)))
            else                                                              ! call function with one positive numeric parameter
               fval= log10(args(1))
            endif
!=======================================================================------------------------------------------------------------
         end select
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("atan2","dim","mod","bessel_jn","bessel_yn","sign","hypot","modulo","scale")
      if(n.ne.2)then                                                           ! check number of parameters
        mssge='*jufuns* incorrect number of parameters in '//wstrng2(:iend)
        ier=-1
      elseif(.not.all(iargs_type(1:2).eq.0))then                                ! check type of parameters
        mssge='*jufuns* parameters not all numeric in '//wstrng2(:iend)
        ier=-1
      else                                                                     ! single numeric argument
         select case (wstrng2(:iend))
         case("atan2");      fval=  atan2      (  args(1),       args(2)       )
         case("dim");        fval=  dim        (  args(1),       args(2)       )
         case("mod");        fval=  mod        (  args(1),       args(2)       )
         case("modulo");     fval=  modulo     (  args(1),       args(2)       )
         case("scale");      fval=  scale      (  args(1),       int(args(2))  )
         case("bessel_jn");  fval=  bessel_jn  (  int(args(1)),  args(2)       )
         case("bessel_yn");  fval=  bessel_yn  (  int(args(1)),  args(2)       )
         case("btest")
            if (btest( int(args(1)), int(args(2)) ) ) then
               fval=TRUE
            else
               fval=FALSE
            endif
         case("sign");       fval=  sign       (  args(1),       args(2)       )
         case("hypot");      fval=  hypot      (  args(1),       args(2)       )
         end select
      endif
!=======================================================================------------------------------------------------------------
case("tiny")
      fval=tiny(0.d0)
case("epsilon")
      fval=epsilon(0.d0)
case("huge")
      fval=huge(0.d0)
!=======================================================================------------------------------------------------------------
case("x");
      ivalue=int(args(1)+0.5)
      if(ivalue.lt.1.or.ivalue.gt.ixy_calc)then              ! if value not at least 1, or if not less than ixy_calc, report it
        mssge='*jufuns* illegal subscript value for x array'
        ier=-1
      else
        fval= x(ivalue)
      endif
!=======================================================================------------------------------------------------------------
case("y")
      ivalue=int(args(1)+0.5)
!     if value not at least 1, make it 1. if not less than ixy_calc, make it ixy_calc
      if(ivalue.lt.1.or.ivalue.gt.ixy_calc)then
         mssge='*jufuns* illegal subscript value for y array'
         ier=-1
      else
         fval= y(ivalue)
      endif
!=======================================================================------------------------------------------------------------
case("max")
      if(n.lt.1)then
         ier=-1
         mssge='*max* incorrect number of parameters for '//wstrng(:iend)
      elseif(.not.all(iargs_type(1:n).eq.0))then ! check type of parameters
         ier=-1
         mssge='*max* illegal parameter type (must be numeric)'
      else
         fval=args(1)
         do i=2,n
            fval=max(fval,args(i))
         enddo
      endif
!=======================================================================------------------------------------------------------------
case("min")
       if(n.lt.1)then
         ier=-1
         mssge='incorrect number of parameters for '//wstrng(:iend)
      elseif(.not.all(iargs_type(1:n).eq.0))then ! check type of parameters
         ier=-1
         mssge='*min* illegal parameter type (must be numeric)'
       else
          fval=args(1)
          do i=2,n
             fval=min(fval,args(i))
          enddo
       endif
!=======================================================================------------------------------------------------------------
case("xstore","ystore")                                        ! xstore function===>(where_to_start,value1,value2,value3...)
      if(n.lt.2)then                                           ! need at least subscript to start storing at and a value
         ier=-1
         mssge='incorrect number of parameters for '//wstrng(:iend)
         fval=0.0d0
      else                                                     ! at least two values so something can be stored
         istoreat=int(args(1)+0.50d0)                          ! array subscript to start storing values at
         if(istoreat.lt.1.or.istoreat+n-2.gt.ixy_calc)then     ! ignore -entire- function call if a bad subscript reference was made
            mssge='*jufuns* illegal subscript value for array in '//wstrng(:iend)
            ier=-1
            fval=0.0d0
         else                                                  ! legitimate subscripts to store at
            STEPTHRU: do i1033=2,n                             ! for each argument after the first one store the argument
               select case(wstrng2(:iend))                     ! select X array or Y array
                  case("xstore");x(istoreat)=args(i1033)
                  case("ystore");y(istoreat)=args(i1033)
               end select
               istoreat=istoreat+1                             ! increment location to store next value at
            enddo STEPTHRU
            fval=args(n)                                       ! last value stored will become current value
         endif
      endif
!=======================================================================------------------------------------------------------------
case("lle","llt","leq","lge","lgt","lne")
      if(iargs_type(1).eq.2.and.iargs_type(2).eq.2)then
         do i2020=1,n
            if(args(i2020).le.0.or.args(i2020).gt.ic_calc)then
               ier=-1
               mssge='unacceptable locations for strings encountered'
               goto 999
            endif
         enddo
         fval=FALSE ! assume false unless proven true
         i1=int(args(1))
         i2=int(args(2))
         ier=0
         select case (wstrng2(:iend))
         case("lle")
            if(values(i1).le.values(i2))fval=TRUE
         case("llt")
            if(values(i1).lt.values(i2))fval=TRUE
         case("leq") ! if any string matches the first
            do i410=2,n
               if(iargs_type(i410).ne.2)then     ! all parameters should be a string
                  ier=-1
                  mssge='non-string value encountered'
               elseif(values(i1).eq.values(int(args(i410)+.5)))then
                  fval=TRUE
               endif
            enddo
         case("lge")
            if(values(i1).ge.values(i2))fval=TRUE
         case("lgt")
            if(values(i1).gt.values(i2))fval=TRUE
         case("lne")
            do i440=2,n
               fval=TRUE
               if(iargs_type(i440).ne.2)then     ! all parameters should be a string
                  ier=-1
                  mssge='non-string value encountered'
               elseif(values(i1).eq.values(int(args(i440)+0.5)))then
                  fval=FALSE
               endif
            enddo
         case default
            ier=-1
            mssge='internal error in jufuns in lexical functions'
         end select
      else
         ier=-1
         mssge='lexical functions must have character parameters'
      endif
!=======================================================================------------------------------------------------------------
case("le","lt","eq","ge","gt","ne")
      fval=FALSE
      do i520=1,n
         if(iargs_type(i520).ne.0)then  ! this parameter was not a number
            ier=-1
            mssge='*logical* parameter was not a number'
            goto 999
         endif
      enddo
      if(n.eq.2.or.n.eq.3)then
         if(n.eq.3)then
            idig=int(args(3))
            if(idig.le.0.or.idig.gt.13)then
               mssge='*logical* precision must be between 1 and 13'
               ier=-1
               goto 999
            endif
            write(junout,'(a,3(g20.13e3,1x),i5)')'args=',args(1),args(2),args(3),idig
            call journal(junout)
            arg1=round(args(1),idig)
            arg2=round(args(2),idig)
            write(junout,'(a,3(g20.13e3,1x),i5,1x,2(g20.13e3,1x))')'b. args=',args(1),args(2),args(3),idig,arg1,arg2
            call journal(junout)
         else
            arg1=args(1)
            arg2=args(2)
         endif
         call stuff('LOGICAL1',arg1)
         call stuff('LOGICAL2',arg2)
         call stuff('STATUS',arg2-arg1)
         select case(wstrng2(:iend))
         case("le"); if(arg1.le.arg2)fval=TRUE
         case("lt"); if(arg1.lt.arg2)fval=TRUE
         case("eq"); if(arg1.eq.arg2)fval=TRUE
         case("ge"); if(arg1.ge.arg2)fval=TRUE
         case("gt"); if(arg1.gt.arg2)fval=TRUE
         case("ne"); if(arg1.ne.arg2)fval=TRUE
         case default
            ier=-1
            mssge='*logical* internal error in jufuns'
         end select
      else
         ier=-1
         mssge='*logical* must have 2 or 3 parameters'
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("ichar")
      if(n.ne.1)then
         mssge='*ichar* takes one parameter'
         ier=-1
      elseif(iargs_type(1).ne.2)then
         mssge='*ichar* parameter must be a string'
         ier=-1
      else
         fval=ichar(values(int(args(1)))(1:1))
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("unix_to_date")
      ifail=0
      select case(n)
      case(0)
         call date_and_time(values=itime)                  ! if no input time given return current time
      case(1)
      uepoch=args(1)
         call unix_to_date(uepoch,itime,ifail)             ! Convert Unix Time to date array
      case default
         mssge="*unix_to_date* incorrect number of parameters"
         ier=-1
      end select
      if(ifail.ne.0)then
         mssge="*unix_to_date* incorrect input parameters - time conversion failed"
         ier=-1
      else
         x(1:8)=dble(itime)
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("date_to_unix")
      ifail=0
      select case(n)
      case(0)
         call date_and_time(values=itime)
         call date_to_unix(itime,uepoch,ifail)                        ! Convert date array to Unix Time
         fval=uepoch
      case(8)
         call date_to_unix(int(args(1:8)),uepoch,ifail)               ! Convert date array to Unix Time
         fval=uepoch
      case default
         mssge="*date_to_unix* incorrect number of parameters"
         ier=-1
      end select
      if(ifail.ne.0)then
         mssge="*date_to_unix* incorrect input parameters - time conversion failed"
         ier=-1
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$fmtdate")  ! $fmtdate (utime,format) ==> fmtdate(dat,format) result (timestring) ! Convert date array to string using format
      ctmp=' '
      ier=2                                                     ! flag indicating returning string
      ! check that arguments are acceptable string and numeric variables, should do generically at name lookup time
      select case(n)
      case(0)                                                   ! if no arguments return current time using default format
         ctmp=now()
      case(1)                                                   ! one parameter
         if(iargs_type(1).eq.2)then                             ! a string is assumed to be a format
            if(values(int(args(1))).eq. "help") then            ! show help
               call fmtdate_usage(3)
               ctmp=' '
            else
               ctmp=now( values(int(args(1))) )
            endif
         else                                                   ! a single numeric value is assumed to be a Unix Epoch time
            uepoch=args(1)
            call unix_to_date(uepoch,itime,ifail)
            ctmp=fmtdate(itime,'%Y-%M-%D %h:%m:%s %Z')
         endif
      case(2)
         if(iargs_type(1).eq.1.and.iargs_type(2).eq.2)then     ! assumed called with Unix Epoch Time and Time Format
            uepoch=args(1)
            call unix_to_date(uepoch,itime,ifail)
            ctmp=fmtdate(itime,values(int(args(2))))
         else
            mssge='*$fmtdate* wrong type of parameters  fmtdate(Unix_Epoch_Time,time_format)'
            ier=-1
         endif
      case(3:9)  ! not very bullet-proof, assumes all numbers except last parameter=format, seeds with current time,
         call date_and_time(values=itime)
         itime(1:n-1)=int([args(1:n-1)])
         ctmp=fmtdate(itime,values(int(args(n))))
      case default
         mssge='*$fmtdate* wrong number of parameters '
         ier=-1
      end select
      iend=len_trim(ctmp)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("same")
      if(n.ne.3)then
         mssge='*digits* takes 3 parameters'
         ier=-1
      else
         call dp_accdig(args(1),args(2),args(3),ACURCY,IND)
         fval=IND
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("if")
      if(args(1).eq. TRUE)then
        fval=args(2)
      else
        fval=args(3)
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$if")                                      ! $if function
      ier=2                                      ! returning string
!     check that 2nd and 3rd are acceptable string variables, should do generically at name lookup time
      if(args(1).eq. TRUE)then
        ii=int(args(2))
      else
        ii=int(args(3))
      endif
      ctmp=values(ii)
      iend=valuer(ii)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("in")                                                            ! in(lower_value,value,upper_value)
      fval=FALSE
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      select case(n)
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case(2)                                                         ! if two parameters test }first - second}<epsilon
        if(iargs_type(1).eq.0.and.iargs_type(2).eq.0)then
           val=abs(args(1)-args(2))
           top=epsilon(0.0d0)
           bottom=-top
        else
         mssge='*in* parameters must be numeric'
         ier=-1
        endif
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case(3)                                                         ! if three parameters test if second between first and third
        if(iargs_type(1).eq.0.and.iargs_type(2).eq.0.and.iargs_type(3).eq.0)then
           bottom=args(1)
           val=args(2)
           top=args(3)
        else
         mssge='*in* parameters must be numeric'
         ier=-1
        endif
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case default
         mssge='*in* number of parameters not valid IN(LOWER_VALUE,VALUE,UPPER_VALUE)'
         ier=-1
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      end select
      !=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      if(ier.ge.0) then
         if(val.ge.bottom.and.val.le.top)fval=TRUE
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("index")
      ii=int(args(1))
      iii=int(args(2))
      if(iargs_type(1).eq.2.and.iargs_type(2).eq.2)then ! if parameter was a string leave it alone
         iend1=valuer(ii)
         iend2=valuer(iii)
         fval=index(values(ii)(:iend1),values(iii)(:iend2))
      endif
      ier=0   ! flag that returning a number, not a string
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("len","len_trim")
      ii=int(args(1))
      iend1=valuer(ii)
      fval=len_trim(values(ii)(:iend1))
      ier=0   ! flag that returning a number, not a string
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("rand")                                                            ! random number
      select case (n)                                                   ! check number of parameters
      case (0)                                                          ! use default method
         itype=3
      case (1)                                                          ! determine user-specified method
         itype=int(args(1)+0.5)
         if(itype.lt.1.or.itype.gt.3)then
            itype=3
         endif
      case default
         mssge='illegal number of arguments for rand()'
         ier=-1
         itype=-1
      end select

      select case (itype)                                               ! select various methods
      case (-1)                                                         ! an error has already occurred
      case (2)                                                          ! standard Fortran function
         call random_number(harvest=fval)
      case default                                                      ! "Numerical Recipes" routine
         fval=ran_mod(ikeepran)
      end select
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("srand")                                                           ! seed random number sequence
      select case (n)                                                   ! check number of parameters
      case (1)                                                          ! no user-specified type
         itype=3                                                        ! use default method
      case (2)                                                          ! determine user-specified method
         itype=int(args(2)+0.5)                                         ! user-specified type
      case default                                                      ! call syntax error
         mssge='illegal number of arguments for srand()'
         ier=-1
      end select
      if(ier.eq.0)then
         ivalue=int(args(1)+0.5)                                           ! determine seed value
         select case (itype)                                               ! select various methods
         case (2)                                                          ! standard Fortran method
            call init_random_seed(ivalue)
         case (3)                                                          ! default is "Numerical Recipes" method
            ikeepran=-abs(ivalue)
            fval=ran_mod(ikeepran)                                         ! just setting seed; fval is a dummy here
         case default
            mssge='unknown type for srand()'
            ier=-1
         end select
         fval=ivalue
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("open")
      ios=0
      if(n.le.0)then
          do i852=1,99
             ctmp=' '
             call print_inquire(i852,ctmp)
             call journal('sc',ctmp,i852)
          enddo
      elseif(n.eq.1)then
      elseif(n.ne.2)then
         fval=-1
      else
         iunit=int(args(1))
         ii= int(args(2)+0.5)
         ctmp=values(ii)
         iend=valuer(ii)
         open(unit=iunit,file=ctmp(:iend),iostat=ios, status='unknown',form='formatted')
      endif
      fval=ios
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("close")
      iunit=int(args(1))
      ios=0
      close(iunit,iostat=ios)
      fval=ios
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("rewind")
      iunit=int(args(1))
      ios=0
      rewind(iunit,iostat=ios)
      fval=ios
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("write")
         iunit=int(args(1))
         ios=0
         ctmp=' '
         ii= int(args(2)+0.5)
         ctmp=values(ii)
         iend=valuer(ii)
         if(iunit.le.0)then
            write(*,'(a)',iostat=ios)values(ii)(:iend)
         else
            write(iunit,'(a)',iostat=ios)values(ii)(:iend)
         endif
      fval=ios
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("flush")
         iunit=int(args(1))
         ios=0
         flush(unit=iunit,iostat=ios)
         !call flush(iunit)
      fval=ios
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$read")
      ier=2                             ! string will be returned
      iunit=int(args(1))
      ios=0
      ctmp=' '
      read(iunit,'(a)',iostat=ios)ctmp
      fval=ios
      iend=len_trim(ctmp)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$inquire")
      ier=2                             ! string will be returned
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$f")                              ! $f(format,value) Using single format specifier, return string
      ier=2                             ! string will be returned
      if(n.eq.0)then
         ctmp=' '
      else
         ctmp=' '
         if(iargs_type(1).eq.2)then     ! if first field is a string
            ii=int(args(1))             ! get index into values() array
            iend1=valuer(ii)            ! maximum end is at end of string
            if(n.gt.1)fval=args(n)      ! get the real value
            write(ctmp,'('// values(ii)(:iend1)//')',iostat=ios)args(2:n)
            if(ios.ne.0)then
               ctmp='*'
               ier=-1
               mssge='*$f() error writing value'
            endif
         endif
      endif
      iend=len_trim(ctmp)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$char")
      ier=2                                      ! return string
      if(n.eq.0)then
         ier=-1
         mssge='*$char* must have at least one parameter'
      else
         iend=0
         do i3030=1,n                            ! unlike FORTRAN, can take multiple characters and mix strings and numbers
            ii=int(args(i3030))
            if(iargs_type(i3030).eq.2)then       ! if parameter was a string leave it alone
               iend2=iend+valuer(ii)
               ctmp(iend+1:iend2)=values(ii)
               iend=iend2
            else                                 ! convert numeric ADE to a character
               iend=iend+1
               ctmp(iend:iend)=char(ii)
            endif
         enddo
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$substr")                                  ! $substr(string,start,end)
      ier=2                                      ! return string
      if(n.eq.0)then
         ctmp=' '
      else
         ii=int(args(1))
         istart=1
         iend1=valuer(ii)                        ! maximum end is at end of string
         ctmp=' '
         if(iargs_type(1).eq.2)then
            if(n.gt.1)istart=min(max(1,int(args(2))),iend1)
            if(n.gt.2)iend1=max(min(int(args(3)),iend1),1)
            iend=iend1-istart+1
            ctmp(:iend)=values(ii)(istart:iend1)
         endif
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$nstore","$xstore","$ystore")
      ier=2                                      ! return string
      if(n.lt.2)then
         ier=-1
         mssge='incorrect number of parameters for '//wstrng(:iend)
      else
         ivalue=int(args(1)+0.5)
!        $nstore function===>store $n(where_to_start,value1,value2,value3...)
!        ignore -entire- function call if a bad subscript reference was made
         if(ivalue.lt.1.or.ivalue+n-2.gt.ixyc_calc)then
           mssge='illegal subscript value for array in '//wstrng2(:iend)
           ier=-1
         else
            do i1066=ivalue,ivalue+n-2,1
               isub=i1066-ivalue+2
               select case(wstrng2(:iend))
               case("$nstore"); nc(i1066)=values(int(args(isub)))
               case("$xstore"); xc(i1066)=values(int(args(isub)))
               case("$ystore"); yc(i1066)=values(int(args(isub)))
               end select
            enddo
            ctmp=values(ivalue+n-2)
            iend=len_trim(ctmp) ! very inefficient
         endif
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("str","$str","$") ! "$str" appends numbers and strings into a new string
                   ! "str" converts string to number IF string is simple numeric value
      jend=0
      ctmp=' '
      do i1010=1,n
         istart=jend+1                                     ! where to start appended argument in output string
         if(iargs_type(i1010).eq.2)then                    ! this parameter was a string
            in=int(args(i1010))                            ! the value of a string argument is the subscript for where the string is
            jend=istart+valuer(in)-1                       ! where appended argument ends in output string
            ctmp(istart:jend)=values(in)(:valuer(in))
         elseif(iargs_type(i1010).eq.0)then                ! this parameter was a number
            if(args(i1010).ne.0)then
               call jurtoa(args(i1010),cnum,ilen,ier)
               if(ier.ne.-1)then
                  ilen=max(ilen,1)
                  jend=istart+ilen-1
                  if(cnum(ilen:ilen).eq.'.')jend=jend-1    ! this number ends in a decimal
                  jend=max(jend,istart)
                  if(jend.gt.len(ctmp))then
                     call journal('*jufuns* $str output string truncated')
                     jend=len(ctmp)
                  endif
                  ctmp(istart:jend)=cnum(:ilen)
               endif
            else                                           ! numeric argument was zero
               ctmp(istart:istart)='0'
               jend=jend+1
            endif
         else
            mssge='*jufuns* parameter to function $str not interpretable'
            ier=-1
         endif
      enddo
      if(ier.ge.0)then
         select case(wstrng2(:iend))
         case("$str","$")
             ier=2
         case("str")
             ier=0
             call juator(ctmp,fval,ier)                    ! str function
         case default
            mssge='*jufuns* internal error: should not get here'
            ier=-1
         end select
      endif
      iend=jend
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$x","$y","$n")
      ier=2                                                  ! returning string
      ivalue=int(args(1)+0.5)
      if(ivalue.lt.1.or.ivalue.gt.ixyc_calc)then             ! if value not at least 1, or if not less than ixyc_calc, report it
        mssge='illegal subscript value for $x array'
        ier=-1
      else
         select case(wstrng2(:iend))
            case("$x");ctmp= xc(ivalue)
            case("$y"); ctmp= yc(ivalue)
            case("$n"); ctmp= nc(ivalue)
         end select
         iend=len_trim(ctmp) ! very inefficient
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("unusedf")
      fval=0.0d0
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$l") ! $l lower(string)
      ier=2                                      ! returning string
      if(n.ne.1)then
         ctmp=' '
         ier=-1
         mssge='*$l* must have one parameter'
      else
         ctmp=lower(values(int(args(1)+0.5)))
         iend=len_trim(ctmp) ! very inefficient
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$u")! $u upper(string)
      ier=2                                      ! returning string
      if(n.ne.1)then
         ctmp=' '
         ier=-1
         mssge='*$u* must have one parameter'
      else
         ctmp=upper(values(int(args(1)+0.5)))
         iend=len_trim(ctmp) ! very inefficient
      endif
!=======================================================================------------------------------------------------------------
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$modif")
      ier=2                                      ! returning string
      if(n.ne.2)then
         ctmp=' '
         ier=-1
         mssge='*modif* must have two parameters'
      elseif(iargs_type(1).ne.2.or.iargs_type(2).ne.2)then ! parameter not a string
         ctmp=' '
         ier=-1
         mssge='*modif* parameter(s) not a string'
      else
         ctmp=values(int(args(1)+0.5)) ! string to modify
         call modif(ctmp,values(int(args(2)+0.5)))
         iend=len_trim(ctmp) ! very inefficient
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("matchw")
      ! make one that ignores case?
      ! make one that returns matched string or blank string?
      if(n.ne.2)then                                   ! if not two parameters
         ier=-1
         mssge='*matchw* takes two parameters'
         ier=-1
         fval=(-1)
       elseif(iargs_type(1).eq.2.and.iargs_type(2).eq.2)then ! parameters are strings
          ii=int(args(1))                              ! index of first string
          iii=int(args(2))                             ! index of second string
          iie=valuer(ii)                               ! last non-blank character
          iiie=valuer(iii)                             ! last non-blank character
          if(matchw(values(iii)(:iiie),values(ii)(:iie)))then ! see if match
             fval=TRUE                                 ! string matched wild-card expression
          else
             fval=FALSE                                ! string did not match wild-card expression
          endif
       else                                            ! parameters were not strings
          fval=(-1)
          mssge='*matchw* parameters must be strings'
          ier=-1
       endif
!=======================================================================------------------------------------------------------------
case("c");      fval= c(args,n)                            !c(curve_number) or c(curve_number,index)
!=======================================================================------------------------------------------------------------
case("ownmode")                                              ! specify whether to look for juown1 routine
      if(n.eq.1.and.iargs_type(1).eq.0)then
         if(args(1).gt.0)then
            ownon=.true.
         else
            ownon=.false.
         endif
         fval= args(1)
      else
        mssge='*ownmode* illegal arguments'
        ier=-1
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("delimx")                ! 'delimx(istore,line,delimiters)  parse a string into $x array and return number of tokens
      if(n.ne.3)then                                                                     ! wrong number of parameters
         ier=-1
         mssge='incorrect number of parameters for '//wstrng(:iend)
      else
         if(iargs_type(2).ne.2)then
            mssge='*delimx* second parameter not a string'
           ier=-1
         else
            ctmp=values(int(args(2)+0.5))                                                ! string to parse
            if(iargs_type(3).ne.2)then
               mssge='*delimx* delimiter parameter not a string'
              ier=-1
            else
               ctmp2=values(int(args(3)+0.5))                                            ! delimiters
               if(iargs_type(1).ne.0)then
                  mssge='*delimx* first parameter not an index number'
                 ier=-1
               else
                  istore=int(args(1)+0.5)                                                ! where to start storing into $n array at
                  call delim(ctmp,['#NULL#'],ixyc_calc,icount,ibegin,iterm,ilen,ctmp2)
                  if(istore.lt.1.or.istore+n-2.gt.ixyc_calc)then  ! ignore entire function call if bad subscript reference was made
                     mssge='illegal subscript value for array in delim'
                     ier=-1
                  else
                     do i1060=1,icount
                        xc(istore)=ctmp(ibegin(i1060):iterm(i1060))
                        istore=istore+1
                     enddo
                     fval=icount  ! return number of tokens found
                     ier=0
                  endif
               endif
            endif
         endif
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$change")                                                         ! $change(instring,'c/oldstring/newstring/')
      ier=2                                                             ! string will be returned
      if(n.ne.2)then                                                    ! wrong number of parameters
         ctmp=' '
         ier=-1
         mssge='*$change* must have two parameters'
      elseif(iargs_type(1).ne.2.or.iargs_type(2).ne.2)then              ! parameters not a string
         ctmp=' '
         ier=-1
         mssge='*$change* parameter(s) not a string'
      else                                                              ! correct numer of type of parameters
         ii=int(args(1)+0.5)                                            ! where directive is stored in values()
         jj=int(args(2)+0.5)                                            ! where directive is stored in values()
         ctmp=values(ii)                                                ! string to change
         call change(ctmp,values(jj),istat)                             ! apply change
         if(istat.lt.0)then                                             ! error occurred
            ctmp=values(ii)                                             ! where string is stored in values()
            mssge='*change* bad directive string'
            ier=-1
         endif
         iend=len_trim(ctmp)                                            ! find length of changed string (very inefficient)
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("round")
      if(n.ne.2)then                                                           ! check number of parameters
        mssge='*jufuns* incorrect number of parameters in '//wstrng2(:iend)
        ier=-1
      elseif(.not.all(iargs_type(1:2).eq.0))then                                ! check type of parameters
        mssge='*jufuns* parameters not all numeric in '//wstrng2(:iend)
        ier=-1
      else                                                                      ! single numeric argument
         fval=round(args(1),int(args(2)))
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("ifdef")
      fval=-1
      if(n.ne.1)then                                            ! check number of parameters
         mssge='*ifdef* incorrect number of parameters in '//wstrng(:iend)
         ier=-1
      elseif(iargs_type(1).ne.2)then                            ! the parameter should be a name of a variable
         mssge='*ifdef* name not a string:'//wstrng(:iend)
         ier=-1
      else
         ii=int(args(1))                                        ! get index into values() array
         iend1=valuer(ii)                                       ! maximum end is at end of string
         if(values(ii)(1:1).eq.'$')then
            call jubous(values(ii)(:iend1),indexout,ix2,ierr)   ! determine if the string variable name exists
         else
            call jubous(values(ii)(:iend1),indexout,ix,ierr)    ! determine f the numeric variable name exists
         endif
         if(ierr.ne.0)then                                      ! unexpected error
            ier=-1
         elseif(indexout.gt.0)then                              ! found variable name
            fval=0
         else                                                   ! did not find variable name
            fval=-1
         endif
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("ye","mo","da","ho","mi","se","year","month","day","hour","minute","second","dw","ju")
         icalen=1                                                              ! default value that is safe even if an error occurs
         !------------------------------------------
         call date_and_time(values=idarray)
         !------------------------------------------
         if(n.eq.0)then
            select case(wstrng2(:iend))                                           ! select desired subscript of value to return
               case("ye","year");     icalen=1                                    ! year
               case("mo","month");    icalen=2                                    ! month
               case("da","day");      icalen=3                                    ! day
               case("dw");            icalen=4                                    ! days since Sunday [ 0-6]
                  call day_of_week(idarray,iweekday,day,ierr)
                  idarray(4)=iweekday
               case("ho","hour");     icalen=5                                    ! hour
               case("mi","minute");   icalen=6                                    ! minute
               case("se","second");   icalen=7                                    ! second
               case("ju");            icalen=8                                    ! days since January 1 [0-365]
                  idarray(8)=d2o(idarray)
               case default                                                       ! report internal error if name was not matched
                  ier=-1
                  mssge='*calendar* internal error, unknown keyword'//wstrng2(:iend)
               end select
            if(ier.eq.0)then                                                      ! if error flag not set set return value
               fval=idarray(icalen)
            else                                                                  ! error has occurred, set default return value
               fval=0.0d0
            endif
         else
            ier=-1
            mssge='*calendar* parameters not allowed'
         endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$mo")                             ! $mo(1-12) is "January, February, ... ")
      ctmp=''
      ier=2                             ! string will be returned
      if(n.lt.1)then                    ! $mo() use today
         call date_and_time(values=idarray)
         ival=idarray(2)
      elseif(n.eq.1)then                ! $mo(N) just index into month names
         ival=mod(int(args(1))-1,12)+1
         if(ival.le.0)ival=ival+12
      elseif(args(2).eq.1)then          ! $mo(YYYYMMDD,1) returns MM
         ival=int(args(1))
         ival=ival-((ival/10000)*10000) ! reduce to a four-digit value
         ival=ival/100                  ! keep two high digits out of the four
         ival=mod(ival-1,12)+1          ! ensure in range 1 to 12
         if(ival.le.0)ival=ival+12
      else
         ival=1
         ctmp='UNKNOWN'
         iend=7
         mssge='*$mo* parameter(s) not valid'
         ier=-1
      endif
      if(ctmp.eq.'')then
         ctmp=months(ival)
      endif
      iend=len_trim(ctmp(1:20))
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$dw")                                             ! $dw(1-7) is "Sunday, Monday, ....")
      ctmp=''
      ier=2                                             ! string will be returned
      if(n.lt.1)then                                    ! if no parameter give day of week of current date
         call date_and_time(values=idat)
         call day_of_week(idat,iweekday,day,ierr)
         ctmp=day
      elseif(n.eq.1)then                                ! just index into days of week array
         ival=mod(int(args(1))-1,7)+1
         if(ival.le.0)ival=ival+7
         ctmp=days(ival)
      else
         ctmp='UNKNOWN'
         mssge='*$dw* parameter(s) not valid'
         ier=-1
      endif
      iend=len_trim(ctmp(1:20))
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
case("$now")                                          ! $now(format)
      ctmp=''
      ier=2                                           ! string will be returned
      select case(n)
      case(0)                                         ! no parameters
         ctmp=now()
      case(1)                                         ! one parameter
         if(iargs_type(1).eq.2)then                   ! a string is assumed to be a format
            ii=int(args(1))                           ! index of first string
            ctmp=now(values(ii)(:valuer(ii)))
         else
            ctmp=''
            mssge='*$now* first parameter is numeric'
            ier=-1
         endif
      case default
         ctmp=''
         mssge='*$now* too many parameter(s)'
         ier=-1
      end select
      iend=len_trim(ctmp)
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
case default
      if(ownon)then
!        ==>wstrng(1:iend)=procedure name.
!        ==>iend=length of procedure name.
!        ==>args=array of ixy_calc elements containing procedure arguments.
!        ==>iargs_type=type of argument
!        ==>n=integer number of parameters
!        ==>x=array of ixy_calc x values
!        ==>y=array of ixy_calc y values
!        ==>ctmp is returned string if string function is called ( in which case fval is returned number of characters in ctmp)
         ier=0
         call juown1(wstrng(:iend),iend,args,iargs_type,n,fval,ctmp,ier)
!        <==fval=returned value to replace function call with
!        <=>ier=returned error flag.  Set to -1 if an error occurs.  Otherwise, user should leave it alone
         if(ier.eq.-1)then
         elseif(ier.eq.2)then
            iend=int(fval) ! string functions should return string length in fval
            if(fval.le.0)then
               mssge='*jufuns* bad length for returned string'
               ier=-1
            endif
         else
            ier=0
         endif
      else
        mssge='*jufuns* function not found: '//wstrng(:iend)
        ier=-1                           ! ya done blown it if you get here
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end select
!-----------------------------------------------------------------------------------------------------------------------------------
999   continue             ! return based on value of ier
      select case(ier)
      case(2)   ! return string value
        call stufftok(fval,wstrng,nchars,ctmp(:iend),iend,ier) ! add a new token variable and assign string to it
      case(0)   ! return numeric value
        call jurtoa(fval,wstrng,nchars,idum)
      case(-1)  ! return error
      case default
         call journal('sc','*jufuns* unknown closing value ',ier)
         ier=-1
      end select
end subroutine jufuns
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine stufftok(fval,wstrng,nchars,string,iend,ier)
implicit doubleprecision (a-h,o-z)
character(len=*),parameter :: ident=&
&"@(#)M_calculator::stufftok(3fp): add a new token variable and assign string to it"
      character(len=5) :: toknam
      character(len=*) :: string
      character(len=*) :: wstrng
!-----------------------------------------------------------------------------------------------------------------------------------
      ktoken=ktoken+1                           ! increment the counter of strings found to get a place to store into
      nchars=5
      write(toknam,'(''$_'',i3.3)')ktoken       ! build a unique name for the token string found for this output string
      wstrng=toknam
      call stuffa(toknam,string(:iend),ival,'')  ! cannot do this earlier or indexs from call that defined args could be wrong
      if(ival.gt.0)then
         fval=ival
         ier=2
         mssge=string(:iend)
      else
         mssge='*stufftok* could not store concatenated string'
         ier=-1
      endif
end subroutine stufftok
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine juargs(line,ilen,array,itype,iarray,ier,mx)
!-----------------------------------------------------------------------------------------------------------------------------------
implicit doubleprecision (a-h,o-z)
character(len=*),parameter :: ident=&
&"@(#)M_calculator::juargs(3fp):given 'par1,par2,...' store  non-parenthesized expression par(n) into a real or string array"
!@ (#) record type of par(n) into itype()"
!@ (#) Commas are only legal delimiters. extra or redundant delimiters are ignored.
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)           :: line      ! input string
integer,intent(in)                    :: ilen      ! length of input string
doubleprecision,intent(out)           :: array(mx)
integer,intent(out)                   :: itype(mx) ! itype=0 for number, itype=2 for string
integer,intent(out)                   :: iarray    ! number of parameters found
integer                               :: ier       ! ier=-1 if error occurs, ier undefined (not changed) if no error.
integer,intent(in)                    :: mx        ! up to mx par(i) will be extracted. if more found an error is generated.
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=1),parameter :: delimc=','
   character(len=icbuf_calc)  :: wstrng
!-----------------------------------------------------------------------------------------------------------------------------------
   iarray=0
   if(ilen.eq.0)then  ! check if input line (line) was totally blank
      return
   endif
!  there is at least one non-delimiter character in the command.
!  ilen is the column position of the last non-blank character
!  find next non-delimiter
   icol=1
   do ilook=1,mx,1
     do
        if(line(icol:icol).ne.delimc)then
           iarray=iarray+1
           istart=icol
           iend=index(line(istart:ilen),delimc)
           if(iend.eq.0)then   ! no delimiter left
              icalc=ilen-istart+1
              wstrng=line(istart:ilen)
              ier=0
              call jucals(wstrng,icalc,array(iarray),ier)
              itype(iarray)=ier
              return
           else
              iend=iend+istart-2
              icalc=iend-istart+1
              wstrng=line(istart:iend)
              ier=0
              call jucals(wstrng,icalc,array(iarray),ier)
              itype(iarray)=ier
              if(ier.eq.-1)return
           endif
           icol=iend+2
           exit
        else
           icol=icol+1
           if(icol.gt.ilen)return       ! last character in line was a delimiter, so no text left
        endif
     enddo
     if(icol.gt.ilen)return       ! last character in line was a delimiter, so no text left
   enddo
   write(mssge,'(a,i4,a)')'more than ',mx,' arguments not allowed'
   ier=-1
end subroutine juargs
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine jucals(string,nchar,value,ier)
implicit doubleprecision (a-h,o-z)
character(len=*),parameter :: ident=&
&"@(#)M_calculator::jucals(3fp): resolve a series of terms into a single value and restring"
      character(len=*)          :: string
      character(len=icbuf_calc) :: dummy            ! no single term may be over (icbuf_calc) characters
!-----------------------------------------------------------------------------------------------------------------------------------
                               !!!!! what happens if the returned string is longer than the input string?
      value=0.0d0              ! initialize sum value to be returned to 0
      if(nchar.eq.0) return    ! if this is a null string return
                 ! first cut at handling string variables. assuming, with little checking, that the only string expression
                 ! that can get here is a single variable name (or variable token) and that string variable names start with a $
                 ! and that the error flag should be set to the value 2 to indicate that a string, not a number, is being returned
                 ! for the 2 to get back, it must not be changed by this routine or anything it calls
      if(string(1:1).eq.'$')then
         call given_name_get_stringvalue(string,ier)
         if(ier.eq.-1)return
         ier=2                 ! flag that a character string is being returned
!x       return
      endif
!x!!!!!
      ista=1                                        ! initialize the position of the unary sum operator for the current term
      if(index('#=',string(1:1)).ne.0)then          ! check if input string starts with a unary (+-) operator
        istat=2                                     ! a starting unary sum operator is present, so the first term starts in column 2
      else                                          ! input string does not start with a unary sum (-+) operator
        istat=1                                     ! no initial sum operator is present, so the first term starts in column 1
      endif
      do
         iendp=index(string(istat:nchar),'#')     ! find left-most addition operator
         iendm=index(string(istat:nchar),'=')     ! find left-most subtraction operator
         iend=min(iendp,iendm)                    ! find left-most sum (+-) operator assuming at least one of each exists
         if(iend.eq.0)iend=max(iendm,iendp)       ! if one of the sum operators is not remaining, find left-most of remaining type
         if(iend.eq.0)then                        ! if no more sum operators remain this is the last remaining term
            iend=nchar                            ! find end character of remaining term
         else                                     ! more than one term remains
            iend=iend+istat-2                     ! find end character position of this (left-most) term
         endif
         dummy=string(istat:iend)                 ! set string dummy to current(left-most) term
         nchar2=iend-istat+1                      ! calculate number of characters in current term
!        given that the current term ( dummy) is an optionally signed string containing only the operators **, * an / and no
!        parenthesis, reduce the string to a single value and add it to the sum of terms (value). do not change the input string.
         call jupows(dummy,nchar2,ier)            ! evaluate and remove ** operators and return the altered string (dummy)
         if(ier.eq.-1) return                     ! if an error occurred, return
         call jufacs(dummy,nchar2,temp,ier)       ! evaluate and remove * and / operators, return the evaluated -value- temp
         if(ier.eq.-1)return                      ! if an error occurred, return
         if(string(ista:ista).eq.'=')then         ! if term operator was a subtraction, subtract temp from value
            value=value-temp
         else                                     ! operator was an addition (+) , add temp to value
!           if first term was not signed, then first character will not be a subtraction, so addition is implied
            value=value+temp
         endif
         ista=iend+1                      ! calculate where next sum operator (assuming there is one) will be positioned in (string)
         istat=ista+1                     ! calculate where beginning character of next term will be (if another term remains)
         if(iend.ne.nchar)then
            if(istat.gt.nchar)then                ! a trailing sum operation on end of string
               ier=-1
               mssge='trailing sum operator'
               return
            endif
         ! if last term was not the end of (string) terms remain. keep summing terms
         else
            exit
         endif
      enddo
      call jurtoa(value,string,nchar,ier) ! successfully completed. convert sum of terms (value) to a string and return
end subroutine jucals
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine jupows(wstrng,nchar,ier)
implicit doubleprecision (a-h,o-z)
character(len=*),parameter :: ident=&
&"@(#)M_calculator::jupows(3fp): expand power functions in a string, working from left to right"
!
!     given an unparenthesized string of form:
!        stringo opo fval1 ** fval2 opo2 stringo2
!     where opo is a preceding optional operator from set /,* and
!     stringo is the string that would precede opo when it exists,
!     and opo2 is an optional trailing operator from set /,*,**
!     and stringo2 the string that would follow op2 when it exists,
!     evaluate the expression fval1**fval2 and restring it; repeating
!     from left to right until no power operators remain in the string
!     or an error occurs
!
!     ip     =position of beginning of first ** operator
!     iz     =beginning of fval1 string
!     iright =end of fval2 string
!     wstrng=input string returned with power operators evaluated
!     nchar  =input length of wstrng, returned corrected for new
!             wstrng returned.
!
      character(len=icname_calc) :: tempch
      character(len=icbuf_calc)  :: dummy
      character(len=1)           :: z
      character(len=*)           :: wstrng
!-----------------------------------------------------------------------------------------------------------------------------------
      INFINITE: do
!        find first occurrence of operator, starting at left and moving right
         ip=index(wstrng(:nchar),'**')
         if(ip.eq.0) then
            exit INFINITE
         elseif(ip.eq.1) then
            ier=-1
            mssge='power function "**" missing exponentiate'
            exit INFINITE
         elseif((ip+2).gt.nchar) then
            ier=-1
            mssge='power function "**" missing power'
            exit INFINITE
         endif
!
!        find beginning of fval1 for this operator. go back to
!        beginning of string or to any previous * or / operator
         FINDVAL: do i=ip-1,1,-1
            iz=i
            z=wstrng(i:i)
               if(index('*/',z).ne.0)then  ! note that use of index function was faster than .eq. on cyber
                  iz=iz+1
                  goto 11
               endif
         enddo FINDVAL
         iz=1
11       continue
         if(ip-iz.eq.0)then
           ier=-1
           mssge='operator / is beside operator **'
           exit INFINITE
         endif
!
!        now isolate beginning and end of fval2 string for current operator
!        note that looking for * also looks for ** operator, so checking
!        for * or / or ** to right
!
         im2=index(wstrng((ip+2):nchar),'*')
         id2=index(wstrng((ip+2):nchar),'/')
         ip2=min0(im2,id2)
         if(ip2.eq.0)ip2=max0(im2,id2)
         if(ip2.eq.0)then
           iright=nchar
         elseif(ip2.eq.1)then
           ier=-1
           mssge='two operators from set [*/**] are side by side'
           exit INFINITE
         else
           iright=ip2+ip
         endif
         call juator(wstrng(iz:ip-1),fval1,ier)
         if(ier.eq.-1)then
            exit INFINITE
         endif
         call juator(wstrng(ip+2:iright),fval2,ier)
         if(ier.eq.-1)then
            exit INFINITE
         endif
         if(fval1.lt.0.0d0)then
!           this form better/safe? if(abs( fval2-int(fval2)/fval2).le..0001)
            if(fval2-int(fval2).eq.0.0d0)then
               fval1=fval1**int(fval2)
            else
               mssge='negative to the real power not allowed'
               ier=-1
               exit INFINITE
            endif
         else
            fval1=fval1**fval2
         endif
         call jurtoa(fval1,tempch,nchart,idum)
!        place new value back into string and correct nchar.
!        note that not checking for nchar greater than (icbuf_calc)
!        in dummy or greater than len(wstrng).
         if(iz.eq.1.and.iright.eq.nchar)then      ! there was only one operator and routine is done
            dummy=tempch(1:nchart)
            nchar=nchart
         else if(iz.eq.1)then                     ! iz was 1, but iright was nchar so
            dummy=tempch(1:nchart)//wstrng(iright+1:nchar)
            nchar=nchart+nchar-(iright+1)+1
         else if(iright.eq.nchar)then             ! iz was not 1, but iright was nchar so
            dummy=wstrng(1:iz-1)//tempch(1:nchart)
            nchar=(iz-1)+nchart
         else                                     ! iz was not 1, and iright was not nchar so
            dummy=wstrng(1:iz-1)//tempch(1:nchart)//wstrng(iright+1:nchar)
            nchar=(iz-1)+nchart+(nchar-(iright+1)+1)
         endif
         wstrng=dummy
      enddo INFINITE
end subroutine jupows
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine jufacs(wstrng,nchr,fval1,ier)
implicit doubleprecision (a-h,o-z)
character(len=*),parameter :: ident=&
&"@(#)M_calculator::jufacs(3fp):reduce unparenthesized string with only * and / operators to val"
!
!     The input string is unaltered. for any single pass thru the routine, the string structure is assumed to be:
!         fval1 op fval2 op fval op fval op fval op fval
!     where no blanks are in the string (only significant if string structure is bad) and the only operators are * or /.
!     working from left to right:
!       1. locate and place into a real variable the fval1 string
!       2. if one exists, locate and place into a real variable the fval2 string
!       3. perform the indicated operation between fval1 and fval2
!          and store into fval1.
!       3. repeat steps 2 thru 4 until no operators are left or
!          an error occurs.
!
!     nchr   = the position of the last non-blank character in the input string wstrng
!     ip     = the position of the current operator to be used.
!              to the left of this is the fval1 string.
!     iright = the position of the last character in the fval2 string.
!     wstrng = the input string to be interpreted.
!       ier  = is a flag indicating whether an error has occurred
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*) :: wstrng
!-----------------------------------------------------------------------------------------------------------------------------------
      if((nchr).eq.0)then
         ier=-1
         mssge='trying to add/subtract a null string'
         return
      endif
!     find position of first operator
      im=index(wstrng(:nchr),'*')
      id=index(wstrng(:nchr),'/')
!     ip should be the position of the left-most operator
      ip=min0(im,id)
!     if one or both of the operators were not present, then
!     either im or id (or both) are zero, so look for max
!     instead of min for ip
      if(ip.eq.0) ip=max0(im,id)
      if( ip.eq.0 )then
!        no operator character (/ or *) left
         call juator(wstrng(1:nchr),fval1,ier)
         return
      elseif (ip.eq.1)then
!        if no string to left of operator, have a bad input string
         ier=-1
         mssge='first factor or quotient for "*" or "/" missing or null'
         return
      endif
!     convert located string for fval1 into real variable fval1
      call juator(wstrng(1:ip-1),fval1,ier)
      if(ier.eq.-1)return
      do
         if(ip.eq.nchr)then
!          if no string to left of operator, have a bad input string
           ier=-1
           mssge='second factor or quotient for "*" or "/" missing or null'
           return
         endif
!        locate string to put into fval2 for current operator by starting just to right of operator and ending at end of current
!        string or at next operator note that because of previous checks we know there is something to right of the operator.
         im2=index(wstrng((ip+1):nchr),'*')
         id2=index(wstrng((ip+1):nchr),'/')
         ip2=min0(im2,id2)
         if(ip2.eq.0)ip2=max0(im2,id2)
         if(ip2.eq.0)then
            iright=nchr
         elseif(ip2.eq.1)then
            ier=-1
            mssge='two operators from set [*/] are side by side'
            return
         else
            iright=ip2+ip-1
         endif
!        place located string for fval2 into real variable fval2
         call juator(wstrng(ip+1:iright),fval2,ier)
         if(ier.eq.-1)return
!        do specified operation between fval1 and fval2
         if(wstrng(ip:ip).eq.'*') then
            fval1=fval1*fval2
         else if(fval2.eq.0) then
            ier=-1
            mssge='division by zero'
            return
         else
            fval1=fval1/fval2
         endif
         if(iright.eq.nchr)return
         ip=iright+1
      enddo
end subroutine jufacs
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     juator(3f) - [M_calculator]returns a double precision value from a numeric character string specifically for M_calculator(3fm)
!!##SYNOPSIS
!!
!!   subroutine juator(chars,rval,ierr)
!!
!!    character(len=*),intent(in) :: chars
!!    doubleprecision,intent(out) :: rval
!!    integer,intent(out)         :: ierr
!!
!!##DESCRIPTION
!!    Convert a string representing a numeric scalar value to a numeric value, specifically
!!    for the M_calculator(3fp) module.Works with any g-format input, including integer, real, and exponential forms.
!!
!!       1. if chars=? set rval to value stored as current value, return.
!!       2. if the string starts with a $ assume it is the name of a
!!          string variable or token and return its location as a doubleprecision number.
!!       3. try to read string into a doubleprecision value. if successful, return.
!!       4. if not interpretable as a doubleprecision value, see if it is a
!!          defined variable name and use that name's value if it is.
!!       5. if no value can be associated to the string and/or if
!!          an unexpected error has occurred, set error flag and
!!          error message and set rval to zero and return.
!!       6. note that blanks are treated as null, not zero.
!!##OPTIONS
!!      chars is the input string
!!      rval  is the doubleprecision output value
!!      ierr  0 if no error occurs
!!
!!##EXAMPLE
!!
!!
!!##VERSION
!!       o 07/15/1986  J. S. Urban
!!       o 12/28/1987  modified to specify bn in formats for reads. vax
!!                    defaults to zero-fill on internal files.
!!       o 12/22/2016  Changed to generate man(1) pages via ufpp(1).
!===================================================================================================================================
subroutine juator(chars,rval8,ierr)
implicit doubleprecision (a-h,o-z)
character(len=*),parameter  :: ident=&
&"@(#)M_calculator::juator(3f):returns a real value rval8 from a numeric character string chars."
character(len=*),intent(in) :: chars ! CAREFUL: LAST is in GLOBAL, but can be read from when passed to this routine as CHARS. DO NOT CHANGE CHARS.
doubleprecision,intent(out) :: rval8
integer,intent(out)         :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=13)        :: frmt
!-----------------------------------------------------------------------------------------------------------------------------------
   ioerr=0
   if(chars.eq.'?')then       ! if string is a (unsigned) question mark, use value returned from last completed calculation
     read(last,'(bn,g20.0)',iostat=ioerr,err=9991)rval8                ! assuming cannot get a read error out of reading last
   elseif('$'.eq.chars(1:1))then                ! string is a string variable name
      call jubous(chars,indx,ix2,ier)           ! try to find the index in the character array for the string variable
      if(indx.le.0)then                         ! if indx is not .gt. 0 string was not a variable name
         ierr=-1
      mssge='undeclared string variable '//chars(:min(len(chars),(icname_calc)))
      else
         rval8=real(indx)   ! set value to position of string in the string array
         !!!! flag via a value for ierr that a string, not a number, has been found
      endif
      return
   ! no error on read on Sun on character string as a number, so make sure first character not numeric and try as variable name
   elseif(index('0123456789.-+',chars(1:1)).eq.0)then   ! does not start with a numeric character. try as a variable name
      call jubous(chars,indx,ix,ier)
      if(indx.le.0)then                                 ! if indx is not .gt. 0 string was not a variable name
         ierr=-1
       mssge='undeclared variable '//chars(:min(len(chars),(icname_calc)))
      else
         rval8=value(indx)
      endif
      return
   else                            ! string is a number or a numeric variable name that starts with a numeric character
     write(frmt,101)len(chars)                       ! build a format statement to try and read the string as a number with
101  format( '(bn,g',i5,'.0)' )
     read(chars,fmt=frmt,iostat=ioerr,err=999)rval8   ! try and read the string as a number
   endif
   return                             ! string has successfully been converted to a number
9991  continue                           ! string could not be read as number,so try as variable name that starts with number
999   continue                           ! string could not be read as number,so try as variable name that starts with number
   rval8=0.0d0
   indx=0
   !  either here because of a read error (too big, too small, bad characters in string) or this is a variable name
   !  or otherwise unreadable.
   !!!!! look carefully at what happens with a possible null string
   call jubous(chars,indx,ix,ier)
   if(indx.le.0)then                             ! if indx is not .gt. 0 string was not a variable name
      mssge='bad variable name or unusable value = '//chars
      ierr=-1
   else
      rval8=value(indx)
   endif
end subroutine juator
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine jurtoa(rval,chars,ilen,ierr)
implicit doubleprecision (a-h,o-z)
character(len=*),parameter :: ident=&
&"@(#)M_calculator::jurtoa(3fp): returns a numeric character string from a real value."
!
!  o  left-justified with number of characters counted
!  o  trailing zeros removed
!  o  if an error occurs in the write, numeric character string is set
!     to '0.0' and ierr is set to -1
!     do not use with len(chars) less than 20
!  o  if somehow fall thru loop (blank string or non-numeric string)
!     ierr=-1
!     uses g format for output. if a number is output under the
!     gw.d specification without an exponent, four spaces are
!     inserted to the right of the field (these spaces are reserved
!     for the exponent field e+xx).
!
!     03/16/87 J. S. Urban
!
      character(len=*)    :: chars
      character(len=20)   :: dummy
!-----------------------------------------------------------------------------------------------------------------------------------
      ioerr=0
      chars=' '
!     note that output is forced to far right of string
!     (d=13,e=3,d+e+6=13+3+4=20)
      write(dummy,fmt='(g20.13e3)',iostat=ioerr,err=999)rval
      iepos=index(dummy,'e')
      iepos2=index(dummy,'E')
      if(iepos.eq.0)iepos=iepos2
      if(iepos.eq.0)then
         !=======================================================================
         !     written with f-format.
         !     remove trailing zeros and left-justify string and find it's
         !     length . note that, written with the g format, output
         !     should always contain a decimal place, so don't have to
         !     special case a string of all zeros.
         !
         do i10=20,1,-1
            if(dummy(i10:i10).ne.'0'.and.dummy(i10:i10).ne.' ')then
              iend=i10
              do i20=iend-1,1,-1
                 if(dummy(i20:i20).eq.' ')then
                 istart=i20+1
                 ilen=iend-istart+1
                 chars(1:ilen)=dummy(istart:iend)
                 return
                 endif
              enddo
              ilen=iend ! chars is completely filled
              chars(1:ilen)=dummy(1:iend)
              return
            else
            endif
         enddo
         !     error has occurred if fall out of loop instead of returning
         !     if error was not do to write, ioerr is zero, but ierr will
         !     still return as -1
      else
!=======================================================================------------------------------------------------------------
!     written with e-format.
!
      iende=iepos
!     find last non-blank character in e+xx field
      do i50=20,iepos+1,-1
         if(dummy(i50:i50).ne.' ')then
            iende=i50
            exit
         endif
      enddo
      do i30=iepos-1,1,-1
         if(dummy(i30:i30).ne.'0'.and.dummy(i30:i30).ne.' ')then
            iend=i30
            do i40=iend-1,1,-1
               if(dummy(i40:i40).eq.' ')then
                  istart=i40+1
                  ilen=(iend-istart+1)+(iende-iepos+1)
                  chars(1:ilen)=dummy(istart:iend)//dummy(iepos:iende)
                  return
               endif
            enddo
            ilen=iend+(iende-iepos+1) ! chars is completely filled
            chars(1:ilen)=dummy(1:iend)//dummy(iepos:iende)
            return
         else
         endif
      enddo
      endif
!     error has occurred if fall out of loop instead of returning
!     if error was not do to write, ioerr is zero, but ierr will
!     still return as -1
999   continue
      chars='0.0'
      mssge='cannot represent value using (g20.13e3) format '
      ierr=-1
end subroutine jurtoa
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    jusqes - [M_calculator]change +-[] to #=(),replace strings with placeholders,delete comments
!!
!!##DESCRIPTION
!!    remove all blanks from input string and return position of last non-blank character in nchars using imax as the highest
!!    column number to search in.  return a zero in nchars if the string is blank.
!!
!!    replace all + and - characters with the # and = characters which will be used to designate + and - operators, as opposed to
!!    value signs.
!!
!!    replace [] with ()
!!
!!    remove all strings from input string and replace them with string tokens and store the values for the string tokens.
!!    assumes character strings are (iclen_calc) characters max.
!!    if string is delimited with double quotes, the double quote character may be represented inside the string by
!!    putting two double quotes beside each other ("he said ""greetings"", i think" ==> he said "greetings", i think)
!!
!!  !!!! if an equal sign is followed by a colon the remainder of the input line is placed into a string as-is
!!  !!!! without the need for delimiting it. ($string1=: he said "greetings", i think ==> he said "greetings", i think)
!!
!!    anything past an # is considered a comment and ignored
!!
!!    assumes length of input string is less than (icbuf_calc) characters
!!
!!    if encounters more than one equal sign, uses right-most as the
!!    end of variable name and replaces others with & and makes a
!!    variable name out of it (ie a=b=10 ===> a&b=10)
!!
!!  !!!!the length of string could actually be increased by converting quoted strings to tokens
!!
!!  !!!!maybe change this to allow it or flag multiple equal signs?
!!
!!  !!!!no check if varnam is a number or composed of characters
!!  !!!!like ()+-*/. . maybe only allow a-z with optional numeric
!!  !!!!suffix and underline character?
!!
!!  !!!!variable names ending in letter e can be confused with
!!  !!!!e-format numbers (is 2e+20 the variable 2e plus 20 or
!!  !!!!the single number 200000000000000000000?). to reduce
!!  !!!!amount of resources used to check for this, and since
!!  !!!!words ending in e are so common, will assume + and -
!!  !!!!following an e are part of an e-format number if the
!!  !!!!character before the e is a period or digit (.0123456789).
!!  !!!!and won't allow variable names of digit-e format).
!!
!!  !!!!make sure variable called e and numbers like e+3 or .e+3 are handled satisfactorily
!===================================================================================================================================
subroutine jusqes(string,imax,nchars,varnam,nchar2,ier)
implicit doubleprecision (a-h,o-z)
character(len=*),parameter :: ident=&
&"@(#)M_calculator::jusqes(3fp):change +-[] to #=(),replace strings with placeholders,delete comments."
   integer, parameter          :: ilen=(icbuf_calc)+2

   character(len=*)            :: string
   character(len=ilen)         :: dummy
   character(len=1)            :: back1
   character(len=1)            :: back2

   character(len=1)            :: currnt
   character(len=icname_calc)  :: varnam
   character(len=iclen_calc)   :: ctoken

   character(len=10),parameter :: list  =' +-="#[]{}'  ! list of special characters
   character(len=10),parameter :: list2 =' #=&  ()()'  ! list of what to convert special characters too when appropriate
   character(len=5)            :: toknam
!-----------------------------------------------------------------------------------------------------------------------------------
!  keep track of previous 2 non-blank characters in dummy for when trying to distinguish between e-format numbers
!  and variables ending in e.
   back1=' '
   back2=' '
   varnam=' '                   ! initialize output variable name to a blank string
   ivar=0
   nchar2=0
   nchars=0                     ! the position of the last non-blank character in the output string (string)
   dummy(1:2)='  '
!-----------------------------------------------------------------------------------------------------------------------------------
!  instead of just copy string to buffer, cut out rows of sign operators
!  dummy(3:)=string
   idum=3
   instring=0
   do i10=1,len(string)
      ! if adjacent sign characters skip new character and maybe change sign of previous character
      if(string(i10:i10).eq.'"'.and.instring.eq.0 )then   ! starting a string
         instring=1
      elseif(string(i10:i10).eq.'"'.and.instring.eq.1)then ! ending a string
         instring=0
      endif
      if(instring.ne.1)then
         if(string(i10:i10).eq.'+')then                 ! if found a + look to see if previous a + or -
            if(dummy(idum-1:idum-1).eq.'+')then         ! last character stored was also a sign (it was +)
               cycle                                    ! skip because ++ in a row
            elseif(dummy(idum-1:idum-1).eq.'-')then     ! skip -+ and just leave -
               cycle
            endif
         elseif(string(i10:i10).eq.'-')then             ! last character stored was also a sign (it was -)
            if(dummy(idum-1:idum-1).eq.'+')then         ! +- in a row
               dummy(idum-1:idum-1)='-'                 ! change sign of previous plus
               cycle                                    ! skip because +- in a row
            elseif(dummy(idum-1:idum-1).eq.'-')then     ! skip but change sign of previous
               dummy(idum-1:idum-1)='+'                 ! change -- to +
               cycle
            endif
         endif
      endif
      ! character not skipped
      dummy(idum:idum)=string(i10:i10)            ! simple copy of character
      idum=idum+1
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   string=' '
   ipoint=2                                       ! ipoint is the current character pointer for (dummy)
   ktoken=0                                       ! initialize the number of strings found in this string
   BIG: do ilook=1,imax
      ipoint=ipoint+1                             ! move current character pointer forward
      currnt=dummy(ipoint:ipoint)                 ! store current character into currnt
      select case(currnt)            ! check to see if current character has special meaning and requires processing ' +-="#[]{}'
!-----------------------------------------------------------------------------------------------------------------------------------
      case(" ")                                   ! current is a blank not in a string. ignore it
         cycle BIG
!-----------------------------------------------------------------------------------------------------------------------------------
      case("+")                                      ! current is a plus
         if(back1.eq.'e'.or.back1.eq.'E')then        ! if previous letter was an e it could be e-format sign or operator.
!           note not using dummy directly, as it may contain blanks letter before +- was an e. must decide if the +- is part of
!           an e-format number or intended to be the last character of a variable name.
!!!!!       what is effect on a---b or other +- combinations?
            ! if letter before e is not numeric this is a variable name and - is an operator
            if(index('0123456789.',back2).eq.0)then
              currnt="#"                        ! no digit before the e, so the e is the end of a variable name
            else                                ! digit before e, so assume this is number and do not change +- to #= operators
            endif
         else
            currnt="#"                          ! previous letter was not e, so +- is an operator so change +- to #= operators
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
      case("-")                                      ! current is a minus
         if(back1.eq.'e'.or.back1.eq.'E')then        ! if previous letter was an e it could be e-format sign or operator.
!           note not using dummy directly, as it may contain blanks letter before +- was an e. must decide if the +- is part of
!           an e-format number or intended to be the last character of a variable name.
!!!!!       what is effect on a---b or other +- combinations?
            ! if letter before e is not numeric this is a variable name and - is an operator
            if(index('0123456789.',back2).eq.0)then
              currnt="="                       ! no digit before the e, so the e is the end of a variable name
            else                               ! digit before e, so assume this is number and do not change +- to #= operators
            endif
         else
            currnt="="                         ! previous letter was not e, so +- is an operator so change +- to #= operators
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
      case("=")                                      ! current is a plus or minus
         currnt="&"
         ivar=nchars+1                               ! ivar is the position of an equal sign, if any
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case ("{", "[")
      currnt='('                                     ! currnt is [ or { . Replace with (
      case ("}", "]")
      currnt=')'                                     ! currnt is ] or }, . Replace with )
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case("#")                                      ! any remainder is a comment
         exit
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case('"')                                   ! if character starts a quoted string, extract it and replace it with a token
!     figure out length of string, find matching left double quote, reduce internal "" to "
      kstrln=0                                    ! initialize extracted string length
      ctoken=' '                                  ! initialize extracted string
      do i20 = ipoint+1,imax+2                    ! try to find a matching double quote to the right of the first
         ipoint=ipoint+1
         if(dummy(ipoint:ipoint).eq.'"')then         !!!!! caution : could look at dummy(imax+1:imax+1)
            if(dummy(ipoint+1:ipoint+1).ne.'"')then  ! this is the end of the string
               goto 30
            else                                     ! this is being used to try and represent an internal double-quote
               kstrln=kstrln+1                               ! determine length of string to remove
               ctoken(kstrln:kstrln)=dummy(ipoint:ipoint)    ! store the character into the current string storage
               ipoint=ipoint+1
            endif
         else                                             ! this is an internal character of the current string
            kstrln=kstrln+1                               ! determining length of string to remove
            ctoken(kstrln:kstrln)=dummy(ipoint:ipoint)    ! store the character into the current string storage
         endif
      enddo
      ier=-1                                         ! if you get here an unmatched string delimiter (") has been detected
      mssge='unmatched quotes in a string'
      return
30    continue
!!!!! check that current token string is not over (iclen_calc) characters long . what about the string "" or """" or """ ?
      ktoken=ktoken+1                             ! increment the counter of strings found
      write(toknam,'(''$_'',i3.3)')ktoken         ! build a unique name for the token string found for this input string
      nchars=nchars+1                             ! increment counter of characters stored
      string(nchars:nchars+4)=toknam              ! replace original delimited string with its token
      nchars=nchars+4
!                                                    store the token name and value in the string variable arrays
      call jubous(toknam,indx,ix2,ier)            ! determine storage placement of the variable and whether it is new
      if(ier.eq.-1)return
      if(indx.le.0)then                           ! check if the token name needs added or is already defined
         call juadds(toknam,5,indx,ier)           ! adding the new variable name in the variable name array
         if(ier.eq.-1)return
      endif
      values(iabs(indx))=ctoken(1:max(1,kstrln))  ! store a defined variable's value
      valuer(iabs(indx))=kstrln                   ! store length of string
!!!!! note that reserving variable names starting with $_ for storing character token strings
      cycle BIG
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      case default                                   ! current is not one of the special characters in list
      end select
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
                                                  ! for all but blank characters and strings
      back2=back1
      back1=currnt
      nchars=nchars+1
      string(nchars:nchars)=currnt
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   enddo BIG
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!  end of string or hit beginning of comment

   if(ivar.ne.0)then                   ! check to see if a variable name was defined:
   nchar2=ivar-1                       ! variable was declared nchar2 is the position of the last character in the variable name
     if(nchar2.gt.20)then
      ier=-1
      mssge='new variable names must be 20 characters long or less'
     else if(nchar2.eq.0)then
      ier=-1
      mssge='input starts with =; cannot define a null variable name'
     else                              ! split up variable name and expression
                                       ! legal length variable name
       if(index('eE',string(nchar2:nchar2)).ne.0.and.nchar2.ne.1)then ! could be an unacceptable variable name
           if(index('0123456789',string(nchar2-1:nchar2-1)).ne.0)then
!            an unacceptable variable name if going to avoid conflict with
!            e-format numbers in a relatively straight-forward manner
             mssge='variable names ending in digit-e not allowed'
             ier=-1
           endif
        endif
        dummy=string
        varnam=dummy(1:ivar-1)
        if(nchars.ge.ivar+1)then
           string=dummy(ivar+1:nchars)
        else
           string=' '
        endif
        nchars=nchars-ivar
     endif
   endif
end subroutine jusqes
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!##SYNOPSIS
!!
!!   subroutine jubous(varnam0,index,ixn,ier)
!!
!!    character(len=*),intent(in)           :: varnam0
!!    integer                               :: index
!!    character(len=icname_calc)            :: ixn(ic_calc)
!!    integer,intent(out)                   :: ier
!!##DESCRIPTION
!!    assuming an alphabetized array of character strings, find the location (index) where that name can be found, unless it is not
!!    found -- in which case report where it should be placed as a negative index number.  it is assumed all variable names are
!!    lexically greater than a blank string.
!!
!!    finds the index assigned to a specific variable name.  assumes that the user index array is sorted in descending order
!!    (highest at top).  if varnam is not found; return line number it should be placed at ; with a negative sign.
!!##OPTIONS
!!    VARNAME0  variable name to find the location for
!!    IXN       sorted array of character strings of standard dictionary size
!!##RETURNS
!!    INDEX     location variable name found at (if positive) or location it should be placed at (if negative)
!!    IER       zero if no error occurred
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine jubous(varnam0,index,ixn,ier)
implicit none
character(len=*),parameter :: ident=&
&"@(#)M_calculator::jubous(3fp):locate where entry is/should go in sorted dictionary"
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)      :: varnam0
integer                          :: index
character(len=icname_calc)       :: ixn(ic_calc)
integer,intent(out)              :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=icname_calc) :: varnam
   integer                    :: maxtry
   integer                    :: i10
   integer                    :: imin
   integer                    :: imax
!-----------------------------------------------------------------------------------------------------------------------------------
   ier=0
   varnam=varnam0(:)
   maxtry=int(log(float(ic_calc))/log(2.0d0)+1.0d0)
   index=(ic_calc+1)/2
   imin=1
   imax=ic_calc
   do i10=1,maxtry
      if(varnam.eq.ixn(index))then
         return
      else if(varnam.gt.ixn(index))then
         imax=index-1
      else
         imin=index+1
      endif
      if(imin.gt.imax)then
         index=-imin
         if(iabs(index).gt.ic_calc)then
            mssge='error 03 in jubous'
            ier=-1
            return
         endif
         return
      endif
      index=(imax+imin)/2
      if(index.gt.ic_calc.or.index.le.0)then
         mssge='error 01 in jubous'
         ier=-1
         return
      endif
   enddo
   mssge='error 02 in jubous'
end subroutine jubous
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     juaddr(3fp) - add new variable to numeric value dictionary at specified location
!!
!!##SYNOPSIS
!!
!!   subroutine juaddr(newnam,nchars,index,ier)
!!
!!    character(len=*),intent(in)  :: newnam
!!    integer,intent(in)           :: nchars
!!    integer,intent(in)           :: index
!!    integer                      :: ier
!!
!!##DESCRIPTION
!!    given a new variable name and place to put it, pull down the character and value arrays and initialize the new
!!    variable's value to zero.  variable names only up to (icname_calc) characters maximum.
!===================================================================================================================================
subroutine juaddr(newnam,nchars,index,ier)
implicit none
character(len=*),parameter :: ident=&
&"@(#)M_calculator::juaddr(3fp): add new variable to numeric value dictionary at specified location"
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: newnam
integer,intent(in)           :: nchars
integer,intent(in)           :: index
integer                      :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: istart
   integer                      :: i70
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ix(ic_calc).ne.' ')then
     mssge='*juaddr* no room left on file to add more variable names'
     ier=-1
   elseif(newnam(1:1).eq.'$')then
     mssge='*juaddr* numeric variable names must not start with a $'
     ier=-1
   else
      istart=iabs(index)
      ! watch out when ic_calc approaches istart that logic is correct.
      do i70=ic_calc-1,istart,-1 ! pull down the array to make room for new value
         value(i70+1)=value(i70)
         ix(i70+1)=ix(i70)
      enddo
      value(istart)=0.0d0
      ix(istart)=newnam(1:nchars)
   endif
end subroutine juaddr
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!       given a new string variable name and place to put it, pull down
!!       the character and value arrays and initialize the new
!!       variable's value to a blank string. variable names only up to (icname_calc)
!!       characters maximum. stored strings up to only (iclen_calc) characters long.
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine juadds(newnam,nchars,index,ier)
!-----------------------------------------------------------------------------------------------------------------------------------
!implicit doubleprecision (a-h,o-z)
implicit none
character(len=*),parameter :: ident=&
&"@(#)M_calculator::juadds(3fp): insert new variable name into string dictionary at specified location"
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in) :: newnam  ! new variable name
      integer,intent(in)          :: nchars  ! last non-blank character position in newnam
      integer,intent(in)          :: index   ! position in array to place newnam, calculated by jubous.
      integer                     :: ier     ! error flag set to -1 if an error occurs; otherwise it is left undefined.
!-----------------------------------------------------------------------------------------------------------------------------------
      integer                     :: istart
      integer                     :: i70
!-----------------------------------------------------------------------------------------------------------------------------------
!     GLOBALS
!     ix2    - storage for variable names
!     values - storage for variable string values
!     mssge  - message associated with error flag
!
!     this routine is very similar to juaddr except that the values
!     to be stored are strings instead of real numbers.
!     it is essentially trusting of its input, and does very little
!     checking of input parameters.
!-----------------------------------------------------------------------------------------------------------------------------------
!     if last position in the name array has already been used, then
!     report that no room is left and set error flag and error message.
      if(ix2(ic_calc).ne.' ')then
        mssge='*juadds* no room left to add more string variable names'
        ier=-1
      elseif(newnam(1:1).ne.'$')then
        mssge='*juadds* string variable names must start with a $'
        ier=-1
      else
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=------------------------------------------------------------
         istart=iabs(index)
!        watch out when ic_calc approaches istart that logic is correct.
         do i70=ic_calc-1,istart,-1 ! pull down the array to make room for new value
            values(i70+1)=values(i70)
            valuer(i70+1)=valuer(i70)
            ix2(i70+1)=ix2(i70)
         enddo
         values(istart)=' '
         valuer(istart)=0
         ix2(istart)=newnam(1:nchars)
      endif
end subroutine juadds
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!       [M_calculator]given_name_get_stringvalue(3fp) - return associated value for variable name"
!!##SYNOPSIS
!!
!!   subroutine given_name_get_stringvalue(chars,ierr)
!!
!!    character(len=*),intent(in)  :: chars
!!    integer,intent(out)          :: ierr
!!##DESCRIPTION
!!       return the actual string when given a string variable name or token
!!       the returned string is passed thru the message/string/error GLOBAL variable
!!##OPTIONS
!!       CHARS
!!       IER     ierr is set and returned as
!!
!!                 -1  an error occurs
!!                  2  a string is returned
!!##RETURNS
!!       MSSGE  when successful the variable value is returned through the global variable MSSGE
!!
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine given_name_get_stringvalue(chars,ierr)
implicit none
character(len=*),parameter :: ident=&
&"@(#)M_calculator::given_name_get_stringvalue(3fp): return associated value for variable name"
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: chars
integer,intent(out)          :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: index
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
   index=0
   call jubous(chars,index,ix2,ierr)
   if(ierr.eq.-1) then
   elseif(index.le.0)then
      ierr=-1
!!!!  what if len(chars) is 0? look carefully at what happens with a possible null string
      mssge=' variable '//chars(:min(icname_calc,len(chars)))//' is undefined'
   else
      ierr=2
      mssge=values(index)
   endif
end subroutine given_name_get_stringvalue
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    getvalue(3f) - [M_calculator]given numeric variable name return doubleprecision value directly from calculator dictionary for efficiency
!!##SYNOPSIS
!!
!!    doubleprecision function getvalue(varnam)
!!
!!     character(len=*),intent(in) :: varnam
!!
!!##DEFINITION
!!       Given numeric variable name return double precision value.
!!       Note this is breaking the rule of only accessing the calculator thru jucalc(3f).
!!       It should only be used from user JUOWN1(3f) routines to avoid recursion
!!##OPTIONS
!!    varnam  name of calculator variable to look up that is assumed to be a valid defined
!!    name of a numeric variable. If it does not exist zero is returned.
!!##EXAMPLE
!!
!!   Program:
!!
!!    program demo_getvalue
!!    use M_calculator_plus, only : rnum0
!!    use M_calculator, only: getvalue
!!    value1=rnum0('A=100/2') ! store something into calculator
!!    write(*,*)value1,getvalue('A')
!!    end program demo_getvalue
!!
!!   Results:
!!
!!    50.0000000       50.000000000000000
!===================================================================================================================================
doubleprecision function getvalue(varnam)
implicit none
character(len=*),parameter :: ident=&
&"@(#)M_calculator::getvalue(3f): given numeric variable name return value"
character(len=*),intent(in) :: varnam
!-----------------------------------------------------------------------------------------------------------------------------------
   integer          :: index
   integer          :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   call jubous(varnam,index,ix,ierr)
   if(index.le.0)then
      ! need option to turn this on and off
      !call journal('*getvalue* error in getvalue')
      getvalue=0.0d0
   else
      getvalue=value(index)
   endif
end function getvalue
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    igetvalue(3f) - [M_calculator]given numeric variable name return integer value directly from calculator dictionary for efficiency
!!##SYNOPSIS
!!
!!    integer function igetvalue(varnam)
!!
!!     character(len=*),intent(in) :: varnam
!!
!!##DEFINITION
!!       Given numeric variable name return integer value.
!!       Note this is breaking the rule of only accessing the calculator thru jucalc(3f).
!!       It should only be used from user JUOWN1(3f) routines to avoid recursion
!!##OPTIONS
!!    varnam  name of calculator variable to look up that is assumed to be a valid defined
!!    name of a numeric variable. If it does not exist zero is returned without an error being
!!    reported.
!!
!!##EXAMPLE
!!
!!   Program:
!!
!!    program demo_igetvalue
!!    use M_calculator_plus, only : rnum0
!!    use M_calculator, only: igetvalue
!!    value1=rnum0('A=100/2') ! store something into calculator
!!    write(*,*)value1,igetvalue('A')
!!    end program demo_igetvalue
!!
!!   Results:
!!
!!    50.0000000       50
!===================================================================================================================================
integer function igetvalue(varnam)
implicit none
character(len=*),intent(in) :: varnam
   igetvalue=int(getvalue(varnam))
end function igetvalue
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    rgetvalue(3f) - [M_calculator]given numeric variable name return real value directly from calculator dictionary for efficiency
!!##SYNOPSIS
!!
!!    real function rgetvalue(varnam)
!!
!!     character(len=*),intent(in) :: varnam
!!
!!##DEFINITION
!!       Given numeric variable name return real value.
!!       Note this is breaking the rule of only accessing the calculator thru jucalc(3f).
!!       It should only be used from user JUOWN1(3f) routines to avoid recursion
!!##OPTIONS
!!    varnam  name of calculator variable to look up that is assumed to be a valid defined
!!    name of a numeric variable. If it does not exist zero is returned without an error being
!!    reported.
!!
!!##EXAMPLE
!!
!!   Program:
!!
!!    program demo_rgetvalue
!!    use M_calculator_plus, only : rnum0
!!    use M_calculator, only: rgetvalue
!!    value1=rnum0('A=100/2') ! store something into calculator
!!    write(*,*)value1,rgetvalue('A')
!!    end program demo_rgetvalue
!!
!!   Results:
!!
!!    50.0000000       50.0000000
!===================================================================================================================================
real function rgetvalue(varnam)
implicit none
character(len=*),intent(in) :: varnam
   rgetvalue=real(getvalue(varnam))
end function rgetvalue
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine integer_stuff(varnam0,int4,ioflag)
implicit none
character(len=*),parameter :: ident=&
&"@(#)M_calculator::integer_stuff(3fp): pass INTEGER value to STUFF(3f)"
   character(len=*),intent(in)      :: varnam0     ! assuming varnam is left justified
   integer,intent(in)               :: int4        ! input value to store
   character(len=*),intent(in),optional      :: ioflag
!#----------------------------------------------------------------------------------------------------------------------------------
   if(.not.present(ioflag))then
      call double_stuff(varnam0,dble(int4))
   else
      call double_stuff(varnam0,dble(int4),ioflag)
   endif
end subroutine integer_stuff
!#----------------------------------------------------------------------------------------------------------------------------------
!#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
!#----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine real_stuff(varnam0,val4,ioflag)
implicit none
character(len=*),parameter :: ident=&
&"@(#)M_calculator::real_stuff(3fp): pass REAL value to STUFF(3f)"
   character(len=*),intent(in)      :: varnam0     ! assuming varnam is left justified
   real,intent(in)                  :: val4        ! input value to store
   character(len=*),intent(in),optional      :: ioflag
!#----------------------------------------------------------------------------------------------------------------------------------
   if(.not.present(ioflag))then
      call double_stuff(varnam0,dble(val4))
   else
      call double_stuff(varnam0,dble(val4),ioflag)
   endif
end subroutine real_stuff
!#----------------------------------------------------------------------------------------------------------------------------------
!#()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
!#----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine double_stuff(varnam0,val8,ioflag)
!     directly store a number into calculator variable name table
!
!     breaking the rule of only accessing the calculator thru jucalc:
!
!     a direct deposit of a value into the calculator assumed to
!     be used only by friendly calls, for efficiency and to avoid
!     problems with recursion if a routine called by the calculator
!     in JUOWN1(3f) wants to store something back into the calculator
!     variable table
!#----------------------------------------------------------------------------------------------------------------------------------
implicit none
character(len=*),parameter :: ident=&
&"@(#)M_calculator::real_stuff(3fp): pass DOUBLEPRECISION value to STUFF(3f)"
character(len=*),intent(in)             :: varnam0     ! assuming varnam is left justified
real(kind=dp),intent(in)                :: val8        ! input value to store
character(len=*),intent(in),optional    :: ioflag
!#----------------------------------------------------------------------------------------------------------------------------------
   character(len=icname_calc)              :: varnam      ! some trouble with variable length character strings on some machines
   character(len=icname_calc+20+1)         :: pass
   integer                                 :: ilen
   integer                                 :: ierr
   integer                                 :: index
!-----------------------------------------------------------------------------------------------------------------------------------
!  assuming friendly, not checking for null or too long varnam0
   varnam=adjustl(varnam0)          ! remove leading spaces
   ilen=len_trim(varnam)            ! get length of trimmed string
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
   call jubous(varnam,index,ix,ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
   if(index.le.0)then
     call juaddr(varnam,ilen,index,ierr)
     if(ierr.eq.-1)then
        call journal('*stuff* error in juaddr')
        return
     endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   value(iabs(index))=val8
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(ioflag))then
      write(pass,'(a,''='',g20.13e3)')varnam(:ilen),val8
      call journal(ioflag,pass)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine double_stuff
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     stuffa(3f) - [M_calculator]stuffa(3f): directly store a string into calculator variable name table"
!!##SYNOPSIS
!!
!!   subroutine stuffa(varnam0,string,index,ioflag)
!!
!!    character(len=*),intent(in)  :: varnam0
!!    character(len=*),intent(in)  :: string
!!    integer                      :: index
!!    character(len=*),intent(in)  :: ioflag
!!##DEFINITION
!!    Breaking the rule of only accessing the calculator thru jucalc:
!!
!!    a direct deposit of a value into the calculator assumed to
!!    be used only by friendly calls, for efficiency and to avoid
!!    problems with recursion if a routine called by the calculator
!!    in JUOWN1(3f) wants to store something back into the calculator
!!    variable table.
!!##OPTIONS
!!    varnam0   variable name to create or replace in calculator module
!!    string    string to associate with the calculator variable name varnam0
!!    index     if less
!!    ioflag    journal logging type passed on to journal(3f) procedure. If it
!!              is blank, the journal(3f) routine is not evoked.
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine stuffa(varnam0,string,index,ioflag)
character(len=*),parameter :: ident=&
&"@(#)M_calculator::stuffa(3f): directly store a string into calculator variable name table"
character(len=*),intent(in)  :: varnam0
character(len=*),intent(in)  :: string
integer,intent(out)          :: index
character(len=*),intent(in)  :: ioflag
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=icname_calc)   :: varnam ! assuming varnam left justified, some machines have trouble
   character(len=101)           :: pass
   integer                      :: ilen
   integer                      :: ierr
   integer                      :: ibig
!-----------------------------------------------------------------------------------------------------------------------------------
!  assuming friendly, not checking for null or too long varnam0
   varnam=adjustl(varnam0)
   ilen=len_trim(varnam)
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
   call jubous(varnam,index,ix2,ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
   if(index.le.0)then
     call juadds(varnam,ilen,index,ierr)
     if(ierr.eq.-1)then
        call journal('*stuffa* error in juadds')
        return
     endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ioflag.ne.'')then          ! display variable string to trail and output as indicated by ioflag
      ibig=min(len(string),iclen_calc)  ! make sure pass string is not too long
      write(pass,'(a,''='',a)')varnam(:ilen),string(1:ibig)
      call journal(ioflag,pass)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   index=iabs(index)
   values(index)=string
   ilen=len(string)
   ilen=len_trim(string(:ilen))
   valuer(index)=max(ilen,1)
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine stuffa
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
end module m_calculator
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
module M_noown
! this module contains routines that are used to customize the
! M_calculator module. Since M_calculator requires them if you do
! not wish to create your own; use these dummy routines
private
public juown1
public c
contains
!#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
subroutine juown1(func,iflen,args,iargstp,n,fval,ctmp,ier)
      ! extend functions available to the calculator routine
!
!     if the function ownmode(1) is called this subroutine
!     will be accessed to do user-written functions.
!
!     func(iend-1)=procedure name.  func should not be changed.
!     iflen=length of procedure name.
!     args=array of 100 elements containing procedure arguments.
!     iargstp=type of argument(1=value,2=position of string value)
!     n=integer number of parameters
!     x=array of 55555 x values
!     y=array of 55555 y values
!     fval=value to replace function call
!     ctmp=string to return when returning a string value
!     ier=returned error flag value.
!         set to -1 if an error occurs.
!         set to  0 if a  number is returned
!         set to  2 if a string is returned
!
      use m_calculator, only : x, y, values, valuer
      character(len=*) func
      character(len=*) ctmp

      integer iflen ,n, ier, iargstp(100)
      integer, parameter        :: k_dbl = SELECTED_REAL_KIND(15,300) ! real*8
      real(kind=k_dbl) ::  fval
      fval=0
!-----------------------------------------------------------------------
      write(*,*)'*juown1* unknown function ', func(1:iflen)
      write(*,*)'function name length is..',iflen
      write(*,*)'number of arguments .....',n
      do i10=1,n
         if(iargstp(i10).eq.0)then
            write(*,*)i10,' VALUE=',args(i10)
         elseif(iargstp(i10).eq.2)then
            iwhich=int(args(i10)+0.5)
            ilen=valuer(iwhich)
            write(*,*)i10,' STRING='//values(iwhich)(:ilen)
         else
            write(*,*)'unknown parameter type is ',iargstp(i10)
         endif
      enddo
end subroutine juown1
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
real function c(fval,n)
!     a built-in calculator function called c must be satisfied.
!     write whatever you want here as a function
      integer, parameter        :: k_dbl = SELECTED_REAL_KIND(15,300) ! real*8
      real(kind=k_dbl) ::  fval
      c=0.0
end function c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
end module M_noown
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
