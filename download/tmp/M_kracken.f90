!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_kracken
use M_debug,   only: debug, io_debug
use M_journal, only: journal
use M_strings, only: upper, string_to_value, split
implicit none
character(len=*),parameter :: ident="@(#)M_kracken(3fm):parse command line options of Fortran programs using Unix-like syntax"
!===================================================================================================================================
   private
!-----------------------------------------------------------------------------------------------------------------------------------
   public :: kracken                ! define command and default parameter values from command arguments
!-----------------------------------------------------------------------------------------------------------------------------------
   public :: rget                   ! fetch real    value of name VERB_NAME from the Language Dictionary
   public :: dget                   ! fetch double  value of name VERB_NAME from the Language Dictionary
   public :: iget                   ! fetch integer value of name VERB_NAME from the Language Dictionary
   public :: lget                   ! fetch logical value of name VERB_NAME from the Language Dictionary
   public :: sget                   ! fetch string  value of name VERB_NAME from the Language Dictionary.
!-----------------------------------------------------------------------------------------------------------------------------------
   public :: rgets                  ! fetch real    values of name VERB_NAME from the Language Dictionary
   public :: dgets                  ! fetch double  values of name VERB_NAME from the Language Dictionary
   public :: igets                  ! fetch integer values of name VERB_NAME from the Language Dictionary
   public :: lgets                  ! fetch logical values of name VERB_NAME from the Language Dictionary
   public :: sgets                  ! fetch string  values of name VERB_NAME from the Language Dictionary.
!-----------------------------------------------------------------------------------------------------------------------------------
   public :: retrev                 ! retrieve token value as string from Language Dictionary when given NAME
!-----------------------------------------------------------------------------------------------------------------------------------
!  SPECIAL-PURPOSE PUBLIC ROUTINES:
   public :: setprompts             ! define prompts for commands in interactive mode
!  Only needs to be public for building languages, not cracking command line arguments
   public :: dissect                ! for user-defined commands: define defaults, then process user input
   public :: parse                  ! parse user command and store tokens into Language Dictionary
   public :: store                  ! replace dictionary name's value (if allow=add add name if necessary)
   public :: show                   ! display dictionary contents for information
!-----------------------------------------------------------------------------------------------------------------------------------
   private :: bounce                ! find location (index) in Language Dictionary where VARNAM can be found
   private :: add_string            ! Add new string name to Language Library dictionary
   private :: subscript             ! return the subscript value of a string when given it's name
   private :: menu                  ! generate an interactive menu when -? option is used
   private :: get_command_arguments ! get_command_arguments: return all command arguments as a string
!-----------------------------------------------------------------------------------------------------------------------------------
! length of verbs and entries in Language dictionary
! NOTE:   many parameters may be  reduced in size so as to just accommodate being used as a command line parser.
!         In particular, some might want to change:
   integer, parameter,public :: IPic=4000                          ! number of entries in language dictionary
   logical,public            :: stop_command=.false.               ! indication to return stop_command as false in interactive mode
   integer, parameter,public :: IPvalue=4096                       ! length of keyword value
   integer, parameter,public :: IPcmd=32768                        ! length of command
   integer, parameter,public :: IPverb=20                          ! length of verb
!-----------------------------------------------------------------------------------------------------------------------------------
   integer, parameter        :: dp = kind(0.d0)
   integer, parameter        :: k_int = SELECTED_INT_KIND(9)       ! integer*4
   integer, parameter        :: k_dbl = SELECTED_REAL_KIND(15,300) ! real*8
!-----------------------------------------------------------------------------------------------------------------------------------
   ! dictionary for Language routines
   character(len=IPvalue),dimension(IPic)     :: dict_vals=" "     ! contains the values of string variables
   character(len=IPverb),dimension(IPic)      :: dict_verbs=" "    ! string variable names
   integer(kind=k_int),dimension(IPic)        :: dict_lens=0       ! significant lengths of string variable values
   integer(kind=k_int),dimension(IPic)        :: dict_calls=0      ! number of times this keyword stored on a call to parse
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=1),save,public               :: kracken_comment='#'
   character(len=IPcmd),public                :: leftover=' '      ! remaining command(s) on line
   integer,public,save                        :: current_command_length=0 ! length of options for current command
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    RETREV(3f) - [ARGUMENTS:M_kracken]get keyword value as a string from a command's argument list processed by kracken(3f)
!!
!!##SYNOPSIS
!!
!!   SUBROUTINE retrev(name, string, len, ier)
!!
!!    CHARACTER(len=*),intent(in)  :: name
!!    CHARACTER(len=*),intent(out) :: string
!!    INTEGER,intent(out)          :: len
!!    INTEGER,intent(out)          :: ier
!!
!!##DESCRIPTION
!!    When a command has had it's command argument list parsed using the
!!    kracken(3f) routine the value associated with any keyword can be retrieved
!!    as a string.
!!
!!##OPTIONS
!!
!!     NAME    parameter name of form VERB_KEYWORD
!!     STRING  returned parameter value
!!     LEN     length of returned STRING
!!     IER     error flag. Any non-zero value means an error occurred
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_retrev
!!     use M_kracken, only : kracken, retrev
!!     use M_kracken, only : IPvalue ! length of keyword value
!!     implicit none
!!     character(len=IPvalue) :: val
!!     integer                :: len, ier
!!
!!     call kracken('demo', ' -value my default string')
!!     call retrev('demo_value',val,len,ier)
!!     write(*,'(a)')'VALUE IS '//trim(val)
!!
!!     end program demo_retrev
!!
!!   Example execution and output:
!!
!!     $ ./demo_retrev
!!     VALUE IS my default string
!!
!!     $ ./demo_retrev -value use this value instead
!!     VALUE IS use this value instead
!===================================================================================================================================
subroutine retrev(name,val,len,ier)
character(len=*),parameter :: ident="@(#)M_kracken::retrev(3f): retrieve token value from Language Dictionary when given NAME"
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)     :: name        ! name of variable to retrieve value for in form VERB_NAME
character(len=*),intent(out)    :: val         ! value for requested variable
   integer,intent(out)          :: len         ! position of last non-blank character in requested variable
   integer,intent(out)          :: ier         ! error flag 0=found requested variable; -1=entry not found
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: isub        ! subscript in dictionary where requested entry and corresponding value are found
!-----------------------------------------------------------------------------------------------------------------------------------
   isub=subscript(name)                        ! get index entry is stored at
!-----------------------------------------------------------------------------------------------------------------------------------
   if(isub > 0)then                            ! entry was in dictionary
      val=dict_vals(isub)                      ! retrieve corresponding value for requested entry
      len=dict_lens(isub)                      ! get significant length of value
      ier=0                                    ! indicate requested entry name was successfully found
   else                                        ! entry was not in dictionary
      val=" "                                  ! set value to blank
      len=0                                    ! set length to zero
      ier=-1                                   ! set error flag to indicate requested entry was not found
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine retrev
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          dget - [ARGUMENTS:M_kracken]given keyword fetch doubleprecision value from command argument
!!##SYNOPSIS
!!
!!    function dget(keyword) result(value)
!!
!!     character(len=*),intent(in)  :: keyword
!!     doubleprecision              :: value
!!##DESCRIPTION
!!     The dget(3f) function returns a scalar doubleprecision value from a command line
!!     argument using the M_kracken(3fm) module.
!!##OPTIONS
!!     KEYWORD    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!                The VERB name comes from the first argument of the KRACKEN(3f)
!!                call. The KEYWORD is a keyword from the KRACKEN(3f) call that
!!                should be interpreted as a doubleprecision value.
!!##RETURNS
!!     VALUE      doubleprecision value returned by function
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_dget
!!    use M_kracken, only: kracken, dget
!!    implicit none
!!    doubleprecision :: val
!!      ! define command arguments and parse user command
!!      call kracken('demo','-val 3.1416' )
!!      val=dget('demo_val') ! get any values specified on -val option
!!      write(*,*)val         ! print the value
!!    end program demo_dget
!!
!!   Example program runs:
!!
!!    $ demo_dget
!!       3.14159989
!!
!!    $ demo_dget -val 10
!!       10.0000000
!!
!!    $ demo_dget -val 3.000
!!       3.00000000
!===================================================================================================================================
function dget(keyword)
character(len=*),parameter :: ident="@(#)M_kracken::dget(3f): given keyword fetch dble value from Language Dictionary (zero on err)"
real(kind=dp)                :: dget              ! function type
character(len=*),intent(in)  :: keyword           ! keyword to retrieve value for from dictionary
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: ier            ! error flag on call to retrieve value
!-----------------------------------------------------------------------------------------------------------------------------------
   call string_to_value(sget(keyword), dget, ier) ! convert the string to a numeric value
!-----------------------------------------------------------------------------------------------------------------------------------
end function dget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          rget - [ARGUMENTS:M_kracken]given keyword fetch real value from command argument
!!##SYNOPSIS
!!
!!    function rget(keyword) result(value)
!!
!!     character(len=*),intent(in)  :: keyword
!!     real                         :: value
!!##DESCRIPTION
!!     The rget(3f) function returns a scalar real value from a command line
!!     argument using the M_kracken(3fm) module.
!!##OPTIONS
!!     KEYWORD    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!                The VERB name comes from the first argument of the KRACKEN(3f)
!!                call. The KEYWORD is a keyword from the KRACKEN(3f) call that
!!                should be interpreted as a REAL value.
!!##RETURNS
!!     VALUE      real value returned by function
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_rget
!!    use M_kracken, only: kracken, rget
!!    implicit none
!!    real :: val
!!      ! define command arguments and parse user command
!!      call kracken('demo','-val 3.1416' )
!!      val=rget('demo_val') ! get any values specified on -val option
!!      write(*,*)val        ! print the value
!!    end program demo_rget
!!
!!   Example program runs:
!!
!!    $ demo_rget
!!       3.14159989
!!
!!    $ demo_rget -val 10
!!       10.0000000
!!
!!    $ demo_rget -val 3.000
!!       3.00000000
!===================================================================================================================================
function rget(keyword)
character(len=*),parameter :: ident="@(#)M_kracken::rget(3f): given keyword fetch real value from language dictionary (zero on err)"
!-----------------------------------------------------------------------------------------------------------------------------------
   real                        :: rget             ! function type
   character(len=*),intent(in) :: keyword          ! keyword to retrieve value for from dictionary
!-----------------------------------------------------------------------------------------------------------------------------------
   rget=real(dget(keyword))                        ! just call DGET(3f) but change returned value to type REAL
!-----------------------------------------------------------------------------------------------------------------------------------
end function rget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          iget - [ARGUMENTS:M_kracken]given keyword fetch integer value from command argument
!!
!!##SYNOPSIS
!!
!!    function iget(keyword) result(value)
!!
!!     character(len=*),intent(in)  :: keyword
!!     integer              :: value
!!
!!##DESCRIPTION
!!     The iget(3f) function returns a scalar integer value from a command line
!!     argument using the M_kracken(3fm) module.
!!
!!##OPTIONS
!!     KEYWORD    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!                The VERB name comes from the first argument of the KRACKEN(3f)
!!                call. The KEYWORD is a keyword from the KRACKEN(3f) call that
!!                should be interpreted as a integer value.
!!
!!##RETURNS
!!     VALUE      integer value returned by function
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_iget
!!    use M_kracken, only: kracken, iget
!!    implicit none
!!    integer :: val
!!      ! define command arguments and parse user command
!!      call kracken('demo','-val 31416' )
!!      val=iget('demo_val') ! get any values specified on -val option
!!      write(*,*)val        ! print the value
!!    end program demo_iget
!!
!!   Example program runs:
!!
!!    $ demo_iget
!!       31416
!!
!!    $ demo_iget -val 10
!!       10
!!
!!    $ demo_iget -val 3.000
!!       3
!===================================================================================================================================
function iget(keyword)
character(len=*),parameter :: ident="@(#)M_kracken::iget(3f): given keyword fetch integer value from Language Dictionary (0 on err)"
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: iget            ! function type
   character(len=*),intent(in)  :: keyword         ! keyword to retrieve value for from dictionary
!-----------------------------------------------------------------------------------------------------------------------------------
   !iget = int(dget(keyword)+0.5_dp)               ! just call DGET(3f) but change returned value to type INTEGER
   iget = int(dget(keyword))                       ! just call DGET(3f) but change returned value to type INTEGER
!-----------------------------------------------------------------------------------------------------------------------------------
end function iget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          lget - [ARGUMENTS:M_kracken]given keyword fetch logical value from command arguments
!!##SYNOPSIS
!!
!!    function lget(keyword) result(lval)
!!
!!     character(len=*),intent(in)  :: keyword
!!     logical                      :: lval
!!##DESCRIPTION
!!     The lget(3f) function returns a scalar logical value from a command line
!!     argument using the M_kracken(3fm) module.
!!
!!##OPTIONS
!!     keyword    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!                The VERB name comes from the first argument of the
!!                KRACKEN(3f) call. The KEYWORD is a keyword from the second
!!                argument to the KRACKEN(3f) call.
!!
!!##RETURNS
!!     lval       logical value returned by function. The input value should be
!!                from the case-insensitive list of the words "true, false,
!!                t, f, yes, no, y, n, .false., .true., .f., .t".
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_lget
!!    use M_kracken, only: kracken, lget
!!    implicit none
!!    logical  :: val
!!      ! define command arguments and parse user command
!!      call kracken('demo','-truth .F.' )
!!      ! get any values specified on command line for -truth
!!      val=lget('demo_truth')
!!      write(*,'("The truth is ",l1)')val
!!    end program demo_lget
!!
!!   Example program runs:
!!
!!      $ demo_lget             # uses the default
!!      The truth is F
!!      $ demo_lget -truth      # A BLANK VALUE IS TRUE
!!      The truth is T
!!      $ demo_lget -truth yes  # Y, yes, T, true, .T., .true. are all true
!!      The truth is T
!!      $ demo_lget -truth F    # N, no, F, false, .F., .FALSE. are all false
!!      The truth is F
!===================================================================================================================================
function lget(keyword)
character(len=*),parameter :: ident="@(#)M_kracken::lget(3f): given keyword fetch logical value from lang. dictionary (.f. on err)"
!-----------------------------------------------------------------------------------------------------------------------------------
logical                      :: lget               ! procedure type
character(len=*),intent(in)  :: keyword            ! the dictionary keyword (in form VERB_KEYWORD) to retrieve
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=:),allocatable :: value           ! value corresponding to the requested keyword
!-----------------------------------------------------------------------------------------------------------------------------------
!  ignore a leading dot character. Then, any word starting in "T" or "Y" is true, any word starting in "F" or "N" is false.
   value=trim(adjustl(upper(sget(keyword))))//' '  ! get value as uppercase, spaces trimmed, then add trailing space
   lget=.false.                                    ! initialize return value to .false.
   if(value.ne."#N#".and.value.ne.'"#N#"')then
      select case(value(1:1))                      ! check first letter
      case('T','Y',' ') ; lget=.true.              ! anything starting with "T" or "Y" or a blank is TRUE (true,t,yes,y,...)
      case('F','N')     ; lget=.false.             ! assume this is false or no
      case('.')                                    ! looking for fortran logical syntax .STRING.
         select case(value(2:2))
         case('T')      ; lget=.true.              ! assume this is .t. or .true.
         case('F')      ; lget=.false.             ! assume this is .f. or .false.
         case default
            call journal("*lget* bad logical expression for "//keyword(:len_trim(keyword))//'='//value)
         end select
      case default
            call journal("*lget* bad logical expression for "//keyword(:len_trim(keyword))//'='//value)
      end select
   else                                            ! special value "#N#" is assumed FALSE
      lget=.false.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function lget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          sget - [ARGUMENTS:M_kracken]given keyword fetch string value and length from command arguments
!!##SYNOPSIS
!!
!!   function sget(name,ilen) result(string)
!!
!!    character(len=*),intent(in)   :: name        ! name to look up in dictionary
!!    integer,intent(out),optional  :: ilen        ! length of returned output string
!!    character(len=:),allocatable  :: string      ! returned value
!!
!!##DESCRIPTION
!!     The sget(3f) function returns a scalar character value from a command line
!!     argument using the M_kracken(3fm) module.
!!
!!##OPTIONS
!!     name    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!             The VERB name comes from the first argument of the
!!             KRACKEN(3f) call. The KEYWORD is a keyword from the second
!!             argument to the KRACKEN(3f) call.
!!             This routine trusts that the desired name exists.
!!             A blank is returned if the name is not in the dictionary
!!
!!##RETURNS
!!     string  returned string
!!     ilen    optional length of returned output string
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_sget
!!    use M_kracken, only: kracken, sget, IPvalue
!!    implicit none
!!    character(len=IPvalue) :: string, a, b
!!      ! define command arguments and parse user command
!!      call kracken('demo','-string This is the default -a A default -b B default' )
!!      ! get any values specified on command line for -truth
!!      string=sget('demo_string')
!!      a=sget('demo_a')
!!      b=sget('demo_b')
!!      write(*,'("string is ",a')trim(string)
!!      write(*,'("a is ",a')trim(a)
!!      write(*,'("b is ",a')trim(b)
!!    end program demo_sget
!!
!!   Example program runs:
!!
!!    $demo_sget
!!    string is This is the default
!!    a is A default
!!    b is B default
!!
!!    $ demo_sget -a A value for A -string new value for string -b BBBBBBB
!!    string is new value for string
!!    a is A value for A
!!    b is BBBBBBB
!===================================================================================================================================
function sget(name,ilen) result(string)
character(len=*),parameter :: ident="@(#)M_kracken::sget(3f): Fetch string value and length of specified NAME from lang. dictionary"
!  This routine trusts that the desired name exists. A blank is returned if the name is not in the dictionary
character(len=:),allocatable  :: string      ! returned value
character(len=*),intent(in)   :: name        ! name to look up in dictionary
integer,intent(out),optional  :: ilen        ! length of returned output string
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                    :: isub        ! index where verb_oo is stored or -1 if this is an unknown name
!-----------------------------------------------------------------------------------------------------------------------------------
   isub=subscript(name)                      ! given name return index name is stored at
!-----------------------------------------------------------------------------------------------------------------------------------
   if(isub > 0)then                          ! if index is valid return string
      string=dict_vals(isub)
   else                                      ! if index is not valid return blank string
      string(:)=" "
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(ilen))then                     ! if ILEN is present on call, return the value
      ilen=dict_lens(isub)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function sget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          dgets - [ARGUMENTS:M_kracken]given keyword fetch doubleprecision array from command arguments
!!##SYNOPSIS
!!
!!    function dgets(keyword) result(darray)
!!
!!     character(len=*),intent(in)  :: keyword
!!     doubleprecision,allocatable  :: DARRAY
!!
!!##DESCRIPTION
!!     The dgets(3f) function returns a dynamically allocated array of
!!     doubleprecision values from a string that is the value for a command
!!     line option. It is part of the M_kracken(3fp) module.
!!
!!     Values that cannot be read as a numeric value are returned as a zero (0).
!!##OPTIONS
!!     keyword  dictionary name to retrieve, of form VERB_NAME where VERB
!!              is taken from the first parameter of the call to KRACKEN(3f)
!!              or DISSECT(3f).
!!##RETURNS
!!     darray   double precision numeric array returned by function
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_dnums
!!    use M_kracken, only: kracken, dgets
!!    implicit none
!!    doubleprecision,allocatable  :: vals(:)
!!    integer              :: i
!!    ! define command arguments and parse user command
!!    call kracken('demo','-nums 1 2 3 1000 100,000 11.11111 77.77777 -77.7777' )
!!    vals=dgets('demo_nums') ! get any values specified for -nums
!!    write(*,'(*(g0:,","))')( vals(i),i=1,size(vals)) ! print the values
!!    end program demo_dnums
!!
!!   Example program runs:
!!
!!    $ demo_dnums
!!     1.0000000000000000,2.0000000000000000,3.0000000000000000,
!!     1000.0000000000000,100000.00000000000,11.111110000000000,
!!     77.777770000000004,-77.777699999999996
!!
!!    $ demo_dnums -nums 89,123,456.789 10.9999999
!!     89123456.789000005,10.999999900000001
!===================================================================================================================================
function dgets(keyword) result(darray)
character(len=*),parameter  :: ident="@(#)M_kracken::dgets(3f): given keyword fetch dble value from Language Dictionary (0 on err)"
character(len=*),intent(in) :: keyword                      ! keyword to retrieve value for from dictionary
real(kind=dp),allocatable   :: darray(:)                    ! function type

   character(len=IPvalue),allocatable :: carray(:)          ! convert value to an array using split(3f)
   integer                            :: i
   integer                            :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
   call split(sget(keyword),carray)                         ! find value associated with keyword and split it into an array
   allocate(darray(size(carray)))                           ! create the output array
   do i=1,size(carray)
      call string_to_value(carray(i), darray(i), ier)       ! convert the string to a numeric value
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end function dgets
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          igets - [ARGUMENTS:M_kracken]given keyword fetch integer array from command arguments
!!##SYNOPSIS
!!
!!    function igets(keyword) result(iarray)
!!
!!     character(len=*),intent(in)  :: keyword
!!     integer,allocatable          :: iarray(:)
!!##DESCRIPTION
!!     The igets(3f) function returns a dynamically allocated array of integers
!!     from a string that is the value for a command line option. It is part of
!!     the M_kracken(3fp) module.
!!
!!     Values that cannot be read as an integer value are returned as a zero (0).
!!
!!##OPTIONS
!!     KEYWORD    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!                The VERB name comes from the first argument of the KRACKEN(3f)
!!                call. The KEYWORD is a keyword from the KRACKEN(3f) call that
!!                should be interpreted as a list of INTEGER values. Decimal values
!!                are allowed but truncated. Note that comma characters are ignored.
!!
!!##RETURNS
!!     IARRAY     INTEGER array returned by function
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_inums
!!    use M_kracken, only: kracken, igets
!!    implicit none
!!    integer,allocatable  :: vals(:)
!!    integer              :: i
!!      ! define command arguments and parse user command
!!      call kracken('demo','-nums 1 2 3 100 1000 10000 100,000 11.11111 77.77777 -77.7777' )
!!      ! get any values specified for -nums
!!      vals=igets('demo_nums')
!!      if(size(vals).gt.0)then
!!         ! print the requested values
!!         write(*,'(*(i0:,","))')( vals(i),i=1,size(vals))
!!      endif
!!    end program demo_inums
!!
!!   Example program runs:
!!
!!      $ demo_inums
!!      1,2,3,100,1000,10000,100000,11,77,-77
!!      $ demo_inums -val 89,123,456 10.9999999
!!      89123456,10
!===================================================================================================================================
function igets(keyword) result(iarray)
character(len=*),parameter :: ident="@(#)M_kracken::igets(3f):given keyword fetch integer array from string in dictionary(0 on err)"
character(len=*),intent(in) :: keyword             ! keyword to retrieve value for from dictionary
integer,allocatable         :: iarray(:)           ! convert value to an array
   iarray=int(dgets(keyword))                      ! just call DGETS(3f) but change returned value to type INTEGER
end function igets
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          rgets - [ARGUMENTS:M_kracken]given keyword fetch real array from command arguments
!!##SYNOPSIS
!!
!!    function rgets(keyword) result(rarray)
!!
!!     character(len=*),intent(in)  :: keyword
!!     real,allocatable             :: rarray(:)
!!##DESCRIPTION
!!     The rgets(3f) function returns a dynamically allocated array of real values
!!     from a string that is the value for a command line option. It is part of
!!     the M_kracken(3fp) module.
!!
!!     Values that cannot be read as a numeric value are returned as a zero (0).
!!
!!##OPTIONS
!!     KEYWORD    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!                The VERB name comes from the first argument of the KRACKEN(3f)
!!                call. The KEYWORD is a keyword from the KRACKEN(3f) call that
!!                should be interpreted as a list of REAL values.
!!
!!##RETURNS
!!     RARRAY     real array returned by function
!!
!!##EXAMPLE
!!
!!   Sample program converts between Celcius and Fahrenheit
!!
!!    program demo_rgets
!!    use M_kracken, only: kracken, rgets
!!    implicit none
!!    real,allocatable  :: val(:)
!!    integer           :: i
!!      ! define command arguments and parse user command
!!      call kracken('fc','-F -C' )
!!
!!      ! get any values specified on -C option
!!      val=rgets('fc_C')
!!      ! test if have something to print in C ==> F table
!!      if(size(val).gt.0)then
!!         ! print the requested values
!!         write(*,'(a,t14,a)')'celsius','fahrenheit'
!!         write(*,'(f5.1,t14,f5.1)')( val(i),(val(i)+40.0)*9.0/5.0 - 40.0,i=1,size(val))
!!      endif
!!
!!      val=rgets('fc_F')
!!      ! check for values on -F
!!      if(size(val).gt.0)then
!!         write(*,'(a,t14,a)') 'fahrenheit', 'celsius'
!!         write(*,'(f5.1,t14,f5.1)')(val(i),(val(i)+40.0)*5.0/9.0 - 40.0,i=1,size(val))
!!      endif
!!    end program demo_rgets
!!
!!   Example program runs:
!!
!!    % demo_rgets -C -273.15 0 100 -40 37
!!    celsius      fahrenheit
!!     -273.15      -459.67
!!        0.0         32.0
!!      100.0        212.0
!!      -40.0        -40.0
!!       37.0         98.6
!!
!!    % demo_rgets -F -459.67 32 212 -40 98.6
!!    fahrenheit   celsius
!!     -459.67      -273.15
!!       32.00         0.00
!!      212.00       100.00
!!      -40.00       -40.00
!!       98.60        37.00
!===================================================================================================================================
function rgets(keyword) result(rarray)
character(len=*),parameter  :: ident="@(#)M_kracken::rgets(3f): given keyword fetch real array from string in dictionary (0 on err)"
character(len=*),intent(in) :: keyword             ! keyword to retrieve value for from dictionary
real,allocatable            :: rarray(:)           ! convert value to an array
   rarray=real(dgets(keyword))                     ! just call DGETS(3f) but change returned value to type REAL
end function rgets
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          lget - [ARGUMENTS:M_kracken]given keyword fetch logical array from command argument
!!##SYNOPSIS
!!
!!    function lgets(keyword) result(lvals)
!!
!!     character(len=*),intent(in)  :: keyword
!!     logical,allocatable          :: lvals(:)
!!##DESCRIPTION
!!     The lgets(3f) function returns a dynamically allocated array of logical values
!!     from a string that is the value for a command line option. It is part of
!!     the M_kracken(3fp) module.
!!
!!     Values that cannot be read as a logical value are returned as a ".FALSE.".
!!##OPTIONS
!!     keyword    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!                The VERB name comes from the first argument of the
!!                KRACKEN(3f) call. The KEYWORD is a keyword from the second
!!                argument to the KRACKEN(3f) call.
!!
!!##RETURNS
!!     lvals      logical array returned by function. The input value should be
!!                from the case-insensitive list of the words "true, false,
!!                t, f, yes, no, y, n, .false., .true., .f., .t".
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_lgets
!!    use M_kracken, only: kracken, lgets
!!    implicit none
!!    logical,allocatable  :: vals(:)
!!      ! define command arguments and parse user command
!!      call kracken('demo','-truths .F. .T. .F. .F. .T. .T.' )
!!      ! get any values specified on command line for -truth
!!      vals=lgets('demo_truths')
!!      write(*,*)vals
!!    end program demo_lgets
!!
!!   Example program runs:
!!
!!    $ demo_lgets
!!     F T F F T T
!!
!!    $ demo_lgets -truths false F .f. no true .true. t T Yes No
!!     F F F F T T T T T T F
!===================================================================================================================================
function lgets(keyword) result(larray)
character(len=*),parameter :: ident="@(#)M_kracken::lgets(3f):given keyword fetch logical array from string in dictionary(F on err)"
character(len=*),intent(in)  :: keyword                    ! the dictionary keyword (in form VERB_KEYWORD) to retrieve
logical,allocatable          :: larray(:)                  ! convert value to an array
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=IPvalue),allocatable :: carray(:)         ! convert value to an array
   integer                            :: i
   integer                            :: ichar             ! point to first character of word unless first character is "."
!-----------------------------------------------------------------------------------------------------------------------------------
   call split(adjustl(upper(sget(keyword))),carray)        ! convert value to uppercase, left spaces trimmed; then parse into array
   if(size(carray).gt.0)then                                  ! if not a null string
      allocate(larray(size(carray)))                          ! allocate output array
      do i=1,size(carray)
         larray(i)=.false.                                    ! initialize return value to .false.
         if(carray(i).ne."#N#".and.carray(i).ne.'"#N#"')then
            if(carray(i)(1:1).eq.'.')then                     ! looking for fortran logical syntax .STRING.
               ichar=2
            else
               ichar=1
            endif
            select case(carray(i)(ichar:ichar))               ! check word to see if true or false
            case('T','Y',' '); larray(i)=.true.               ! anything starting with "T" or "Y" or a blank is TRUE (true,yes,...)
            case('F','N');     larray(i)=.false.              ! assume this is false or no
            case default
                  call journal("*lgets* bad logical expression for "//trim(keyword)//'='//carray(i))
            end select
         else                                                 ! special value "#N#" is assumed FALSE
            larray(i)=.false.
         endif
      enddo
   else                                                       ! for a blank string return one T
      allocate(larray(1))                                     ! allocate output array
      larray(1)=.true.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function lgets
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          sgets - [ARGUMENTS:M_kracken]given keyword fetch string value parsed on whitespace into an array
!!##SYNOPSIS
!!
!!   function sgets(name,delim) result(strings)
!!
!!    character(len=*),intent(in) :: name
!!    character(len=*),intent(in),optional :: delim
!!    character(len=IPvalue),allocatable :: strings(:)
!!
!!##DESCRIPTION
!!     The sgets(3f) function returns a dynamically allocated array of character values
!!     from a string that is the value for a command line option. It is part of
!!     the M_kracken(3fp) module.
!!##OPTIONS
!!     name     the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!              The VERB name comes from the first argument of the
!!              KRACKEN(3f) or DISSECT(3f) call. The KEYWORD is a keyword from the second
!!              argument to the KRACKEN(3f) or DISSECT(3f) call.
!!              This routine trusts that the desired name exists.
!!              A blank is returned if the name is not in the dictionary.
!!     delim    characters to split the string at into elements
!!
!!##RETURNS
!!     strings  returned string array
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_sgets
!!    use M_kracken, only : kracken, sgets, IPvalue
!!    character(len=IPvalue),allocatable :: strings(:)
!!       call kracken('cmd',' -string    This   is  a sentence ')
!!       strings= sgets("cmd_string")            ! get -strings words
!!       print *, "string=",('['//trim(strings(i))//']',i=1,size(strings))
!!    end program demo_sgets
!!
!!   Example program execution:
!!    $ xxx
!!     string=[This][is][a][sentence]
!!
!!    $ xxx -string parse this into words
!!     string=[parse][this][into][words]
!===================================================================================================================================
function sgets(name,delim) result(strings)
character(len=*),parameter :: ident="@(#)M_kracken::sgets(3f): Fetch strings value for specified NAME from the lang. dictionary"
! This routine trusts that the desired name exists. A blank is returned if the name is not in the dictionary
character(len=IPvalue),allocatable  :: strings(:)
character(len=*),intent(in)         :: name                       ! name to look up in dictionary
character(len=*),intent(in),optional :: delim
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                          :: isub                       ! index where verb_oo is stored or -1 if this is an unknown name
!-----------------------------------------------------------------------------------------------------------------------------------
   isub=subscript(name)                                           ! given name return index name is stored at
!-----------------------------------------------------------------------------------------------------------------------------------
   if(isub > 0)then                                               ! if index is valid return strings
      if(present(delim))then
         call split(dict_vals(isub),strings,delim)
      else
         call split(dict_vals(isub),strings)
      endif
   else                                                           ! if index is not valid return blank string
      allocate(character(len=IPvalue) :: strings(1))
      strings(1)=" "
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function sgets
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          kracken(3f) - [ARGUMENTS:M_kracken]crack command line options on Fortran programs, using "-KEYWORD VALUE" syntax
!!##SYNOPSIS
!!
!!     subroutine kracken(verb, string[,ierror])
!!
!!        character(len=*), intent(in) ::  verb
!!        character(len=*), intent(in) :: string
!!        integer, intent(out), optional :: ierror
!!
!!##DESCRIPTION
!!
!!     This is the main public procedure in the M_kracken(3f) module.
!!     It is used to define the command line options, their default
!!     values, and to crack the command line options using a syntax
!!     that looks very much like an execution of the program.
!!
!!##OPTIONS
!!     VERB     arbitrary command name, usually 'cmd' or the name of the
!!              program calling the routine. This defines the
!!              variable prefix name used by the other functions to
!!              retrieve command option values.
!!
!!     STRING   prototype command to define keywords and defaults.
!!              This string is simply a list of all keywords and their
!!              default values exactly as you would type them on the
!!              command line, with default values explicitly set.
!!
!!     IERROR   If an error occurs such as an unknown keyword the
!!              calling program will be stopped unless the optional
!!              parameter IERROR is present. If present, it is up
!!              to the calling program to decide what to do if
!!              a non-zero value is returned.
!!##EXAMPLE
!!
!!
!!   Sample program:
!!
!!       program krackenbasic
!!
!!       use M_kracken
!!       ! define command arguments, default values and crack command line
!!       call kracken('cmd',              &
!!          &   '-int 20                  &
!!          &   -real 10e3                &
!!          &   -file input               &
!!          &   -dble 4.11223344556677d0  &
!!          &   -help    .false.          &
!!          &   -version .false.         '&
!!          &   )
!!       ! that's it. You defined your command arguments and their default
!!       ! values and parsed the user-supplied command line arguments.
!!
!!       ! Now you can just retrieve the values as strings using
!!       ! names of the form VERB_SWITCHNAME anywhere in your program.
!!       ! Note that the special name "VERB_oo"  is for the string
!!       ! before any switch.
!!          if(lget('cmd_help'))then ! was -help specified?
!!             write(*,*)'The help text'
!!             stop
!!          endif
!!          if(lget('cmd_version'))then ! was -version specified?
!!             write(*,*)'version 1.0 20161030'
!!             stop
!!          endif
!!          ! convert all the remaining options to scalar values
!!          ! and call a procedure with the values
!!          call mymain(                  &
!!          & sget('cmd_file'),           &
!!          & rget('cmd_real'),           &
!!          & dget('cmd_dble'),           &
!!          & iget('cmd_int')             &
!!          & )
!!       end program krackenbasic
!!
!!       subroutine mymain(filename,value1,value2,ivalue3)
!!       ! this routine is using conventional values and does
!!       ! not use M_kracken(3fm) module at all
!!       implicit none
!!       character(len=*),intent(in) :: filename
!!       real,intent(in)             :: value1
!!       doubleprecision,intent(in)  :: value2
!!       integer,intent(in)          :: ivalue3
!!          ! just to show the command arguments have
!!          ! been processed echo the values
!!          print *, 'filename=',trim(filename)
!!          print *, 'values=',value1,value2,ivalue3
!!       end subroutine mymain
!!
!!   expected output from : "./cmd"
!!
!!          filename=input
!!          values= 10000.0000  4.1122334455667700  20
!!
!!   expected output from : "./cmd -file myfile -int 1234"
!!
!!          filename=myfile
!!          values= 10000.0000  4.1122334455667700  1234
!===================================================================================================================================
subroutine kracken(verb,string,error_return)
character(len=*),parameter :: ident="@(#)M_kracken::kracken(3f): define and parse command line options"
!  get the entire command line argument list and pass it and the prototype to dissect()
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in)    :: string
      character(len=*),intent(in)    :: verb
      integer,intent(out),optional   :: error_return
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=:),allocatable   :: command
      integer                        :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
      if(present(error_return))then
         error_return=0
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      call get_command_arguments(command,ier)
      if(debug)then
         write(*,*)'KRACKEN ',trim(command)
      endif
      if(ier.ne.0)then
         call journal("*kracken* could not get command line arguments")
         if(present(error_return))error_return=ier
      else
         call dissect(verb,string,command,ier)
         ! if calling procedure is not testing error flag stop program on error
         if(.not.present(error_return).and.ier.ne.0)then
            call journal("*kracken* (V 20151212) STOPPING: error parsing arguments")
            stop
         endif
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine kracken
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          setprompts(3f) - [ARGUMENTS:M_kracken]set explicit prompts for keywords in interactive mode
!!##SYNOPSIS
!!
!!   subroutine setprompts(verb,init)
!!
!!    character(len=*),intent(in):: verb
!!    character(len=*),intent(in):: init
!!
!!##DESCRIPTION
!!
!!    Optionally set prompts for interactive prompting mode.
!!    The syntax of the call is the same as for KRACKEN(3f)/DISSECT(3f) except that prompt
!!    strings are given instead of default values. It is called after a call to KRACKEN(3f)
!!    or DISSECT(3f).
!!
!!##OPTIONS
!!    verb    name to define prompts for
!!    string  to define prompts instead of values
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine setprompts(verb,init)
character(len=*),parameter :: ident="@(#)M_kracken::setprompts(3f): set explicit prompts for keywords in interactive mode"
character(len=*),intent(in):: verb   ! verb name to define prompts for
character(len=*),intent(in):: init   ! string to define prompts instead of values
      call parse('?'//trim(verb),init,"add") ! initialize command, prefixing verb with question mark character to designate prompts
end subroutine setprompts
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          dissect(3f) - [ARGUMENTS:M_kracken]convenient call to parse() -- define defaults, then process
!!
!!##SYNOPSIS
!!
!!   subroutine dissect(verb,init,pars,error_return)
!!
!!    character(len=*),intent(in)  :: verb
!!    character(len=*),intent(in)  :: init
!!    character(len=*),intent(in)  :: pars
!!    integer,intent(out),optional :: error_return
!!##DESCRIPTION
!!
!!##OPTIONS
!!    VERB          the name of the command to be reset/defined
!!    INIT          used to define command options; usually hard-set in the program.
!!    PARS          defines the command options to be set, usually from user input
!!
!!##RETURNS
!!    ERROR_RETURN  error code. If zero no error occurred.
!!
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine dissect(verb,init,pars,error_return)
character(len=*),parameter :: ident="@(#)M_kracken::dissect(3f): convenient call to parse() -- define defaults, then process"
character(len=*),intent(in)  :: verb                     ! the name of the command to be reset/defined  and then set
character(len=*),intent(in)  :: init                     ! used to define or reset command options; usually hard-set in the program.
character(len=*),intent(in)  :: pars                     ! defines the command options to be set, usually from a user input file
integer,intent(out),optional :: error_return
!-----------------------------------------------------------------------------------------------------------------------------------
      integer                :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
      call store(trim(verb)//'_?','.false.',"add",ier)   ! all commands have the option -? to invoke prompt mode
      call parse(trim(verb),init,"add")                  ! initialize command
!-----------------------------------------------------------------------------------------------------------------------------------
      call parse(verb,pars,"no_add",ier)                 ! process user command options
      if(lget(trim(verb)//'_?'))then                     ! if -? option was present prompt for values
         call menu(verb)
      endif
      if(present(error_return))error_return=ier
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine dissect
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          parse(3f) - [ARGUMENTS:M_kracken]parse user command and store tokens into Language Dictionary
!!
!!##SYNOPSIS
!!
!! subroutine parse(verb,string,allow,error_return)
!!
!!    character(len=*),intent(in)     ::  verb
!!    character(len=*),intent(in)     ::  string
!!    character(len=*),intent(in)     ::  allow
!!    integer,optional,intent(out)    ::  error_return
!!
!!##DESCRIPTION
!!    given a string of form
!!
!!      value  -var value -var value
!!
!!    define variables of form
!!
!!      verb_var(i) = value
!!
!!    --var will become verb__var
!!
!!    o  values may be in double quotes if they contain alphameric characters
!!    o  a # signifies the rest of the line is a comment
!!    o  adjacent double quotes put one double quote into value
!!    o  processing ends when an unquoted semi-colon or end of string is encountered
!!    o  the variable name for the first value is verb_init (often verb_oo)
!!    o  leading and trailing blanks are removed from values
!!    o  call it once to give defaults
!!    o  call it again and vars without values are set to null strings
!!
!!##OPTIONS
!!
!!    VERB          command name to process
!!    STRING        string is character input string with first verb removed (options + other commands)
!!    ALLOW         keyword indicating whether commands may be added or only replaced
!!    ERROR_RETURN  error code. If zero, no error occurred
!!
!!##RETURNS
!!
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine parse(verb,string,allow,error_return)
character(len=*),parameter :: ident="@(#)M_kracken::parse(3f):parse user command and store tokens into Language Dictionary"
!!!   set up odd for future expansion
!!!   need to handle a minus followed by a blank character
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
!     SPECIAL FORM:
!        VERB="MODE"
!           then STRING is a special keyword used to identify a special mode to set
!           and ALLOW is a value used to set the mode
!-----------------------------------------------------------------------------------------------------------------------------------
!     for left-over command string for Language routines
!     optionally needed if you are going to allow multiple commands on a line
      ! number of characters left over,
      ! number of non-blank characters in actual parameter list
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)     ::  verb   ! command name to process
character(len=*),intent(in)     ::  string ! string is character input string with first verb removed (options + other commands)
character(len=*),intent(in)     ::  allow  ! keyword indicating whether commands may be added or only replaced
integer,optional,intent(out)    ::  error_return
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=IPvalue+2)             ::  dummy  ! working copy of string
character(len=IPvalue),dimension(2)  ::  var
character(len=3)                     ::  delmt
character(len=2)                     ::  init
character(len=1)                     ::  currnt  ! current character being processed
character(len=1)                     ::  prev    ! character to left of CURRNT
character(len=1)                     ::  forwrd  ! character to right of CURRNT
character(len=IPvalue)               ::  val
character(len=IPverb)                ::  name
integer,dimension(2)                 ::  ipnt
integer,save                         ::  ileave=1 ! if 0, leave " where you find them; else if 1 remove them. Normally removed
integer                              ::  ilist
integer                              ::  ier
integer                              ::  islen
integer                              ::  ipln
integer                              ::  ipoint
integer                              ::  itype
integer                              ::  ifwd
integer                              ::  ibegin
integer                              ::  iend
!-----------------------------------------------------------------------------------------------------------------------------------
   leftover=" "
   current_command_length=0
   ilist=1
   init="oo"
   ier=0
   if(present(error_return)) error_return=0
   islen=len_trim(string)                               ! find number of characters in input string
   if(islen  ==  0)then                                 ! if input string is blank, even default variable will not be changed
      return
   endif
   dummy=string
   ipln=len_trim(verb)             ! find number of characters in verb prefix string
   dict_calls=0                    ! clear number of times this keyword stored on a call to parse
                                      ! should more efficiently only do this for current VERB instead of entire array in dictionary
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   if(subscript(trim(verb)//'_?') .le. 0 )then          ! assuming if adding this is initial call
      call store(trim(verb)//'_?','.false.',"add",ier)  ! all commands have the option -? to invoke prompt mode
   elseif(allow.eq.'add')then
      call store(trim(verb)//'_?','.false.',"add",ier)  ! all commands have the option -? to invoke prompt mode
   endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   if(subscript(trim(verb)//'_>') .le. 0 )then          ! assuming if adding this is initial call
      call store(trim(verb)//'_>','#N#',"add",ier)      ! all commands have the option -> to write journal(3f) output
   elseif(allow.eq.'add')then
      call store(trim(verb)//'_>','#N#',"add",ier)      ! all commands have the option -> to write journal(3f) output
   endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!  Process special mode-setting calls
   if(verb(:ipln)=="MODE")then
      if(string=="LEAVEQUOTES")then
         if(allow=="YES")then
            ileave=0
         elseif(allow=="NO")then
            ileave=1
         else
            call journal("*parse* LEAVEQUOTES value bad")
            ileave=1
         endif
      else
         call journal("*parse* UNKNOWN MODE")
      endif
      return
   endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   var(2)=init         ! initial variable name
   var(1)=" "          ! initial value of a string
   ipoint=0            ! ipoint is the current character pointer for (dummy)
   ipnt(2)=2           ! pointer to position in parameter name
   ipnt(1)=1           ! pointer to position in parameter value
   itype=1             ! itype=1 for value, itype=2 for variable
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   delmt="off"
   prev=" "
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   do
      ipoint=ipoint+1               ! move current character pointer forward
      currnt=dummy(ipoint:ipoint)   ! store current character into currnt
      ifwd=min(ipoint+1,islen)
      forwrd=dummy(ifwd:ifwd)       ! next character (or duplicate if last)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      if((currnt=="-".and.prev==" ".and.delmt == "off".and.index("0123456789.",forwrd) == 0).or.ipoint > islen)then
      ! beginning of a parameter name
         if(forwrd.eq.'-')then                      ! change --var to -var so "long" syntax is supported
            dummy(ifwd:ifwd)='_'
            ipoint=ipoint+1                         ! ignore second - instead
         endif
         if(ipnt(1)-1 >= 1)then
            ibegin=1
            iend=len_trim(var(1)(:ipnt(1)-1))
            do
               if(iend  ==  0)then                  ! len_trim returned 0, parameter value is blank
                  iend=ibegin
                  exit
               else if(var(1)(ibegin:ibegin) == " ")then
                  ibegin=ibegin+1
               else
                  exit
               endif
            enddo
            name=verb(:ipln)//"_"//var(2)(:ipnt(2))
            ! #A# means append
            ! #R# means retain previous value if any
            val=var(1)(ibegin:iend)
            if(val.eq.'"#R#"')then               ! special value saying to retain previous value so commands can remember last value
               if(subscript(name).le.0)then
                  call store(name,' ',allow,ier) ! store name and blank value
               endif
            else
               call store(name,val,allow,ier)    ! store name and it's value
            endif
            if(present(error_return).and.ier.ne.0)error_return=ier
         else
            name=verb(:ipln)//"_"//var(2)(:ipnt(2))
            val=" "                                 ! store name and null value
            call store(name,val,allow,ier)
            if(present(error_return).and.ier.ne.0)error_return=ier
         endif
         ilist=ilist+ipln+1+ipnt(2)
         ilist=ilist+1
         itype=2                          ! change to filling a variable name
         var(1)=" "                       ! clear value for this variable
         var(2)=" "                       ! clear variable name
         ipnt(1)=1                        ! restart variable value
         ipnt(2)=1                        ! restart variable name
         if(currnt.ne.' ')current_command_length=ipoint
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      elseif(currnt == kracken_comment .and. delmt == "off")then   ! rest of line is comment
         islen=ipoint
         dummy=" "
         prev=" "
         leftover=" "
         cycle
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      elseif(currnt.eq.';'.and.delmt.eq.'off')then ! rest of line is another command(s)
         if(islen-ipoint.gt.0)then
            leftover=dummy(ipoint+1:)
         else
            leftover=' '
         endif

         islen=ipoint
         dummy=" "
         prev=" "
         cycle
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      else       ! currnt is not one of the special characters
         ! the space after a keyword before the value
         if(currnt == " ".and.itype  ==  2)then
            ! switch from building a keyword string to building a value string
            itype=1
         ! beginning of a delimited parameter value
         elseif(currnt  ==  """".and.itype  ==  1)then
            ! second of a double quote, put quote in
            if(prev  ==  """")then
                var(itype)(ipnt(itype):ipnt(itype))=currnt
                ipnt(itype)=ipnt(itype)+1
                current_command_length=ipoint
                delmt="on"
            elseif(delmt  ==  "on")then     ! first quote of a delimited string
                delmt="off"
            else
                delmt="on"
            endif
            if(ileave  ==  0.and.prev /= """")then  ! leave quotes where found them
               var(itype)(ipnt(itype):ipnt(itype))=currnt
               ipnt(itype)=ipnt(itype)+1
               current_command_length=ipoint
            endif
         else     ! add character to current parameter name or parameter value
            var(itype)(ipnt(itype):ipnt(itype))=currnt
            ipnt(itype)=ipnt(itype)+1
            if(currnt /= " ")then
               current_command_length=ipoint
            endif
         endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
      prev=currnt
      if(ipoint <= islen)then
         cycle
      endif
      exit
   enddo
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   if(lget(trim(verb)//'_?'))then    ! if -? option was present prompt for values
      call menu(verb)
   endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
end subroutine parse
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          store(3fp) - [ARGUMENTS:M_kracken]add or replace dict. name's value (if allow='add' add name if necessary)
!!##SYNOPSIS
!!
!!   subroutine store(name1,value1,allow1,ier)
!!
!!    character(len=*),intent(in) :: name1
!!    character(len=*),intent(in) :: value1
!!    character(len=*),intent(in) :: allow1
!!    integer,intent(out)         :: ier
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!    NAME1    name in dictionary of form VERB_KEYWORD
!!    VALUE1   value to be associated to NAME1
!!    ALLOW1   flag to allow new VERB_KEYWORD name being added
!!
!!##RETURNS
!!    IER      flag if error occurs in adding or setting value
!!
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine store(name1,value1,allow1,ier)
character(len=*),parameter :: ident="@(#)M_kracken::store(3fp): replace dict. name's value (if allow='add' add name if necessary)"
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)           :: name1       ! name in dictionary of form VERB_KEYWORD
character(len=*),intent(in)           :: value1      ! value to be associated to NAME1
character(len=*),intent(in)           :: allow1      ! flag to allow new VERB_KEYWORD name being added
integer,intent(out)                   :: ier         ! flag if error occurs in adding or setting value
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                            :: ilen
   character(len=IPverb)              :: name
   integer                            :: indx
   character(len=10)                  :: allow
   character(len=IPvalue)             :: value
   character(len=IPvalue)             :: mssge       ! the  message/error/string  value
   integer                            :: nlen
   integer                            :: new
   integer                            :: ii
   integer                            :: i10
!-----------------------------------------------------------------------------------------------------------------------------------
   value=" "
   name=" "
   allow=" "
   name=name1                                        ! store into a standard size variable for this type
   value=value1                                      ! store into a standard size variable for this type
   allow=allow1                                      ! store into a standard size variable for this type
   nlen=len(name1)
!-----------------------------------------------------------------------------------------------------------------------------------
   call bounce(name,indx,dict_verbs,ier,mssge)       ! determine storage placement of the variable and whether it is new
   if(ier  ==  -1)then                               ! an error occurred in determining the storage location
      call journal("error occurred in *store*")
      call journal(mssge)
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(indx > 0)then                                  ! found the variable name
      new=1
   elseif(indx <= 0.and.allow  ==  "add")then        ! check if the name needs added
      call add_string(name,nlen,indx,ier)            ! adding the new variable name in the variable name array
      if(ier  ==  -1)then
         call journal("*store* could not add "//name(:nlen))
         call journal(mssge)
         return
      endif
      new=0
!-----------------------------------------------------------------------------------------------------------------------------------
   else                                              ! did not find variable name but not allowed to add it
      ii=index(name,"_")
      call journal("########################################################")
      call journal("error: UNKNOWN OPTION -"//name(ii+1:))
      if(ii > 0)then
         call journal(name(:ii-1)//" parameters are")
         do i10=1,IPic
            if(name(:ii)  ==  dict_verbs(i10)(:ii))then
               if(dict_verbs(i10)(ii:ii+1).eq.'__')then
                  call journal(" --"//dict_verbs(i10)(ii+2:len_trim(dict_verbs(i10)))//" "//dict_vals(i10)(:dict_lens(i10)))
               else
                  call journal(" -"//dict_verbs(i10)(ii+1:len_trim(dict_verbs(i10)))//" "//dict_vals(i10)(:dict_lens(i10)))
               endif
            endif
         enddo
      endif
      call journal("########################################################")
      ier=-10
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ! ignore special value that means leave alone, used by 'set up' calls to leave a value alone
   ! note that this will prevent the keyword from being defined.
   if(value(1:4)  ==  "@LV@")then
      ! a leave-alone flag (for use by a 'defining' call)
      if(new  ==  0) then
         value=value(5:)                                                              ! trim off the leading @LV@
         if(dict_calls(iabs(indx)).eq.0.or.dict_vals(iabs(indx)).eq.' ')then
            dict_vals(iabs(indx))=value                                               ! store a defined variable's value
         else
            dict_vals(iabs(indx))= trim(dict_vals(iabs(indx)))//' '//value            ! append a defined variable's value
         endif
         dict_lens(iabs(indx))= len_trim(dict_vals(iabs(indx)))                       ! store length of string
         dict_calls(iabs(indx))=dict_calls(iabs(indx))+1                              ! detect duplicate use of a keyword
      endif
   else
      if(dict_calls(iabs(indx)).eq.0.or.dict_vals(iabs(indx)).eq.' ')then
         dict_vals(iabs(indx))=value                                                  ! store a defined variable's value
      else
         dict_vals(iabs(indx))= trim(dict_vals(iabs(indx)))//' '//value               ! append a defined variable's value
      endif
      dict_lens(iabs(indx))= len_trim(dict_vals(iabs(indx)))                          ! store length of string
      dict_calls(iabs(indx))=dict_calls(iabs(indx))+1                                 ! detect duplicate use of a keyword
   endif
   !---------------------------------------------------!
   !()()()()()()()()()()-                              !
   !---------------------                              !
   ! assume suffix _> is used to open file for journal()   !
   ! special-purpose just for USH.                     !
   ilen=len_trim(name)                                 !
      if(ilen.ge.2)then                                !
         if(name(ilen-1:ilen).eq.'_>')then             !
         if(value.ne."#N#")then                        !
            call journal('N',value)                    !
         endif                                         !
      endif                                            !
   endif                                               !
   !---------------------                              !
   !()()()()()()()()()()-                              !
   !---------------------------------------------------!
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine store
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          bounce(3fp) - [ARGUMENTS:M_kracken]find index in Language Dictionary where VARNAM can be found
!!##SYNOPSIS
!!
!!   subroutine bounce(varnam,index,dictionary,ier,mssge)
!!
!!    character(len=*),intent(in)              :: varnam
!!    integer,intent(out)                      :: index
!!    character(len=*),dimension(:),intent(in) :: dictionary
!!    integer,intent(out)                      :: ier
!!    character(len=*),intent(out)             :: mssge
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!    VARNAM      variable name to look up in dictionary
!!    DICTIONARY  sorted dictionary array to find varnam in
!!
!!##RETURNS
!!    INDEX       location where variable is or should be
!!    IER         error code
!!    MSSGE       message to describe error code
!!
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine bounce(varnam,index,dictionary,ier,mssge)
character(len=*),parameter :: ident="@(#)M_kracken::bounce(3fp): find index in Language Dictionary where VARNAM can be found"
!
!  If VARNAM is not found report where it should be placed as a NEGATIVE index number.
!  Assuming DICTIONARY is an alphabetized array
!  Assuming all variable names are lexically greater than a blank string.
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)                :: varnam      ! variable name to look up in dictionary
integer,intent(out)                        :: index       ! location where variable is or should be
character(len=*),dimension(:),intent(in)   :: dictionary  ! sorted dictionary array to find varnam in
integer,intent(out)                        :: ier
character(len=*),intent(out)               :: mssge
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                                 :: maxtry      ! maximum number of tries that should be required
   integer                                 :: imin
   integer                                 :: imax
   integer                                 :: i10
!-----------------------------------------------------------------------------------------------------------------------------------
   maxtry=int(log(float(IPic))/log(2.0)+1.0)              ! calculate max number of tries required to find a conforming name
   index=(IPic+1)/2
   imin=1
   imax=IPic
!-----------------------------------------------------------------------------------------------------------------------------------
   do i10=1,maxtry
      if(varnam  ==  dictionary(index))then
         return
      else if(varnam > dictionary(index))then
         imax=index-1
      else
         imin=index+1
      endif
      if(imin > imax)then
         index=-imin
         if(iabs(index) > IPic)then
            mssge="error 03 in bounce"
            ier=-1
            return
         endif
         return
      endif
      index=(imax+imin)/2
      if(index > IPic.or.index <= 0)then
         mssge="error 01 in bounce"
         ier=-1
         return
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   mssge="error 02 in bounce"
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine bounce
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          add_string(3fp) - [ARGUMENTS:M_kracken]Add new string name to Language Library dictionary
!!
!!##SYNOPSIS
!!
!!   subroutine add_string(newnam,nchars,index,ier)
!!
!!    character(len=*),intent(in) :: newnam
!!    integer,intent(in)          :: nchars
!!    integer,intent(in)          :: index
!!    integer,intent(out)         :: ier
!!##DESCRIPTION
!!
!!##OPTIONS
!!    NEWNAM  new variable name to add to dictionary
!!    NCHARS  number of characters in NEWNAM
!!
!!##RETURNS
!!
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine add_string(newnam,nchars,index,ier)
character(len=*),parameter :: ident="@(#)M_kracken::add_string(3fp): Add new string name to Language Library dictionary"
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
character(len=*),intent(in)       :: newnam     ! new variable name to add to dictionary
integer,intent(in)                :: nchars     ! number of characters in NEWNAM
integer,intent(in)                :: index
integer,intent(out)               :: ier
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   integer                     :: istart
   integer                     :: i10
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   istart=iabs(index)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
!  if last position in the name array has already been used, then report no room is left and set error flag and error message.
   if(dict_verbs(IPic) /= " ")then                                                  ! check if dictionary full
      call journal("*add_string* no room left to add more string variable names")
      ier=-1
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   elseif(istart.gt.IPic)then
      call journal("*add_string* dictionary size exceeded")
      ier=-1
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   else                                                                             ! pull down the array to make room for new value
      do i10=IPic-1,istart,-1
         dict_vals(i10+1)=dict_vals(i10)
         dict_lens(i10+1)=dict_lens(i10)
         dict_verbs(i10+1)=dict_verbs(i10)
         dict_calls(i10+1)=dict_calls(i10)
      enddo
      dict_vals(istart)=" "
      dict_lens(istart)= 0
      dict_calls(istart)= 0
      dict_verbs(istart)=newnam(1:nchars)
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
end subroutine add_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          subscript(3fp) - [ARGUMENTS:M_kracken]return the subscript value of a string when given it's name
!!##SYNOPSIS
!!
!!   function subscript(chars0)
!!
!!    character(len=*),intent(in) :: chars0
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
function subscript(chars0)
character(len=*),parameter :: ident="@(#)M_kracken::subscript(3fp): return the subscript value of a string when given it's name"
!  WARNING: only request value of names known to exist
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in)        :: chars0
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=IPverb)              :: chars
   character(len=IPvalue)             :: mssge
   integer                            :: ierr
   integer                            :: index
   integer                            :: subscript
!-----------------------------------------------------------------------------------------------------------------------------------
   chars=chars0
   index=0
   ierr=0
   call bounce(chars,index,dict_verbs,ierr,mssge)                        ! look up position
!-----------------------------------------------------------------------------------------------------------------------------------
   if((ierr  ==  -1).or.(index <= 0))then
      !call journal("*subscript* variable "//trim(chars)//" undefined")
      subscript=-1                                                       ! very unfriendly subscript value
   else
      subscript=index
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function subscript
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          get_command_arguments(3fp) - [ARGUMENTS:M_kracken]return all command arguments as an allocated string
!!
!!##SYNOPSIS
!!
!!   subroutine get_command_arguments(string,istatus)
!!
!!    character(len=:),allocatable,intent(out) :: string
!!    integer,intent(out)                      :: istatus
!!##DESCRIPTION
!!
!!##RETURNS
!!    STRING  composed of all command arguments concatenated into a string
!!    ISTATUS status (non-zero means error)
!!
!!##EXAMPLE
!!
!!
!!   Sample usage if procedure is made public for debugging:
!!
!!    program testit
!!    use M_journal, only : journal
!!    implicit none
!!    integer :: ier
!!    character(len=:),allocatable :: cmd
!!    call get_command_arguments(cmd,ier)
!!    write(*,*)'CMD=',trim(cmd)
!!    write(*,*)'LEN(CMD)=',len(cmd)
!!    write(*,*)'IER=',ier
!!    end program testit
!===================================================================================================================================
subroutine get_command_arguments(string,istatus)
character(len=*),parameter :: ident="@(#)M_kracken::get_command_arguments(3fp): return all command arguments as an allocated string"
!  try to guess original quoting and reintroduce quotes
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=:),allocatable,intent(out) :: string            ! string of all arguments to create
integer,intent(out)                      :: istatus           ! status (non-zero means error)
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: max_string_len             ! allowed length of output string
   integer                      :: i                          ! loop count
   character(len=:),allocatable :: value                      ! store individual arguments one at a time
   integer                      :: ilength                    ! length of individual arguments
   character(len=1024)          :: deallocate_error_message
   integer                      :: deallocate_status
!-----------------------------------------------------------------------------------------------------------------------------------
   call get_command(LENGTH=max_string_len, STATUS=istatus)
   if(istatus > 0)then
     STOP "*get_command_arguments* error: could not retrieve command line"
   elseif (max_string_len == 0) then
     STOP "*get_command_arguments* error: could not determine command length"
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   allocate(character(len=max_string_len) :: value)           ! no single argument should be longer than entire command length
   istatus=0                                                  ! initialize returned error code
   string=""                                                  ! initialize returned output string
!-----------------------------------------------------------------------------------------------------------------------------------
   APPEND_ARGS: do i=1,command_argument_count()               ! append any arguments together
      call get_command_argument(i,value,ilength,istatus)      ! get next argument
      if(istatus /= 0) then                                   ! stop program on error
         call journal('sc','*get_command_arguments* error obtaining argument ',i)
         exit APPEND_ARGS
      elseif(ilength.gt.0)then
         !---------------------
         ! BEGIN GUESS AT RE-QUOTING STRING
         ! if argument contains a space and does not contain a double-quote and is short enough to have double quotes added
         ! assume this argument was quoted but that the shell stripped the quotes and add double quotes. This is an optional
         ! behavior and assumes an operating system that strips the quotes from quoted strings on the command line. If the
         ! operating system is smarter than that remove this section
         if(index(value(:ilength),' ').ne.0.and.index(value(:ilength),'"').eq.0)then
            if((ilength+2).le.len(value))then
               string=string//' "'//value(:ilength)//'"'
            endif
         ! END GUESS AT RE-QUOTING STRING
         !---------------------
         else
            string=string//' '//value(:ilength) ! append strings together
         endif
      endif
   enddo APPEND_ARGS
!-----------------------------------------------------------------------------------------------------------------------------------
   deallocate(value,stat=deallocate_status,errmsg=deallocate_error_message) ! should be automatically removed in newer compilers
   if(deallocate_status.ne.0)then
      call journal('*get_command_arguments *'//trim(deallocate_error_message))
   endif
end subroutine get_command_arguments
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          menu(3fp) - [ARGUMENTS:M_kracken]prompt for values using a menu interface
!!##SYNOPSIS
!!
!!   subroutine menu(verb)
!!
!!    character(len=*),intent(in)  :: verb
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine menu(verb)
character(len=*),parameter   :: ident="@(#)M_kracken::menu(3fp):prompt for values using a menu interface"
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: verb
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=IPvalue)    :: reply
   character(len=IPvalue)    :: prompt
   integer                   :: ii
   integer                   :: icount
   integer                   :: ios
   integer                   :: i10
   integer                   :: i20
   integer                   :: istart
   integer                   :: iend
   integer                   :: iend_OK   ! last open actually printed
   integer                   :: ifound
   integer                   :: ireply
   integer                   :: ivalu
   integer                   :: ierr
   integer                   :: index
   character(len=IPvalue)    :: mssge     ! the message/error/string  value returned by BOUNCE(3f)
   character(len=1)          :: prefix
   integer                   :: icurrent  ! current menu item
   integer                   :: icmd
   integer                   :: imenu
   character(len=80),allocatable :: help_text(:)
   integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   stop_command=.false.
   ii=len_trim(verb)
   call journal(verb(:ii)//" parameters are")
   istart=1
   icurrent=1
!-----------------------------------------------------------------------------------------------------------------------------------
   INFINITE: do
      icount=0                                                ! how many entries in the dictionary belong to this command
      iend=IPic                                               ! last dictionary entry to search for current command
      iend_OK=istart
      MAKEMENU: do i10=istart,iend                            ! search dictionary for keywords for current command
         if(verb(:ii)//'_'  ==  dict_verbs(i10)(:ii+1))then   ! found part of the desired command
            if(istart.eq.0)then
               istart=i10                                     ! store index to the beginning of this command
               icurrent=i10
            endif
            icount=icount+1                                   ! count keywords that start with VERB_
            if(dict_verbs(i10).eq.verb(:ii)//'_?')then        ! do not show the keyword VERB_?
               cycle MAKEMENU
            endif
            call bounce('?'//dict_verbs(i10),index,dict_verbs,ierr,mssge) ! if ?VERB is defined assume it is a prompt
            if(index.gt.0)then
               prompt=dict_vals(index)
            else
               prompt=' '
            endif
            if(i10.eq.icurrent)then
              prefix='>'
            else
              prefix=' '
            endif
            imenu=i10-istart+1
            if(prompt.eq.'')then
               write(*,'(a,i4,")",a,a)') prefix,imenu,dict_verbs(i10)(ii+2:),trim(dict_vals(i10)(:dict_lens(i10)))
               iend_OK=i10
            elseif(prompt.eq.'#N#'.or.prompt.eq.'"#N#"')then                 ! special prompt value which means to skip prompting
            else
               write(*,'(a,i4,")",a,":[",a,"]")') prefix,imenu,trim(prompt),trim(dict_vals(i10))
               iend_OK=i10
            endif
         endif
      enddo MAKEMENU
      iend=icount+istart-1                                 ! no need to go thru entire dictionary on subsequent passes
!-----------------------------------------------------------------------------------------------------------------------------------
      write(*,'(a)',advance='no')'Enter parameter number to change("RETURN" to finish):'
      read(*,'(a)',iostat=ios)reply
      reply=adjustl(reply)
      ivalu=-1
!-----------------------------------------------------------------------------------------------------------------------------------
      select case(REPLY(1:1))
!-----------------------------------------------------------------------------------------------------------------------------------
      case('-')  ! if it starts with a - assume it is a new specification of the arguments
         call parse(verb,trim(reply)//' -? .false.',"no_add")
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('@')                                        ! debug option to dump dictionary
         do i20=1,IPic
            if(dict_verbs(i20).ne.' ')then
                 write(*,*)i20,trim(dict_verbs(i20)),trim(dict_vals(i20)(:dict_lens(i20)))
            endif
         enddo
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('#')                                                                      ! ignore
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case(' ','q','e','0')                                                          ! exit menu changes
         exit INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('x','.')                                                                  ! return value to indicate command has been stopped
         stop_command=.true.
         exit INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('!')                                                                      ! call system
         call execute_command_line(trim(reply(2:)), exitstat=icmd)                   ! execute system command
         if(icmd.ne.0)then                                                           ! if system command failed exit program
            call journal('sc','*m_kracken:menu* ERROR - SYSTEM COMMAND FAILED:',icmd)
         endif
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('?','h','i')                                                              ! get help information
      help_text=[ CHARACTER(LEN=80) ::                                                       &
!        &'12345678901234567890123456789012345678901234567890123456789012345678901234567890',&
         &'#------------------------------------------------------------------------------#',&
         &'| How to change parameter options by number:                                   |',&
         &'|  o  NNN              the number of the menu option to change the value of    |',&
         &'#------------------------------------------------------------------------------#',&
         &'| How to change parameter options by respecifying them:                        |',&
         &'|  o  -key1 value1 -key2 value2 ...                                            |',&
         &'|                      to respecify values using original specification style  |',&
         &'#------------------------------------------------------------------------------#',&
         &'# Working on the current keyword(identified by a ">" prefix in the menu):      |',&
         &'|  o  c                change current option value with command-line editor    |',&
         &'|  o  n                change "current" to next menu option                    |',&
         &'|  o  p                change "current" to previous menu option                |',&
!        &'|  o  /name            change "current" option to keyword "name"               |',&
         &'#------------------------------------------------------------------------------#',&
         &'| Exit menu mode:                                                              |',&
         &'|  o   |q|e|0          a RETURN on a blank line or the indicated characters    |',&
         &'|                      exits the menu and processes the command                |',&
         &'|  o  .                indicate to program to ignore command (may be ignored ) |',&
         &'#------------------------------------------------------------------------------#',&
         &'| Special functions:                                                           |',&
         &'|  o  !command         execute system command                                  |',&
         &'|  o  ?|i|h            display this help                                       |',&
         &'#------------------------------------------------------------------------------#']
         WRITE(*,'(a)')(help_text(i),i=1,size(help_text))
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('1','2','3','4','5','6','7','8','9')
         ivalu=iget(reply)
         ivalu=ivalu+istart-1
!-----------------------------------------------------------------------------------------------------------------------------------
      case('c')                                            ! change current menu item
         ivalu=icurrent
!-----------------------------------------------------------------------------------------------------------------------------------
      case('d')                                            ! turn on debug mode
         debug=.true.
         call journal('*menu* DEBUG: debug mode on')
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('D')                                            ! turn off debug mode
         debug=.false.
         call journal('*menu* DEBUG: debug mode off')
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('p')                                            ! change previous menu item
         icurrent=icurrent-1
         icurrent=max(icurrent,istart)
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('v')                                            ! show version
        !call journal('Version 20140403')
        !call journal('Version 20151229')
         call journal('Version 20160414')
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('n')                                            ! change next menu item
         icurrent=icurrent+1
         if(debug)then
             call journal('sc','*menu* DEBUG: ICURRENT=',icurrent)
             call journal('sc','*menu* DEBUG: ISTART=',istart)
             call journal('sc','*menu* DEBUG: IEND=',iend)
             call journal('sc','*menu* DEBUG: IEND_OK=',iend_ok)
         endif
         icurrent=min(icurrent,iend_OK)
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case default
         call journal(' Unrecognized selection (? for help)')
         cycle INFINITE
      end select
!-----------------------------------------------------------------------------------------------------------------------------------
      ireply=ivalu
!-----------------------------------------------------------------------------------------------------------------------------------
      if((ivalu.lt.istart).or.(ivalu.gt.iend))then
         write(*,*)'illegal menu choice ',istart,'<=',ivalu,'<=',iend, ' (enter "?" for help)'
!-----------------------------------------------------------------------------------------------------------------------------------
      else
         ifound=ireply                                                    ! index into dictionary for requested keyword and value
         if(dict_verbs(ifound).eq.verb(:ii)//'_?')then                    ! replaced this with FINISHED so exit
            exit INFINITE
         endif
         call bounce('?'//dict_verbs(ifound),index,dict_verbs,ierr,mssge) ! if ?VERB is defined assume it is a prompt
         if(index.gt.0)then
            prompt=dict_vals(index)
         else
            prompt=' '
         endif
         if(prompt.eq.'')then
            write(*,'("Enter value for ",a,":")',advance='no') trim(dict_verbs(ifound)(ii+2:))
         elseif(prompt.eq.'#N#'.or.prompt.eq.'"#N#"')then                 ! special prompt value
         else
            write(*,'(a,":")',advance='no') trim(prompt)
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
         read(*,'(a)',iostat=ios)reply
         call store(dict_verbs(ifound),reply,"no_add",ierr)
!-----------------------------------------------------------------------------------------------------------------------------------
      endif
   enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
   call store(trim(verb)//'_?','.false.',"add",ierr)                      ! all commands have the option -? to invoke prompt mode
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine menu
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!          show(3f) - [ARGUMENTS:M_kracken]dump dictionary entries
!!
!!##SYNOPSIS
!!
!!   subroutine show(VERB_NAME0,VERBS_ONLY,IWIDE0)
!!
!!    character(len=*),intent(in)   :: VERB_NAME0
!!    logical,intent(in)            :: VERBS_ONLY
!!    integer,intent(in)            :: iwide0
!!##DESCRIPTION
!!    Write information about a command from the command dictionary or list all the
!!    command verbs in the dictionary
!!
!!##OPTIONS
!!    VERB_NAME0   verb prefix to display. Default is all
!!    VERBS_ONLY   flag to show verbs only
!!    IWIDE0       if .ge. zero, how many columns wide to show just verbs
!!
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine show(VERB_NAME0,VERBS_ONLY,IWIDE0)
character(len=*),parameter    :: ident="@(#)M_kracken::show(3f):dump dictionary entries"
character(len=*),intent(in)   :: VERB_NAME0     ! verb prefix to display. Default is all
logical,intent(in)            :: VERBS_ONLY     ! flag to show verbs only
integer,intent(in)            :: iwide0         ! if .ge. zero, how many columns wide to show just verbs
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=IPvalue)     :: VERB_NAME      ! verb prefix to display. Default is all
   character(len=IPvalue)     :: message
   integer                    :: keyword_length
   integer                    :: i
   integer                    :: ii
   integer                    :: ich
   integer                    :: ii71b
   integer                    :: ilens
   integer                    :: istart
   integer                    :: iwide
   integer                    :: verb_length
!-----------------------------------------------------------------------------------------------------------------------------------
   keyword_length=len(dict_verbs(1))                       ! get the length of dictionary keyword names
   ii71b=len(message)-keyword_length-4                     ! assuming DICT_VERBS(ii) can fit into message
   iwide=iwide0
   if(iwide.le.0)iwide=80
   VERB_NAME=VERB_NAME0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(VERBS_ONLY)then                                      ! show all verbs
      message=' '
      istart=1
      do i=IPic,1,-1                                       ! loop thru entire dictionary
         verb_length=len_trim(DICT_VERBS(i))
         if(verb_length.lt.3)cycle
         if(DICT_VERBS(i)(1:1).eq.'?')cycle                ! remove prompts
         if(DICT_VERBS(i)(1:1).eq.'_')cycle                ! remove initial values
         if(DICT_VERBS(i)(verb_length-2:).ne.'_oo') cycle  ! assume all commands have a VERB_oo value
         if(istart.gt.iwide-IPverb-1)then
               call journal(message)
               message=' '
               istart=1
         endif
         write(message(istart:istart+20),'(1x,a20)')DICT_VERBS(i)(:verb_length-3)
         istart=istart+21
      enddo
      if(istart.gt.1)then
         call journal(message)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(VERB_NAME.eq.' ')then                                   ! show all variables
     do i=1,IPic
        if(DICT_VERBS(i).ne.' ')then
           ii=max(1,dict_lens(i))                                 ! number of characters in corresponding dictionary VALUE
           if(ii.gt.ii71b)then                                    ! getting a little long, break it into two lines
              ilens=1+keyword_length+3
              write(message,'(3a)')' ',DICT_VERBS(i),' = '
              call journal(message(:ilens))
              ilens=ii
              write(message,'(a)')dict_vals(i)(:ii)
           else
              ilens=1+keyword_length+3+ii
              write(message,'(4a)')' ',DICT_VERBS(i),' = ', dict_vals(i)(:ii)
           endif
           call journal(message(:ilens))
        endif
     enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   else                                                        ! show only verb_ variables
      ich=index(VERB_NAME,' ')                                 ! VERB_NAME assumed longer than any verb name, so at least one space
      VERB_NAME(ich:ich)='_'
      SCAN_DICTIONARY: do i=1,IPic
         if(DICT_VERBS(i).eq.' ')cycle SCAN_DICTIONARY
         if(VERB_NAME(:ich).eq.DICT_VERBS(i)(:ich))then
            ii=max(1,dict_lens(i))                          ! number of characters in corresponding dictionary VALUE
            if(ii.gt.ii71b)then                             ! getting a little long, break it into two lines
               ilens=1+keyword_length+3
               write(message,'(3a)')' ',DICT_VERBS(i),' = '
               call journal(message(:ilens))
               ilens=ii
               write(message,'(a)')dict_vals(i)(:ii)
            else
               ilens=1+keyword_length+3+ii
               write(message,'(4a)')' ',DICT_VERBS(i),' = ', dict_vals(i)(:ii)
            endif
            call journal(message(:ilens))
         endif
      enddo SCAN_DICTIONARY
!-----------------------------------------------------------------------------------------------------------------------------------
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine show
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_kracken
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! HISTORY:
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20160414
! multiple uses of a keyword appends values together with a space in between rather than taking right-most definition
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20151228
! merged command-parsing module back into kracken. Makes kracken a little dirty and makes it require M_debug and M_journal
! but code is too similar in function to keep separate
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20151212
! allow cmd_oo to have default value
! requote token from the command line if it starts with - and has a space in it to make is possible to pass "-[^0-9-] values
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20131224
! minor cleanup
! updated 20131214
! added preliminary setprompts and menu as routines to explore prompting modes.
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20131206
! added optional error flag to KRACKEN(3f). If the error flag is not present, an error will cause the program to stop instead of
! always returning to the calling procedure.
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20131201
! create name CMD__NAME if --NAME is specified; so --version and --help are more easily used
! add dget(3f) function for returning doubleprecision values
! rename parse_two(3f) to dissect(3f) and make it public so input from sources other than
! command line arguments can be parsed easily.
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20131029
! read environment variable DEFAULT_CMD
! REMOVED
!-----------------------------------------------------------------------------------------------------------------------------------
