










!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_kracken
use M_verify,   only: debug, io_debug
use M_journal, only: journal
use M_strings, only: upper, string_to_value, split, s2v, atleast
use M_list,    only: locate, insert, replace
use M_args,    only: get_command_arguments_string, longest_command_argument
implicit none

! ident_1="@(#)M_kracken(3fm): parse command line options of Fortran programs using Unix-like syntax"

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
   private :: subscript_            ! return the subscript value of a string when given its name
   private :: menu                  ! generate an interactive menu when -? option is used
!-----------------------------------------------------------------------------------------------------------------------------------
   public test_suite_M_kracken
!-----------------------------------------------------------------------------------------------------------------------------------
! length of verbs and entries in Language dictionary
! NOTE:   many parameters may be  reduced in size so as to just accommodate being used as a command line parser.
!         In particular, some might want to change:
   logical,public            :: stop_command=.false.               ! indication to return stop_command as false in interactive mode
   integer,parameter,public  :: IPvalue=4096*16                    ! length of keyword value
   integer,parameter,public  :: IPverb=20                          ! length of verb
!-----------------------------------------------------------------------------------------------------------------------------------
   integer, parameter        :: dp = kind(0.d0)
   integer, parameter        :: k_int = SELECTED_INT_KIND(9)       ! integer*4
   integer, parameter        :: k_dbl = SELECTED_REAL_KIND(15,300) ! real*8
!-----------------------------------------------------------------------------------------------------------------------------------
   ! dictionary for Language routines
   character(len=:),allocatable               :: dict_vals(:)      ! contains the values of string variables
   character(len=:),allocatable               :: dict_verbs(:)     ! string variable names
   integer(kind=k_int),allocatable            :: dict_lens(:)      ! significant lengths of string variable values
   integer(kind=k_int),allocatable            :: dict_calls(:)     ! number of times this keyword stored on a call to parse
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=1),save,public               :: kracken_comment='#'
   character(len=:),public,allocatable,save   :: leftover          ! remaining command(s) on line
   integer,public,save                        :: current_command_length=0 ! length of options for current command
!-----------------------------------------------------------------------------------------------------------------------------------
   public :: cmd_args_to_dictionary
   public :: print_kracken_dictionary
   public unnamed
   public kracken_method
   character(len=:),allocatable,save :: unnamed(:)
   character(len=10),save            :: kracken_method='kracken'
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    RETREV(3f) - [ARGUMENTS:M_kracken] get keyword value as a string from a command's argument list processed by kracken(3f)
!!    (LICENSE:PD)
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
!!    When a command has had its command argument list parsed using the
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
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine retrev(name,val,len,ier)

! ident_2="@(#)M_kracken::retrev(3f): retrieve token value from Language Dictionary when given NAME"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)     :: name        ! name of variable to retrieve value for in form VERB_NAME
character(len=*),intent(out)    :: val         ! value for requested variable
   integer,intent(out)          :: len         ! position of last non-blank character in requested variable
   integer,intent(out)          :: ier         ! error flag 0=found requested variable; -1=entry not found
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                      :: isub        ! subscript in dictionary where requested entry and corresponding value are found
!-----------------------------------------------------------------------------------------------------------------------------------
   isub=subscript_(name)                       ! get index entry is stored at
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
!!    dget(3f) - [ARGUMENTS:M_kracken] given keyword fetch doubleprecision value from command argument
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function dget(keyword) result(value)
!!
!!     character(len=*),intent(in)  :: keyword
!!     doubleprecision              :: value
!!
!!##DESCRIPTION
!!     The dget(3f) function returns a scalar doubleprecision value from a command line
!!     argument using the M_kracken(3fm) module.
!!
!!##OPTIONS
!!     KEYWORD    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!                The VERB name comes from the first argument of the KRACKEN(3f)
!!                call. The KEYWORD is a keyword from the KRACKEN(3f) call that
!!                should be interpreted as a doubleprecision value.
!!
!!##RETURNS
!!     VALUE      doubleprecision value returned by function
!!
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
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function dget(keyword)

! ident_3="@(#)M_kracken::dget(3f): given keyword fetch dble value from Language Dictionary (zero on err)"

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
!!    rget(3f) - [ARGUMENTS:M_kracken] given keyword fetch real value from command argument
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function rget(keyword) result(value)
!!
!!     character(len=*),intent(in)  :: keyword
!!     real                         :: value
!!
!!##DESCRIPTION
!!     The rget(3f) function returns a scalar real value from a command line
!!     argument using the M_kracken(3fm) module.
!!
!!##OPTIONS
!!     KEYWORD    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!                The VERB name comes from the first argument of the KRACKEN(3f)
!!                call. The KEYWORD is a keyword from the KRACKEN(3f) call that
!!                should be interpreted as a REAL value.
!!
!!##RETURNS
!!     VALUE      real value returned by function
!!
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
!!
!!##SEE ALSO
!!    M_kracken(3fm), kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse,dissect,store,setprompts,show
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function rget(keyword)

! ident_4="@(#)M_kracken::rget(3f): given keyword fetch real value from language dictionary (zero on err)"

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
!!    iget(3f) - [ARGUMENTS:M_kracken] given keyword fetch integer value from command argument
!!    (LICENSE:PD)
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
!!
!!##SEE ALSO
!!    M_kracken(3fm), kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function iget(keyword)

! ident_5="@(#)M_kracken::iget(3f): given keyword fetch integer value from Language Dictionary (0 on err)"

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
!!    lget(3f) - [ARGUMENTS:M_kracken] given keyword fetch logical value from command arguments
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function lget(keyword) result(lval)
!!
!!     character(len=*),intent(in)  :: keyword
!!     logical                      :: lval
!!##DESCRIPTION
!!
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
!!                t, f, yes, no, y, n, .false., .true., .f., .t.,''". .TRUE. is returned
!!                if the corresponding string in the dictionary for KEYWORD is blank.
!!                .FALSE. is returned if a string not in the list is found.
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
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function lget(keyword)

! ident_6="@(#)M_kracken::lget(3f): given keyword fetch logical value from lang. dictionary (.f. on err)"

!-----------------------------------------------------------------------------------------------------------------------------------
logical                      :: lget                  ! procedure type
character(len=*),intent(in)  :: keyword               ! the dictionary keyword (in form VERB_KEYWORD) to retrieve
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=:),allocatable :: value              ! value corresponding to the requested keyword
!-----------------------------------------------------------------------------------------------------------------------------------
!  ignore a leading dot character. Then, any word starting in "T" or "Y" is true, any word starting in "F" or "N" is false.
   if(len(sget(keyword)).ne.0)then
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
               call journal("*lget* bad logical expression for "//trim(keyword)//'='//value)
            end select
         case default
               call journal("*lget* bad logical expression for "//trim(keyword)//'='//value)
         end select
      else                                            ! special value "#N#" is assumed FALSE
         lget=.false.
      endif
   else
      lget=.false.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function lget
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sget(3f) - [ARGUMENTS:M_kracken] given keyword fetch string value and length from command arguments
!!    (LICENSE:PD)
!!
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
!!
!!##RETURNS
!!     string  returned string. If LEN(STRING).EQ.0 an error occurred, such
!!             as NAME not being in the dictionary.
!!     ilen    optional length of returned output string
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_sget
!!    use M_kracken, only: kracken, sget
!!    implicit none
!!    character(len=:),allocatable :: string, a, b
!!      ! define command arguments and parse user command
!!      call kracken('demo','-string This is the default -a A default -b B default' )
!!      ! get any values specified on command line for -truth
!!      string=sget('demo_string')
!!      a=sget('demo_a')
!!      b=sget('demo_b')
!!      write(*,'("string is ",a)')trim(string)
!!      write(*,'("a is ",a)')trim(a)
!!      write(*,'("b is ",a)')trim(b)
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
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function sget(name,ilen) result(string)

! ident_7="@(#)M_kracken::sget(3f): Fetch string value and length of specified NAME from lang. dictionary"

!  This routine trusts that the desired name exists. A blank is returned if the name is not in the dictionary
character(len=:),allocatable  :: string      ! returned value
character(len=*),intent(in)   :: name        ! name to look up in dictionary
integer,intent(out),optional  :: ilen        ! length of returned output string
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                    :: isub        ! index where verb_oo is stored or -1 if this is an unknown name
!-----------------------------------------------------------------------------------------------------------------------------------
   isub=subscript_(name)                     ! given name return index name is stored at
!-----------------------------------------------------------------------------------------------------------------------------------
   if(isub > 0)then                          ! if index is valid return string
      string=trim(dict_vals(isub))
      if(len(string).eq.0)string=" "
   else                                      ! if index is not valid return blank string
      string=""
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
!!    dgets(3f) - [ARGUMENTS:M_kracken] given keyword fetch doubleprecision array from command arguments
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function dgets(keyword,ier) result(darray)
!!
!!     character(len=*),intent(in)   :: keyword
!!     doubleprecision,allocatable   :: DARRAY
!!     integer,optional,intent(iout) :: ier
!!
!!##DESCRIPTION
!!     The dgets(3f) function returns a dynamically allocated array of
!!     doubleprecision values from a string that is the value for a command
!!     line option. It is part of the M_kracken(3fm) module.
!!
!!     Values that cannot be read as a numeric value are returned as a NaN.
!!
!!##OPTIONS
!!     keyword  dictionary name to retrieve, of form VERB_NAME where VERB
!!              is taken from the first parameter of the call to KRACKEN(3f)
!!              or DISSECT(3f).
!!
!!##RETURNS
!!     darray   double precision numeric array returned by function. The array
!!              will have zero size if the parsed dictionary entry is blank.
!!     IER      If present and non-zero an error occurred in converting strings to a value
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_dgets
!!    use M_kracken, only: kracken, dgets
!!    implicit none
!!    doubleprecision,allocatable  :: vals(:)
!!    integer              :: i
!!    ! define command arguments and parse user command
!!    call kracken('demo','-nums 1 2 3 1000 100,000 11.11111 77.77777 -77.7777' )
!!    vals=dgets('demo_nums') ! get any values specified for -nums
!!    write(*,'(*(g0:,","))')( vals(i),i=1,size(vals)) ! print the values
!!    end program demo_dgets
!!
!!   Example program runs:
!!
!!    $ demo_dgets
!!     1.0000000000000000,2.0000000000000000,3.0000000000000000,
!!     1000.0000000000000,100000.00000000000,11.111110000000000,
!!     77.777770000000004,-77.777699999999996
!!
!!    $ demo_dgets -nums 89,123,456.789 10.9999999
!!     89123456.789000005,10.999999900000001
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function dgets(keyword,ier) result(darray)

! ident_8="@(#)M_kracken::dgets(3f): given keyword fetch dble value from Language Dictionary (0 on err)"

character(len=*),intent(in) :: keyword                      ! keyword to retrieve value for from dictionary
real(kind=dp),allocatable   :: darray(:)                    ! function type

   character(len=:),allocatable  :: carray(:)          ! convert value to an array using split(3f)
   integer                       :: i
   integer,optional              :: ier
   integer                       :: ier_local
!-----------------------------------------------------------------------------------------------------------------------------------
   if(sget(keyword).ne.' ')then
      call split(sget(keyword),carray)                      ! find value associated with keyword and split it into an array
   else
      allocate(character(len=0) :: carray(0))
   endif
   allocate(darray(size(carray)))                           ! create the output array
   ier_local=0
   if(present(ier))then
         ier=0
   endif
   do i=1,size(carray)
      call string_to_value(carray(i), darray(i), ier_local)       ! convert the string to a numeric value
      if(present(ier).and.ier_local.ne.0)then
         ier=ier_local
      endif
      !if(ier_local.ne.0)then
      !   !darray(i)=0.0d0
      !   deallocate(darray)
      !   allocate(darray(i-1))                              ! create the output array
      !   exit
      !endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end function dgets
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    igets(3f) - [ARGUMENTS:M_kracken] given keyword fetch integer array from command arguments
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function igets(keyword,ier) result(iarray)
!!
!!     character(len=*),intent(in)   :: keyword
!!     integer,allocatable           :: iarray(:)
!!     integer,optional,intent(iout) :: ier
!!
!!##DESCRIPTION
!!     The igets(3f) function returns a dynamically allocated array of integers
!!     from a string that is the value for a command line option. It is part of
!!     the M_kracken(3fm) module.
!!
!!     Values that cannot be read as an integer value are returned as a NaN.
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
!!                The array will have zero size if the parsed dictionary
!!     IER        If present and non-zero an error occurred in converting strings to a value
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_igets
!!    use M_kracken, only: kracken, igets
!!    implicit none
!!    doubleprecision,allocatable  :: vals(:)
!!    integer              :: i
!!    ! define command arguments and parse user command
!!    call kracken('demo','-nums 1 2 3 100 1000 10000 100,000 11.11111 77.77777 -77.7777' )
!!    vals=igets('demo_nums') ! get any values specified for -nums
!!    write(*,'(*(g0:,","))')( vals(i),i=1,size(vals)) ! print the values
!!    end program demo_igets
!!
!!   Example program runs:
!!
!!    $ demo_igets
!!    1,2,3,100,1000,10000,100000,11,77,-77
!!    $ demo_igets -val 89,123,456 10.9999999
!!    89123456,10
!!
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function igets(keyword,ier) result(iarray)

! ident_9="@(#)M_kracken::igets(3f): given keyword fetch integer array from string in dictionary(0 on err)"

character(len=*),intent(in) :: keyword             ! keyword to retrieve value for from dictionary
integer,allocatable         :: iarray(:)           ! convert value to an array
doubleprecision,allocatable :: darray(:)           ! convert value to an array
integer,optional            :: ier
   if(present(ier))then
      darray=dgets(keyword,ier)
      if(ier.eq.0)then
         iarray=int(darray)                           ! just call DGETS(3f) but change returned value to type INTEGER
      else
         iarray=[integer ::]
      endif
   else
      iarray=int(dgets(keyword))                   ! just call DGETS(3f) but change returned value to type INTEGER
   endif
end function igets
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    rgets(3f) - [ARGUMENTS:M_kracken] given keyword fetch real array from command arguments
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function rgets(keyword,ier) result(rarray)
!!
!!     character(len=*),intent(in)   :: keyword
!!     real,allocatable              :: rarray(:)
!!     integer,optional,intent(iout) :: ier
!!
!!##DESCRIPTION
!!     The rgets(3f) function returns a dynamically allocated array of real values
!!     from a string that is the value for a command line option. It is part of
!!     the M_kracken(3fm) module.
!!
!!     Values that cannot be read as a numeric value are returned as a NaN.
!!
!!##OPTIONS
!!     KEYWORD    the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!                The VERB name comes from the first argument of the KRACKEN(3f)
!!                call. The KEYWORD is a keyword from the KRACKEN(3f) call that
!!                should be interpreted as a list of REAL values.
!!
!!##RETURNS
!!     RARRAY     real array returned by function.
!!                The array will have zero size if the parsed dictionary
!!                entry is blank.
!!     IER        If present and non-zero an error occurred in converting strings to a value
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
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function rgets(keyword,ier) result(rarray)

! ident_10="@(#)M_kracken::rgets(3f): given keyword fetch real array from string in dictionary (0 on err)"

character(len=*),intent(in) :: keyword             ! keyword to retrieve value for from dictionary
real,allocatable            :: rarray(:)           ! convert value to an array
doubleprecision,allocatable :: darray(:)           ! convert value to an array
integer,optional            :: ier
   if(present(ier))then
      darray=dgets(keyword,ier)
      if(ier.eq.0)then
         rarray=real(darray)              ! just call DGETS(3f) but change returned value to type REAL
      else
         rarray=[real ::]
      endif
   else
      rarray=real(dgets(keyword))                  ! just call DGETS(3f) but change returned value to type REAL
   endif
end function rgets
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    lget(3f) - [ARGUMENTS:M_kracken] given keyword fetch logical array from command argument
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function lgets(keyword) result(lvals)
!!
!!     character(len=*),intent(in)  :: keyword
!!     logical,allocatable          :: lvals(:)
!!
!!##DESCRIPTION
!!     The lgets(3f) function returns a dynamically allocated array of logical values
!!     from a string that is the value for a command line option. It is part of
!!     the M_kracken(3fm) module.
!!
!!     Values that cannot be read as a logical value are returned as a ".FALSE.".
!!
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
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function lgets(keyword) result(larray)

! ident_11="@(#)M_kracken::lgets(3f): given keyword fetch logical array from string in dictionary(F on err)"

character(len=*),intent(in)  :: keyword                    ! the dictionary keyword (in form VERB_KEYWORD) to retrieve
logical,allocatable          :: larray(:)                  ! convert value to an array
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=:),allocatable       :: carray(:)         ! convert value to an array
   integer                            :: i
   integer                            :: ichar             ! point to first character of word unless first character is "."
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len(sget(keyword)).ne.0)then
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
   else
      allocate(larray(1))                                     ! allocate output array
      larray(1)=.false.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function lgets
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sgets(3f) - [ARGUMENTS:M_kracken] given keyword fetch string value parsed on whitespace into an array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function sgets(name,delim) result(strings)
!!
!!    character(len=*),intent(in) :: name
!!    character(len=*),intent(in),optional :: delim
!!    character(len=:),allocatable :: strings(:)
!!
!!##DESCRIPTION
!!     The sgets(3f) function returns a dynamically allocated array of character values
!!     from a string that is the value for a command line option. It is part of
!!     the M_kracken(3fm) module.
!!
!!##OPTIONS
!!     name     the dictionary keyword (in form VERB_KEYWORD) to retrieve.
!!              The VERB name comes from the first argument of the
!!              KRACKEN(3f) or DISSECT(3f) call. The KEYWORD is a keyword from the second
!!              argument to the KRACKEN(3f) or DISSECT(3f) call.
!!              This routine trusts that the desired name exists.
!!              If the name does not exist the array [char(0)] is returned.
!!              An array of zero size is returned if the string is blank.
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
!!    use M_kracken, only : kracken, sgets
!!    character(len=:),allocatable :: strings(:)
!!       call kracken('cmd',' -string    This   is  a sentence ')
!!       strings= sgets("cmd_string")            ! get -strings words
!!       print *, "string=",('['//trim(strings(i))//']',i=1,size(strings))
!!       print *, "len= ",len(strings)
!!       print *, "size=",size(strings)
!!    end program demo_sgets
!!
!!   Example program execution:
!!
!!    $ demo_sgets
!!     string=[This][is][a][sentence]
!!     len=            8
!!     size=           4
!!
!!    $ demo_sgets -string a b c d e f g
!!     string=[a][b][c][d][e][f][g]
!!     len=            1
!!     size=           7
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function sgets(name,delim) result(strings)

! ident_12="@(#)M_kracken::sgets(3f): Fetch strings value for specified NAME from the lang. dictionary"

! This routine trusts that the desired name exists. A blank is returned if the name is not in the dictionary
character(len=:),allocatable         :: strings(:)
character(len=*),intent(in)          :: name                       ! name to look up in dictionary
character(len=*),intent(in),optional :: delim

integer                              :: isub                      ! index where verb_oo is stored or -1 if this is an unknown name
!-----------------------------------------------------------------------------------------------------------------------------------
   isub=subscript_(name)                                          ! given name return index name is stored at
!-----------------------------------------------------------------------------------------------------------------------------------
   if(isub > 0)then                                               ! if index is valid return strings
      if(present(delim))then
         call split(dict_vals(isub),strings,delim)
      else
         call split(dict_vals(isub),strings)
      endif
   else                                                           ! if index is not valid return NULL string
      allocate(character(len=1) :: strings(1))
      strings(1)=char(0)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function sgets
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    kracken(3f) - [ARGUMENTS:M_kracken] crack command line options on Fortran programs, using "-KEYWORD VALUE" syntax
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     subroutine kracken(verb, string[,ierror][style])
!!
!!        character(len=*), intent(in) ::  verb
!!        character(len=*), intent(in) :: string
!!        integer, intent(out), optional :: ierror
!!        character(len=*), intent(in),optional :: style
!!
!!##DESCRIPTION
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
!!     STYLE    parsing style. Either 'kracken' or 'args'. The default
!!              is 'kracken'.
!!
!!##EXAMPLE
!!
!!
!!   Sample program:
!!
!!       program demo_kracken
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
!!       contains
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
!!       end program demo_kracken
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
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine kracken(verb,string,error_return,style)

! ident_13="@(#)M_kracken::kracken(3f): define and parse command line options"

!  get the entire command line argument list and pass it and the prototype to dissect()
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)          :: string
character(len=*),intent(in)          :: verb
integer,intent(out),optional         :: error_return
character(len=*),intent(in),optional :: style
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=:),allocatable         :: command
integer                              :: ier
integer                              :: ibig
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(error_return))then
      error_return=0
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ier=0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(style))then
      kracken_method=style
   else
      kracken_method='kracken'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ! no matter what method make sure this is allocated so user can query it
   ! and so methods can use unnamed array without having to test it
   if(allocated(unnamed))then
      deallocate(unnamed)
   endif
   ibig=longest_command_argument() ! bug in gfortran. len=0 should be fine
   allocate(character(len=ibig) ::unnamed(0))
   unnamed=[character(len=ibig) ::]            ! kludge
!-----------------------------------------------------------------------------------------------------------------------------------
   select case(upper(kracken_method))
    case('ARGS')
      call parse(trim(verb),string,'add')                ! initialize command
      call cmd_args_to_dictionary(trim(verb))            ! process user command options
      if(lget(trim(verb)//'_?'))then                     ! if -? option was present prompt for values
         call menu(verb)
      endif
      ! if calling procedure is not testing error flag stop program on error
      if(.not.present(error_return).and.ier.ne.0)then
         call journal("*kracken* (V 20191018) STOPPING: error parsing arguments using ARGS method")
         stop
      endif
    case default
      call store(trim(verb)//'_?','.false.','add',ier)   ! all commands have the option -? to invoke prompt mode
      call get_command_arguments_string(command,ier)
      if(debug) call journal('sc','KRACKEN ',trim(command))
      if(ier.ne.0)then
         call journal("*kracken* could not get command line arguments")
         if(present(error_return))error_return=ier
      else
         call dissect(verb,string,command,ier)
         ! if calling procedure is not testing error flag stop program on error
         if(.not.present(error_return).and.ier.ne.0)then
            call journal("*kracken* (V 20191018) STOPPING: error parsing arguments using DEFAULT method")
            stop
         endif
      endif
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine kracken
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    setprompts(3f) - [ARGUMENTS:M_kracken] set explicit prompts for keywords in interactive mode
!!    (LICENSE:PD)
!!
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
!!    strings are given instead of default values. It is called before a call to KRACKEN(3f)
!!    or DISSECT(3f).
!!
!!##OPTIONS
!!    verb    name to define prompts for
!!    string  to define prompts instead of values
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_setprompts
!!     use M_kracken, only : kracken,iget,rget,sget,setprompts
!!     implicit none
!!
!!     call setprompts('demo', ' -int My INTEGER value  -float My REAL value  -str My CHARACTER value')
!!     call kracken(   'demo', ' -int 100 -float 123.456 -str DEFAULT')
!!     write(*,'(a,i0)')'INTEGER IS ',iget('demo_int')
!!     write(*,'(a,g0)')'REAL IS ',rget('demo_float')
!!     write(*,'(a,a)')'STRING IS '//trim(sget('demo_str'))
!!
!!     end program demo_setprompts
!!
!!   Example execution and output:
!!
!!        $ demo_setprompts -?
!!        demo parameters are
!!        >   1)My CHARACTER value:[DEFAULT]
!!            3)My INTEGER value:[100]
!!            4)My REAL value:[123.456]
!!        Enter parameter number to change("RETURN" to finish):
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine setprompts(verb,init)

! ident_14="@(#)M_kracken::setprompts(3f): set explicit prompts for keywords in interactive mode"

character(len=*),intent(in):: verb   ! verb name to define prompts for
character(len=*),intent(in):: init   ! string to define prompts instead of values
      call parse('?'//trim(verb),init,'add') ! initialize command, prefixing verb with question mark character to designate prompts
end subroutine setprompts
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    dissect(3f) - [ARGUMENTS:M_kracken] convenient call to parse() -- define defaults, then process
!!    (LICENSE:PD)
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
!!   Sample program:
!!
!!     program demo_dissect
!!     use M_kracken, only : kracken,iget,rget,sget,dissect
!!     implicit none
!!     integer :: ierr
!!
!!     call dissect('demo',' -int 1000 -float 1234.567 -str CHARACTER value','-int 456 -float 50.00 ',ierr)
!!     write(*,'(a,i0)')'INTEGER IS ',iget('demo_int')
!!     write(*,'(a,g0)')'REAL IS ',rget('demo_float')
!!     write(*,'(a,a)')'STRING IS '//trim(sget('demo_str'))
!!
!!     end program demo_dissect
!!
!!   Results:
!!
!!    INTEGER IS 456
!!    REAL IS 50.0000000
!!    STRING IS CHARACTER value
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine dissect(verb,init,pars,error_return)

! ident_15="@(#)M_kracken::dissect(3f): convenient call to parse() define defaults, then process"

character(len=*),intent(in)  :: verb                     ! the name of the command to be reset/defined  and then set
character(len=*),intent(in)  :: init                     ! used to define or reset command options; usually hard-set in the program.
character(len=*),intent(in)  :: pars                     ! defines the command options to be set, usually from a user input file
integer,intent(out),optional :: error_return
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
   if(debug) call journal('sc','START DISSECT ',trim(verb)//'::'//trim(init)//'::'//trim(pars))
!-----------------------------------------------------------------------------------------------------------------------------------
   call store(trim(verb)//'_?','.false.','add',ier)   ! all commands have the option -? to invoke prompt mode
   call parse(trim(verb),init,'add')                  ! initialize command
!-----------------------------------------------------------------------------------------------------------------------------------
   call parse(verb,pars,"no_add",ier)                 ! process user command options
   if(lget(trim(verb)//'_?'))then                     ! if -? option was present prompt for values
      call menu(verb)
   endif
   if(present(error_return))error_return=ier
!-----------------------------------------------------------------------------------------------------------------------------------
   if(debug) call journal('sc','END DISSECT ',trim(verb)//'::'//trim(init)//'::'//trim(pars))
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine dissect
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    parse(3f) - [ARGUMENTS:M_kracken] parse user command and store tokens into Language Dictionary
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!! recursive subroutine parse(verb,string,allow,error_return)
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
!!    VERB     command name to process
!!    STRING   string is character input string with first verb removed (options + other commands)
!!    ALLOW    flag to allow or disallow new VERB_KEYWORD name being added. Should be
!!              NEW VARIABLES ARE ALLOWED
!!               o 'define'  -  add or replace a new VERB_KEYWORD entry and value
!!               o 'add'     -  add or append to a new VERB_KEYWORD entry and value
!!              NO NEW VARIABLES ARE ALLOWED
!!               o 'append' or 'no_add' - append to an *EXISTING* entry value
!!               o 'replace' - replace an *EXISTING* entry
!!
!!             That is, ff 'add' or 'append' and the value is not blank
!!             it will be APPENDED to the current value. If 'define' or
!!             'replace' it will replace the value instead of appending
!!             to it.
!!##RETURNS
!!    ERROR_RETURN  error code. If zero, no error occurred
!!
!!##EXAMPLE
!!
!!   sample program:
!!
!!    program demo_parse
!!    use M_kracken, only : parse, sget, iget, rget
!!    use M_strings, only : chomp
!!    implicit none
!!    character(len=:),allocatable  :: verb
!!    character(len=*),parameter    :: delimiters=' ;,'
!!    integer     :: i
!!    integer     :: ierr
!!    character(len=132) :: line
!!    character(len=132), parameter :: commands(5)= [character(len=132) :: &
!!      'start -i 10 -message this is a message', &
!!      'end -i 20 -j 30 -k 55.55 ', &
!!      'list', &
!!      'help -oo', &
!!      'end -i 44.44 ']
!!      do i=1,size(commands)
!!         line=commands(i) ! need mutable line
!!         if(chomp(line,verb,delimiters).ge. 0)then
!!            call parse(verb,line,'add',ierr)
!!            write(*,*)'do whatever a '//verb//' command does'
!!            select case(verb)
!!            case('start')
!!               write(*,*)trim(sget('start_i'))
!!               write(*,*)trim(sget('start_message'))
!!            case('end')
!!               write(*,*)iget('end_i')
!!               write(*,*)iget('end_j')
!!               write(*,*)rget('end_k')
!!            case('list')
!!               write(*,*)'list things'
!!            case('help')
!!               write(*,*)'show help text'
!!            endselect
!!         endif
!!      enddo
!!      ! look at some of the values as strings or numbers
!!      write(*,*)trim(sget('start_i'))
!!      write(*,*)trim(sget('start_message'))
!!      write(*,*)iget('end_i')
!!      write(*,*)iget('end_j')
!!      write(*,*)rget('end_k')
!!    end program demo_parse
!!
!!   Results:
!!
!!     do whatever a start command does
!!     10
!!     this is a message
!!     do whatever a end command does
!!              20
!!              30
!!       55.5499992
!!     do whatever a list command does
!!     list things
!!     do whatever a help command does
!!     show help text
!!     do whatever a end command does
!!              44
!!              30
!!       55.5499992
!!     10
!!     this is a message
!!              44
!!              30
!!       55.5499992
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
recursive subroutine parse(verb,string,allow,error_return)

! ident_16="@(#)M_kracken::parse(3f): parse user command and store tokens into Language Dictionary"

!!!   set up odd for future expansion
!!!   need to handle a minus followed by a blank character
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
character(len=:),allocatable         ::  dummy  ! working copy of string
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
   if(.not.allocated(dict_verbs)) call initd()
!-----------------------------------------------------------------------------------------------------------------------------------
   if(debug) call journal('sc','PARSE ',trim(verb)//'::'//trim(string)//'::'//trim(allow))
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
   dummy=string//'  '
   ipln=len_trim(verb)             ! find number of characters in verb prefix string
   if(size(dict_verbs).ne.0)then
      dict_calls=0                 ! clear number of times this keyword stored on a call to parse
                                   ! should more efficiently only do this for current VERB instead of entire array in dictionary
   endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   if(subscript_(trim(verb)//'_?') .le. 0 )then         ! assuming if adding this is initial call
      call store(trim(verb)//'_?','.false.','add',ier)  ! all commands have the option -? to invoke prompt mode
   elseif(allow.eq.'add')then
      call store(trim(verb)//'_?','.false.','add',ier)  ! all commands have the option -? to invoke prompt mode
   endif
!=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
   if(subscript_(trim(verb)//'_>') .le. 0 )then         ! assuming if adding this is initial call
      call store(trim(verb)//'_>','#N#','add',ier)      ! all commands have the option -> to write journal(3f) output
   elseif(allow.eq.'add')then
      call store(trim(verb)//'_>','#N#','add',ier)      ! all commands have the option -> to write journal(3f) output
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
               elseif(var(1)(ibegin:ibegin) == " ")then
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
               if(subscript_(name).le.0)then
                  call store(name,' ',allow,ier) ! store name and blank value
               endif
            else
               call store(name,val,allow,ier)    ! store name and its value
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
         dummy(:)=" "
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
         dummy(:)=" "
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
!!    store(3f) - [ARGUMENTS:M_kracken] add or replace value for specified name in dictionary(if allow='add' add name if needed)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine store(name1,value1,allow1,ier)
!!
!!    character(len=*),intent(in) :: name1
!!    class(*),intent(in)         :: value1
!!    character(len=*),intent(in) :: allow1
!!    integer,intent(out)         :: ier
!!
!!##DESCRIPTION
!!    Normally a command string and the associated values are placed in
!!    the dictionary by a call to KRACKEN(3f) when parsing a command
!!    line, or DISSECT(3f) and PARSE(3f) when creating input file
!!    interpreters. Rarely there may be a need to place
!!    <NAME,VALUE> pairs directly into the command dictionary, so this
!!    routine is public in the M_kracken(3fm) module. However,
!!    *this routine is primarily assumed to be an internal procedure*.
!!
!!##OPTIONS
!!    NAME1    name in dictionary of form VERB_KEYWORD
!!    VALUE1   value to be associated to NAME1. Value may be of type INTEGER,
!!             REAL, DOUBLEPRECISION, LOGICAL or CHARACTER.
!!    ALLOW1   flag to allow new VERB_KEYWORD name being added. Should be
!!              'define'  add or replace a new VERB_KEYWORD entry and value
!!              'add'     add or append to a new VERB_KEYWORD entry and value
!!              'no_add' or 'append'  append to an *EXISTING* entry value
!!              'replace'             replace an *EXISTING* entry
!!
!!             If 'add' or 'append' and the value is not blank it will
!!             be APPENDED to the current value. If 'define' or 'replace'
!!             it will replace the value instead of appending to it.
!!
!!##RETURNS
!!    IER      flag if error occurs in adding or setting value
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_store
!!    use M_kracken, only : store, show
!!    implicit none
!!    integer :: ier
!!    ! The following should be equivalent to
!!    ! call kracken('MY',' &
!!    ! & -STRING My string value &
!!    ! & -INTEGER 1234 &
!!    ! & -INTEGER 0987654321 &
!!    ! & -REAL 1234.5678 &
!!    ! & -DOUBLE 123.4567d8 &
!!    ! & -LOGICAL T &
!!    ! & '
!!    call store('MY_STRING','My string value','add',ier)
!!    if(ier.ne.0)write(*,*)'ERROR: could not store MY_STRING ier=',ier
!!    ! now the verb MY is defined with the option -STRING so the
!!    ! dictionary has MY_STRING='My string value' defined
!!
!!    ! this will be an error because MY does not have the -INTEGER
!!    ! keyword defined
!!    call store('MY_INTEGER',12345678,'no_add',ier)
!!
!!    ! now define MY_INTEGER
!!    call store('MY_INTEGER',1234,'add',ier)
!!    ! if 'no_add' it will APPEND to current string
!!    call store('MY_INTEGER',987654321,'add',ier)
!!
!!    call store('MY_REAL',1234.5678,'add',ier)
!!    call store('MY_DOUBLE',123.4567d8,'add',ier)
!!    call store('MY_LOGICAL',.true.,'add',ier)
!!
!!    call show('MY',.false.,0)
!!    write(*,*)repeat('=',76)
!!
!!    ! if 'replace' is used REPLACE instead of APPEND to current value
!!    call store('MY_INTEGER',987654321,'replace',ier)
!!    call show('MY',.false.,0)
!!    write(*,*)repeat('=',76)
!!
!!    ! 'replace' can only replace an existing entry, not add one
!!    call store('MY_UNKNOWN',987654321,'replace',ier)
!!    call show('MY',.false.,0)
!!    write(*,*)repeat('=',76)
!!
!!    end program demo_store
!!
!!   Results:
!!
!!    >########################################################
!!    >error: UNKNOWN OPTION -INTEGER
!!    >MY parameters are
!!    > -STRING My string value
!!    >########################################################
!!    > MY_STRING            = My string value
!!    > MY_REAL              = 1234.5677
!!    > MY_LOGICAL           = T
!!    > MY_INTEGER           = 1234 987654321
!!    > MY_DOUBLE            = 12345670000.000000
!!    > =======================================================================
!!    > MY_STRING            = My string value
!!    > MY_REAL              = 1234.5677
!!    > MY_LOGICAL           = T
!!    > MY_INTEGER           = 987654321
!!    > MY_DOUBLE            = 12345670000.000000
!!    > =======================================================================
!!    >########################################################
!!    >error: UNKNOWN OPTION -UNKNOWN
!!    >MY parameters are
!!    > -STRING My string value
!!    > -REAL 1234.5677
!!    > -LOGICAL T
!!    > -INTEGER 987654321
!!    > -DOUBLE 12345670000.000000
!!    >########################################################
!!    > MY_STRING            = My string value
!!    > MY_REAL              = 1234.5677
!!    > MY_LOGICAL           = T
!!    > MY_INTEGER           = 987654321
!!    > MY_DOUBLE            = 12345670000.000000
!!    > =======================================================================
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine store(name1,value1,allow1,ier)

! ident_17="@(#)M_kracken::store(3f): replace or add dictionary entry name  and value (if allow='add' add name if necessary)"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)        :: name1       ! name in dictionary of form VERB_KEYWORD
class(*),intent(in)                :: value1      ! value to be associated to NAME1
character(len=*),intent(in)        :: allow1      ! flag to allow new VERB_KEYWORD name being added
integer,intent(out)                :: ier         ! flag if error occurs in adding or setting value
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=:),allocatable       :: l_value1    ! value to be associated to NAME1
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
integer                            :: inew
!-----------------------------------------------------------------------------------------------------------------------------------
   select type(value1)                         ! convert non-character values to character string
   type is(integer)
      allocate(character(len=30):: l_value1)
      write(l_value1,'(i0)')value1
   type is(logical)
      l_value1=merge('T','F',value1)
   type is(real)
      allocate(character(len=30):: l_value1)
      write(l_value1,'(g0.8)')value1
   type is(doubleprecision)
      allocate(character(len=30):: l_value1)
      write(l_value1,'(g0)')value1
   type is(character(len=*))
      l_value1=value1
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   if(debug) write(*,*)'STORE ',trim(name1)//'::'//trim(l_value1)//'::'//trim(allow1)
!-----------------------------------------------------------------------------------------------------------------------------------
   value=" "
   name=" "                                          ! compiler bug. KLUDGE
   allow=" "
   name=name1                                        ! store into a standard size variable for this type

   ii=index(name,"_")                                ! -- is an alias for -oo
   ii=min(ii,IPverb-2)
   if(name(ii+1:).eq.' ')then
      name(ii+1:)='oo'
   endif

   value=l_value1                                    ! store into a standard size variable for this type
   allow=allow1                                      ! store into a standard size variable for this type
   nlen=len(name1)
!-----------------------------------------------------------------------------------------------------------------------------------
   if(.not.allocated(dict_verbs)) call initd()
   call locate(dict_verbs,name,indx,ier,mssge)       ! determine storage placement of the variable and whether it is new
   if(ier  ==  -1)then                               ! an error occurred in determining the storage location
      call journal("error occurred in *store*")
      call journal(mssge)
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(indx > 0)then                                  ! found the variable name
      new=1
   elseif(indx <= 0.and.(allow  ==  'add'.or. allow == 'define'))then        ! check if the name needs added and allow to add
      inew=iabs(indx)                                ! adding the new variable name in the variable name array
      call insert(dict_verbs,name,inew)              ! pull down the dictionary arrays to make room for new value
      call insert(dict_vals," ",inew)
      call insert(dict_calls,0,inew)
      call insert(dict_lens,0,inew)

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
      call journal("*store* error: UNKNOWN OPTION -"//name(ii+1:))
      if(ii > 0)then
         call journal(name(:ii-1)//" parameters are")
         do i10=1,size(dict_verbs)
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
   indx=iabs(indx)  ! entry existed or was added
   if(indx.eq.0)then
      write(*,*)'*store* error: INDEX=0'
   elseif(value(1:4)  ==  "@LV@")then
      ! a leave-alone flag (for use by a 'defining' call)
      if(new  ==  0) then
         value=value(5:)                                                  ! trim off the leading @LV@
         if(dict_calls(INDX).eq.0.or.dict_vals(INDX).eq.' ')then
            call replace(dict_vals,value,INDX)
         else
            if(allow.eq.'define')then
               call replace(dict_vals,value,INDX)                             ! set a defined variable's value
            else
               call replace(dict_vals,trim(dict_vals(INDX))//' '//value,INDX) ! append a defined variable's value
            endif
         endif
         dict_lens(INDX)= len_trim(dict_vals(INDX))                       ! store length of string
         dict_calls(INDX)=dict_calls(INDX)+1                              ! detect duplicate use of a keyword
      endif
   else
      if(dict_calls(INDX).eq.0.or.dict_vals(INDX).eq.' ')then             ! if first time given a value or value blank
         call replace(dict_vals,value,indx)                               ! store a defined variable's value
      elseif(allow.eq.'replace'.or.allow.eq.'define')then
         call replace(dict_vals,value,indx)                               ! set a defined variable's value
      else
         call replace(dict_vals,trim(dict_vals(INDX))//' '//value,indx)   ! set a defined variable's value
      endif
      dict_lens(INDX)= len_trim(dict_vals(INDX))                          ! store length of string
      dict_calls(INDX)=dict_calls(INDX)+1                                 ! detect duplicate use of a keyword
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
   if(debug) write(*,*)'STORE END ',trim(name1)//'::'//trim(l_value1)//'::'//trim(allow1)
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine store
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    subscript_(3fp) - [ARGUMENTS:M_kracken] return the subscript value of a string when given its name
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function subscript_(chars0)
!!
!!    character(len=*),intent(in) :: chars0
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!
!!##RETURNS
!!
!!##EXAMPLE
!!
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function subscript_(chars0)

! ident_18="@(#)M_kracken::subscript_(3fp): return the subscript value of a string when given its name"

!  WARNING: only request value of names known to exist
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in)        :: chars0
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=IPverb)              :: chars
   character(len=IPvalue)             :: mssge
   integer                            :: ierr
   integer                            :: indx
   integer                            :: subscript_
!-----------------------------------------------------------------------------------------------------------------------------------
   chars=chars0
   indx=0
   ierr=0
   call locate(dict_verbs,chars,indx,ierr,mssge)                        ! look up position
!-----------------------------------------------------------------------------------------------------------------------------------
   if((ierr  ==  -1).or.(indx <= 0))then
      !call journal("*subscript_* variable "//trim(chars)//" undefined")
      subscript_=-1                                                       ! very unfriendly subscript value
   else
      subscript_=indx
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function subscript_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    menu(3fp) - [ARGUMENTS:M_kracken] prompt for values using a menu interface
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine menu(verb)
!!
!!    character(len=*),intent(in)  :: verb
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!
!!##RETURNS
!!
!!##EXAMPLE
!!
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine menu(verb)

! ident_19="@(#)M_kracken::menu(3fp): prompt for values using a menu interface"

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
   integer                   :: indx
   character(len=IPvalue)    :: mssge     ! the message/error/string  value returned by BOUNCE(3f)
   character(len=1)          :: prefix
   integer                   :: icurrent  ! current menu item
   integer                   :: icmd
   integer                   :: imenu
   character(len=80),allocatable :: help_text(:)
   integer                       :: i
   integer                       :: cstat
   character(len=256)            :: sstat
!-----------------------------------------------------------------------------------------------------------------------------------
   stop_command=.false.
   ii=len_trim(verb)
   call journal(verb(:ii)//" parameters are")
   istart=1
   icurrent=1
!-----------------------------------------------------------------------------------------------------------------------------------
   if(.not.allocated(dict_verbs)) call initd()
   INFINITE: do
      icount=0                                                ! how many entries in the dictionary belong to this command
      iend=size(dict_verbs)                                   ! last dictionary entry to search for current command
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
            call locate(dict_verbs,'?'//dict_verbs(i10),indx,ierr,mssge) ! if ?VERB is defined assume it is a prompt
            if(indx.gt.0)then
               prompt=dict_vals(indx)
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
      if(ios.ne.0)then
         reply=' '
      else
         reply=adjustl(reply)
      endif
      ivalu=-1
!-----------------------------------------------------------------------------------------------------------------------------------
      select case(REPLY(1:1))
!-----------------------------------------------------------------------------------------------------------------------------------
      case('-')  ! if it starts with a - assume it is a new specification of the arguments
         call parse(verb,trim(reply)//' -? .false.',"no_add")
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('@')                                        ! debug option to dump dictionary
         do i20=1,size(dict_verbs)
            if(dict_verbs(i20).ne.' ')then
                 write(*,*)i20,trim(dict_verbs(i20)),trim(dict_vals(i20)(:dict_lens(i20)))
            endif
         enddo
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('#')                                                                  ! ignore
         cycle INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case(' ','q','e','0')                                                      ! exit menu changes
         exit INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('x','.')                                                              ! return value to indicate command has been stopped
         stop_command=.true.
         exit INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
      case('!')                                                                      ! call system
         call execute_command_line(trim(reply(2:)), exitstat=icmd,cmdstat=cstat,cmdmsg=sstat)   ! execute system command
         if(icmd.ne.0)then                                                           ! if system command failed exit program
            call journal('sc','*M_kracken:menu* ERROR - SYSTEM COMMAND FAILED:',icmd)
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
         ivalu=nint(s2v(reply))
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
        !call journal('Version 20160414')
         call journal('Version 20191018')
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
         call locate(dict_verbs,'?'//dict_verbs(ifound),indx,ierr,mssge) ! if ?VERB is defined assume it is a prompt
         if(indx.gt.0)then
            prompt=dict_vals(indx)
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
   call store(trim(verb)//'_?','.false.','add',ierr)                      ! all commands have the option -? to invoke prompt mode
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine menu
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    show(3f) - [ARGUMENTS:M_kracken] dump dictionary entries
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine show(VERB_NAME0,VERBS_ONLY,IWIDE)
!!
!!    character(len=*),intent(in)   :: VERB_NAME0
!!    logical,intent(in)            :: VERBS_ONLY
!!    integer,intent(in)            :: iwide
!!
!!##DESCRIPTION
!!    Write information about a command from the command dictionary or list all the
!!    command verbs in the dictionary
!!
!!##OPTIONS
!!    VERB_NAME0   verb prefix to display. Default is all
!!    VERBS_ONLY   flag to show verbs only
!!    IWIDE        if .ge. zero, how many columns wide to show just verbs
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_show
!!     use M_kracken, only : kracken, show
!!     implicit none
!!
!!     call kracken('demo', ' default keyword -i 10 -j 20.20 -value my default string')
!!     call show('demo',.false.,0)
!!
!!     end program demo_show
!!   Results:
!!
!!     demo_value           = my default string
!!     demo_oo              = default keyword
!!     demo_j               = 20.20
!!     demo_i               = 10
!!     demo_?               = .false.
!!     demo_>               = #N#
!!
!!##SEE ALSO
!!    M_kracken(3f),  kracken(3f)
!!
!!    dget(3f), dgets(3f), iget(3f), igets(3f), lget(3f), lgets(3f),
!!    rget(3f), rgets(3f), sget(3f), sgets(3f), retrev(3f)
!!
!!    parse(3f), dissect(3f), store(3f), setprompts(3f), show(3f)
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine show(VERB_NAME0,VERBS_ONLY,IWIDE)

! ident_20="@(#)M_kracken::show(3f): dump dictionary entries"

character(len=*),intent(in)   :: VERB_NAME0     ! verb prefix to display. Default is all
logical,intent(in)            :: VERBS_ONLY     ! flag to show verbs only
integer,intent(in)            :: iwide          ! if .ge. zero, how many columns wide to show just verbs
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=IPvalue)        :: VERB_NAME      ! verb prefix to display. Default is all
character(len=IPvalue)        :: message
integer                       :: i
integer                       :: j
integer                       :: ii
integer                       :: ich
integer                       :: istart
integer                       :: istep
integer                       :: iwide_local
integer                       :: verb_length
!-----------------------------------------------------------------------------------------------------------------------------------
   if(.not.allocated(dict_verbs)) call initd()
!-----------------------------------------------------------------------------------------------------------------------------------
   iwide_local=iwide
   if(iwide_local.le.0)iwide_local=80
   VERB_NAME=VERB_NAME0
!-----------------------------------------------------------------------------------------------------------------------------------
   if(VERBS_ONLY)then                                         ! show just verbs
      message=' '
      istart=1
      !!istep=len(DICT_VERBS)
      istep=1
      verb_length=1
      call journal('+c','')                                   ! start comment line
      do j=1,2                                                ! to make compact, make 1st pass to get length, 2nd pass to print
         do i=size(dict_verbs),1,-1                           ! loop thru entire dictionary
            if(DICT_VERBS(i)(1:1).eq.'?')cycle                ! remove prompts
            if(DICT_VERBS(i)(1:1).eq.'_')cycle                ! remove initial values
            verb_length=len_trim(DICT_VERBS(i))               ! find longest verb
            if(DICT_VERBS(i)(verb_length-2:).ne.'_oo') cycle  ! assume all commands have a VERB_oo value
            if(verb_length.lt.3)cycle                         ! looking for VERB_oo
            if(j.eq.1)then
               istep=max(istep,verb_length-3+1)
            elseif(istart+istep+1.gt.iwide_local)then
               call journal('ts','')                          ! end line
               call journal('+c','')                          ! start next comment line in trail so get pound character
               istart=1
               call journal('+st',adjustr(atleast(DICT_VERBS(i)(:verb_length-3),istep)))
               istart=istart+istep
            elseif(verb_length-3.gt.0.and.j.eq.2)then
               call journal('+st',adjustr(atleast(DICT_VERBS(i)(:verb_length-3),istep)))
               istart=istart+istep
            endif
         enddo
      enddo
      call journal('st','')
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(VERB_NAME.eq.' ')then                                   ! show all variables
      do i=1,size(dict_verbs)
         if(DICT_VERBS(i).ne.' ')then
            ii=max(1,dict_lens(i))                                 ! number of characters in corresponding dictionary VALUE
            call journal('sc',' ',atleast(DICT_VERBS(i),20)//'=',dict_vals(i))
         endif
      enddo
      call journal('sc',' dictionary size=',size(DICT_VERBS),'verb length=',len(DICT_VERBS),'value length=',len(DICT_VALS))
!-----------------------------------------------------------------------------------------------------------------------------------
   else                                                        ! show only verb_ variables
      ich=index(VERB_NAME,' ')                                 ! VERB_NAME assumed longer than any verb name, so at least one space
      VERB_NAME(ich:ich)='_'
      SCAN_DICTIONARY: do i=1,size(dict_verbs)
         if(DICT_VERBS(i).eq.' ')cycle SCAN_DICTIONARY
         if(VERB_NAME(:ich).eq.DICT_VERBS(i)(:ich))then
           ii=max(1,dict_lens(i))                          ! number of characters in corresponding dictionary VALUE
           call journal('+sc',' ',atleast(DICT_VERBS(i),20)//'=')
           call journal(dict_vals(i)(:ii))
         endif
      enddo SCAN_DICTIONARY
!-----------------------------------------------------------------------------------------------------------------------------------
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine show
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine initd()
   dict_verbs=[character(len=0) ::]  ! string variable names
   dict_vals=[character(len=0)  ::]  ! contains the values of string variables
   dict_calls=[integer ::]           ! number of times this keyword stored on a call to parse
   dict_lens=[integer ::]            ! significant lengths of string variable values
end subroutine initd
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_kracken()
use,intrinsic :: IEEE_ARITHMETIC, only : IEEE_IS_NAN       ! Determine if value is IEEE Not-a-Number.
!!use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level

!! setup
call test_dget()
call test_dgets()
call test_iget()
call test_igets()
call test_lget()
call test_lgets()
call test_rget()
call test_rgets()
call test_sget()
call test_sgets()
call test_store()
call test_retrev()
call test_parse()
call test_dissect()
   call test_kracken()
   call test_setprompts()
   call test_show()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dget()
use M_verify,  only : almost
integer         :: ier
doubleprecision :: dd=huge(0.0d0)*0.999999999999d0
   call unit_check_start('dget',msg=' direct tests of dget(3f)')
   call store('MY_DOUBLE1',dd,'define',ier)
   call store('MY_DOUBLE2',-1234.0d-20,'define',ier)
   call store('BAD','nN3.3','define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('dget', almost(dget('MY_DOUBLE1'),dd,15),  'MY_DOUBLE1',dget('MY_DOUBLE1'),'versus',dd)
   call unit_check('dget', dget('MY_DOUBLE2').eq.-1234.0d-20, 'MY_DOUBLE2',dget('MY_DOUBLE2'))
   call unit_check('dget', dget('NOTTHERE').eq.0,             'NOTTHERE',dget('NOTTHERE'))
   call unit_check('dget', ieee_is_nan(dget('BAD')),          'BAD',dget('BAD'))
   call unit_check('dget', dget('BLANK').eq.0,                'BLANK',dget('BLANK'))
   call unit_check_done('dget',msg='')
end subroutine test_dget
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dgets()
integer                     :: ier
doubleprecision,allocatable :: d(:)
   call unit_check_start('dgets',msg=' direct tests of dgets(3f)')
   call store('MY_DOUBLE1','100.0d0 0.0d0 300.33333d2','define',ier)
   call store('MY_DOUBLE2',-1234.0d-20,'define',ier)
   call store('BAD','sfs','define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('dgets', all(dgets('MY_DOUBLE1').eq.[100.0d0,0.0d0,300.33333d2]),'MY_DOUBLE1')
   call unit_check('dgets', all(dgets('MY_DOUBLE2').eq.[-1234.0d-20]),              'MY_DOUBLE2')
   call unit_check('dgets', size(dgets('NOTTHERE')).eq.0,                           'NOTTHERE')
   d=dgets('BAD')
   if(size(d).gt.0)then
      call unit_check('dgets', size(d).eq.1.and.ieee_is_nan(d(1)),                  'BAD IS NAN',size(d))
   else
      call unit_check('dgets', size(d).eq.1,                                        'BAD HAS SIZE 1',size(d))
   endif
   call unit_check('dgets', size(dgets('BLANK')).eq.0,                              'BLANK')
   call unit_check_done('dgets',msg='')
end subroutine test_dgets
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dissect()
integer :: ierr
   call dissect('demo',' -int 1000 -float 1234.567 -str CHARACTER value','-int 456 -float 50.00 ',ierr)
   call unit_check_start('dissect',msg='')
   call unit_check('dissect', iget('demo_int').eq.456, 'demo_int',iget('demo_int'))
   call unit_check('dissect', rget('demo_float').eq.50.0, 'demo_float',rget('demo_float'))
   call unit_check('dissect', sget('demo_str').eq.'CHARACTER value', 'demo_str',sget('demo_str'))
   call unit_check_done('dissect',msg='')
end subroutine test_dissect
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_iget()
integer        :: ier
   call unit_check_start('iget',msg=' direct tests of iget(3f)')
   call store('MY_INTEGER1',1234,'define',ier)
   call store('MY_INTEGER2',-1234,'define',ier)
   call store('BAD','3z4j','define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('iget', iget('MY_INTEGER1').eq.1234,  'MY_INTEGER1',iget('MY_INTEGER1'))
   call unit_check('iget', iget('MY_INTEGER2').eq.-1234, 'MY_INTEGER2',iget('MY_INTEGER2'))
   call unit_check('iget', iget('NOTTHERE').eq.0,        'NOTTHERE',iget('NOTTHERE'))
   call unit_check('iget', iget('BLANK').eq.0,           'BLANK',iget('BLANK'))
   call unit_check_done('iget',msg='')
end subroutine test_iget
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_igets()
integer        :: ier
real,allocatable :: i(:)
   call unit_check_start('igets',msg=' direct tests of igets(3f)')
   call store('MY_INTEGER1','100 0 -321','define',ier)
   call store('MY_INTEGER2',-1234,'define',ier)
   call store('BAD','sfs','define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('igets', all(igets('MY_INTEGER1').eq.[100,0,-321]), 'MY_INTEGER1')
   call unit_check('igets', all(igets('MY_INTEGER2').eq.[-1234]),      'MY_INTEGER2')
   call unit_check('igets', size(igets('NOTTHERE')).eq.0,              'NOTTHERE')
   i=igets('BAD')
   call unit_check('igets', size(i).eq.1.and.i(1).eq.-huge(0),         'BAD')
   call unit_check('igets', size(igets('BLANK')).eq.0,                 'BLANK')
   call unit_check_done('igets',msg='')
end subroutine test_igets
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_kracken()
   call unit_check_start('kracken',msg='')
   !!call unit_check('kracken', 0.eq.0, 'checking',100)
   call unit_check_done('kracken',msg='')
end subroutine test_kracken
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lget()
integer        :: ier
   call unit_check_start('lget',msg=' direct tests of LGET(3f)')
   call store('MY_LOGICAL_TRUE',.true.,'define',ier)
   call store('MY_LOGICAL_FALSE',.false.,'define',ier)
   call store('BAD','.bad.','define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('lget', lget('MY_LOGICAL_TRUE'),       'MY_LOGICAL_TRUE',lget('MY_LOGICAL_TRUE'))
   call unit_check('lget', .not.lget('MY_LOGICAL_FALSE'), 'MY_LOGICAL_FALSE',lget('MY_LOGICAL_FALSE'))
   call unit_check('lget', .not.lget('NOTTHERE'),         'NOTTHERE',lget('NOTTHERE'))
   call unit_check('lget', .not.lget('BAD'),              'BAD',lget('BAD'))
   call unit_check('lget', lget('BLANK'),                 'BLANK',lget('BLANK'))
   call unit_check_done('lget',msg='')
end subroutine test_lget
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lgets()
integer        :: ier
   call unit_check_start('lgets',msg=' direct tests of lgets(3f)')
   call store('MY_LOGICAL_TRUE','.true. .true.','define',ier)
   call store('MY_LOGICAL_FALSE','.false.','define',ier)
   call store('MY_LOGICAL3','.true. .false. .false. .true.','define',ier)
   call store('BAD','.bad.','define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('lgets',all(lgets('MY_LOGICAL_TRUE').eqv.[.true.,.true.]),             'MY_LOGICAL_TRUE')
   call unit_check('lgets',all(lgets('MY_LOGICAL_FALSE').eqv.[.false.]),                  'MY_LOGICAL_FALSE')
   call unit_check('lgets',all(lgets('MY_LOGICAL3').eqv.[.true.,.false.,.false.,.true.]), 'MY_LOGICAL3')
   call unit_check('lgets',all(lgets('NOTTHERE').eqv.[.false.]),                          'NOTTHERE')
   call unit_check('lgets',all(lgets('BAD').eqv.[.false.]),                               'BAD')
   call unit_check('lgets',all(lgets('BLANK').eqv.[.true.]),                              'BLANK')
   call unit_check_done('lgets',msg='')
end subroutine test_lgets
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_parse()
character(len=:),allocatable  :: verb
integer     :: i
integer     :: ierr
character(len=132) :: line
character(len=132), parameter :: commands(5,2)= reshape([character(len=132) :: &
  'start', ' -i 10 -message this is a message', &
  'end'  , ' -i 20 -j 30 -k 55.55 -l .false.', &
  'list' , ' ', &
  'help' , ' -oo', &
  'where', ' -i 44.44 '],shape(commands),order=[2,1])
   call unit_check_start('parse',msg='')
   do i=1,size(commands,dim=1)
      line=commands(i,2) ! need mutable line
      verb=commands(i,1)
      call parse(verb,line,'define',ierr)
      select case(verb)
      case('start')
         call unit_check('parse',sget('start_i').eq.'10','start_i')
         call unit_check('parse',sget('start_message').eq.'this is a message','start_message')
      case('end')
         call unit_check('parse',iget('end_i').eq.20,'end_i')
         call unit_check('parse',iget('end_j').eq.30,'end_j')
         call unit_check('parse',rget('end_k').eq.55.55,'end_k')
      case('list')
         call unit_check('parse',sget('list_oo').eq.'','list_oo')
      case('help')
         call unit_check('parse',sget('help_oo').eq.'','help_oo')
      endselect
   enddo
   call unit_check_done('parse',msg='')
end subroutine test_parse
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_retrev()
character(len=IPvalue) :: val
integer                :: len, ier
   call unit_check_start('retrev',msg='')
   call kracken('demo', ' -value my default string -x one -x two -A 12345')
   call retrev('demo_value',val,len,ier)
   call unit_check('retrev', val.eq.'my default string', 'value',val)
   call retrev('demo_x',val,len,ier)
   call unit_check('retrev', val.eq.'one two', 'x',val)
   call retrev('demo_A',val,len,ier)
   call unit_check('retrev', val.eq.'12345', 'A',val)
   call unit_check_done('retrev',msg='')
end subroutine test_retrev
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rget()
integer        :: ier
   call unit_check_start('rget',msg=' direct tests of rget(3f)')
   call store('MY_REAL1',1234.567,'define',ier)
   call store('MY_REAL2',-1234.567e3,'define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('rget', rget('MY_REAL1').eq.1234.567,    'MY_REAL1',rget('MY_REAL1'))
   call unit_check('rget', rget('MY_REAL2').eq.-1234.567e3, 'MY_REAL2',rget('MY_REAL2'))
   call unit_check('rget', rget('NOTTHERE').eq.0,           'NOTTHERE',rget('NOTTHERE'))
   call unit_check('dget', ieee_is_nan(dget('BAD')),        'BAD',dget('BAD'))
   call unit_check('rget', rget('BLANK').eq.0,              'BLANK',rget('BLANK'))
   call unit_check_done('rget',msg='')
end subroutine test_rget
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rgets()
integer        :: ier
real,allocatable :: r(:)
   call unit_check_start('rgets',msg=' direct tests of rgets(3f)')
   call store('MY_REAL1','100.0e0 0.0e0 300.33333e2','define',ier)
   call store('MY_REAL2',-1234.0e-20,'define',ier)
   call store('BAD','sfs','define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('rgets', all(rgets('MY_REAL1').eq.[100.0e0,0.0e0,300.33333e2]),'MY_REAL1')
   call unit_check('rgets', all(rgets('MY_REAL2').eq.[-1234.0e-20]),              'MY_REAL2')
   call unit_check('rgets', size(rgets('NOTTHERE')).eq.0,                         'NOTTHERE')
   r=rgets('BAD')
   if(size(r).gt.0)then
      call unit_check('rgets', size(r).eq.1.and.ieee_is_nan(r(1)),                'BAD')
   else
      call unit_check('rgets', size(r).eq.1,                                      'BAD')
   endif
   call unit_check('rgets', size(rgets('BLANK')).eq.0,                            'BLANK')
   call unit_check_done('rgets',msg='')
end subroutine test_rgets
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_setprompts()
   call unit_check_start('setprompts',msg='')
   !!call unit_check('setprompts', 0.eq.0, 'checking',100)
   call unit_check_done('setprompts',msg='')
end subroutine test_setprompts
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sget()
integer        :: ier
   call unit_check_start('sget',msg=' direct tests of sget(3f)')
   call store('MY_STRING1','100 0 -321','define',ier)
   call store('MY_STRING2',-1234,'define',ier)
   call store('BLANK',' ','define',ier)
   call unit_check('sget', sget('MY_STRING1').eq.'100 0 -321',               'MY_STRING1')
   call unit_check('sget', sget('MY_STRING2').eq.'-1234',                    'MY_STRING2')
   call unit_check('sget', len(sget('NOTTHERE')).eq.0,                       'NOTTHERE')
   call unit_check('sget', len(sget('BLANK')).ne.0.and.sget('BLANK').eq.' ', 'BLANK')
   call unit_check_done('sget',msg='')
end subroutine test_sget
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sgets()
integer        :: ier
   call unit_check_start('sgets',msg=' direct tests of sgets(3f)')
   call store('MY_STRING1','100 0 -321','define',ier)
   call store('MY_STRING2',-1234,       'define',ier)
   call store('BLANK',' ',              'define',ier)
   call unit_check('sgets', all(sgets('MY_STRING1').eq. ['100 ','0   ','-321']),      'MY_STRING1')
   call unit_check('sgets', all(sgets('MY_STRING2').eq.['-1234']),                    'MY_STRING2')
   call unit_check('sgets', all(sgets('NOTTHERE_SGETS').eq.[char(0)]),                'NOTTHERE_SGETS')
   call unit_check('sgets', size(sgets('BLANK')).eq.0.and.len(sgets('BLANK')).eq.0,   'BLANK')
   call unit_check_done('sgets',msg='')
end subroutine test_sgets
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_show()
   call unit_check_start('show',msg='')
   !!call unit_check('show', 0.eq.0, 'checking',100)
   call unit_check_done('show',msg='')
end subroutine test_show
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_store()
integer :: ier
 call unit_check_start('store',msg='')

 call store('MY_STRING','My string value','add',ier)
 if(ier.ne.0)write(*,*)'ERROR: could not store MY_STRING ier=',ier
 call store('MY_INTEGER',12345678,'no_add',ier)    ! this will be an error because MY does not have the -INTEGER keyword defined
 call store('MY_INTEGER',1234,'add',ier)           ! now define MY_INTEGER
 call store('MY_INTEGER',987654321,'add',ier)      ! if 'no_add' it will APPEND to current string
 call store('MY_REAL',1234.5678,'add',ier)
 call store('MY_DOUBLE',123.4567d8,'add',ier)
 call store('MY_LOGICAL',.true.,'add',ier)
 call store('MY_INTEGER',987654321,'replace',ier)  ! if 'replace' is used REPLACE instead of APPEND to current value
 call store('MY_UNKNOWN',987654321,'replace',ier)  ! 'replace' can only replace an existing entry, not add one

 call unit_check('store',sget('MY_STRING')  == 'My string value',    'MY_STRING',sget('MY_STRING'),'My string value')
 call unit_check('store',rget('MY_REAL')    == 1234.5677,            'MY_REAL',rget('MY_REAL'),1234.5677)
 call unit_check('store',lget('MY_LOGICAL'),                         'MY_LOGICAL',lget('MY_LOGICAL'),.true.)
 call unit_check('store',iget('MY_INTEGER') == 987654321,            'MY_INTEGER',iget('MY_INTEGER'),987654321)
 call unit_check('store',dget('MY_DOUBLE')  == 12345670000.000000d0, 'MY_DOUBLE',dget('MY_DOUBLE'),12345670000.000000d0)

 call unit_check_done('store',msg='')
end subroutine test_store
end subroutine test_suite_M_kracken
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmd_args_to_dictionary(verb)

! ident_21="@(#)M_kracken::cmd_args_to_dictionary(3f): convert command line arguments to dictionary entries using alternate style"

character(len=*),intent(in)  :: verb
integer                      :: pointer
character(len=:),allocatable :: lastkeyword
integer                      :: i
integer                      :: ilength, istatus, imax
integer                      :: ibig
character(len=:),allocatable :: current_argument
character(len=:),allocatable :: current_argument_padded
character(len=:),allocatable :: dummy
character(len=:),allocatable :: oldvalue
logical                      :: nomore
logical                      :: keyword_single
integer                      :: ierr
! revisit this. Assuming .false. and .true. can only occur as values for a logical switch is not valid but is a low risk.
! this could be particularly strange because .false. and .true. get converted to .true. to handle a duplicate logical switch
   if(allocated(unnamed))then
      deallocate(unnamed)
   endif
   ibig=longest_command_argument() ! bug in gfortran. len=0 should be fine
   allocate(character(len=ibig) ::unnamed(0))
   unnamed=[character(len=ibig) ::]            ! kludge

   nomore=.false.
   pointer=0
   lastkeyword=' '
   keyword_single=.true.
   GET_ARGS: do i=1, command_argument_count()                                                        ! insert and replace entries
      call get_command_argument(number=i,length=ilength,status=istatus)                              ! get next argument
      if(istatus /= 0) then                                                                          ! stop program on error
         call journal('sc','*prototype_and_cmd_args_to_nlist* error obtaining argument ',i,&
            &'status=',istatus,&
            &'length=',ilength)
         exit GET_ARGS
      else
         if(allocated(current_argument))deallocate(current_argument)
         ilength=max(ilength,1)
         allocate(character(len=ilength) :: current_argument)
         call get_command_argument(number=i,value=current_argument,length=ilength,status=istatus)    ! get next argument
         if(istatus /= 0) then                                                                       ! stop program on error
            call journal('sc','*prototype_and_cmd_args_to_nlist* error obtaining argument ',i,&
               &'status=',istatus,&
               &'length=',ilength,&
               &'target length=',len(current_argument))
            exit GET_ARGS
          endif
      endif

      if(current_argument.eq.'--')then ! everything after this goes into the unnamed array
         nomore=.true.
         pointer=0
         cycle
      endif
      dummy=current_argument//'   '
      current_argument_padded=current_argument//'   '
      if(.not.nomore.and.current_argument_padded(1:2).eq.'--'.and.index('0123456789.',dummy(3:3)).eq.0)then ! beginning of long word
         keyword_single=.false.
         if(lastkeyword.ne.'')then
            call ifnull()
         endif
         call locate(dict_verbs,verb//'_'//current_argument_padded(3:),pointer)
         if(pointer.le.0)then
            call journal('sc','*cmd_args_to_dictionary* UNKNOWN LONG KEYWORD: ',current_argument)
            call print_kracken_dictionary('OPTIONS:')
            stop 1
         endif
         lastkeyword=verb//'_'//trim(current_argument_padded(3:))
      elseif(.not.nomore .and. current_argument_padded(1:1).eq.'-' .and. index('0123456789.',dummy(2:2)).eq.0 .and. &
         & current_argument_padded.ne.'-')then  ! short word
         keyword_single=.true.
         if(lastkeyword.ne.'')then
            call ifnull()
         endif
         call locate(dict_verbs,verb//'_'//current_argument_padded(2:),pointer)
         if(pointer.le.0)then
            call journal('sc','*cmd_args_to_dictionary* UNKNOWN SHORT KEYWORD: ',current_argument)
            call print_kracken_dictionary('OPTIONS:')
            stop 2
         endif
         lastkeyword=verb//'_'//trim(current_argument_padded(2:))
      elseif(pointer.eq.0)then                                                                           ! unnamed arguments
         imax=max(len(unnamed),len(current_argument))
         !!write(*,*)'GOT HERE 4 UNNAMED:',current_argument,size(unnamed)
         unnamed=[character(len=imax) :: unnamed,current_argument]
      else
         if(debug)then
            call journal('sc','POINTER=',pointer,' KEYWORD=',dict_verbs(pointer),' VALUE=',current_argument,' LENGTH=',ilength)
         endif
         oldvalue=sget(dict_verbs(pointer))//'  '
         if(upper(oldvalue).eq.'.F'.or.upper(oldvalue).eq.'.T')then  ! assume boolean parameter
            if(current_argument.ne.' ')then
               imax=max(len(unnamed),len(current_argument))
               !!write(*,*)'GOT HERE 5',current_argument,size(unnamed)
               unnamed=[character(len=imax) :: unnamed,current_argument]
            endif
            current_argument='.true.'
         endif
         !!call journal('sc','GOT HERE D KEY=',dict_verbs(pointer),'VALUE=',current_argument,&
         !!   &'OLDVALUE=',oldvalue,'LASTKEYWORD=',lastkeyword)
         if(upper(oldvalue).eq.'.FALSE.'.or.upper(oldvalue).eq.'.TRUE.')then
            imax=max(len(unnamed),len(current_argument))
            unnamed=[character(len=imax) :: unnamed,current_argument]
            call store(dict_verbs(pointer),'.true.','replace',ierr)
         else
            call store(dict_verbs(pointer),current_argument,'replace',ierr)
         endif
         pointer=0
         lastkeyword=''
      endif
   enddo GET_ARGS
   if(lastkeyword.ne.'')then
      call ifnull()
   endif

contains
subroutine ifnull()
   oldvalue=sget(lastkeyword)//'  '
   if(upper(oldvalue).eq.'.F'.or.upper(oldvalue).eq.'.T')then
      !!call journal('sc','GOT HERE E','KEY=',dict_verbs(pointer),'VALUE ',oldvalue,'TO T',' LASTKEYWORD=',lastkeyword)
      call store(lastkeyword,'.true.','replace',ierr)
   else
      !!call journal('sc','GOT HERE F','KEY=',dict_verbs(pointer),'VALUE ',oldvalue,'TO BLANK',' LASTKEYWORD=',lastkeyword)
      if(upper(oldvalue).eq.'.FALSE.'.or.upper(oldvalue).eq.'.TRUE')then
         call store(lastkeyword,'.true.','replace',ierr)
      else
         call store(lastkeyword,' ','replace',ierr)
      endif
   endif
end subroutine ifnull

end subroutine cmd_args_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine print_kracken_dictionary(header)
character(len=*),intent(in) :: header
integer                     :: i
   if(allocated(dict_verbs))then
      if(size(dict_verbs).gt.0)then
         write(*,'(a,t21,1x,a5,a5,1x,a)')'OPTION','COUNT','LEN','VALUE'
         do i=1,size(dict_verbs)
            write(*,'(a,t21,i5,1x,i5,1x,"[",a,"]")') dict_verbs(i), dict_calls(i), dict_lens(i),trim(dict_vals(i))
         enddo
      endif
   endif
   if(allocated(unnamed))then
      if(size(unnamed).gt.0)then
         write(*,*)'UNNAMED:'
         do i=1,size(unnamed)
            write(*,'(i5.5,"[",a,"]")')i,unnamed(i)
         enddo
      endif
   endif
end subroutine print_kracken_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_kracken
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! HISTORY:
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20191018
! added 'kracken_method' and the 'args' method for users that prefer a more 1-like feel requiring quoted arguments on input
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20160414
! multiple uses of a keyword appends values together with a space in between rather than taking right-most definition
!-----------------------------------------------------------------------------------------------------------------------------------
! updated 20151228
! merged command-parsing module back into kracken. Makes kracken a little dirty and makes it require M_verify and M_journal
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
