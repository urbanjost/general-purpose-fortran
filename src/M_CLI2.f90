!VERSION 1.0 2020-01-15
!VERSION 2.0 2020-08-02
!VERSION 3.0 2020-10-21  LONG:SHORT syntax
!VERSION 3.1 2020-11-15  LONG:SHORT:: syntax
!VERSION 3.2 2023-02-05  set_mode()
!VERSION 3.3 2024-08-18  autoresponse
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    M_CLI2(3fm) - [ARGUMENTS:M_CLI2::INTRO] command line argument
!!    parsing using a prototype command
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   Available procedures and variables:
!!
!!      ! basic procedures
!!      use M_CLI2, only : set_args, get_args, specified, set_mode
!!      ! convenience functions
!!      use M_CLI2, only : dget, iget, lget, rget, sget, cget
!!      use M_CLI2, only : dgets, igets, lgets, rgets, sgets, cgets
!!      ! variables
!!      use M_CLI2, only : unnamed, remaining, args
!!      ! working with non-allocatable strings and arrays
!!      use M_CLI2, only : get_args_fixed_length, get_args_fixed_size
!!      ! special function for creating subcommands
!!      use M_CLI2, only : get_subcommand(3)
!!
!!##DESCRIPTION
!!    The M_CLI2 module cracks a Unix-style command line.
!!
!!    Typically one call to SET_ARGS(3) is made to define the command
!!    arguments, set default values and parse the command line. Then a call
!!    is made to the convenience procedures or GET_ARGS(3) proper for each
!!    command keyword to obtain the argument values.
!!
!!    Detailed descriptions of each procedure and example programs are
!!    included.
!!
!!##EXAMPLES
!!
!!
!! Sample minimal program:
!!
!!     program minimal
!!     use M_CLI2,  only : set_args, lget, rget, sgets
!!     implicit none
!!     real    :: x, y
!!     integer :: i
!!     character(len=:),allocatable :: version_text(:), help_text(:)
!!     character(len=:),allocatable :: filenames(:)
!!        ! define and crack command line.
!!        ! creates argument --yvalue with short name y with default value 0
!!        ! creates argument --xvalue with short name x with default value 0
!!        ! creates boolean argument
!!        call setup() ! define help text and version text
!!        call set_args(' --yvalue:y 0.0 --xvalue:x 0.0 --debug F',&
!!             & help_text=help_text,&
!!             & version_text=version_text)
!!        ! get values
!!        x=rget('xvalue')
!!        y=rget('yvalue')
!!        if(lget('debug'))then
!!           write(*,*)'X=',x
!!           write(*,*)'Y=',y
!!           write(*,*)'ATAN2(Y,X)=',atan2(x=x,y=y)
!!        else
!!           write(*,*)atan2(x=x,y=y)
!!        endif
!!        filenames=sgets() ! sgets(3) with no name gets "unnamed" values
!!        if(size(filenames) > 0)then
!!           write(*,'(g0)')'filenames:'
!!           write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
!!        endif
!!     contains
!!     subroutine setup()
!!
!!     help_text=[character(len=80) :: &
!!                 & "wish I put instructions", &
!!                 & "here I suppose.        ", &
!!                 & " "]
!!
!!     version_text=[character(len=80) :: "version 1.0","author: me"]
!!
!!     end subroutine setup
!!     end program minimal
!!
!! which may be called in various ways:
!!
!!     mimimal -x 100.3 -y 3.0e4
!!     mimimal --xvalue=300 --debug
!!     mimimal --yvalue 400
!!     mimimal -x 10 file1 file2 file3
!!
!! Sample program using get_args() and variants
!!
!!     program demo_M_CLI2
!!     use M_CLI2,  only : set_args, get_args
!!     use M_CLI2,  only : filenames=>unnamed
!!     use M_CLI2,  only : get_args_fixed_length, get_args_fixed_size
!!     implicit none
!!     integer,parameter            :: dp=kind(0.0d0)
!!     integer                      :: i
!!      !
!!      ! Define ARGS
!!     real                         :: x, y, z
!!     logical                      :: l, lbig
!!     character(len=40)            :: label    ! FIXED LENGTH
!!     real(kind=dp),allocatable    :: point(:)
!!     logical,allocatable          :: logicals(:)
!!     character(len=:),allocatable :: title    ! VARIABLE LENGTH
!!     real                         :: p(3)     ! FIXED SIZE
!!     logical                      :: logi(3)  ! FIXED SIZE
!!      !
!!      ! DEFINE AND PARSE (TO SET INITIAL VALUES) COMMAND LINE
!!      !   o set a value for all keywords.
!!      !   o double-quote strings, strings must be at least one space
!!      !     because adjacent double-quotes designate a double-quote
!!      !     in the value.
!!      !   o set all logical values to F
!!      !   o numeric values support an "e" or "E" exponent
!!      !   o for lists delimit with a comma, colon, or space
!!     call set_args('                         &
!!             & -x 1 -y 2 -z 3                &
!!             & -p -1 -2 -3                   &
!!             & --point 11.11, 22.22, 33.33e0 &
!!             & --title "my title" -l F -L F  &
!!             & --logicals  F F F F F         &
!!             & --logi F T F                  &
!!             & --label " " &
!!             ! note space between quotes is required
!!             & ')
!!      ! Assign values to elements using G_ARGS(3).
!!      ! non-allocatable scalars can be done up to twenty per call
!!     call get_args('x',x, 'y',y, 'z',z, 'l',l, 'L',lbig)
!!      ! As a convenience multiple pairs of keywords and variables may be
!!      ! specified if and only if all the values are scalars and the CHARACTER
!!      ! variables are fixed-length or pre-allocated.
!!      !
!!      ! After SET_ARGS(3) has parsed the command line
!!      ! GET_ARGS(3) retrieves the value of keywords except for
!!      ! two special cases. For fixed-length CHARACTER variables
!!      ! see GET_ARGS_FIXED_LENGTH(3). For fixed-size arrays see
!!      ! GET_ARGS_FIXED_SIZE(3).
!!      !
!!      ! allocatables should be done one at a time
!!     call get_args('title',title) ! allocatable string
!!     call get_args('point',point) ! allocatable arrays
!!     call get_args('logicals',logicals)
!!      !
!!      ! less commonly ...
!!
!!      ! for fixed-length strings
!!     call get_args_fixed_length('label',label)
!!
!!      ! for non-allocatable arrays
!!     call get_args_fixed_size('p',p)
!!     call get_args_fixed_size('logi',logi)
!!      !
!!      ! all done parsing, use values
!!     write(*,*)'x=',x, 'y=',y, 'z=',z, x+y+z
!!     write(*,*)'p=',p
!!     write(*,*)'point=',point
!!     write(*,*)'title=',title
!!     write(*,*)'label=',label
!!     write(*,*)'l=',l
!!     write(*,*)'L=',lbig
!!     write(*,*)'logicals=',logicals
!!     write(*,*)'logi=',logi
!!      !
!!      ! unnamed strings
!!      !
!!     if(size(filenames) > 0)then
!!        write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
!!     endif
!!      !
!!     end program demo_M_CLI2
!!
!! Results:
!!
!!  >  x=1.00000000     y=2.00000000     z=3.00000000       6.00000000
!!  >  p=  -1.00000000      -2.00000000      -3.00000000
!!  >  point=   11.109999999999999 22.219999999999999 33.329999999999998
!!  >  title=my title
!!  >  label=
!!  >  l= F
!!  >  L= F
!!  >  logicals= F F F F F
!!  >  logi= F T F
!!
!!##AUTHOR
!!     John S. Urban, 2019
!!##LICENSE
!!     Public Domain
!!##SEE ALSO
!!     + get_args(3)
!!     + get_args_fixed_size(3)
!!     + get_args_fixed_length(3)
!!     + get_subcommand(3)
!!     + set_mode(3)
!!     + specified(3)
!!
!! Note that the convenience routines are described under get_args(3):
!! dget(3), iget(3), lget(3), rget(3), sget(3), cget(3) dgets(3),
!! igets(3), lgets(3), rgets(3), sgets(3), cgets(3)
!===================================================================================================================================
module M_CLI2
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT, warn=>OUTPUT_UNIT
implicit none
private

integer,parameter,private :: dp=kind(0.0d0)
integer,parameter,private :: sp=kind(0.0)

character(len=*),parameter          :: gen='(*(g0))'
character(len=1),parameter          :: slash=achar(47)  ! ichar("/")
character(len=1),parameter          :: bslash=achar(92) ! ichar("\") ! some compilers default to \ being like C escape character

character(len=:),allocatable,public :: unnamed(:)
character(len=:),allocatable,public :: args(:)
character(len=:),allocatable,public :: remaining
public                              :: set_mode
public                              :: set_args
public                              :: get_subcommand
public                              :: get_args
public                              :: get_args_fixed_size
public                              :: get_args_fixed_length
public                              :: specified
public                              :: print_dictionary

public                              :: dget, iget, lget, rget, sget, cget
public                              :: dgets, igets, lgets, rgets, sgets, cgets

type option
   character(:),allocatable :: shortname
   character(:),allocatable :: longname
   character(:),allocatable :: value
   integer                  :: length
   logical                  :: present_in
   logical                  :: mandatory
end type option

character(len=:),allocatable,save :: keywords(:)
character(len=:),allocatable,save :: shorts(:)
character(len=:),allocatable,save :: values(:)
integer,allocatable,save          :: counts(:)
logical,allocatable,save          :: present_in(:)
logical,allocatable,save          :: mandatory(:)

logical,save                      :: G_DEBUG=.false.
logical,save                      :: G_UNDERDASH=.false.
logical,save                      :: G_NODASHUNDER=.false.
logical,save                      :: G_IGNORELONGCASE=.false.  ! ignore case of long keywords
logical,save                      :: G_IGNOREALLCASE=.false.   ! ignore case of long and short keywords
logical,save                      :: G_STRICT=.false.          ! strict short and long rules or allow -longname and --shortname
logical,save                      :: G_APPEND=.true.           ! whether to append or replace when duplicate keywords found

logical,save                      :: G_keyword_single_letter=.true.
character(len=:),allocatable,save :: G_passed_in
logical,save                      :: G_remaining_on, G_remaining_option_allowed
character(len=:),allocatable,save :: G_remaining
character(len=:),allocatable,save :: G_subcommand              ! possible candidate for a subcommand
character(len=:),allocatable,save :: G_STOP_MESSAGE
integer,save                      :: G_STOP
logical,save                      :: G_QUIET
character(len=:),allocatable,save :: G_PREFIX

! try out response files
! CLI_RESPONSE_FILE is left public for backward compatibility, but should be set via "set_mode('response_file')
logical,save,public               :: CLI_RESPONSE_FILE=.false. ! allow @name abbreviations
logical,save,public               :: CLI_AUTO_RESPONSE_FILE=.false. ! allow @name abbreviations but call @$0 automatically
logical,save,public               :: CLI_AUTO_QUIET=.false.
logical,save                      :: G_OPTIONS_ONLY            ! process response file only looking for options for get_subcommand()
logical,save                      :: G_RESPONSE                ! allow @name abbreviations
character(len=:),allocatable,save :: G_RESPONSE_IGNORED
character(len=:),allocatable,save :: G_RESPONSE_PREFIX

! return allocatable arrays
interface  get_args;  module  procedure  get_anyarray_d;  end interface  ! any size array
interface  get_args;  module  procedure  get_anyarray_i;  end interface  ! any size array
interface  get_args;  module  procedure  get_anyarray_r;  end interface  ! any size array
interface  get_args;  module  procedure  get_anyarray_x;  end interface  ! any size array
interface  get_args;  module  procedure  get_anyarray_c;  end interface  ! any size array and any length
interface  get_args;  module  procedure  get_anyarray_l;  end interface  ! any size array

! return scalars
interface  get_args;  module  procedure  get_scalar_d;               end interface
interface  get_args;  module  procedure  get_scalar_i;               end interface
interface  get_args;  module  procedure  get_scalar_real;            end interface
interface  get_args;  module  procedure  get_scalar_complex;         end interface
interface  get_args;  module  procedure  get_scalar_logical;         end interface
interface  get_args;  module  procedure  get_scalar_anylength_c;     end interface  ! any length

! multiple scalars
interface  get_args;  module  procedure  many_args;               end  interface

! return non-allocatable arrays
! said in conflict with get_args_*. Using class to get around that.
! that did not work either. Adding size parameter as optional parameter works; but using a different name
interface  get_args_fixed_size;  module procedure get_fixedarray_class;            end interface ! any length, fixed size array
!interface   get_args;           module procedure get_fixedarray_d;                end interface
!interface   get_args;           module procedure get_fixedarray_i;                end interface
!interface   get_args;           module procedure get_fixedarray_r;                end interface
!interface   get_args;           module procedure get_fixedarray_l;                end interface
!interface   get_args;           module procedure get_fixedarray_fixed_length_c;   end interface

interface   get_args_fixed_length;  module  procedure  get_args_fixed_length_a_array; end interface  ! fixed length any size array
interface   get_args_fixed_length;  module  procedure  get_args_fixed_length_scalar_c;  end interface       ! fixed length

! Generic subroutine inserts element into allocatable array at specified position

! find PLACE in sorted character array where value can be found or should be placed
interface  locate_;  module procedure locate_c                            ; end interface

! insert entry into a sorted allocatable array at specified position
interface  insert_;  module procedure insert_c,      insert_i,  insert_l  ; end interface

! replace entry by index from a sorted allocatable array if it is present
interface  replace_; module procedure replace_c,     replace_i, replace_l ; end interface

! delete entry by index from a sorted allocatable array if it is present
interface  remove_;  module procedure remove_c,      remove_i,  remove_l  ; end interface

! convenience functions
interface cgets;module procedure cgs, cg;end interface
interface dgets;module procedure dgs, dg;end interface
interface igets;module procedure igs, ig;end interface
interface lgets;module procedure lgs, lg;end interface
interface rgets;module procedure rgs, rg;end interface
interface sgets;module procedure sgs, sg;end interface

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    check_commandline(3) - [ARGUMENTS:M_CLI2]check command and process
!!    pre-defined options
!!
!!##SYNOPSIS
!!
!!      subroutine check_commandline(help_text,version_text,ierr,errmsg)
!!
!!       character(len=*),intent(in),optional :: help_text(:)
!!       character(len=*),intent(in),optional :: version_text(:)
!!
!!##DESCRIPTION
!!     Checks the commandline  and processes the implicit --help, --version,
!!     --verbose, and --usage parameters.
!!
!!     If the optional text values are supplied they will be displayed by
!!     --help and --version command-line options, respectively.
!!
!!##OPTIONS
!!
!!     HELP_TEXT     if present, will be displayed if program is called with
!!                   --help switch, and then the program will terminate. If
!!                   not supplied, the command line initialized string will be
!!                   shown when --help is used on the commandline.
!!
!!     VERSION_TEXT  if present, will be displayed if program is called with
!!                   --version switch, and then the program will terminate.
!!
!!        If the first four characters of each line are "@(#)" this prefix
!!        will not be displayed and the last non-blank letter will be
!!        removed from each line. This if for support of the SCCS what(1)
!!        command. If you do not have the what(1) command on GNU/Linux and
!!        Unix platforms you can probably see how it can be used to place
!!        metadata in a binary by entering:
!!
!!         strings demo_commandline|grep '@(#)'|tr '>' '\n'|sed -e 's/  */ /g'
!!
!!##EXAMPLES
!!
!!
!! Typical usage:
!!
!!      program check_commandline
!!      use M_CLI2,  only : unnamed, set_args, get_args
!!      implicit none
!!      integer                      :: i
!!      character(len=:),allocatable :: version_text(:), help_text(:)
!!      real               :: x, y, z
!!      character(len=*),parameter :: cmd='-x 1 -y 2 -z 3'
!!         version_text=[character(len=80) :: "version: 1.0","author: me"]
!!         help_text=[character(len=80) :: &
!!                 & "wish I put instructions","here","I suppose?"]
!!         call set_args(cmd,help_text,version_text)
!!         call get_args('x',x,'y',y,'z',z)
!!         ! All done cracking the command line. Use the values in your program.
!!         write (*,*)x,y,z
!!         ! the optional unnamed values on the command line are
!!         ! accumulated in the character array "UNNAMED"
!!         if(size(unnamed) > 0)then
!!            write (*,'(a)')'files:'
!!            write (*,'(i6.6,3a)') (i,'[',unnamed(i),']',i=1,size(unnamed))
!!         endif
!!      end program check_commandline
!===================================================================================================================================
subroutine check_commandline(help_text,version_text)
character(len=*),intent(in),optional :: help_text(:)
character(len=*),intent(in),optional :: version_text(:)
character(len=:),allocatable         :: line
integer                              :: i
integer                              :: istart
integer                              :: iback
!character(len=255)                   :: string
   if(get('usage') == 'T')then
      ! kludge to test interactive mode concept
      !   do
      !      call print_dictionary_usage()
      !      read(*,'(a)')string
      !      if(string.eq.'.')exit
      !      call prototype_to_dictionary(string)
      !   enddo
      call print_dictionary_usage()
      call mystop(32)
      return
   endif
   if(present(help_text))then
      if(get('help') == 'T')then
         do i=1,size(help_text)
            call journal(help_text(i))
         enddo
         call mystop(1,'displayed help text')
         return
      endif
   elseif(get('help') == 'T')then
      call default_help()
      call mystop(2,'displayed default help text')
      return
   endif
   if(present(version_text))then
      if(get('version') == 'T')then
         istart=1
         iback=0
         if(size(version_text) > 0)then
            if(index(version_text(1),'@'//'(#)') == 1)then ! allow for what(1) syntax
               istart=5
               iback=1
            endif
         endif
         do i=1,size(version_text)
            !xINTEL BUG*!call journal(version_text(i)(istart:len_trim(version_text(i))-iback))
            line=version_text(i)(istart:len_trim(version_text(i))-iback)
            call journal(line)
         enddo
         call mystop(3,'displayed version text')
         return
      endif
   elseif(get('version') == 'T')then

      if(G_QUIET)then
         G_STOP_MESSAGE = 'no version text'
      else
         call journal('*check_commandline* no version text')
      endif
      call mystop(4,'displayed default version text')
      return
   endif
contains
subroutine default_help()
character(len=:),allocatable :: cmd_name
integer :: ilength
   call get_command_argument(number=0,length=ilength)
   if(allocated(cmd_name))deallocate(cmd_name)
   allocate(character(len=ilength) :: cmd_name)
   call get_command_argument(number=0,value=cmd_name)
   G_passed_in=G_passed_in//repeat(' ',len(G_passed_in))
   G_passed_in=replace_str(G_passed_in, ' --', NEW_LINE('A')//' --')
   if(.not.G_QUIET)then
      call journal(cmd_name,G_passed_in) ! no help text, echo command and default options
   endif
   deallocate(cmd_name)
end subroutine default_help
end subroutine check_commandline
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    set_args(3) - [ARGUMENTS:M_CLI2] command line argument parsing
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     subroutine set_args(prototype,help_text,version_text,ierr,errmsg)
!!
!!      character(len=*),intent(in),optional              :: prototype
!!      character(len=*),intent(in),optional              :: help_text(:)
!!      character(len=*),intent(in),optional              :: version_text(:)
!!      integer,intent(out),optional                      :: ierr
!!      character(len=:),intent(out),allocatable,optional :: errmsg
!!##DESCRIPTION
!!
!!    SET_ARGS(3) requires a unix-like command prototype which defines
!!    the command-line options and their default values. When the program
!!    is executed this and the command-line options are applied and the
!!    resulting values are placed in an internal table for retrieval via
!!    GET_ARGS(3).
!!
!!    The built-in --help and --version options require optional help_text
!!    and version_text values to be provided to be particularly useful.
!!
!!##OPTIONS
!!
!!    PROTOTYPE   composed of all command arguments concatenated
!!                into a Unix-like command prototype string. For example:
!!
!!                 call set_args('-L F --ints 1,2,3 --title "my title" -R 10.3')
!!
!!                Note that the following options are predefined for all
!!                commands:
!!
!!                    --verbose F --usage F --help F --version F
!!
!!                see "DEFINING THE PROTOTYPE" in the next section for
!!                further details.
!!
!!    HELP_TEXT   if present, will be displayed when the program is called with
!!                a --help switch, and then the program will terminate. If
!!                help text is not supplied the command line initialization
!!                string will be echoed.
!!
!!    VERSION_TEXT  if present, any version text defined will be displayed
!!                  when the program is called with a --version switch,
!!                  and then the program will terminate.
!!    IERR          if present a non-zero option is returned when an
!!                  error occurs instead of the program terminating.
!!    ERRMSG        a description of the error if ierr is present.
!!
!!##DEFINING THE PROTOTYPE
!!
!!    o Keywords start with a single dash for short single-character
!!      keywords, and with two dashes for longer keywords.
!!
!!    o all keywords on the prototype MUST get a value.
!!
!!       * logicals must be set to an unquoted F.
!!
!!       * strings must be delimited with double-quotes.
!!         Since internal double-quotes are represented with two
!!         double-quotes the string must be at least one space.
!!
!!    o numeric keywords are not allowed; but this allows
!!      negative numbers to be used as values.
!!
!!    o lists of values should be comma-delimited unless a
!!      user-specified delimiter is used. The prototype
!!      must use the same array delimiters as the call to
!!      get the value.
!!
!!    o to define a zero-length allocatable array make the
!!      value a delimiter (usually a comma) or an empty set
!!      of braces ("[]").
!!
!!    LONG AND SHORT NAMES
!!
!!    Long keywords start with two dashes followed by more than one letter.
!!    Short keywords are a dash followed by a single letter.
!!
!!    o It is recommended long names (--keyword) should be all lowercase
!!      but are case-sensitive by default, unless
!!      "set_mode('ignorelongcase')" or "set_mode('ignoreallcase')" is
!!      in effect.
!!
!!    o Long names should always be more than one character.
!!
!!    o The recommended way to have short names is to suffix the long
!!      name with :LETTER in the definition.
!!
!!      If this syntax is used then logical shorts may be combined on the
!!      command line when "set_mode('strict')" is in effect.
!!
!!    SPECIAL BEHAVIORS
!!
!!    o A special behavior occurs if a keyword name ends in ::.
!!      When the program is called the next parameter is taken as a value
!!      even if it starts with -. This is not generally needed but is
!!      useful in rare cases where non-numeric values starting with a dash
!!      are desired.
!!
!!    o If the prototype ends with "--" a special mode is turned
!!      on where anything after "--" on input goes into the variable
!!      REMAINING with values double-quoted and also into the array ARGS
!!      instead of becoming elements in the UNNAMED array. This is not
!!      needed for normal processing, but was needed for a program that
!!      needed this behavior for its subcommands.
!!
!!      That is, for a normal call all unnamed values go into UNNAMED
!!      and ARGS and REMAINING are ignored. So for
!!
!!          call set_args('-x 10 -y 20 ')
!!
!!      A program invocation such as
!!
!!          xx a b c -- A B C " dd "
!!
!!      results in
!!
!!       UNNAMED= ['a','b','c','A','B','C',' dd']
!!       REMAINING= ''
!!       ARGS= [character(len=0) :: ] ! ie, an empty character array
!!
!!      Whereas
!!
!!       call set_args('-x 10 -y 20 --')
!!
!!      generates the following output from the same program execution:
!!
!!       UNNAMED= ['a','b','c']
!!       REMAINING= '"A" "B" "C" " dd "'
!!       ARGS= ['A','B','C,' dd']
!!
!!##USAGE NOTES
!!      When invoking the program line note the following restrictions
!!      (which often differ between various command-line parsers and are
!!      subject to change):
!!
!!      o By defaul tvalues for duplicate keywords are appended together
!!        with a space separator.
!!
!!      o shuffling is not supported. Values immediately follow their
!!        keywords.
!!
!!      o Only short Boolean keywords can be bundled together.
!!        If allowing bundling is desired call "set_mode('strict')".
!!        This will require prefixing long names with "--" and short
!!        names with "-". Otherwise M_CLI2 relaxes that requirement
!!        and mostly does not care what prefix is used for a keyword.
!!        But this would make it unclear what was meant by "-ox" if
!!        allowed options were "-o F -x F --ox F " for example, so
!!        "strict" mode is required to remove the ambiguity.
!!
!!      o if a parameter value of just "-" is supplied it is
!!        converted to the string "stdin".
!!
!!      o values not needed for a keyword value go into the character
!!        array "UNNAMED".
!!
!!        In addition if the keyword "--" is encountered on the command
!!        line the rest of the command line goes into the character array
!!        "UNNAMED".
!!
!!##EXAMPLES
!!
!!
!! Sample program:
!!
!!     program demo_set_args
!!     use M_CLI2,  only : filenames=>unnamed, set_args, get_args
!!     use M_CLI2,  only : get_args_fixed_size
!!     implicit none
!!     integer                      :: i
!!     ! DEFINE ARGS
!!     real                         :: x, y, z
!!     real                         :: p(3)
!!     character(len=:),allocatable :: title
!!     logical                      :: l, lbig
!!     integer,allocatable          :: ints(:)
!!     !
!!     !  DEFINE COMMAND (TO SET INITIAL VALUES AND ALLOWED KEYWORDS)
!!     !  AND READ COMMAND LINE
!!     call set_args(' &
!!        ! reals
!!        & -x 1 -y 2.3 -z 3.4e2 &
!!        ! integer array
!!        & -p -1,-2,-3 &
!!        ! always double-quote strings
!!        & --title "my title" &
!!        ! string should be a single character at a minimum
!!        & --label " ", &
!!        ! set all logical values to F
!!        & -l F -L F &
!!        ! set allocatable size to zero if you like by using a delimiter
!!        & --ints , &
!!        & ')
!!     ! ASSIGN VALUES TO ELEMENTS
!!     !     SCALARS
!!     call get_args('x',x)
!!     call get_args('y',y)
!!     call get_args('z',z)
!!     call get_args('l',l)
!!     call get_args('L',lbig)
!!     call get_args('ints',ints)      ! ALLOCATABLE ARRAY
!!     call get_args('title',title)    ! ALLOCATABLE STRING
!!     call get_args_fixed_size('p',p) ! NON-ALLOCATABLE ARRAY
!!     ! USE VALUES
!!     write(*,*)'x=',x
!!     write(*,*)'y=',y
!!     write(*,*)'z=',z
!!     write(*,*)'p=',p
!!     write(*,*)'title=',title
!!     write(*,*)'ints=',ints
!!     write(*,*)'l=',l
!!     write(*,*)'L=',lbig
!!     ! UNNAMED VALUES
!!     if(size(filenames) > 0)then
!!        write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
!!     endif
!!     end program demo_set_args
!!
!!##RESPONSE FILES
!!
!!  If you have no interest in using external files as abbreviations
!!  you can ignore this section. Otherwise, before calling set_args(3)
!!  add:
!!
!!     use M_CLI2, only : set_mode
!!     call set_mode('response_file')
!!
!!  M_CLI2 Response files are small files containing CLI (Command Line
!!  Interface) arguments that end with ".rsp" that can be used when command
!!  lines are so long that they would exceed line length limits or so complex
!!  that it is useful to have a platform-independent method of creating
!!  an abbreviation.
!!
!!  Shell aliases and scripts are often used for similar purposes (and
!!  allow for much more complex conditional execution, of course), but
!!  they generally cannot be used to overcome line length limits and are
!!  typically platform-specific.
!!
!!  Examples of commands that support similar response files are the Clang
!!  and Intel compilers, although there is no standard format for the files.
!!
!!  They are read if you add options of the syntax "@NAME" as the FIRST
!!  parameters on your program command line calls. They are not recursive --
!!  that is, an option in a response file cannot be given the value "@NAME2"
!!  to call another response file.
!!
!!  More than one response name may appear on a command line.
!!
!!  They are case-sensitive names.
!!
!!  Note "@" is a special character in Powershell, and therefore requires being
!!  escaped with a grave character or placed in double-quotes if the name
!!  is alphanumeric (using names like "a-b" or other non-alphanumeric
!!  characters also prevents the "@" from being treated specially).
!!
!!
!!   TRAILING AT IS EQUIVALENT TO LEADING AT
!!  Alternatively To accommodate special handling of leading "@" characters
!!  the "@" character may alternatively appear on the end
!!  of the name instead of the beginning. It will be internally moved to
!!  the beginning before processing commences.
!!
!!   CHANGING THE PREFIX IDENTIFIER
!!  It is not recommended in general but the response name prefix may
!!  be changed via the environment variable CLI_RESPONSE_PREFIX if in an
!!  environment preventing the use of the "@" character. Typically "^" or
!!  "%" or "_" are unused characters. In the very worst case an arbitrary
!!  string is allowed such as "rsp_".
!!
!!  Currently this also means changing the prefix in the response files as
!!  well. This may be changed so the @ character usage remains unchanged
!!  in the file.
!!
!!   LOCATING RESPONSE FILES
!!
!!  A search for the response file always starts with the current directory.
!!  The search then proceeds to look in any additional directories specified
!!  with the colon-delimited environment variable CLI_RESPONSE_PATH.
!!
!!  The first resource file found that results in lines being processed
!!  will be used and processing stops after that first match is found. If
!!  no match is found an error occurs and the program is stopped.
!!
!!   RESPONSE FILE SECTIONS
!!
!!  A simple response file just has options for calling the program in it
!!  prefixed with the word "options".
!!  But they can also contain section headers to denote selections that are
!!  only executed when a specific OS is being used, print messages, and
!!  execute system commands.
!!
!!   SEARCHING FOR OSTYPE IN REGULAR FILES
!!
!!  So assuming the name @NAME was specified on the command line a file
!!  named NAME.rsp will be searched for in all the search directories
!!  and then in that file a string that starts with the string @OSTYPE
!!  (if the environment variables $OS and $OSTYPE are not blank. $OSTYPE
!!  takes precedence over $OS).
!!
!!   SEARCHING FOR UNLABELED DIRECTIVES IN REGULAR FILES
!!
!!  Then, the same files will be searched for lines above any line starting
!!  with "@". That is, if there is no special section for the current OS
!!  it just looks at the top of the file for unlabeled options.
!!
!!   SEARCHING FOR OSTYPE AND NAME IN THE COMPOUND FILE
!!
!!  In addition or instead of files with the same name as the @NAME option
!!  on the command line, you can have one file named after the executable
!!  name that contains multiple abbreviation names.
!!
!!  So if your program executable is named EXEC you create a single file
!!  called EXEC.rsp and can append all the simple files described above
!!  separating them with lines of the form @OSTYPE@NAME or just @NAME.
!!
!!  So if no specific file for the abbreviation is found a file called
!!  "EXEC.rsp" is searched for where "EXEC" is the name of the executable.
!!  This file is always a "compound" response file that uses the following format:
!!
!!  Any compound EXEC.rsp file found in the current or searched directories
!!  will be searched for the string @OSTYPE@NAME first.
!!
!!  Then if nothing is found, the less specific line @NAME is searched for.
!!
!!   THE SEARCH IS OVER
!!
!!  Sounds complicated but actually works quite intuitively. Make a file in
!!  the current directory and put options in it and it will be used. If that
!!  file ends up needing different cases for different platforms add a line
!!  like "@Linux" to the file and some more lines and that will only be
!!  executed if the environment variable OSTYPE or OS is "Linux". If no match
!!  is found for named sections the lines at the top before any "@" lines
!!  will be used as a default if no match is found.
!!
!!  If you end up using a lot of files like this you can combine them all
!!  together and put them into a file called "program_name".rsp and just
!!  put lines like @NAME or @OSTYPE@NAME at that top of each selection.
!!
!!  Now, back to the details on just what you can put in the files.
!!
!!##SPECIFICATION FOR RESPONSE FILES
!!
!!   SIMPLE RESPONSE FILES
!!
!!  The first word of a line is special and has the following meanings:
!!
!!    options|-  Command options following the rules of the SET_ARGS(3)
!!               prototype. So
!!                o It is preferred to specify a value for all options.
!!                o double-quote strings.
!!                o give a blank string value as " ".
!!                o use F|T for lists of logicals,
!!                o lists of numbers should be comma-delimited.
!!                o --usage, --help, --version, --verbose, and unknown
!!                  options are ignored.
!!
!!    comment|#  Line is a comment line
!!    system|!   System command.
!!               System commands are executed as a simple call to
!!               system (so a cd(1) or setting a shell variable
!!               would not effect subsequent lines, for example)
!!               BEFORE the command being processed.
!!    print|>    Message to screen
!!    stop       display message and stop program.
!!
!!  NOTE: system commands are executed when encountered, but options are
!!  gathered from multiple option lines and passed together at the end of
!!  processing of the block; so all commands will be executed BEFORE the
!!  command for which options are being supplied no matter where they occur.
!!
!!  So if a program that does nothing but echos its parameters
!!
!!    program testit
!!    use M_CLI2, only : set_args, rget, sget, lget, set_mode
!!    implicit none
!!       real :: x,y                           ; namelist/args/ x,y
!!       character(len=:),allocatable :: title ; namelist/args/ title
!!       logical :: big                        ; namelist/args/ big
!!       call set_mode('response_file')
!!       call set_args('-x 10.0 -y 20.0 --title "my title" --big F')
!!       x=rget('x')
!!       y=rget('y')
!!       title=sget('title')
!!       big=lget('big')
!!       write(*,nml=args)
!!    end program testit
!!
!!  And a file in the current directory called "a.rsp" contains
!!
!!     # defaults for project A
!!     options -x 1000 -y 9999
!!     options --title " "
!!     options --big T
!!
!!  The program could be called with
!!
!!     $myprog     # normal call
!!      X=10.0 Y=20.0 TITLE="my title"
!!
!!     $myprog @a  # change defaults as specified in "a.rsp"
!!     X=1000.0 Y=9999.0 TITLE=" "
!!
!!     # change defaults but use any option as normal to override defaults
!!     $myprog @a -y 1234
!!      X=1000.0 Y=1234.0 TITLE=" "
!!
!!   COMPOUND RESPONSE FILES
!!
!!  A compound response file has the same basename as the executable with a
!!  ".rsp" suffix added. So if your program is named "myprg" the filename
!!  must be "myprg.rsp".
!!
!!    Note that here `basename` means the last leaf of the
!!    name of the program as returned by the Fortran intrinsic
!!    GET_COMMAND_ARGUMENT(0,...) trimmed of anything after a period ("."),
!!    so it is a good idea not to use hidden files.
!!
!!  Unlike simple response files compound response files can contain multiple
!!  setting names.
!!
!!  Specifically in a compound file
!!  if the environment variable $OSTYPE (first) or $OS is set the first search
!!  will be for a line of the form (no leading spaces should be used):
!!
!!    @OSTYPE@alias_name
!!
!!  If no match or if the environment variables $OSTYPE and $OS were not
!!  set or a match is not found then a line of the form
!!
!!    @alias_name
!!
!!  is searched for in simple or compound files. If found subsequent lines
!!  will be ignored that start with "@" until a line not starting with
!!  "@" is encountered. Lines will then be processed until another line
!!  starting with "@" is found or end-of-file is encountered.
!!
!!   COMPOUND RESPONSE FILE EXAMPLE
!!  An example compound file
!!
!!    #################
!!    @if
!!    > RUNNING TESTS USING RELEASE VERSION AND ifort
!!    options test --release --compiler ifort
!!    #################
!!    @gf
!!    > RUNNING TESTS USING RELEASE VERSION AND gfortran
!!    options test --release --compiler gfortran
!!    #################
!!    @nv
!!    > RUNNING TESTS USING RELEASE VERSION AND nvfortran
!!    options test --release --compiler nvfortran
!!    #################
!!    @nag
!!    > RUNNING TESTS USING RELEASE VERSION AND nagfor
!!    options test --release --compiler nagfor
!!    #
!!    #################
!!    # OS-specific example:
!!    @Linux@install
!!    #
!!    # install executables in directory (assuming install(1) exists)
!!    #
!!    system mkdir -p ~/.local/bin
!!    options run --release T --runner "install -vbp -m 0711 -t ~/.local/bin"
!!    @install
!!    STOP INSTALL NOT SUPPORTED ON THIS PLATFORM OR $OSTYPE NOT SET
!!    #
!!    #################
!!    @fpm@testall
!!    #
!!    !fpm test --compiler nvfortran
!!    !fpm test --compiler ifort
!!    !fpm test --compiler gfortran
!!    !fpm test --compiler nagfor
!!    STOP tests complete. Any additional parameters were ignored
!!    #################
!!
!!  Would be used like
!!
!!    fpm @install
!!    fpm @nag --
!!    fpm @testall
!!
!!   NOTES
!!
!!    The intel Fortran compiler now calls the response files "indirect
!!    files" and does not add the implied suffix ".rsp" to the files
!!    anymore. It also allows the @NAME syntax anywhere on the command line,
!!    not just at the beginning. -- 20201212
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine set_args(prototype,help_text,version_text,string,prefix,ierr,errmsg)

! ident_1="@(#) M_CLI2 set_args(3) parse prototype string"

character(len=*),intent(in)                       :: prototype
character(len=*),intent(in),optional              :: help_text(:)
character(len=*),intent(in),optional              :: version_text(:)
character(len=*),intent(in),optional              :: string
character(len=*),intent(in),optional              :: prefix
integer,intent(out),optional                      :: ierr
character(len=:),intent(out),allocatable,optional :: errmsg
character(len=:),allocatable                      :: hold               ! stores command line argument
integer                                           :: ibig
character(len=:),allocatable                      :: debug_mode

   debug_mode= upper(get_env('CLI_DEBUG_MODE','FALSE'))//' '
   select case(debug_mode(1:1))
   case('Y','T')
      G_DEBUG=.true.
   end select

   G_response=CLI_RESPONSE_FILE
   if(CLI_AUTO_RESPONSE_FILE)then
      CLI_AUTO_QUIET=.true.
      G_response=.true.
   endif

   G_options_only=.false.
   G_passed_in=''
   G_STOP=0
   G_STOP_MESSAGE=''
   if(present(prefix))then
      G_PREFIX=prefix
   else
      G_PREFIX=''
   endif
   if(present(ierr))then
      G_QUIET=.true.
   else
      G_QUIET=.false.
   endif
   ibig=longest_command_argument() ! bug in gfortran. len=0 should be fine
   IF(ALLOCATED(UNNAMED)) DEALLOCATE(UNNAMED)
   ALLOCATE(CHARACTER(LEN=IBIG) :: UNNAMED(0))
   if(allocated(args)) deallocate(args)
   allocate(character(len=ibig) :: args(0))

   call wipe_dictionary()
   hold='--version F --usage F --help F --version F '//adjustl(prototype)
   call prototype_and_cmd_args_to_nlist(hold,string)
   if(allocated(G_RESPONSE_IGNORED))then
      if(G_DEBUG)write(*,gen)'<DEBUG>SET_ARGS:G_RESPONSE_IGNORED:',G_RESPONSE_IGNORED
      !if(size(unnamed) /= 0)write(*,*)'LOGIC ERROR'
      call split(G_RESPONSE_IGNORED,unnamed)
   endif

   if(.not.allocated(unnamed))then
       allocate(character(len=0) :: unnamed(0))
   endif
   if(.not.allocated(args))then
       allocate(character(len=0) :: args(0))
   endif
   call check_commandline(help_text,version_text) ! process --help, --version, --usage
   if(present(ierr))then
      ierr=G_STOP
   endif
   if(present(errmsg))then
      errmsg=G_STOP_MESSAGE
   endif
end subroutine set_args
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    get_subcommand(3) - [ARGUMENTS:M_CLI2] special-case routine for
!!    handling subcommands on a command line
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function get_subcommand()
!!
!!     character(len=:),allocatable :: get_subcommand
!!
!!##DESCRIPTION
!!    In the special case when creating a program with subcommands it
!!    is assumed the first word on the command line is the subcommand. A
!!    routine is required to handle response file processing, therefore
!!    this routine (optionally processing response files) returns that
!!    first word as the subcommand name.
!!
!!    It should not be used by programs not building a more elaborate
!!    command with subcommands.
!!
!!##RETURNS
!!    NAME   name of subcommand
!!
!!##EXAMPLES
!!
!! Sample program:
!!
!!    program demo_get_subcommand
!!    !x! SUBCOMMANDS
!!    !x! For a command with subcommands like git(1)
!!    !x! you can make separate namelists for each subcommand.
!!    !x! You can call this program which has two subcommands (run, test),
!!    !x! like this:
!!    !x!    demo_get_subcommand --help
!!    !x!    demo_get_subcommand run -x -y -z --title -l -L
!!    !x!    demo_get_subcommand test --title -l -L --testname
!!    !x!    demo_get_subcommand run --help
!!       implicit none
!!    !x! DEFINE VALUES TO USE AS ARGUMENTS WITH INITIAL VALUES
!!       real               :: x=-999.0,y=-999.0,z=-999.0
!!       character(len=80)  :: title="not set"
!!       logical            :: l=.false.
!!       logical            :: l_=.false.
!!       character(len=80)  :: testname="not set"
!!       character(len=20)  :: name
!!       call parse(name) !x! DEFINE AND PARSE COMMAND LINE
!!       !x! ALL DONE CRACKING THE COMMAND LINE.
!!       !x! USE THE VALUES IN YOUR PROGRAM.
!!       write(*,*)'command was ',name
!!       write(*,*)'x,y,z .... ',x,y,z
!!       write(*,*)'title .... ',title
!!       write(*,*)'l,l_ ..... ',l,l_
!!       write(*,*)'testname . ',testname
!!    contains
!!    subroutine parse(name)
!!    !x! PUT EVERYTHING TO DO WITH COMMAND PARSING HERE FOR CLARITY
!!    use M_CLI2, only : set_args, get_args, get_args_fixed_length
!!    use M_CLI2, only : get_subcommand, set_mode
!!    character(len=*)              :: name    ! the subcommand name
!!    character(len=:),allocatable  :: help_text(:), version_text(:)
!!       call set_mode('response_file')
!!    ! define version text
!!       version_text=[character(len=80) :: &
!!          '@(#)PROGRAM:     demo_get_subcommand            >', &
!!          '@(#)DESCRIPTION: My demo program  >', &
!!          '@(#)VERSION:     1.0 20200715     >', &
!!          '@(#)AUTHOR:      me, myself, and I>', &
!!          '@(#)LICENSE:     Public Domain    >', &
!!          '' ]
!!        ! general help for "demo_get_subcommand --help"
!!        help_text=[character(len=80) :: &
!!         ' allowed subcommands are          ', &
!!         '   * run  -l -L --title -x -y -z  ', &
!!         '   * test -l -L --title           ', &
!!         '' ]
!!       ! find the subcommand name by looking for first word on command
!!       ! not starting with dash
!!       name = get_subcommand()
!!       select case(name)
!!       case('run')
!!        help_text=[character(len=80) :: &
!!         '                                  ', &
!!         ' Help for subcommand "run"        ', &
!!         '                                  ', &
!!         '' ]
!!        call set_args( &
!!        & '-x 1 -y 2 -z 3 --title "my title" -l F -L F',&
!!        & help_text,version_text)
!!        call get_args('x',x)
!!        call get_args('y',y)
!!        call get_args('z',z)
!!        call get_args_fixed_length('title',title)
!!        call get_args('l',l)
!!        call get_args('L',l_)
!!       case('test')
!!        help_text=[character(len=80) :: &
!!         '                                  ', &
!!         ' Help for subcommand "test"       ', &
!!         '                                  ', &
!!         '' ]
!!        call set_args(&
!!        & '--title "my title" -l F -L F --testname "Test"',&
!!        & help_text,version_text)
!!        call get_args_fixed_length('title',title)
!!        call get_args('l',l)
!!        call get_args('L',l_)
!!        call get_args_fixed_length('testname',testname)
!!       case default
!!        ! process help and version
!!        call set_args(' ',help_text,version_text)
!!        write(*,'(*(a))')'unknown or missing subcommand [',trim(name),']'
!!        write(*,'(a)')[character(len=80) ::  &
!!        ' allowed subcommands are          ', &
!!        '   * run  -l -L -title -x -y -z   ', &
!!        '   * test -l -L -title            ', &
!!        '' ]
!!        stop
!!       end select
!!    end subroutine parse
!!    end program demo_get_subcommand
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
function get_subcommand() result(sub)

! ident_2="@(#) M_CLI2 get_subcommand(3) parse prototype string to get subcommand allowing for response files"

character(len=:),allocatable  :: sub
character(len=:),allocatable  :: cmdarg
character(len=:),allocatable  :: array(:)
character(len=:),allocatable  :: prototype
integer                       :: ilongest
integer                       :: i
integer                       :: j
   G_RESPONSE_PREFIX=get_env('CLI_RESPONSE_PREFIX','@')
   G_subcommand=''
   G_options_only=.true.
   sub=''

   if(.not.allocated(unnamed))then
      allocate(character(len=0) :: unnamed(0))
   endif

   ilongest=longest_command_argument()
   allocate(character(len=max(63,ilongest)):: cmdarg)
   cmdarg(:) = ''
   ! look for @NAME if CLI_RESPONSE_FILE=.TRUE. AND LOAD THEM
   do i = 1, command_argument_count()
      call get_command_argument(i, cmdarg)
      call move_from_end(cmdarg)
      cmdarg=change_leading_underscore_to_prefix(cmdarg)
      if(scan(adjustl(cmdarg(1:len(G_RESPONSE_PREFIX))),G_RESPONSE_PREFIX)  ==  1)then
         call get_prototype(cmdarg,prototype)
         call split(prototype,array)
         ! assume that if using subcommands first word not starting with dash is the subcommand
         do j=1,size(array)
            if(adjustl(array(j)(1:1))  /=  '-')then
            G_subcommand=trim(array(j))
            sub=G_subcommand
            exit
         endif
         enddo
      endif
   enddo

   if(G_subcommand /= '')then
      sub=G_subcommand
   elseif(size(unnamed) /= 0)then
      sub=unnamed(1)
   else
      cmdarg(:) = ''
      do i = 1, command_argument_count()
         call get_command_argument(i, cmdarg)
         if(adjustl(cmdarg(1:1))  /=  '-')then
            sub=trim(cmdarg)
           exit
        endif
      enddo
   endif
   G_options_only=.false.
end function get_subcommand
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine move_from_end(string)
character(len=*) :: string
integer          :: iend
! @ is treated as a special character in powershell so allow the prefix to be a suffix and move it to beginning of line
   iend=len_trim(string)
   if(string(iend-len(G_RESPONSE_PREFIX)+1:iend)== G_RESPONSE_PREFIX)then
      string(:)= G_RESPONSE_PREFIX//string(1:iend-len(G_RESPONSE_PREFIX))
   endif
end subroutine move_from_end
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function change_leading_underscore_to_prefix(string) result(newstring)
! CAUSES FILENAMES STaRTING WITH _ TO bE TREATED AS REQUESTS FOR RESPONSE FILES. TURN IT OFF; maybe make optional
character(len=*) :: string
character(len=:),allocatable :: newstring
! @ is treated as a special character in powershell so allow the underscore to be a prefix
!x!   if(string.eq.'')then
!x!      newstring=string
!x!   elseif(string(1:1).eq.'_')then
!x!      newstring=G_RESPONSE_PREFIX//string(2:)
!x!   else
      newstring=string
!x!   endif
end function change_leading_underscore_to_prefix
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    set_usage(3) - [ARGUMENTS:M_CLI2] allow setting a short description
!!    of keywords for the --usage switch
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     subroutine set_usage(keyword,description)
!!
!!      character(len=*),intent(in)     ::  keyword
!!      character(len=*),intent(in)     ::  description
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!     KEYWORD      the name of a command keyword
!!     DESCRIPTION  a brief one-line description of the keyword
!!
!!
!!##EXAMPLES
!!
!! sample program:
!!
!!    program demo_set_usage
!!    use M_CLI2,  only : set_args, igets, rgets, specified, sget, lget
!!    implicit none
!!
!!    integer,allocatable  :: ints(:)
!!    logical              :: flag
!!       call set_args(' --flag:f F --ints:i 1,10,11 ')
!!       call set_usage('flag','This is my flag')
!!       call set_usage('ints','These are my whole numbers')
!!       flag=lget('flag')
!!       ints=igets('ints')
!!       write(*,*)'flag=',flag
!!       write(*,*)'ints=',ints
!!    end program demo_set_usage
!!
!!    Results:
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine set_usage(keyword,description,value)
character(len=*),intent(in) :: keyword
character(len=*),intent(in) :: description
character(len=*),intent(in) :: value
write(*,*)keyword
write(*,*)description
write(*,*)value
! store the descriptions in an array and then apply them when set_args(3) is called.
! alternatively, could allow for a value as well in lieu of the prototype
end subroutine set_usage
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    prototype_to_dictionary(3) - [ARGUMENTS:M_CLI2] parse user command
!!    and store tokens into dictionary
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     recursive subroutine prototype_to_dictionary(string)
!!
!!      character(len=*),intent(in)     ::  string
!!
!!##DESCRIPTION
!!      given a string of form
!!
!!        -var value -var value
!!
!!      define dictionary of form
!!
!!        keyword(i), value(i)
!!
!!      o  string values
!!
!!          o must be delimited with double quotes.
!!          o adjacent double quotes put one double quote into value
!!          o must not be null. A blank is specified as " ", not "".
!!
!!      o  logical values
!!
!!          o logical values must have a value. Use F.
!!
!!      o  leading and trailing blanks are removed from unquoted values
!!
!!
!!##OPTIONS
!!      STRING   string is character input string to define command
!!
!!##RETURNS
!!
!!##EXAMPLES
!!
!! sample program:
!!
!!     call prototype_to_dictionary(' -l F --ignorecase F --title "my title string" -x 10.20')
!!     call prototype_to_dictionary(' --ints 1,2,3,4')
!!
!! Results:
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
recursive subroutine prototype_to_dictionary(string,pass)

! ident_3="@(#) M_CLI2 prototype_to_dictionary(3) parse user command and store tokens into dictionary"

character(len=*),intent(in)   :: string  ! string is character input string of options and values
integer,intent(in),optional   :: pass

character(len=:),allocatable  :: dummy   ! working copy of string
character(len=:),allocatable  :: value
character(len=:),allocatable  :: keyword
character(len=:),allocatable  :: oldvalue
character(len=3)              :: delmt   ! flag if in a delimited string or not
character(len=1)              :: currnt  ! current character being processed
character(len=1)              :: prev    ! character to left of CURRNT
character(len=1)              :: forwrd  ! character to right of CURRNT
integer,dimension(2)          :: ipnt
integer                       :: islen   ! number of characters in input string
integer                       :: ipoint
integer                       :: itype
integer,parameter             :: VAL=1, KEYW=2
integer                       :: ifwd
integer                       :: ibegin
integer                       :: iend
integer                       :: place
integer                       :: pass_local
integer                       :: longest

   if(present(pass))then
      pass_local=pass
   else
      pass_local=1
   endif

   islen=len_trim(string)                               ! find number of characters in input string
   if(islen  ==  0)then                                 ! if input string is blank, even default variable will not be changed
      return
   endif
   dummy=adjustl(string)//'  '

   keyword=""          ! initial variable name
   value=""            ! initial value of a string
   ipoint=0            ! ipoint is the current character pointer for (dummy)
   ipnt(2)=2           ! pointer to position in keyword
   ipnt(1)=1           ! pointer to position in value
   itype=VAL           ! itype=1 for value, itype=2 for variable

   delmt="off"
   prev=" "

   G_keyword_single_letter=.true.
   do
      ipoint=ipoint+1               ! move current character pointer forward
      currnt=dummy(ipoint:ipoint)   ! store current character into currnt
      ifwd=min(ipoint+1,islen)      ! ensure not past end of string
      forwrd=dummy(ifwd:ifwd)       ! next character (or duplicate if last)

      if((currnt=="-" .and. prev==" " .and. delmt == "off" .and. index("0123456789.",forwrd) == 0).or.ipoint > islen)then
         ! beginning of a keyword
         if(forwrd == '-')then                      ! change --var to -var so "long" syntax is supported
            !x!dummy(ifwd:ifwd)='_'
            ipoint=ipoint+1                         ! ignore second - instead (was changing it to _)
            G_keyword_single_letter=.false.         ! flag this is a long keyword
         else
            G_keyword_single_letter=.true.          ! flag this is a short (single letter) keyword
         endif
         if(ipnt(1)-1 >= 1)then                     ! position in value
            ibegin=1
            iend=len_trim(value(:ipnt(1)-1))
            TESTIT: do
               if(iend  ==  0)then                  ! len_trim returned 0, value is blank
                  iend=ibegin
                  exit TESTIT
               elseif(value(ibegin:ibegin) == " ")then
                  ibegin=ibegin+1
               else
                  exit TESTIT
               endif
            enddo TESTIT
            if(keyword /= ' ')then
               if(value=='[]')value=','
               if(pass_local == 1 )then
                  call update(keyword,value)            ! store name and its value
               else
                  oldvalue=get(keyword)
                  select case(oldvalue)
                  case('T','F')
                     call update(keyword,'T')
                     longest=max(len(unnamed),len_trim(value))
                     unnamed=[character(len=longest) :: unnamed,value]
                  case default
                     call update(keyword,value)            ! store name and its value
                  end select
               endif
            elseif( G_remaining_option_allowed)then  ! meaning "--" has been encountered
               if(value=='[]')value=','
               call update('_args_',trim(value))
            else
               !x!write(warn,'(*(g0))')'*prototype_to_dictionary* warning: ignoring string [',trim(value),'] for ',trim(keyword)
               G_RESPONSE_IGNORED=TRIM(VALUE)
               if(G_DEBUG)write(*,gen)'<DEBUG>PROTOTYPE_TO_DICTIONARY:G_RESPONSE_IGNORED:',G_RESPONSE_IGNORED
            endif
         else
            call locate_key(keyword,place)
            if(keyword /= ' '.and.place < 0)then
               call update(keyword,'F')           ! store name and null value (first pass)
            elseif(keyword /= ' ')then
               call update(keyword,' ')           ! store name and null value (second pass)
            elseif(.not.G_keyword_single_letter.and.ipoint-2 == islen) then ! -- at end of line
               G_remaining_option_allowed=.true.  ! meaning for "--" is that everything on commandline goes into G_remaining
            endif
         endif
         itype=KEYW                            ! change to expecting a keyword
         value=""                              ! clear value for this variable
         keyword=""                            ! clear variable name
         ipnt(1)=1                             ! restart variable value
         ipnt(2)=1                             ! restart variable name

      else       ! currnt is not one of the special characters
         ! the space after a keyword before the value
         if(currnt == " " .and. itype  ==  KEYW)then
            ! switch from building a keyword string to building a value string
            itype=VAL
            ! beginning of a delimited value
         elseif(currnt  ==  """".and.itype  ==  VAL)then
            ! second of a double quote, put quote in
            if(prev  ==  """")then
               if(itype == VAL)then
                  value=value//currnt
               else
                  keyword=keyword//currnt
               endif
               ipnt(itype)=ipnt(itype)+1
               delmt="on"
            elseif(delmt  ==  "on")then     ! first quote of a delimited string
               delmt="off"
            else
               delmt="on"
            endif
            if(prev /= """")then  ! leave quotes where found them
               if(itype == VAL)then
                  value=value//currnt
               else
                  keyword=keyword//currnt
               endif
               ipnt(itype)=ipnt(itype)+1
            endif
         else     ! add character to current keyword or value
            if(itype == VAL)then
               value=value//currnt
            else
               keyword=keyword//currnt
            endif
            ipnt(itype)=ipnt(itype)+1
         endif

      endif

      prev=currnt
      if(ipoint <= islen)then
         cycle
      else
         exit
      endif
   enddo

end subroutine prototype_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    specified(3) - [ARGUMENTS:M_CLI2] return true if keyword was present
!!    on command line
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    elemental impure function specified(name)
!!
!!     character(len=*),intent(in) :: name
!!     logical :: specified
!!
!!##DESCRIPTION
!!
!!    specified(3) returns .true. if the specified keyword was present on
!!    the command line.
!!
!!    M_CLI2 intentionally does not have validators except for SPECIFIED(3)
!!    and of course a check whether the input conforms to the type when
!!    requesting a value (with get_args(3) or the convenience functions
!!    like inum(3)).
!!
!!    Fortran already has powerful validation capabilities. Logical
!!    expressions ANY(3) and ALL(3) are standard Fortran features which
!!    easily allow performing the common validations for command line
!!    arguments without having to learn any additional syntax or methods.
!!
!!##OPTIONS
!!
!!    NAME   name of commandline argument to query the presence of. Long
!!           names should always be used.
!!
!!##RETURNS
!!    SPECIFIED  returns .TRUE. if specified NAME was present on the command
!!               line when the program was invoked.
!!
!!##EXAMPLES
!!
!! Sample program:
!!
!!    program demo_specified
!!    use, intrinsic :: iso_fortran_env, only : &
!!    & stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
!!    use M_CLI2,  only : set_args, igets, rgets, specified, sget, lget
!!    implicit none
!!
!!    ! Define args
!!    integer,allocatable  :: ints(:)
!!    real,allocatable     :: floats(:)
!!    logical              :: flag
!!    character(len=:),allocatable :: color
!!    character(len=:),allocatable :: list(:)
!!    integer :: i
!!
!!     call set_args('&
!!        & --color:c "red"       &
!!        & --flag:f F            &
!!        & --ints:i 1,10,11      &
!!        & --floats:T 12.3, 4.56 &
!!        & ')
!!     ints=igets('ints')
!!     floats=rgets('floats')
!!     flag=lget('flag')
!!     color=sget('color')
!!
!!     write(*,*)'color=',color
!!     write(*,*)'flag=',flag
!!     write(*,*)'ints=',ints
!!     write(*,*)'floats=',floats
!!
!!     write(*,*)'was -flag specified?',specified('flag')
!!
!!     ! elemental
!!     write(*,*)specified(['floats','ints  '])
!!
!!     ! If you want to know if groups of parameters were specified use
!!     ! ANY(3) and ALL(3)
!!     write(*,*)'ANY:',any(specified(['floats','ints  ']))
!!     write(*,*)'ALL:',all(specified(['floats','ints  ']))
!!
!!     ! For mutually exclusive
!!     if (all(specified(['floats','ints  '])))then
!!         write(*,*)'You specified both names --ints and --floats'
!!     endif
!!
!!     ! For required parameter
!!     if (.not.any(specified(['floats','ints  '])))then
!!         write(*,*)'You must specify --ints or --floats'
!!     endif
!!
!!    ! check if all values are in range from 10 to 30 and even
!!    write(*,*)'are all numbers good?',all([ints>=10,ints<= 30,(ints/2)*2==ints])
!!
!!    ! perhaps you want to check one value at a time
!!    do i=1,size(ints)
!!       write(*,*)ints(i),[ints(i) >= 10,ints(i) <= 30,(ints(i)/2)*2 == ints(i)]
!!       if(all([ints(i) >= 10,ints(i) <= 30,(ints(i)/2)*2 == ints(i)]) )then
!!          write(*,*)ints(i),'is an even number from 10 to 30 inclusive'
!!       else
!!          write(*,*)ints(i),'is not an even number from 10 to 30 inclusive'
!!       endif
!!    enddo
!!
!!    list = [character(len=10) :: 'red','white','blue']
!!    if( any(color == list) )then
!!       write(*,*)color,'matches a value in the list'
!!    else
!!       write(*,*)color,'not in the list'
!!    endif
!!
!!    if(size(ints).eq.3)then
!!       write(*,*)'ints(:) has expected number of values'
!!    else
!!       write(*,*)'ints(:) does not have expected number of values'
!!    endif
!!
!!    end program demo_specified
!!
!! Default output
!!
!!  > color=red
!!  > flag= F
!!  > ints=           1          10          11
!!  > floats=   12.3000002       4.55999994
!!  > was -flag specified? F
!!  > F F
!!  > ANY: F
!!  > ALL: F
!!  > You must specify --ints or --floats
!!  >           1 F T F
!!  >           1  is not an even number from 10 to 30 inclusive
!!  >          10 T T T
!!  >          10  is an even number from 10 to 30 inclusive
!!  >          11 T T F
!!  >          11  is not an even number from 10 to 30 inclusive
!!  > red matches a value in the list
!!  > ints(:) has expected number of values
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
elemental impure function specified(key)
character(len=*),intent(in) :: key
logical                     :: specified
integer                     :: place
   call locate_key(key,place)                   ! find where string is or should be
   if(place < 1)then
      specified=.false.
   else
      specified=present_in(place)
   endif
end function specified
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    update(3) - [ARGUMENTS:M_CLI2] update internal dictionary given
!!    keyword and value
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     subroutine update(key,val)
!!
!!      character(len=*),intent(in)           :: key
!!      character(len=*),intent(in),optional  :: val
!!##DESCRIPTION
!!      Update internal dictionary in M_CLI2(3fm) module.
!!##OPTIONS
!!      key  name of keyword to add, replace, or delete from dictionary
!!      val  if present add or replace value associated with keyword. If not
!!           present remove keyword entry from dictionary.
!!
!!           If "present" is true, a value will be appended
!!##EXAMPLES
!!
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine update(key,val)
character(len=*),intent(in)           :: key
character(len=*),intent(in),optional  :: val
integer                               :: place, ii
integer                               :: iilen
character(len=:),allocatable          :: val_local
character(len=:),allocatable          :: short
character(len=:),allocatable          :: long
character(len=:),allocatable          :: long_short(:)
integer                               :: isize
logical                               :: set_mandatory
character(len=:),allocatable          :: kludge(:)
   set_mandatory=.false.
   if(G_IGNOREALLCASE) then
      call split(lower(trim(key)),long_short,':',nulls='return') ! split long:short keyword or long:short:: or long:: or short::
   else
      call split(trim(key),long_short,':',nulls='return') ! split long:short keyword or long:short:: or long:: or short::
   endif
   ! check for :: on end
   isize=size(long_short)

   if(isize > 0)then                     ! very special-purpose syntax where if ends in :: next field is a value even
      if(long_short(isize) == '')then    ! if it starts with a dash, for --flags option on fpm(1).
         set_mandatory=.true.
         !================================
         !long_short=long_short(:isize-1)
         kludge=long_short(:isize-1)     ! use kludge to avoid nvfortran bug
         long_short=kludge
         !================================
      endif
   endif

   select case(size(long_short))
   case(0)
      long=''
      short=''
   case(1)
      long=trim(long_short(1))
      if(len_trim(long) == 1)then
         !x!ii= findloc (shorts, long, dim=1) ! if parsing arguments on line and a short keyword look up long value
         ii=maxloc([0,merge(1, 0, shorts == long)],dim=1)
         if(ii > 1)then
            long=keywords(ii-1)
         endif
         short=long
      else
         short=''
      endif
   case(2)
      long=trim(long_short(1))
      short=trim(long_short(2))
   case default
      write(warn,*)'WARNING: incorrect syntax for key: ',trim(key)
      long=trim(long_short(1))
      short=trim(long_short(2))
   end select
   if(G_UNDERDASH) long=replace_str(long,'-','_')
   if(G_NODASHUNDER)then
      long=replace_str(long,'-','')
      long=replace_str(long,'_','')
   endif
   if(G_IGNORELONGCASE.and.len_trim(long) > 1)long=lower(long)
   if(present(val))then
      val_local=val
      iilen=len_trim(val_local)
      call locate_key(long,place)                  ! find where string is or should be
      if(place < 1)then                                ! if string was not found insert it
         call insert_(keywords,long,iabs(place))
         call insert_(values,val_local,iabs(place))
         call insert_(counts,iilen,iabs(place))
         call insert_(shorts,short,iabs(place))
         call insert_(present_in,.true.,iabs(place))
         call insert_(mandatory,set_mandatory,iabs(place))
      else
         if(present_in(place))then                      ! if multiple keywords append values with space between them
            if(G_append)then
               if(values(place)(1:1) == '"')then
               ! UNDESIRABLE: will ignore previous blank entries
                  val_local='"'//trim(unquote(values(place)))//' '//trim(unquote(val_local))//'"'
               else
                  val_local=clipends(values(place))//' '//val_local
               endif
            endif
            iilen=len_trim(val_local)
         endif
         call replace_(values,val_local,place)
         call replace_(counts,iilen,place)
         call replace_(present_in,.true.,place)
      endif
   else                                                 ! if no value is present remove the keyword and related values
      call locate_key(long,place)                       ! check name as long and short
      if(place > 0)then
         call remove_(keywords,place)
         call remove_(values,place)
         call remove_(counts,place)
         call remove_(shorts,place)
         call remove_(present_in,place)
         call remove_(mandatory,place)
      endif
   endif
end subroutine update
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    wipe_dictionary(3fp) - [ARGUMENTS:M_CLI2] reset private M_CLI2(3fm)
!!    dictionary to empty
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!      subroutine wipe_dictionary()
!!##DESCRIPTION
!!      reset private M_CLI2(3fm) dictionary to empty
!!##EXAMPLES
!!
!! Sample program:
!!
!!      program demo_wipe_dictionary
!!      use M_CLI2, only : dictionary
!!         call wipe_dictionary()
!!      end program demo_wipe_dictionary
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine wipe_dictionary()
   if(allocated(keywords))   deallocate(keywords)
   if(allocated(values))     deallocate(values)
   if(allocated(counts))     deallocate(counts)
   if(allocated(shorts))     deallocate(shorts)
   if(allocated(present_in)) deallocate(present_in)
   if(allocated(mandatory))  deallocate(mandatory)
   allocate(character(len=0) :: keywords(0))
   allocate(character(len=0) :: values(0))
   allocate(counts(0))
   allocate(character(len=0) :: shorts(0))
   allocate(present_in(0))
   allocate(mandatory(0))
end subroutine wipe_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    get(3) - [ARGUMENTS:M_CLI2] get dictionary value associated with
!!    key name in private M_CLI2(3fm) dictionary
!!##SYNOPSIS
!!
!!
!!##DESCRIPTION
!!    Get dictionary value associated with key name in private M_CLI2(3fm)
!!    dictionary.
!!##OPTIONS
!!##RETURNS
!!##EXAMPLES
!!
!===================================================================================================================================
function get(key) result(valout)
character(len=*),intent(in)   :: key
character(len=:),allocatable  :: valout
integer                       :: place
   ! find where string is or should be
   call locate_key(key,place)
   if(place < 1)then
      valout=''
   else
      valout=values(place)(:counts(place))
   endif
end function get
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    prototype_and_cmd_args_to_nlist(3) - [ARGUMENTS:M_CLI2] convert
!!    Unix-like command arguments to table
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     subroutine prototype_and_cmd_args_to_nlist(prototype)
!!
!!      character(len=*) :: prototype
!!##DESCRIPTION
!!    create dictionary with character keywords, values, and value lengths
!!    using the routines for maintaining a list from command line arguments.
!!##OPTIONS
!!      prototype
!!##EXAMPLES
!!
!! Sample program
!!
!!      program demo_prototype_and_cmd_args_to_nlist
!!      use M_CLI2,  only : prototype_and_cmd_args_to_nlist, unnamed
!!      implicit none
!!      character(len=:),allocatable :: readme
!!      character(len=256)           :: message
!!      integer                      :: ios
!!      integer                      :: i
!!      doubleprecision              :: something
!!
!!      ! define arguments
!!      logical            :: l,h,v
!!      real               :: p(2)
!!      complex            :: c
!!      doubleprecision    :: x,y,z
!!
!!      ! uppercase keywords get an underscore to make it easier to remember
!!      logical            :: l_,h_,v_
!!      ! character variables must be long enough to hold returned value
!!      character(len=256) :: a_,b_
!!      integer            :: c_(3)
!!
!!         ! give command template with default values
!!         ! all values except logicals get a value.
!!         ! strings must be delimited with double quotes
!!         ! A string has to have at least one character as for -A
!!         ! lists of numbers should be comma-delimited.
!!         ! No spaces are allowed in lists of numbers
!!         call prototype_and_cmd_args_to_nlist('&
!!         & -l -v -h -LVH -x 0 -y 0.0 -z 0.0d0 -p 0,0 &
!!         & -A " " -B "Value B" -C 10,20,30 -c (-123,-456)',readme)
!!
!!         call get_args('x',x,'y',y,'z',z)
!!            something=sqrt(x**2+y**2+z**2)
!!            write (*,*)something,x,y,z
!!            if(size(unnamed) > 0)then
!!               write (*,'(a)')'files:'
!!               write (*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
!!            endif
!!      end program demo_prototype_and_cmd_args_to_nlist
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine prototype_and_cmd_args_to_nlist(prototype,string)

! ident_4="@(#) M_CLI2 prototype_and_cmd_args_to_nlist create dictionary from prototype if not null and update from command line"

character(len=*),intent(in)           :: prototype
character(len=*),intent(in),optional  :: string
integer                               :: ibig
integer                               :: itrim
integer                               :: iused

   if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:START'
   G_passed_in=prototype                            ! make global copy for printing
   ibig=longest_command_argument()                  ! bug in gfortran. len=0 should be fine
   ibig=max(ibig,1)
   IF(ALLOCATED(UNNAMED))DEALLOCATE(UNNAMED)
   ALLOCATE(CHARACTER(LEN=IBIG) :: UNNAMED(0))
   if(allocated(args))deallocate(args)
   allocate(character(len=ibig) :: args(0))

   G_remaining_option_allowed=.false.
   G_remaining_on=.false.
   G_remaining=''
   if(prototype /= '')then
      call prototype_to_dictionary(prototype,1)       ! build dictionary from prototype

      ! if short keywords not used by user allow them for standard options

      call locate_key('h',iused)
      if(iused <= 0)then
         call update('help')
         call update('help:h','F')
      endif

      call locate_key('v',iused)
      if(iused <= 0)then
         call update('version')
         call update('version:v','F')
      endif

      call locate_key('V',iused)
      if(iused <= 0)then
         call update('verbose')
         call update('verbose:V','F')
      endif

      call locate_key('u',iused)
      if(iused <= 0)then
         call update('usage')
         call update('usage:u','F')
      endif

      present_in=.false.                            ! reset all values to false so everything gets written
   endif

   if(present(string))then                          ! instead of command line arguments use another prototype string
      if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:CALL PROTOTYPE_TO_DICTIONARY:STRING=',STRING
      call prototype_to_dictionary(string,2)        ! build dictionary from prototype
   else
      if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:CALL CMD_ARGS_TO_DICTIONARY:CHECK=',.true.
      call cmd_args_to_dictionary()
   endif

   if( len(G_remaining) > 1)then                    ! if -- was in prototype then after -- on input return rest in this string
      itrim=len(G_remaining)
      if(G_remaining(itrim:itrim) == ' ')then       ! was adding a space at end as building it, but do not want to remove blanks
         G_remaining=G_remaining(:itrim-1)
      endif
      remaining=G_remaining
   endif
   if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_NLIST:NORMAL END'
end subroutine prototype_and_cmd_args_to_nlist
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine expand_response(name)
character(len=*),intent(in)  :: name
character(len=:),allocatable :: prototype
logical                      :: hold

   if(G_DEBUG)write(*,gen)'<DEBUG>EXPAND_RESPONSE:START:NAME=',name

   call get_prototype(name,prototype)

   if(prototype /= '')then
      hold=G_append
      G_append=.false.
      if(G_DEBUG)write(*,gen)'<DEBUG>EXPAND_RESPONSE:CALL PROTOTYPE_TO_DICTIONARY:PROTOTYPE=',prototype
      call prototype_to_dictionary(prototype,1)       ! build dictionary from prototype
      G_append=hold
   endif

   if(G_DEBUG)write(*,gen)'<DEBUG>EXPAND_RESPONSE:END'

end subroutine expand_response
!===================================================================================================================================
subroutine get_prototype(name,prototype) ! process @name abbreviations
character(len=*),intent(in) :: name
character(len=:),allocatable,intent(out) :: prototype
character(len=:),allocatable             :: filename
character(len=:),allocatable             :: os
character(len=:),allocatable             :: plain_name
character(len=:),allocatable             :: search_for
integer                                  :: lun
integer                                  :: ios
integer                                  :: itrim
character(len=4096)                      :: line !x! assuming input never this long
character(len=256)                       :: message
character(len=:),allocatable             :: array(:) ! output array of tokens
integer                                  :: lines_processed

   G_RESPONSE_PREFIX=get_env('CLI_RESPONSE_PREFIX','@')
   lines_processed=0
   plain_name=name//'  '
   plain_name=trim(name(len(G_RESPONSE_PREFIX)+1:))
   os= G_RESPONSE_PREFIX // get_env('OSTYPE',get_env('OS'))
   if(G_DEBUG)write(*,gen)'<DEBUG>GET_PROTOTYPE:OS=',OS

   search_for=''
   ! look for NAME.rsp and see if there is an @OS  section in it and position to it and read
   if(os /= G_RESPONSE_PREFIX)then
      search_for=os
      call find_and_read_response_file(plain_name)
      if(lines_processed /= 0)return
   endif

   ! look for NAME.rsp and see if there is anything before an OS-specific section
   search_for=''
   call find_and_read_response_file(plain_name)
   if(lines_processed /= 0)return

   ! look for ARG0.rsp  with @OS@NAME  section in it and position to it
   if(os /= G_RESPONSE_PREFIX)then
      search_for=os//name
      call find_and_read_response_file(basename(get_name(),keep_suffix=.false.))
      if(lines_processed /= 0)return
   endif

   ! look for ARG0.rsp  with a section called @NAME in it and position to it
   search_for=name
   call find_and_read_response_file(basename(get_name(),keep_suffix=.false.))
   if(lines_processed /= 0)return

   if(.not.CLI_AUTO_QUIET)then
      write(*,gen)'<ERROR> response name ['//trim(name)//'] not found'
      stop 1
   endif
contains
!===================================================================================================================================
subroutine find_and_read_response_file(rname)
! search for a simple file named the same as the @NAME field with one entry assumed in it
character(len=*),intent(in)  :: rname
character(len=:),allocatable :: paths(:)
character(len=:),allocatable :: testpath
character(len=256)           :: message
integer                      :: i
integer                      :: ios
   prototype=''
   ! look for NAME.rsp
   ! assume if have / or \ a full filename was supplied to support ifort(1)
   if((index(rname,slash) /= 0.or.index(rname,bslash) /= 0) .and. len(rname) > 1 )then
      filename=rname
      lun=fileopen(filename,message)
      if(lun /= -1)then
         call process_response()
         close(unit=lun,iostat=ios)
      endif
      return
   else
      filename=rname//'.rsp'
   endif
   if(G_DEBUG)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:FILENAME=',filename

   ! look for name.rsp in directories from environment variable assumed to be a colon-separated list of directories
   call split(get_env('CLI_RESPONSE_PATH',join_path(get_env('HOME'),'/.local/share/rsp')),paths)
   paths=[character(len=len(paths)) :: ' ',paths]
   if(G_DEBUG)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:PATHS=',paths

   do i=1,size(paths)
      testpath=join_path(paths(i),filename)
      lun=fileopen(testpath,message)
      if(lun /= -1)then
         if(G_DEBUG)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:SEARCH_FOR=',search_for
         if(search_for /= '') call position_response() ! set to end of file or where string was found
         call process_response()
         if(G_DEBUG)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:LINES_PROCESSED=',LINES_PROCESSED
         close(unit=lun,iostat=ios)
         if(G_DEBUG)write(*,gen)'<DEBUG>FIND_AND_READ_RESPONSE_FILE:CLOSE:LUN=',LUN,' IOSTAT=',IOS
         if(lines_processed /= 0)exit
      endif
   enddo

end subroutine find_and_read_response_file
!===================================================================================================================================
subroutine position_response()
integer :: ios
   line=''
   INFINITE: do
      read(unit=lun,fmt='(a)',iostat=ios,iomsg=message)line
      if(is_iostat_end(ios))then
         if(G_DEBUG)write(*,gen)'<DEBUG>POSITION_RESPONSE:EOF'
         backspace(lun,iostat=ios)
         exit INFINITE
      elseif(ios /= 0)then
         write(*,gen)'<ERROR>*position_response*:'//trim(message)
         exit INFINITE
      endif
      line=adjustl(line)
      if(line == search_for)return
   enddo INFINITE
end subroutine position_response
!===================================================================================================================================
subroutine process_response()
character(len=:),allocatable :: padded
character(len=:),allocatable :: temp
   G_RESPONSE_PREFIX=get_env('CLI_RESPONSE_PREFIX','@')
   line=''
   lines_processed=0
   INFINITE: do
      read(unit=lun,fmt='(a)',iostat=ios,iomsg=message)line
      if(is_iostat_end(ios))then
         backspace(lun,iostat=ios)
         exit INFINITE
      elseif(ios /= 0)then
         write(*,gen)'<ERROR>*process_response*:'//trim(message)
         exit INFINITE
      endif
      line=clipends(line)
      temp=line
      if(index(temp//' ','#') == 1)cycle
      if(temp /= '')then

         if(index(temp,G_RESPONSE_PREFIX) == 1.and.lines_processed /= 0)exit INFINITE

         call split(temp,array) ! get first word
         itrim=len_trim(array(1))+2
         temp=temp(itrim:)

         PROCESS: select case(lower(array(1)))
         case('comment','#','')
         case('system','!','$')
            if(G_options_only)exit PROCESS
            lines_processed= lines_processed+1
            call execute_command_line(temp)
         case('options','option','-')
            lines_processed= lines_processed+1
            prototype=prototype//' '//trim(temp)
         case('print','>','echo')
            if(G_options_only)exit PROCESS
            lines_processed= lines_processed+1
            write(*,'(a)')trim(temp)
         case('stop')
            if(G_options_only)exit PROCESS
            write(*,'(a)')trim(temp)
            stop
         case default
            if(array(1)(1:1) == '-')then
               ! assume these are simply options to support ifort(1)
               ! if starts with a single dash must assume a single argument
               ! and rest is value to support -Dname and -Ifile option
               ! which currently is not supported, so multiple short keywords
               ! does not work. Just a ifort(1) test at this point, so do not document
               if(G_options_only)exit PROCESS
               padded=trim(line)//'  '
               if(padded(2:2) == '-')then
                  prototype=prototype//' '//trim(line)
               else
                  prototype=prototype//' '//padded(1:2)//' '//trim(padded(3:))
               endif
               lines_processed= lines_processed+1
            else
               if(array(1)(1:len(G_RESPONSE_PREFIX)) == G_RESPONSE_PREFIX)cycle INFINITE !skip adjacent @ lines from first
               lines_processed= lines_processed+1
               write(*,'(*(g0))')'unknown response keyword [',array(1),'] with options of [',trim(temp),']'
            endif
         end select PROCESS

      endif
   enddo INFINITE
end subroutine process_response
!===================================================================================================================================
subroutine show_response_file(quiet)  ! copy response file to stdout
logical,intent(in),optional :: quiet
logical                     :: quiet_local
   if(present(quiet))then
      quiet_local=quiet
   else
      quiet_local=.false.
   endif
   G_RESPONSE_PREFIX=get_env('CLI_RESPONSE_PREFIX','@')
   line=''
   INFINITE: do
      read(unit=lun,fmt='(a)',iostat=ios,iomsg=message)line
      if(is_iostat_end(ios))then
         backspace(lun,iostat=ios)
         exit INFINITE
      elseif(ios /= 0)then
         if(quiet_local)then
            write(*,gen)'<ERROR>*show_response_file*:'//trim(message)
         endif
         exit INFINITE
      endif
      line=clipends(line)
      if(index(line//' ','#') == 1)cycle
      if(line == '' )cycle
      if(index(line,G_RESPONSE_PREFIX) == 1)then
         PROCESS: select case(lower(array(1)))
         case('comment','#','')
         case('system','!','$')
         case('options','option','-')
         case('print','>','echo')
         case('stop')
         case default
         end select PROCESS
         write(*,*)trim(line)
      endif
   enddo INFINITE
end subroutine show_response_file
!===================================================================================================================================
end subroutine get_prototype
!===================================================================================================================================
function fileopen(filename,message) result(lun)
character(len=*),intent(in)              :: filename
character(len=*),intent(out),optional    :: message
integer                                  :: lun
integer                                  :: ios
character(len=256)                       :: message_local

   ios=0
   message_local=''
   open(file=filename,newunit=lun,&
    & form='formatted',access='sequential',action='read',&
    & position='rewind',status='old',iostat=ios,iomsg=message_local)

   if(ios /= 0)then
      lun=-1
      if(present(message))then
         message=trim(message_local)
      else
         write(*,gen)trim(message_local)
      endif
   endif
   if(G_DEBUG)write(*,gen)'<DEBUG>FILEOPEN:FILENAME=',filename,' LUN=',lun,' IOS=',IOS,' MESSAGE=',trim(message_local)

end function fileopen
!===================================================================================================================================
function get_env(NAME,DEFAULT) result(VALUE)
character(len=*),intent(in)          :: NAME
character(len=*),intent(in),optional :: DEFAULT
character(len=:),allocatable         :: VALUE
integer                              :: howbig
integer                              :: stat
integer                              :: length
   ! get length required to hold value
   length=0
   if(NAME /= '')then
      call get_environment_variable(NAME, length=howbig,status=stat,trim_name=.true.)
      select case (stat)
      case (1)
          !x!print *, NAME, " is not defined in the environment. Strange..."
          VALUE=''
      case (2)
          !x!print *, "This processor doesn't support environment variables. Boooh!"
          VALUE=''
      case default
          ! make string to hold value of sufficient size
          if(allocated(value))deallocate(value)
          allocate(character(len=max(howbig,1)) :: VALUE)
          ! get value
         call get_environment_variable(NAME,VALUE,status=stat,trim_name=.true.)
          if(stat /= 0)VALUE=''
      end select
   else
      VALUE=''
   endif
   if(VALUE == ''.and.present(DEFAULT))VALUE=DEFAULT
end function get_env
!===================================================================================================================================
function join_path(a1,a2,a3,a4,a5) result(path)
   ! Construct path by joining strings with os file separator
   !
   character(len=*), intent(in)           :: a1, a2
   character(len=*), intent(in), optional :: a3, a4, a5
   character(len=:), allocatable          :: path
   character(len=1)                       :: filesep

   filesep = separator()
   if(a1 /= '')then
      path = trim(a1) // filesep // trim(a2)
   else
      path = trim(a2)
   endif
   if (present(a3)) path = path // filesep // trim(a3)
   if (present(a4)) path = path // filesep // trim(a4)
   if (present(a5)) path = path // filesep // trim(a5)
   path=adjustl(path//'   ')
   ! clean up duplicate adjacent separators
   path=path(1:2)//replace_str(path(3:),filesep//filesep,filesep) ! some systems allow filepath starting with // or \\
   path=trim(path)
end function join_path
!===================================================================================================================================
function get_name() result(name)
! get the pathname of arg0
character(len=:),allocatable :: arg0
integer                      :: arg0_length
integer                      :: istat
character(len=4096)          :: long_name
character(len=:),allocatable :: name
   arg0_length=0
   name=''
   long_name=''
   call get_command_argument(0,length=arg0_length,status=istat)
   if(istat == 0)then
      if(allocated(arg0))deallocate(arg0)
      allocate(character(len=arg0_length) :: arg0)
      call get_command_argument(0,arg0,status=istat)
      if(istat == 0)then
         inquire(file=arg0,iostat=istat,name=long_name)
         name=trim(long_name)
      else
         name=arg0
      endif
   endif
end function get_name
!===================================================================================================================================
function basename(path,keep_suffix) result (base)
    ! Extract filename from path with/without keep_suffix
    !
character(*), intent(In) :: path
logical, intent(in), optional :: keep_suffix
character(:), allocatable :: base

character(:), allocatable :: file_parts(:)
logical :: return_with_suffix
integer :: iend

   if (.not.present(keep_suffix)) then
      return_with_suffix = .true.
   else
      return_with_suffix = keep_suffix
   endif

   call split(path,file_parts,delimiters=bslash//slash)
   if(size(file_parts) > 0)then
      base = trim(file_parts(size(file_parts)))
   else
      base = ''
   endif
   if(.not.return_with_suffix)then
      iend=index(base,'.',back=.true.)
      if(iend.gt.1)then
         base=base(:iend-1)
      endif
   endif
end function basename
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!! !>
!!##NAME
!!     separator(3) - [M_io:ENVIRONMENT] try to determine pathname directory
!!     separator character
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!     function separator() result(sep)
!!
!!      character(len=1) :: sep
!!
!!##DESCRIPTION
!!    First testing for the existence of "/.",  then if that fails a list
!!    of variable names assumed to contain directory paths {PATH|HOME} are
!!    examined first for a backslash, then a slash. Assuming basically the
!!    choice is a ULS or MSWindows system, and users can do weird things like
!!    put a backslash in a ULS path and break it.
!!
!!    Therefore can be very system dependent. If the queries fail the
!!    default returned is "/".
!!
!!##EXAMPLES
!!
!!
!!    sample usage
!!
!!     program demo_separator
!!     use M_io, only : separator
!!     implicit none
!!        write(*,*)'separator=',separator()
!!     end program demo_separator
!===================================================================================================================================
function separator() result(sep)
! use the pathname returned as arg0 to determine pathname separator
integer                      :: ios
integer                      :: i
logical,save                 :: existing=.false.
character(len=1)             :: sep
!x!IFORT BUG:character(len=1),save        :: sep_cache=' '
integer,save                 :: isep=-1
character(len=4096)          :: name
character(len=:),allocatable :: envnames(:)

    ! NOTE:  A parallel code might theoretically use multiple OS
    !x!FORT BUG:if(sep_cache /= ' ')then  ! use cached value.
    !x!FORT BUG:    sep=sep_cache
    !x!FORT BUG:    return
    !x!FORT BUG:endif
    if(isep /= -1)then  ! use cached value.
        sep=char(isep)
        return
    endif
    FOUND: block
    ! simple, but does not work with ifort
    ! most MSWindows environments see to work with backslash even when
    ! using POSIX filenames so do not rely on '\.'.
    inquire(file='/.',exist=existing,iostat=ios,name=name)
    if(existing.and.ios == 0)then
        sep=slash
        exit FOUND
    endif
    ! check variables names common to many platforms that usually have a
    ! directory path in them although a ULS file can contain a backslash
    ! and vice-versa (eg. "touch A\\B\\C"). Removed HOMEPATH because it
    ! returned a name with backslash on CygWin, Mingw, WLS even when using
    ! POSIX filenames in the environment.
    envnames=[character(len=10) :: 'PATH', 'HOME']
    do i=1,size(envnames)
       if(index(get_env(envnames(i)),bslash) /= 0)then
          sep=bslash
          exit FOUND
       elseif(index(get_env(envnames(i)),slash) /= 0)then
          sep=slash
          exit FOUND
       endif
    enddo

    write(*,*)'<WARNING>unknown system directory path separator'
    sep=bslash
    endblock FOUND
    !x!IFORT BUG:sep_cache=sep
    isep=ichar(sep)
end function separator
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmd_args_to_dictionary()
! convert command line arguments to dictionary entries
!x!logical                      :: guess_if_value
integer                      :: pointer
character(len=:),allocatable :: lastkeyword
integer                      :: i, jj, kk
integer                      :: ilength, istatus, imax
character(len=1)             :: letter
character(len=:),allocatable :: current_argument
character(len=:),allocatable :: current_argument_padded
character(len=:),allocatable :: dummy
character(len=:),allocatable :: oldvalue
logical                      :: nomore
logical                      :: next_mandatory
   if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:START'
   G_RESPONSE_PREFIX=get_env('CLI_RESPONSE_PREFIX','@')
   next_mandatory=.false.
   nomore=.false.
   pointer=0
   lastkeyword=' '
   G_keyword_single_letter=.true.
   if(CLI_AUTO_RESPONSE_FILE)then
      i=0  ! cause get_next_argument to return a response macro name
   else
      i=1
   endif
   current_argument=''
   GET_ARGS: do while (get_next_argument()) ! insert and replace entries
      if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:WHILE:CURRENT_ARGUMENT=',current_argument

      if( current_argument  ==  '-' .and. nomore .eqv. .true. )then   ! sort of
      elseif( current_argument  ==  '-')then                          ! sort of
         current_argument='"stdin"'
      endif
      if( current_argument  ==  '--' .and. nomore .eqv. .true. )then  ! -- was already encountered
      elseif( current_argument  ==  '--' )then                        ! everything after this goes into the unnamed array
         nomore=.true.
         pointer=0
         if(G_remaining_option_allowed)then
            G_remaining_on=.true.
         endif
         cycle GET_ARGS
      endif

      dummy=current_argument//'   '
      current_argument_padded=current_argument//'   '

      if(.not.next_mandatory.and..not.nomore.and.current_argument_padded(1:2) == '--')then    ! beginning of long word
         if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:START_LONG:'
         G_keyword_single_letter=.false.
         if(lastkeyword /= '')then
            call ifnull()
         endif
         call locate_key(current_argument_padded(3:),pointer)
         if(pointer <= 0)then
            if(G_QUIET)then
               lastkeyword="UNKNOWN"
               pointer=0
               cycle GET_ARGS
            endif
            call print_dictionary('UNKNOWN LONG KEYWORD: '//current_argument)
            call mystop(1)
            return
         endif
         lastkeyword=trim(current_argument_padded(3:))
         next_mandatory=mandatory(pointer)
      elseif(.not.next_mandatory &
      & .and..not.nomore &
      & .and.current_argument_padded(1:1) == '-' &
      & .and.index("0123456789.",dummy(2:2)) == 0)then
      ! short word
         if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:START_SHORT'
         G_keyword_single_letter=.true.
         if(lastkeyword /= '')then
            call ifnull()
         endif
         call locate_key(current_argument_padded(2:),pointer)
         jj=len(current_argument)
         if( (pointer <= 0.or.jj.ge.3).and.(G_STRICT) )then  ! name not found
            if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:SHORT NOT FOUND:',current_argument_padded(2:)
            ! in strict mode this might be multiple single-character values
            do kk=2,jj
               letter=current_argument_padded(kk:kk)
               call locate_key(letter,pointer)
               if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:LETTER:',letter,pointer
               if(pointer > 0)then
                  call update(keywords(pointer),'T')
               else
                  if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:UNKNOWN SHORT:',letter
                  call print_dictionary('UNKNOWN SHORT KEYWORD:'//letter) ! //' in '//current_argument)
                  if(G_QUIET)then
                     lastkeyword="UNKNOWN"
                     pointer=0
                     cycle GET_ARGS
                  endif
                  call mystop(2)
                  return
               endif
               current_argument='-'//current_argument_padded(jj:jj)
            enddo
            !--------------
            lastkeyword=""
            pointer=0
            if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:SHORT_END:2:'
            cycle GET_ARGS
            !--------------
         elseif(pointer<0)then
            if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:UNKNOWN SHORT_CONFIRMED:',letter
            call print_dictionary('UNKNOWN SHORT KEYWORD:'//current_argument_padded(2:))
            if(G_QUIET)then
               lastkeyword="UNKNOWN"
               pointer=0
               cycle GET_ARGS
            endif
            call mystop(2)
            return
         endif
         if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:SHORT_END:1:'
         lastkeyword=trim(current_argument_padded(2:))
         next_mandatory=mandatory(pointer)
      elseif(pointer == 0)then                                       ! unnamed arguments
         if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:UNNAMED ARGUMENT:',current_argument
         if(G_remaining_on)then
            if(len(current_argument) < 1)then
               G_remaining=G_remaining//'"" '
            elseif(current_argument(1:1) == '-')then
               !get fancier to handle spaces and =!G_remaining=G_remaining//current_argument//' '
               G_remaining=G_remaining//'"'//current_argument//'" '
            else
               G_remaining=G_remaining//'"'//current_argument//'" '
            endif
            imax=max(len(args),len(current_argument))
            args=[character(len=imax) :: args,current_argument]
         else
            imax=max(len(unnamed),len(current_argument))
            if(scan(current_argument//' ',G_RESPONSE_PREFIX) == 1.and.G_response)then
               if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:1:CALL EXPAND_RESPONSE:CURRENT_ARGUMENT=',current_argument
               call expand_response(current_argument)
            else
               unnamed=[character(len=imax) :: unnamed,current_argument]
            endif
         endif
      else
         if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:FOUND:',current_argument
         oldvalue=get(keywords(pointer))//' '
         if(oldvalue(1:1) == '"')then
            current_argument=quote(current_argument(:ilength))
         endif
         if(upper(oldvalue) == 'F'.or.upper(oldvalue) == 'T')then  ! assume boolean parameter
            if(current_argument /= ' ')then
               if(G_remaining_on)then
                  if(len(current_argument) < 1)then
                        G_remaining=G_remaining//'"" '
                  elseif(current_argument(1:1) == '-')then
                       !get fancier to handle spaces and =!G_remaining=G_remaining//current_argument//' '
                        G_remaining=G_remaining//'"'//current_argument//'" '
                  else
                        G_remaining=G_remaining//'"'//current_argument//'" '
                  endif
                  imax=max(len(args),len(current_argument))
                  args=[character(len=imax) :: args,current_argument]
               else
                  imax=max(len(unnamed),len(current_argument))
                  if(scan(current_argument//' ',G_RESPONSE_PREFIX) == 1.and.G_response)then
                    if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:2:CALL EXPAND_RESPONSE:CURRENT_ARGUMENT=',current_argument
                    call expand_response(current_argument)
                  else
                    unnamed=[character(len=imax) :: unnamed,current_argument]
                  endif
               endif
            endif
            current_argument='T'
         endif
         call update(keywords(pointer),current_argument)
         pointer=0
         lastkeyword=''
         next_mandatory=.false.
      endif
      if(CLI_AUTO_QUIET)CLI_AUTO_QUIET=.false.
   enddo GET_ARGS
   if(lastkeyword /= '')then
      call ifnull()
   endif
   if(G_DEBUG)write(*,gen)'<DEBUG>CMD_ARGS_TO_DICTIONARY:NORMAL END'

contains

subroutine ifnull()

   oldvalue=clipends(get(lastkeyword))//' '

   if(upper(oldvalue(1:1)) == 'F'.or.upper(oldvalue(1:1)) == 'T')then
      call update(lastkeyword,'T')
      !call update(lastkeyword,upper(oldvalue(1:1)))
   elseif(oldvalue(1:1) == '"')then
      call update(lastkeyword,'" "')
   else
      call update(lastkeyword,' ')
   endif

end subroutine ifnull

function get_next_argument()
!
! get next argument from command line into allocated variable current_argument
!
logical,save :: hadequal=.false.
character(len=:),allocatable,save :: right_hand_side
logical :: get_next_argument
integer :: iright
integer :: iequal

   if(hadequal)then  ! use left-over value from previous -NAME=VALUE syntax
      current_argument=right_hand_side
      right_hand_side=''
      hadequal=.false.
      get_next_argument=.true.
      ilength=len(current_argument)
      return
   endif

   if(i == 0)then ! auto_response_file is true and first call to here
      get_next_argument=.true.
      current_argument=G_RESPONSE_PREFIX//basename(get_name(),keep_suffix=.false.)
      i=i+1
      return
   endif

   if(i>command_argument_count())then
      get_next_argument=.false.
      return
   else
      get_next_argument=.true.
   endif

   call get_command_argument(number=i,length=ilength,status=istatus)                              ! get next argument
   if(istatus /= 0) then                                                                          ! on error
      write(warn,*)'*prototype_and_cmd_args_to_nlist* error obtaining argument ',i,&
         &'status=',istatus,&
         &'length=',ilength
      get_next_argument=.false.
   else
      ilength=max(ilength,1)
      if(allocated(current_argument))deallocate(current_argument)
      allocate(character(len=ilength) :: current_argument)
      call get_command_argument(number=i,value=current_argument,length=ilength,status=istatus)    ! get next argument
      if(istatus /= 0) then                                                                       ! on error
         write(warn,*)'*prototype_and_cmd_args_to_nlist* error obtaining argument ',i,&
            &'status=',istatus,&
            &'length=',ilength,&
            &'target length=',len(current_argument)
         get_next_argument=.false.
       endif

       ! if an argument keyword and an equal before a space split on equal and save right hand side for next call
       if(nomore)then
       elseif( len(current_argument) == 0)then
       else
          iright=index(current_argument,' ')
          if(iright == 0)iright=len(current_argument)
          iequal=index(current_argument(:iright),'=')
          if(next_mandatory)then
          elseif(iequal /= 0.and.current_argument(1:1) == '-')then
             if(iequal /= len(current_argument))then
                right_hand_side=current_argument(iequal+1:)
             else
                right_hand_side=''
             endif
             hadequal=.true.
             current_argument=current_argument(:iequal-1)
          endif
       endif
   endif
   i=i+1
end function get_next_argument

end subroutine cmd_args_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    print_dictionary(3) - [ARGUMENTS:M_CLI2] print internal dictionary
!!    created by calls to set_args(3)
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     subroutine print_dictionary(header,stop)
!!
!!      character(len=*),intent(in),optional :: header
!!      logical,intent(in),optional          :: stop
!!##DESCRIPTION
!!    Print the internal dictionary created by calls to set_args(3).
!!    This routine is intended to print the state of the argument list
!!    if an error occurs in using the set_args(3) procedure.
!!##OPTIONS
!!     HEADER  label to print before printing the state of the command
!!             argument list.
!!     STOP    logical value that if true stops the program after displaying
!!             the dictionary.
!!##EXAMPLES
!!
!!
!!
!! Typical usage:
!!
!!       program demo_print_dictionary
!!       use M_CLI2,  only : set_args, get_args
!!       implicit none
!!       real :: x, y, z
!!          call set_args('-x 10 -y 20 -z 30')
!!          call get_args('x',x,'y',y,'z',z)
!!          ! all done cracking the command line; use the values in your program.
!!          write(*,*)x,y,z
!!       end program demo_print_dictionary
!!
!!      Sample output
!!
!!      Calling the sample program with an unknown parameter or the --usage
!!      switch produces the following:
!!
!!         $ ./demo_print_dictionary -A
!!         UNKNOWN SHORT KEYWORD: -A
!!         KEYWORD             PRESENT  VALUE
!!         z                   F        [3]
!!         y                   F        [2]
!!         x                   F        [1]
!!         help                F        [F]
!!         version             F        [F]
!!         usage               F        [F]
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine print_dictionary(header,stop)
character(len=*),intent(in),optional :: header
logical,intent(in),optional          :: stop
integer          :: i
   if(G_QUIET)return
   if(present(header))then
      if(header /= '')then
         write(warn,'(a)')header
      endif
   endif
   if(allocated(keywords))then
      if(size(keywords) > 0)then
         write(warn,'(a,1x,a,1x,a,1x,a)')atleast('KEYWORD',max(len(keywords),8)),'SHORT','PRESENT','VALUE'
         write(warn,'(*(a,1x,a5,1x,l1,8x,"[",a,"]",/))') &
         & (atleast(keywords(i),max(len(keywords),8)),shorts(i),present_in(i),values(i)(:counts(i)),i=size(keywords),1,-1)
      endif
   endif
   if(allocated(unnamed))then
      if(size(unnamed) > 0)then
         write(warn,'(a)')'UNNAMED'
         write(warn,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
      endif
   endif
   if(allocated(args))then
      if(size(args) > 0)then
         write(warn,'(a)')'ARGS'
         write(warn,'(i6.6,3a)')(i,'[',args(i),']',i=1,size(args))
      endif
   endif
   if(G_remaining /= '')then
      write(warn,'(a)')'REMAINING'
      write(warn,'(a)')G_remaining
   endif
   if(present(stop))then
      if(stop) call mystop(5)
   endif
end subroutine print_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    get_args(3) - [ARGUMENTS:M_CLI2] return keyword values when parsing
!!    command line arguments
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   get_args(3) and its convenience functions:
!!
!!     use M_CLI2, only : get_args
!!     ! convenience functions
!!     use M_CLI2, only : dget, iget, lget, rget, sget, cget
!!     use M_CLI2, only : dgets, igets, lgets, rgets, sgets, cgets
!!
!!     subroutine get_args(name,value,delimiters)
!!
!!      character(len=*),intent(in) :: name
!!
!!      type(${TYPE}),allocatable,intent(out) :: value(:)
!!      ! or
!!      type(${TYPE}),allocatable,intent(out) :: value
!!
!!      character(len=*),intent(in),optional :: delimiters
!!
!!      where ${TYPE} may be from the set
!!              {real,doubleprecision,integer,logical,complex,character(len=:)}
!!##DESCRIPTION
!!
!!    GET_ARGS(3) returns the value of keywords after SET_ARGS(3) has
!!    been called to parse the command line. For fixed-length CHARACTER
!!    variables see GET_ARGS_FIXED_LENGTH(3). For fixed-size arrays see
!!    GET_ARGS_FIXED_SIZE(3).
!!
!!    As a convenience multiple pairs of keywords and variables may be
!!    specified if and only if all the values are scalars and the CHARACTER
!!    variables are fixed-length or pre-allocated.
!!
!!##OPTIONS
!!
!!     NAME        name of commandline argument to obtain the value of
!!     VALUE       variable to hold returned value. The kind of the value
!!                 is used to determine the type of returned value. May
!!                 be a scalar or allocatable array. If type is CHARACTER
!!                 the scalar must have an allocatable length.
!!     DELIMITERS  By default the delimiter for array values are comma,
!!                 colon, and whitespace. A string containing an alternate
!!                 list of delimiter characters may be supplied.
!!
!!##CONVENIENCE FUNCTIONS
!!    There are convenience functions that are replacements for calls to
!!    get_args(3) for each supported default intrinsic type
!!
!!      o scalars -- dget(3), iget(3), lget(3), rget(3), sget(3),
!!                   cget(3)
!!      o vectors -- dgets(3), igets(3), lgets(3), rgets(3),
!!                   sgets(3), cgets(3)
!!
!!    D is for DOUBLEPRECISION, I for INTEGER, L for LOGICAL, R for REAL,
!!    S for string (CHARACTER), and C for COMPLEX.
!!
!!    If the functions are called with no argument they will return the
!!    UNNAMED array converted to the specified type; which is all the
!!    values on the command line not associated with a keyword that
!!    should come at the end of the command line.
!!
!!##EXAMPLES
!!
!!
!! Sample program:
!!
!!     program demo_get_args
!!     use M_CLI2,  only : filenames=>unnamed, set_args, get_args
!!     use M_CLI2,  only : dget, iget, lget, rget, sget, cget
!!     use M_CLI2,  only : dgets, igets, lgets, rgets, sgets, cgets
!!     implicit none
!!     integer                      :: i
!!      ! Define ARGS
!!     real                         :: x, y, z
!!     real,allocatable             :: p(:)
!!     character(len=:),allocatable :: title
!!     logical                      :: l, lbig
!!      ! Define and parse (to set initial values) command line
!!      !   o only quote strings and use double-quotes
!!      !   o set all logical values to F or T.
!!     call set_args('         &
!!        & -x 1 -y 2 -z 3     &
!!        & -p -1,-2,-3        &
!!        & --title "my title" &
!!        & -l F -L F          &
!!        & --label " "        &
!!        & ')
!!      ! Assign values to elements
!!      ! Scalars
!!     call get_args( 'x',x, 'y',y, 'z',z, 'l',l, 'L',lbig )
!!      ! Allocatable string
!!     call get_args('title',title)
!!      ! Allocatable arrays
!!     call get_args('p',p)
!!      ! Use values
!!     write(*,'(1x,g0,"=",g0)')'x',x, 'y',y, 'z',z
!!     write(*,*)'p=',p
!!     write(*,*)'title=',title
!!     write(*,*)'l=',l
!!     write(*,*)'L=',lbig
!!     if(size(filenames) > 0)then
!!        write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
!!     endif
!!     ! or the equivalent using functions instead of get_args(3)
!!     write(*,'(1x,g0,"=",g0)')'x',rget('x'), 'y',rget('y'), 'z',rget('z')
!!     write(*,*)'p=',rgets('p')
!!     write(*,*)'title=',sget('title')
!!     write(*,*)'l=',lget('l')
!!     write(*,*)'L=',lget('L')
!!     end program demo_get_args
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
!>
!!##NAME
!!    get_args_fixed_length(3) - [ARGUMENTS:M_CLI2] return keyword values
!!    for fixed-length string when parsing command line
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine get_args_fixed_length(name,value)
!!
!!     character(len=*),intent(in)  :: name
!!     character(len=:),allocatable :: value
!!     character(len=*),intent(in),optional :: delimiters
!!
!!##DESCRIPTION
!!
!!    get_args_fixed_length(3) returns the value of a string
!!    keyword when the string value is a fixed-length CHARACTER
!!    variable.
!!
!!##OPTIONS
!!
!!    NAME   name of commandline argument to obtain the value of
!!
!!    VALUE  variable to hold returned value.
!!           Must be a fixed-length CHARACTER variable.
!!
!!    DELIMITERS  By default the delimiter for array values are comma,
!!                colon, and whitespace. A string containing an alternate
!!                list of delimiter characters may be supplied.
!!
!!##EXAMPLES
!!
!! Sample program:
!!
!!     program demo_get_args_fixed_length
!!     use M_CLI2,  only : set_args, get_args_fixed_length
!!     implicit none
!!
!!      ! Define args
!!     character(len=80)   :: title
!!      ! Parse command line
!!     call set_args(' --title "my title" ')
!!      ! Assign values to variables
!!     call get_args_fixed_length('title',title)
!!      ! Use values
!!     write(*,*)'title=',title
!!
!!     end program demo_get_args_fixed_length
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
!>
!!##NAME
!!    get_args_fixed_size(3) - [ARGUMENTS:M_CLI2] return keyword values
!!    for fixed-size array when parsing command line arguments
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine get_args_fixed_size(name,value)
!!
!!     character(len=*),intent(in) :: name
!!     [real|doubleprecision|integer|logical|complex] :: value(NNN)
!!        or
!!     character(len=MMM) :: value(NNN)
!!
!!     character(len=*),intent(in),optional :: delimiters
!!
!!##DESCRIPTION
!!
!!    get_args_fixed_size(3) returns the value of keywords for fixed-size
!!    arrays after set_args(3) has been called. On input on the command
!!    line all values of the array must be specified.
!!
!!##OPTIONS
!!    NAME        name of commandline argument to obtain the value of
!!
!!    VALUE       variable to hold returned values. The kind of the value
!!                is used to determine the type of returned value. Must be
!!                a fixed-size array. If type is CHARACTER the length must
!!                also be fixed.
!!
!!    DELIMITERS  By default the delimiter for array values are comma,
!!                colon, and whitespace. A string containing an alternate
!!                list of delimiter characters may be supplied.
!!
!!##EXAMPLES
!!
!! Sample program:
!!
!!     program demo_get_args_fixed_size
!!     use M_CLI2,  only : set_args, get_args_fixed_size
!!     implicit none
!!     integer,parameter   :: dp=kind(0.0d0)
!!     ! DEFINE ARGS
!!     real                :: x(2)
!!     real(kind=dp)       :: y(2)
!!     integer             :: p(3)
!!     character(len=80)   :: title(1)
!!     logical             :: l(4), lbig(4)
!!     complex             :: cmp(2)
!!     ! DEFINE AND PARSE (TO SET INITIAL VALUES) COMMAND LINE
!!     !   o only quote strings
!!     !   o set all logical values to F or T.
!!     call set_args(' &
!!        & -x 10.0,20.0 &
!!        & -y 11.0,22.0 &
!!        & -p -1,-2,-3 &
!!        & --title "my title" &
!!        & -l F,T,F,T -L T,F,T,F  &
!!        & --cmp 111,222.0,333.0e0,4444 &
!!        & ')
!!     ! ASSIGN VALUES TO ELEMENTS
!!        call get_args_fixed_size('x',x)
!!        call get_args_fixed_size('y',y)
!!        call get_args_fixed_size('p',p)
!!        call get_args_fixed_size('title',title)
!!        call get_args_fixed_size('l',l)
!!        call get_args_fixed_size('L',lbig)
!!        call get_args_fixed_size('cmp',cmp)
!!     ! USE VALUES
!!        write(*,*)'x=',x
!!        write(*,*)'p=',p
!!        write(*,*)'title=',title
!!        write(*,*)'l=',l
!!        write(*,*)'L=',lbig
!!        write(*,*)'cmp=',cmp
!!     end program demo_get_args_fixed_size
!!   Results:
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
subroutine get_fixedarray_class(keyword,generic,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
class(*)                             :: generic(:)
character(len=*),intent(in),optional :: delimiters
   select type(generic)
    type is (character(len=*));  call get_fixedarray_fixed_length_c(keyword,generic,delimiters)
    type is (integer);           call get_fixedarray_i(keyword,generic,delimiters)
    type is (real);              call get_fixedarray_r(keyword,generic,delimiters)
    type is (complex);           call get_fixed_size_complex(keyword,generic,delimiters)
    type is (real(kind=dp));     call get_fixedarray_d(keyword,generic,delimiters)
    type is (logical);           call get_fixedarray_l(keyword,generic,delimiters)
    class default
      call mystop(-7,'*get_fixedarray_class* crud -- procedure does not know about this type')
   end select
end subroutine get_fixedarray_class
!===================================================================================================================================
! return allocatable arrays
!===================================================================================================================================
subroutine get_anyarray_l(keyword,larray,delimiters)

! ident_5="@(#) M_CLI2 get_anyarray_l(3) given keyword fetch logical array from string in dictionary(F on err)"

character(len=*),intent(in)  :: keyword                    ! the dictionary keyword (in form VERB_KEYWORD) to retrieve
logical,allocatable          :: larray(:)                  ! convert value to an array
character(len=*),intent(in),optional   :: delimiters
character(len=:),allocatable :: carray(:)                  ! convert value to an array
character(len=:),allocatable :: val
integer                      :: i
integer                      :: place
integer                      :: iichar                     ! point to first character of word unless first character is "."
   call locate_key(keyword,place)                          ! find where string is or should be
   if(place > 0)then                                      ! if string was found
      val=values(place)(:counts(place))
      call split(adjustl(upper(val)),carray,delimiters=delimiters)  ! convert value to uppercase, trimmed; then parse into array
   else
      call journal('*get_anyarray_l* unknown keyword',keyword)
      call mystop(8 ,'*get_anyarray_l* unknown keyword '//keyword)
      if(allocated(larray))deallocate(larray)
      allocate(larray(0))
      return
   endif
   if(size(carray) > 0)then                                  ! if not a null string
      if(allocated(larray))deallocate(larray)
      allocate(larray(size(carray)))                          ! allocate output array
      do i=1,size(carray)
         larray(i)=.false.                                    ! initialize return value to .false.
         if(carray(i)(1:1) == '.')then                        ! looking for fortran logical syntax .STRING.
            iichar=2
         else
            iichar=1
         endif
         select case(carray(i)(iichar:iichar))             ! check word to see if true or false
         case('T','Y',' '); larray(i)=.true.               ! anything starting with "T" or "Y" or a blank is TRUE (true,yes,...)
         case('F','N');     larray(i)=.false.              ! assume this is false or no
         case default
            call journal("*get_anyarray_l* bad logical expression for ",(keyword),'=',carray(i))
         end select
      enddo
   else                                                       ! for a blank string return one T
      if(allocated(larray))deallocate(larray)
      allocate(larray(1))                                     ! allocate output array
      larray(1)=.true.
   endif
end subroutine get_anyarray_l
!===================================================================================================================================
subroutine get_anyarray_d(keyword,darray,delimiters)

! ident_6="@(#) M_CLI2 get_anyarray_d(3) given keyword fetch dble value array from Language Dictionary (0 on err)"

character(len=*),intent(in)           :: keyword      ! keyword to retrieve value from dictionary
real(kind=dp),allocatable,intent(out) :: darray(:)    ! function type
character(len=*),intent(in),optional  :: delimiters

character(len=:),allocatable          :: carray(:)    ! convert value to an array using split(3)
integer                               :: i
integer                               :: place
integer                               :: ierr
character(len=:),allocatable          :: val
!-----------------------------------------------------------------------------------------------------------------------------------
   call locate_key(keyword,place)                    ! find where string is or should be
   if(place > 0)then                                 ! if string was found
      val=values(place)(:counts(place))
      val=replace_str(val,'(','')
      val=replace_str(val,')','')
      call split(val,carray,delimiters=delimiters)    ! find value associated with keyword and split it into an array
   else
      call journal('*get_anyarray_d* unknown keyword '//keyword)
      call mystop(9 ,'*get_anyarray_d* unknown keyword '//keyword)
      if(allocated(darray))deallocate(darray)
      allocate(darray(0))
      return
   endif
   if(allocated(darray))deallocate(darray)
   allocate(darray(size(carray)))                     ! create the output array
   do i=1,size(carray)
      call a2d(carray(i), darray(i),ierr) ! convert the string to a numeric value
      if(ierr /= 0)then
         call mystop(10 ,'*get_anyarray_d* unreadable value '//carray(i)//' for keyword '//keyword)
      endif
   enddo
end subroutine get_anyarray_d
!===================================================================================================================================
subroutine get_anyarray_i(keyword,iarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
integer,allocatable                  :: iarray(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
   call get_anyarray_d(keyword,darray,delimiters)
   iarray=nint(darray)
end subroutine get_anyarray_i
!===================================================================================================================================
subroutine get_anyarray_r(keyword,rarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
real,allocatable                     :: rarray(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
   call get_anyarray_d(keyword,darray,delimiters)
   rarray=real(darray)
end subroutine get_anyarray_r
!===================================================================================================================================
subroutine get_anyarray_x(keyword,xarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
complex(kind=sp),allocatable         :: xarray(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
integer                              :: half,sz,i
   call get_anyarray_d(keyword,darray,delimiters)
   sz=size(darray)
   half=sz/2
   if(sz /= half+half)then
      call journal('*get_anyarray_x* uneven number of values defining complex value '//keyword)
      call mystop(11,'*get_anyarray_x* uneven number of values defining complex value '//keyword)
      if(allocated(xarray))deallocate(xarray)
      allocate(xarray(0))
   endif

   !x!================================================================================================
   !x!IFORT,GFORTRAN OK, NVIDIA RETURNS NULL ARRAY: xarray=cmplx(real(darray(1::2)),real(darray(2::2)))
   if(allocated(xarray))deallocate(xarray)
   allocate(xarray(half))
   do i=1,sz,2
      xarray((i+1)/2)=cmplx( darray(i),darray(i+1),kind=sp )
   enddo
   !x!================================================================================================

end subroutine get_anyarray_x
!===================================================================================================================================
subroutine get_anyarray_c(keyword,strings,delimiters)

! ident_7="@(#) M_CLI2 get_anyarray_c(3) Fetch strings value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)          :: keyword       ! name to look up in dictionary
character(len=:),allocatable         :: strings(:)
character(len=*),intent(in),optional :: delimiters
integer                              :: place
character(len=:),allocatable         :: val
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                  ! if index is valid return strings
      val=unquote(values(place)(:counts(place)))
      call split(val,strings,delimiters=delimiters)   ! find value associated with keyword and split it into an array
   else
      call journal('*get_anyarray_c* unknown keyword '//keyword)
      call mystop(12,'*get_anyarray_c* unknown keyword '//keyword)
      if(allocated(strings))deallocate(strings)
      allocate(character(len=0)::strings(0))
   endif
end subroutine get_anyarray_c
!===================================================================================================================================
subroutine get_args_fixed_length_a_array(keyword,strings,delimiters)

! ident_8="@(#) M_CLI2 get_args_fixed_length_a_array(3) Fetch strings value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)          :: keyword       ! name to look up in dictionary
character(len=*),allocatable         :: strings(:)
character(len=*),intent(in),optional :: delimiters
character(len=:),allocatable         :: strings_a(:)
integer                              :: place
character(len=:),allocatable         :: val
integer                              :: ibug
   call locate_key(keyword,place)                     ! find where string is or should be
   if(place > 0)then                                  ! if index is valid return strings
      val=unquote(values(place)(:counts(place)))
      call split(val,strings_a,delimiters=delimiters)   ! find value associated with keyword and split it into an array
      if( len(strings_a) <= len(strings) )then
         strings=strings_a
      else
         ibug=len(strings)
         call journal('*get_args_fixed_length_a_array* values too long. Longest is',len(strings_a),'allowed is',ibug)
         write(*,'("strings=",3x,*(a,1x))')strings
         call journal('*get_args_fixed_length_a_array* keyword='//keyword)
         call mystop(13,'*get_args_fixed_length_a_array* keyword='//keyword)
         strings=[character(len=len(strings)) ::]
      endif
   else
      call journal('*get_args_fixed_length_a_array* unknown keyword '//keyword)
      call mystop(14,'*get_args_fixed_length_a_array* unknown keyword '//keyword)
      strings=[character(len=len(strings)) ::]
   endif
end subroutine get_args_fixed_length_a_array
!===================================================================================================================================
! return non-allocatable arrays
!===================================================================================================================================
subroutine get_fixedarray_i(keyword,iarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
integer                              :: iarray(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
integer                              :: dsize
integer                              :: ibug
   call get_anyarray_d(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(iarray,dim=1) == dsize)then
      iarray=nint(darray)
   else
      ibug=size(iarray)
      call journal('*get_fixedarray_i* wrong number of values for keyword',keyword,'got',dsize,'expected',ibug)
      call print_dictionary_usage()
      call mystop(33)
      iarray=0
   endif
end subroutine get_fixedarray_i
!===================================================================================================================================
subroutine get_fixedarray_r(keyword,rarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
real                                 :: rarray(:)
character(len=*),intent(in),optional :: delimiters
real,allocatable                     :: darray(:)    ! function type
integer                              :: dsize
integer                              :: ibug
   call get_anyarray_r(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(rarray,dim=1) == dsize)then
      rarray=darray
   else
      ibug=size(rarray)
      call journal('*get_fixedarray_r* wrong number of values for keyword',keyword,'got',dsize,'expected',ibug)
      call print_dictionary_usage()
      call mystop(33)
      rarray=0.0
   endif
end subroutine get_fixedarray_r
!===================================================================================================================================
subroutine get_fixed_size_complex(keyword,xarray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
complex                              :: xarray(:)
character(len=*),intent(in),optional :: delimiters
complex,allocatable                  :: darray(:)    ! function type
integer                              :: half, sz
integer                              :: dsize
integer                              :: ibug
   call get_anyarray_x(keyword,darray,delimiters)
   dsize=size(darray)
   sz=dsize*2
   half=sz/2
   if(sz /= half+half)then
      call journal('*get_fixed_size_complex* uneven number of values defining complex value '//keyword)
      call mystop(15,'*get_fixed_size_complex* uneven number of values defining complex value '//keyword)
      xarray=0
      return
   endif
   if(ubound(xarray,dim=1) == dsize)then
      xarray=darray
   else
      ibug=size(xarray)
      call journal('*get_fixed_size_complex* wrong number of values for keyword',keyword,'got',dsize,'expected',ibug)
      call print_dictionary_usage()
      call mystop(34)
      xarray=cmplx(0.0,0.0)
   endif
end subroutine get_fixed_size_complex
!===================================================================================================================================
subroutine get_fixedarray_d(keyword,darr,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
real(kind=dp)                        :: darr(:)
character(len=*),intent(in),optional :: delimiters
real(kind=dp),allocatable            :: darray(:)    ! function type
integer                              :: dsize
integer                              :: ibug
   call get_anyarray_d(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(darr,dim=1) == dsize)then
      darr=darray
   else
      ibug=size(darr)
      call journal('*get_fixedarray_d* wrong number of values for keyword',keyword,'got',dsize,'expected',ibug)
      call print_dictionary_usage()
      call mystop(35)
      darr=0.0d0
   endif
end subroutine get_fixedarray_d
!===================================================================================================================================
subroutine get_fixedarray_l(keyword,larray,delimiters)
character(len=*),intent(in)          :: keyword      ! keyword to retrieve value from dictionary
logical                              :: larray(:)
character(len=*),intent(in),optional :: delimiters
logical,allocatable                  :: darray(:)    ! function type
integer                              :: dsize
integer                              :: ibug
   call get_anyarray_l(keyword,darray,delimiters)
   dsize=size(darray)
   if(ubound(larray,dim=1) == dsize)then
      larray=darray
   else
      ibug=size(larray)
      call journal('*get_fixedarray_l* wrong number of values for keyword',keyword,'got',dsize,'expected',ibug)
      call print_dictionary_usage()
      call mystop(36)
      larray=.false.
   endif
end subroutine get_fixedarray_l
!===================================================================================================================================
subroutine get_fixedarray_fixed_length_c(keyword,strings,delimiters)

! ident_9="@(#) M_CLI2 get_fixedarray_fixed_length_c(3) Fetch strings value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*)                     :: strings(:)
character(len=*),intent(in),optional :: delimiters
character(len=:),allocatable         :: str(:)
character(len=*),intent(in)          :: keyword   ! name to look up in dictionary
integer                              :: place
integer                              :: ssize
integer                              :: ibug
character(len=:),allocatable         :: val
   call locate_key(keyword,place)                 ! find where string is or should be
   if(place > 0)then                              ! if index is valid return strings
      val=unquote(values(place)(:counts(place)))
      call split(val,str,delimiters=delimiters)   ! find value associated with keyword and split it into an array
      ssize=size(str)
      if(ssize==size(strings))then
         strings(:ssize)=str
      else
         ibug=size(strings)
         call journal('*get_fixedarray_fixed_length_c* wrong number of values for keyword',&
            & keyword,'got',ssize,'expected ',ibug) !,ubound(strings,dim=1)
         call print_dictionary_usage()
         call mystop(30,'*get_fixedarray_fixed_length_c* unknown keyword '//keyword)
         strings=''
      endif
   else
      call journal('*get_fixedarray_fixed_length_c* unknown keyword '//keyword)
      call mystop(16,'*get_fixedarray_fixed_length_c* unknown keyword '//keyword)
      strings=''
   endif
end subroutine get_fixedarray_fixed_length_c
!===================================================================================================================================
! return scalars
!===================================================================================================================================
subroutine get_scalar_d(keyword,d)
character(len=*),intent(in)   :: keyword      ! keyword to retrieve value from dictionary
real(kind=dp)                 :: d
real(kind=dp),allocatable     :: darray(:)    ! function type
integer                       :: ibug
   call get_anyarray_d(keyword,darray)
   if(size(darray) == 1)then
      d=darray(1)
   else
      ibug=size(darray)
      call journal('*get_anyarray_d* incorrect number of values for keyword "',keyword,'" expected one found',ibug)
      call print_dictionary_usage()
      call mystop(31,'*get_anyarray_d* incorrect number of values for keyword "'//keyword//'" expected one')
   endif
end subroutine get_scalar_d
!===================================================================================================================================
subroutine get_scalar_real(keyword,r)
character(len=*),intent(in)   :: keyword      ! keyword to retrieve value from dictionary
real,intent(out)              :: r
real(kind=dp)                 :: d
   call get_scalar_d(keyword,d)
   r=real(d)
end subroutine get_scalar_real
!===================================================================================================================================
subroutine get_scalar_i(keyword,i)
character(len=*),intent(in)   :: keyword      ! keyword to retrieve value from dictionary
integer,intent(out)           :: i
real(kind=dp)                 :: d
   call get_scalar_d(keyword,d)
   i=nint(d)
end subroutine get_scalar_i
!===================================================================================================================================
subroutine get_scalar_anylength_c(keyword,string)

! ident_10="@(#) M_CLI2 get_scalar_anylength_c(3) Fetch string value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)   :: keyword              ! name to look up in dictionary
character(len=:),allocatable,intent(out)  :: string
integer                       :: place
   call locate_key(keyword, place)                     ! find where string is or should be
   if (place > 0) then                                  ! if index is valid return string
      string = unquote(values(place) (:counts(place)))
   else
      call journal('*get_anyarray_c* unknown keyword '//keyword)
      call mystop(17, '*get_anyarray_c* unknown keyword '//keyword)
      string = ''
   endif
end subroutine get_scalar_anylength_c
!===================================================================================================================================
elemental impure subroutine get_args_fixed_length_scalar_c(keyword,string)

! ident_11="@(#) M_CLI2 get_args_fixed_length_scalar_c(3) Fetch string value for specified KEYWORD from the lang. dictionary"

! This routine trusts that the desired keyword exists. A blank is returned if the keyword is not in the dictionary
character(len=*),intent(in)   :: keyword              ! name to look up in dictionary
character(len=*),intent(out)  :: string
integer                       :: place
integer                       :: unlen
integer                       :: ibug
   call locate_key(keyword, place)                     ! find where string is or should be

   if (place > 0) then                                  ! if index is valid return string
      string = unquote(values(place) (:counts(place)))
   else
      call mystop(18, '*get_args_fixed_length_scalar_c* unknown keyword '//keyword)
      string = ''
   endif

   unlen = len_trim(unquote(values(place) (:counts(place))))
   if (unlen > len(string)) then
      ibug = len(string)
      call journal('*get_args_fixed_length_scalar_c* value too long for', keyword, 'allowed is', ibug,&
      & 'input string [', values(place), '] is', unlen)
      call mystop(19, '*get_args_fixed_length_scalar_c* value too long')
      string = ''
   endif

end subroutine get_args_fixed_length_scalar_c
!===================================================================================================================================
subroutine get_scalar_complex(keyword,x)
character(len=*),intent(in) :: keyword      ! keyword to retrieve value from dictionary
complex,intent(out)         :: x
real(kind=dp)               :: d(2)

   call get_fixedarray_d(keyword,d)
   x=cmplx(d(1),d(2),kind=sp)

end subroutine get_scalar_complex
!===================================================================================================================================
subroutine get_scalar_logical(keyword,l)
character(len=*),intent(in)   :: keyword      ! keyword to retrieve value from dictionary
logical                       :: l
logical,allocatable           :: larray(:)    ! function type
integer                       :: ibug

   l = .false.

   call get_anyarray_l(keyword, larray)

   if (.not. allocated(larray)) then
      call journal('*get_scalar_logical* expected one value found not allocated')
      call mystop(37, '*get_scalar_logical* incorrect number of values for keyword "'//keyword//'"')
   elseif (size(larray) == 1) then
      l = larray(1)
   else
      ibug = size(larray)
      call journal('*get_scalar_logical* expected one value found', ibug)
      call mystop(21, '*get_scalar_logical* incorrect number of values for keyword "'//keyword//'"')
   endif

end subroutine get_scalar_logical
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! THE REMAINDER SHOULD BE ROUTINES EXTRACTED FROM OTHER MODULES TO MAKE THIS MODULE STANDALONE BY POPULAR REQUEST
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!use M_strings,                     only : UPPER, LOWER, QUOTE, REPLACE_STR=>REPLACE, UNQUOTE, SPLIT, STRING_TO_VALUE
!use M_list,                        only : insert, locate, remove, replace
!use M_journal,                     only : JOURNAL

!use M_args,                        only : LONGEST_COMMAND_ARGUMENT
! routines extracted from other modules
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    longest_command_argument(3) - [ARGUMENTS:M_args] length of longest
!!    argument on command line
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function longest_command_argument() result(ilongest)
!!
!!     integer :: ilongest
!!
!!##DESCRIPTION
!!    length of longest argument on command line. Useful when allocating
!!    storage for holding arguments.
!!##RETURNS
!!    longest_command_argument  length of longest command argument
!!##EXAMPLES
!!
!! Sample program
!!
!!      program demo_longest_command_argument
!!      use M_args, only : longest_command_argument
!!         write(*,*)'longest argument is ',longest_command_argument()
!!      end program demo_longest_command_argument
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function longest_command_argument() result(ilongest)
integer :: i
integer :: ilength
integer :: istatus
integer :: ilongest

   ilength = 0
   ilongest = 0

   GET_LONGEST: do i = 1, command_argument_count()                          ! loop throughout command line arguments to find longest

      call get_command_argument(number=i, length=ilength, status=istatus)   ! get next argument

      if (istatus /= 0) then                                                ! on error
         write (warn, *) '*prototype_and_cmd_args_to_nlist* error obtaining length for argument ', i
         exit GET_LONGEST
      elseif (ilength > 0) then
         ilongest = max(ilongest, ilength)
      endif

   end do GET_LONGEST

end function longest_command_argument
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    journal(3) - [M_CLI2] converts a list of standard scalar types to a string and writes message
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine journal(g0,g1,g2,g3,g4,g5,g6,g7,g8,g9,ga,gb,gc,gd,ge,gf,gg,gh,gi,gj,sep,line)
!!
!!     class(*),intent(in),optional  :: g0,g1,g2,g3,g4,g5,g6,g7,g8,g9
!!     class(*),intent(in),optional  :: ga,gb,gc,gd,ge,gf,gg,gh,gi,gj
!!     character(len=*),intent(in),optional :: sep
!!     character(len=:),intent(out),allocatable,optional :: line
!!
!!##DESCRIPTION
!!    journal(3) builds and prints a space-separated string from up to twenty scalar values.
!!
!!##OPTIONS
!!    g[0-9a-j]   optional value to print the value of after the message. May
!!                be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION,
!!                COMPLEX, or CHARACTER.
!!
!!    sep         separator to place between values. Defaults to a space.
!!    line        if present, the output is placed in the variable instead of
!!                being written
!!##RETURNS
!!    journal     description to print
!!##EXAMPLES
!!
!! Sample program:
!!
!!     program demo_journal
!!     use M_CLI2, only : journal
!!     implicit none
!!     character(len=:),allocatable :: frmt
!!     integer                      :: biggest
!!
!!     call journal('HUGE(3) integers',huge(0),'and real',&
!!               & huge(0.0),'and double',huge(0.0d0))
!!     call journal('real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
!!     call journal('doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
!!     call journal('complex         :',cmplx(huge(0.0),tiny(0.0)) )
!!
!!     end program demo_journal
!!
!!  Output
!!
!!     HUGE(3) integers 2147483647 and real 3.40282347E+38 and
!!     double 1.7976931348623157E+308
!!     real            : 3.40282347E+38 0.00000000 12345.6787 1.17549435E-38
!!     doubleprecision : 1.7976931348623157E+308 0.0000000000000000
!!     12345.678900000001 2.2250738585072014E-308
!!     complex         : (3.40282347E+38,1.17549435E-38)
!!      format=(*(i9:,1x))
!!      program will now stop
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine journal(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, sep,line)

! ident_12="@(#) M_CLI2 journal(3fp) writes a message to stdout or a string composed of any standard scalar types"

class(*),intent(in),optional         :: g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj
character(len=*),intent(in),optional :: sep
character(len=:),intent(out),allocatable,optional :: line
character(len=:),allocatable         :: sep_local
character(len=4096)                  :: local_line
integer                              :: istart
integer                              :: increment
   if(present(sep))then
      sep_local=sep
      increment=len(sep_local)+1
   else
      sep_local=' '
      increment=2
   endif

   istart=1
   local_line=''
   if(present(g0))call print_generic(g0)
   if(present(g1))call print_generic(g1)
   if(present(g2))call print_generic(g2)
   if(present(g3))call print_generic(g3)
   if(present(g4))call print_generic(g4)
   if(present(g5))call print_generic(g5)
   if(present(g6))call print_generic(g6)
   if(present(g7))call print_generic(g7)
   if(present(g8))call print_generic(g8)
   if(present(g9))call print_generic(g9)
   if(present(ga))call print_generic(ga)
   if(present(gb))call print_generic(gb)
   if(present(gc))call print_generic(gc)
   if(present(gd))call print_generic(gd)
   if(present(ge))call print_generic(ge)
   if(present(gf))call print_generic(gf)
   if(present(gg))call print_generic(gg)
   if(present(gh))call print_generic(gh)
   if(present(gi))call print_generic(gi)
   if(present(gj))call print_generic(gj)
   if(present(line))then
      line=trim(local_line)
   else
      write(*,'(a)')trim(local_line)
   endif
contains
!===================================================================================================================================
subroutine print_generic(generic)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic
   select type(generic)
      type is (integer(kind=int8));     write(local_line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(local_line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(local_line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(local_line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(local_line(istart:),'(1pg0)') generic
      type is (real(kind=real64))
         write(local_line(istart:),'(1pg0)') generic
      !x! DOES NOT WORK WITH NVFORTRAN: type is (real(kind=real128));     write(local_line(istart:),'(1pg0)') generic
      type is (logical)
         write(local_line(istart:),'(l1)') generic
      type is (character(len=*))
         write(local_line(istart:),'(a)') trim(generic)
      type is (complex);                write(local_line(istart:),'("(",1pg0,",",1pg0,")")') generic
   end select
   istart=len_trim(local_line)+increment
   local_line=trim(local_line)//sep_local
end subroutine print_generic
!===================================================================================================================================
end subroutine journal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function str(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, sep) result(line)

! ident_13="@(#) M_CLI2 str(3fp) writes a message to a string composed of any standard scalar types"

class(*),intent(in),optional         :: g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj
character(len=*),intent(in),optional :: sep
character(len=:),allocatable         :: line
call journal(g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj, sep,line)

end function str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function upper(str) result (string)

! ident_14="@(#) M_CLI2 upper(3) Changes a string to uppercase"

character(*), intent(in)      :: str
character(:),allocatable      :: string
integer                       :: i
   string = str
   do i = 1, len_trim(str)
       select case (str(i:i))
       case ('a':'z')
          string(i:i) = char(iachar(str(i:i))-32)
       end select
   end do
end function upper
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function lower(str) result (string)

! ident_15="@(#) M_CLI2 lower(3) Changes a string to lowercase over specified range"

character(*), intent(In)     :: str
character(:),allocatable     :: string
integer                      :: i
   string = str
   do i = 1, len_trim(str)
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))+32)
      end select
   end do
end function lower
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine a2i(chars,valu,ierr)

! ident_16="@(#) M_CLI2 a2i(3fp) subroutine returns integer value from string"

character(len=*),intent(in) :: chars                      ! input string
integer,intent(out)         :: valu                       ! value read from input string
integer,intent(out)         :: ierr                       ! error flag (0 == no error)
doubleprecision             :: valu8
integer,parameter           :: ihuge=huge(0)

   valu8 = 0.0d0
   call a2d(chars, valu8, ierr, onerr=0.0d0)

   if (valu8 <= huge(valu)) then

      if (valu8 <= huge(valu)) then
         valu = int(valu8)
      else
         call journal('*a2i*', '- value too large', valu8, '>', ihuge)
         valu = huge(valu)
         ierr = -1
      endif

   endif

end subroutine a2i
!----------------------------------------------------------------------------------------------------------------------------------
subroutine a2d(chars,valu,ierr,onerr)

! ident_17="@(#) M_CLI2 a2d(3fp) subroutine returns double value from string"

!     1989,2016 John S. Urban.
!
!  o  works with any g-format input, including integer, real, and exponential.
!  o  if an error occurs in the read, iostat is returned in ierr and value is set to zero. If no error occurs, ierr=0.
!  o  if the string happens to be 'eod' no error message is produced so this string may be used to act as an end-of-data.
!     IERR will still be non-zero in this case.
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: chars                        ! input string
character(len=:),allocatable :: local_chars
doubleprecision,intent(out)  :: valu                         ! value read from input string
integer,intent(out)          :: ierr                         ! error flag (0 == no error)
class(*),optional,intent(in) :: onerr
!----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter   :: fmt="('(bn,g',i5,'.0)')"     ! format used to build frmt
character(len=15)            :: frmt                         ! holds format built to read input string
character(len=256)           :: msg                          ! hold message from I/O errors
integer                      :: intg
integer                      :: pnd
integer                      :: basevalue, ivalu
character(len=3),save        :: nan_string='NaN'
!----------------------------------------------------------------------------------------------------------------------------------
   ierr=0                                                       ! initialize error flag to zero
   local_chars=unquote(chars)
   msg=''
   if(len(local_chars) == 0)local_chars=' '
   local_chars=replace_str(local_chars,',','')                  ! remove any comma characters
   pnd=scan(local_chars,'#:')
   if(pnd /= 0)then
      write(frmt,fmt)pnd-1                                      ! build format of form '(BN,Gn.0)'
      read(local_chars(:pnd-1),fmt=frmt,iostat=ierr,iomsg=msg)basevalue   ! try to read value from string
      if(decodebase(local_chars(pnd+1:),basevalue,ivalu))then
         valu=real(ivalu,kind=kind(0.0d0))
      else
         valu=0.0d0
         ierr=-1
      endif
   else
      select case(local_chars(1:1))
      case('z','Z','h','H')                                     ! assume hexadecimal
         write(frmt,"('(Z',i0,')')")len(local_chars)
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('b','B')                                             ! assume binary (base 2)
         write(frmt,"('(B',i0,')')")len(local_chars)
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case('o','O')                                             ! assume octal
         write(frmt,"('(O',i0,')')")len(local_chars)
         read(local_chars(2:),frmt,iostat=ierr,iomsg=msg)intg
         valu=dble(intg)
      case default
         write(frmt,fmt)len(local_chars)                        ! build format of form '(BN,Gn.0)'
         read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu   ! try to read value from string
      end select
   endif
   if(ierr /= 0)then                                            ! if an error occurred ierr will be non-zero.
      if(present(onerr))then
         select type(onerr)
         type is (integer)
            valu=onerr
         type is (real)
            valu=onerr
         type is (doubleprecision)
            valu=onerr
         end select
      else                                                      ! set return value to NaN
         read(nan_string,'(f3.3)')valu
      endif
      if(local_chars /= 'eod')then                           ! print warning message except for special value "eod"
         call journal('*a2d* - cannot produce number from string ['//trim(chars)//']')
         if(msg /= '')then
            call journal('*a2d* - ['//trim(msg)//']')
         endif
      endif
   endif
end subroutine a2d
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    split(3) - [M_CLI2:TOKENS] parse string into an array using specified
!!    delimiters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine split(input_line,array,delimiters,order,nulls)
!!
!!     character(len=*),intent(in)              :: input_line
!!     character(len=:),allocatable,intent(out) :: array(:)
!!     character(len=*),optional,intent(in)     :: delimiters
!!     character(len=*),optional,intent(in)     :: order
!!     character(len=*),optional,intent(in)     :: nulls
!!##DESCRIPTION
!!    SPLIT(3) parses a string using specified delimiter characters and
!!    store tokens into an allocatable array
!!
!!##OPTIONS
!!
!!    INPUT_LINE  Input string to tokenize
!!
!!    ARRAY       Output array of tokens
!!
!!    DELIMITERS  List of delimiter characters.
!!                The default delimiters are the "whitespace" characters
!!                (space, tab,new line, vertical tab, formfeed, carriage
!!                return, and null). You may specify an alternate set of
!!                delimiter characters.
!!
!!                Multi-character delimiters are not supported (Each
!!                character in the DELIMITERS list is considered to be
!!                a delimiter).
!!
!!                Quoting of delimiter characters is not supported.
!!
!!    ORDER SEQUENTIAL|REVERSE|RIGHT  Order of output array.
!!                By default ARRAY contains the tokens having parsed
!!                the INPUT_LINE from left to right. If ORDER='RIGHT'
!!                or ORDER='REVERSE' the parsing goes from right to left.
!!
!!    NULLS IGNORE|RETURN|IGNOREEND  Treatment of null fields.
!!                By default adjacent delimiters in the input string
!!                do not create an empty string in the output array. if
!!                NULLS='return' adjacent delimiters create an empty element
!!                in the output ARRAY. If NULLS='ignoreend' then only
!!                trailing delimiters at the right of the string are ignored.
!!
!!##EXAMPLES
!!
!! Sample program:
!!
!!     program demo_split
!!     use M_CLI2, only: split
!!     character(len=*),parameter     :: &
!!     & line='  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
!!     character(len=:),allocatable :: array(:) ! output array of tokens
!!        write(*,*)'INPUT LINE:['//LINE//']'
!!        write(*,'(80("="))')
!!        write(*,*)'typical call:'
!!        CALL split(line,array)
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!        write(*,'(80("-"))')
!!        write(*,*)'custom list of delimiters (colon and vertical line):'
!!        CALL split(line,array,delimiters=':|',order='sequential',nulls='ignore')
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!        write(*,'(80("-"))')
!!        write(*,*)&
!!      &'custom list of delimiters, reverse array order and count null fields:'
!!        CALL split(line,array,delimiters=':|',order='reverse',nulls='return')
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!        write(*,'(80("-"))')
!!        write(*,*)'INPUT LINE:['//LINE//']'
!!        write(*,*)&
!!        &'default delimiters and reverse array order and return null fields:'
!!        CALL split(line,array,delimiters='',order='reverse',nulls='return')
!!        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
!!        write(*,*)'SIZE:',SIZE(array)
!!     end program demo_split
!!
!!   Output
!!
!!    > INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    ]
!!    > ===========================================================================
!!    >  typical call:
!!    > 1 ==> aBcdef
!!    > 2 ==> ghijklmnop
!!    > 3 ==> qrstuvwxyz
!!    > 4 ==> 1:|:2
!!    > 5 ==> 333|333
!!    > 6 ==> a
!!    > 7 ==> B
!!    > 8 ==> cc
!!    >  SIZE:           8
!!    > --------------------------------------------------------------------------
!!    >  custom list of delimiters (colon and vertical line):
!!    > 1 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!    > 2 ==> 2     333
!!    > 3 ==> 333 a B cc
!!    >  SIZE:           3
!!    > --------------------------------------------------------------------------
!!    >  custom list of delimiters, reverse array order and return null fields:
!!    > 1 ==> 333 a B cc
!!    > 2 ==> 2     333
!!    > 3 ==>
!!    > 4 ==>
!!    > 5 ==>   aBcdef   ghijklmnop qrstuvwxyz  1
!!    >  SIZE:           5
!!    > --------------------------------------------------------------------------
!!    >  INPUT LINE:[  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    ]
!!    >  default delimiters and reverse array order and count null fields:
!!    > 1 ==>
!!    > 2 ==>
!!    > 3 ==>
!!    > 4 ==> cc
!!    > 5 ==> B
!!    > 6 ==> a
!!    > 7 ==> 333|333
!!    > 8 ==>
!!    > 9 ==>
!!    > 10 ==>
!!    > 11 ==>
!!    > 12 ==> 1:|:2
!!    > 13 ==>
!!    > 14 ==> qrstuvwxyz
!!    > 15 ==> ghijklmnop
!!    > 16 ==>
!!    > 17 ==>
!!    > 18 ==> aBcdef
!!    > 19 ==>
!!    > 20 ==>
!!    >  SIZE:          20
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
subroutine split(input_line,array,delimiters,order,nulls)

! ident_18="@(#) M_CLI2 split(3) parse string on delimiter characters and store tokens into an allocatable array"

!  John S. Urban
intrinsic index, min, present, len
!  given a line of structure " par1 par2 par3 ... parn " store each par(n) into a separate variable in array.
!    o by default adjacent delimiters in the input string do not create an empty string in the output array
!    o no quoting of delimiters is supported
character(len=*),intent(in)              :: input_line  ! input string to tokenize
character(len=*),optional,intent(in)     :: delimiters  ! list of delimiter characters
character(len=*),optional,intent(in)     :: order       ! order of output array sequential|[reverse|right]
character(len=*),optional,intent(in)     :: nulls       ! return strings composed of delimiters or not ignore|return|ignoreend
character(len=:),allocatable,intent(out) :: array(:)    ! output array of tokens
!-----------------------------------------------------------------------------------------------------------------------------------
integer                       :: n                      ! max number of strings INPUT_LINE could split into if all delimiter
integer,allocatable           :: ibegin(:)              ! positions in input string where tokens start
integer,allocatable           :: iterm(:)               ! positions in input string where tokens end
character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
character(len=:),allocatable  :: ordr                   ! string containing order keyword
character(len=:),allocatable  :: nlls                   ! string containing nulls keyword
integer                       :: ii,iiii                ! loop parameters used to control print order
integer                       :: icount                 ! number of tokens found
integer                       :: iilen                  ! length of input string with trailing spaces trimmed
integer                       :: i10,i20,i30            ! loop counters
integer                       :: icol                   ! pointer into input string as it is being parsed
integer                       :: idlim                  ! number of delimiter characters
integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
integer                       :: inotnull               ! count strings not composed of delimiters
integer                       :: ireturn                ! number of tokens returned
integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   ! decide on value for optional DELIMITERS parameter
   if (present(delimiters)) then                                     ! optional delimiter list was present
      if(delimiters /= '')then                                       ! if DELIMITERS was specified and not null use it
         dlim=delimiters
      else                                                           ! DELIMITERS was specified on call as empty string
         dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)//',:' ! use default delimiter when not specified
      endif
   else                                                              ! no delimiter value was specified
      dlim=' '//char(9)//char(10)//char(11)//char(12)//char(13)//char(0)//',:'    ! use default delimiter when not specified
   endif
   idlim=len(dlim)                                                   ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(order))then; ordr=lower(adjustl(order)); else; ordr='sequential'; endif ! decide on value for optional ORDER parameter
   if(present(nulls))then; nlls=lower(adjustl(nulls)); else; nlls='ignore'    ; endif ! optional parameter
!-----------------------------------------------------------------------------------------------------------------------------------
   n=len(input_line)+1                        ! max number of strings INPUT_LINE could split into if all delimiter
   if(allocated(ibegin))deallocate(ibegin)    !x! intel compiler says allocated already ???
   allocate(ibegin(n))                        ! allocate enough space to hold starting location of tokens if string all tokens
   if(allocated(iterm))deallocate(iterm)      !x! intel compiler says allocated already ???
   allocate(iterm(n))                         ! allocate enough space to hold ending location of tokens if string all tokens
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   iilen=len(input_line)                                          ! IILEN is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
   if(iilen > 0)then                                              ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,iilen,1                                  ! store into each array element
         ibegin(i30)=icol                                         ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)) == 0)then  ! if current character is not a delimiter
            iterm(i30)=iilen                                      ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(i30):iilen),dlim(i10:i10))
               IF(ifound > 0)then
                  iterm(i30)=min(iterm(i30),ifound+ibegin(i30)-2)
               endif
            enddo
            icol=iterm(i30)+2                                     ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(i30)=icol-1                                     ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(i30)-ibegin(i30)+1)
         icount=i30                                               ! increment count of number of tokens found
         if(icol > iilen)then                                     ! no text left
            exit INFINITE
         endif
      enddo INFINITE
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (clipends(nlls))
   case ('ignore','','ignoreend')
      ireturn=inotnull
   case default
      ireturn=icount
   end select
   if(allocated(array))deallocate(array)
   allocate(character(len=imax) :: array(ireturn))                ! allocate the array to return
   !allocate(array(ireturn))                                      ! allocate the array to turn
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (clipends(ordr))                                   ! decide which order to store tokens
   case ('reverse','right') ; ii=ireturn ; iiii=-1                ! last to first
   case default             ; ii=1       ; iiii=1                 ! first to last
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,icount                                                ! fill the array with the tokens that were found
      if(iterm(i20) < ibegin(i20))then
         select case (clipends(nlls))
         case ('ignore','','ignoreend')
         case default
            array(ii)=' '
            ii=ii+iiii
         end select
      else
         array(ii)=input_line(ibegin(i20):iterm(i20))
         ii=ii+iiii
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine split
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    replace_str(3) - [M_CLI2:EDITING] function globally replaces one
!!    substring for another in string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function replace_str(targetline,old,new,range,ierr) result (newline)
!!
!!     character(len=*)               :: targetline
!!     character(len=*),intent(in)    :: old
!!     character(len=*),intent(in)    :: new
!!     integer,intent(in),optional    :: range(2)
!!     integer,intent(out),optional   :: ierr
!!     logical,intent(in),optional    :: clip
!!     character(len=:),allocatable   :: newline
!!##DESCRIPTION
!!    Globally replace one substring for another in string.
!!    Either CMD or OLD and NEW must be specified.
!!
!!##OPTIONS
!!     targetline  input line to be changed
!!     old         old substring to replace
!!     new         new substring
!!     range       if present, only change range(1) to range(2) of
!!                 occurrences of old string
!!     ierr        error code. If ier = -1 bad directive, >= 0 then
!!                 count of changes made
!!     clip        whether to return trailing spaces or not. Defaults to .false.
!!##RETURNS
!!     newline     allocatable string returned
!!
!!##EXAMPLES
!!
!! Sample Program:
!!
!!       program demo_replace_str
!!       use M_CLI2, only : replace_str
!!       implicit none
!!       character(len=:),allocatable :: targetline
!!
!!       targetline='this is the input string'
!!
!!       call testit('th','TH','THis is THe input string')
!!
!!       ! a null old substring means "at beginning of line"
!!       call testit('','BEFORE:', 'BEFORE:THis is THe input string')
!!
!!       ! a null new string deletes occurrences of the old substring
!!       call testit('i','', 'BEFORE:THs s THe nput strng')
!!
!!       targetline=replace_str('a b ab baaa aaaa','a','A')
!!       write(*,*)'replace a with A ['//targetline//']'
!!
!!       write(*,*)'Examples of the use of RANGE='
!!
!!       targetline=replace_str('a b ab baaa aaaa','a','A',range=[3,5])
!!       write(*,*)'replace a with A instances 3 to 5 ['//targetline//']'
!!
!!       targetline=replace_str('a b ab baaa aaaa','a','',range=[3,5])
!!       write(*,*)'replace a with null instances 3 to 5 ['//targetline//']'
!!
!!       targetline=replace_str('a b ab baaa aaaa aa aa a a a aa aaaaaa',&
!!        & 'aa','CCCC',range=[3,5])
!!       write(*,*)'replace aa with CCCC instances 3 to 5 ['//targetline//']'
!!
!!       contains
!!       subroutine testit(old,new,expected)
!!       character(len=*),intent(in) :: old,new,expected
!!       write(*,*)repeat('=',79)
!!       write(*,*)':STARTED ['//targetline//']'
!!       write(*,*)':OLD['//old//']', ' NEW['//new//']'
!!       targetline=replace_str(targetline,old,new)
!!       write(*,*)':GOT     ['//targetline//']'
!!       write(*,*)':EXPECTED['//expected//']'
!!       write(*,*)':TEST    [',targetline == expected,']'
!!       end subroutine testit
!!
!!       end program demo_replace_str
!!
!!   Expected output
!!
!!     ===============================================================================
!!     STARTED [this is the input string]
!!     OLD[th] NEW[TH]
!!     GOT     [THis is THe input string]
!!     EXPECTED[THis is THe input string]
!!     TEST    [ T ]
!!     ===============================================================================
!!     STARTED [THis is THe input string]
!!     OLD[] NEW[BEFORE:]
!!     GOT     [BEFORE:THis is THe input string]
!!     EXPECTED[BEFORE:THis is THe input string]
!!     TEST    [ T ]
!!     ===============================================================================
!!     STARTED [BEFORE:THis is THe input string]
!!     OLD[i] NEW[]
!!     GOT     [BEFORE:THs s THe nput strng]
!!     EXPECTED[BEFORE:THs s THe nput strng]
!!     TEST    [ T ]
!!     replace a with A [A b Ab bAAA AAAA]
!!     Examples of the use of RANGE=
!!     replace a with A instances 3 to 5 [a b ab bAAA aaaa]
!!     replace a with null instances 3 to 5 [a b ab b aaaa]
!!     replace aa with CCCC instances 3 to 5 [a b ab baaa aaCCCC CCCC CCCC
!!     a a a aa aaaaaa]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function replace_str(targetline,old,new,ierr,range) result (newline)

! ident_19="@(#) M_CLI2 replace_str(3) Globally replace one substring for another in string"

!-----------------------------------------------------------------------------------------------------------------------------------
! parameters
character(len=*),intent(in)            :: targetline   ! input line to be changed
character(len=*),intent(in)            :: old          ! old substring to replace
character(len=*),intent(in)            :: new          ! new substring
integer,intent(out),optional           :: ierr         ! error code. If ierr = -1 bad directive, >=0 then ierr changes made
integer,intent(in),optional            :: range(2)     ! start and end of which changes to make
!-----------------------------------------------------------------------------------------------------------------------------------
! returns
character(len=:),allocatable  :: newline               ! output string buffer
!-----------------------------------------------------------------------------------------------------------------------------------
! local
integer  :: icount,ichange
integer  :: original_input_length
integer  :: len_old, len_new
integer  :: ladd
integer  :: left_margin, right_margin
integer  :: ind
integer  :: ic
integer  :: iichar
integer  :: range_local(2)
!-----------------------------------------------------------------------------------------------------------------------------------
   icount=0                                            ! initialize error flag/change count
   ichange=0                                           ! initialize error flag/change count
   original_input_length=len_trim(targetline)          ! get non-blank length of input line
   len_old=len(old)                                    ! length of old substring to be replaced
   len_new=len(new)                                    ! length of new substring to replace old substring
   left_margin=1                                       ! left_margin is left margin of window to change
   right_margin=len(targetline)                        ! right_margin is right margin of window to change
   newline=''                                          ! begin with a blank line as output string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(range))then
      range_local=range
   else
      range_local=[1,original_input_length]
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(len_old == 0)then                                ! c//new/ means insert new at beginning of line (or left margin)
      iichar=len_new + original_input_length
      if(len_new > 0)then
         newline=new(:len_new)//targetline(left_margin:original_input_length)
      else
         newline=targetline(left_margin:original_input_length)
      endif
      ichange=1                                        ! made one change. actually, c/// should maybe return 0
      if(present(ierr))ierr=ichange
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   iichar=left_margin                                  ! place to put characters into output string
   ic=left_margin                                      ! place looking at in input string
   loop: do
      ind=index(targetline(ic:),old(:len_old))+ic-1 ! try finding start of OLD in remaining part of input in change window
      if(ind == ic-1.or.ind > right_margin)then           ! did not find old string or found old string past edit window
         exit loop                                        ! no more changes left to make
      endif
      icount=icount+1                                  ! found an old string to change, so increment count of change candidates
      if(ind > ic)then                                 ! if found old string past at current position in input string copy unchanged
         ladd=ind-ic                                   ! find length of character range to copy as-is from input to output
         newline=newline(:iichar-1)//targetline(ic:ind-1)
         iichar=iichar+ladd
      endif
      if(icount >= range_local(1).and.icount <= range_local(2))then    ! check if this is an instance to change or keep
         ichange=ichange+1
         if(len_new /= 0)then                                          ! put in new string
            newline=newline(:iichar-1)//new(:len_new)
            iichar=iichar+len_new
         endif
      else
         if(len_old /= 0)then                                          ! put in copy of old string
            newline=newline(:iichar-1)//old(:len_old)
            iichar=iichar+len_old
         endif
      endif
      ic=ind+len_old
   enddo loop
!-----------------------------------------------------------------------------------------------------------------------------------
   select case (ichange)
   case (0)                                            ! there were no changes made to the window
      newline=targetline                               ! if no changes made output should be input
   case default
      if(ic <= len(targetline))then                    ! if there is more after last change on original line add it
         newline=newline(:iichar-1)//targetline(ic:max(ic,original_input_length))
      endif
   end select
   if(present(ierr))ierr=ichange
!-----------------------------------------------------------------------------------------------------------------------------------
end function replace_str
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    quote(3) - [M_CLI2:QUOTES] add quotes to string as if written with
!!    list-directed input
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   function quote(str,mode,clip) result (quoted_str)
!!
!!    character(len=*),intent(in)          :: str
!!    character(len=*),optional,intent(in) :: mode
!!    logical,optional,intent(in)          :: clip
!!    character(len=:),allocatable         :: quoted_str
!!##DESCRIPTION
!!    Add quotes to a CHARACTER variable as if it was written using
!!    list-directed input. This is particularly useful for processing
!!    strings to add to CSV files.
!!
!!##OPTIONS
!!    str         input string to add quotes to, using the rules of
!!                list-directed input (single quotes are replaced by two
!!                adjacent quotes)
!!    mode        alternate quoting methods are supported:
!!
!!                   DOUBLE   default. replace quote with double quotes
!!                   ESCAPE   replace quotes with backslash-quote instead
!!                            of double quotes
!!
!!    clip        default is to trim leading and trailing spaces from the
!!                string. If CLIP
!!                is .FALSE. spaces are not trimmed
!!
!!##RETURNS
!!    quoted_str  The output string, which is based on adding quotes to STR.
!!##EXAMPLES
!!
!! Sample program:
!!
!!     program demo_quote
!!     use M_CLI2, only : quote
!!     implicit none
!!     character(len=:),allocatable :: str
!!     character(len=1024)          :: msg
!!     integer                      :: ios
!!     character(len=80)            :: inline
!!        do
!!           write(*,'(a)',advance='no')'Enter test string:'
!!           read(*,'(a)',iostat=ios,iomsg=msg)inline
!!           if(ios /= 0)then
!!              write(*,*)trim(inline)
!!              exit
!!           endif
!!
!!           ! the original string
!!           write(*,'(a)')'ORIGINAL     ['//trim(inline)//']'
!!
!!           ! the string processed by quote(3)
!!           str=quote(inline)
!!           write(*,'(a)')'QUOTED     ['//str//']'
!!
!!           ! write the string list-directed to compare the results
!!           write(*,'(a)',iostat=ios,iomsg=msg) 'LIST DIRECTED:'
!!           write(*,*,iostat=ios,iomsg=msg,delim='none') inline
!!           write(*,*,iostat=ios,iomsg=msg,delim='quote') inline
!!           write(*,*,iostat=ios,iomsg=msg,delim='apostrophe') inline
!!        enddo
!!     end program demo_quote
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
function quote(str,mode,clip) result (quoted_str)
character(len=*),intent(in)          :: str                ! the string to be quoted
character(len=*),optional,intent(in) :: mode
logical,optional,intent(in)          :: clip
logical                              :: clip_local
character(len=:),allocatable         :: quoted_str

character(len=1),parameter           :: double_quote = '"'
character(len=20)                    :: local_mode

   if(present(mode))then
      local_mode=mode
   else
      local_mode='DOUBLE'
   endif

   if(present(clip))then
      clip_local=clip
   else
      clip_local=.false.
   endif

   if(clip_local)then
      quoted_str=adjustl(str)
   else
      quoted_str=str
   endif

   select case(lower(local_mode))
   case('double')
      quoted_str=double_quote//trim(replace_str(quoted_str,'"','""'))//double_quote
   case('escape')
      quoted_str=double_quote//trim(replace_str(quoted_str,'"',bslash//'"'))//double_quote
   case default
      call journal('*quote* ERROR: unknown quote mode ',local_mode)
      quoted_str=str
   end select

end function quote
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    unquote(3) - [M_CLI2:QUOTES] remove quotes from string as if read
!!    with list-directed input
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   pure function unquote(quoted_str,esc) result (unquoted_str)
!!
!!    character(len=*),intent(in)          :: quoted_str
!!    character(len=1),optional,intent(in) :: esc
!!    character(len=:),allocatable         :: unquoted_str
!!##DESCRIPTION
!!    Remove quotes from a CHARACTER variable as if it was read using
!!    list-directed input. This is particularly useful for processing
!!    tokens read from input such as CSV files.
!!
!!    Fortran can now read using list-directed input from an internal file,
!!    which should handle quoted strings, but list-directed input does not
!!    support escape characters, which UNQUOTE(3) does.
!!##OPTIONS
!!    quoted_str  input string to remove quotes from, using the rules of
!!                list-directed input (two adjacent quotes inside a quoted
!!                region are replaced by a single quote, a single quote or
!!                double quote is selected as the delimiter based on which
!!                is encountered first going from left to right, ...)
!!    esc         optional character used to protect the next quote
!!                character from being processed as a quote, but simply as
!!                a plain character.
!!##RETURNS
!!    unquoted_str  The output string, which is based on removing quotes
!!                  from quoted_str.
!!##EXAMPLES
!!
!! Sample program:
!!
!!       program demo_unquote
!!       use M_CLI2, only : unquote
!!       implicit none
!!       character(len=128)           :: quoted_str
!!       character(len=:),allocatable :: unquoted_str
!!       character(len=1),parameter   :: esc=bslash
!!       character(len=1024)          :: msg
!!       integer                      :: ios
!!       character(len=1024)          :: dummy
!!       do
!!          write(*,'(a)',advance='no')'Enter test string:'
!!          read(*,'(a)',iostat=ios,iomsg=msg)quoted_str
!!          if(ios /= 0)then
!!             write(*,*)trim(msg)
!!             exit
!!          endif
!!
!!          ! the original string
!!          write(*,'(a)')'QUOTED       ['//trim(quoted_str)//']'
!!
!!          ! the string processed by unquote(3)
!!          unquoted_str=unquote(trim(quoted_str),esc)
!!          write(*,'(a)')'UNQUOTED     ['//unquoted_str//']'
!!
!!          ! read the string list-directed to compare the results
!!          read(quoted_str,*,iostat=ios,iomsg=msg)dummy
!!          if(ios /= 0)then
!!             write(*,*)trim(msg)
!!          else
!!             write(*,'(a)')'LIST DIRECTED['//trim(dummy)//']'
!!          endif
!!       enddo
!!       end program demo_unquote
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
pure function unquote(quoted_str,esc) result (unquoted_str)
character(len=*),intent(in)          :: quoted_str              ! the string to be unquoted
character(len=1),optional,intent(in) :: esc                     ! escape character
character(len=:),allocatable         :: unquoted_str
integer                              :: inlen
character(len=1),parameter           :: single_quote = "'"
character(len=1),parameter           :: double_quote = '"'
integer                              :: quote                   ! whichever quote is to be used
integer                              :: before
integer                              :: current
integer                              :: iesc
integer                              :: iput
integer                              :: i
logical                              :: inside
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(esc))then                           ! select escape character as specified character or special value meaning not set
      iesc=ichar(esc)                             ! allow for an escape character
   else
      iesc=-1                                     ! set to value that matches no character
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   inlen=len(quoted_str)                          ! find length of input string
   if(allocated(unquoted_str))deallocate(unquoted_str)
   allocate(character(len=inlen) :: unquoted_str) ! initially make output string length of input string
!-----------------------------------------------------------------------------------------------------------------------------------
   if(inlen >= 1)then                             ! double_quote is the default quote unless the first character is single_quote
      if(quoted_str(1:1) == single_quote)then
         quote=ichar(single_quote)
      else
         quote=ichar(double_quote)
      endif
   else
      quote=ichar(double_quote)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   before=-2                                      ! initially set previous character to impossible value
   unquoted_str(:)=''                             ! initialize output string to null string
   iput=1
   inside=.false.
   STEPTHROUGH: do i=1,inlen
      current=ichar(quoted_str(i:i))
      if(before == iesc)then                      ! if previous character was escape use current character unconditionally
           iput=iput-1                            ! backup
           unquoted_str(iput:iput)=char(current)
           iput=iput+1
           before=-2                              ! this could be second esc or quote
      elseif(current == quote)then                ! if current is a quote it depends on whether previous character was a quote
         if(before == quote)then
           unquoted_str(iput:iput)=char(quote)    ! this is second quote so retain it
           iput=iput+1
           before=-2
         elseif(.not.inside.and.before /= iesc)then
            inside=.true.
         else                                     ! this is first quote so ignore it except remember it in case next is a quote
            before=current
         endif
      else
         unquoted_str(iput:iput)=char(current)
         iput=iput+1
         before=current
      endif
   enddo STEPTHROUGH
!-----------------------------------------------------------------------------------------------------------------------------------
   unquoted_str=unquoted_str(:iput-1)
!-----------------------------------------------------------------------------------------------------------------------------------
end function unquote
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    decodebase(3) - [M_CLI2:BASE] convert whole number string in base
!!                     [2-36] to base 10 number
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   logical function decodebase(string,basein,out10)
!!
!!    character(len=*),intent(in)  :: string
!!    integer,intent(in)           :: basein
!!    integer,intent(out)          :: out10
!!##DESCRIPTION
!!
!!    Convert a numeric string representing a whole number in base BASEIN
!!    to base 10. The function returns FALSE if BASEIN is not in the range
!!    [2..36] or if string STRING contains invalid characters in base BASEIN
!!    or if result OUT10 is too big
!!
!!    The letters A,B,...,Z represent 10,11,...,36 in the base > 10.
!!
!!##OPTIONS
!!    string   input string. It represents a whole number in
!!             the base specified by BASEIN unless BASEIN is set
!!             to zero. When BASEIN is zero STRING is assumed to
!!             be of the form BASE#VALUE where BASE represents
!!             the function normally provided by BASEIN.
!!    basein   base of input string; either 0 or from 2 to 36.
!!    out10    output value in base 10
!!
!!##EXAMPLES
!!
!! Sample program:
!!
!!      program demo_decodebase
!!      use M_CLI2, only : codebase, decodebase
!!      implicit none
!!      integer           :: ba,bd
!!      character(len=40) :: x,y
!!      integer           :: r
!!
!!      print *,' BASE CONVERSION'
!!      write(*,'("Start   Base (2 to 36): ")',advance='no'); read *, bd
!!      write(*,'("Arrival Base (2 to 36): ")',advance='no'); read *, ba
!!      INFINITE: do
!!         print *,''
!!         write(*,'("Enter number in start base: ")',advance='no'); read *, x
!!         if(x == '0') exit INFINITE
!!         if(decodebase(x,bd,r)) then
!!            if(codebase(r,ba,y)) then
!!              write(*,'("In base ",I2,": ",A20)')  ba, y
!!            else
!!              print *,'Error in coding number.'
!!            endif
!!         else
!!            print *,'Error in decoding number.'
!!         endif
!!      enddo INFINITE
!!
!!      end program demo_decodebase
!!
!!##AUTHOR
!!    John S. Urban
!!
!!       Ref.: "Math matiques en Turbo-Pascal by
!!              M. Ducamp and A. Reverchon (2),
!!              Eyrolles, Paris, 1988".
!!
!!    based on a F90 Version By J-P Moreau (www.jpmoreau.fr)
!!
!!##LICENSE
!!    Public Domain
logical function decodebase(string,basein,out_baseten)

! ident_20="@(#) M_CLI2 decodebase(3) convert whole number string in base [2-36] to base 10 number"

character(len=*),intent(in)  :: string
integer,intent(in)           :: basein
integer,intent(out)          :: out_baseten

character(len=len(string))   :: string_local
integer           :: long, i, j, k
real              :: y
real              :: mult
character(len=1)  :: ch
real,parameter    :: XMAXREAL=real(huge(1))
integer           :: out_sign
integer           :: basein_local
integer           :: ipound
integer           :: ierr

  string_local=upper(clipends(string))
  decodebase=.false.

  ipound=index(string_local,'#')                                       ! determine if in form [-]base#whole
  if(basein == 0.and.ipound > 1)then                                  ! split string into two values
     call a2i(string_local(:ipound-1),basein_local,ierr)   ! get the decimal value of the base
     string_local=string_local(ipound+1:)                              ! now that base is known make string just the value
     if(basein_local >= 0)then                                         ! allow for a negative sign prefix
        out_sign=1
     else
        out_sign=-1
     endif
     basein_local=abs(basein_local)
  else                                                                 ! assume string is a simple positive value
     basein_local=abs(basein)
     out_sign=1
  endif

  out_baseten=0
  y=0.0
  ALL: if(basein_local<2.or.basein_local>36) then
    print *,'(*decodebase* ERROR: Base must be between 2 and 36. base=',basein_local
  else ALL
     out_baseten=0;y=0.0; mult=1.0
     long=LEN_TRIM(string_local)
     do i=1, long
        k=long+1-i
        ch=string_local(k:k)
        IF(CH == '-'.AND.K == 1)THEN
           out_sign=-1
           cycle
        endif
        if(ch<'0'.or.ch>'Z'.or.(ch>'9'.and.ch<'A'))then
           write(*,*)'*decodebase* ERROR: invalid character ',ch
           exit ALL
        endif
        if(ch<='9') then
              j=IACHAR(ch)-IACHAR('0')
        else
              j=IACHAR(ch)-IACHAR('A')+10
        endif
        if(j>=basein_local)then
           exit ALL
        endif
        y=y+mult*j
        if(mult>XMAXREAL/basein_local)then
           exit ALL
        endif
        mult=mult*basein_local
     enddo
     decodebase=.true.
     out_baseten=nint(out_sign*y)*sign(1,basein)
  endif ALL
end function decodebase
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    locate_(3) - [M_CLI2] finds the index where a string is found or
!!                  should be in a sorted array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine locate_(list,value,place,ier,errmsg)
!!
!!    character(len=:)|doubleprecision|real|integer,allocatable :: list(:)
!!    character(len=*)|doubleprecision|real|integer,intent(in)  :: value
!!    integer, intent(out)                  :: PLACE
!!
!!    integer, intent(out),optional         :: IER
!!    character(len=*),intent(out),optional :: ERRMSG
!!
!!##DESCRIPTION
!!
!!    LOCATE_(3) finds the index where the VALUE is found or should
!!    be found in an array. The array must be sorted in descending
!!    order (highest at top). If VALUE is not found it returns the index
!!    where the name should be placed at with a negative sign.
!!
!!    The array and list must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL,INTEGER)
!!
!!##OPTIONS
!!
!!    VALUE         the value to locate in the list.
!!    LIST          is the list array.
!!
!!##RETURNS
!!    PLACE         is the subscript that the entry was found at if it is
!!                  greater than zero(0).
!!
!!                  If PLACE is negative, the absolute value of
!!                  PLACE indicates the subscript value where the
!!                  new entry should be placed in order to keep the
!!                  list alphabetized.
!!
!!    IER           is zero(0) if no error occurs.
!!                  If an error occurs and IER is not
!!                  present, the program is stopped.
!!
!!    ERRMSG        description of any error
!!
!!##EXAMPLES
!!
!!
!! Find if a string is in a sorted array, and insert the string into
!! the list if it is not present ...
!!
!!     program demo_locate
!!     use M_sort, only : sort_shell
!!     use M_CLI2, only : locate_
!!     implicit none
!!     character(len=:),allocatable  :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     call update_dic(arr,'b')
!!     call update_dic(arr,'[')
!!     call update_dic(arr,'c')
!!     call update_dic(arr,'ZZ')
!!     call update_dic(arr,'ZZZZ')
!!     call update_dic(arr,'z')
!!
!!     contains
!!     subroutine update_dic(arr,string)
!!     character(len=:),intent(in),allocatable :: arr(:)
!!     character(len=*),intent(in)  :: string
!!     integer                      :: place, plus, ii, end
!!     ! find where string is or should be
!!     call locate_(arr,string,place)
!!     write(*,*)'for "'//string//'" index is ',place, size(arr)
!!     ! if string was not found insert it
!!     if(place < 1)then
!!        plus=abs(place)
!!        ii=len(arr)
!!        end=size(arr)
!!        ! empty array
!!        if(end == 0)then
!!           arr=[character(len=ii) :: string ]
!!        ! put in front of array
!!        elseif(plus == 1)then
!!           arr=[character(len=ii) :: string, arr]
!!        ! put at end of array
!!        elseif(plus == end)then
!!           arr=[character(len=ii) :: arr, string ]
!!        ! put in middle of array
!!        else
!!           arr=[character(len=ii) :: arr(:plus-1), string,arr(plus:) ]
!!        endif
!!        ! show array
!!        write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     endif
!!     end subroutine update_dic
!!     end program demo_locate
!!
!!   Results:
!!
!!     for "b" index is            2           5
!!     for "[" index is           -4           5
!!    SIZE=5 xxx,b,aaa,[,ZZZ,
!!     for "c" index is           -2           6
!!    SIZE=6 xxx,c,b,aaa,[,ZZZ,
!!     for "ZZ" index is           -7           7
!!    SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!     for "ZZZZ" index is           -6           8
!!    SIZE=8 xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!     for "z" index is           -1           9
!!    SIZE=9 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine locate_c(list,value,place,ier,errmsg)

! ident_21="@(#) M_CLI2 locate_c(3) find PLACE in sorted character array LIST where VALUE can be found or should be placed"

character(len=*),intent(in)             :: value
integer,intent(out)                     :: place
character(len=:),allocatable            :: list(:)
integer,intent(out),optional            :: ier
character(len=*),intent(out),optional   :: errmsg
integer                                 :: i
character(len=:),allocatable            :: message
integer                                 :: arraysize
integer                                 :: maxtry
integer                                 :: imin, imax
integer                                 :: error
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   arraysize=size(list)

   error=0
   if(arraysize == 0)then
      maxtry=0
      place=-1
   else
      maxtry=nint(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value == list(PLACE))then
         exit LOOP
      elseif(value > list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin > imax)then
         place=-imin
         if(iabs(place) > arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place > arraysize.or.place <= 0)then
         message='*locate_* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate_* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   elseif(error /= 0)then
      write(warn,*)message//' VALUE=',trim(value)//' PLACE=',place
      call mystop(-24,'(*locate_c* '//message)
   endif
   if(present(errmsg))then
      errmsg=message
   endif
end subroutine locate_c
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    remove_(3) - [M_CLI2] remove entry from an allocatable array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine remove_(list,place)
!!
!!    character(len=:)|doubleprecision|real|integer,intent(inout) :: list(:)
!!    integer, intent(out) :: PLACE
!!
!!##DESCRIPTION
!!
!!    Remove a value from an allocatable array at the specified index.
!!    The array is assumed to be sorted in descending order. It may be of
!!    type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER.
!!
!!##OPTIONS
!!
!!    list    is the list array.
!!    PLACE   is the subscript for the entry that should be removed
!!
!!##EXAMPLES
!!
!!
!! Sample program
!!
!!     program demo_remove
!!     use M_sort, only : sort_shell
!!     use M_CLI2, only : locate_, remove_
!!     implicit none
!!     character(len=:),allocatable :: arr(:)
!!     integer                       :: i
!!     integer                       :: end
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'Z', 'aaa', 'b', 'b', 'ab', 'bb', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove_(arr,1)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove_(arr,4)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end program demo_remove
!!
!!   Results:
!!
!!    Expected output
!!
!!     SIZE=9 xxx,bb,b,b,ab,aaa,ZZZ,Z,,
!!     SIZE=8 bb,b,b,ab,aaa,ZZZ,Z,,
!!     SIZE=7 bb,b,b,aaa,ZZZ,Z,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine remove_c(list,place)

! ident_22="@(#) M_CLI2 remove_c(3fp) remove string from allocatable string array at specified position"

character(len=:),allocatable :: list(:)
integer,intent(in)           :: place
integer                      :: ii, end
   if(.not.allocated(list))then
      list=[character(len=2) :: ]
   endif
   ii=len(list)
   end=size(list)
   if(place <= 0.or.place > end)then                       ! index out of bounds of array
   elseif(place == end)then                                 ! remove from array
      list=[character(len=ii) :: list(:place-1) ]
   else
      list=[character(len=ii) :: list(:place-1), list(place+1:) ]
   endif
end subroutine remove_c
subroutine remove_l(list,place)

! ident_23="@(#) M_CLI2 remove_l(3fp) remove value from allocatable array at specified position"

logical,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(place <= 0.or.place > end)then                       ! index out of bounds of array
   elseif(place == end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif

end subroutine remove_l
subroutine remove_i(list,place)

! ident_24="@(#) M_CLI2 remove_i(3fp) remove value from allocatable array at specified position"
integer,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(place <= 0.or.place > end)then                       ! index out of bounds of array
   elseif(place == end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif

end subroutine remove_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    replace_(3) - [M_CLI2] replace entry in a string array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine replace_(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer, intent(out) :: place
!!
!!##DESCRIPTION
!!
!!    replace a value in an allocatable array at the specified index. Unless the
!!    array needs the string length to increase this is merely an assign of a value
!!    to an array element.
!!
!!    The array may be of type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER>
!!    It is assumed to be sorted in descending order without duplicate values.
!!
!!    The value and list must be of the same type.
!!
!!##OPTIONS
!!
!!    VALUE         the value to place in the array
!!    LIST          is the array.
!!    PLACE         is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!
!! Replace key-value pairs in a dictionary
!!
!!     program demo_replace
!!     use M_CLI2, only  : insert_, locate_, replace_
!!     ! Find if a key is in a list and insert it
!!     ! into the key list and value list if it is not present
!!     ! or replace the associated value if the key existed
!!     implicit none
!!     character(len=20)            :: key
!!     character(len=100)           :: val
!!     character(len=:),allocatable :: keywords(:)
!!     character(len=:),allocatable :: values(:)
!!     integer                      :: i
!!     integer                      :: place
!!     call update_dic('b','value of b')
!!     call update_dic('a','value of a')
!!     call update_dic('c','value of c')
!!     call update_dic('c','value of c again')
!!     call update_dic('d','value of d')
!!     call update_dic('a','value of a again')
!!     ! show array
!!     write(*,'(*(a,"==>",a,/))')(trim(keywords(i)),trim(values(i)),i=1,size(keywords))
!!
!!     call locate_key('a',place)
!!     if(place > 0)then
!!        write(*,*)'The value of "a" is',trim(values(place))
!!     else
!!        write(*,*)'"a" not found'
!!     endif
!!
!!     contains
!!     subroutine update_dic(key,val)
!!     character(len=*),intent(in)  :: key
!!     character(len=*),intent(in)  :: val
!!     integer                      :: place
!!
!!     ! find where string is or should be
!!     call locate_key(key,place)
!!     ! if string was not found insert it
!!     if(place < 1)then
!!        call insert_(keywords,key,abs(place))
!!        call insert_(values,val,abs(place))
!!     else ! replace
!!        call replace_(values,val,place)
!!     endif
!!
!!     end subroutine update_dic
!!    end program demo_replace
!!
!!   Expected output
!!
!!    d==>value of d
!!    c==>value of c again
!!    b==>value of b
!!    a==>value of a again
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine replace_c(list,value,place)

! ident_25="@(#) M_CLI2 replace_c(3fp) replace string in allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: tlen
integer                      :: end
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   tlen=len_trim(value)
   end=size(list)
   if(place < 0.or.place > end)then
           write(warn,*)'*replace_c* error: index out of range. end=',end,' index=',place
   elseif(len_trim(value) <= len(list))then
      list(place)=value
   else  ! increase length of variable
      ii=max(tlen,len(list))
      kludge=[character(len=ii) :: list ]
      list=kludge
      list(place)=value
   endif
end subroutine replace_c
subroutine replace_l(list,value,place)

! ident_26="@(#) M_CLI2 replace_l(3fp) place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(end == 0)then                                          ! empty array
      list=[value]
   elseif(place > 0.and.place <= end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(warn,*)'*replace_l* error: index out of range. end=',end,' index=',place
   endif
end subroutine replace_l
subroutine replace_i(list,value,place)

! ident_27="@(#) M_CLI2 replace_i(3fp) place value into allocatable array at specified position"

integer,intent(in)    :: value
integer,allocatable   :: list(:)
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(end == 0)then                                          ! empty array
      list=[value]
   elseif(place > 0.and.place <= end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(warn,*)'*replace_i* error: index out of range. end=',end,' index=',place
   endif
end subroutine replace_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    insert_(3) - [M_CLI2] insert entry into a string array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine insert_(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer,intent(in)    :: place
!!
!!##DESCRIPTION
!!
!!    Insert a value into an allocatable array at the specified index.
!!    The list and value must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL, or INTEGER)
!!
!!##OPTIONS
!!
!!    list    is the list array. Must be sorted in descending order.
!!    value   the value to place in the array
!!    PLACE   is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!
!! Find if a string is in a sorted array, and insert the string into
!! the list if it is not present ...
!!
!!     program demo_insert
!!     use M_sort, only : sort_shell
!!     use M_CLI2, only : locate_, insert_
!!     implicit none
!!     character(len=:),allocatable :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!     ! add or replace values
!!     call update_dic(arr,'b')
!!     call update_dic(arr,'[')
!!     call update_dic(arr,'c')
!!     call update_dic(arr,'ZZ')
!!     call update_dic(arr,'ZZZ')
!!     call update_dic(arr,'ZZZZ')
!!     call update_dic(arr,'')
!!     call update_dic(arr,'z')
!!
!!     contains
!!     subroutine update_dic(arr,string)
!!     character(len=:),allocatable :: arr(:)
!!     character(len=*)             :: string
!!     integer                      :: place, end
!!
!!     end=size(arr)
!!     ! find where string is or should be
!!     call locate_(arr,string,place)
!!     ! if string was not found insert it
!!     if(place < 1)then
!!        call insert_(arr,string,abs(place))
!!     endif
!!     ! show array
!!     end=size(arr)
!!     write(*,'("array is now SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end subroutine update_dic
!!     end program demo_insert
!!
!!   Results:
!!
!!     array is now SIZE=5 xxx,b,aaa,ZZZ,,
!!     array is now SIZE=6 xxx,b,aaa,[,ZZZ,,
!!     array is now SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!     array is now SIZE=8 xxx,c,b,aaa,[,ZZZ,ZZ,,
!!     array is now SIZE=9 xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!     array is now SIZE=10 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine insert_c(list,value,place)

! ident_28="@(#) M_CLI2 insert_c(3fp) place string into allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: end

   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif

   ii=max(len_trim(value),len(list),2)
   end=size(list)

   if(end == 0)then                                        ! empty array
      list=[character(len=ii) :: value ]
   elseif(place == 1)then                                  ! put in front of array
      kludge=[character(len=ii) :: value, list]
      list=kludge
   elseif(place > end)then                                 ! put at end of array
      kludge=[character(len=ii) :: list, value ]
      list=kludge
   elseif(place >= 2.and.place <= end)then                 ! put in middle of array
      kludge=[character(len=ii) :: list(:place-1), value,list(place:) ]
      list=kludge
   else                                                      ! index out of range
      write(warn,*)'*insert_c* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_c
subroutine insert_l(list,value,place)

! ident_29="@(#) M_CLI2 insert_l(3fp) place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(end == 0)then                                          ! empty array
      list=[value]
   elseif(place == 1)then                                    ! put in front of array
      list=[value, list]
   elseif(place > end)then                                   ! put at end of array
      list=[list, value ]
   elseif(place >= 2.and.place <= end)then                   ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(warn,*)'*insert_l* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_l
subroutine insert_i(list,value,place)

! ident_30="@(#) M_CLI2 insert_i(3fp) place value into allocatable array at specified position"

integer,allocatable   :: list(:)
integer,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(end == 0)then                                          ! empty array
      list=[value]
   elseif(place == 1)then                                    ! put in front of array
      list=[value, list]
   elseif(place > end)then                                   ! put at end of array
      list=[list, value ]
   elseif(place >= 2.and.place <= end)then                   ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(warn,*)'*insert_i* error: index out of range. end=',end,' index=',place,' value=',value
   endif

end subroutine insert_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine many_args(n0,g0, n1,g1, n2,g2, n3,g3, n4,g4, n5,g5, n6,g6, n7,g7, n8,g8, n9,g9, &
                   & na,ga, nb,gb, nc,gc, nd,gd, ne,ge, nf,gf, ng,gg, nh,gh, ni,gi, nj,gj )

! ident_31="@(#) M_CLI2 many_args(3fp) allow for multiple calls to get_args(3)"

character(len=*),intent(in)          :: n0, n1
character(len=*),intent(in),optional :: n2, n3, n4, n5, n6, n7, n8, n9, na, nb, nc, nd, ne, nf, ng, nh, ni, nj
class(*),intent(out)           :: g0, g1
class(*),intent(out),optional  :: g2, g3, g4, g5, g6, g7, g8, g9, ga, gb, gc, gd, ge, gf, gg, gh, gi, gj
   call get_generic(n0,g0)
   call get_generic(n1,g1)
   if( present(n2) .and. present(g2) )call get_generic(n2,g2)
   if( present(n3) .and. present(g3) )call get_generic(n3,g3)
   if( present(n4) .and. present(g4) )call get_generic(n4,g4)
   if( present(n5) .and. present(g5) )call get_generic(n5,g5)
   if( present(n6) .and. present(g6) )call get_generic(n6,g6)
   if( present(n7) .and. present(g7) )call get_generic(n7,g7)
   if( present(n8) .and. present(g8) )call get_generic(n8,g8)
   if( present(n9) .and. present(g9) )call get_generic(n9,g9)
   if( present(na) .and. present(ga) )call get_generic(na,ga)
   if( present(nb) .and. present(gb) )call get_generic(nb,gb)
   if( present(nc) .and. present(gc) )call get_generic(nc,gc)
   if( present(nd) .and. present(gd) )call get_generic(nd,gd)
   if( present(ne) .and. present(ge) )call get_generic(ne,ge)
   if( present(nf) .and. present(gf) )call get_generic(nf,gf)
   if( present(ng) .and. present(gg) )call get_generic(ng,gg)
   if( present(nh) .and. present(gh) )call get_generic(nh,gh)
   if( present(ni) .and. present(gi) )call get_generic(ni,gi)
   if( present(nj) .and. present(gj) )call get_generic(nj,gj)
contains
!===================================================================================================================================
function c(generic)
class(*),intent(in) :: generic
character(len=:),allocatable :: c
   select type(generic)
      type is (character(len=*)); c=trim(generic)
      class default
         c='unknown'
         stop 'get_many:: parameter name is not character'
   end select
end function c
!===================================================================================================================================
subroutine get_generic(name,generic)
use,intrinsic :: iso_fortran_env, only : real64
character(len=*),intent(in)  :: name
class(*),intent(out)         :: generic
   select type(generic)
      type is (integer);                        call get_args(name,generic)
      type is (real);                           call get_args(name,generic)
      type is (real(kind=real64));              call get_args(name,generic)
      type is (logical);                        call get_args(name,generic)
      !x!type is (character(len=:),allocatable ::);   call get_args(name,generic)
      type is (character(len=*));
      call get_args_fixed_length(name,generic)
      type is (complex);                        call get_args(name,generic)
      class default
         stop 'unknown type in *get_generic*'
   end select
end subroutine get_generic
!===================================================================================================================================
end subroutine many_args
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function iget(n); integer                      :: iget; character(len=*),intent(in) :: n; call get_args(n,iget); end function iget
function rget(n); real                         :: rget; character(len=*),intent(in) :: n; call get_args(n,rget); end function rget
function dget(n); real(kind=dp)                :: dget; character(len=*),intent(in) :: n; call get_args(n,dget); end function dget
function sget(n); character(len=:),allocatable :: sget; character(len=*),intent(in) :: n; call get_args(n,sget); end function sget
function cget(n); complex                      :: cget; character(len=*),intent(in) :: n; call get_args(n,cget); end function cget
function lget(n); logical                      :: lget; character(len=*),intent(in) :: n; call get_args(n,lget); end function lget

function igs(n); integer,allocatable          :: igs(:); character(len=*),intent(in) :: n; call get_args(n,igs); end function igs
function rgs(n); real,allocatable             :: rgs(:); character(len=*),intent(in) :: n; call get_args(n,rgs); end function rgs
function dgs(n); real(kind=dp),allocatable    :: dgs(:); character(len=*),intent(in) :: n; call get_args(n,dgs); end function dgs
function sgs(n,delims)
character(len=:),allocatable         :: sgs(:)
character(len=*),optional,intent(in) :: delims
character(len=*),intent(in)          :: n
   call get_args(n,sgs,delims)
end function sgs
function cgs(n); complex,allocatable          :: cgs(:); character(len=*),intent(in) :: n; call get_args(n,cgs); end function cgs
function lgs(n); logical,allocatable          :: lgs(:); character(len=*),intent(in) :: n; call get_args(n,lgs); end function lgs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function ig()
integer,allocatable :: ig(:)
integer             :: i, ierr
   if(allocated(ig))deallocate(ig)
   allocate(ig(size(unnamed)))
   do i=1,size(ig)
      call a2i(unnamed(i),ig(i),ierr)
   enddo
end function ig
!===================================================================================================================================
function rg()
real,allocatable :: rg(:)
   rg=real(dg())
end function rg
!===================================================================================================================================
function dg()
real(kind=dp),allocatable :: dg(:)
integer                   :: i
integer                   :: ierr
   if(allocated(dg))deallocate(dg)
   allocate(dg(size(unnamed)))
   do i=1,size(dg)
      call a2d(unnamed(i),dg(i),ierr)
   enddo
end function dg
!===================================================================================================================================
function lg()
logical,allocatable   :: lg(:)
integer               :: i
integer               :: iichar
character,allocatable :: hold
   if(allocated(lg))deallocate(lg)
   allocate(lg(size(unnamed)))
   do i=1,size(lg)
      hold=upper(clipends(unnamed(i)))
      if(hold(1:1) == '.')then                 ! looking for fortran logical syntax .STRING.
         iichar=2
      else
         iichar=1
      endif
      select case(hold(iichar:iichar))         ! check word to see if true or false
      case('T','Y',' '); lg(i)=.true.          ! anything starting with "T" or "Y" or a blank is TRUE (true,yes,...)
      case('F','N');     lg(i)=.false.         ! assume this is false or no
      case default
         call journal("*lg* bad logical expression for element",i,'=',hold)
      end select
   enddo
end function lg
!===================================================================================================================================
function cg()
complex,allocatable :: cg(:)
integer             :: i, ierr
real(kind=dp)       :: rc, ic
   if(allocated(cg))deallocate(cg)
   allocate(cg(size(unnamed)))
   do i=1,size(cg),2
      call a2d(unnamed(i),rc,ierr)
      call a2d(unnamed(i+1),ic,ierr)
      cg(i)=cmplx(rc,ic,kind=sp)
   enddo
end function cg
!===================================================================================================================================
! Does not work with gcc 5.3
!function sg()
!character(len=:),allocatable :: sg(:)
!   sg=unnamed
!end function sg

!===================================================================================================================================
function sg()
character(len=:),allocatable :: sg(:)
   if(allocated(sg))deallocate(sg)
   allocate(sg,source=unnamed)
end function sg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine mystop(sig,msg)
! negative signal means always stop program
! else do not stop and set G_STOP_MESSAGE if G_QUIET is true
! or
! print message and stop if G_QUIET is false
! the MSG is NOT for displaying except for internal errors when the program will be stopped.
! It is for returning a value when the stop is being ignored
!
integer,intent(in) :: sig
character(len=*),intent(in),optional :: msg
   if(sig < 0)then
      if(present(msg))call journal(msg)
      stop 1
   elseif(.not.G_QUIET)then
      stop
   else
      if(present(msg)) then
         G_STOP_MESSAGE=msg
      else
         G_STOP_MESSAGE=''
      endif
      G_STOP=sig
   endif
end subroutine mystop
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function atleast(line,length,pattern) result(strout)

! ident_32="@(#) M_strings atleast(3) return string padded to at least specified length"

character(len=*),intent(in)                :: line
integer,intent(in)                         :: length
character(len=*),intent(in),optional       :: pattern
character(len=max(length,len(trim(line)))) :: strout
if(present(pattern))then
   strout=line//repeat(pattern,len(strout)/len(pattern)+1)
else
   strout=line
endif
end function atleast
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function clipends(string) result(lopped)
! trim leading and trailings spaces from resulting string
character(len=*),intent(in)  :: string
character(len=:),allocatable :: lopped
integer                      :: ends(2)
   ends=verify( string, " ", [.false.,.true.] )
   if(ends(1) == 0)then
      lopped=""
   else
      lopped=string(ends(1):ends(2))
   endif
end function clipends
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine locate_key(keyname,place)

! ident_33="@(#) M_CLI2 locate_key(3) find PLACE in sorted character array where KEYNAME can be found or should be placed"

character(len=*),intent(in)             :: keyname
integer,intent(out)                     :: place
integer                                 :: ii
character(len=:),allocatable            :: keyword_local

   if(G_UNDERDASH)then
      keyword_local=trim(replace_str(keyname,'-','_'))
   else
      keyword_local=trim(keyname)
   endif

   if(G_NODASHUNDER)then
      keyword_local=replace_str(keyword_local,'-','')
      keyword_local=replace_str(keyword_local,'_','')
   endif

   if(G_IGNORELONGCASE.and.len_trim(keyword_local) > 1)keyword_local=lower(keyword_local)
   if(G_IGNOREALLCASE)keyword_local=lower(keyword_local)

   if(len(keyword_local) == 1)then
      !x!ii=findloc(shorts,keyword_local,dim=1)
      ii=maxloc([0,merge(1, 0, shorts == keyword_local)],dim=1)
      if(ii > 1)then
         place=ii-1
      else
         call locate_(keywords,keyword_local,place)
      endif
   else
      call locate_(keywords,keyword_local,place)
   endif

   if(G_DEBUG) write(*,gen)'<DEBUG>LOCATE_KEY:KEYNAME:',trim(keyname),':KEYWORD:',keyword_local

end subroutine locate_key
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    set_mode(3) - [ARGUMENTS:M_CLI2] turn on optional modes+
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine set_mode(key,mode)
!!
!!     character(len=*),intent(in) :: key
!!     logical,intent(in),optional :: mode
!!
!!##DESCRIPTION
!!     Allow optional behaviors.
!!
!!##OPTIONS
!!    KEY    name of option
!!
!!    The following values are allowed:
!!
!!    o  response_file - enable use of response file
!!
!!    o  auto_response_file - enable use of response file
!!       but also act as if @$0 was entered on the command line
!!       where $0 is the basename of the file being executed
!!
!!    o  ignorelongcase - ignore case in long key names. So the user
!!       does not have to remember if the option is --CurtMode or --curtmode
!!       or --curtMode .
!!
!!    o  ignoreallcase - ignore case in long and short key names.
!!       This is similar to Powershell, which is case-insensitive.
!!
!!    o  dashunder  - treat dash in keyword as an underscore.
!!       So the user should not have to remember if the option is
!!       --ignore_case or --ignore-case.
!!
!!    o  nodashunder  - ignore dash and underscore in keywords.
!!
!!    o  strict - allow Boolean keys to be bundled, but requires
!!       a single dash prefix be used for short key names and long names
!!       must be prefixed with two dashes.
!!
!!    o  lastonly  - when multiple keywords occur keep the rightmost
!!       value specified instead of appending the values together.
!!
!!    MODE   set to .true. to activate the optional mode.
!!           Set to .false. to deactivate the mode.
!!           It is .true. by default.
!!
!!##EXAMPLES
!!
!! Sample program:
!!
!!    program demo_set_mode
!!    use M_CLI2,  only : set_args, lget, set_mode
!!    implicit none
!!    character(len=*),parameter :: all='(*(g0))'
!!       !
!!       ! enable use of response files
!!       call set_mode('response_file')
!!       !
!!       ! Any dash in a keyword is treated as an underscore
!!       call set_mode('underdash')
!!       !
!!       ! The case of long keywords are ignored.
!!       ! Values and short names remain case-sensitive
!!       call set_mode('ignorelongcase')
!!       ! The case of short and long keywords are ignored
!!       call set_mode('ignoreallcase')
!!       !
!!       ! short single-character boolean keys may be bundled
!!       ! but it is required that a single dash is used for
!!       ! short keys and a double dash for long keywords.
!!       call set_mode('strict')
!!       !
!!       call set_args(' --switch_X:X F --switch-Y:Y F --ox:O F -t F -x F -o F')
!!       !
!!       ! show the results
!!       print all,'--switch_X or -X ... ',lget('switch_X')
!!       print all,'--switch_Y or -Y ... ',lget('switch_Y')
!!       print all,'--ox or -O       ... ',lget('ox')
!!       print all,'-o               ... ',lget('o')
!!       print all,'-x               ... ',lget('x')
!!       print all,'-t               ... ',lget('t')
!!    end program demo_set_mode
!!
!!##AUTHOR
!!      John S. Urban, 2019
!!##LICENSE
!!      Public Domain
!===================================================================================================================================
elemental impure subroutine set_mode(key,mode)
character(len=*),intent(in)   :: key
logical,intent(in),optional   :: mode
logical                       :: local_mode
character(len=:),allocatable  :: debug_mode

   debug_mode= upper(get_env('CLI_DEBUG_MODE','FALSE'))//' '
   select case(debug_mode(1:1))
   case('Y','T')
      G_DEBUG=.true.
   end select

   if(present(mode))then
      local_mode=mode
   else
      local_mode=.true.
   endif

   select case(lower(key))
   case('response_file','response file'); CLI_RESPONSE_FILE=local_mode
   case('auto_response_file','auto response file'); CLI_AUTO_RESPONSE_FILE=local_mode
   case('debug');                         G_DEBUG=local_mode
   case('ignorecase','ignorelongcase');   G_IGNORELONGCASE=local_mode
   case('ignoreallcase');   G_IGNOREALLCASE=local_mode
   case('underdash','dashunder');         G_UNDERDASH=local_mode
   case('nodashunder','nounderdash');     G_NODASHUNDER=local_mode
   case('strict');                        G_STRICT=local_mode
   case('lastonly');                      G_APPEND=.not.local_mode
   case default
      call journal('*set_mode* unknown key name ',key)
   end select

   if(G_DEBUG)write(*,gen)'<DEBUG>SET_MODE:END'

end subroutine set_mode
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine print_dictionary_usage()
   if(G_DEBUG)then
      call print_dictionary( str('response_file=', CLI_RESPONSE_FILE, &
                                &'ignorelongcase=', G_IGNORELONGCASE,&
                                &'ignoreallcase=', G_IGNOREALLCASE,&
                                &'underdash=', G_UNDERDASH,&
                                &'strict=', G_STRICT,&
                                &'lastonly=', G_APPEND,&
                                &'NODASHUNDER=', G_NODASHUNDER,&
                                &'debug=', G_DEBUG) )
   else
      call print_dictionary('USAGE:')
   endif
end subroutine print_dictionary_usage
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_CLI2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
