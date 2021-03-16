










!>
!!##NAME
!!    M_getopt_long(3fm) - [ARGUMENTS:M_getopt_long] parse command line options like Sun getopt_long, including the Sun CLIP specification
!!    (LICENSE:PD)
!!##SYNTAX
!!    use M_getopt_long, only : getopt_new, getopt
!!    use M_getopt_long, only : getopt_type, getopt_option_type
!!    use M_getopt_long, only : getopt_argv
!!
!!##DESCRIPTION
!!    This is based on SunOS getopt_long(3), and includes the Sun CLIP
!!    specification, which requires matching short and long versions of
!!    all options.
!!
!!    Precise getopt functionality is not really desirable. The biggest
!!    drawback of getopt is the use of globals. (It was designed a long
!!    time ago.). This interface uses OOP with a derived-type data object.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_M_getopt_long
!!      use M_getopt_long
!!      implicit none
!!      character(len=1) :: c
!!      integer :: i
!!      integer :: digit_optind = 0
!!      type(getopt_type), pointer :: opts
!!
!!      integer :: this_option_optind
!!      integer :: option_index
!!      character(len=1), parameter :: NIL = char(0)
!!
!!      type(getopt_option_type) :: long_options(6) = (/ &
!!          getopt_option_type("add",     1, NULL(), NIL), &
!!          getopt_option_type("append",  0, NULL(), NIL), &
!!          getopt_option_type("delete",  1, NULL(), NIL), &
!!          getopt_option_type("verbose", 0, NULL(), NIL), &
!!          getopt_option_type("create",  1, NULL(), 'c'), &
!!          getopt_option_type("file",    1, NULL(), NIL) /)
!!      character(len=*), parameter :: optstring = "abc:d:012"
!!
!!      call getopt_new(opts,optstring,long_options)
!!
!!      do
!!        this_option_optind = merge(opts%index,1,opts%index>0)
!!        option_index = 0
!!        c = getopt(opts,option_index)
!!        write(*,*)'retval=',c
!!        select case(c)
!!        case (GETOPT_STATUS_END)
!!          exit
!!        case (GETOPT_STATUS_NIL)
!!          write(*,'(2A)',advance='no') "option ", trim(long_options(option_index)%name)
!!          if (associated(opts%optarg)) &
!!            write(*,'(2A)',advance='no') "with arg ", opts%optarg
!!          write(*,*) !newline
!!
!!        case ('0','1','2')
!!          if (digit_optind /= 0 .and. digit_optind /= this_option_optind) &
!!            write(*,*) "digits occur in two different argv-elements."
!!          digit_optind = this_option_optind
!!          write(*,*)"option ",c
!!
!!        case ('a','b')
!!          write(*,*)"option ",c
!!
!!        case ('c','d')
!!          write(*,*)"option ",c," with value '",opts%optarg,'"'
!!
!!        case default
!!          write(*,*) "?? getopt returned character code ",ichar(c)," ??"
!!
!!        end select
!!
!!      end do
!!
!!      if (opts%index <= opts%argc) then
!!        write(*,'(A)',advance='no') "non-option ARGV-elements: "
!!        do i=opts%index,opts%argc
!!          write(*,'(A,1X)',advance='no') getopt_argv(opts,i)
!!        end do
!!        write(*,*) ! newline
!!      end if
!!
!!      stop
!!    end program demo_M_getopt_long
!!
!!##AUTHOR
!!     * [[getopt_long_module]] by [[Joe Krahn]].
!!     * slightly modified from original - JSU
!!##LICENSE
!!     Public Domain
!!
!!     Obtained from http://fortran.wiki:
!!
!!     When contributing code, please specify a license so that others
!!     know the extent to which they may use and modify your code. All code
!!     on the Fortran Wiki shall be in the public domain unless otherwise
!!     noted.
module M_getopt_long
public
integer, parameter :: max_option_len = 15

! return values
character(len=1), parameter :: GETOPT_STATUS_BADCH = '?'
character(len=1), parameter :: GETOPT_STATUS_BADARG = ':'
character(len=1), parameter :: GETOPT_STATUS_INORDER = char(1)
character(len=1), parameter :: GETOPT_STATUS_END = char(255)
character(len=1), parameter :: GETOPT_STATUS_NIL = char(0)

! long options argument flag:
integer, parameter :: GETOPT_NO_ARG = 0
integer, parameter :: GETOPT_REQ_ARG = 1
integer, parameter :: GETOPT_OPT_ARG = 2

! Behavior flags:
integer, parameter :: GETOPT_FLAG_PERMUTE             = int(z'01') ! Permute non-optstring to the end of argv
integer, parameter :: GETOPT_FLAG_ALLARGS             = int(z'02') ! Treat non-optstring as args to option INORDER
integer, parameter :: GETOPT_FLAG_LONGONLY            = int(z'04') ! Operate as getopt_long_only()
integer, parameter :: GETOPT_FLAG_OPTIONAL_ARGS       = int(z'08') ! Support optional arguments in optstring
integer, parameter :: GETOPT_FLAG_REQUIRE_EQUIVALENTS = int(z'10') ! Require short<->long equivalents
integer, parameter :: GETOPT_FLAG_ABBREV              = int(z'20') ! Support long option abbreviations
integer, parameter :: GETOPT_FLAG_W_SEMICOLON         = int(z'40') ! Support W; in optstring
integer, parameter :: GETOPT_FLAG_PLUS_DASH_START     = int(z'80') ! Support leading '+' or '-' in optstring

integer, parameter :: GETOPT_FLAGS_GNU = &
         GETOPT_FLAG_PERMUTE + GETOPT_FLAG_OPTIONAL_ARGS + GETOPT_FLAG_ABBREV + &
         GETOPT_FLAG_W_SEMICOLON + GETOPT_FLAG_PLUS_DASH_START

integer, parameter :: GETOPT_FLAGS_POSIX = &
         GETOPT_FLAG_ABBREV  !?  + GETOPT_FLAG_PLUS_DASH_START

integer, parameter :: GETOPT_FLAGS_DEFAULT = &
         GETOPT_FLAG_PERMUTE + GETOPT_FLAG_ABBREV + GETOPT_FLAG_PLUS_DASH_START

integer, parameter :: GETOPT_FLAGS_LONG = &
         GETOPT_FLAG_PERMUTE + GETOPT_FLAG_OPTIONAL_ARGS + GETOPT_FLAG_ABBREV + &
         GETOPT_FLAG_W_SEMICOLON + GETOPT_FLAG_PLUS_DASH_START

integer, parameter :: GETOPT_FLAGS_LONG_ONLY = &
         GETOPT_FLAGS_LONG + GETOPT_FLAG_LONGONLY

integer, parameter :: GETOPT_FLAGS_SUN_CLIP = &
         GETOPT_FLAG_W_SEMICOLON + GETOPT_FLAG_REQUIRE_EQUIVALENTS
! GETOPT_FLAGS_SUN_CLIP behaves as Sun's getopt_clip() --
!  Parse argc/argv argument vector, requiring compliance with
!   Sun's CLIP specification (11/12/02)
! o Does not allow arguments to be optional (optional_argument is
!   treated as required_argument).
! o Does not allow long optstring to be abbreviated on the command line
! o Does not allow long argument to be preceded by a single dash
!   (Double-dash '--' is required)
! o Stops option processing at the first non-option
! o Requires that every long option have a short-option (single
!   character) equivalent and vice-versa. If a short option or
!   long option without an equivalent is found, an error message
!   is printed and -1 is returned on the first call, and errno
!   is set to EINVAL.
! o Leading + or - in optstring is ignored, and opstring is
!   treated as if it began after the + or - .
!
! It does support the special "W;" in optstring
!-----------------------------------------------------------------------------------------------------------------------------------
type getopt_option_type
  character(len=max_option_len) :: name
  integer :: has_arg = GETOPT_NO_ARG
  character(len=1), pointer :: flag => NULL() ! Reference to user's flag variable
  character(len=1) :: val = char(0)
end type getopt_option_type

type getopt_string_type
  character(len=1), allocatable :: string(:)
end type getopt_string_type

type getopt_type
! private:
  character(len=1), pointer :: place(:) ! => empty_string_array
  integer :: nonopt_start != -1; ! first non option argument (for permute)
  integer :: nonopt_end != -1;   ! first option after non optstring (for permute)
  logical :: posixly_correct ! = .false.
  integer :: flags
! character(len=512), pointer :: optstring ! should be (len=*)
  character(len=:), pointer :: optstring ! should be (len=*)
  integer :: optstring_len
  type(getopt_option_type), pointer :: long_opts(:) ! reference to user's longopts array
! public:
  integer :: argc
  type(getopt_string_type), allocatable :: argv(:)
  integer :: index ! = 0
  logical :: error ! = .false.
  character(len=1), pointer :: optarg(:) ! => NULL()
  character(len=1) :: opt ! = char(0) == GETOPT_STATUS_NIL
end type getopt_type

character(len=1), target :: empty_string_array(0)
!-----------------------------------------------------------------------------------------------------------------------------------
interface warn
  module procedure warn_string, warn_char_array
end interface warn

private :: warn, warn_string, warn_char_array

private :: permute_args, parse_longopts

public test_suite_M_getopt_long
!================================================================================
contains

subroutine getopt_new(self, optstring, long_opts, flags, status)
  implicit none
  type(getopt_type), pointer :: self
  character(len=*), intent(in), target :: optstring
  type(getopt_option_type), intent(in), target, optional :: long_opts(:)
  integer, intent(in), optional  :: flags
  logical, intent(out), optional :: status

  intrinsic :: command_argument_count, &
               get_command_argument, &
               get_environment_variable
  integer :: i, arglen

  allocate(self)
  self%optarg => NULL()
  self%place => empty_string_array
  self%nonopt_start = -1
  self%nonopt_end = -1
  self%index = 1
  self%error = .false.
  self%opt = GETOPT_STATUS_NIL

  if (present(flags)) then
    self%flags = flags
  else if (present(long_opts)) then
    self%flags = GETOPT_FLAGS_LONG
  else
    self%flags = GETOPT_FLAGS_DEFAULT
  end if

! Disable GNU extensions if ENV{POSIXLY_CORRECT} is set.
! If optstring begins with a '+' or '-', and the option flags
! enable this feature, the caller chooses POSIX correctness.
  self%posixly_correct = ( i <= 0 )
  self%optstring => optstring(1:len(self%optstring))
  self%optstring_len=len(optstring)
  if (iand(self%flags,GETOPT_FLAG_PLUS_DASH_START)/=0) then
    select case(optstring(1:1))
    case('+')
      self%posixly_correct = .true.
      self%optstring => optstring(2:)
      self%optstring_len=len(optstring)-1
    case('-')
      self%posixly_correct = .false.
      self%optstring => optstring(2:)
      self%optstring_len=len(optstring)-1
      self%flags = ior(self%flags,GETOPT_FLAG_ALLARGS)
    case default
      self%posixly_correct = ( i <= 0 )
    end select
  else
  end if

  if (self%posixly_correct) then
    self%flags = iand(self%flags,not( &
          GETOPT_FLAG_PERMUTE + GETOPT_FLAG_ALLARGS + GETOPT_FLAG_OPTIONAL_ARGS ))
  end if
  self%optarg => NULL()

!-------------------------------------------------------------------------
! EXTENSION: Sun's CLIP specification (11/12/02)
! This option requires a matching long and short version for every option.
  if ((self%index == 1) .and. iand(self%flags,GETOPT_FLAG_REQUIRE_EQUIVALENTS)/=0) then
    if (.not. verify_short_long_equivalents()) then
      if (.not. present(status)) stop 'Error in getopt_long() arguments'
      deallocate(self)
      status=.false.
      return
    end if
  end if
!-------------------------------------------------------------------------

  self%argc=command_argument_count()
  allocate(self%argv(0:self%argc))
  do i=0,self%argc
    call get_command_argument(i,length=arglen)
    allocate(self%argv(i)%string(arglen))
    call get_command_argument(i,self%argv(i)%string(1)(1:arglen))
  end do
  if (present(status))then
     status=.true.
  endif
  return

contains
!-----------------------------------------------------------------------------------------------------------------------------------
! Verify that each short option (character flag) has a long equivalent,
! and that each long option has a short option equivalent. Note that
! multiple long optstring can map to the same character.
! This behavior is defined by Sun's CLIP specification (11/12/02).
!
! If error output is enabled and an error is found, this function
! prints ONE error message (the first error found) and returns an
! error value.
! ASSUMES: optstring is present and long_opts is optional
! Returns .TRUE. on success
  logical &
  function verify_short_long_equivalents() result(ok)
    implicit none
    !accessed by host association:
    !type(getopt_type), pointer :: self
    !character(len=*), intent(in) :: optstring
    !type(getopt_option_type), intent(in), optional :: long_opts(:)
    !integer, intent(in) :: flags

    integer :: i
    character(len=1) :: ch
    ch = GETOPT_STATUS_NIL

  ! Find a long option for each short option
    i=1
    do while ( ok .and. (i<=len(optstring)))
      ch = optstring(i:i)
      if (ch == ':') then
        i=i+1
        cycle
      end if

  ! 'W;' is a special case, if GETOPT_FLAG_W_SEMICOLON is set:
      if (iand(flags,GETOPT_FLAG_W_SEMICOLON)/=0 .and. &
        (ch == 'W') .and. (optstring(i+1:i+1) == ';')) then
        i=i+2
        cycle
      end if

      if (associated(self%long_opts)) then
        ok=any(ch==self%long_opts(:)%val)
      else
        ok=.false.
      end if
      if (.not.ok) then
        call warn(self,"equivalent long option required",ch)
        return
      end if

      i=i+1
    end do

  ! Find a short option for each long option.
    if (associated(self%long_opts)) then
      do i=1,size(self%long_opts)
        ok = (self%long_opts(i)%val /= GETOPT_STATUS_NIL .and. &
            index(optstring, self%long_opts(i)%val) > 0)
        if (.not. ok) then
          call warn(self,"equivalent short option required",self%long_opts(i)%name)
          return
        end if
      end do
    end if
    ok=.true.
    return
  end function verify_short_long_equivalents
end subroutine getopt_new
!-----------------------------------------------------------------------------------------------------------------------------------
! Public interface procedures:
!-----------------------------------------------------------------------------------------------------------------------------------
function getopt_argv(self,argn) result(argv)
implicit none
type(getopt_type), pointer                :: self
integer, intent(in)                       :: argn
character(len=getopt_argv_len(self,argn)) :: argv

  argv = self%argv(argn)%string(1)(1:len(argv))

end function getopt_argv
!-----------------------------------------------------------------------------------------------------------------------------------
! getopt() --
!  Parse argc/argv argument vector.  Called by user level routines.
! This implements all of the getopt variants, depending on flag bits.
character(len=1) &
function getopt(self, longindex) result(retval)
  implicit none
  type(getopt_type), pointer :: self
  integer, intent(out), optional :: longindex

  integer :: option_letter_index
  character(len=1) :: optchar
  logical :: short_too

  if (.not. allocated(self%argv)) then
    retval = GETOPT_STATUS_END
    return
  end if

  self%optarg => NULL()
  self%opt = GETOPT_STATUS_NIL
write(*,*)'index=',self%index
NEXT_ARG: do
    if (self%index==0 .or. size(self%place)==0) then    ! update scanning pointer
      if (self%index > ubound(self%argv,1)) then    ! end of argument vector
        self%place => empty_string_array
        if (self%nonopt_end /= -1) then
! do permutation, if we have to
          self%index = self%index - (self%nonopt_end - self%nonopt_start)

        else if (self%nonopt_start /= -1) then
! If we skipped non-optstring, set self%index
! to the first of them.
          self%index = self%nonopt_start
        end if
        self%nonopt_start = -1
        self%nonopt_end = -1
        retval = GETOPT_STATUS_END
        return
      end if
      self%place => self%argv(self%index)%string
      if (self%place(1) /= '-' .or. size(self%place,1)<2 ) then
        self%place => empty_string_array  ! found non-option
        write(*,*) 'found non-option at ',self%index
        if (iand(self%flags,GETOPT_FLAG_ALLARGS)/=0) then
! GNU extension:
! return non-option as argument to option char(1)
          self%index=self%index+1
          self%optarg => self%argv(self%index)%string
          retval = GETOPT_STATUS_INORDER
          return
        end if
        if (iand(self%flags,GETOPT_FLAG_PERMUTE)==0) then
! If no permutation wanted, stop parsing
! at first non-option.
          retval = GETOPT_STATUS_END
          return
        end if
! do permutation
        if (self%nonopt_start == -1) then
          self%nonopt_start = self%index
        else if (self%nonopt_end /= -1) then
          self%nonopt_start = self%index - (self%nonopt_end - self%nonopt_start)
          self%nonopt_end = -1
        end if
        self%index = self%index + 1
! process next argument
        cycle NEXT_ARG
      end if
      if (self%nonopt_start /= -1 .and. self%nonopt_end == -1) &
        self%nonopt_end = self%index

! Check for "--" or "--foo" with no long optstring
! but if self%place is simply "-" leave it unmolested.
      if (size(self%place)>1) then
        self%place=>self%place(2:)
        if (self%place(1) == '-' .and. &
           (size(self%place)==1 .or. .not. associated(self%long_opts))) then
          self%index = self%index + 1
          self%place => empty_string_array
! We found an option (--), so if we skipped
! non-optstring, we have to permute.
          if (self%nonopt_end /= -1) then
            self%index = self%index - (self%nonopt_end - self%nonopt_start)
          end if
          self%nonopt_start = -1
          self%nonopt_end = -1
          retval = GETOPT_STATUS_END
          return
        end if
      end if
    end if
    exit NEXT_ARG
  end do NEXT_ARG

! Check long optstring if:
!  1) we were passed some
!  2) the arg is not just "-"
!  3) either the arg starts with -- or we are getopt_long_only()
!if (self%long_opts /= NULL .and. self%place /= self%argv[self%index] && (*self%place == '-' .or. (GETOPT_FLAG_IS_SET(GETOPT_FLAG_LONGONLY)))) then

  if (associated(self%long_opts) .and. &
      (.not. associated(self%place,self%argv(self%index)%string)) .and. &
      (self%place(1) == '-' .or. iand(self%flags,GETOPT_FLAG_LONGONLY)/=0)) then
    short_too = .false.
    if (self%place(1) == '-') then
      self%place => self%place(2:)  ! --foo long option
    else if (self%place(1) /= ':' .and. index(self%optstring, self%place(1)) > 0) then
      short_too = .true.    ! could be short option too
    end if

    optchar = parse_longopts(self, longindex, short_too)
    if (optchar /= GETOPT_STATUS_END) then
      self%place => empty_string_array
      retval = optchar
      return
    end if
  end if

  optchar = self%place(1)
  self%place => self%place(2:)
  option_letter_index = index(self%optstring(1:self%optstring_len), optchar)
  if (optchar == ':' .or. option_letter_index==0) then
! If the user didn't specify '-' as an option,
! assume it means GETOPT_STATUS_END (-1) as POSIX specifies.
    if (optchar == '-') then
      retval = GETOPT_STATUS_END
      return
    end if
! option letter unknown or ':'
    if (size(self%place)==0) self%index=self%index+1
    if ((self%error) .and. self%optstring(1:1) /= ':') then
    end if
    self%opt = optchar
    retval = GETOPT_STATUS_BADCH
    return
  end if

! -W long-option
  if (iand(self%flags,GETOPT_FLAG_W_SEMICOLON)/=0 &
      .and. associated(self%long_opts) &
      .and. optchar == 'W' &
      .and. self%optstring(option_letter_index:option_letter_index) == ';') then
    if (size(self%place)>0) then
      self%index=self%index+1
      if (self%index > ubound(self%argv,1)) then  ! no long-option after -W
        self%place => empty_string_array
        if ((self%error) .and. self%optstring(1:1) /= ':') then
        end if
        self%opt = optchar
        retval = merge(GETOPT_STATUS_BADARG,GETOPT_STATUS_BADCH,self%optstring(1:1)==':')
        return
      else      ! white space
        self%place => self%argv(self%index)%string
      end if
    end if
    optchar = parse_longopts(self, longindex, .false.)

! PSARC 2003/645 - Match GNU behavior, set self%optarg to
! the long-option.
    if (.not. associated(self%optarg)) then
      self%optarg => self%argv(self%index-1)%string
    end if
    self%place => empty_string_array
    retval = optchar
    return
  end if

  option_letter_index=option_letter_index+1
  if (self%optstring(option_letter_index:option_letter_index) /= ':') then ! no ':' suffix: doesn't take argument
    if (size(self%place)==0) self%index=self%index+1
  else        ! ':' or '::' suffix: takes (optional) argument
    self%optarg => NULL()
    if (size(self%place)>0) then  ! arg value joined (no white space)
      self%optarg => self%place
! XXX: disable test for :: if PC? (GNU doesn't)
    else if (iand(self%flags,GETOPT_FLAG_OPTIONAL_ARGS)==0 .and. &
          self%optstring(option_letter_index+1:option_letter_index+1) == ':') then
! arg is required (not optional)
      self%index=self%index+1
      if (self%index > ubound(self%argv,1)) then  ! no arg
        self%place => empty_string_array
        if ((self%error) .and. self%optstring(1:1) /= ':') then
        end if
        self%opt = optchar
        retval = merge(GETOPT_STATUS_BADARG,GETOPT_STATUS_BADCH,self%optstring(1:1)==':')
        return
      else
        self%optarg => self%argv(self%index)%string
      end if
    end if
    self%place => empty_string_array
    self%index=self%index+1
  end if
! return valid option letter
  self%opt = optchar  ! preserve getopt() behavior
  retval = optchar
  return
end function getopt
!-----------------------------------------------------------------------------------------------------------------------------------
pure integer &
function getopt_argv_len(self,argn) result(len)
  implicit none
  type(getopt_type), pointer :: self
  integer, intent(in) :: argn
  if (.not. allocated(self%argv)) then
    len = 0
  else if ( (argn < 0) .or. (argn > ubound(self%argv,1)) ) then
    len = 0
  else
    len = size(self%argv(argn)%string)
  end if
end function getopt_argv_len
!-----------------------------------------------------------------------------------------------------------------------------------
! Private procedures:
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine warn_string(self, msg, arg)
  implicit none
  type(getopt_type), pointer :: self
  character(len=*), intent(in) :: msg, arg
  if (self%error .and. self%optstring(1:1) /= ':') then
    write(0,"(A,': ',A,' -- ',A)") &
        self%argv(0)%string(1)(1:ubound(self%argv(0)%string,1)), &
        msg, trim(arg)
  end if
end subroutine warn_string
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine warn_char_array(self, msg, arg)
  implicit none
  type(getopt_type), pointer :: self
  character(len=*), intent(in) :: msg
  character(len=1), intent(in) :: arg(:)
  if (self%error .and. self%optstring(1:1) /= ':') then
    write(0,"(A,': ',A,' -- ',100A1)") &
        self%argv(0)%string(1)(1:ubound(self%argv(0)%string,1)), &
        msg, arg
  end if
end subroutine warn_char_array
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine permute_args(self)
  implicit none
  type(getopt_type), pointer :: self

  integer :: cstart, cyclelen, i, j, ncycle, nnonopts, nopts, pos
  character(len=1), allocatable :: swap(:)

  if (.not. allocated(self%argv)) return

! compute lengths of blocks and number and size of cycles
  nnonopts = self%nonopt_end - self%nonopt_start
  nopts = self%index - self%nonopt_end
  ncycle = gcd(nnonopts, nopts)
  cyclelen = (self%index - self%nonopt_start) / ncycle

  do i=1,ncycle
    cstart = self%nonopt_end+i
    pos = cstart
    do j=0,cyclelen-1
      if (pos >= self%nonopt_end) then
        pos = pos - nnonopts
      else
        pos = pos + nopts
      end if

      allocate(swap(size(self%argv(pos)%string)))

      swap = self%argv(pos)%string
      deallocate(self%argv(pos)%string)
      allocate(self%argv(pos)%string(size(self%argv(cstart)%string)))
      self%argv(pos)%string = self%argv(cstart)%string

      deallocate(self%argv(cstart)%string)
      allocate(self%argv(cstart)%string(size(swap)))
      self%argv(cstart)%string = swap

      deallocate(swap)
    end do
  end do
contains
! Compute the greatest common divisor of a and b.
  integer function gcd(aval, bval)
    integer, intent(in) :: aval, bval
    integer :: a,b,c
    a = aval
    b = bval
    c = modulo(a,b)
    do while (c /= 0)
      a = b
      b = c
      c = modulo(a,b)
    end do
    gcd = b
  end function gcd
end subroutine permute_args
!-----------------------------------------------------------------------------------------------------------------------------------
! parse_longopts --
!  Parse long optstring in argc/argv argument vector.
! Returns -1 if short_too is set and the option does not match self%long_opts.
character(len=1) &
function parse_longopts(self, longindex, short_too) result(retval)
  implicit none
  type(getopt_type), pointer :: self
  integer, intent(inout), optional :: longindex
  logical, intent(in) :: short_too

  character(len=1), pointer :: current_argv(:)
  character(len=1), pointer :: argv_equal_ptr(:)
  integer :: current_argv_len
  integer :: long_option_len
  integer :: i
  integer :: match
  logical :: longopt_requires_arg
  integer :: argv_equal_index

  current_argv => NULL()
  current_argv_len = 0
  long_option_len = 0
  i = 0
  match = 0
  current_argv => self%place
  match = -1

  self%index=self%index+1

  argv_equal_index = index(current_argv(1)(1:size(current_argv)), '=')
  if (argv_equal_index>0) then
! argument found (--option=arg)
    current_argv_len = argv_equal_index - 1
    argv_equal_ptr => current_argv(argv_equal_index+1:)
  else
    current_argv_len = size(current_argv)
    argv_equal_ptr => NULL()
  end if

! find matching long option
  do i=1,size(self%long_opts)
    long_option_len = len_trim(self%long_opts(i)%name)
    if (long_option_len < current_argv_len) cycle
    if (current_argv(1)(1:current_argv_len) /= self%long_opts(i)%name(1:current_argv_len)) cycle

    if (iand(self%flags,GETOPT_FLAG_ABBREV)==0 .and. &
        long_option_len > current_argv_len) then
      cycle  ! Abbreviations are disabled
    end if

    if (long_option_len == current_argv_len) then
! exact match
      match = i
      exit
    end if

! If this is a known short option, don't allow
! a partial match of a single character.
    if (short_too .and. current_argv_len == 1) cycle

    if (match == -1) then ! first partial match
      match = i
    else ! ambiguous abbreviation
      if (self%error .and. self%optstring(1:1) /= ':') then
      end if
      self%opt = GETOPT_STATUS_NIL
      retval = GETOPT_STATUS_BADCH
      return
    end if
  end do

  if (match /= -1) then    ! option found
    if (self%long_opts(match)%has_arg == GETOPT_NO_ARG .and. &
        associated(argv_equal_ptr)) then
      if (self%error .and. self%optstring(1:1) /= ':') then
      end if
! XXX: GNU sets self%opt to val regardless of flag
      if (.not. associated(self%long_opts(match)%flag)) then
        self%opt = self%long_opts(match)%val
      else
        self%opt = GETOPT_STATUS_NIL
      end if
      retval = merge(GETOPT_STATUS_BADARG,GETOPT_STATUS_BADCH,self%optstring(1:1)==':')
      return
    end if

    longopt_requires_arg = &
        ( self%long_opts(match)%has_arg == GETOPT_OPT_ARG &
        .and.  iand(self%flags,GETOPT_FLAG_OPTIONAL_ARGS) == 0 ) &
        .or. self%long_opts(match)%has_arg == GETOPT_REQ_ARG

    if (self%long_opts(match)%has_arg == GETOPT_REQ_ARG .or. &
        self%long_opts(match)%has_arg == GETOPT_OPT_ARG) then
      if (associated(argv_equal_ptr)) then
        self%optarg => argv_equal_ptr
      else if (longopt_requires_arg) then
! The next argv must be the option argument
        if (self%index <= ubound(self%argv,1)) then
          self%optarg => self%argv(self%index)%string
        end if
        self%index=self%index+1 ! code below depends on this
      end if
    end if

    if (longopt_requires_arg .and. .not. associated(self%optarg)) then
! Missing argument; leading ':' indicates no error
! should be generated.
      if ((self%error) .and. (self%optstring(1:1) /= ':')) then
      end if
! XXX: GNU sets self%opt to val regardless of flag
      if (.not. associated(self%long_opts(match)%flag)) then
        self%opt = self%long_opts(match)%val
      else
        self%opt = GETOPT_STATUS_NIL
      end if
      self%index=self%index-1
      retval = merge(GETOPT_STATUS_BADARG,GETOPT_STATUS_BADCH,self%optstring(1:1)==':')
      return
    end if

  else     ! match==-1; unknown option
    if (short_too) then
      self%index=self%index-1
      retval = GETOPT_STATUS_END
      return
    end if
    if ((self%error) .and. (self%optstring(1:1) /= ':')) then
    end if
    self%opt = GETOPT_STATUS_NIL
    retval = GETOPT_STATUS_BADCH
    return
  end if

  if (present(longindex)) longindex = match
  if (associated(self%long_opts(match)%flag)) then
    self%long_opts(match)%flag = self%long_opts(match)%val
    retval = GETOPT_STATUS_NIL
    return
  else
    self%opt = self%long_opts(match)%val
    retval = self%opt
  end if
end function parse_longopts
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_getopt_long()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level

!! setup
   call test_getopt()
   call test_getopt_argv()
   call test_getopt_argv_len()
   call test_getopt_new()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getopt()

   call unit_check_start('getopt',msg='')
   !!call unit_check('getopt', 0.eq.0, 'checking',100)
   call unit_check_done('getopt',msg='')
end subroutine test_getopt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getopt_argv()

   call unit_check_start('getopt_argv',msg='')
   !!call unit_check('getopt_argv', 0.eq.0, 'checking',100)
   call unit_check_done('getopt_argv',msg='')
end subroutine test_getopt_argv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getopt_argv_len()

   call unit_check_start('getopt_argv_len',msg='')
   !!call unit_check('getopt_argv_len', 0.eq.0, 'checking',100)
   call unit_check_done('getopt_argv_len',msg='')
end subroutine test_getopt_argv_len
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getopt_new()

   call unit_check_start('getopt_new',msg='')
   !!call unit_check('getopt_new', 0.eq.0, 'checking',100)
   call unit_check_done('getopt_new',msg='')
end subroutine test_getopt_new
!===================================================================================================================================
end subroutine test_suite_M_getopt_long
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_getopt_long
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
