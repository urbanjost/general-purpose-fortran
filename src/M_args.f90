!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    M_args(3fm) - [ARGUMENTS::M_args::INTRO] - define a NAMELIST in a module template to provide command line argument parsing
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!  Usage:
!!
!!      use M_args, only : get_namelist, print_dictionary, unnamed
!!      use M_args, only : get_command_arguments_as_raw_namelist
!!      use M_args, only : get_command_arguments_stack
!!      use M_args, only : get_command_arguments_string
!!      use M_args, only : longest_command_argument
!!      use M_args, only : debug
!!      use M_args, only : oneline
!!
!!##DESCRIPTION
!!    Use the M_arguments(3fp) module template in the following example
!!    program to allow for command line parsing much like standard
!!    Unix command line parsing. Just change the variables defined in
!!    the NAMELIST. There are further details in the documentation for
!!    get_namelist(3f) and print_dictionary(3f), but for basic use starting
!!    with the example program should be sufficient.
!!
!!    Then, your program can be called with forms like:
!!
!!     cmd -x 1.0 -y -20 --points 1,2,3 -title 'This is my title'
!!     cmd --help *.data
!!
!!    A variable of the form LETTER_ becomes the uppercase keyword -LETTER,
!!    and negative values do not need quoted as values. Single-letter keywords
!!    are assumed to be used on the command line as short options with a single
!!    dash prefix, while multi-letter keywords are assumed to be long options.
!!    variable names may be equivalenced to allow for short and long versions of
!!    a keyword.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    !program demo_M_args
!!    module M_arguments
!!    use M_args,    only : get_namelist, print_dictionary, unnamed, oneline
!!
!!    ! >>> CHANGE THIS
!!    ! declare and initialize a namelist. Letter_ denotes an uppercase short command keyword
!!    real              :: x=111.1, y=222.2, z=333.3
!!    real              :: point(3)=[10.0,20.0,30.0]
!!    character(len=80) :: title=" "
!!    logical           :: l=.false., l_=.false.
!!    logical           :: help=.false., version=.false., v=.false., h=.false.
!!    equivalence       (help,h),(version,v)
!!    namelist /args/ x,y,z,point,title,help,h,version,v,l,l_
!!    ! << END OF CHANGES
!!
!!    contains
!!       subroutine get_args()
!!       integer :: ios
!!       character(len=255) :: message ! use for I/O error messages
!!       character(len=:),allocatable :: readme  ! stores updated namelist
!!       character(len=10000) :: hold_namelist(60)
!!          hold_namelist=''
!!          write(hold_namelist,nml=args,iostat=ios,iomsg=message)
!!          if(ios.eq.0)then
!!             readme=get_namelist(oneline(hold_namelist))
!!             read(readme,nml=args,iostat=ios,iomsg=message)
!!          endif
!!          if(ios.ne.0)then
!!             write(*,'("ERROR:",i0,1x,a)')ios, trim(message)
!!             call print_dictionary()
!!             stop 1
!!          endif
!!       end subroutine get_args
!!    end module M_arguments
!!
!!    program short
!!    use M_arguments, only : get_args, unnamed
!!    use M_arguments  ! make user variables available
!!    implicit none
!!    integer :: i
!!       call get_args()  ! crack command line options
!!       ! >> USER YOUR VARIABLES HERE. FOR EXAMPLE:
!!       write(*,*)'VALUES ARE NOW ', new_line('A'),&
!!       &'x        ',x,              new_line('A'),&
!!       &'y        ',y,              new_line('A'),&
!!       &'z        ',z,              new_line('A'),&
!!       &'point    ',point,          new_line('A'),&
!!       &'title    ',title,          new_line('A'),&
!!       &'help     ',help,'h ',h,    new_line('A'),&
!!       &'version  ',version,'v ',v, new_line('A'),&
!!       &'l        ',l,              new_line('A'),&
!!       &'l_       ',l_
!!       if(size(unnamed).gt.0)then
!!          write(*,'(a)')'UNNAMED:'
!!          write(*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
!!       endif
!!       !<< END OF EXAMPLE USAGE OF VARIABLES
!!    end program short
!!    !end program demo_M_args
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    Public Domain
module M_args
use M_framework__journal, only : journal
use M_list,    only : insert, locate, replace, remove
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,stdin=>INPUT_UNIT    ! access computing environment
use M_strings, only : isupper, lower, quote, upper
private
!===================================================================================================================================
public  :: get_command_arguments_stack
public  :: get_command_arguments_string
public  :: longest_command_argument
public  :: get_namelist
public  :: print_dictionary
public  :: oneline
public debug
public unnamed

public :: get_command_arguments_as_raw_namelist

private :: namelist_to_dictionary
private :: prototype_and_cmd_args_to_nlist
private :: prototype_to_dictionary
private :: update
private :: get
private :: wipe_dictionary

type option
   character(:),allocatable :: shortname
   character(:),allocatable :: longname
   character(:),allocatable :: value
   integer                  :: length
   logical                  :: present_in
end type option
!===================================================================================================================================
character(len=:),allocatable   :: keywords(:)
character(len=:),allocatable   :: values(:)
integer,allocatable            :: counts(:)
logical,allocatable            :: present_in(:)

logical                        :: keyword_single=.true.
character(len=:),allocatable   :: passed_in
character(len=:),allocatable   :: namelist_name

character(len=:),allocatable   :: unnamed(:)
logical                        :: debug=.false.
logical                        :: return_all

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    get_command_arguments_stack(3f) - [ARGUMENTS:M_args] return a character array containing all the command line arguments
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function get_command_arguments(stack) result (args)
!!
!!     character(len=:),allocatable :: args(:)
!!
!!##DESCRIPTION
!!    Return a character array containing all the command arguments.
!!    For cases where it is difficult to process the command arguments
!!    one at a time, this function returns an array of the command line
!!    arguments
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_get_command_arguments_stack
!!    use M_args,    only : get_command_arguments_stack
!!    implicit none
!!    character(len=:),allocatable :: myargs(:)
!!    integer                      :: i
!!    myargs=get_command_arguments_stack()
!!    write(*,'(i0,t10,a)')(i,myargs(i),i=1,size(myargs))
!!    write(*,*)'longest argument is ',len(myargs)
!!    write(*,*)'number of arguments is ',size(myargs)
!!    end program demo_get_command_arguments_stack
!!
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    Public Domain
function get_command_arguments_stack() result(args)
character(len=:),allocatable :: args(:)
integer :: ilength, ilongest, iargs, istatus, i
ilength=0
ilongest=1 ! get an error if try to get string of zero length in gfortran 7.0.4 so set to 1 instead of 0
iargs=command_argument_count()
   GET_LONGEST: do i=1,iargs                                                ! look at all arguments
      call get_command_argument(number=i,length=ilength,status=istatus)     ! get next argument
      if(istatus /= 0) then                                                 ! stop program on error
         call journal('sc','*get_command_arguments_stack* error obtaining argument ',i)
         exit GET_LONGEST
      elseif(ilength.gt.0)then
         ilongest=max(ilongest,ilength)
      endif
   enddo GET_LONGEST
   allocate(character(len=ilongest) :: args(iargs))
   args(:)=''
   GET_ARGS: do i=1,command_argument_count()                                             ! copy array of arguments
      call get_command_argument(number=i,value=args(i),length=ilength,status=istatus)    ! get next argument
      if(istatus /= 0) then                                                              ! stop program on error
         call journal('sc','*get_command_arguments_stack* error obtaining argument ',i)
         exit GET_ARGS
      endif
   enddo GET_ARGS
end function get_command_arguments_stack
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    get_command_arguments_string(3f) - [ARGUMENTS:M_args] return all command arguments as an allocated string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine get_command_arguments_string(string,istatus)
!!
!!    character(len=:),allocatable,intent(out) :: string
!!    integer,intent(out)                      :: istatus
!!##DESCRIPTION
!!    Returns the entire command line sans the command verb
!!
!!##RETURNS
!!    STRING  composed of all command arguments concatenated into a string
!!    ISTATUS status (non-zero means error)
!!
!!##EXAMPLE
!!
!!   Sample usage
!!
!!    program demo_get_command_arguments_string
!!    use M_framework__journal, only : journal
!!    use M_args, only : get_command_arguments_string
!!    implicit none
!!    integer :: ier
!!    character(len=:),allocatable :: cmd
!!    call get_command_arguments_string(cmd,ier)
!!    write(*,*)'CMD=',trim(cmd)
!!    write(*,*)'LEN(CMD)=',len(cmd)
!!    write(*,*)'IER=',ier
!!    end program demo_get_command_arguments_string
!!##SEE ALSO
!!    M_kracken, kracken
!!
!!    dget,dgets,iget,igets,lget,lgets,rget,rgets,sget,sgets,retrev
!!
!!    parse,dissect,store,setprompts,show
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    Public Domain
subroutine get_command_arguments_string(string,istatus)

! ident_1="@(#) M_args get_command_arguments_string(3f) return all command arguments as an allocated string"

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
     STOP "*get_command_arguments_string* error: could not retrieve command line"
   elseif (max_string_len == 0) then
     STOP "*get_command_arguments_string* error: could not determine command length"
   endif
   max_string_len=max_string_len+2*command_argument_count()   ! leave room for adding double quotes to each argument
!-----------------------------------------------------------------------------------------------------------------------------------
   allocate(character(len=max_string_len) :: value)           ! no single argument should be longer than entire command length
   istatus=0                                                  ! initialize returned error code
   string=""                                                  ! initialize returned output string
!-----------------------------------------------------------------------------------------------------------------------------------
   APPEND_ARGS: do i=1,command_argument_count()               ! append any arguments together
      call get_command_argument(i,value,ilength,istatus)      ! get next argument
      if(istatus /= 0) then                                   ! stop program on error
         call journal('sc','*get_command_arguments_string* error obtaining argument ',i)
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
      call journal('*get_command_arguments_string *'//trim(deallocate_error_message))
   endif
end subroutine get_command_arguments_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    get_namelist(3f) - [ARGUMENTS:M_args] NAMELIST-based command line argument parsing
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function get_namelist(definition,all) result(string)
!!
!!    character(len=*),intent(in),optional  :: definition
!!    logical,intent(in),optional  :: all
!!    character(len=:),allocatable :: string
!!
!!##DESCRIPTION
!!    This routine leverages NAMELIST groups to do the conversion from strings
!!    to numeric values required by other command line parsers.
!!
!!    Several of the following example programs simply need an initialized
!!    variable added to the NAMELIST and it automatically is available as
!!    a command line argument. Hard to imagine it getting much simpler.
!!
!!    To use the routine define a NAMELIST group called ARGS.
!!
!!    The routine provides three modes
!!
!!    o keyword=value(s) pairs on the command line.
!!
!!      Typical program usage:
!!
!!       cmd x=1 point=-1,-2,-3 help=T
!!
!!      This requires nothing but a call to the get_namelist(3f) procedure
!!      with no arguments and is very suitable if you just need to pass in
!!      a few numeric values. the syntax used on the command line is the
!!      syntax required for a NAMELIST input string which is very good for
!!      numeric values but does not follow the common syntax rules found
!!      in routines like getopts(3c) or IEEE Std 1003.1-2001, for example.
!!
!!    o Unix-like command usage when provided a NAMELIST group string.
!!
!!      Typical program usage:
!!
!!       cmd -x 1 --point -1,-2,-3 --title 'my string' --help file1 file2
!!
!!      You can use an internal write to generate the input string (which
!!      means to add a new parameter you need to do nothing but initialize
!!      the variable and add the name to the ARGS NAMELIST group and it
!!      automatically becomes a new command line argument).
!!
!!    o Unix-like command usage when provided a Unix-like prototype.
!!
!!      Typical program usage:
!!
!!       cmd -x 1 --point -1,-2,-3 --title 'my string' --help file1 file2
!!
!!      If you are not familiar with NAMELIST input and output you can
!!      declare all the members of the namelist and their default values
!!      much like you were calling the program with a command prototype
!!      string.
!!
!!    For all three modes there is no need to convert from strings to numeric
!!    values in the source code. Even arrays and user-defined types can be
!!    used, complex values can be input ... just define the variable and
!!    add it to the NAMELIST definition.
!!
!!    Note that since all the arguments are defined in a NAMELIST group
!!    that config files can easily be used for the same options.
!!    Just create a NAMELIST input file and read it.
!!
!!    NAMELIST syntax can vary between different programming environments.
!!    Currently, this routine has only been tested using gfortran 7.0.4;
!!    and requires at least Fortran 2003.
!!
!!    NO DEFINITION
!!
!!    If the routine is called with no definition string arguments are passed
!!    in on the command line using NAMELIST syntax (ie. KEYWORD=VALUE). This
!!    is particularly suited for passing a few numeric values.
!!
!!    For example:
!!
!!     program nooptions
!!     use M_args, only : get_namelist
!!     implicit none
!!     character(len=255)           :: message ! use for I/O error messages
!!     character(len=:),allocatable :: readme  ! stores command line
!!     integer                      :: ios     ! I/O error number
!!
!!     ! declare and initialize a namelist that defines all
!!     ! the command keywords
!!     integer    :: i=1, j=2, k=3
!!     real       :: s=111.1, t=222.2, r=333.3
!!     real       :: point(3)=[10.0,20.0,30.0]
!!     logical    :: help=.false.,version=.false.
!!
!!     ! just add a variable here and it is a new parameter
!!     namelist /args/ i,j,k,s,t,r,point,help,version
!!
!!        ! return command line arguments as NAMELIST input
!!        readme=get_namelist()
!!        ! internal read of namelist
!!        read(readme,nml=args,iostat=ios,iomsg=message)
!!        if(ios.ne.0)then
!!           write(*,'("ERROR:",i0,1x,a)')ios, trim(message)
!!           write(*,*)'OPTIONS:'
!!           write(*,nml=args)
!!           stop 1
!!        endif
!!        ! all done cracking the command line
!!
!!        ! use the values in your program.
!!        write(*,nml=args)
!!     end program nooptions
!!
!!    You can call the example program with syntax like:
!!
!!       cmd  r=200e3 i=200
!!       cmd  K=33333,J=22222,I=11111
!!       cmd  point = 1, 2, 3 s= -3.0e4 t = 405.5
!!
!!    If you do pass in strings nested quotes or escaped double-quote
!!    characters are typically required. How to do that can vary with what
!!    shell and OS you are running in. Typically the following will work ...
!!
!!       # just quote the entire argument list with single quotes ...
!!       cmd 'c="my character string" S=10,T=20.30,R=3e-2'
!!
!!       # or nest the quotes ...
!!       cmd c='"string"' S=20.30
!!
!!       # or escape the quotes ...
!!       cmd c=\"string\"
!!
!!    PASS IN A NAMELIST STRING
!!
!!    If you want to pass in options using syntax similar to that provided
!!    by the C getopts(3c) procedure pass in a NAMELIST string. Typically,
!!    you would generate the input string by writing the NAMELIST group to
!!    an internal file.
!!
!!    The following program can be called using commands like
!!
!!      cmd -A 'string Value' -l -V --help -p 3.4,5.6 -- *
!!
!!    Typical program skeleton:
!!
!!     program demo_get_namelist
!!     use M_args,  only : unnamed
!!     implicit none
!!     integer :: i
!!
!!     ! declare and initialize a namelist
!!     ! letter_ denotes an uppercase short command keyword
!!     ! all values should be allocated before calling get_args(3f)
!!     real              :: x=111.1, y=222.2, z=333.3
!!     real              :: point(3)=[10.0,20.0,30.0]
!!     character(len=80) :: title=" "
!!     logical           :: help=.false., version=.false.
!!     logical           :: l=.false., l_=.false., v=.false., h=.false.
!!     ! you can equivalence short and long options
!!     equivalence       (help,h),(version,v)
!!     ! just add a variable here and it is a new parameter !!
!!     namelist /args/ x,y,z,point,title,help,h,version,v,l,l_
!!     !
!!        call get_args()  ! crack command line options
!!        ! do stuff with your variables
!!        write(*,*)'VALUES ARE NOW'
!!        write(*,nml=args)
!!        if(size(unnamed).gt.0)then
!!           write(*,'(a)')'UNNAMED:'
!!           write(*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
!!        endif
!!     contains
!!     subroutine get_args()
!!     ! The NAMELIST cannot be passed as an option to a routine so this
!!     ! routine must be in a contained routine or directly in the body of
!!     ! the routine that declares the NAMELIST. get_args(3f) should not
!!     ! need changed except for possibly the length of HOLD_NAMELIST
!!     use M_args,    only : get_namelist, print_dictionary, oneline
!!     !
!!     integer :: ios
!!     character(len=255) :: message ! use for I/O error messages
!!     character(len=:),allocatable :: readme  ! stores updated namelist
!!     ! make big enough for all of namelist
!!     character(len=10000) :: hold_namelist(60)
!!     ! the routine needs a copy of the options to determine what values
!!     ! are character and logical versus numeric
!!        write(hold_namelist,nml=args,iostat=ios,iomsg=message)
!!        if(ios.eq.0)then
!!           ! pass in the namelist and get an updated copy that includes
!!           ! values specified on the command line
!!           readme=get_namelist(oneline(hold_namelist))
!!           ! read the updated namelist to update the values
!!           ! in the namelist group
!!           read(readme,nml=args,iostat=ios,iomsg=message)
!!        endif
!!        if(ios.ne.0)then
!!           write(*,'("ERROR:",i0,1x,a)')ios, trim(message)
!!           call print_dictionary()
!!           stop 1
!!        endif
!!        ! all done cracking the command line
!!     end subroutine get_args
!!     end program demo_get_namelist
!!
!!    Instead of writing the NAMELIST group into a string you can compose
!!    the string yourself. only defined names will be able to be specified
!!    on the command line. For example:
!!
!!     call get_namelist('&ARGS A_="A value",B_=" ",C_=11 22 33, help=F/')
!!
!!   Sample with manual definition of NAMELIST string
!!
!!     program show_get_namelist_manual
!!     use M_args,  only : unnamed, get_namelist, print_dictionary
!!     implicit none
!!     integer            :: i, ios
!!     character(len=255) :: message
!!     ! define namelist
!!     real               :: x, y, z
!!     logical            :: help, h, version, v
!!     namelist /args/ x,y,z,help,h,version,v
!!     ! equivalence short and long version and help options
!!     equivalence           (help,h),(version,v)
!!     ! define NAMELIST string that defines all NAMELIST
!!     ! group variables
!!     character(len=:),allocatable :: cmd
!!        cmd='&ARGS X=1 Y=2 Z=3 HELP=F H=F VERSION=F V=F/'
!!        ! initialize all values in NAMELIST by reading string
!!        read(cmd,nml=args,iostat=ios,iomsg=message)
!!        if(ios.eq.0)then
!!           ! reduce NAMELIST string to just values on command line
!!           cmd=get_namelist(cmd)
!!           ! update NAMELIST group with values from command line
!!           read(cmd,nml=args,iostat=ios,iomsg=message)
!!        endif
!!        if(ios.ne.0)then
!!           call print_dictionary('ERROR: '//message)
!!           stop 1
!!        endif
!!        ! all done. use values in program
!!        write(*,nml=args)
!!        if(size(unnamed).gt.0)then
!!           write(*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
!!        endif
!!     end program show_get_namelist_manual
!!
!!    UNIX PROTOTYPE
!!
!!    Instead of passing in a NAMELIST string a Unix-like command prototype
!!    string can be used. Something like:
!!
!!       call get_namelist('-A " " -l -x -30.34e2 --help -version ')
!!
!!    typical usage:
!!
!!       program show_get_namelist_unix_prototype
!!          use M_args,  only : unnamed, get_namelist, print_dictionary
!!          implicit none
!!          integer                      :: i
!!          character(len=255) :: message ! use for I/O error messages
!!          character(len=:),allocatable :: readme ! stores updated namelist
!!          integer                      :: ios
!!
!!       ! declare a namelist
!!          real               :: x, y, z, point(3)
!!          character(len=80)  :: title
!!          logical            :: help, version, l, l_, v, h
!!          equivalence       (help,h),(version,v)
!!          namelist /args/ x,y,z,point,title,help,h,version,v,l,l_
!!
!!       ! Define the prototype
!!       !  o All parameters must be listed with a default value
!!       !  o string values  must be double-quoted
!!       !  o numeric lists must be comma-delimited. No spaces are allowed
!!          character(len=*),parameter  :: cmd='&
!!          & -x 1 -y 2 -z 3     &
!!          & --point -1,-2,-3   &
!!          & --title "my title" &
!!          & -h --help          &
!!          & -v --version       &
!!          & -l -L'
!!          ! reading in a NAMELIST definition defining the entire NAMELIST
!!          readme=get_namelist(cmd,all=.true.)
!!          read(readme,nml=args,iostat=ios,iomsg=message)
!!          if(ios.ne.0)then
!!             write(*,'("ERROR:",i0,1x,a)')ios, trim(message)
!!             call print_dictionary('OPTIONS:')
!!             stop 1
!!          endif
!!          ! all done cracking the command line
!!
!!          ! use the values in your program.
!!          write(*,nml=args)
!!          ! the optional unnamed values on the command line are
!!          ! accumulated in the character array "UNNAMED"
!!          if(size(unnamed).gt.0)then
!!             write(*,'(a)')'files:'
!!             write(*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
!!          endif
!!       end program show_get_namelist_unix_prototype
!!
!!##OPTIONS
!!    DESCRIPTION   null or composed of all command arguments concatenated
!!                  into a string prepared for reading as a NAMELIST group
!!                  or a Unix-line command prototype string.
!!
!!                  When creating a Unix-like prototype
!!
!!                  o all values except logicals get a value.
!!                  o long names (--keyword) should be all lowercase
!!                  o short names (-letter) that are uppercase map to a
!!                    NAMELIST variable called "letter_", but lowercase
!!                    short names map to NAMELIST name "letter".
!!                  o strings MUST be delimited with double-quotes and
!!                    must be at least one space and internal
!!                    double-quotes are represented with two double-quotes
!!                  o lists of numbers should be comma-delimited.
!!                     No spaces are allowed in lists of numbers.
!!                  o the values follow the rules for NAMELIST values, so
!!                    "-p 2*0" for example would define two values.
!!
!!    ALL           By default the output NAMELIST string only contains
!!                  keywords and values for names that were specified on
!!                  the command line. If ALL is .TRUE. a full NAMELIST
!!                  string is returned containing all the variables from
!!                  the input string.
!!##RETURNS
!!    STRING   The output is a NAMELIST string than can be read to update
!!             the NAMELIST "ARGS" with the keywords that were supplied on
!!             the command line.
!!
!!    When using one of the Unix-like command line forms note that
!!    (subject to change) the following variations from other common
!!    command-line parsers:
!!
!!       o duplicate keywords are replaced by the rightmost entry
!!
!!       o numeric keywords are not allowed; but this allows
!!         negative numbers to be used as values.
!!
!!       o specifying both names of an equivalenced keyword will have
!!         undefined results (currently, their alphabetical order
!!         will define what the Fortran variable values become).
!!
!!       o there is currently no mapping of short names to long
!!         names except via an EQUIVALENCE.
!!
!!       o short keywords cannot be combined. -a -b -c is required,
!!         not -abc even for Boolean keys.
!!
!!       o shuffling is not supported. Values must follow their
!!         keywords.
!!
!!       o if a parameter value of just "-" is supplied it is
!!         converted to the string "stdin".
!!
!!       o if the keyword "--" is encountered the rest of the
!!         command arguments go into the character array "UNUSED".
!!
!!       o values not matching a keyword go into the character
!!         array "UNUSED".
!!
!!       o long names do not take the --KEY=VALUE form, just
!!         --KEY VALUE; and long names should be all lowercase and
!!         always more than one character.
!!
!!       o short-name parameters of the form -LETTER VALUE
!!         map to a NAMELIST name of LETTER_ if uppercase
!!
!!##AUTHOR
!!    John S. Urban, 2019
!!
!!##LICENSE
!!    Public Domain
function get_namelist(definition,all) result (readme)

! ident_2="@(#) M_args get_namelist(3f) return all command arguments as a NAMELIST(3f) string to read"

character(len=*),intent(in),optional :: definition
logical,intent(in),optional          :: all
character(len=:),allocatable         :: hold               ! stores command line argument
character(len=:),allocatable         :: readme             ! stores command line argument
integer                              :: ibig

   if(allocated(unnamed))then
       deallocate(unnamed)
   endif
   ibig=longest_command_argument() ! bug in gfortran. len=0 should be fine
   allocate(character(len=ibig) :: unnamed(0))
   if(present(all))then
      return_all=all
   else
      return_all=.false.
   endif
   if(present(definition))then
      if(definition.eq.'')then
         readme=get_command_arguments_as_raw_namelist()
      else
         call wipe_dictionary()
         hold=adjustl(definition)
         if(hold(1:1).eq.'&')then                          ! definition is assumed to be a NAMELIST string
            call namelist_to_dictionary(hold)
            present_in=.false.
            call prototype_and_cmd_args_to_nlist(' ',readme)
         else                                              ! definition is assumed to be a prototype of the command
            call prototype_and_cmd_args_to_nlist(hold,readme)
         endif
      endif
   else                                                    ! assume should read command line as a raw string in NAMELIST format
      readme=get_command_arguments_as_raw_namelist()
   endif

   if(.not.allocated(unnamed))then
       allocate(character(len=0) :: unnamed(0))
   endif

end function get_namelist
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    get_command_arguments_as_raw_namelist(3f) - [ARGUMENTS:M_args] NAMELIST-based command line argument parsing
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine get_command_arguments_as_raw_namelist(string)
!!
!!    character(len=:),allocatable,intent(out) :: string
!!##DESCRIPTION
!!
!!    NAMELIST can be used to pass keyword-value pairs via the command
!!    line. the following example program simply needs an initialized
!!    variable added to the NAMELIST and it automatically is available as
!!    a command line argument. Hard to imagine it getting much simpler.
!!
!!    You can call the example program with syntax like:
!!
!!       testit r=200e3 i=200
!!       testit K=33333,J=22222,I=11111
!!
!!    Note that if you pass in strings you probably will have to use nested
!!    quotes or escape your quote characters. How you do that can vary with
!!    what shell and OS you are running in.
!!
!!       # just quote the entire argument list with single quotes ...
!!       testit 'c="my character string" S=10,T=20.30,R=3e-2'
!!
!!       or nest the quotes ...
!!       testit c='"string"' S=20.30
!!
!!       or escape the quotes ...
!!       testit c=\"string\"
!!
!!    As you will see, there is no need to convert from strings to numeric
!!    values in the source code. Even arrays and user-defined types can be
!!    used, complex values can be input ... just define the variable and
!!    add it to the NAMELIST definition.
!!
!!    And if you want to use a config file instead of command line arguments
!!    since your arguments are defined in a NAMELIST group just create a
!!    NAMELIST input file and read it.
!!
!!##RETURNS
!!    STRING  composed of all command arguments concatenated into a string
!!            prepared for reading as a NAMELIST.
!!
!!##EXAMPLE
!!
!!   Sample usage
!!
!!    program demo_get_command_arguments_as_raw_namelist
!!    implicit none
!!    character(len=255)           :: message ! use for I/O error messages
!!    character(len=:),allocatable :: string  ! stores command line argument
!!    integer                      :: ios
!!
!!    ! declare and initialize a namelist
!!    integer    :: i=1, j=2, k=3
!!    real       :: s=111.1, t=222.2, r=333.3
!!    real       :: arr(3)=[10.0,20.0,30.0]
!!    character(len=255) :: c=' '
!!    ! just add a variable here and it is a new parameter !!
!!    namelist /args/ i,j,k,s,t,r,c,arr
!!
!!       ! return command line arguments as NAMELIST input
!!       string=get_command_arguments_as_raw_namelist()
!!       ! internal read of namelist
!!       read(string,nml=args,iostat=ios,iomsg=message)
!!       if(ios.ne.0)then
!!          write(*,'("ERROR:",i0,1x,a)')ios, trim(message)
!!          write(*,*)'OPTIONS:'
!!          write(*,nml=args)
!!          stop 1
!!       endif
!!       ! all done cracking the command line
!!
!!       ! use the values in your program. For example ...
!!       sum=i+j+k
!!       write(*,*)'sum=',sum
!!    end program demo_get_command_arguments_as_raw_namelist
!!
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    Public Domain
function get_command_arguments_as_raw_namelist() result (string)

! ident_3="@(#) M_args get_command_arguments_as_raw_namelist(3f) return all command arguments as a NAMELIST(3f) string"

character(len=:),allocatable :: string                     ! stores command line argument
character(len=:),allocatable :: string_bug                 ! bug in gfortran 7.4.0 where string in LHS and RHS causes problems
integer :: command_line_length
   call get_command(length=command_line_length)            ! get length needed to hold command
   allocate(character(len=command_line_length) :: string)
   call get_command(string)
   ! trim off command name and get command line arguments
   string_bug=adjustl(string)//' '                         ! assuming command verb does not have spaces in it
   string=string_bug(index(string_bug,' '):)
   string="&ARGS "//string//" /"                            ! add namelist prefix and terminator
   end function get_command_arguments_as_raw_namelist
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    prototype_to_dictionary(3f) - [ARGUMENTS:M_args] parse user command and store tokens into dictionary
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine prototype_to_dictionary(string)
!!
!!    character(len=*),intent(in)     ::  string
!!
!!##DESCRIPTION
!!    given a string of form
!!
!!      -var value -var value
!!
!!    define dictionary of form
!!
!!      keyword(i), value(i)
!!
!!    o  string values
!!
!!        o must be delimited with double quotes.
!!        o adjacent double quotes put one double quote into value
!!        o must not be null. A blank is specified as " ", not "".
!!
!!    o  logical values
!!
!!        o logical values must not have a value
!!
!!    o  leading and trailing blanks are removed from unquoted values
!!
!!
!!##OPTIONS
!!
!!    STRING   string is character input string to define command
!!
!!##RETURNS
!!
!!##EXAMPLE
!!
!!   sample program:
!!
!!    program demo_prototype_to_dictionary
!!       ! look at some of the values as strings or numbers
!!    end program demo_prototype_to_dictionary
!!
!!   Results:
!!
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    Public Domain
subroutine prototype_to_dictionary(string)
implicit none

! ident_4="@(#) M_args prototype_to_dictionary(3f) parse user command and store tokens into dictionary"

character(len=*),intent(in)       :: string ! string is character input string of options and values

character(len=:),allocatable      :: dummy   ! working copy of string
character(len=:),allocatable      :: value
character(len=:),allocatable      :: keyword
character(len=3)                  :: delmt   ! flag if in a delimited string or not
character(len=1)                  :: currnt  ! current character being processed
character(len=1)                  :: prev    ! character to left of CURRNT
character(len=1)                  :: forwrd  ! character to right of CURRNT
integer,dimension(2)              :: ipnt
integer                           :: islen   ! number of characters in input string
integer                           :: ipoint
integer                           :: itype
integer                           :: ifwd
integer                           :: ibegin
integer                           :: iend

   islen=len_trim(string)                               ! find number of characters in input string
   if(islen  ==  0)then                                 ! if input string is blank, even default variable will not be changed
      return
   endif
   dummy=string//'  '

   keyword=""          ! initial variable name
   value=""            ! initial value of a string
   ipoint=0            ! ipoint is the current character pointer for (dummy)
   ipnt(2)=2           ! pointer to position in parameter name
   ipnt(1)=1           ! pointer to position in parameter value
   itype=1             ! itype=1 for value, itype=2 for variable

   delmt="off"
   prev=" "

   keyword_single=.true.
   do
      ipoint=ipoint+1               ! move current character pointer forward
      currnt=dummy(ipoint:ipoint)   ! store current character into currnt
      ifwd=min(ipoint+1,islen)
      forwrd=dummy(ifwd:ifwd)       ! next character (or duplicate if last)

      if((currnt=="-".and.prev==" ".and.delmt == "off".and.index("0123456789.",forwrd) == 0).or.ipoint > islen)then
         ! beginning of a parameter name
         if(forwrd.eq.'-')then                      ! change --var to -var so "long" syntax is supported
            dummy(ifwd:ifwd)='_'
            ipoint=ipoint+1                         ! ignore second - instead
            keyword_single=.false.
         else
            keyword_single=.true.
         endif
         if(ipnt(1)-1 >= 1)then
            ibegin=1
            iend=len_trim(value(:ipnt(1)-1))
            do
               if(iend  ==  0)then                  ! len_trim returned 0, parameter value is blank
                  iend=ibegin
                  exit
               elseif(value(ibegin:ibegin) == " ")then
                  ibegin=ibegin+1
               else
                  exit
               endif
            enddo
            if(keyword.ne.' ')then
               call update(keyword,value)         ! store name and its value
            else
               write(stderr,*)'*prototype_to_dictionary* warning: ignoring blank keyword ',trim(value)
            endif
         else
            if(keyword.ne.' ')then
               call update(keyword,'F')           ! store name and null value
            else
               if(debug)then
                  write(stderr,*)'*prototype_to_dictionary* warning: blank keyword, and ignoring blank value',trim(value)
               endif
            endif
         endif
         itype=2                               ! change to filling a variable name
         value=""                              ! clear value for this variable
         keyword=""                            ! clear variable name
         ipnt(1)=1                             ! restart variable value
         ipnt(2)=1                             ! restart variable name

      else       ! currnt is not one of the special characters
         ! the space after a keyword before the value
         if(currnt == " ".and.itype  ==  2)then
            ! switch from building a keyword string to building a value string
            itype=1
            ! beginning of a delimited parameter value
         elseif(currnt  ==  """".and.itype  ==  1)then
            ! second of a double quote, put quote in
            if(prev  ==  """")then
               if(itype.eq.1)then
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
               if(itype.eq.1)then
                  value=value//currnt
               else
                  keyword=keyword//currnt
               endif
               ipnt(itype)=ipnt(itype)+1
            endif
         else     ! add character to current parameter name or parameter value
            if(itype.eq.1)then
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
      endif
      exit
   enddo

end subroutine prototype_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    update(3f) - [ARGUMENTS:M_args] update internal dictionary given keyword and value
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine update(key,val)
!!
!!    character(len=*),intent(in)           :: key
!!    character(len=*),intent(in),optional  :: val
!!##DESCRIPTION
!!    Update internal dictionary in M_args(3fm) module.
!!##OPTIONS
!!    key  name of keyword to add, replace, or delete from dictionary
!!    val  if present add or replace value associated with keyword. If not
!!         present remove keyword entry from dictionary.
!!##RETURNS
!!##EXAMPLE
!!
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    Public Domain
subroutine update(key,val)
character(len=*),intent(in)           :: key
character(len=*),intent(in),optional  :: val
integer                               :: place
integer                               :: ilen
character(len=:),allocatable          :: val_local
   if(debug)then
      if(present(val))then
         write(stderr,*)'*update* DEBUG: KEY=',key,' VAL=',val
      else
         write(stderr,*)'*update* DEBUG: KEY=',key
      endif
   endif
   if(present(val))then
      val_local=val
      ilen=len_trim(val_local)
      call locate(keywords,key,place)                   ! find where string is or should be
      if(place.lt.1)then                                ! if string was not found insert it
         call insert(keywords,key,iabs(place))
         call insert(values,val_local,iabs(place))
         call insert(counts,ilen,iabs(place))
         call insert(present_in,.true.,iabs(place))
      else
         call replace(values,val_local,place)
         call replace(counts,ilen,place)
         call replace(present_in,.true.,place)
      endif
   else                                                 ! if no value is present remove the keyword and related values
      call locate(keywords,key,place)
      if(place.gt.0)then
         call remove(keywords,place)
         call remove(values,place)
         call remove(counts,place)
         call remove(present_in,place)
      endif
   endif
   if(debug)then
      if(present(val))then
         write(stderr,*)'*update* DEBUG: KEY=',key,'PLACE=',place,' VAL=',val, &
                &size(keywords),size(values),size(counts),size(present_in)
      else
         write(stderr,*)'*update* DEBUG: KEY=',key,'PLACE=',place,size(keywords),size(values),size(counts),size(present_in)
      endif
      write(stderr,*)present_in
   endif
end subroutine update
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    wipe_dictionary(3fp) - [ARGUMENTS:M_args] reset private M_args(3fm) dictionary to empty
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    subroutine wipe_dictionary()
!!##DESCRIPTION
!!    reset private M_args(3fm) dictionary to empty
!!##EXAMPLE
!!
!!    program demo_wipe_dictionary
!!    use M_args, only : dictionary
!!       call wipe_dictionary()
!!    end program demo_wipe_dictionary
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    Public Domain
subroutine wipe_dictionary()
   if(allocated(keywords))deallocate(keywords)
   allocate(character(len=0) :: keywords(0))
   if(allocated(values))deallocate(values)
   allocate(character(len=0) :: values(0))
   if(allocated(counts))deallocate(counts)
   allocate(counts(0))
   if(allocated(present_in))deallocate(present_in)
   allocate(present_in(0))
end subroutine wipe_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##SYNOPSIS
!!
!!    get(3f) - [ARGUMENTS:M_args] get dictionary value associated with key name in private M_args(3fm) dictionary
!!##DESCRIPTION
!!    Get dictionary value associated with key name in private M_args(3fm) dictionary.
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
function get(key) result(valout)
character(len=*),intent(in)   :: key
character(len=:),allocatable  :: valout
integer                       :: place
   ! find where string is or should be
   call locate(keywords,key,place)
   if(place.lt.1)then
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
!!    prototype_and_cmd_args_to_nlist(3f) - [ARGUMENTS:M_args] convert Unix-like command arguments to namelist
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine prototype_and_cmd_args_to_nlist(prototype,nml)
!!
!!    character(len=*)             :: prototype
!!    character(len=:),allocatable :: nml
!!##DESCRIPTION
!!    create dictionary with character keywords, values, and value lengths using the routines for maintaining a list from
!!    command line arguments.
!!##OPTIONS
!!    prototype
!!##RETURNS
!!    nml
!!##EXAMPLE
!!
!!   Sample program
!!    program demo_prototype_and_cmd_args_to_nlist
!!    use M_args,  only : prototype_and_cmd_args_to_nlist, unnamed, debug
!!    implicit none
!!    character(len=:),allocatable :: readme
!!    character(len=256)           :: message
!!    integer                      :: ios
!!    integer                      :: i
!!    doubleprecision              :: something
!!
!!    ! define namelist
!!    ! lowercase keywords
!!    logical            :: l,h,v
!!    real               :: p(2)
!!    complex            :: c
!!    doubleprecision    :: x,y,z
!!
!!    ! uppercase keywords get an underscore
!!    logical            :: l_,h_,v_
!!    character(len=256) :: a_,b_                  ! character variables must be long enough to hold returned value
!!    integer            :: c_(3)
!!    namelist /args/ l,h,v,p,c,x,y,z,a_,b_,c_,l_,h_,v_
!!
!!       debug=.true.
!!       ! give command template with default values
!!       ! all values except logicals get a value.
!!       ! strings must be delimited with double quotes
!!       ! A string has to have at least one character as for -A
!!       ! lists of numbers should be comma-delimited. No spaces are allowed in lists of numbers
!!       ! the values follow the rules for NAMELIST input, so  -p 2*0 would define two values.
!!       call prototype_and_cmd_args_to_nlist('-l -v -h -LVH -x 0 -y 0.0 -z 0.0d0 -p 0,0 -A " " -B "Value B" -C 10,20,30 -c (-123,-456)',readme)
!!       read(readme,nml=args,iostat=ios,iomsg=message)
!!       if(ios.ne.0)then
!!          write(*,*)'ERROR:',trim(message)
!!          write(*,'("INPUT WAS ",a)')readme
!!          write(*,args)
!!          stop 3
!!       else
!!          something=sqrt(x**2+y**2+z**2)
!!          write(*,*)something,x,y,z
!!          if(size(unnamed).gt.0)then
!!             write(*,'(a)')'files:'
!!             write(*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
!!          endif
!!       endif
!!    end program demo_prototype_and_cmd_args_to_nlist
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    Public Domain
subroutine prototype_and_cmd_args_to_nlist(prototype,nml)
implicit none

! ident_5="@(#) M_args prototype_and_cmd_args_to_nlist create dictionary from prototype (if not null) and update from command line arguments"

character(len=*)             :: prototype
character(len=:),allocatable :: nml
integer                      :: ibig
   if(debug)then
      write(stderr,*)'*prototype_and_cmd_args_to_nlist* DEBUG: prototype=',trim(prototype)
   endif

   passed_in=prototype ! make global copy for printing

   if(allocated(unnamed))deallocate(unnamed)
   ibig=longest_command_argument() ! bug in gfortran. len=0 should be fine
   allocate(character(len=ibig) ::unnamed(0))

   if(prototype.ne.'')then
      call prototype_to_dictionary(prototype)  ! build dictionary from prototype
      namelist_name='&ARGS'
      present_in=.false.  ! reset all values to false
   endif

   if(debug)then                            ! look at some of the values as strings or numbers
      call print_dictionary('DICTIONARY FROM PROTOTYPE')
   endif

   call cmd_args_to_dictionary(check=.true.)

   call dictionary_to_namelist(nml)

   ! show array
   if(debug)then
      call print_dictionary('DICTIONARY FROM COMMAND LINE:')
   endif

end subroutine prototype_and_cmd_args_to_nlist
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine cmd_args_to_dictionary(check)
! convert command line arguments to dictionary entries
! reading the namelist output will trap unknown option names so do not really need to trap them here
logical,intent(in),optional  :: check
logical                      :: check_local
integer                      :: pointer
character(len=:),allocatable :: lastkeyword
integer                      :: i
integer                      :: ilength, istatus, imax
character(len=:),allocatable :: current_argument
character(len=:),allocatable :: current_argument_padded
character(len=:),allocatable :: dummy
character(len=:),allocatable :: oldvalue
logical                      :: nomore
   if(present(check))then
      check_local=check
   else
      check_local=.false.
   endif
   nomore=.false.
   pointer=0
   lastkeyword=' '
   keyword_single=.true.
   GET_ARGS: do i=1, command_argument_count()                                                        ! insert and replace entries
      call get_command_argument(number=i,length=ilength,status=istatus)                              ! get next argument
      if(istatus /= 0) then                                                                          ! stop program on error
         write(stderr,*)'*prototype_and_cmd_args_to_nlist* error obtaining argument ',i,&
            &'status=',istatus,&
            &'length=',ilength
         exit GET_ARGS
      else
         if(allocated(current_argument))deallocate(current_argument)
         ilength=max(ilength,1)
         allocate(character(len=ilength) :: current_argument)
         call get_command_argument(number=i,value=current_argument,length=ilength,status=istatus)    ! get next argument
         if(istatus /= 0) then                                                                       ! stop program on error
            write(stderr,*)'*prototype_and_cmd_args_to_nlist* error obtaining argument ',i,&
               &'status=',istatus,&
               &'length=',ilength,&
               &'target length=',len(current_argument)
            exit GET_ARGS
          endif
      endif

      if(current_argument.eq.'-')then  ! sort of
         current_argument='"stdin"'
      endif
      if(current_argument.eq.'--')then ! everything after this goes into the unnamed array
         nomore=.true.
         pointer=0
         cycle
      endif
      dummy=current_argument//'   '
      current_argument_padded=current_argument//'   '
      if(.not.nomore.and.current_argument_padded(1:2).eq.'--'.and.index("0123456789.",dummy(3:3)).eq.0)then ! beginning of long word
         keyword_single=.false.
         if(lastkeyword.ne.'')then
            call ifnull()
         endif
         call locate(keywords,current_argument_padded(3:),pointer)
         if(pointer.le.0.and.check_local)then
            call print_dictionary('UNKNOWN LONG KEYWORD: '//current_argument)
            stop 1
         endif
         lastkeyword=trim(current_argument_padded(3:))
      elseif(.not.nomore.and.current_argument_padded(1:1).eq.'-'.and.index("0123456789.",dummy(2:2)).eq.0)then  ! short word
         keyword_single=.true.
         if(lastkeyword.ne.'')then
            call ifnull()
         endif
         call locate(keywords,current_argument_padded(2:),pointer)
         if(pointer.le.0.and.check_local)then
            call print_dictionary('UNKNOWN SHORT KEYWORD: '//current_argument)
            stop 2
         endif
         lastkeyword=trim(current_argument_padded(2:))
      elseif(pointer.eq.0)then                                                                           ! unnamed arguments
         imax=max(len(unnamed),len(current_argument))
         unnamed=[character(len=imax) :: unnamed,current_argument]
      else
         if(debug)then
            write(stderr,*)'POINTER=',pointer,' KEYWORD=',keywords(pointer),' VALUE=',current_argument,' LENGTH=',ilength
         endif
         oldvalue=get(keywords(pointer))//' '
         if(oldvalue(1:1).eq.'"')then
            current_argument=quote(current_argument(:ilength))
         endif
         if(upper(oldvalue).eq.'F'.or.upper(oldvalue).eq.'T')then  ! assume boolean parameter
            if(current_argument.ne.' ')then
               imax=max(len(unnamed),len(current_argument))
               unnamed=[character(len=imax) :: unnamed,current_argument]
            endif
            current_argument='T'
         endif
         call update(keywords(pointer),current_argument)
         pointer=0
         lastkeyword=''
      endif
   enddo GET_ARGS
   if(lastkeyword.ne.'')then
      call ifnull()
   endif


contains
subroutine ifnull()
   oldvalue=get(lastkeyword)//' '
   if(upper(oldvalue).eq.'F'.or.upper(oldvalue).eq.'T')then
      call update(lastkeyword,'T')
   elseif(oldvalue(1:1).eq.'"')then
      call update(lastkeyword,'" "')
   else
      call update(lastkeyword,' ')
   endif
end subroutine ifnull

end subroutine cmd_args_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine dictionary_to_namelist(nml)
character(len=:),allocatable,intent(out) :: nml
integer :: i
character(len=:),allocatable :: newkeyword
   ! build namelist string
   nml=namelist_name//' '
   if(return_all)then  ! if returning all first do keywords not present on command line so equivalences work
      do i=1,size(keywords)
         if(isupper(keywords(i)(1:1)))then
            newkeyword=trim(lower(keywords(i)))//'_'
         else
            newkeyword=trim(keywords(i))
         endif
         if(.not.present_in(i))then
            nml=nml//newkeyword//'='//trim(values(i))//' '
         endif
      enddo
   endif

   do i=1,size(keywords)  ! now only do keywords present on command line
      if(isupper(keywords(i)(1:1)))then
         newkeyword=trim(lower(keywords(i)))//'_'
      else
         newkeyword=trim(keywords(i))
      endif
      if(present_in(i))then
         nml=nml//newkeyword//'='//trim(values(i))//' '
      endif
   enddo

   nml=nml//' /'
   if(debug)then
      write(stderr,'(a)')'NAMELIST:'
      write(stderr,'(a)')nml
      if(size(unnamed).gt.0)then
         write(stderr,'(a)')'UNNAMED'
         write(stderr,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
      endif
   endif
end subroutine dictionary_to_namelist
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   print_dictionary(3f) - [ARGUMENTS:M_args] print internal dictionary created by calls to get_namelist(3f)
!!   (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine print_dictionary(header)
!!
!!    character(len=*),intent(in),optional :: header
!!##DESCRIPTION
!!   Print the internal dictionary created by calls to get_namelist(3f).
!!   This routine is intended to print the state of the argument list
!!   if an error occurs in using the get_namelist(3f) procedure..
!!##OPTIONS
!!   HEADER  label to print before printing the state of the command
!!           argument list.
!!##EXAMPLE
!!
!!    Typical usage:
!!
!!     program demo_print_dictionary
!!     use M_args,  only : unnamed, get_namelist, print_dictionary
!!     implicit none
!!     integer                      :: i
!!     character(len=255)           :: message ! use for I/O error messages
!!     character(len=:),allocatable :: readme  ! stores updated namelist
!!     integer                      :: ios
!!     real               :: x, y, z
!!     logical            :: help, h
!!     equivalence       (help,h)
!!     namelist /args/ x,y,z,help,h
!!     character(len=*),parameter :: cmd='&ARGS X=1 Y=2 Z=3 HELP=F H=F /'
!!     ! initialize namelist from string and then update from command line
!!     readme=cmd
!!     read(readme,nml=args,iostat=ios,iomsg=message)
!!     if(ios.eq.0)then
!!        ! update cmd with options from command line
!!        readme=get_namelist(cmd)
!!        read(readme,nml=args,iostat=ios,iomsg=message)
!!     endif
!!     if(ios.ne.0)then
!!        write(*,'("ERROR:",i0,1x,a)')ios, trim(message)
!!        call print_dictionary('OPTIONS:')
!!        stop 1
!!     endif
!!     ! all done cracking the command line
!!     ! use the values in your program.
!!     write(*,nml=args)
!!     ! the optional unnamed values on the command line are
!!     ! accumulated in the character array "UNNAMED"
!!     if(size(unnamed).gt.0)then
!!        write(*,'(a)')'files:'
!!        write(*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
!!     endif
!!     end program demo_print_dictionary
!!
!!    Sample output
!!
!!    Calling the sample program with an unknown
!!    parameter produces the following:
!!
!!       $ ./print_dictionary -A
!!       UNKNOWN SHORT KEYWORD: -A
!!       KEYWORD             PRESENT  VALUE
!!       z                   F        [3]
!!       y                   F        [2]
!!       x                   F        [1]
!!       help                F        [F]
!!       h                   F        [F]
!!
!!       STOP 2
!!
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    Public Domain
subroutine print_dictionary(header)
character(len=*),intent(in),optional :: header
integer          :: i
   if(present(header))then
      if(header.ne.'')then
         write(stderr,'(a)')header
      endif
   endif
   if(allocated(keywords))then
      if(size(keywords).gt.0)then
         write(stderr,'(*(a,t21,a,t30,a))')'KEYWORD','PRESENT','VALUE'
         write(stderr,'(*(a,t21,l1,t30,"[",a,"]",/))')(trim(keywords(i)),present_in(i),values(i)(:counts(i)),i=1,size(keywords))
      endif
   endif
   if(allocated(unnamed))then
      if(size(unnamed).gt.0)then
         write(stderr,'(a)')'UNNAMED'
         write(stderr,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
      endif
   endif
end subroutine print_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    longest_command_argument(3f) - [ARGUMENTS:M_args] length of longest argument on command line
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function longest_command_argument() result(ilongest)
!!
!!     integer :: ilongest
!!
!!##DESCRIPTION
!!    length of longest argument on command line. Useful when allocating storage for holding arguments.
!!##RESULT
!!    longest_command_argument  length of longest command argument
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_longest_command_argument
!!    use M_args, only : longest_command_argument
!!       write(*,*)'longest argument is ',longest_command_argument()
!!    end program demo_longest_command_argument
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    Public Domain
function longest_command_argument() result(ilongest)
integer :: i
integer :: ilength
integer :: istatus
integer :: ilongest
   ilength=0
   ilongest=0
   GET_LONGEST: do i=1,command_argument_count()                             ! loop throughout command line arguments to find longest
      call get_command_argument(number=i,length=ilength,status=istatus)     ! get next argument
      if(istatus /= 0) then                                                 ! stop program on error
         write(stderr,*)'*prototype_and_cmd_args_to_nlist* error obtaining length for argument ',i
         exit GET_LONGEST
      elseif(ilength.gt.0)then
         ilongest=max(ilongest,ilength)
      endif
   enddo GET_LONGEST
end function longest_command_argument
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    namelist_to_dictionary(3f) - [ARGUMENTS:M_args] parse namelist string and store tokens into dictionary
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine namelist_to_dictionary(string)
!!
!!    character(len=*),intent(in)     ::  string
!!
!!##DESCRIPTION
!!   must start with a keyword, any keyword that appears must have a value. A character array can have more than one delimited string
!!   unallocated and null values are not allowed
!!   set parameter name to blank
!!   find undelimited =
!!   find previous , or beginning of string. in-between is a keyword= to , that starts a keyword is a value
!!   one keyword and value are known store them
!!
!!##OPTIONS
!!    STRING   string is character input string to define command
!!
!!##EXAMPLE
!!
!!   Typical string:
!!
!!    >&ARGS
!!    > L = F,
!!    > A_="xxxxxxxxxxxxxxxxxxxxxxxxxx                                                      ",
!!    > B_="Value B                                                                         ",
!!    > P= 2*0.00000000      ,
!!    > C_=         10,         20,         30, XYZ_=(-123.000000,-456.000000),
!!    > X=  0.0000000000000000     ,
!!    > Y=  0.0000000000000000     ,
!!    > Z=  0.0000000000000000     ,
!!    > /
!!##AUTHOR
!!    John S. Urban, 2019
!!##LICENSE
!!    Public Domain
subroutine namelist_to_dictionary(string)
implicit none

! ident_6="@(#) M_args namelist_to_dictionary(3f) parse user command and store tokens into dictionary"

character(len=*),intent(in)       :: string ! string is character input string of options and values

character(len=:),allocatable      :: dummy       ! working copy of string
character(len=:),allocatable      :: dummy_bug   ! bug in gfortran 7.4.0 where if dummy is on LHS and used in RHS get wrong result
character(len=:),allocatable      :: keyword_value
character(len=:),allocatable      :: value
character(len=:),allocatable      :: keyword
logical                           :: delmt   ! flag if in a delimited string or not
character(len=1)                  :: currnt  ! current character being processed
character(len=1)                  :: prev    ! current character being processed
integer                           :: islen   ! number of characters in input string
integer                           :: ipoint
integer                           :: istart
integer                           :: iend
integer                           :: ileft
integer                           :: icut
integer                           :: i
integer                           :: iback1,iback2
   if(debug)then
      write(stderr,*)'*namelist_to_dictionary* INPUT=',trim(string)
   endif
   islen=len_trim(string)                               ! find number of characters in input string
   if(islen  ==  0)then                                 ! if input string is blank, even default variable will not be changed
      return
   endif
   islen=islen-1                                        ! by definition last character in NAMELIST output is /
   dummy=trim(adjustl(string(:islen)))
   ! strip off namelist group name
   ileft=index(dummy,'&')
   dummy_bug=adjustl(dummy(ileft+1:))
   ileft=index(dummy_bug,' ')
   if(ileft.eq.0)then
      ileft=len(dummy_bug)
   endif
   namelist_name=upper('&'//dummy_bug(:ileft-1))
   dummy=adjustl(dummy_bug(ileft:))

   islen=len(dummy)
   dummy=dummy//'    '
   if(debug)then
      write(stderr,*)'*namelist_to_dictionary* NAMELIST_NAME=['//namelist_name//']'
      write(stderr,*)'*namelist_to_dictionary* DUMMY=['//dummy//']'
   endif


   keyword=""          ! initial variable name
   value=""            ! initial value of a string
   delmt=.false.       ! whether in a character string or not
   prev=" "
   istart=1
   do ipoint=1,islen
      currnt=dummy(ipoint:ipoint)             ! store current character into currnt
      if(currnt=="=".and..not.delmt)then ! end of a parameter name
         keyword_value=''
         iend=0
         do i=ipoint-1,1,-1
            if(dummy(i:i).eq.' ')cycle
            ! found non-space
            iback1=index(dummy(:i),' ',back=.true.)
            iback2=index(dummy(:i),',',back=.true.)
            iend=max(iback1,iback2)
            exit
         enddo
         if(iend.ne.0)then
            call splitit()
         endif
         istart=iend+1
      elseif(currnt  ==  """")then
         if(prev  ==  """")then               ! second of a double quote, put quote in
            delmt=.not.delmt
         elseif(delmt)then
            delmt=.false.
         else
            delmt=.true.
         endif
      endif
      prev=currnt
      if(ipoint.ge.islen)then
         iend=ipoint
         call splitit()
      endif
   enddo
   if(debug)then
      call print_dictionary('NAMELIST TO DICTIONARY')
   endif
contains

subroutine splitit()
integer :: ilast
keyword_value=dummy(istart:iend)
! split keyword_value on first = and convert values to lowercase except for LETTER_ convert to uppercase LETTER and
! remove trailing , as NAMELIST output being read should not contain null values as everything in a namelist needs
! to be allocated (at least in this version of Fortran?).
   icut=index(keyword_value,'=')
   if(icut.eq.0)then
      write(stderr,*)'*splitit* INTERNAL ERROR: KEYWORD_VALUE=['//keyword_value//']'
   else
      if(debug)then
         write(stderr,*)'*splitit* KEYWORD_VALUE=['//keyword_value//']',icut
      endif
      keyword=adjustl(trim(lower(keyword_value(:icut-1))))
      if(len(keyword).eq.2)then
         if(keyword(2:2).eq.'_')then
            keyword=upper(keyword(1:1))
         endif
      endif
      if(icut.eq.len(keyword_value))then
         value=''
      else
         value=trim(adjustl(keyword_value(icut+1:)))
         ilast=len(value)
         if(ilast.eq.0)then
            value=''
         else
            if(value(ilast:ilast).eq.',')then
               value=trim(value(:ilast-1))
            endif
         endif
      endif
      call update(keyword,value)
   endif
end subroutine splitit

end subroutine namelist_to_dictionary
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function oneline(str) result (string)

! ident_7="@(#) M_strings oneline(3f) append an array of character variables with space separator into a single CHARACTER variable"

character(len=*),intent(in)          :: str(:)
character(len=:),allocatable         :: string
integer                              :: i
character(len=1),parameter           :: sep=' '

   string=''
   do i = 1,size(str)
      string=string//trim(str(i))//sep
   enddo
end function oneline
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_args
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
