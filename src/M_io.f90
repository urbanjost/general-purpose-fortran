










!===================================================================================================================================
MODULE M_io
use, intrinsic :: iso_fortran_env, only : error_unit,input_unit,output_unit     ! access computing environment
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
use M_strings, only : merge_str, lower, notabs, s2v, isnumber, decodebase, s2vs, split, substitute
use M_uuid,    only : generate_uuid
use M_journal, only : journal
implicit none
integer,parameter,private:: sp=kind(1.0), dp=kind(1.0d0)
private
public uniq
public print_inquire
public notopen
public slurp
public swallow
public number_of_lines
public dirname
public basename
public splitpath
public joinpath
public fileopen
public fileclose
public filewrite
public filedelete
public get_tmp
public scratch
public read_line
public getline
public read_table
public rd
public separator
public lookfor
public which
public get_env
public getname

! ident_1="@(#)M_io::read_table(3f): read file containing a table of numeric values"

! ident_2="@(#)M_io::rd(3f): ask for string or number from standard input with user-definable prompt"
interface rd
   module procedure rd_character
   module procedure rd_integer
   module procedure rd_real
   module procedure rd_doubleprecision
end interface

interface read_table
   module procedure read_table_i
   module procedure read_table_r
   module procedure read_table_d
end interface


CONTAINS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      uniq(3f) - [M_io] append a number to the end of filename to make a unique name if name exists
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!      Usage
!!
!!       character(len=:),allocatable function uniq(name,istart,verbose,create)
!!       character(len=*),intent(in) :: name
!!       integer,intent(in),optional :: istart
!!       logical,intent(in),optional :: verbose
!!       logical,intent(in),optional :: create
!!
!!##DESCRIPTION
!!    Given a filename test if it is in use or exists. If it is, or if it
!!    ends in a period add a number to the end of the name and
!!    test if the new name exists. If necessary, increment the number and
!!    try again up to the value 9999999. By default an empty file is created
!!    if an unused name is found.
!!
!!    o relatively non-generic;
!!    o does not try to detect io errors
!!
!!##OPTIONS
!!    name     base input name used to create output filename
!!             If name ends in "." a numeric suffix is always added.
!!    istart   number to start with as a suffix. Default is 1. Must be a
!!             positive integer less than 9999999.
!!    verbose  writes extra messages to stdout. Defaults to .false.
!!    create   create file if new name is successfully found. Defaults
!!             to .true. .
!!
!!##RETURNS
!!    uniq     A unique filename that is the same as the NAME input parameter
!!             except with a number appended at the end if needed. If could
!!             not find a unique name a blank is returned.
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!       program demo_uniq
!!       use M_io, only : uniq
!!       implicit none
!!       character(len=4096) :: myname
!!       integer             :: i
!!          myname=uniq('does_not_exist')
!!          open(unit=10,file='does_exist')
!!          write(*,*)'name stays the same ',trim(myname)
!!          myname=uniq('does_exist')
!!          write(*,*)'name has suffix added ',trim(myname)
!!          do i=1,10
!!             myname=uniq('does_exist')
!!             write(*,*) 'FILENAME:',trim(myname)
!!             open(unit=20+i,file=myname)
!!          enddo
!!       end program demo_uniq
!!
!!    Expected output
!!
!!     name stays the same does_not_exist
!!     name has suffix added does_exist0001
!!     FILENAME:does_exist0002
!!     FILENAME:does_exist0003
!!     FILENAME:does_exist0004
!!     FILENAME:does_exist0005
!!     FILENAME:does_exist0006
!!     FILENAME:does_exist0007
!!     FILENAME:does_exist0008
!!     FILENAME:does_exist0009
!!     FILENAME:does_exist0010
!!     FILENAME:does_exist0011
!!
!!##AUTHOR
!!    John S. Urban, 1993
!!##LICENSE
!! Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
function uniq(name,istart,verbose,create)
implicit none

! ident_3="@(#)M_io::uniq(3f): append a number to the end of filename to make a unique name if name exists"

!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in)  :: name
character(len=:),allocatable :: uniq
integer,intent(in),optional  :: istart
logical,intent(in),optional  :: verbose
logical,intent(in),optional  :: create
!-----------------------------------------------------------------------------------------------------------------------------------
logical                     :: around
integer,save                :: icount=1           ! counter to generate suffix from
character(len=4096),save    :: lastname=' '       ! name called with last time the routine was called
integer                     :: ilen
integer                     :: itimes
integer                     :: iscr
integer                     :: ios
logical                     :: verbose_local
logical                     :: create_local
!-----------------------------------------------------------------------------------------------------------------------------------
   uniq=trim(name)                                   ! the input name will be returned if it passes all the tests
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lastname.ne.name)then                          ! if a different input name than last time called reset icount
      lastname=name                                  ! a new name to keep for subsequent calls
      icount=1                                       ! icount is used to make a suffix to add to make the file unique
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(create))then
      create_local=create
   else
      create_local=.true.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(istart))then
      icount=istart                                  ! icount is used to make a suffix to add to make the file unique
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len_trim(name)                               ! find last non-blank character in file name
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ilen.ne.0)then                                 ! a blank input name so name will just be a suffix
      if(name(ilen:ilen).ne.'.')then                 ! always append a number to a file ending in .
         inquire(file=name(:ilen),exist=around)      ! check filename as-is
         if(.not.around)then                         ! file name does not exist, can use it as-is
            uniq=trim(name)
            if(create_local)then
               open(newunit=iscr,file=uniq,iostat=ios,status='new')
               close(unit=iscr,iostat=ios)
            endif
            return
         endif
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   itimes=0                                           ! count number of times tried to get a uniq name
   deallocate(uniq)
   allocate(character(len=ilen+8) :: uniq)            ! make it useable with an internal WRITE(3f) with room for a numeric suffix
   uniq(:)=name
   INFINITE: do                                       ! top of loop trying for a unique name
      if(itimes.ge.9999999)then                       ! if too many tries to be reasonable give up
         call journal('sc','*uniq* unable to find a unique filename. Too many tries')
         uniq=''
         return
      endif
      if(icount.gt.9999999) icount=1                  ! reset ICOUNT when it hits arbitrary maximum value
      if(icount.le.9999)then
         write(uniq(ilen+1:),'(i4.4)')icount          ! create name by adding a numeric string to end
      else
         write(uniq(ilen+1:),'(i7.7)')icount          ! create name by adding a numeric string to end
      endif
      icount=icount+1                                 ! increment counter used to come up with suffix
      inquire(file=uniq,exist=around)                 ! see if this filename already exists
      if(.not.around)then                             ! found an unused name
         if(verbose_local)then
            call journal('c',trim('*uniq* name='//trim(uniq))) ! write out message reporting name used
         endif
         if(create_local)then
            open(newunit=iscr,file=uniq,iostat=ios,status='new')
            close(unit=iscr,iostat=ios)
         endif
         uniq=trim(uniq)
         return                                       ! return successfully
      endif
      itimes=itimes+1                                 ! haven't found a unique name, try again
   enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
end function uniq
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    print_inquire(3f) - [M_io] Do INQUIRE on file by name/number and print results
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   Definition:
!!
!!    subroutine print_inquire(lun)
!!      or
!!    subroutine print_inquire(name)
!!    integer,intent(in),optional          :: lun
!!    character(len=*),intent(in),optional :: name
!!
!!##DESCRIPTION
!!    Given either a Fortran file-unit-number or filename, call the INQUIRE(3f)
!!    intrinsic and print typical status information.
!!
!!##OPTIONS
!!    lun    if lun is not equal to -1 then query by number and ignore
!!           filename even if present
!!    name   if lun = -1  or is not present then query by this filename
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_print_inquire
!!    use M_io, only : print_inquire, fileopen
!!    implicit none
!!    character(len=4096)  :: filename
!!    character(len=20)    :: mode
!!    integer              :: ios
!!    character(len=256)   :: message
!!    integer              :: lun
!!       do
!!          write(*,'(a)',advance='no')'enter filename>'
!!          read(*,'(a)',iostat=ios)filename
!!          if(ios.ne.0)exit
!!          write(*,'(a)',advance='no')'enter mode ([rwa][bt][+]>'
!!          read(*,'(a)',iostat=ios)mode
!!          if(ios.ne.0)exit
!!          lun=fileopen(filename,mode,ios)
!!          if(ios.eq.0)then
!!             write(*,*)'OPENED'
!!          else
!!             write(*,*)'ERROR: IOS=',ios
!!          endif
!!          if(lun.ne.-1)then
!!             call print_inquire(lun,'')
!!             close(lun,iostat=ios,iomsg=message)
!!             if(ios.ne.0)then
!!                write(*,'(a)')trim(message)
!!             endif
!!          endif
!!       enddo
!!    end program demo_print_inquire
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine print_inquire(lun_in,namein_in) ! Version: JSU-1997-12-31, 2020-01-11

! ident_4="@(#)M_io::print_inquire(3f): Do INQUIRE on file by name/number and print results"

integer,intent(in),optional             :: lun_in        ! if unit >= 0 then query by unit number, else by name
character(len=*),intent(in),optional    :: namein_in
integer                        :: ios
character(len=256)             :: message
character(len=:),allocatable   :: namein
integer                        :: lun
!==============================================================================================
!  ACCESS    =  SEQUENTIAL  |  DIRECT       |  STREAM
!  ACTION    =  READ        | WRITE         |  READWRITE
!  FORM      =  FORMATTED   |  UNFORMATTED
!  POSITION  =  ASIS        |  REWIND       |  APPEND
!  STATUS    =  NEW         |  REPLACE      |  OLD     |  SCRATCH   | UNKNOWN
character(len=20)              :: access         ; namelist/inquire/access
character(len=20)              :: action         ; namelist/inquire/action
character(len=20)              :: asynchronous   ; namelist/inquire/asynchronous
character(len=20)              :: blank          ; namelist/inquire/blank
character(len=20)              :: decimal        ; namelist/inquire/decimal
character(len=20)              :: delim          ; namelist/inquire/delim
character(len=20)              :: direct         ; namelist/inquire/direct
character(len=20)              :: encoding       ; namelist/inquire/encoding
logical                        :: exist          ; namelist/inquire/exist
character(len=20)              :: form           ; namelist/inquire/form
character(len=20)              :: formatted      ; namelist/inquire/formatted
integer                        :: id             ; namelist/inquire/id
character(len=20)              :: name           ; namelist/inquire/name
logical                        :: named          ; namelist/inquire/named
integer                        :: nextrec        ; namelist/inquire/nextrec
integer                        :: number         ; namelist/inquire/number
logical                        :: opened         ; namelist/inquire/opened
character(len=20)              :: pad            ; namelist/inquire/pad
logical                        :: pending        ; namelist/inquire/pending
integer                        :: pos            ; namelist/inquire/pos
character(len=20)              :: position       ; namelist/inquire/position
character(len=20)              :: read           ; namelist/inquire/read
character(len=20)              :: readwrite      ; namelist/inquire/readwrite
integer                        :: recl           ; namelist/inquire/recl
character(len=20)              :: round          ; !BUG!namelist/inquire/round
character(len=20)              :: sequential     ; namelist/inquire/sequential
character(len=20)              :: sign           ; !BUG!namelist/inquire/sign
integer                        :: size           ; namelist/inquire/size
character(len=20)              :: stream         ; namelist/inquire/stream
character(len=20)              :: unformatted    ; namelist/inquire/unformatted
character(len=20)              :: write          ; namelist/inquire/write
!==============================================================================================
   namein=merge_str(namein_in,'',present(namein_in))
   lun=merge(lun_in,-1,present(lun_in))
   ! exist, opened, and named always become defined unless an error condition occurs.
   !!write(*,*)'LUN=',lun,' FILENAME=',namein
   !-----------------------------------------------------------------------------------------------------------------------------------
   name=''
   if(namein.eq.''.and.lun.ne.-1)then
         call journal('sc','*print_inquire* checking unit',lun)
         inquire(unit=lun,                                                                               &
     &   recl=recl,nextrec=nextrec,pos=pos,size=size,                                                    &
     &   position=position,                                                                              &
     &   name=name,                                                                                      &
     &   form=form,formatted=formatted,unformatted=unformatted,                                          &
     &   access=access,sequential=sequential,direct=direct,stream=stream,                                &
     &   action=action,read=read,write=write,readwrite=readwrite,                                        &
     !BUG!&   sign=sign,                                                                                      &
     !BUG!&   round=round,                                                                                    &
     &   blank=blank,decimal=decimal,delim=delim,encoding=encoding,pad=pad,                              &
     &   named=named,opened=opened,exist=exist,number=number,pending=pending,asynchronous=asynchronous,  &
     &   iostat=ios,err=999,iomsg=message)
    elseif(namein.ne.'')then
         call journal('sc','*print_inquire* checking file:'//namein)
         inquire(file=namein,                                                                            &
     &   recl=recl,nextrec=nextrec,pos=pos,size=size,                                                    &
     &   position=position,                                                                              &
     &   name=name,                                                                                      &
     &   form=form,formatted=formatted,unformatted=unformatted,                                          &
     &   access=access,sequential=sequential,direct=direct,stream=stream,                                &
     &   action=action,read=read,write=write,readwrite=readwrite,                                        &
     !BUG!&   sign=sign,                                                                                      &
     !BUG!&   round=round,                                                                                    &
     &   blank=blank,decimal=decimal,delim=delim,encoding=encoding,pad=pad,                              &
     &   named=named,opened=opened,exist=exist,number=number,pending=pending,asynchronous=asynchronous,  &
     &   iostat=ios,err=999,iomsg=message)
     if(name.eq.'')name=namein
    else
       call journal('sc','*print_inquire* must specify either filename or unit number')
    endif
!-----------------------------------------------------------------------------------------------------------------------------------
   write(*,nml=inquire,delim='none')
   return
!-----------------------------------------------------------------------------------------------------------------------------------
999   continue
   call journal('sc','*print_inquire* bad inquire')
!  If an error condition occurs during execution of an INQUIRE  statement,
!  all of the inquiry identifiers except ios become undefined.
   call journal('sc','*print_inquire* inquire call failed,iostat=',ios,'message=',message)
end subroutine print_inquire
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    separator(3f) - [M_io:ENVIRONMENT] try to determine pathname directory separator character
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function separator() result(sep)
!!
!!     character(len=1) :: sep
!!
!!##DESCRIPTION
!!
!!    Try to determine the separator character used to separate directory
!!    names from file basenames. It is assumed it is either a backslash or
!!    a slash character.
!!
!!    First, the environment variables PATH, HOME, PWD, and  SHELL  are
!!    examined for a backslash, then a slash.
!!
!!    Then, using the name the program was invoked with, then an INQUIRE(3f)
!!    of that name, then ".\NAME" and "./NAME" try to find an expected
!!    separator character.
!!
!!    Can be very system dependent. If the queries fail the default returned
!!    is "/".
!!
!!    The value is cached as a return value for subsequent calls.
!!
!!##EXAMPLE
!!
!!   sample usage
!!
!!    program demo_separator
!!    use M_io, only : separator
!!    implicit none
!!       write(*,*)'separator=',separator()
!!    end program demo_separator
function separator2() result(sep)

! use the pathname returned as arg0 to determine pathname separator
implicit none
character(len=:),allocatable :: arg0
integer                      :: arg0_length
integer                      :: ios
logical                      :: existing
character(len=1)             :: sep
character(len=1),save        :: sep_cache=' '
character(len=4096)          :: name
character(len=:),allocatable :: fname
character(len=:),allocatable :: envnames(:)
integer                      :: i

   if(sep_cache.ne.' ')then  ! use cached value. NOTE:  A parallel code might theoretically use multiple OS
         sep=sep_cache
         return
   endif

   TESTS: block

   ! check variables names common to many platforms that usually have a directory path in them
   envnames=[character(len=10) :: 'PATH', 'HOME','PWD','SHELL']
   ! check PATH variable for slash or backslash
   do i=1,size(envnames)
      if(index(get_env(envnames(i)),'\').ne.0)then
         sep='\'
         exit TESTS
      elseif(index(get_env(envnames(i)),'/').ne.0)then
         sep='/'
         exit TESTS
      endif
   enddo

   ! get argument name ARG0, although this may be just the command verb or nothing at all
   arg0_length=0
   name=' '
   call get_command_argument(0,length=arg0_length,status=ios)
   if(allocated(arg0))deallocate(arg0)
   allocate(character(len=arg0_length) :: arg0)
   call get_command_argument(0,arg0,status=ios)

   if(index(arg0,'\').ne.0)then
      sep='\'
      exit TESTS
   elseif(index(arg0,'/').ne.0)then
      sep='/'
      exit TESTS
   endif

   ! used to try './' and '.\' but exist test on some systems only returns true
   ! for a regular file so directory names always fail; although this can cause
   ! problems if trying to see if a filename is unused (the reverse is true in
   ! that you think a data file exists that is actually a directory!)

   ! try name returned by INQUIRE(3f) of arg0, as some PE will give canonical name
   existing=.false.
   name=' '
   inquire(file=arg0,iostat=ios,name=name)
   if(ios.eq.0)then
      if(index(name,'\').ne.0)then
         sep='\'
         exit TESTS
      elseif(index(name,'/').ne.0)then
         sep='/'
         exit TESTS
      endif
   endif

   ! well, try some common syntax and assume arg0 is in current directory
   ! could try opening a file assuming in a directory with write permission
   ! or can open /tmp/unique_file_name can be opened, which does on any Unix-Like System I know of
   fname='.\'//arg0
   inquire(file=fname,iostat=ios,exist=existing)
   if(ios.eq.0)then
      if(existing)then
         sep='\'
         exit TESTS
      endif
   endif
   fname='./'//arg0
   inquire(file=fname,iostat=ios,exist=existing)
   if(ios.eq.0)then
      if(existing)then
         sep='/'
         exit TESTS
      endif
   endif

   ! used to then call which(arg0) and see if could find pathname but now it calls this function

   ! used to then try to open "/tmp/UNIQUE_NAME" and assume "/" if successful, as any normal ULS has /tmp.
   ! but now some systems allow a "/" in a filename

   inquire(file='///',iostat=ios,exist=existing) ! on POSIX systems this is the same as '/'.
   if(ios.eq.0)then
      if(existing)then
         sep='/'
         exit TESTS
      endif
   endif

   endblock TESTS

   if(sep.eq.' ')then
      write(*,*)'<WARNING>unknown system directory path separator, defaulting to slash ("/")'
      sep='\'
   endif

   sep_cache=sep
end function separator2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function separator() result(sep)
!>
!!##NAME
!!    separator(3f) - [M_io:ENVIRONMENT] try to determine pathname directory separator character
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function separator() result(sep)
!!
!!     character(len=1) :: sep
!!
!!##DESCRIPTION
!!   First testing for the existence of "/.",  then if that fails a list
!!   of variable names assumed to contain directory paths {PATH|HOME} are
!!   examined first for a backslash, then a slash.  Assuming basically the
!!   choice is a ULS or MSWindows system, and users can do weird things like
!!   put a backslash in a ULS path and break it.
!!
!!   Therefore can be very system dependent. If the queries fail the
!!   default returned is "/".
!!
!!##EXAMPLE
!!
!!   sample usage
!!
!!    program demo_separator
!!    use M_io, only : separator
!!    implicit none
!!       write(*,*)'separator=',separator()
!!    end program demo_separator

! use the pathname returned as arg0 to determine pathname separator
implicit none
integer                      :: ios
integer                      :: i
logical                      :: existing=.false.
character(len=1)             :: sep
!*!IFORT BUG:character(len=1),save        :: sep_cache=' '
integer,save                 :: isep=-1
character(len=4096)          :: name
character(len=:),allocatable :: envnames(:)

    ! NOTE:  A parallel code might theoretically use multiple OS
    !*!FORT BUG:if(sep_cache.ne.' ')then  ! use cached value.
    !*!FORT BUG:    sep=sep_cache
    !*!FORT BUG:    return
    !*!FORT BUG:endif
    if(isep.ne.-1)then  ! use cached value.
        sep=char(isep)
        return
    endif
    FOUND: block
    ! simple, but does not work with ifort
    ! most MSWindows environments see to work with backslash even when
    ! using POSIX filenames to do not rely on '\.'.
    inquire(file='/.',exist=existing,iostat=ios,name=name)
    if(existing.and.ios.eq.0)then
        sep='/'
        exit FOUND
    endif
    ! check variables names common to many platforms that usually have a
    ! directory path in them although a ULS file can contain a backslash
    ! and vice-versa (eg. "touch A\\B\\C"). Removed HOMEPATH because it
    ! returned a name with backslash on CygWin, Mingw, WLS even when using
    ! POSIX filenames in the environment.
    envnames=[character(len=10) :: 'PATH', 'HOME']
    do i=1,size(envnames)
       if(index(get_env(envnames(i)),'\').ne.0)then
          sep='\'
          exit FOUND
       elseif(index(get_env(envnames(i)),'/').ne.0)then
          sep='/'
          exit FOUND
       endif
    enddo

    write(*,*)'<WARNING>unknown system directory path separator'
    sep='\'
    endblock FOUND
    !*!IFORT BUG:sep_cache=sep
    isep=ichar(sep)
end function separator
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    read_table(3f) - [M_io] read file containing a table of numeric values
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine read_table(filename,array,ierr)
!!
!!    character(len=*),intent(in)              :: filename
!!    integer,allocatable,intent(out)          :: array(:,:)
!!       or
!!    real,allocatable,intent(out)             :: array(:,:)
!!       or
!!    doubleprecision,allocatable,intent(out)  :: array(:,:)
!!    integer,intent(out)                      :: ierr
!!
!!##DESCRIPTION
!!    Read a table from a file that is assumed to be columns of
!!    space-delimited numbers, with each row containing the same
!!    number of values
!!
!!##OPTIONS
!!    filename   filename to read
!!    array      array to create. May be INTEGER, REAL, or DOUBLEPRECISION
!!    ierr       zero if no error occurred.
!!##EXAMPLES
!!
!!    Sample program, assuming the input file "inputfile" exists:
!!
!!     program demo_read_table
!!     use M_io, only : read_table
!!     implicit none
!!     doubleprecision,allocatable :: array(:,:)
!!     integer :: i, ierr
!!
!!     ! create test file
!!     open(file='inputfile',unit=10)
!!     write(10,'(a)') '1 10  45'
!!     write(10,'(a)') '10 10  45'
!!     write(10,'(a)') '  2 20  15'
!!     write(10,'(a)') ' 20.345 20  15'
!!     write(10,'(a)') '  3 30.111   0'
!!     write(10,'(a)') '30 30e3   0'
!!     write(10,'(a)') '  4 300.444e-1 -10'
!!     write(10,'(a)') '40 30.5555d0 -10'
!!     write(10,'(a)') '  4 300.444E-1 -10'
!!     write(10,'(a)') '40 30.5555D0 -10'
!!     close(unit=10)
!!
!!     ! read file as a table
!!     call read_table('inputfile',array,ierr)
!!
!!     ! print values
!!     write(*,*)'size=       ',size(array)
!!     write(*,*)'size(dim=1)=',size(array,dim=1)
!!     write(*,*)'size=(dim=2)',size(array,dim=2)
!!     do i=1,size(array,dim=1)
!!        write(*,*)array(i,:)
!!     enddo
!!
!!     ! remove sample file
!!     open(file='inputfile',unit=10)
!!     close(unit=10,status='delete')
!!
!!     end program demo_read_table
!!
!!   Results:
!!
!!     size=          30
!!     size(dim=1)=   10
!!     size(dim=2)=    3
!!       1.0000000000000000        10.000000000000000        45.000000000000000
!!       10.000000000000000        10.000000000000000        45.000000000000000
!!       2.0000000000000000        20.000000000000000        15.000000000000000
!!       20.344999999999999        20.000000000000000        15.000000000000000
!!       3.0000000000000000        30.111000000000001        0.0000000000000000
!!       30.000000000000000        30000.000000000000        0.0000000000000000
!!       4.0000000000000000        30.044400000000000       -10.000000000000000
!!       40.000000000000000        30.555499999999999       -10.000000000000000
!!       4.0000000000000000        30.044400000000000       -10.000000000000000
!!       40.000000000000000        30.555499999999999       -10.000000000000000
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine read_table_d(filename,darray,ierr)
implicit none
! note the array is allocated as text, and then doubleprecision, and then placed in the output array.
! for large files it would be worth it to just determine the file size and allocate and fill the output
! array

character(len=*),intent(in)             :: FILENAME
doubleprecision,allocatable,intent(out) :: darray(:,:)
integer,intent(out)                     :: ierr

character(len=1),allocatable :: text(:) ! array to hold file in memory
integer                      :: length
integer                      :: irows
integer                      :: icols
integer                      :: nchars
integer                      :: i
integer                      :: j
integer                      :: k
integer                      :: istart
character(len=:),allocatable :: line

    call slurp(FILENAME,text,lines=irows,length=length) ! allocate character array and copy file into it
    nchars=size(text)
    ierr=0

    if(.not.allocated(text))then
       write(*,*)'*read_table* failed to load file '//FILENAME
       ierr=-1
    else
       if(allocated(line))deallocate(line)
       allocate(character(len=length) :: line)
       ! find number of values on first line and assume this is constant
       line(:)=''
       do i=1,nchars
          if(text(i).eq.NEW_LINE('A'))then
             exit
          endif
          if(text(i).eq.char(9))then
             line(i:i)=' '
          else
             line(i:i)=text(i)
          endif
       enddo
       icols=size(s2vs(line))
       if(allocated(darray))deallocate(darray)
       allocate(darray(irows,icols))

       darray=0.0d0
       istart=1
       do j=1,irows
          k=0
          line(:)=''
          do i=istart,nchars
             if(text(i).eq.NEW_LINE('A').or.i.eq.nchars)then
                exit
             endif
             k=k+1
             if(text(i).eq.char(9))then
                line(k:k)=' '
             else
                line(k:k)=text(i)
             endif
          enddo
          istart=i+1
          darray(j,:)=s2vs(line)
       enddo
       deallocate(text)  ! release memory
    endif

end subroutine read_table_d
!===================================================================================================================================
subroutine read_table_i(filename,array,ierr)
implicit none
character(len=*),intent(in)             :: FILENAME
integer,allocatable,intent(out)         :: array(:,:)
integer,intent(out)                     :: ierr
doubleprecision,allocatable             :: darray(:,:)
call read_table_d(filename,darray,ierr)
array=nint(darray)
end subroutine read_table_i
!===================================================================================================================================
subroutine read_table_r(filename,array,ierr)
implicit none
character(len=*),intent(in)             :: FILENAME
real,allocatable,intent(out)            :: array(:,:)
integer,intent(out)                     :: ierr
doubleprecision,allocatable             :: darray(:,:)
call read_table_d(filename,darray,ierr)
array=real(darray)
end subroutine read_table_r
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    swallow(3f) - [M_io] read a file into a character array line by line
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine swallow(filename,pageout)
!!
!!    character(len=*),intent(in) :: filename
!!      or
!!    integer,intent(in)          :: io
!!
!!    character(len=:),allocatable,intent(out) :: pageout(:)
!!##DESCRIPTION
!!    Read an entire file into memory as a character array, one character
!!    variable per line.
!!
!!    NOTE:
!!
!!    Never casually read an entire file into memory if you can process it
!!    per line or in smaller units; as large files can consume unreasonable
!!    amounts of memory.
!!
!!##OPTIONS
!!    filename   filename to read into memory, or LUN (Fortran Logical Unit Number).
!!               If filename is a LUN, file must be opened with
!!
!!                  form='unformatted',access='stream'
!!
!!               as in
!!
!!                 open(unit=igetunit, file=filename,     &
!!                 & action="read", iomsg=message,        &
!!                 & form="unformatted", access="stream", &
!!                 & status='old',iostat=ios)
!!
!!    pageout    array of characters to hold file
!!
!!##EXAMPLES
!!
!!   Sample program
!!
!!    program demo_swallow
!!    use M_io,      only : swallow
!!    implicit none
!!    character(len=4096)          :: FILENAME   ! file to read
!!    character(len=:),allocatable :: pageout(:) ! array to hold file in memory
!!    integer                      :: longest, lines, i
!!       ! get a filename
!!       call get_command_argument(1, FILENAME)
!!       ! allocate character array and copy file into it
!!       call swallow(FILENAME,pageout)
!!       if(.not.allocated(pageout))then
!!          write(*,*)'*demo_swallow* failed to load file '//FILENAME
!!       else
!!          ! write file from last line to first line
!!          longest=len(pageout)
!!          lines=size(pageout)
!!          write(*,*)'number of lines is ',lines
!!          write(*,*)'and length of lines is ',longest
!!          write(*,'(a)')repeat('%',longest+2)
!!          write(*,'("%",a,"%")')(trim(pageout(i)),i=lines,1,-1)
!!          write(*,'(a)')repeat('%',longest+2)
!!          deallocate(pageout)  ! release memory
!!       endif
!!    end program demo_swallow
!!
!!   Given
!!
!!    first line
!!    second line
!!    third line
!!
!!   Expected output
!!
!!     number of lines is 3
!!     and length of lines is 11
!!    %%%%%%%%%%%%%
!!    %third line %
!!    %second line%
!!    %first line %
!!    %%%%%%%%%%%%%
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine swallow(FILENAME,pageout)
implicit none
class(*),intent(in)                      :: FILENAME   ! file to read
character(len=:),allocatable,intent(out) :: pageout(:) ! page to hold file in memory
character(len=1),allocatable             :: text(:)    ! array to hold file in memory

   call slurp(FILENAME,text) ! allocate character array and copy file into it

   if(.not.allocated(text))then
      select type(FILENAME)
       type is (character(len=*)); write(*,*)'*swallow* failed to load file '//FILENAME
       type is (integer);          write(*,'(a,i0)')'*swallow* failed to load file unit ',FILENAME
      end select
   else  ! convert array of characters to array of lines
      pageout=page(text)
      deallocate(text)     ! release memory
   endif

contains
function page(array)  result (table)

! ident_5="@(#)page(3fp): function to copy char array to page of text"

character(len=1),intent(in)  :: array(:)
character(len=:),allocatable :: table(:)
integer                      :: i
integer                      :: linelength
integer                      :: length
integer                      :: lines
integer                      :: linecount
integer                      :: position
integer                      :: sz
!!character(len=1),parameter   :: nl=new_line('A')
character(len=1),parameter   :: nl=char(10)
   lines=0
   linelength=0
   length=0
   sz=size(array)
   do i=1,sz
      if(array(i).eq.nl)then
         linelength=max(linelength,length)
         lines=lines+1
         length=0
      else
         length=length+1
      endif
   enddo
   if(sz.gt.0)then
      if(array(sz).ne.nl)then
         lines=lines+1
      endif
   endif

   if(allocated(table))deallocate(table)
   allocate(character(len=linelength) :: table(lines))
   table(:)=' '

   linecount=1
   position=1
   do i=1,sz
      if(array(i).eq.nl)then
         linecount=linecount+1
         position=1
      elseif(linelength.ne.0)then
         table(linecount)(position:position)=array(i)
         position=position+1
      endif
   enddo
end function page
end subroutine swallow
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    SLURP(3f) - [M_io] read a file into a character array
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine slurp(filename,text)
!!
!!    character(len=*),intent(in) :: filename
!!     or
!!    integer,intent(in)          :: filenumber
!!
!!    character(len=1),allocatable,intent(out) :: text(:)
!!    integer,intent(out),optional :: length
!!    integer,intent(out),optional :: lines
!!##DESCRIPTION
!!    Read an entire file as a stream into memory as an array of single
!!    characters, retaining line end terminators.
!!
!!    NOTE:
!!
!!    Never casually read an entire file into memory if you can process it
!!    per line or in smaller units; as large files can consume unreasonable
!!    amounts of memory.
!!
!!##OPTIONS
!!       filename   filename to read into memory or LUN (Fortran Logical
!!                  Unit Number) If a LUN, file must be opened with
!!
!!                     form='unformatted',access='stream'
!!
!!                  as in
!!
!!                    open(unit=igetunit, file=filename,     &
!!                    & action="read", iomsg=message,        &
!!                    & form="unformatted", access="stream", &
!!                    & status='old',iostat=ios)
!!
!!       text       array of characters to hold file
!!       length     length of longest line read(Optional).
!!       lines      number of lines read(Optional).
!!
!!##EXAMPLES
!!
!!    Sample program, which  creates test input file "inputfile":
!!
!!     program demo_slurp
!!     use M_io, only      : slurp
!!     implicit none
!!     character(len=1),allocatable :: text(:) ! array to hold file in memory
!!     character(len=*),parameter :: FILENAME='inputfile' ! file to read
!!
!!     ! create test file
!!     open(file=FILENAME,unit=10)
!!     write(10,'(a)') new_line('A')//'esrever lliw'
!!     write(10,'(a)') 'margorp elpmas eht taht'
!!     write(10,'(a)') 'elif elpmas a si sihT'
!!     close(unit=10)
!!
!!     call slurp(FILENAME,text) ! allocate character array and copy file into it
!!
!!     if(.not.allocated(text))then
!!        write(*,*)'*rever* failed to load file '//FILENAME
!!     else
!!        ! write file reversed to stdout
!!        write(*,'(*(a:))',advance='no')text(size(text):1:-1)
!!        deallocate(text)  ! release memory
!!     endif
!!
!!     end program demo_slurp
!!
!!    Expected output:
!!
!!     >This is a sample file
!!     >that the sample program
!!     >will reverse
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine slurp(filename,text,length,lines)
implicit none

! ident_6="@(#)M_io::slurp(3f): allocate text array and read file filename into it"

class(*),intent(in)                      :: filename    ! filename to shlep
character(len=1),allocatable,intent(out) :: text(:)     ! array to hold file
integer,intent(out),optional             :: length      ! length of longest line
integer,intent(out),optional             :: lines       ! number of lines
integer :: nchars=0             ! holds size of file
integer :: igetunit             ! use newunit=igetunit in f08
integer :: ios=0                ! used for I/O error status
integer :: length_local
integer :: lines_local
integer :: i
integer :: icount
character(len=256)  :: message
character(len=4096) :: local_filename
   length_local=0
   lines_local=0
   message=''
      select type(FILENAME)
       type is (character(len=*))
          open(newunit=igetunit, file=trim(filename), action="read", iomsg=message,&
           &form="unformatted", access="stream",status='old',iostat=ios)
          local_filename=filename
       type is (integer)
          rewind(unit=filename,iostat=ios,iomsg=message)
          write(local_filename,'("unit ",i0)')filename
          igetunit=filename
      end select
   if(ios.eq.0)then  ! if file was successfully opened
      inquire(unit=igetunit, size=nchars)
      if(nchars.le.0)then
         call stderr_local( '*slurp* empty file '//trim(local_filename) )
         return
      endif
      ! read file into text array
      if(allocated(text))deallocate(text) ! make sure text array not allocated
      allocate ( text(nchars) )           ! make enough storage to hold file
      read(igetunit,iostat=ios,iomsg=message) text      ! load input file -> text array
      if(ios.ne.0)then
         call stderr_local( '*slurp* bad read of '//trim(local_filename)//':'//trim(message) )
      endif
   else
      call stderr_local('*slurp* '//message)
      allocate ( text(0) )           ! make enough storage to hold file
   endif

   close(iostat=ios,unit=igetunit)            ! close if opened successfully or not

   if(present(lines).or.present(length))then  ! get length of longest line and number of lines
      icount=0
      do i=1,nchars
         if(text(i).eq.NEW_LINE('A'))then
            lines_local=lines_local+1
            length_local=max(length_local,icount)
            icount=0
         endif
         icount=icount+1
      enddo
      if(nchars.ne.0)then
         if(text(nchars).ne.NEW_LINE('A'))then
            lines_local=lines_local+1
            length_local=max(length_local,icount)
         endif
      endif
      if(present(lines))lines=lines_local
      if(present(length))length=length_local
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine stderr_local(message)
character(len=*) :: message
   write(error_unit,'(a)')trim(message)    ! write message to standard error
end subroutine stderr_local
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine slurp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    number_of_lines(3f) - [M_io] read an open sequential file to get
!!                          number of lines
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function number_of_lines(lun) result(nlines)
!!
!!    integer,intent(in)          :: lun
!!    integer                     :: nlines
!!
!!##DESCRIPTION
!!    Rewind an open sequential file and read through it to count the number
!!    of lines. The file is rewound on exit.
!!
!!##OPTIONS
!!    lun       logical unit number of open sequential file to count lines in
!!
!!##RETURNS
!!    nlines    number of lines read
!!
!!##EXAMPLES
!!
!!   Sample program
!!
!!    program demo_number_of_lines
!!    use M_io,      only : number_of_lines, fileopen
!!    implicit none
!!    integer :: ios
!!    integer :: lun
!!       lun=fileopen('test.txt','r',ios)
!!       if(ios.eq.0)then
!!          write(*,*) number_of_lines(lun)
!!       else
!!          write(*,*)'ERROR: IOS=',ios
!!       endif
!!    end program demo_number_of_lines
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function number_of_lines(lun) result(nlines)
!@(#) determine number or lines in file given a LUN to the open file
integer,intent(in) :: lun
integer            :: ios
integer            :: nlines
   if(lun.ne.stdin)rewind(lun,iostat=ios)
   nlines = 0
   do
      read(lun, '(A)', iostat=ios)
      if (ios /= 0) exit
      nlines = nlines + 1
   enddo
   if(lun.ne.stdin)rewind(lun,iostat=ios)
end function number_of_lines
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    notopen(3f) - [M_io] Find a FUN/LUN (Fortran-unit-number) that is not in use
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    Usage
!!
!!       integer function notopen(start,end,err)
!!       integer,optional,intent(in)  :: start
!!       integer,optional,intent(in)  :: end
!!       integer,optional,intent(out) :: err
!!##DESCRIPTION
!!    A free FORTRAN unit number is needed to OPEN a file. NOTOPEN() returns
!!    a FORTRAN unit number from START to END not currently associated with
!!    an I/O unit. START and END are expected to be positive integers where
!!    END .ge. START.
!!
!!    If NOTOPEN() returns -1, then no free FORTRAN unit could be found in
!!    the specified range.
!!
!!    Otherwise, NOTOPEN() returns an integer representing a free FORTRAN
!!    logical unit number. Note that NOTOPEN() assumes the following unit
!!    numbers defined by the Fortran 2008 ISO_FORTRAN_ENV module
!!
!!       ERROR_UNIT,INPUT_UNIT,OUTPUT_UNIT
!!
!!    are special, and will never return those values.
!!
!!##OPTIONS
!!       start  optional logical unit number to start scan at, defaults to 10.
!!       end    optional logical unit number to stop scan at, defaults to 99.
!!       err    optional error flag returned. ERR will be non-zero if no errors.
!!              If not present and an error occurs the program will stop instead
!!              of returning.
!!
!!##NOTES
!!
!!    Why are the default START and END limits from 10 to 99? the Fortran 77
!!    standard did not specify a specific limit on the upper range limit, but
!!    the LUN range of 1 to 99 was almost always supported in conventional
!!    programming environments. Additionally, units in the range 0-10 have
!!    often been the units used for pre-assigned files. Occasionally 100,
!!    101 and 102 are reserved (for files such as standard input, standard
!!    output, standard error, ...). Therefore, the defaults for START and
!!    END were selected to be 10 and 99. And most programs do not need
!!    more than 90 files simultaneously open, so the defaults work well in
!!    practice with many versions/vintages of Fortran.
!!
!!    Note that an environment may impose a limit on the number of
!!    simultaneously open files (which some compilers work around).
!!
!!    Beginning with f2008, you can probably use OPEN(NEWUNIT=...) instead
!!    of an open unit locator.
!!
!!##EXAMPLE
!!
!!
!!    Sample program:
!!
!!     program demo_notopen ! test the NOTOPEN(3f) function
!!     use m_io, only: notopen
!!     implicit none
!!     integer :: ii, ierr, igot
!!
!!     write(*,*)'check for preassigned files from unit 0 to unit 1000'
!!     write(*,*)'(5 and 6 always return -1)'
!!
!!     do ii=0,1000
!!        if(notopen(ii,ii,ierr) .ne. ii)then
!!           write(*,*)'INUSE:',ii, notopen(ii,ii,ierr)
!!        endif
!!     enddo
!!
!!     ! open all files from UNIT=10 to UNIT=30 so have used units
!!     do ii=10,30,1
!!       open(unit=ii,status="scratch")
!!     enddo
!!     ! close UNIT=25
!!     close(25)
!!
!!     ! find open file in range 10 to 30
!!     write(*,*)'Should get 25 for this ..',notopen(10,30,ierr)
!!
!!     close(18)
!!     do ii=10,32
!!       igot=notopen(ii,ii,ierr)
!!       write(*,*)'For unit ',ii,' I got ',igot,' with ERR=',ierr
!!     enddo
!!
!!     end program demo_notopen
!!
!!    Expected output(can vary with each programming environment):
!!
!!       check for preassigned files from unit 0 to unit 1000
!!       (5 and 6 always return -1)
!!       INUSE:    0    -1
!!       INUSE:    5    -1
!!       INUSE:    6    -1
!!       Should get 25 for this .. 25
!!       For  unit  10  I  got  -1  with  ERR=  -1
!!       For  unit  11  I  got  -1  with  ERR=  -1
!!       For  unit  12  I  got  -1  with  ERR=  -1
!!       For  unit  13  I  got  -1  with  ERR=  -1
!!       For  unit  14  I  got  -1  with  ERR=  -1
!!       For  unit  15  I  got  -1  with  ERR=  -1
!!       For  unit  16  I  got  -1  with  ERR=  -1
!!       For  unit  17  I  got  -1  with  ERR=  -1
!!       For  unit  18  I  got  18  with  ERR=   0
!!       For  unit  19  I  got  -1  with  ERR=  -1
!!       For  unit  20  I  got  -1  with  ERR=  -1
!!       For  unit  21  I  got  -1  with  ERR=  -1
!!       For  unit  22  I  got  -1  with  ERR=  -1
!!       For  unit  23  I  got  -1  with  ERR=  -1
!!       For  unit  24  I  got  -1  with  ERR=  -1
!!       For  unit  25  I  got  25  with  ERR=   0
!!       For  unit  26  I  got  -1  with  ERR=  -1
!!       For  unit  27  I  got  -1  with  ERR=  -1
!!       For  unit  28  I  got  -1  with  ERR=  -1
!!       For  unit  29  I  got  -1  with  ERR=  -1
!!       For  unit  30  I  got  -1  with  ERR=  -1
!!       For  unit  31  I  got  31  with  ERR=   0
!!       For  unit  32  I  got  32  with  ERR=   0
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
integer function notopen(start,end,err)
implicit none

! ident_7="@(#)M_io::notopen(3f): find free FORTRAN unit number to OPEN() a file"

integer,optional,intent(in)    :: start                           ! unit number to start looking at
integer,optional,intent(in)    :: end                             ! last unit number to look at
integer,optional,intent(out)   :: err                             ! error flag returned
integer                        :: istart
integer                        :: iend
integer                        :: ierr

integer         :: i10                                            ! counter from start to end
integer         :: ios                                            ! iostatus from INQUIRE
logical         :: lopen                                          ! returned from INQUIRE
logical         :: lexist                                         ! returned from INQUIRE
!-----------------------------------------------------------------------------------------------------------------------------------
   !! IEND=MERGE( END, 99, PRESENT(END)) do not use merge, as TSOURCE must be evaluated before the call
   if(present(start))then; istart=start; else; istart=10; endif
   if(present(end  ))then; iend  =end  ; else; iend  =99; endif
   ierr=0
   notopen=(-1)                                                   ! result if no units are available
!-----------------------------------------------------------------------------------------------------------------------------------
   do i10=istart,iend                                             ! check units over selected range
      select case (i10)                                           ! always skip these predefined units
      case(error_unit,input_unit,output_unit)
          cycle
      end select
      inquire( unit=i10, opened=lopen, exist=lexist, iostat=ios )
      if( ios == 0 )then                                          ! no error on inquire
         if(.not. lopen .and. lexist)then                         ! if unit number not in use, return it
            notopen = i10
            exit                                                  ! only need to find one, so return
         endif
      else
         write(error_unit,*)'*notopen*:error on unit ',i10,'=',ios
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   if (notopen .lt. 0 )then                                       ! no valid unit was found in given range
      ierr=-1
   else                                                           ! valid value being returned
      ierr=0
   endif
   if(present(err))then                                           ! if error flag is present set it
      err=ierr
   elseif(ierr.ne.0)then                                          ! if error occurred and error flag not present stop program
      stop 1
   endif
end function notopen
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    dirname(3f) - [M_io] strip last component from filename
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function dirname(FILENAME) result (DIRECTORY)
!!
!!      character(len=*),intent(in)  :: FILENAME
!!      character(len=:),allocatable :: DIRECTORY
!!
!!##DESCRIPTION
!!    Output FILENAME with its last non-slash component and trailing slashes
!!    removed. If FILENAME contains no slash or backslash character, output
!!    '.' (meaning the current directory).
!!
!!    Assumes leaf separator is a slash or backslash as determined by
!!    separator(3f) and that FILENAME does not contain trailing spaces.
!!
!!##OPTIONS
!!      FILENAME   pathname to remove the last leaf from
!!
!!##RETURNS
!!      DIRECTORY  directory name for pathname
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_dirname
!!    use M_io, only : dirname
!!    implicit none
!!    character(len=:),allocatable :: filename
!!    integer                      :: filename_length
!!    integer                      :: i
!!    ! get pathname from command line arguments
!!    do i = 1 , command_argument_count()
!!       call get_command_argument (i , length=filename_length)
!!       if(allocated(filename))deallocate(filename)
!!       allocate(character(len=filename_length) :: filename)
!!       call get_command_argument (i , value=filename)
!!       write(*,'(a)')dirname(filename)
!!       deallocate(filename)
!!    enddo
!!    end program demo_dirname
!!
!!   Sample program executions:
!!
!!      demo_dirname /usr/bin/          -> "/usr"
!!      demo_dirname dir1/str dir2/str  -> "dir1" followed by "dir2"
!!      demo_dirname stdio.h            -> "."
!!
!!##SEE ALSO
!!    dirname(3c), basename(3c), readlink(3c), realpath(3c)
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        dirname(3f)
!! DESCRIPTION:    strip last component from filename
!!##VERSION:        1.0.0
!!##DATE:           2015-06-26
!! AUTHOR:         John S. Urban
!! REPORTING BUGS: http://www.urbanjost.altervista.org/
!! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
function dirname(filename) result (directory)
implicit none

! ident_8="@(#)M_io::dirname(3f): strip last component from filename"

character(len=*),intent(in)      :: filename
character(len=:),allocatable     :: directory
integer                          :: iend
character(len=1)                 :: sep
!-----------------------------------------------------------------------------------------------------------------------------------
   sep=separator()
   directory=trim(filename)
   call removetail()                         ! trim trailing slashes even if duplicates
   iend=index(directory,sep,back=.true.)     ! find last slash if any
   if(iend.eq.0)then                         ! filename is a leaf
      directory='.'                          ! special case
   else
      directory=directory(:iend-1)           ! remove leaf
      call removetail()                      ! trim off trailing slashes in case duplicates
   endif
   directory=trim(directory)                 ! clean up return value
contains
   subroutine removetail()              ! replace trailing slashes with spaces even if duplicates
   integer :: right
   do right=len(directory),1,-1
      if(directory(right:right).eq.sep.or.directory(right:right).eq.' ')then
         directory(right:right)=' '
      else
         exit
      endif
   enddo
   end subroutine removetail

end function dirname
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    basename(3f) - [M_io] return last component from filename
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function basename(FILENAME,SUFFIX) result (LEAF)
!!
!!      character(len=:),allocatable :: FILENAME
!!      character(len=*),intent(in),optional :: SUFFIX
!!      character(len=*),intent(in) :: LEAF
!!
!!##DESCRIPTION
!!    Output LEAF of filename with directory paths removed.
!!
!!    Assumes leaf separator is a slash or backslash as determined by
!!    separator(3f) and that filename does not contain trailing spaces.
!!
!!##OPTIONS
!!      FILENAME   pathname to extract the last leaf from
!!      SUFFIX     suffix to remove. If not present
!!                 the rightmost ".string" string is removed.
!!                 If present the LEAF is returned with any matching
!!                 suffix removed.
!!
!!##RETURNS
!!      LEAF  returned leaf name
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_basename
!!    use M_io, only : basename
!!    implicit none
!!    character(len=:),allocatable :: fn
!!    integer                      :: filename_length
!!    integer                      :: i
!!    ! get pathname from command line arguments
!!    do i = 1, command_argument_count()
!!       call get_command_argument (i, length=filename_length)
!!       if(allocated(fn))deallocate(fn)
!!       allocate(character(len=filename_length) :: fn)
!!       call get_command_argument (i, value=fn)
!!       ! leaf with any suffix removed
!!       ! leaf with suffix retained
!!       ! with suffix unless it is ".f90"
!!       write(*,'(*(a,1x))') basename(fn), basename(fn,''), basename(fn,'.f90')
!!       deallocate(fn)
!!    enddo
!!    end program demo_basename
!!
!!   Sample program executions:
!!
!!     $demo_basename /usr/bin/
!!     bin bin bin
!!     $demo_basename dir1/fred.x dir2/.y
!!     fred fred.x fred.x
!!     .y .y .y
!!     $demo_basename stdio.h
!!     stdio stdio.h stdio.h
!!     $demo_basename /name.f90
!!     name name.f90 name
!!##SEE ALSO
!!    basename(3c), basename(3c), readlink(3c), realpath(3c)
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        basename(3f)
!! DESCRIPTION:    strip last component from filename
!!##VERSION:        1.0.0
!!##DATE:           2015-06-26
!! AUTHOR:         John S. Urban
!! REPORTING BUGS: http://www.urbanjost.altervista.org/
!! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
function basename(filename,suffix) result (leaf)
implicit none

! ident_9="@(#)M_io::basename(3f): strip last component from filename"

character(len=*),intent(in)          :: filename
character(len=*),intent(in),optional :: suffix
character(len=:),allocatable         :: leaf
integer                              :: iend
integer                              :: i
integer,parameter                    :: maxlen=4096
character(len=maxlen)                :: name
character(len=maxlen)                :: bname
character(len=maxlen)                :: extension
character(len=1)                 :: sep
   sep=separator()
   iend=len_trim(filename)
   do i=iend,1,-1
      if(filename(i:i).ne.sep)exit
      iend=iend-1
   enddo
   call splitpath(filename(:iend),name=name,basename=bname,ext=extension)
   if(present(suffix))then
      leaf=merge(bname,name,suffix.eq.extension)
   else
      leaf=bname
   endif
   if(leaf.eq.'')leaf=name
   leaf=trim(leaf)
end function basename
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fileopen(3f) - [M_io] A simple open of a sequential file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   function fileopen(filename,mode,ios) result(lun)
!!
!!    character(len=*),intent(in)           :: filename
!!    character(len=*),intent(in),optional  :: mode
!!    integer,intent(out),optional          :: ios
!!    integer                               :: lun
!!
!!##DESCRIPTION
!!    fileopen(3f) is a convenience routine that allows you to open a file
!!    for sequential reading and writing as a text file in a form commonly
!!    found in C and interpreted languages such as shells. See the OPEN(3f)
!!    statement for more demanding I/O specifications (asynchronous, direct,
!!    unformatted, ... ). The documentation for the flexible and powerful
!!    OPEN(3f) statement can be a bit overwhelming; this routine cuts it
!!    down to the just the simple basic functions typically available in
!!    a scripting language such as bash, tcsh, sh, ...
!!
!!    Specify the file's name as the string FILENAME with a shell-like prefix
!!    specifying the access mode, or alternatively specify a plain FILENAME
!!    and the kind of access you need to the file with the string MODE.
!!
!!    Three fundamental kinds of access are available: read, write,
!!    and append.
!!
!!##OPTION
!!   FILENAME  The filename to open. If the beginning of the filename is
!!
!!             <   open for read. File must exist
!!             >   open for write. Will overwrite current file
!!             >>  open for append. Will append to current file
!!
!!             If no prefix exists to specify a file access mode, it
!!             will depend on the values of the MODE argument (meaning
!!             the default will be "readwrite").
!!
!!             A blank filename causes a unit number for a scratch file
!!             to be returned.
!!
!!   MODE     [rwa][tb][+]
!!            An alternate way to specify the file access mode is to specify
!!            a MODE value. It should begin with one of the three characters
!!            "r", "w", or "a". It defaults to 'rw'. It is case-insensitive.
!!
!!
!!        READING PREFIX
!!        r,<   Open the file for reading; the operation will fail if the
!!              file does not exist, or if the host system does not permit
!!              you to read it.
!!
!!        WRITING PREFIXES
!!        w,>   Open a file for writing from the beginning of the file.
!!              If the file whose name you specified already existed,
!!              the call fails.
!!
!!        o     Open the file for writing from the beginning of the file:
!!              effectively, this always creates a new file. If the file
!!              whose name you specified already existed, its old contents
!!              are discarded.
!!
!!        a,<<  Initially open the file for appending data (ie. writing
!!              at the end of file).
!!
!!        SUFFIX
!!
!!        b   Append a "b" to any of the three modes above to specify that
!!            you are opening the file as a "binary file" (the default is
!!            to open the file as a sequential formatted text file. This
!!            switch changes to to an unformatted stream).
!!
!!                   open( ... access='stream';form='unformatted')
!!
!!        t   Append a "t" to any of the three modes (rwa) to specify a
!!            formatted stream
!!
!!                   open( ... access='stream';form='formatted')
!!
!!        +   Finally, you might need to both read and write from the same
!!            file. You can specify "rw" or you can append a `+' to any of
!!            the three primary modes ("rwa") to permit "readwrite" access
!!
!!        v   Additionally, "v" selects verbose mode, which prints the
!!            OPEN(3f) options explicitly selected
!!
!!        NOTES
!!
!!            If you want to append both `b' and `+', you can do it in
!!            either order: for example, "rb+" means the same thing as
!!            "r+b" when used as a mode string.)
!!
!!    IOS    The error code returned by the OPEN(3f) statement ultimately
!!           executed by this function. If not present the program stops on
!!           an error.
!!##RETURNS
!!        FILEOPEN(3f) returns a Fortran unit number which you can use for other file
!!        operations, unless the file you requested could not be opened;
!!        in that situation, the result is -1 (a reserved value that cannot be returned
!!        as a NEWUNIT value on an OPEN(3f)) and IOS will be non-zero.
!!
!!##EXAMPLE
!!
!!  Common usage
!!
!!   READ
!!     R=fileopen('<in.txt')
!!     or
!!     R=fileopen('in.txt','r')
!!
!!   WRITE
!!     W=fileopen('>out.txt')
!!     or
!!     W=fileopen('out.txt','W')
!!
!!   READWRITE
!!     RW=fileopen('inout.txt')
!!
!!   APPEND
!!     A=fileopen('>>inout.txt')
!!     or
!!     A=fileopen('inout.txt','a')
!!
!!   Sample program
!!
!!       program demo_fileopen
!!       use M_io, only : fileopen, fileclose, print_inquire
!!       implicit none
!!       integer :: lun
!!       lun=fileopen('fred.txt')
!!       call print_inquire(lun)
!!       end program demo_fileopen
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function fileopen(filename,mode,ios) result(lun)
character(len=*),intent(in)           :: filename
character(len=*),intent(in),optional  :: mode
integer,intent(out),optional          :: ios
integer                               :: lun, i, ios_local,ifound,gts
character(len=:),allocatable          :: local_mode
character(len=256)                    :: message
character(len=:),allocatable          :: action, position, access, form, status, file
logical                               :: verbose
   local_mode=lower(merge_str(mode,'',present(mode)))
   file=trim(adjustl(filename))//'   '
   ifound=index(file,'>>')
   if(ifound.ne.0)then
      file(ifound:ifound+1)='  '
      local_mode=local_mode//'a'
   endif
   ifound=index(file,'>')
   if(ifound.ne.0)then
      file(ifound:ifound)=' '
      local_mode=local_mode//'w'
   endif
   ifound=index(file,'<')
   if(ifound.ne.0)then
      file(ifound:ifound)=' '
      local_mode=local_mode//'r'
   endif
   file=adjustl(file)
   local_mode=merge_str('rw',local_mode,local_mode.eq.'')
   file=trim(file)

   gts=0
   action=''
   position='asis'
   form='formatted'
   access='sequential'
   status='unknown'
   verbose=.false.
   do i=1,len(local_mode) ! create order independence
      select case(local_mode(i:i))
       case('r','<'); if(action.ne.'readwrite'.and.action.ne.'read')action='read'//action
                      if(status.eq.'unknown')status='old'
       case('w','>'); if(action.ne.'readwrite'.and.action.ne.'write')action=action//'write'
                      if(status=='unknown')status='new'
                      if(gts.gt.0)then
                         position='append'
                      endif
                      gts=gts+1
       case('o');     if(action.ne.'readwrite'.and.action.ne.'write')action=action//'write'
                      if(status=='unknown')then
                         status='replace'
                      endif
       case('a');     position='append'
                      if(action.ne.'readwrite'.and.action.ne.'write')action=action//'write'
                      if(status.eq.'old')status='unknown'
       case('b');     access='stream';form='unformatted'
       case('t');     access='stream';form='formatted'
       case('+');     action='readwrite'
                      status='unknown'
       case('v');     verbose=.true.
       case default
         write(*,'(*(g0))',advance='no')'*fileopen* unknown mode key ',local_mode(i:i)
         write(*,'(*(:,"[",g0,"=",g0,"]"))',advance='no') &
         & ' INPUTNAME=',trim(file), &
         & ' MODE=',trim(local_mode)
      end select
   enddo
   if(action.eq.'')action='readwrite'

   if(verbose)then
      write(*,'(*(:,"[",g0,"=",g0,"]"))',advance='no') &
         & 'INPUTNAME=',trim(file), &
         & 'MODE=',trim(local_mode)
      write(*,'(a)',advance='no')'==>'
      write(*,'(*(:,"[",g0,"=",g0,"]"))') &
         & 'FILE=',trim(file), &
         & 'FORM=',trim(form), &
         & 'ACCESS=',trim(access), &
         & 'ACTION=',trim(action), &
         & 'POSITION=',trim(position), &
         & 'STATUS=',trim(status)
   endif
   if(file.ne.' ')then
    open(file=file,newunit=lun,form=form,access=access,action=action,position=position,status=status,iostat=ios_local,iomsg=message)
   else
    open(newunit=lun,form=form,access=access,action=action,status='scratch',iostat=ios_local,iomsg=message)
   endif
   !  ACCESS    =  SEQUENTIAL  |  DIRECT       |  STREAM
   !  ACTION    =  READ|WRITE  |  READWRITE
   !  FORM      =  FORMATTED   |  UNFORMATTED
   !  POSITION  =  ASIS        |  REWIND       |  APPEND
   !  STATUS    =  NEW         |  REPLACE      |  OLD     |  SCRATCH   | UNKNOWN
   if(ios_local.ne.0)then
      call journal('sc','*fileopen* ',message)
      lun=-1
   endif
   if(present(ios))then        ! caller has asked for status so let caller process any error
      ios=ios_local
   elseif(ios_local.ne.0)then  ! caller did not ask for status so stop program on error
      stop 1
   endif
end function fileopen
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    fileclose(3f) - [M_io] A simple close of a sequential file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     function fileclose(lun) result(ios)
!!
!!      integer,intent(in)       :: lun
!!      integer                  :: ios
!!##DESCRIPTION
!!   A convenience command for closing a file that leaves an
!!   error message in the current journal file if active.
!!##OPTION
!!   LUN unit number to close
!!##RETURNS
!!   IOS status value from CLOSE
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_fileclose
!!     use M_io, only : fileclose, fileopen
!!     implicit none
!!     integer :: lun
!!     integer :: ios, ierr
!!        lun=fileopen('<input.txt',ios=ierr)
!!        if(ierr.ne.0)then
!!           write(*,*)'<ERROR> opening file'
!!        endif
!!        ios=fileclose(lun)
!!     end program demo_fileclose
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function fileclose(lun) result(ios)
integer,intent(in)       :: lun
integer                  :: ios
character(len=256)       :: message
   close(unit=lun,iostat=ios,iomsg=message)
   if(ios.ne.0)then
      call journal('sc','*fileclose* ',message)
      stop
   endif
end function fileclose
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    filewrite(3f) - [M_io] A simple write of a CHARACTER array to a file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     function filewrite(filename,data,status) result(ierr)
!!
!!      character(len=*),intent(in) :: filename
!!      character(len=*),intent(in) :: data(:)
!!      character(len=*),intent(in),optional :: status
!!      character(len=*),intent(in),optional :: position
!!      integer                     :: ierr
!!##DESCRIPTION
!!   A convenience procedure for writing a CHARACTER array as
!!   a new file.
!!##OPTION
!!   FILENAME   file to create or write. If the name ends
!!              in ">" the default for STATUS changes to
!!              "REPLACE". If it ends ">>" STATUS changes to
!!              "UNKNOWN" and the default POSTION changes to "APPEND".
!!   DATA       CHARACTER array to write to file
!!   STATUS     STATUS to use on OPEN(7f). Defaults to "NEW"
!!              allowed values are  NEW|REPLACE|OLD|SCRATCH|UNKNOWN
!!   POSITION   POSITION to use of OPEN(7f). Defaults to "REWIND".
!!              allowed values are  ASIS|REWIND|APPEND
!!##RETURNS
!!   IERR       status value. Zero indicates no error occurred
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_filewrite
!!     use M_io, only : filewrite
!!     implicit none
!!     integer :: ierr
!!     character(len=:),allocatable :: data(:)
!!        data=[ character(len=80) :: &
!!             &'This is the text to write  ', &
!!             &'into the file. It will be  ', &
!!             &'trimmed on the right side. ', &
!!             &' ', &
!!             &'     That is all Folks!    ', &
!!             &'']
!!        ierr=filewrite('_scratch.txt',data)
!!     end program demo_filewrite
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function filewrite(filename,filedata,status,position) result (ierr)
! write filedata to file filename
character(len=*),intent(in)           :: filename
character(len=*),intent(in)           :: filedata(:)
character(len=*),intent(in),optional  :: status
character(len=*),intent(in),optional  :: position
integer                               :: ierr
integer                               :: lun, i, ios, ilen
character(len=256)                    :: message
character(len=:),allocatable          :: file
character(len=:),allocatable          :: local_status
character(len=:),allocatable          :: local_position
character(len=:),allocatable          :: default_status
character(len=:),allocatable          :: default_position
   ierr=0
   default_status='NEW'
   default_position='REWIND'
   file=trim(adjustl(filename))//'  '
   ilen=max(len_trim(file),2)
   if(file(ilen-1:ilen).eq.'>>')then
      ilen=ilen-2
      file=file(:ilen)
      default_status='UNKNOWN'
      default_position='APPEND'
   elseif(file(ilen:ilen).eq.'>')then
      ilen=ilen-1
      file=file(:ilen)
      default_status='REPLACE'
   else
      file=trim(file)
   endif
   if(present(position))then; local_position=position; else; local_position=default_position; endif
   if(present(status))then;   local_status=status;     else; local_status=default_status;     endif
   if(file.ne.' ')then
      open(file=file, &
      & newunit=lun, &
      & form='formatted', &         !  FORM      =  FORMATTED   |  UNFORMATTED
      & access='sequential', &      !  ACCESS    =  SEQUENTIAL  |  DIRECT       |  STREAM
      & action='write', &           !  ACTION    =  READ|WRITE  |  READWRITE
      & position=local_position, &  !  POSITION  =  ASIS        |  REWIND       |  APPEND
      & status=local_status, &      !  STATUS    =  NEW         |  REPLACE      |  OLD     |  SCRATCH   | UNKNOWN
      & iostat=ios, &
      & iomsg=message)
   else
      lun=stdout
      ios=0
   endif
   if(ios.ne.0)then
      write(stderr,'(*(a,1x))')'*filewrite* error:',file,trim(message)
      ierr=ios
   else
      do i=1,size(filedata)                                                    ! write file
         write(lun,'(a)',iostat=ios,iomsg=message)trim(filedata(i))
         if(ios.ne.0)then
            write(stderr,'(*(a,1x))')'*filewrite* error:',file,trim(message)
            ierr=ios
            exit
         endif
      enddo
   endif
   close(unit=lun,iostat=ios,iomsg=message)                                 ! close file
   if(ios.ne.0)then
      write(stderr,'(*(a,1x))')'*filewrite* error:',trim(message)
      ierr=ios
   endif
end function filewrite
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    filedelete(3f) - [M_io] A simple close of an open file with STATUS='DELETE'
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function filedelete(lun) result(ios)
!!
!!     integer,intent(in)    :: lun
!!     integer               :: ios
!!
!!##DESCRIPTION
!!   A convenience command for deleting an OPEN(3f) file that leaves an
!!   error message in the current journal file if active.
!!##OPTION
!!   LUN  unit number of open file to delete
!!##RETURNS
!!   IOS  status returned by CLOSE().
!!##EXAMPLE
!!
!!  Sample program:
!!
!!     program demo_filedelete
!!     use M_io, only : filedelete, fileopen
!!     implicit none
!!     integer :: lun
!!     integer :: ios
!!        lun=fileopen('<input.txt')
!!        ios=filedelete(lun)
!!     end program demo_filedelete
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function filedelete(lun) result(ios)
integer,intent(in)    :: lun
integer               :: ios
character(len=256)    :: message
   close(unit=lun,iostat=ios,status='delete',iomsg=message)
   if(ios.ne.0)then
      call journal('sc','*filedelete* ',message)
   endif
end function filedelete
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    joinpath(3f) - [M_io] join parts of a pathname together
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function joinpath(a1,a2,a3,a4,a5,a6,a7,a8,a9)  result(path)
!!
!!     character(len=*), intent(in)           :: a1, a2
!!     character(len=*), intent(in), optional :: a3, a4, a5, a6, a7, a8, a9
!!     character(len=:), allocatable          :: path
!!##DESCRIPTION
!!##OPTIONS
!!     a1,a2  the first two pathname sections to join. Required
!!     a3-a9  additional optional sections to join
!!##RETURNS
!!     pathname sections joined together with trailing spaces removed from the ends
!!     of sections and a separator (as returned by separator(3f) placed between
!!     them, and duplicate adjacent separators removed accept for one beginning the
!!     joined pathname.
!!##EXAMPLE
!!
!!   Sample program
!!
!!      program demo_joinpath
!!      use M_io, only : joinpath
!!      implicit none
!!         write(*,*)joinpath('/share/user','/man/','man3','joinpath.3m_io'//'.gz')
!!      end program demo_joinpath
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function joinpath(a1,a2,a3,a4,a5,a6,a7,a8,a9) result(path)
   ! Construct path by joining strings with os file separator
   !
   character(len=*), intent(in)           :: a1, a2
   character(len=*), intent(in), optional :: a3, a4, a5, a6, a7, a8, a9
   character(len=:), allocatable          :: path
   character(len=1)                       :: filesep

   filesep = separator()
   if(a1.ne.'')then
      path = trim(a1) // filesep // trim(a2)
   else
      path = trim(a2)
   endif
   if (present(a3)) path = path // filesep // trim(a3)
   if (present(a4)) path = path // filesep // trim(a4)
   if (present(a5)) path = path // filesep // trim(a5)
   if (present(a6)) path = path // filesep // trim(a6)
   if (present(a7)) path = path // filesep // trim(a7)
   if (present(a8)) path = path // filesep // trim(a8)
   if (present(a9)) path = path // filesep // trim(a9)
   path=adjustl(path//'  ')
   call substitute(path,filesep//filesep,filesep,start=2) ! some systems allow names starting with '//' or '\\'
   path=trim(path)
end function joinpath
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     splitpath(3f) - [M_io] split a Unix pathname into components
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine splitpath(path,dir,name,basename,ext)
!!
!!    integer,parameter :: maxlen=4096
!!    character(len=maxlen),intent(in)  :: path
!!    character(len=maxlen),intent(out),optional :: dir
!!    character(len=maxlen),intent(out),optional :: name
!!    character(len=maxlen),intent(out),optional :: basename
!!    character(len=maxlen),intent(out),optional :: ext
!!
!!##DESCRIPTION
!!    splitpath(3f) splits given pathname assuming a forward slash separates
!!    filename components and that the right-most period in the last leaf
!!    of the pathname is considered the beginning of an extension. If
!!    an extension is found it is left present in NAME but removed from
!!    BASENAME.
!!
!!    This routine does not check the system for the existence or type of
!!    the filename components; it merely parses a string.
!!
!!    Assumes leaf separator is a slash or backslash as determined by
!!    separator(3f) and that filename does not contain trailing spaces.
!!
!!##OPTIONS
!!    path      Path to be broken into components. It is assumed
!!
!!              o Forward slashes (/) separate pathname components.
!!              o the name '.' means "current directory"
!!              o the name '..' means "up one directory"
!!              o a pathname ending in a slash is a directory name
!!              o a slash starting the pathname represents the root
!!                directory.
!!              o trailing spaces are insignificant.
!!
!!    Using these rules helps to reduce incorrect parsing, but the
!!    routine is only intended for simple parsing of names of the form
!!    "[dir/]name[.extension].
!!
!!##RESULTS
!!    dir       Path of directories, including the trailing slash.
!!    name      Name of file leaf or, if no file is specified in path,
!!              name of the lowest directory.
!!    basename  NAME with any extension removed
!!    ext       File name extension, if any, including the leading period (.).
!!
!!    The path parameter can be a complete or partial file specification. The
!!    special name "." is assumed to mean the current directory, and the
!!    special name ".." is assumed to mean one directory above the current
!!    directory.
!!
!!##EXAMPLE
!!
!!   program demo_splitpath
!!
!!    use m_io, only : splitpath
!!    implicit none
!!    integer,parameter :: maxlen=4096
!!    character(len=maxlen),parameter   :: file(*)=[&
!!       & 'dirs/name.ext  ', &
!!       & 'xx/IO/zz/NN.FF ', &
!!       & 'xx/IO/zz/NN    ', &
!!       & '/xx/IO/zz/NN   ', &
!!       & '/xx/IO/zz/     ', &
!!       & '/xx/IO/zz.A/   ', &
!!       & '/xx/IO/zz/.    ', &
!!       & '               ', &
!!       & './             ', &
!!       & '/              ', &
!!       & '/..            ', &
!!       & './..           ', &
!!       & 'name.          ', &
!!       & '.name          ', &
!!       & '.name.         ', &
!!       & '.              ', &
!!       & '..             ', &
!!       & '...            ']
!!
!!    character(len=maxlen)  :: dir
!!    character(len=maxlen)  :: name
!!    character(len=maxlen)  :: basename
!!    character(len=maxlen)  :: ext
!!    integer                :: i
!!    integer                :: longest
!!    longest=maxval(len_trim(file)) ! find longest filename
!!
!!    do i=1,size(file)
!!       call splitpath(file(i), dir, name, basename, ext)
!!       write(*,'(*("| ",a:))')  &
!!       & file(i)(:longest),     &
!!       & dir(:longest),         &
!!       & name(:longest),        &
!!       & basename(:longest),    &
!!       & ext(:longest)
!!    enddo
!!   end program demo_splitpath
!!
!!   Output
!!
!!    | dirs/name.ext | dirs          | name.ext      | name          | .ext
!!    | xx/IO/zz/NN.FF| xx/IO/zz      | NN.FF         | NN            | .FF
!!    | xx/IO/zz/NN   | xx/IO/zz      | NN            | NN            |
!!    | /xx/IO/zz/NN  | /xx/IO/zz     | NN            | NN            |
!!    | /xx/IO/zz/    | /xx/IO/zz     |               |               |
!!    | /xx/IO/zz.A/  | /xx/IO/zz.A   |               |               |
!!    | /xx/IO/zz/.   | /xx/IO/zz/.   |               |               |
!!    |               | .             |               |               |
!!    | ./            | .             |               |               |
!!    | /             | /             |               |               |
!!    | /..           | /             |               |               |
!!    | ./..          | ./..          |               |               |
!!    | name.         |               | name.         | name          | .
!!    | .name         |               | .name         | .name         |
!!    | .name.        |               | .name.        | .name         | .
!!    | .             | .             |               |               |
!!    | ..            |               |               |               |
!!    | ...           |               | ...           | ..            | .
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine splitpath(path,dir,name,basename,ext)
implicit none

! ident_10="@(#)M_io::splitpath(3f): split Unix pathname into components (dir,name,basename,extension)"

!===================================================================================================================================
character(len=*),intent(in)           :: path
character(len=*),intent(out),optional :: dir
character(len=*),intent(out),optional :: name
character(len=*),intent(out),optional :: basename
character(len=*),intent(out),optional :: ext
integer,parameter                     :: maxlen=4096
character(len=maxlen)                 :: dir_local
character(len=maxlen)                 :: name_local
character(len=maxlen)                 :: basename_local
character(len=maxlen)                 :: ext_local
character(len=len(path)+1)            :: path_local
integer                               :: where
integer                               :: i
integer                               :: iend
character(len=1)                 :: sep
   sep=separator()
!===================================================================================================================================
   path_local=path                           ! initialize variables
   dir_local=''
   name_local=''
   basename_local=''
   ext_local=''
   iend=len_trim(path_local)
   LOCAL : block
!===================================================================================================================================
   if(iend.eq.0)then                         ! blank input path
      dir_local='.'
      exit LOCAL
   endif
!===================================================================================================================================
   if(path_local(iend:iend).eq.sep)then      ! assume entire name is a directory if it ends in a slash
      if(iend.gt.1)then
         dir_local=path_local(:iend-1)
      else                                   ! if just a slash it means root directory so leave it as slash
         dir_local=path_local
      endif
      exit LOCAL
   endif
!===================================================================================================================================
   TRIMSLASHES: do i=iend,1,-1               ! trim off trailing slashes even if duplicates
      if(path_local(i:i).eq.sep)then
         path_local(i:i)=' '
         iend=i-1
      else
         iend=i
         exit TRIMSLASHES
      endif
   enddo TRIMSLASHES

   if(iend.eq.0)then                         ! path composed entirely of slashes.
      dir_local=sep
      exit LOCAL
   endif
!===================================================================================================================================
   where=INDEX(path_local,sep,BACK=.true.)   ! find any right-most slash in remaining non-null name_local after trimming trailing slashes
   if(where.le.0)then                        ! no slash in path so everything left is name_local
      name_local=path_local(:iend)                 ! this is name_local unless '.' or '..'
   else                                      ! last slash found
      dir_local=path_local(:where-1)               ! split into directory
      name_local=path_local(where+1:iend)          ! this is name_local unless '.' or '..'
   endif
!===================================================================================================================================
   select case (name_local(1:3))                   ! special cases where name_local is a relative directory name_local '.' or '..'
   case('.  ')
      dir_local=path_local
      name_local=''
   case('.. ')
      if(dir_local.eq.'')then
         if(path_local(1:1).eq.sep)then
            dir_local=sep
         endif
      else
         dir_local=path_local
      endif
      name_local=''
   case default
   end select
!===================================================================================================================================
   if(name_local.eq.'.')then
      name_local=''
   endif
!===================================================================================================================================
   iend=len_trim(name_local)
   where=INDEX(name_local,'.',BACK=.true.)         ! find any extension
   if(where.gt.0.and.where.ne.1)then         ! only consider a non-blank extension name_local
      ext_local=name_local(where:)
      basename_local=name_local(:where-1)
   else
      basename_local=name_local
   endif
!===================================================================================================================================
   endblock LOCAL
   if(present(dir))dir=dir_local
   if(present(name))name=name_local
   if(present(basename))basename=basename_local
   if(present(ext))ext=ext_local
end subroutine splitpath
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    getline(3f) - [M_io] read a line from specified LUN into allocatable string up to line length limit
!!    (LICENSE:PD)
!!
!!##SYNTAX
!!   function getline(line,lun) result(ier)
!!
!!    character(len=:),allocatable,intent(out) :: line
!!    integer,intent(in),optional              :: lun
!!    integer,intent(out)                      :: ier
!!
!!##DESCRIPTION
!!    Read a line of any length up to programming environment maximum
!!    line length. Requires Fortran 2003+.
!!
!!    It is primarily expected to be used when reading input which will
!!    then be parsed.
!!
!!    The input file must have a PAD attribute of YES for the function
!!    to work properly, which is typically true.
!!
!!    The simple use of a loop that repeatedly re-allocates a character
!!    variable in addition to reading the input file one buffer at a
!!    time could (depending on the programming environment used) be
!!    inefficient, as it could reallocate and allocate memory used for
!!    the output string with each buffer read.
!!
!!##OPTIONS
!!    LINE   line read
!!    LUN    optional LUN (Fortran logical I/O unit) number. Defaults
!!           to stdin.
!!##RETURNS
!!    IER    zero unless an error occurred. If not zero, LINE returns the
!!           I/O error message.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_getline
!!    use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
!!    use M_io, only : getline
!!    implicit none
!!    character(len=:),allocatable :: line
!!       open(unit=stdin,pad='yes')
!!       INFINITE: do while (getline(line)==0)
!!          write(*,'(a)')'['//line//']'
!!       enddo INFINITE
!!    end program demo_getline
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
function getline(line,lun) result(ier)
implicit none

! ident_11="@(#)M_io::getline(3f): read a line from specified LUN into allocatable string up to line length limit"

character(len=:),allocatable,intent(out) :: line
integer,intent(in),optional              :: lun
integer                                  :: ier
character(len=4096)                      :: message

integer,parameter                        :: buflen=1024
character(len=:),allocatable             :: line_local
character(len=buflen)                    :: buffer
integer                                  :: isize
integer                                  :: lun_local

   line_local=''
   ier=0
   if(present(lun))then
      lun_local=lun
   else
      lun_local=INPUT_UNIT
   endif
   open(lun_local,pad='yes')

   INFINITE: do                                                      ! read characters from line and append to result
      read(lun_local,iostat=ier,fmt='(a)',advance='no',size=isize,iomsg=message) buffer ! read next buffer (might use stream I/O for files
                                                                     ! other than stdin so system line limit is not limiting
      if(isize.gt.0)line_local=line_local//buffer(:isize)            ! append what was read to result
      if(is_iostat_eor(ier))then                                     ! if hit EOR reading is complete unless backslash ends the line
         ier=0                                                       ! hitting end of record is not an error for this routine
         exit INFINITE                                               ! end of reading line
     elseif(ier.ne.0)then                                            ! end of file or error
        line=trim(message)
        exit INFINITE
     endif
   enddo INFINITE
   line=line_local                                                   ! trim line
end function getline
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     read_line(3f) - [M_io] read a line from specified LUN into allocatable string up to line length limit cleaning up input line
!!     (LICENSE:PD)
!!
!!##SYNTAX
!!   function read_line(line,lun,ios) result(ier)
!!
!!    character(len=:),allocatable,intent(out) :: line
!!    integer,intent(in),optional              :: lun
!!    integer,optional                         :: ios
!!    integer                                  :: ier
!!
!!##DESCRIPTION
!!
!!    Read a line of any length up to the programming environment maximum
!!    line length. Requires Fortran 2003+.
!!
!!    It is primarily expected to be used when reading input which will
!!    then be parsed.
!!
!!    The input file must have a PAD attribute of YES for the function to
!!    work properly, which is typically true but can be set on an open file.
!!
!!    o Append lines that end in a backslash with next line
!!    o Expand tabs
!!    o Replace unprintable characters with spaces
!!    o Remove trailing carriage return characters and white space
!!
!!    The simple use of a loop that repeatedly re-allocates a character
!!    variable in addition to reading the input file one buffer at a time
!!    could (depending on the programming environment used) be inefficient,
!!    as it could reallocate and allocate memory used for the output string
!!    with each buffer read.
!!
!!##OPTIONS
!!    LINE   the line read from the file.
!!    LUN    The LUN (logical unit) to read from. Defaults to stdin.
!!    IOS    status returned by READ(IOSTAT=IOS). If not zero, an error
!!           occurred or an end-of-file or end-of-record was encountered.
!!           This is the same value as returned by the function. See the
!!           example program for a usage case.
!!##RETURNS
!!    IER    status returned by READ(IOSTAT=IER). If not zero, an error
!!           occurred or an end-of-file or end-of-record was encountered.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!   Checking the error message and counting lines:
!!
!!     program demo_read_line
!!     use,intrinsic :: iso_fortran_env, only : stdin  => input_unit
!!     use,intrinsic :: iso_fortran_env, only : stderr => error_unit
!!     use,intrinsic :: iso_fortran_env, only : iostat_end, iostat_eor
!!     use M_io, only : read_line
!!     implicit none
!!     character (len =: ), allocatable :: line
!!     integer                          :: stat
!!     integer                          :: icount=0
!!        open(unit=stdin,pad='yes')
!!        INFINITE: do while (read_line(line,ios=stat) == 0)
!!           icount=icount
!!           write (*, '(*(g0))') icount,' [',line,']'
!!        enddo INFINITE
!!        if ( .not.is_iostat_end(stat) ) then
!!           write (stderr, '(*(g0))') &
!!           & 'error: line ',icount,'==>',trim (line)
!!        endif
!!     end program demo_read_line
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function read_line(line,lun,ios) result(ier)
implicit none

! ident_12="@(#)M_io::read_line(3f): read a line from specified LUN into allocatable string up to line length limit"

character(len=:),allocatable,intent(out) :: line
integer,intent(in),optional              :: lun
integer,optional                         :: ios
integer                                  :: ier

integer,parameter                        :: buflen=1024
character(len=:),allocatable             :: line_local
character(len=256)                       :: message
integer                                  :: biggest
character(len=buflen)                    :: buffer
integer                                  :: last
integer                                  :: isize
integer                                  :: lun_local

   line_local=''
   ier=0
   lun_local=merge(lun,INPUT_UNIT,present(lun))
   open(lun_local,pad='yes')
   INFINITE: do                                                           ! read characters from line and append to result
      read(lun_local,iostat=ier,fmt='(a)',advance='no',size=isize,iomsg=message) buffer ! read next buffer (might use stream I/O for
                                                                          ! files other than stdin so system line limit
                                                                          ! is not limiting
      if(isize.gt.0)line_local=line_local//buffer(:isize)   ! append what was read to result
      if(is_iostat_eor(ier))then                            ! if hit EOR reading is complete unless backslash ends the line
         last=len(line_local)
         if(last.ne.0)then
            if(line_local(last:last).eq.'\')then            ! if line ends in backslash it is assumed a continued line
               line_local=line_local(:last-1)               ! remove backslash
               cycle INFINITE                               ! continue on and read next line and append to result
            endif
         endif
         ier=0                                              ! hitting end of record is not an error for this routine
         exit INFINITE                                      ! end of reading line
     elseif(ier.ne.0)then                                   ! end of file or error
        line_local=trim(message)
        exit INFINITE
     endif
   enddo INFINITE
   biggest=8*len(line_local)                                ! worst case is raw line is all tab characters
   if(allocated(line))deallocate(line)
   allocate(character(len=biggest) :: line)
   call notabs(line_local,line,last)                        ! expand tabs, trim carriage returns, remove unprintable characters
   line=trim(line(:last))                                   ! trim line
   if(present(ios))then
      ios=ier
   endif
end function read_line
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      get_tmp(3f) - [M_io] Return the name of the scratch directory
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!     function get_tmp() result(tname)
!!
!!      character(len=:),allocatable :: tname
!!##DESCRIPTION
!!
!!    Return the name of the scratch directory set by the most common
!!    environment variables used to designate a scratch directory.
!!    $TMPDIR is the canonical environment variable in Unix and POSIX[1]
!!    to use to specify a temporary directory for scratch space. If $TMPDIR
!!    is not set, $TEMP, $TEMPDIR, and $TMP are examined in that order. If
!!    nothing is set "/tmp/" is returned. The returned value always ends in
!!    "/". No test is made that the directory exists or is writable.
!!
!!##EXAMPLE
!!
!!
!!   Sample:
!!
!!     program demo_get_tmp
!!     use M_io, only : get_tmp, uniq
!!     implicit none
!!     character(len=:),allocatable :: answer
!!        answer=get_tmp()
!!        write(*,*)'result is ',answer
!!        answer=get_tmp()//uniq('_scratch',create=.false.)
!!        write(*,*)'the file ',answer,' was a good scratch file name, at least a moment ago'
!!     end program demo_get_tmp
!!
!!   Sample Results:
!!
!!     result is /cygdrive/c/Users/JSU/AppData/Local/Temp/
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function get_tmp() result(tname)

! ident_13="@(#)M_io::get_tmp(3f): Return the name of the scratch directory"

character(len=:),allocatable :: tname
integer                      :: lngth
character(len=10),parameter  :: names(4)=["TMPDIR    ","TEMP      ","TEMPDIR   ","TMP       "]
integer                      :: i
character(len=1)             :: sep
   sep=separator()
   tname=''
   do i=1,size(names)
      call get_environment_variable(name=names(i), length=lngth)
      if(lngth.ne.0)then
         if(allocated(tname))deallocate(tname)
         allocate(character(len=lngth) :: tname)
         call get_environment_variable(name=names(i), value=tname)
         exit
      endif
   enddo
   if(lngth.eq.0)then
      tname='/tmp'
      lngth=len_trim(tname)
   endif
   if(scan(tname(lngth:lngth),'/\').eq.0)then
      tname=tname//sep
   endif
end function get_tmp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      scratch(3f) - [M_io] Return the name of a scratch file
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!     function scratch(prefix) result(tname)
!!
!!      character(len=:),allocatable         :: tname
!!      character(len=*),intent(in),optional :: prefix
!!##DESCRIPTION
!!
!!    Fortran supports non-retainable scratch files via
!!    OPEN(STATUS='SCRATCH',...) . There are circumstances where a file with
!!    a unique name is required instead. Specifying the pathname of a file
!!    can be required for performance reasons, file space limitations, or
!!    to support the ability for other processes or subprocesses to access
!!    the file.
!!
!!    SCRATCH(3f) Return the name of a scratch file in the scratch directory
!!    set by the most common environment variables used to designate a
!!    scratch directory unless the prefix contains the character "/" or "\".
!!
!!    $TMPDIR is the canonical environment variable in Unix and POSIX[1]
!!    used to specify a temporary directory for scratch space. If $TMPDIR
!!    is not set, $TEMP, $TEMPDIR, and $TMP are examined in that order. If
!!    nothing is set "/tmp/" is used.
!!
!!##OPTIONS
!!    prefix  an optional prefix for the leaf of the filename. A suffix created
!!            by genuuid(3) is used to make the name unique. The prefix is used
!!            as-is if it contains the character "/" or "\". Otherwise, the
!!            prefix is prefixed by the first value that is not blank from the
!!            set {$TMPDIR, $TEMP, $TEMPDIR, $TMP, /tmp}.
!!
!!            The default prefix is the basename of the program that
!!            called the procedure (the name trimmed of directories and
!!            anything from the right-most period in the name to the end
!!            of the name).
!!##EXAMPLE
!!
!!   Sample:
!!
!!     program demo_scratch
!!     use M_io, only : scratch
!!     implicit none
!!     write(*,*)'find good scratch file name candidates; one should test if writable'
!!     call printit('JUNK:')
!!     call printit('./')
!!     call printit('/var/tmp/')
!!     call printit('')
!!     call printit()
!!     contains
!!     subroutine printit(NAME)
!!     character(len=*),intent(in),optional :: NAME
!!     if(present(NAME))then
!!        write(*,'(a,t20,a)')NAME,scratch(NAME)
!!     else
!!        write(*,'(a,t20,a)')'*NOT PRESENT*',scratch()
!!     endif
!!     end subroutine printit
!!     end program demo_scratch
!!
!!   Results:
!!
!!     find good scratch file name candidates; one should test if writable
!!    JUNK:              /tmp/JUNK:405d766e-1320-4405-50e1-5d88fffbee9a.scr
!!    ./                 ./xx-901606b1-6ad2-4e96-6b17-e8bffedf2452.scr
!!    /var/tmp/          /var/tmp/xx-3f5c55fa-17ca-4020-4a05-a9d9cfad8dbe.scr
!!                       /tmp/f10e0491-a2ff-4455-5ff6-55d7dfe7fa8c.scr
!!    *NOT PRESENT*      /tmp/xx-f4fed5f7-3694-4609-5af4-8902ffa75839.scr
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function scratch(prefix) result(tname)

! ident_14="@(#)M_io::scratch(3f): Return the name of a scratch file"

character(len=*),intent(in),optional :: prefix
character(len=:),allocatable         :: tname

integer,parameter                    :: maxlen=4096
character(len=maxlen)                :: path
character(len=maxlen)                :: bname
integer                              :: ilen
character(len=1)                     :: sep
   sep=separator()

   if(present(prefix))then
      ilen=len_trim(prefix)
      if(index(prefix,'/')+index(prefix,'\').ne.0)then  ! if contains with / or \ do not use current temp directory but use as-is
         if(prefix(ilen:ilen).eq.'/'.or.prefix(ilen:ilen).eq.'\')then ! assumed that the prefix is a directory name
            call get_command_argument(number=0,value=path)
            call splitpath(path,basename=bname)
            tname=trim(prefix)//trim(bname)//'-'//generate_uuid()//'.scr'
         else
            tname=trim(prefix)//generate_uuid()//'.scr'
         endif
      else
         tname=get_tmp()//trim(prefix)//generate_uuid()//'.scr'
      endif
   else
      call get_command_argument(number=0,value=path)
      call splitpath(path,basename=bname)
      tname=get_tmp()//trim(bname)//'-'//generate_uuid()//'.scr'
   endif
end function scratch
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!! rd(3f) - [M_io] ask for string from standard input with user-definable prompt
!! (LICENSE:PD)
!!
!!   function rd(prompt,default) result(strout)
!!
!!    character(len=*),intent(in)              :: prompt
!!
!!    character(len=*),intent(in)              :: default
!!          or
!!    integer,intent(in)                       :: default
!!          or
!!    real,intent(in)                          :: default
!!          or
!!    doubleprecision,intent(in)               :: default
!!
!!    character(len=:),allocatable,intent(out) :: strout
!!
!!##DESCRIPTION
!!    Ask for string or value from standard input with user-definable prompt
!!    up to 20 times.
!!
!!    Do not use the function in an I/O statement as not all versions of
!!    Fortran support this form of recursion. Numeric values may be input
!!    in standard INTEGER, REAL, and DOUBLEPRECISION formats or as whole
!!    numbers in base 2 to 36 in the format BASE#VALUE.
!!
!!##OPTIONS
!!    prompt    Prompt string; displayed on same line as input is read from
!!    default   default answer on carriage-return. The type of the default
!!              determines the type of the output.
!!##RETURNS
!!    strout    returned string or value. If an end-of-file or system error
!!              is encountered the string "EOF" is returned, or a "Nan"
!!              numeric value.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_rd
!!    use M_io, only : rd
!!    implicit none
!!    character(len=:),allocatable :: mystring
!!    doubleprecision              :: d
!!    real                         :: r
!!    integer                      :: i
!!
!!    INFINITE: do
!!       mystring=rd('Enter string or "STOP":',default='Today')
!!       if(mystring.eq.'STOP')stop
!!       i=rd('Enter integer:',default=huge(0))
!!       r=rd('Enter real:',default=huge(0.0))
!!       d=rd('Enter double:',default=huge(0.0d0))
!!
!!       write(*,*)'I=', i, 'R=', r, 'D=',d,  'MYSTRING=', mystring
!!    enddo INFINITE
!!
!!    end program demo_rd
!!
!!##AUTHOR
!!    John S. Urban, 1993
!!##LICENSE
!!    Public Domain
function rd_character(prompt,default) result(strout)
! 1995 John S. Urban
!
implicit none

! ident_15="@(#)M_io::rd_character(3fp): ask for string from standard input with user-definable prompt"

character(len=*),intent(in)  :: prompt
character(len=*),intent(in)  :: default
character(len=:),allocatable :: strout

integer                      :: len_default
integer                      :: igot
integer                      :: ierr
integer                      :: icount
!===================================================================================================================================
   len_default=len(prompt)
!===================================================================================================================================
   do icount=1,20                                                  ! prevent infinite loop on error or end-of-file
      if(len_default.gt.0)write(*,'(a,'' '')',advance='no')prompt  ! write prompt
      ierr=getline(strout,stdin)                                  ! get back string
      igot=len(strout)
      if(ierr.ne.0)then
         strout='EOF'
         cycle
      elseif(igot.eq.0.and.len_default.gt.0)then
         strout=default
         exit
      elseif(igot.le.0)then
         call journal('*rd* blank string not allowed')
         cycle
      else
         exit
      endif
   enddo
end function rd_character
!===================================================================================================================================
function rd_doubleprecision(prompt,default,iostat) result(dvalue)
implicit none

! ident_16="@(#)M_io::rd_doubleprecision(3fp): ask for number from standard input with user-definable prompt"

doubleprecision              :: dvalue
integer                      :: ivalue
character(len=*),intent(in)  :: prompt
doubleprecision,intent(in)   :: default
integer,intent(out),optional :: iostat
character(len=:),allocatable :: strout
character(len=:),allocatable :: message
integer                      :: itest

   iostat=0
   dvalue=default
   strout=adjustl(rd_character(prompt,'NaN'))

   ! 1 for an integer [-+]NNNNN
   ! 2 for a whole number [-+]NNNNN.
   ! 3 for a real value [-+]NNNNN.MMMM
   ! 4 for a exponential value [-+]NNNNN.MMMM[-+]LLLL [-+]NNNNN.MMMM[ed][-+]LLLL
   ! values less than 1 represent an error
   if(strout.eq.'NaN')then
      dvalue=default
   elseif(index(strout,'#').ne.0)then
      if( decodebase(strout,0,ivalue))then
         dvalue=ivalue
      else
         iostat=-1
         write(*,*)'ERROR> could not convert ',strout
      endif
   else
      itest=isnumber(strout,message)
      if(itest.gt.0)then
         dvalue=s2v(strout,ierr=iostat)
      else
         iostat=-2
         write(*,*)' ERROR> for ',strout,' ',itest,':',trim(message)
      endif
   endif
end function rd_doubleprecision
!===================================================================================================================================
function rd_real(prompt,default,iostat) result(rvalue)
implicit none

! ident_17="@(#)M_io::rd_real(3fp): ask for number from standard input with user-definable prompt"

real                         :: rvalue
real(kind=dp)                :: dvalue
character(len=*),intent(in)  :: prompt
real,intent(in)              :: default
integer,intent(out),optional :: iostat
   !*! what about Nan, Inf, -Inf? Likely place for compiler bugs
   dvalue=rd_doubleprecision(prompt,dble(default),iostat)
   if(dvalue.ne.dvalue)then
      write(stderr,'(*(g0))') &
      & '<ERROR>*input* value [',dvalue,'] is indefinite'
      rvalue=huge(0.0)
   else
      rvalue=real(dvalue)
   endif
end function rd_real
!===================================================================================================================================
function rd_integer(prompt,default,iostat) result(ivalue)
implicit none

! ident_18="@(#)M_io::rd_integer(3fp): ask for number from standard input with user-definable prompt"

integer                      :: ivalue
real(kind=dp)                :: dvalue
character(len=*),intent(in)  :: prompt
integer,intent(in)           :: default
integer,intent(out),optional :: iostat
   dvalue=rd_doubleprecision(prompt,dble(default),iostat)
   !*! what about Nan, Inf, -Inf?
   if(dvalue.ne.dvalue)then
      write(stderr,'(*(g0))') &
      & '<ERROR>*input* value [',dvalue,'] is indefinite'
      ivalue=huge(0)
   elseif(dvalue.gt.huge(0))then
      write(stderr,'(*(g0))') &
      & '<ERROR>*input* value [',dvalue,'] greater than ', huge(0)
      ivalue=huge(0)
   elseif(dvalue.lt.1-huge(0))then
      write(stderr,'(*(g0))') &
      & '<ERROR>*input* value [',dvalue,'] less than ', 1-huge(0)
      ivalue=1-huge(0)
   else
      ivalue=nint(dvalue)
   endif
end function rd_integer
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    getname(3f) - [M_io:ENVIRONMENT] get name of the current executable
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function getname() result(name)
!!
!!     character(len=:),allocatable         :: getname
!!
!!##DESCRIPTION
!!    The getname() returns the name of the current executable using
!!    get_command_argument(3f) and inquire(3f).
!!
!!##EXAMPLE
!!
!!    Sample getting a pathname of current executable:
!!
!!      program demo_getname
!!      use M_io, only : getname
!!      implicit none
!!         write(*,*)'Running ',getname()
!!      end program demo_getname
!!
!!##AUTHOR
!!        John S. Urban
!!
!!##LICENSE
!!        Public Domain
function getname() result(name)
! get the pathname of arg0
implicit none
character(len=:),allocatable :: arg0
integer                      :: arg0_length
integer                      :: ios
character(len=4096)          :: long_name
character(len=:),allocatable :: name
   arg0_length=0
   name=''
   long_name=''
   call get_command_argument(0,length=arg0_length,status=ios)
   if(ios.eq.0)then
      if(allocated(arg0))deallocate(arg0)
      allocate(character(len=arg0_length) :: arg0)
      call get_command_argument(0,arg0,status=ios)
      if(ios.eq.0)then
         inquire(file=arg0,iostat=ios,name=long_name)
         name=trim(long_name)
      else
         name=arg0
      endif
   endif
end function getname
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     which(3f) - [M_io:ENVIRONMENT] given a command name find the pathname by searching
!!                 the directories in the environment variable $PATH
!!     (LICENSE:PD)
!!
!!##SYNTAX
!!   function which(command) result(pathname)
!!
!!    character(len=*),intent(in)  :: command
!!    character(len=:),allocatable :: pathname
!!
!!##DESCRIPTION
!!    Given a command name find the first file with that name in the directories
!!    specified by the environment variable $PATH.
!!
!!##OPTIONS
!!    COMMAND   the command to search for
!!
!!##RETURNS
!!    PATHNAME  the first pathname found in the current user path. Returns blank
!!              if the command is not found.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_which
!!     use M_io, only : which
!!     implicit none
!!        write(*,*)'ls is ',which('ls')
!!        write(*,*)'dir is ',which('dir')
!!        write(*,*)'install is ',which('install')
!!     end program demo_which
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function which(command) result(pathname)
character(len=*),intent(in)     :: command
character(len=:),allocatable    :: pathname, checkon, paths(:), exts(:)
integer                         :: i, j
   pathname=''
   call split(get_env('PATH'),paths,delimiters=merge(';',':',separator().eq.'\'))
   SEARCH: do i=1,size(paths)
      checkon=trim(joinpath(trim(paths(i)),command))
      select case(separator())
      case('/')
         if(exists(checkon))then
            pathname=checkon
            exit SEARCH
         endif
      case('\')
         if(exists(checkon))then
            pathname=checkon
            exit SEARCH
         endif
         if(exists(checkon//'.bat'))then
            pathname=checkon//'.bat'
            exit SEARCH
         endif
         if(exists(checkon//'.exe'))then
            pathname=checkon//'.exe'
            exit SEARCH
         endif
         call split(get_env('PATHEXT'),exts,delimiters=';')
         do j=1,size(exts)
            if(exists(checkon//'.'//trim(exts(j))))then
               pathname=checkon//'.'//trim(exts(j))
               exit SEARCH
            endif
         enddo
      end select
   enddo SEARCH
contains
logical function exists(filename) result(r)
character(len=*), intent(in) :: filename
    inquire(file=filename, exist=r)
end function
end function which
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     lookfor(3f) - [M_io:ENVIRONMENT] look for a filename in a number of directories
!!                 specified by an environment variable
!!     (LICENSE:PD)
!!
!!##SYNTAX
!!   function lookfor(basename,env) result(pathname)
!!
!!    character(len=:),intent(in)  :: basename
!!    character(len=:),intent(in)  :: env
!!    character(len=:),allocatable :: pathname
!!
!!##DESCRIPTION
!!    Given a base filename find the first file with that name in the directories
!!    specified by the environment variable ENV
!!
!!##OPTIONS
!!    BASENAME   the file to search for
!!    ENV        environment variable name. Seperator between directory names is
!!               assumed to be a colon on ULS (Unix-Like Systems) and semi-colon on
!!               MS-Windows machines.
!!
!!##RETURNS
!!    PATHNAME  the first pathname found in the current user path. Returns blank
!!              if the file is not found.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_lookfor
!!     use M_io, only : lookfor
!!     implicit none
!!     character(len=:),allocatable :: returned
!!        returned=lookfor('ls','PATH')
!!        write(*,*)'ls is ',returned
!!        returned=lookfor('dir.exe','PATH')
!!        write(*,*)'dir is ',returned
!!     end program demo_lookfor
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function lookfor(basename,env) result(pathname)
character(len=*),intent(in)     :: basename
character(len=*),intent(in)     :: env
character(len=:),allocatable    :: pathname, checkon, paths(:)
integer                         :: i
logical                         :: r
   pathname=''
   call split(get_env(env),paths,delimiters=merge(';',':',separator().eq.'\'))
   if(size(paths).eq.0)then
      paths=['']
   endif
   do i=1,size(paths)
      checkon=trim(joinpath(trim(paths(i)),basename))
      inquire(file=checkon, exist=r)
      if(r)then
         pathname=checkon
         exit
      endif
   enddo
end function lookfor
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function get_env(NAME,DEFAULT) result(VALUE)
implicit none
character(len=*),intent(in)          :: NAME
character(len=*),intent(in),optional :: DEFAULT
character(len=:),allocatable         :: VALUE
integer                              :: howbig
integer                              :: stat
integer                              :: length
   ! get length required to hold value
   length=0
   if(NAME.ne.'')then
      call get_environment_variable(NAME, length=howbig,status=stat,trim_name=.true.)
      select case (stat)
      case (1)
         !*!print *, NAME, " is not defined in the environment. Strange..."
         VALUE=''
      case (2)
         !*!print *, "This processor doesn't support environment variables. Boooh!"
         VALUE=''
      case default
         ! make string to hold value of sufficient size
         allocate(character(len=max(howbig,1)) :: VALUE)
         ! get value
         call get_environment_variable(NAME,VALUE,status=stat,trim_name=.true.)
         if(stat.ne.0)VALUE=''
      end select
   else
      VALUE=''
   endif
   if(VALUE.eq.''.and.present(DEFAULT))VALUE=DEFAULT
end function get_env
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module m_io
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
