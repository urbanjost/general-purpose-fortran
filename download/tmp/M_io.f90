!===================================================================================================================================
MODULE M_IO
private
public uniq
public print_inquire
public notopen
public slurp
public dirname
public splitpath
public isdir
public readline
CONTAINS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      uniq(3f) - [M_io] append a number to the end of filename to make a unique name if name exists
!!##SYNOPSIS
!!
!!      Usage
!!
!!       character(len=255) function uniq(name,istart)
!!       character(len=*),intent(in) :: name
!!       integer,intent(in),optional :: istart
!!
!!##DESCRIPTION
!!    Given a filename test if it is in use or exists. If it is, or if it
!!    ends in a period add a four-digit number to the end of the name and
!!    test if the new name is in use. If necessary, increment the number
!!    and try again up to the value 9999.
!!
!!    o relatively non-generic;
!!    o does not try to detect io errors
!!
!!    could make this use $TMPDIR and $$ to suffix and prefix file with to come up with a unique filename
!!##OPTIONS
!!       name    base input name used to create output filename
!!       istart  number to start with as a suffix
!!##RETURNS
!!       uniq  A unique filename that is the same as the NAME input parameter
!!             except with a number appended at the end if needed.
!!##EXAMPLE
!!
!!    Sample program
!!
!!       program demo_uniq
!!       use M_io, only : uniq
!!       implicit none
!!       character(len=255) :: myname
!!       integer            :: i
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
!!     name has suffix added does_exist0000
!!     FILENAME:does_exist0001
!!     FILENAME:does_exist0002
!!     FILENAME:does_exist0003
!!     FILENAME:does_exist0004
!!     FILENAME:does_exist0005
!!     FILENAME:does_exist0006
!!     FILENAME:does_exist0007
!!     FILENAME:does_exist0008
!!     FILENAME:does_exist0009
!!     FILENAME:does_exist0010
!!
!!##AUTHOR
!!    John S. Urban, 1993
!===================================================================================================================================
function uniq(name,istart)
!-----------------------------------------------------------------------------------------------------------------------------------
use M_journal, only : journal
implicit none
character(len=*),parameter::ident="@(#)M_io::uniq(3f):append a number to the end of filename to make a unique name if name exists"
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),intent(in) :: name
character(len=255)          :: uniq
integer,intent(in),optional :: istart
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                     :: istart_local
   logical                     :: around
   integer,save                :: icount=0           ! counter to generate suffix from
   character(len=255),save     :: lastname=' '       ! name called with last time the routine was called
   integer                     :: ilen
   integer                     :: itimes
!-----------------------------------------------------------------------------------------------------------------------------------
   uniq=name                                         ! the input name will be returned if it passes all the tests
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lastname.ne.name)then                          ! if a different input name than last time called reset icount
      lastname=name                                  ! a new name to keep for subsequent calls
      icount=0                                       ! icount is used to make a suffix to add to make the file unique
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(istart))then
      istart_local=istart
   else
      istart_local=0
   endif
   if(istart_local.ne.0) icount=istart_local         ! icount is used to make a suffix to add to make the file unique
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len_trim(name)                               ! find last non-blank character in file name
   if(ilen.ne.0)then                                 ! a blank input name so name will just be a suffix
!-----------------------------------------------------------------------------------------------------------------------------------
      if(name(ilen:ilen).ne.'.')then                 ! always append a number to a file ending in .
         inquire(file=name(:ilen),exist=around)      ! check filename as-is
         if(.not.around)then                         ! file name does not exist, can use it as-is
            uniq=name
            return
         endif
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ilen.gt.(255-4))then                            ! input filename is too long to add suffix to
      call journal('sc','*uniq* ERROR: unable to append number to filename. Filename is too long')
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   itimes=0                                           ! count number of times tried to get a uniq name
   INFINITE: do                                       ! top of loop trying for a unique name
      if(itimes.ge.9999)then                          ! if too many tries to be reasonable give up
         call journal('sc','*uniq* unable to find a unique filename. Too many tries')
         return
      endif
      if(icount.gt.9999) icount=0                     ! reset ICOUNT when it hits arbitrary maximum value
      write(uniq(ilen+1:),'(i4.4)')icount             ! create name by adding a numeric string to end
      icount=icount+1                                 ! increment counter used to come up with suffix
      inquire(file=uniq(:ilen+4),exist=around)        ! see if this filename already exists
      if(.not.around)then                             ! found an unused name
         call journal('c',trim('*uniq* name='//uniq)) ! write out message reporting name used
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
!!##SYNOPSIS
!!
!!   Definition:
!!
!!    subroutine print_inquire(iunit,name)
!!    integer,intent(in)          :: iunit
!!    character(len=*),intent(in) :: name
!!##DESCRIPTION
!!    Given either a Fortran file-unit-number or filename, call the INQUIRE(3f)
!!    intrinsic and print typical status information.
!!##OPTIONS
!!    iunit  if >=0 then query by number and ignore filename
!!    name   if IUNIT < 0 then query by this filename
!!##EXAMPLE
!!
!!    Sample program:
!!
!!       program demo_print_inquire
!!       use M_io, only : print_inquire
!!
!!       call print_inquire(5,'')
!!
!!       call print_inquire(19,'')
!!
!!       open(unit=20)
!!       call print_inquire(20,'')
!!
!!       open(unit=21,status='scratch')
!!       call print_inquire(21,'')
!!
!!       open(unit=22,file='junko')
!!       write(22,*)'WRITE TO JUNKO'
!!       close(unit=22)
!!       call print_inquire(22,'')
!!       call print_inquire(-1,'junko')
!!
!!       end program demo_print_inquire
!!
!!   Expected output:
!!
!!    ====================================================
!!    *print_inquire* checking file: /dev/pty1
!!    *print_inquire* file exists
!!    *print_inquire* using unit number  5
!!    *print_inquire* access type SEQUENTIAL,FORMATTED
!!    ====================================================
!!    *print_inquire* unit number is not open ,unit= 19
!!    ====================================================
!!    *print_inquire* checking file: fort.20
!!    *print_inquire* file exists
!!    *print_inquire* using unit number  20
!!    *print_inquire* access type SEQUENTIAL,FORMATTED
!!    ====================================================
!!    *print_inquire* unit number is not named ,unit= 21
!!    ====================================================
!!    *print_inquire* unit number is not open ,unit= 22
!!    ====================================================
!!    *print_inquire* checking file: junko
!!    *print_inquire* file exists
!!    *print_inquire* file is not open
!===================================================================================================================================
subroutine print_inquire(iunit,name) ! Version: JSU-1997-12-31
use M_journal, only : journal
character(len=*),parameter::ident="@(#)M_io::print_inquire(3f): Do INQUIRE on file by name/number and print results"
integer,intent(in)          :: iunit         ! if iunit >= 0 then query by unit number, else by name
character(len=*),intent(in) :: name
      character(len=1024)   :: nm
      integer               :: iend,inum,ircl,inext,ios
      logical               :: ex,od,nmd
      character(len=20)     :: a1,a2,a3,a4,a5,a6

      ! ex, od, and nmd always become defined unless an error condition occurs.
      call journal('sc','====================================================')
!-----------------------------------------------------------------------------------------------------------------------------------
      if(iunit.ge.0)then ! query by number
         inquire(iunit,opened=od,iostat=ios,err=999,named=nmd,exist=ex)
         if(.not.ex)then
            call journal('sc','*print_inquire* unit number is not valid ,unit=',iunit)
            return
         elseif(.not.od)then
            call journal('sc','*print_inquire* unit number is not open ,unit=',iunit)
            return
         elseif(.not.nmd)then
            call journal('sc','*print_inquire* unit number is not named ,unit=',iunit)
            return
         endif
         ! unit is connected and name and exists so get name
         inquire(iunit,iostat=ios,err=999,name=nm) ! set nm to filename
         iend=len_trim(nm)
         ! go on, pretending you queried by name
      else ! query by name
         nm=name
         iend=len_trim(nm(:len(name)))
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(iend.eq.0)then
         call journal('sc','*print_inquire* blank filename')
         inquire(unit=iunit,     &
     &   exist=ex,               &
     &   opened=od,              &
     &   number=inum,            &
     &   direct=a1,              &
     &   sequential=a2,          &
     &   access=a3,              &
     &   formatted=a4,           &
     &   unformatted=a5,         &
     &   recl=ircl,              &
     &   nextrec=inext,          &
     &   form=a6,                &
     &   iostat=ios,err=999)
      else
         call journal('sc','*print_inquire* checking file:'//trim(nm(1:iend)) )
         inquire(file=nm(:iend), &
     &   exist=ex,               &
     &   opened=od,              &
     &   number=inum,            &
     &   direct=a1,              &
     &   sequential=a2,          &
     &   access=a3,              &
     &   formatted=a4,           &
     &   unformatted=a5,         &
     &   recl=ircl,              &
     &   nextrec=inext,          &
     &   form=a6,                &
     &   iostat=ios,err=999)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(.not.ex)then
         call journal('sc','*print_inquire* file does not exist')
         return
      else
         call journal('sc','*print_inquire* file exists')
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(.not.od)then
         call journal('sc','*print_inquire* file is not open')
         return
      else
         call journal('sc','*print_inquire* using unit number ',inum)
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
!      call journal('sc','can access be sequential? '//a2)
!      call journal('sc','can access be direct? '//a1)
!      call journal('sc','is formatted I/O allowed? '//a4)
!      call journal('sc','is unformatted I/O allowed? '//a5)
!     DIRECT/SEQUENTIAL, FORMATTED/UNFORMATTED
      call journal('sc','*print_inquire* access type '//trim(a3)//','//trim(a6))
      if(a3.eq.'DIRECT')then
         call journal('sc','*print_inquire* using record length of RECL=',ircl)
         call journal('sc','*print_inquire* next record to sequentially read=',inext)
      endif
      return
!-----------------------------------------------------------------------------------------------------------------------------------
999   continue
      call journal('sc','*print_inquire* bad inquire')
!     If an error condition occurs during execution of an INQUIRE  statement,
!     all of the inquiry identifiers except ios become undefined.
      call journal('sc','*print_inquire* inquire call failed,iostat=',ios)
end subroutine print_inquire
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    SLURP(3f) - [M_io] read a file into a character array
!!##SYNOPSIS
!!
!!    Usage
!!
!!       subroutine slurp(filename,text)
!!       character(len=*),intent(in) :: filename
!!       character(len=1),allocatable,intent(out) :: text(:)
!!##DESCRIPTION
!!    Read an entire file into memory as a stream, retaining line end
!!    terminals.  Never casually read an entire file into memory if you can
!!    process it per line or in smaller units; as large files can consume
!!    unreasonable amounts of memory.
!!##OPTIONS
!!       filename   filename to read into memory
!!       text       array of characters to hold file
!!##EXAMPLES
!!
!!    Sample program, assuming the input file "inputfile" exists:
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
!===================================================================================================================================
subroutine slurp(filename,text)
!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
character(len=*),parameter::ident="@(#)M_io::slurp(3f):allocate text array and read file filename into it"
character(len=*),intent(in)              :: filename    ! filename to shlep
character(len=1),allocatable,intent(out) :: text(:)     ! array to hold file
!-----------------------------------------------------------------------------------------------------------------------------------
   integer :: nchars=0             ! holds size of file
   integer :: igetunit             ! use newunit=igetunit in f08
   integer :: ios=0                ! used for I/O error status
!-----------------------------------------------------------------------------------------------------------------------------------
   igetunit=notopen(10,99)         ! find unused file unit number (hopefully)
   if(igetunit.lt.0)then
      call stderr('*slurp* could not find unused file unit number')
      return
   endif
   ! open named file in stream mode positioned to append
   open (unit=igetunit,                  &
   & file=filename(:len_trim(filename)), &
   & access='stream',                    &
   & status='old',                       &
   & position='append',iostat=ios)
   if(ios.eq.0)then  ! if file was successfully opened
      !
      ! get file size in bytes and position file to beginning of file
      !
      inquire(unit=igetunit,pos=nchars)   ! get number of bytes in file plus one
      rewind(igetunit)                    ! get back to beginning of file
      nchars=nchars-1 ! opened for append, so subtract one to get current length
      if(nchars.le.0)then
         call stderr('*slurp* empty file '//filename(:len_trim(filename)))
         return
      endif
      !
      ! read file into text array
      !
      if(allocated(text))deallocate(text) ! make sure text array not allocated
      allocate ( text(nchars) )           ! make enough storage to hold file
      read(igetunit,iostat=ios) text      ! load input file -> text array
      if(ios.ne.0)then
         call stderr('*slurp* bad read of '//filename(:len_trim(filename)))
      endif
   endif
   close(iostat=ios,unit=igetunit)     ! close if opened successfully or not
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine stderr(message)
use iso_fortran_env, only : error_unit
character(len=*) :: message
   write(error_unit,'(a)')trim(message)    ! write message to standard error
end subroutine stderr
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine slurp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    notopen(3f) - [M_io] Find a FUN/LUN (Fortran-unit-number) that is not in use
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
!!    Beginning with f2008, you can probably use OPEN(NEWUNIT=...) instead.
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
!!##AUTHORS
!!       John S. Urban
!===================================================================================================================================
integer function notopen(start,end,err)
!-----------------------------------------------------------------------------------------------------------------------------------
! AUTHOR: John S. Urban
!-----------------------------------------------------------------------------------------------------------------------------------
! access computing environment
use iso_fortran_env, only : error_unit,input_unit,output_unit
implicit none
character(len=*),parameter::ident="@(#)M_io::notopen(3f): find free FORTRAN unit number to OPEN() a file"
integer,optional,intent(in)    :: start                           ! unit number to start looking at
integer,optional,intent(in)    :: end                             ! last unit number to look at
integer,optional,intent(out)   :: err                             ! error flag returned
   integer                     :: istart
   integer                     :: iend
   integer                     :: ierr

   integer      :: i10                                            ! counter from start to end
   integer      :: ios                                            ! iostatus from INQUIRE
   logical      :: lopen                                          ! returned from INQUIRE
   logical      :: lexist                                         ! returned from INQUIRE
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
!!
!!##SYNOPSIS
!!
!!    function dirname(FILENAME) result (DIRECTORY)
!!
!!      character(len=*),intent(in)  :: FILENAME
!!      character(len=:),allocatable :: DIRECTORY
!!
!!##DESCRIPTION
!!    Output FILENAME with its last non-slash component and trailing slashes removed.
!!    if FILENAME contains no /'s, output '.' (meaning the current directory).
!!
!!    Assumes leaf seperator is a slash ('/') and that filename does not contain
!!    trailing spaces.
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
!===================================================================================================================================
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        dirname(3f)
!! DESCRIPTION:    strip last component from filename
!!##VERSION:        1.0.0
!!##DATE:           2015-06-26
!! AUTHOR:         John S. Urban
!! REPORTING BUGS: http://www.urbanjost.altervista.org/
!! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
!! LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.
!!                 There is NO WARRANTY, to the extent permitted by law.
!===================================================================================================================================
function dirname(filename) result (directory)
implicit none
character(len=*),parameter::ident="@(#)M_io::dirname(3f):strip last component from filename"
character(len=*),intent(in)      :: filename
character(len=:),allocatable     :: directory
integer                          :: iend
!-----------------------------------------------------------------------------------------------------------------------------------
   directory=trim(filename)
   call removetail()                         ! trim trailing slashes even if duplicates
   iend=index(directory,'/',back=.true.)     ! find last slash if any
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
      if(directory(right:right).eq.'/'.or.directory(right:right).eq.' ')then
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
!!     splitpath(3f) - [M_io] split a Unix pathname into components
!!
!!##SYNOPSIS
!!
!!   splitpath(path,dir,name,basename,ext)
!!
!!    integer,parameter :: maxlen=4096
!!    character(len=maxlen),intent(in)  :: path
!!    character(len=maxlen),intent(out) :: dir
!!    character(len=maxlen),intent(out) :: name
!!    character(len=maxlen),intent(out) :: basename
!!    character(len=maxlen),intent(out) :: ext
!!
!!##DESCRIPTION
!!    splitpath(3f) splits given pathname assuming a forward slash seperates
!!    filename components and that the right-most period in the last leaf
!!    of the pathname is considered the beginning of an extension. If
!!    an extension is found it is left present in NAME but removed from
!!    BASENAME.
!!
!!    This routine does not check the system for the existence or type of the
!!    filename componenents; it merely parses a string.
!!
!!
!!##OPTIONS
!!    path      Path to be broken into components.
!!
!!              o Forward slashes (/) are assumed to separate pathname components.
!!              o the name '.' is assumed to mean "current directory"
!!              o the name '..' is assumed to mean "up one directory
!!              o a pathname ending in a slash is assumed to be a directory name
!!              o a slash starting the pathname is assumed to represent the root
!!                directory.
!!              o trailing spaces are assumed insignificant.
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
!===================================================================================================================================
subroutine splitpath(path,dir,name,basename,ext)
use M_strings, only : split
implicit none
character(len=*),parameter::ident="@(#)M_io::splitpath(3f):split Unix pathname into components (dir,name,basename,extension)"
!===================================================================================================================================
character(len=*),intent(in)     :: path
character(len=*),intent(out)    :: dir
character(len=*),intent(out)    :: name
character(len=*),intent(out)    :: basename
character(len=*),intent(out)    :: ext
!===================================================================================================================================
   character(len=len(path)+1)   :: path_local
   integer                      :: where
   integer                      :: i
   integer                      :: iend
!===================================================================================================================================
   path_local=path                           ! initialize variables
   dir=' '
   name=' '
   basename=' '
   ext=' '
   iend=len_trim(path_local)
!===================================================================================================================================
   if(iend.eq.0)then                         ! blank input path
      dir='.'
      return
   endif
!===================================================================================================================================
   if(path_local(iend:iend).eq.'/')then      ! assume entire name is a directory if it ends in a slash
      if(iend.gt.1)then
         dir=path_local(:iend-1)
      else                                   ! if just a slash it means root directory so leave it as slash
         dir=path_local
      endif
      return
   endif
!===================================================================================================================================
   TRIMSLASHES: do i=iend,1,-1               ! trim off trailing slashes even if duplicates
      if(path_local(i:i).eq.'/')then
         path_local(i:i)=' '
         iend=i-1
      else
         iend=i
         exit TRIMSLASHES
      endif
   enddo TRIMSLASHES

   if(iend.eq.0)then                         ! path composed entirely of slashes.
      dir='/'
      return
   endif
!===================================================================================================================================
   where=INDEX(path_local,'/',BACK=.true.)   ! find any right-most slash in remaining non-null name after trimming trailing slashes
   if(where.le.0)then                        ! no slash in path so everything left is name
      name=path_local(:iend)                 ! this is name unless '.' or '..'
   else                                      ! last slash found
      dir=path_local(:where-1)               ! split into directory
      name=path_local(where+1:iend)          ! this is name unless '.' or '..'
   endif
!===================================================================================================================================
   select case (name(1:3))                   ! special cases where name is a relative directory name '.' or '..'
   case('.  ')
      dir=path_local
      name=''
   case('.. ')
      if(dir.eq.'')then
         if(path_local(1:1).eq.'/')then
            dir='/'
         endif
      else
         dir=path_local
      endif
      name=''
   case default
   end select
!===================================================================================================================================
   if(name.eq.'.')then
      name=''
   endif
!===================================================================================================================================
   iend=len_trim(name)
   where=INDEX(name,'.',BACK=.true.)         ! find any extension
   if(where.gt.0.and.where.ne.1)then         ! only consider a non-blank entension name
      ext=name(where:)
      basename=name(:where-1)
   else
      basename=name
   endif
!===================================================================================================================================
end subroutine splitpath
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    isdir(3f) - [M_io] checks if argument is a directory path
!!##SYNTAX
!!    logical function isdir(path)
!!##DESCRIPTION
!!    isdir(3f) checks if path is a path to a directory.
!!##OPTIONS
!!    path    a character string representing a directory pathname.
!!            Trailing spaces are ignored.
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_isdir
!!    Use M_io, only : isdir
!!    implicit none
!!    integer                     :: i
!!    character(len=80),parameter :: names(*)=[ &
!!    '/tmp            ', &
!!    '/tmp/NOTTHERE   ', &
!!    '/usr/local      ', &
!!    '.               ', &
!!    'PROBABLY_NOT    ']
!!    do i=1,size(names)
!!       write(*,*)' is ',trim(names(i)),' a directory? ', isdir(names(i))
!!    enddo
!!    end program demo_isdir
!!
!!   Results:
!!
!!    is /tmp a directory?  T
!!    is /tmp/NOTTHERE a directory?  F
!!    is /usr/local a directory?  T
!!    is . a directory?  T
!!    is PROBABLY_NOT a directory?  F
!===================================================================================================================================
function isdir(dirname)
implicit none
character(len=*),parameter::ident="@(#)M_io::isdir(3f): determine if DIRNAME is a directory name ON UNIX-COMPATIBLE file systems"
logical                     :: isdir
character(len=*),intent(in) :: dirname

! a trick to be sure DIRNAME is a dir on systems supporting Unix-compatible pathnames
if(dirname.ne.'')then
   inquire( file=trim(dirname)//"/.", exist=isdir )
else
   isdir=.false.
endif

end function isdir
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     readline(3f)  - [M_IO] read a line from specified LUN into allocatable string
!!
!!##SYNTAX
!!   readline(lun,line,ier)
!!
!!    integer,intent(in) :: lun
!!    character(len=:),allocatable,intent(out) :: line
!!    integer,intent(out) :: ier
!!
!!##DESCRIPTION
!!
!!    Read a line of any length up to programming environment's maximum
!!    line length. Requires Fortran 2003+.
!!
!!    It is primarily expected to be used when reading input which will
!!    then be parsed.
!!
!!    o Append lines that end in a backslash with next line
!!    o Expand tabs
!!    o Replace unprintable characters with spaces
!!    o Remove trailing carriage return characters and white space
!!
!!    The simple use of a loop that repeatedly re-allocates a character
!!    variable in addition to reading the input file one buffer at a
!!    time could (depending on the programming environment used) be
!!    inefficient, as it could reallocate and allocate memory used for
!!    the output string with each buffer read.
!!
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_readline
!!    use M_io, only : readline
!!    implicit none
!!    character(len=:),allocatable :: line
!!       INFINITE: do while (readline(line)==0)
!!          write(*,'(a)')'['//line//']'
!!       enddo INFINITE
!!    end program demo_readline
!===================================================================================================================================
function readline(line,lun) result(ier)
use iso_fortran_env, only : INPUT_UNIT
use M_strings,only : notabs
implicit none
character(len=:),allocatable,intent(out) :: line
integer,intent(in),optional              :: lun
integer                                  :: ier

   integer,parameter                     :: buflen=1024
   character(len=:),allocatable          :: line_local
   integer                               :: biggest
   character(len=buflen)                 :: buffer
   integer                               :: last
   integer                               :: isize
   integer                               :: lun_local

   line_local=''
   ier=0
   if(present(lun))then
      lun_local=lun
   else
      lun_local=INPUT_UNIT
   endif

   INFINITE: do                                                           ! read characters from line and append to result
      read(lun_local,iostat=ier,fmt='(a)',advance='no',size=isize) buffer ! read next buffer (might use stream I/O for files
                                                                          ! other than stdin so system line limit is not limiting
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
        exit INFINITE
     endif
   enddo INFINITE
   biggest=8*len(line_local)                                ! worst case is raw line is all tab characters
   allocate(character(len=biggest) :: line)
   call notabs(line_local,line,last)                        ! expand tabs, trim carriage returns, remove unprintable characters
   line=trim(line(:last))                                   ! trim line

end function readline
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module m_io
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
