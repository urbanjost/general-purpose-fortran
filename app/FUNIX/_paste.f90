!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program paste
use M_CLI,  only : filenames=>unnamed, commandline, check_commandline
use M_io,   only : fileopen
implicit none
integer                      :: i
integer                      :: number_of_files
integer,allocatable          :: lun(:)
logical,allocatable          :: ok(:)
character(len=1),parameter   :: tab=char(9)
! declare a namelist
logical            :: z, zero_terminated  ; namelist/args/ z, zero_terminated
logical            :: s, serial           ; namelist/args/ s, serial
character(len=256) :: d, delimiters       ; namelist/args/ d, delimiters
equivalence (z,zero_terminated)
equivalence (s,serial)
equivalence (d,delimiters)
character(len=:),allocatable :: help_text(:), version_text(:)
call crack_commandline( '-s F --serial F -d "'//tab//'" --delimiters "'//tab//'" -z F --zero_terminated F')
! problems: had to define a null delimiter and a space delimiter because have to have fixed length. Change M_CLI so a
!           null character is placed at end of strings or length can be queried? NAMELIST requires strings to be preallocated
!           I believe
!-----------------------------------------------------------------------------------------------------------------------------------
   number_of_files=size(filenames)
   allocate(lun(number_of_files))        ! keep track of LUN for each file opened
   allocate(ok(number_of_files))         ! keep track of whether an error has occurred on specified LUN
   do i=1,number_of_files                ! open all the files and track their LUNs
      if(filenames(i).eq.'"stdin"')then  ! assumes an actual file is not called "stdin" from M_io
         lun(i)=5
      else
         lun(i)=fileopen(filenames(i))
      endif
      ok(i)=merge(.false.,.true.,lun(i).eq.-1)  ! make LUN as bad if got an error on the OPEN(7f)
   enddo
   call dofile()
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine dofile()
use M_io,    only  : read_line, getline
use M_verify, only  : stderr
character(len=256)           :: msg
integer                      :: i, ios, istat, idelims, id
character(len=:),allocatable :: line
   idelims=max(1,len_trim(delimiters))
   INFINITE: do
      id=1                                                          ! point to delimiter letter from list of delimiters
      do i=1,number_of_files                                        ! cycle over all files
         if(ok(i)) then
            istat=getline(line,lun(i))                              ! read a line from next file
         else
            istat=-1
         endif
         if(i.ne.1)then
           write(*,'(a)',advance='no')delimiters(id:id)             ! put out delimiter if line is already started
           id=id+1                                                  ! step through delimiter list
           if(id.gt.idelims)id=1
         endif
         if(istat.eq.0)then                                         ! successfully read something
            write(*,'(a)',iostat=ios,iomsg=msg,advance='no')line    ! append new data to pasted line
         else
            ok(i)=.false.                                           ! flag this file is bad
         endif
      enddo
      if(zero_terminated)then
         write(*,'(a)',advance='no')char(0)
      else
         write(*,*)
      endif
      if( all(ok .eqv. .false.) )exit INFINITE
   enddo INFINITE
end subroutine dofile
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine crack_commandline(cmd)
   ! define command arguments,default values and crack command line
   ! the only thing that should change is the prototype for the command
   character(len=*),intent(in)  :: cmd
   character(len=255)           :: message ! for I/O error messages
   character(len=:),allocatable :: readme  ! stores updated namelist
   integer                      :: ios
      call set()
      readme=commandline(cmd)
      !!write(*,*)'README=',readme
      read(readme,nml=args,iostat=ios,iomsg=message)
      !!write(*,nml=args)
      call check_commandline(ios,message,help_text,version_text)
end subroutine crack_commandline
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine set()
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'       paste(1f) - [FILE FILTER] merge lines of files                           ',&
'       (LICENSE:PD)                                                             ',&
'SYNOPSIS                                                                        ',&
'       paste [OPTIONS] [filenames] |--version|--help                            ',&
'       (LICENSE:PD)                                                             ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   paste(1f) writes to standard output lines consisting of sequentially         ',&
'   corresponding lines of each given file, separated by a TAB character.        ',&
'   Standard input is used for a file name of ''-'' or if no input files are     ',&
'   given.                                                                       ',&
'                                                                                ',&
'   An exit status of zero indicates success, and a nonzero value                ',&
'   indicates failure.                                                           ',&
'                                                                                ',&
'   UNLIKE C VERSION the delimiter list cannot be a null set. A zero-length      ',&
'   list will be treated as a blank; and if a blank is a member of the set       ',&
'   it cannot be at the end of the delimiters list.                              ',&
'                                                                                ',&
'   Fortran does not allow the same file to be opened simultaneously more        ',&
'   than once so filenames cannot be repeated                                    ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   -d, --delimiters=LIST  Consecutively use the characters in LIST              ',&
'                          to separate merged lines. When LIST is                ',&
'                          exhausted, start again at its beginning.              ',&
'                                                                                ',&
'   -s, --serial           paste one file at a time instead of in parallel       ',&
'                                                                                ',&
'   -z, --zero-terminated  line delimiter is NUL, not newline                    ',&
'                          Delimit items with a zero byte rather than            ',&
'                          a newline (ASCII LF). I.E., treat input as            ',&
'                          items separated by ASCII NUL and terminate            ',&
'                          output items with ASCII NUL. This option can          ',&
'                          be useful in conjunction with ''perl -0'' or          ',&
'                          ''find -print0'' and ''xargs -0'' which do the        ',&
'                          same in order to reliably handle arbitrary            ',&
'                          file names (even those containing blanks              ',&
'                          or other special characters).                         ',&
'                                                                                ',&
'   --help                 Display a help message and exit.                      ',&
'   --version              Display version information and exit.                 ',&
'   --usage                Display table of commandline parameters               ',&
'EXAMPLES                                                                        ',&
'   For example, with:                                                           ',&
'                                                                                ',&
'     $ cat two_numbers                                                          ',&
'     1                                                                          ',&
'     2                                                                          ',&
'     $ cat three_letters                                                        ',&
'     a                                                                          ',&
'     b                                                                          ',&
'     c                                                                          ',&
'                                                                                ',&
'   Take lines sequentially from each file:                                      ',&
'                                                                                ',&
'     $ paste two_numbers three_letters                                          ',&
'     1       a                                                                  ',&
'     2       b                                                                  ',&
'             c                                                                  ',&
'                                                                                ',&
'   Duplicate lines from a file:                                                 ',&
'                                                                                ',&
'     # Fortran does not allow a file to be open twice                           ',&
'     # so the C program could do this:                                          ',&
'     $ paste two_numbers three_letters two_numbers                              ',&
'     1       a      1                                                           ',&
'     2       b      2                                                           ',&
'             c                                                                  ',&
'     # but you have to make a copy of two_numbers to                            ',&
'     # do that with this program                                                ',&
'                                                                                ',&
'   Intermix lines from stdin:                                                   ',&
'                                                                                ',&
'     $ paste - three_letters - < two_numbers                                    ',&
'     1       a      2                                                           ',&
'             b                                                                  ',&
'             c                                                                  ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
!>
!!##NAME
!!        paste(1f) - [FILE FILTER] merge lines of files
!!        (LICENSE:PD)
!!##SYNOPSIS
!!
!!        paste [OPTIONS] [filenames] |--version|--help
!!        (LICENSE:PD)
!!
!!##DESCRIPTION
!!    paste(1f) writes to standard output lines consisting of sequentially
!!    corresponding lines of each given file, separated by a TAB character.
!!    Standard input is used for a file name of '-' or if no input files are
!!    given.
!!
!!    An exit status of zero indicates success, and a nonzero value
!!    indicates failure.
!!
!!    UNLIKE C VERSION the delimiter list cannot be a null set. A zero-length
!!    list will be treated as a blank; and if a blank is a member of the set
!!    it cannot be at the end of the delimiters list.
!!
!!    Fortran does not allow the same file to be opened simultaneously more
!!    than once so filenames cannot be repeated
!!
!!##OPTIONS
!!    -d, --delimiters=LIST  Consecutively use the characters in LIST
!!                           to separate merged lines. When LIST is
!!                           exhausted, start again at its beginning.
!!
!!    -s, --serial           paste one file at a time instead of in parallel
!!
!!    -z, --zero-terminated  line delimiter is NUL, not newline
!!                           Delimit items with a zero byte rather than
!!                           a newline (ASCII LF). I.E., treat input as
!!                           items separated by ASCII NUL and terminate
!!                           output items with ASCII NUL. This option can
!!                           be useful in conjunction with 'perl -0' or
!!                           'find -print0' and 'xargs -0' which do the
!!                           same in order to reliably handle arbitrary
!!                           file names (even those containing blanks
!!                           or other special characters).
!!
!!    --help                 Display a help message and exit.
!!    --version              Display version information and exit.
!!    --usage                Display table of commandline parameters
!!##EXAMPLES
!!
!!    For example, with:
!!
!!      $ cat two_numbers
!!      1
!!      2
!!      $ cat three_letters
!!      a
!!      b
!!      c
!!
!!    Take lines sequentially from each file:
!!
!!      $ paste two_numbers three_letters
!!      1       a
!!      2       b
!!              c
!!
!!    Duplicate lines from a file:
!!
!!      # Fortran does not allow a file to be open twice
!!      # so the C program could do this:
!!      $ paste two_numbers three_letters two_numbers
!!      1       a      1
!!      2       b      2
!!              c
!!      # but you have to make a copy of two_numbers to
!!      # do that with this program
!!
!!    Intermix lines from stdin:
!!
!!      $ paste - three_letters - < two_numbers
!!      1       a      2
!!              b
!!              c
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples            ',&
'PROGRAM:        _paste(1)                                                       ',&
'DESCRIPTION:    merge lines of files                                            ',&
'VERSION:        1.0, 20200202                                                   ',&
'AUTHOR:         John S. Urban                                                   ',&
'LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.',&
'                There is NO WARRANTY, to the extent permitted by law.           ',&
'']
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine set
!-----------------------------------------------------------------------------------------------------------------------------------
end program paste
!-----------------------------------------------------------------------------------------------------------------------------------
