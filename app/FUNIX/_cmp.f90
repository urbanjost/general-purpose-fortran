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
'NAME                                                                                                                            ',&
'     _cmp(1f) - [FUNIX] compare two files byte by byte                                                                          ',&
'     (LICENSE:PD)                                                                                                               ',&
'SYNOPSIS                                                                                                                        ',&
'     _cmp file1 file2 [ -quiet] [ -show]| [ [ -help] [ -version] ]                                                              ',&
'DESCRIPTION                                                                                                                     ',&
'     The _cmp(1) utility compares two files byte by byte and returns                                                            ',&
'     true if no differences are found, or false otherwise                                                                       ',&
'OPTIONS                                                                                                                         ',&
'     The following options are supported:                                                                                       ',&
'                                                                                                                                ',&
'     file1,file2  names of files to compare                                                                                     ',&
'     -quiet       suppress normal output                                                                                        ',&
'     -show        show all bytes that are different until an end of                                                             ',&
'                  file is encountered on both files                                                                             ',&
'     -help        Print description of this program.                                                                            ',&
'     -version     Print version information for this program                                                                    ',&
'                                                                                                                                ',&
'EXAMPLES                                                                                                                        ',&
'                                                                                                                                ',&
' Given files "x1" and "x2" are identical, and file "x3" is the same except one line                                             ',&
' is uppercase instead of lowercase ...                                                                                          ',&
'                                                                                                                                ',&
'   $ _cmp x1 x2                                                                                                                 ',&
'     x1 x2  are identical                                                                                                       ',&
'                                                                                                                                ',&
'   $ _cmp x1 x3                                                                                                                 ',&
'     x1 x3 differ: byte 03814,  line 00086, ADE= 99 c miniscule c ADE= 67 C majuscule C                                         ',&
'     STOP 3                                                                                                                     ',&
'                                                                                                                                ',&
'   $ _cmp x1 x3 -show                                                                                                           ',&
'     byte 03814,  line 00086, ADE= 99 c miniscule c              ADE= 67 C majuscule C                                          ',&
'     byte 03815,  line 00086, ADE=111 o miniscule o              ADE= 79 O majuscule O                                          ',&
'     byte 03816,  line 00086, ADE=110 n miniscule n              ADE= 78 N majuscule N                                          ',&
'     byte 03817,  line 00086, ADE=116 t miniscule t              ADE= 84 T majuscule T                                          ',&
'     byte 03818,  line 00086, ADE= 97 a miniscule a              ADE= 65 A majuscule A                                          ',&
'     byte 03819,  line 00086, ADE=105 i miniscule i              ADE= 73 I majuscule I                                          ',&
'     byte 03820,  line 00086, ADE=110 n miniscule n              ADE= 78 N majuscule N                                          ',&
'     byte 03821,  line 00086, ADE=115 s miniscule s              ADE= 83 S majuscule S                                          ',&
'     x1 x3  are different by 8 bytes                                                                                            ',&
'                                                                                                                                ',&
'   $ _cmp x1 x3 -show -quiet                                                                                                    ',&
'     3814            99    c  67    C                                                                                           ',&
'     3815           111    o  79    O                                                                                           ',&
'     3816           110    n  78    N                                                                                           ',&
'     3817           116    t  84    T                                                                                           ',&
'     3818            97    a  65    A                                                                                           ',&
'     3819           105    i  73    I                                                                                           ',&
'     3820           110    n  78    N                                                                                           ',&
'     3821           115    s  83    S                                                                                           ',&
'                                                                                                                                ',&
'   $ _cmp x1 x2 -quiet                                                                                                          ',&
'   $ echo $?                                                                                                                    ',&
'     0                                                                                                                          ',&
'                                                                                                                                ',&
'   $ _cmp x1 x3 -quiet                                                                                                          ',&
'     STOP 3                                                                                                                     ',&
'   $ echo $?                                                                                                                    ',&
'     3                                                                                                                          ',&
'                                                                                                                                ',&
'EXIT STATUS                                                                                                                     ',&
'     The following exit values are returned:                                                                                    ',&
'                                                                                                                                ',&
'      0     no differences were found                                                                                           ',&
'      1     differences were found                                                                                              ',&
'AUTHOR                                                                                                                          ',&
'   John S. Urban                                                                                                                ',&
'LICENSE                                                                                                                         ',&
'   Public Domain                                                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!      _cmp(1f) - [FUNIX] compare two files byte by byte
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!      _cmp file1 file2 [ -quiet] [ -show]| [ [ -help] [ -version] ]
!!##DESCRIPTION
!!      The _cmp(1) utility compares two files byte by byte and returns
!!      true if no differences are found, or false otherwise
!!##OPTIONS
!!      The following options are supported:
!!
!!      file1,file2  names of files to compare
!!      -quiet       suppress normal output
!!      -show        show all bytes that are different until an end of
!!                   file is encountered on both files
!!      -help        Print description of this program.
!!      -version     Print version information for this program
!!
!!##EXAMPLES
!!
!!
!!  Given files "x1" and "x2" are identical, and file "x3" is the same except one line
!!  is uppercase instead of lowercase ...
!!
!!    $ _cmp x1 x2
!!      x1 x2  are identical
!!
!!    $ _cmp x1 x3
!!      x1 x3 differ: byte 03814,  line 00086, ADE= 99 c miniscule c ADE= 67 C majuscule C
!!      STOP 3
!!
!!    $ _cmp x1 x3 -show
!!      byte 03814,  line 00086, ADE= 99 c miniscule c              ADE= 67 C majuscule C
!!      byte 03815,  line 00086, ADE=111 o miniscule o              ADE= 79 O majuscule O
!!      byte 03816,  line 00086, ADE=110 n miniscule n              ADE= 78 N majuscule N
!!      byte 03817,  line 00086, ADE=116 t miniscule t              ADE= 84 T majuscule T
!!      byte 03818,  line 00086, ADE= 97 a miniscule a              ADE= 65 A majuscule A
!!      byte 03819,  line 00086, ADE=105 i miniscule i              ADE= 73 I majuscule I
!!      byte 03820,  line 00086, ADE=110 n miniscule n              ADE= 78 N majuscule N
!!      byte 03821,  line 00086, ADE=115 s miniscule s              ADE= 83 S majuscule S
!!      x1 x3  are different by 8 bytes
!!
!!    $ _cmp x1 x3 -show -quiet
!!      3814            99    c  67    C
!!      3815           111    o  79    O
!!      3816           110    n  78    N
!!      3817           116    t  84    T
!!      3818            97    a  65    A
!!      3819           105    i  73    I
!!      3820           110    n  78    N
!!      3821           115    s  83    S
!!
!!    $ _cmp x1 x2 -quiet
!!    $ echo $?
!!      0
!!
!!    $ _cmp x1 x3 -quiet
!!      STOP 3
!!    $ echo $?
!!      3
!!
!!##EXIT STATUS
!!      The following exit values are returned:
!!
!!       0     no differences were found
!!       1     differences were found
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
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
'@(#)PRODUCT:        GPF library utilities and examples>',&
'@(#)PROGRAM:        _cmp(1)>',&
'@(#)DESCRIPTION:    compare two files byte by byte>',&
'@(#)VERSION:        1.0-20171126>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       2021-08-21 22:20:03 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
program same
!use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
use,intrinsic :: iso_fortran_env, only : iostat_end
use M_kracken, only : kracken,lget,sget,sgets
use M_strings, only : split, substitute
use M_strings, only : describe, visible
use M_verify,   only : stderr
implicit none

! ident_1="@(#)_cmp(1f): compare two files byte by byte"

character(len=4096),allocatable :: filenames(:)      ! array of filenames to read
character(len=256)              :: message           ! message field for returned messages
logical                         :: quiet   = .FALSE. ! switch to describe difference found
logical                         :: show    = .FALSE. ! switch to show all differences
integer                         :: fd(2)=[10,20]     ! file descriptor for file currently being read
integer                         :: ios               ! hold I/O error flag
integer                         :: i                 ! loop counter
character                       :: c1,c2             ! current character read
integer                         :: ios1,ios2         ! hold I/O error flag
integer                         :: icount  = 0       ! number of characters read from file
integer                         :: idiff   = 0       ! number of characters different in files
integer                         :: ilines  = 0       ! number of newline characters encountered in first file
!-----------------------------------------------------------------------------------------------------------------------------------
! define command arguments and parse command line
call kracken('same',' -version .F. -help .F. -quiet .F. -show .F.')
call help_usage(lget('same_help'))
call help_version(lget('same_version'))
quiet=lget('same_quiet')                       ! check if quiet flag present on command line
show=lget('same_show')                         ! check if show flag present on command line
filenames=sgets('same_oo')                     ! get filenames to scan from command line
if(size(filenames).ne.2)then
   write(*,'(a)')'*cmp* ERROR: missing filename. Two filenames required'
   stop 4
endif
!-----------------------------------------------------------------------------------------------------------------------------------
do i=1,2  ! open input files
   open(unit=fd(i),file=trim(filenames(i)),access='stream',status='old',iostat=ios,action='read',form='unformatted',iomsg=message)
   if(ios.ne.0)then
      call stderr('*cmp* ERROR: could not open '//trim(filenames(i)))
      call stderr('*cmp* ERROR: '//trim(message))
      stop 5
   endif
enddo
!-----------------------------------------------------------------------------------------------------------------------------------
ONE_CHAR_AT_A_TIME: do                                                ! loop through read of files one character at a time
   call getnextchar1(fd(1),c1,ios1)                                   ! get next character from buffered read from file 1
   call getnextchar2(fd(2),c2,ios2)                                   ! get next character from buffered read from file 2

   if(ios1.eq.iostat_end.and.ios2.eq.iostat_end)then                  ! reached end of both files so stop
      if(quiet)then                                                   ! no normal messages
      elseif(idiff.eq.0)then                                          ! message for identical files
         write(*,'(*(a:,1x))',advance='no')trim(filenames(1)),trim(filenames(2)),' are identical'
         write(*,*)
      else                                                            ! messages when files are different
         write(*,'(*(a:,1x))',advance='no')trim(filenames(1)),trim(filenames(2)),' are different by '
         write(*,'(i0," bytes")')idiff
      endif
      stop
   elseif(ios1.ne.0 )then                                             ! error or end of file on file 1
      if(.not.quiet)then
         write(*,*)'*cmp* ERROR: EOF or error on '//trim(filenames(1))//' before end of '//trim(filenames(2))
      endif
      stop 1
   elseif(ios2.ne.0 )then                                             ! error or end of file on file 2
      if(.not.quiet)then
         write(*,*)'*cmp* ERROR: EOF or error on '//trim(filenames(2))//' before end of '//trim(filenames(1))
      endif
      stop 2
   endif
   icount=icount+1                                                    ! increment count of characters read
   if(c1.eq.NEW_LINE('A'))ilines=ilines+1                             ! increment count of newline characters in file 1

   if(c1.ne.c2)then                                                   ! if characters are different
      idiff=idiff+1
      if(show)then
         if(quiet)then
            write(*,'(i0,tr8,i3,1x,a4,1x,i3,1x,a4)')icount,ichar(c1),visible(c1),ichar(c2),visible(c2)
         else
            write(*,'(" byte ",i0.5,", ")',advance='no')icount
            write(*,'(" line ",i0.5,", ")',advance='no')ilines+1
            write(*,'("ADE=",i3,1x,a,1x,a)',advance='no')ichar(c1),describe(c1),char(9)
            write(*,'(" ADE=",i3,1x,a)',advance='no')ichar(c2),describe(c2)
            write(*,*)
         endif
      else
         if(.not.quiet)then
            write(*,'(*(a:,1x))',advance='no')trim(filenames(1)),trim(filenames(2)),'differ:'
            write(*,'(" byte ",i0.5,", ")',advance='no')icount
            write(*,'(" line ",i0.5,", ")',advance='no')ilines+1
            write(*,'("ADE=",i3,1x,a)',advance='no')ichar(c1),describe(c1)
            write(*,'(" ADE=",i3,1x,a)',advance='no')ichar(c2),describe(c2)
            write(*,*)
         endif
         stop 3
      endif
   endif
enddo ONE_CHAR_AT_A_TIME
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine getnextchar1(fd,c,ios)
! replace "read(fd,iostat=ios) c" because gfortran on CygWin sixty times slower with plain read (no system buffering?)
! quick buffering read
implicit none
integer,intent(in)          :: fd
character,intent(out)       :: c
integer,intent(out)         :: ios
integer,parameter           :: bufsize=1048576
character(len=1),save       :: buff(bufsize)
integer,save                :: point=0
integer,save                :: filepoint=1
integer,save                :: sz=bufsize

ios=0

100 continue
select case(point)
case(0)                                            ! read a buffer
   read(fd,iostat=ios,pos=filepoint) buff(1:sz)
   if(is_iostat_end(ios))then                      ! this is the last buffer
      if(sz.ne.1)then                              ! try again with a smaller buffer
         sz=sz/2
         sz=max(1,sz)
         goto 100
      endif
   elseif(ios.eq.0)then                            ! no error occurred so successfully read a buffer
      c=buff(1)
      filepoint=filepoint+sz
      point=sz-1
   endif
case(1:)                                           ! getting a character from a previous buffer
   point=point-1
   c=buff(sz-point)
case default
   write(*,*)'*getnextchar1* internal error '
   read(fd,iostat=ios) c
end select
! assume if IOS is not zero, not called again until new file is started
   if(ios.ne.0)then
      filepoint=1
      point=0
      sz=bufsize
   endif
end subroutine getnextchar1
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine getnextchar2(fd,c,ios)
! replace "read(fd,iostat=ios) c" because gfortran on CygWin sixty times slower with plain read (no system buffering?)
! quick buffering read
implicit none
integer,intent(in)          :: fd
character,intent(out)       :: c
integer,intent(out)         :: ios
integer,parameter           :: bufsize=1048576
character(len=1),save       :: buff(bufsize)
integer,save                :: point=0
integer,save                :: filepoint=1
integer,save                :: sz=bufsize

ios=0
100 continue
select case(point)
case(0)                                            ! read a buffer
   read(fd,iostat=ios,pos=filepoint) buff(1:sz)
   if(is_iostat_end(ios))then                      ! this is the last buffer
      if(sz.ne.1)then                              ! try again with a smaller buffer
         sz=sz/2
         sz=max(1,sz)
         goto 100
      endif
   elseif(ios.eq.0)then                            ! no error occurred so successfully read a buffer
      c=buff(1)
      filepoint=filepoint+sz
      point=sz-1
   endif
case(1:)                                           ! getting a character from a previous buffer
   point=point-1
   c=buff(sz-point)
case default
   write(*,*)'*getnextchar2* internal error '
   read(fd,iostat=ios) c
end select
! assume if IOS is not zero, not called again until new file is started
   if(ios.ne.0)then
      filepoint=1
      point=0
      sz=bufsize
   endif
end subroutine getnextchar2
!-----------------------------------------------------------------------------------------------------------------------------------
end program same
!-----------------------------------------------------------------------------------------------------------------------------------
