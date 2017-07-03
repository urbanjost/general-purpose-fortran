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
'NAME                                                                            ',&
'       _uname(1f) - [FUNIX]print system information                             ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'       _uname [OPTION]...                                                       ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'       Print certain system information. With no OPTION, print all              ',&
'       information, one value per line.                                         ',&
'                                                                                ',&
'       -a, --all                print all information, in the                   ',&
'                                following order:                                ',&
'                                                                                ',&
'       -s, --kernel-name        print the kernel name                           ',&
'       -n, --nodename           print the network node hostname                 ',&
'       -r, --kernel-release     print the kernel release                        ',&
'       -v, --kernel-version     print the kernel version                        ',&
'       -m, --machine            print the machine hardware name                 ',&
'                                                                                ',&
'       --help                   display this help and exit                      ',&
'       --version                output version information and exit             ',&
'EXAMPLE                                                                         ',&
'                                                                                ',&
'  Sample usage:                                                                 ',&
'                                                                                ',&
'      >_uname                                                                   ',&
'      >kernel-name    : CYGWIN_NT-10.0                                          ',&
'      >nodename       : buzz                                                    ',&
'      >kernel-release : 2.6.0(0.304/5/3)                                        ',&
'      >kernel-version : 2016-08-31 14:32                                        ',&
'      >machine        : x86_64                                                  ',&
'                                                                                ',&
'      >_uname -all                                                              ',&
'      >CYGWIN_NT-10.0 buzz 2.6.0(0.304/5/3) 2016-08-31 14:32 x86_64             ',&
'                                                                                ',&
'      >_uname -machine                                                          ',&
'      >x86_64                                                                   ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        _uname(1f) - [FUNIX]print system information
!!
!!##SYNOPSIS
!!
!!        _uname [OPTION]...
!!
!!##DESCRIPTION
!!        Print certain system information. With no OPTION, print all
!!        information, one value per line.
!!
!!        -a, --all                print all information, in the
!!                                 following order:
!!
!!        -s, --kernel-name        print the kernel name
!!        -n, --nodename           print the network node hostname
!!        -r, --kernel-release     print the kernel release
!!        -v, --kernel-version     print the kernel version
!!        -m, --machine            print the machine hardware name
!!
!!        --help                   display this help and exit
!!        --version                output version information and exit
!!##EXAMPLE
!!
!!
!!   Sample usage:
!!
!!       >_uname
!!       >kernel-name    : CYGWIN_NT-10.0
!!       >nodename       : buzz
!!       >kernel-release : 2.6.0(0.304/5/3)
!!       >kernel-version : 2016-08-31 14:32
!!       >machine        : x86_64
!!
!!       >_uname -all
!!       >CYGWIN_NT-10.0 buzz 2.6.0(0.304/5/3) 2016-08-31 14:32 x86_64
!!
!!       >_uname -machine
!!       >x86_64
!===================================================================================================================================
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
'@(#)PROGRAM:        _uname(3f)>',&
'@(#)DESCRIPTION:    print system information>',&
'@(#)VERSION:        1.0, 20161107>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:15:01 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program testit
use M_kracken, only: kracken, lget
use M_system, only: system_uname
implicit none
character(len=1024) :: string=' '
logical             :: picked=.false.
integer             :: i
logical             :: pick_all
! unfortunately this currently only defines a vector, not a matrix
character(len=*),parameter :: names1(10)=[  &
      & 'kernel-name   ','s             ',  &
      & 'nodename      ','n             ',  &
      & 'kernel-release','r             ',  &
      & 'kernel-version','v             ',  &
      & 'machine       ','m             ']
character(len=14), dimension(:, :), allocatable :: names

! arrange the data in NAMES1(:) into a two-column format in NAMES(:,2)
allocate (names(size(names1)/2, 2))
names = transpose(reshape( names1, (/ size(names, 2), size(names, 1) /)))

! define and parse command line options
call kracken('uname','               &
   &  -a  .f.  -all             .f.  &
   &  -s  .f.  -kernel-name     .f.  &
   &  -n  .f.  -nodename        .f.  &
   &  -r  .f.  -kernel-release  .f.  &
   &  -v  .f.  -kernel-version  .f.  &
   &  -m  .f.  -machine         .f.  &
   & -help .f. -version .f. ')

! process command line options
call help_usage(lget('uname_help'))
call help_version(lget('uname_version'))
if( lget('uname_all') .or. lget('uname_a') )then
   pick_all=.true.
else
   pick_all=.false.
endif

! if all picked or specific value picked print it on a single line
do i=1,size(names,dim=1)
   string=' '
   if( lget('uname_'//names(i,1)) .or. lget('uname_'//names(i,2)) .or. pick_all)then
      call system_uname(trim(names(i,2)),string)
      write(*,'(1x,a)',advance='no')trim(string)
      picked=.true.
   endif
enddo

! print all long names followed by returned value for that name
if(.not.picked)then
   do i=1,size(names,dim=1)
      call system_uname(trim(names(i,2)),string)
      write(*,'(a," : ",a)')names(i,1), trim(string)
   enddo
else
   write(*,*)
endif

deallocate (names)

end program testit
