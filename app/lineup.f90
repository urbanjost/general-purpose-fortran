program testit
!@(#) line up :: string, mostly for Fortran declarations
use M_io,      only : fileread
use M_strings, only : stretch
use M_CLI2,    only : set_args, filenames=>unnamed
implicit none
character(len=:),allocatable :: pageout(:) ! page to hold file in memory
character(len=:),allocatable :: left,right
integer :: i, j, k, ii, imax
call set_args('', help_text=[ CHARACTER(LEN=128) :: &
   'NAME                                                                         ',&
   '    lineup(1f) - [APPS] align :: strings in Fortran declarations             ',&
   '    (LICENSE:PD)                                                             ',&
   '                                                                             ',&
   'SYNOPSIS                                                                     ',&
   '    lineup [filename(s) ][ --help| --version]                                ',&
   '                                                                             ',&
   'DESCRIPTION                                                                  ',&
   '    A simple filter that finds and aligns the string "::" in a block of text.',&
   '    A stylistic preference for Fortran declaration statements                ',&
   '                                                                             ',&
   'OPTIONS                                                                      ',&
   '    filename(s)  defaults to "-", which represents stdin. Otherwise it is    ',&
   '                 a list of files to process one file at a time.              ',&
   '    --help       display this help and exit                                  ',&
   '    --version    output version information and exit                         ',&
   '                                                                             ',&
   'EXAMPLES                                                                     ',&
   '    Sample input:                                                            ',&
   '                                                                             ',&
   '       lineup <<\EOF                                                         ',&
   '       logical :: glob                                                       ',&
   '       character(len=*) :: tame ! A string without wildcards                 ',&
   '       character(len=*) :: wild ! A corresponding wildcards string           ',&
   '       character(len=len(tame)+1) :: tametext                                ',&
   '       character(len=len(wild)+1):: wildtext                                 ',&
   '       character(len=1),parameter :: NULL=char(0)                            ',&
   '       integer  :: wlen                                                      ',&
   '       integer:: ti, wi                                                      ',&
   '       integer  :: i                                                         ',&
   '       character(len=:),allocatable :: tbookmark, wbookmark                  ',&
   '       EOF                                                                   ',&
   '                                                                             ',&
   '    Sample output:                                                           ',&
   '                                                                             ',&
   '       logical                      :: glob                                  ',&
   '       character(len=*)             :: tame ! A string without wildcards     ',&
   '       character(len=*)             :: wild ! A corresponding wildcards string  ',&
   '       character(len=len(tame)+1)   :: tametext                                 ',&
   '       character(len=len(wild)+1)   :: wildtext                                 ',&
   '       character(len=1),parameter   :: NULL=char(0)                             ',&
   '       integer                      :: wlen                                     ',&
   '       integer                      :: ti, wi                                   ',&
   '       integer                      :: i                                        ',&
   '       character(len=:),allocatable :: tbookmark, wbookmark                     ',&
   '                                                                                ',&
   'SEE ALSO                                                                        ',&
   '    column(1)                                                                   ',&
   'AUTHOR                                                                          ',&
   '   John S. Urban                                                                ',&
   'LICENSE                                                                         ',&
   '   Public Domain                                                                ',&
   ''], &
   version_text=[ CHARACTER(LEN=128) :: &
   'PRODUCT:        GPF (General Purpose Fortran) utilities and examples            ',&
   'PROGRAM:        lineup(1)                                                       ',&
   'DESCRIPTION:    Align "::" strings in Fortran declararations into a column      ',&
   'VERSION:        1.0, 2024-08-18                                                 ',&
   'AUTHOR:         John S. Urban                                                   ',&
   'REPORTING BUGS: http://www.urbanjost.altervista.org/                            ',&
   'HOME PAGE:      http://www.urbanjost.altervista.org/index.html                  ',&
   'LICENSE:        Public Domain. This is free software: you are free to change    ',&
   'LICENSE:        and redistribute it.                                            ',&
   '                There is NO WARRANTY, to the extent permitted by law.           ',&
   ''])

   if(size(filenames).eq.0)filenames=['-']
   do ii=1,size(filenames)
      call fileread(filenames(ii),pageout)
      imax=0
      do i=1,size(pageout)
         j=index(pageout(i),'::')
         if(j.gt.1)then
            j=len_trim(pageout(i)(:j-1))+2
         endif
         imax=max(imax,j)
      enddo
      do i=1,size(pageout)
         j= index(pageout(i),'::')
         if( j.ge.2)then
            left=trim(pageout(i)(:j-1))
            write(*,'(*(g0))')stretch(left,imax-1,' '),':: ',adjustl(trim(pageout(i)(j+2:)))
         else
            write(*,'(*(g0))')trim(pageout(i))
         endif
      enddo
   enddo
end program testit
! size=          10 ,len=          87
! IN:
! logical :: glob
! character(len=*)  :: tame       ! A string without wildcards
! character(len=*)  :: wild       ! A (potentially) corresponding string with wildcards
! character(len=len(tame)+1) :: tametext
! character(len=len(wild)+1) :: wildtext
! character(len=1),parameter :: NULL=char(0)
! integer  :: wlen
! integer  :: ti, wi
! integer  :: i
! character(len=:),allocatable :: tbookmark, wbookmark
! OUT:
! logical                      :: glob
! character(len=*)             :: tame       ! A string without wildcards
! character(len=*)             :: wild       ! A (potentially) corresponding string with wildcards
! character(len=len(tame)+1)   :: tametext
! character(len=len(wild)+1)   :: wildtext
! character(len=1),parameter   :: NULL=char(0)
! integer                      :: wlen
! integer                      :: ti, wi
! integer                      :: i
! character(len=:),allocatable :: tbookmark, wbookmark
