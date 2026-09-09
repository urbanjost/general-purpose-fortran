program txt2f90
! make program to convert text file to Fortran character definition
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdout=>OUTPUT_UNIT
use m_cli2,    only : set_args, sgets, filenames=>unnamed
use m_io,      only : fileread, basename
use m_strings, only : substitute, transliterate, stretch
implicit none
character(len=:),allocatable :: pageout(:) ! array to hold file in memory
integer                      :: i, j, iwidth, isize
character(len=*),parameter   :: gen='(*(g0,1x))'
character(len=:),allocatable :: varname
character(len=:),allocatable :: help_text(:), version_text(:)
   call setup()
   call set_args('',help_text,version_text)
   if(size(filenames).eq.0)then
      filenames=['-']  ! if no file specified, read from stdin
   endif
   do i=1,size(filenames)
      call fileread(filenames(i),pageout)
      iwidth=len(pageout)
      isize=size(pageout)
      if(.not.allocated(pageout))then
         write(stderr,gen)'*demo_fileread* failed to load file',filenames(i)
      else
         varname=basename(filenames(i))
         if(varname.eq.'-')varname='stdin'
         varname=transliterate(varname,'-.','_')
         pageout=pageout//repeat(' ',len(pageout)) ! ensure space in buffer for substitute
         call substitute(pageout,"'","''")         ! change single quotes in input to two adjacent single quotes
         ! write file from first line to last line
         write(stdout,'(a,i0,a)')trim(varname)//'=[ character(len=',iwidth,') :: &'
         if(isize.gt.1) write(stdout,'("''",a,"'',&")') (stretch(trim(pageout(j)),iwidth),j=1,isize-1)
         write(stdout,'("''",a,"'']")') stretch(trim(pageout(isize)),iwidth)
         deallocate(pageout)  ! release memory
      endif
   enddo
contains
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'   txt2f90(1f) - [FUNIX:ENCODE] encode a file as a Fortran character            ',&
'   variable declaration                                                         ',&
'   (LICENSE:MIT)                                                                ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'    txt2f90 [FILE]                                                              ',&
'    |[ --help|--version]                                                        ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
' Read a file or stdin and write it back out as a Fortran character variable     ',&
' declaration.                                                                   ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'                                                                                ',&
'    filename             name of file to encode                                 ',&
'    --version,-v         Print version information on standard output then      ',&
'                         exit successfully.                                     ',&
'    --help,-h            Print usage information on standard output then        ',&
'                         exit successfully.                                     ',&
'EXAMPLE                                                                         ',&
'   Sample commands                                                              ',&
'                                                                                ',&
'    txt2f90 input > output.f90                                                  ',&
'SEE ALSO                                                                        ',&
'    prep(1)                                                                     ',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples            ',&
'PROGRAM:        txt2f90(1f)                                                     ',&
'DESCRIPTION:    encode a file as a Fortran character variable declaration       ',&
'VERSION:        1.0, 2024-11-24                                                 ',&
'AUTHOR:         John S. Urban                                                   ',&
'LICENSE:        MIT                                                             ',&
'']
end subroutine setup
end program txt2f90
