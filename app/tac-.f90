program tac
!LICENSE:PD)
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
use M_io,      only : fileread
use M_strings, only : switch
use M_CLI2   , only : set_args, lget, sget, filenames=>unnamed, specified
implicit none
character(len=:),allocatable     :: help_text(:), version_text(:)
call setup()
!  define command arguments, default values and crack command line
   call set_args(' --transpose:t F ',help_text,version_text)
   if(size(filenames)==0)filenames=['-']
   if(lget('transpose'))then
      call rotate()
   else
      call bottom_to_top()
   endif
contains
subroutine bottom_to_top()
character(len=:),allocatable :: pageout(:) ! array to hold file in memory
integer                      :: i, j
integer                      :: icount
   icount=size(filenames)
   if(icount.eq.0)filenames=['-']
   do j=1,max(icount,1)
      ! allocate character array and copy file into it
      if(filenames(j).eq.'-')then
         call fileread(5,pageout)
      else
         call fileread(filenames(j),pageout)
      endif
      if(.not.allocated(pageout))then
         write(*,*)'*tac* failed to load file '//filenames(j)
      else
         ! write file from last line to first line
         do i=size(pageout),1,-1
            write(*,'(a)')trim(pageout(i))
         enddo
         deallocate(pageout)  ! release memory
      endif
   enddo
end subroutine bottom_to_top

subroutine rotate()
! transpose file
character(len=:),allocatable :: pageout(:) ! hold file as lines
character(len=1),allocatable :: chars(:,:) ! hold file as letters
integer                      :: i, j
integer                      :: icount
   icount=size(filenames)
   if(icount.eq.0)filenames=['-']
   do j=1, max(1,icount)
      ! allocate character array and copy file into it
      if(allocated(pageout))deallocate(pageout)  ! release memory
      if(filenames(j).eq.'-')then
         call fileread(5,pageout)
      else
         call fileread(filenames(j),pageout)
      endif
      ! convert to a matrix of single characters
      if(allocated(chars))deallocate(chars)
      allocate(character(len=1) :: chars(size(pageout),len(pageout)))
      do i=1,size(pageout)
         chars(i,:)=switch(pageout(i))
      enddo
      ! write file from first to last character column
      chars=transpose(chars)
      write(*,'(*(a))')(chars(i,:),new_line('a'),i=1,len(pageout))
   enddo
end subroutine rotate

subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'   tac-(1f) - [FUNIX:FILESYSTEM] reverse or transpose lines in a file',&
'   (LICENSE:PD)',&
'',&
'SYNOPSIS',&
'  syntax:',&
'',&
'   tac- [NAME...] [ --transpose]',&
'     or',&
'   tac- -help|-version',&
'',&
'DESCRIPTION',&
'   Read file into memory and then write it out with the lines',&
'   reversed or transposed.',&
'',&
'   If no NAME is specified read data from stdin.',&
'',&
'OPTIONS',&
'  NAME(S)         filenames',&
'  --transpose,-t  transpose the file instead of reversing it',&
'  --help          display this help and exit',&
'  --version       output version information and exit',&
'',&
'EXAMPLES',&
' Typical usage:',&
'',&
'  tac myfile.txt',&
'',&
'SEE ALSO',&
'   tac(1), cat(1)',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples',&
'PROGRAM:        tac-(1f)',&
'DESCRIPTION:    reverse lines in file or transpose file',&
'VERSION:        1.0.0',&
'DATE:           2015-06-26',&
'AUTHOR:         John S. Urban',&
'REPORTING BUGS: http://www.urbanjost.altervista.org/',&
'HOME PAGE:      http://www.urbanjost.altervista.org/index.html',&
'LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.',&
'                There is NO WARRANTY, to the extent permitted by law.',&
'']
end subroutine setup

end program tac
