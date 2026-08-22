program test_id
implicit none
   call platform()
contains

subroutine platform()
use, intrinsic :: iso_fortran_env, only : compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_options
implicit none
character(len=:),allocatable :: version, options
character(len=*),parameter   :: nl=new_line('a')
integer                      :: where, start, break, i, last, col
   version=compiler_version()//' '
   options=' '//compiler_options()
   start=1
   do 
      where=index(options(start:),' -')
      if(where.eq.0)exit
      break=where+start-1
      options(break:break)=nl
      start=where
   enddo
   if(start.eq.1)then
      do 
         where=index(options(start:),' /')
         if(where.eq.0)exit
         break=where+start-1
         options(break:break)=nl
         start=where
      enddo
   endif
   last=len_trim(version)+1
   col=0
   do i=1,len_trim(version)
    col=col+1
    if(version(i:i).eq.' ')last=i
    if(col.gt.76)then
       version(last:last)=nl
       col=0
    endif
   enddo
   print '(a,/,3x,*(a))', 'This file was compiled by :', inset(version)
   if(options.ne.'')then
      print '(*(a))', 'using the options :', inset(options)
   endif
end subroutine platform

function inset(string) result(longer)
character(len=*),intent(in)  :: string
character(len=:),allocatable :: longer
character(len=*),parameter   :: nl=new_line('a')
integer                      :: i
   longer=''
   do i=1,len(string)
      longer=longer//string(i:i)
      if(string(i:i).eq.nl)then
         longer=longer//'   '
      endif
   enddo
end function inset

end program test_id
