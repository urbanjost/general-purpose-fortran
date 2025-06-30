program test_id
use, intrinsic :: iso_fortran_env, only : compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_options
implicit none
character(len=:),allocatable :: version, options
character(len=*),parameter   :: nl=new_line('a')
integer                      :: where, start, break
   version=compiler_version()
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
   print '(*(1x,a))', &
    'This file was compiled by ', &
    version,nl,        &
    'using the options ',         &
    options
end program test_id
