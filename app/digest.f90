program demo_sha256
use,intrinsic :: iso_fortran_env, only : ERROR_UNIT
use M_hashkeys,                   only : sha256, test_suite_sha256
use M_io,                         only : slurp
use M_strings,                    only : switch
use M_system,                     only : system_isreg
implicit none

! ident_1="@(#)sha256(1f): read file into memory and generate SHA-256 digest value"

character(len=1),allocatable :: text(:)                      ! array to hold file in memory
character(len=:),allocatable :: string
integer                      :: i=0
character(len=4096)          :: filename
   do i=1,command_argument_count()                           ! step through filenames on command line
      call get_command_argument(i, filename)
      if(.not.system_isreg(filename))cycle                   ! ignore anything except regular files
      call slurp(filename,text)                              ! allocate character array and copy file into it
      if(.not.allocated(text))then
         write(ERROR_UNIT,*)'*sha256* ERROR: failed to load file '//trim(filename)
      else
         string=switch(text)                                 ! switch array to a single character variable
         deallocate(text)                                    ! release memory
         write(*,*)sha256(string),len(string),trim(filename) ! write digest value
      endif
   enddo
   if(i.le.1) call test_suite_sha256()
end program demo_sha256
