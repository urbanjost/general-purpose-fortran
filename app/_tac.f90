program tac
!LICENSE:PD)
use M_io,      only : swallow
use M_strings, only : notabs
implicit none
character(len=4096)          :: FILENAME   ! file to read
character(len=:),allocatable :: pageout(:) ! array to hold file in memory
integer                      :: i, j
integer                      :: icount
   icount=command_argument_count()
   do j=1,icount
      call get_command_argument(j, FILENAME) ! get a filename
      ! allocate character array and copy file into it
      if(FILENAME.eq.'-')then
         call swallow(5,pageout)
      else
         call swallow(FILENAME,pageout)
      endif
      if(.not.allocated(pageout))then
         write(*,*)'*tac* failed to load file '//FILENAME
      else
         ! write file from last line to first line
         do i=size(pageout),1,-1
            write(*,'(a)')trim(pageout(i))
         enddo
         deallocate(pageout)  ! release memory
      endif
   enddo
end program tac

