      program demo_open
      integer            :: iostat
      character(len=256) :: iomsg
      integer            :: lun
         open  (                  &
         & newunit=lun,           &
         & file='employee.names', &
         & action='readwrite',    & ! read write readwrite
         & iostat=iostat,         &
         & status='unknown',      & ! old new replace unknown
         & iomsg=iomsg)
         if (iostat < 0) then
            ! Perform end-of-file processing
            call end_processing()
         elseif (iostat > 0) then
            ! Perform error processing
            write(*,'(a)')trim(iomsg)
            call error_processing()
            stop
         else
            write(*,*)'OPENED FILE'
         endif
      contains
         !
         subroutine end_processing()
            write(*,*)'<END OF FILE>:',iostat,'iomsg=',trim(iomsg)
            close(unit=lun,iostat=iostat)
            stop
         end subroutine end_processing
         !
         subroutine error_processing()
            write(*,*)'<ERROR>:',iostat,'iomsg=',trim(iomsg)
            close(unit=lun,iostat=iostat)
            stop
         end subroutine error_processing
         !
      end program demo_open
