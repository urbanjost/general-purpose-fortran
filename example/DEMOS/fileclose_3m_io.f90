      program demo_fileclose
      use M_io, only : fileclose, fileopen
      implicit none
      integer :: lun
      integer :: iostat, ierr
         lun=fileopen('<input.txt',iostat=ierr)
         if(ierr /= 0)then
            write(*,*)'<ERROR> opening file'
         endif
         iostat=fileclose(lun)
      end program demo_fileclose
