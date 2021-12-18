      program demo_fileclose
      use M_io, only : fileclose, fileopen
      implicit none
      integer :: lun
      integer :: ios, ierr
         lun=fileopen('<input.txt',ios=ierr)
         if(ierr.ne.0)then
            write(*,*)'<ERROR> opening file'
         endif
         ios=fileclose(lun)
      end program demo_fileclose
