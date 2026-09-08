      program demo_filedelete
      use M_io, only : filedelete, fileopen
      implicit none
      integer :: lun
      integer :: iostat
         lun=fileopen('<input.txt')
         iostat=filedelete(lun)
      end program demo_filedelete
