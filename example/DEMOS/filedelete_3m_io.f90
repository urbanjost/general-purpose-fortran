           program demo_filedelete
           use M_io, only : filedelete, fileopen
           implicit none
           integer :: lun
           integer :: ios
              lun=fileopen('<input.txt')
              ios=filedelete(lun)
           end program demo_filedelete
