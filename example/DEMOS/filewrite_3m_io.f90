           program demo_filewrite
           use M_io, only : filewrite
           implicit none
           integer :: ierr
           character(len=:),allocatable :: data(:)
              data=[ character(len=80) :: &
                   &'This is the text to write  ', &
                   &'into the file. It will be  ', &
                   &'trimmed on the right side. ', &
                   &' ', &
                   &'     That is all Folks!    ', &
                   &'']
              ierr=filewrite('_scratch.txt',data)
           end program demo_filewrite
