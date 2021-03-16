            program demo_process_readall
             use M_process ,only: process_readall
             !!use M_strings ,only: split
             implicit none
             integer                      :: ierr
             integer                      :: i
             character(len=:),allocatable :: string
             character(len=:),allocatable :: array(:)
                string=process_readall('ls',delim=NEW_LINE("A"),ierr=ierr)
                !!call split(string,array,delimiters=NEW_LINE("A"))
                !!do i=1,size(array)
                !!   write(*,'(i0,t10,"[",a,"]")')i,trim(array(i))
                !!enddo
                !!write(*,*)string=process_readall(&
                !!& 'ls',delim=NEW_LINE("A"),ierr=ierr)
                write(*,*)string
             end program demo_process_readall
