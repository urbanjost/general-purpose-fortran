           program demo_nospace
           use M_strings, only: nospace
           implicit none
           character(len=:),allocatable  :: s
              s='  This     is      a     test  '
              write(*,*) 'original input string is ....',s
              write(*,*) 'processed output string is ...',nospace(s)
              if(nospace(s).eq.'Thisisatest')then
                 write(*,*)'nospace test passed'
              else
                 write(*,*)'nospace test error'
              endif
           end program demo_nospace
