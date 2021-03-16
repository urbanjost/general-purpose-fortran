             program demo_reverse
             use M_strings, only: reverse
             implicit none
             character(len=:),allocatable  :: s
                write(*,*)'REVERSE STRINGS:',reverse('Madam, I''m Adam')
                s='abcdefghijklmnopqrstuvwxyz'
                write(*,*) 'original input string is ....',s
                write(*,*) 'reversed output string is ...',reverse(s)
             end program demo_reverse
