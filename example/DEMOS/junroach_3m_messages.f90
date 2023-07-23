       program demo_junroach
       use M_messages, only : junroach
       implicit none
       logical :: error=.true.
          if (error)then
             write(*,*)'ERROR:'
             call junroach('s')
             write(*,*)'   explanation of error.'
             write(*,*)' '
             write(*,*)'Pauci sunt errata in codice tuo'
             write(*,*)' or'
             write(*,*)'There are a few errors in your code'
          endif
       end program demo_junroach
