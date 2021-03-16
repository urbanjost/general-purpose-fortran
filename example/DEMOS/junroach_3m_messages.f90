            program demo_junroach
            use M_messages, only : junroach
            implicit none
            logical :: error=.true.
               if (error)then
                  write(*,*)'ERROR:'
                  call junroach('s')
                  write(*,*)'   explanation of error.'
               endif
            end program demo_junroach
