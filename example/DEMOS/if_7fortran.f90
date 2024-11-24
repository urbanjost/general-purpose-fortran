          program demo_if
          implicit none
          character(len=:),allocatable :: cvar
          logical :: PROP=.false.
          real :: a, b, c, d
          integer :: case=0
          integer :: i, j, k
          logical :: nextprop=.true.
           !
           ! basic IF
           !
           cvar='NO'
           if (cvar == 'RESET') then
              i = 0; j = 0; k = 0
           endif
           !
           ! labeled and nested IF constructs
           !
           OUTER: if (case.eq.0)then
              PROOF_DONE: if (PROP) then
                 write (3, '(''QED'')')
                 exit OUTER
              else
                 PROP = nextprop
              endif PROOF_DONE
              write(*,*)'END OF PROOF_DONE'
           else OUTER
                   write(*,*)'else outer'
           endif OUTER
           !
           ! if-elseif-endif
           !
           if (a > 0) then
              b = c/a
              if (b > 0) then
                 d = 1.0
              endif
           elseif (c > 0) then
              b = a/c
              d = -1.0
           else
              b = abs (max (a, c))
              d = 0
           endif
           !
          end program demo_if
