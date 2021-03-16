          program demo_implied_do
          implicit none
             integer :: i
             ! [A-Z] [a-z] alphabet
             character(len=1),parameter :: a2z(26*2)=[(char(i),i=65,90),(char(i),i=97,122)]
             write(*,*)'Alphabet=',a2z
          end program demo_implied_do
