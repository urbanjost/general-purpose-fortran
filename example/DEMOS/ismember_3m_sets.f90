     program demo_ismember
     use M_sets, only: ismember
     character(len=*),parameter :: g='(*(g0,1x))'
     integer, allocatable      :: A(:)
     integer, allocatable      :: B(:)

        write(*,g) 'ISMEMBER', 'Determine which elements of A are also in B.'

         A=[5,3,4,2]
         B=[2,4,4,4,6,8]
         write(*,g) 'A=', A
         write(*,g) 'B=', B
         write(*,g) ismember(A,B)

     end program demo_ismember
