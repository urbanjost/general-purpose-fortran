     program demo_chsran
     use m_datapac, only : chsran, plott, label, plotxt, sort
     implicit none
     integer,parameter :: n=4000
     integer           :: iseed
     integer           :: Nu
     real              :: x(n)
        call label('chsran')
        Nu=8
        iseed=12345
        call chsran(N,Nu,Iseed,X)
        call plotxt(x,n)
        call sort(x,n,x) ! sort to show distribution
        call plotxt(x,n)
     end program demo_chsran
