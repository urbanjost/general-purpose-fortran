     program demo_nbran
     use m_datapac, only : nbran, plott, label, plotxt, sort
     implicit none
     integer,parameter :: n=400
     real              :: p
     integer           :: Npar
     integer           :: Istart
     real              :: x(n)
        call label('nbran')
        p=0.4
        Npar=3
        istart=12345
        call nbran(N,P,Npar,Istart,X)
        call plotxt(x,n)
        call sort(x,n,x) ! sort to show distribution
        call plotxt(x,n)
     end program demo_nbran
