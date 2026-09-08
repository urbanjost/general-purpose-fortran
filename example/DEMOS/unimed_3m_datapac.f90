     program demo_unimed
     use M_datapac, only : unimed, label, plotxt
     implicit none
     integer,parameter :: N=100
     real              :: X(N)
        call label('unimed')
        call unimed(N,X)
        call plotxt(x,n)
     end program demo_unimed
