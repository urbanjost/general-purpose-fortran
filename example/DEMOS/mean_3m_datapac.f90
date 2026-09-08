     program demo_mean
     use M_datapac, only : mean, label
     implicit none
     real :: sp_mean
     double precision :: dp_mean
        call label('mean')
        call mean([4.0, 36.0, 45.0, 50.0, 75.0], 5, 1, sp_mean)
        write(*,*)sp_mean,sp_mean==42.0
        call mean([4.0d0, 36.0d0, 45.0d0, 50.0d0, 75.0d0], 5, 1, dp_mean)
        write(*,*)dp_mean,dp_mean==42.0
     end program demo_mean
