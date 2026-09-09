     program demo_midr
     use M_datapac, only : midr, label
     implicit none
     integer :: i
     real :: xmidr
        call label('midr')

        call midr([real :: (i,i=0,100) ],101,1,xmidr)
        write(*,*)merge('GOOD','BAD ',xmidr == 50.0),xmidr

        call midr([real :: (i,i=0,101) ],102,1,xmidr)
        write(*,*)merge('GOOD','BAD ',xmidr == 50.5),xmidr

     end program demo_midr
