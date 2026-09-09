     program demo_midm
     use M_datapac, only : midm, label
     implicit none
     integer :: i
     real :: xmidm
        call label('midm')

        call midm([real :: (i,i=0,100) ],101,1,xmidm)
        write(*,*)merge('GOOD','BAD ',xmidm == 50.0),xmidm

        call midm([real :: (i,i=0,101) ],102,1,xmidm)
        write(*,*)merge('GOOD','BAD ',xmidm == 50.5),xmidm

     end program demo_midm
