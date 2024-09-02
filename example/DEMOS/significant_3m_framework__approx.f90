     program demo_significant
     use M_framework__approx, only : significant
     implicit none
     integer :: i
     real :: r, v
     character(len=*),parameter :: g='(*(g0.7,1x))'

        write(*,g)significant([8765.43210,0.1234567890],5)

        write(*,*)'default:',1.23456789012345
        write(*,g)significant(1.23456789012345,[1,2,3,4,5,6,7,8,9])
        write(*,g)significant(1.23456789012345,[1,2,3,4,5,6,7,8,9],'RU'),'RU'
        write(*,g)significant(1.23456789012345,[1,2,3,4,5,6,7,8,9],'RD'),'RD'
        write(*,g)significant(1.23456789012345,[1,2,3,4,5,6,7,8,9],'RZ'),'RZ'
        write(*,g)significant(1.23456789012345,[1,2,3,4,5,6,7,8,9],'RN'),'RN'
        write(*,g)significant(1.23456789012345,[1,2,3,4,5,6,7,8,9],'RC'),'RC'
        write(*,g)significant(1.23456789012345,[1,2,3,4,5,6,7,8,9],'RP'),'RP'
     end program demo_significant
