     program demo_elementcopy
     use m_la, only : elementcopy
     implicit none
     character(len=*),parameter :: g='(*(g0:,","))'
     real :: b, b1(3), b2(2,3), b3(2,2,2)
     real :: c8(8), c6(6), c3(3), c
     integer :: ib, ib1(3), ib2(2,3), ib3(2,2,2)
     integer :: ic8(8), ic6(6), ic3(3), ic
        ! default real
        call elementcopy(100.0,b)
        write(*,g)'b',b
        call elementcopy([1.0,2.0,3.0],b1)
        write(*,g)'b1',b1
        call elementcopy(reshape([1.0,2.0,3.0,4.0,5.0,6.0],[2,3]),b2)
        write(*,g)'b2',b2
        call elementcopy(reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0],[2,2,2]),b3)
        write(*,g)'b3',b3
        call elementcopy(b3,c8) ! pack
        write(*,g)'c8',c8
        call elementcopy(b3*10,c3) ! smaller
        write(*,g)'c3',c3
        call elementcopy(pack(b3*111.0,.true.),b) ! to scalar
        write(*,g)'b',b
        c6=-999.0
        call elementcopy(b1*10,c6) ! bigger
        write(*,g)'c6',c6
        call elementcopy(b3(2:,2,2),c) !  to scalar from vector
        write(*,g)'c',c
        call elementcopy(b3(2,1,1),c) !  to scalar from element
        write(*,g)'c',c
        call elementcopy(b3,c) !  to scalar
        write(*,g)'c',c
        ! default integer
        call elementcopy(100,ib)
        write(*,g)'ib',ib
        call elementcopy([1,2,3],ib1)
        write(*,g)'ib1',ib1
        call elementcopy(reshape([1,2,3,4,5,6],[2,3]),ib2)
        write(*,g)'ib2',ib2
        call elementcopy(reshape([1,2,3,4,5,6,7,8],[2,2,2]),ib3)
        write(*,g)'ib3',ib3
        call elementcopy(ib3,ic8) ! pack
        write(*,g)'ic8',ic8
        call elementcopy(ib3*10,ic3) ! smaller
        write(*,g)'ic3',ic3
        call elementcopy(pack(ib3*111,.true.),ib) ! to scalar
        write(*,g)'ib',ib
        ic6=-999
        call elementcopy(ib1*10,ic6) ! bigger
        write(*,g)'ic6',ic6
        call elementcopy(ib3(2:,2,2),ic) !  to scalar from vector
        write(*,g)'ic',ic
        call elementcopy(ib3(2,1,1),ic) !  to scalar from element
        write(*,g)'ic',ic
        call elementcopy(ib3,ic) !  to scalar
        write(*,g)'ic',ic
        !
        tesseract: block
        integer :: box(2,3,4,5)
        integer :: i
           call elementcopy([(i,i=1,size(box))],box)
           write(*,g)'box',box
        endblock tesseract
     end program demo_elementcopy
