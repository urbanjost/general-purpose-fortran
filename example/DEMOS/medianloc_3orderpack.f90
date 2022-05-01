     program demo_medianloc
     ! return index of median value
     use M_orderpack, only : medianloc
     implicit none
     real,allocatable :: INVALS(:)
     character(len=:),allocatable :: cdont(:)
     character(len=*),parameter :: fmt='(i5,t11,g0)'
     integer :: ii
        write(*,*) 'location  median'

        INVALS=[80.0,70.0,20.0,10.0,1000.0]
        call medianloc(INVALS,ii)
        write(*,fmt) ii,INVALS(ii)
        !
        INVALS=[11, 22, 33, 44, 55, 66, 77, 88]
        call medianloc(INVALS,ii)
        write(*,fmt) ii,INVALS(ii)
        !
        INVALS=[11.0d0,77.0d0,22.0d0,66.0d0,33.0d0,88.0d0]
        call medianloc(INVALS,ii)
        write(*,fmt) ii,INVALS(ii)
        !
        cdont=[character(len=20) :: 'apple','bee','cherry','duck',&
                'elephant','finger','goose','h','insect','j']
        call medianloc(cdont,ii)
        write(*,fmt) ii,cdont(ii)
        !
     end program demo_medianloc
