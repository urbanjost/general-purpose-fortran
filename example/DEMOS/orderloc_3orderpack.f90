     program demo_orderloc
     ! find Nth lowest ordered value in an array without sorting entire array
     use M_orderpack, only : orderloc
     use M_orderpack, only : medianloc
     implicit none
     integer,allocatable :: iarr(:)
     character(len=*),parameter :: list= '(*(g0:,", "))',sp='(*(g0,1x))'
     integer :: i
     integer :: indx
        iarr=[80,70,30,40,50,60,20,10,0,-100]
        print list, 'ORIGINAL:',iarr
        ! like minloc(3f) and maxloc(3f)
        print sp,'minloc',orderloc(iarr,1),                minloc(iarr)
        print sp,'maxloc',orderloc(iarr,size(iarr)),       maxloc(iarr)
        ! can find median
        call medianloc(iarr,indx)
        print sp,'median',orderloc(iarr,(size(iarr)+1)/2), indx
        ! but more general so can find location of the Nth lowest value ...
        !
        ! sort the hard way, finding location of Nth value one at a time
        do i=1,size(iarr)
           write(*,sp,advance='no') iarr(orderloc(iarr,i))
        enddo
        print *
     contains
     subroutine printme(n)
     integer,intent(in) :: n
     integer :: ii
        ii=orderloc(iarr,n)
        print sp,'nord=',n,' index=',ii,' fractile=',iarr(ii)
     end subroutine printme
     end program demo_orderloc
