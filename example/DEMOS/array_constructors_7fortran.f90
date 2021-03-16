                 program demo_array_constructors ! initializing small arrays
                 use M_display, only : disp, disp_set
                 implicit none
                 integer :: i
                 integer, parameter :: yy(*) = [  10,20,30  ,  40,50,60  ] ! make some data in a vector, could type this where yy appears below
                 ! xx is same thing as yy, just using syntax for filling it that makes it clearer what I want to do with the data
                 integer, parameter :: xx(*) = [ [10,20,30] , [40,50,60] ] ! make some data in a vector, could type this where xx appears below

                 integer, dimension(2,3)::aa = reshape(xx,shape(aa),order=[2,1])      ! 2d by rows using reshaped scalar expression
                 integer, dimension(2,3)::bb = reshape(xx,shape(bb)             )     ! 2d by columns
                 integer, dimension(2,3)::cc = reshape(xx,shape(cc),order=[1,2])      ! 2d by columns
                 integer, dimension(2,3)::dd = reshape([(i*10,i=1,size(dd))],shape(dd)) ! an implied do by columns

                 integer, dimension(2,3):: ff, gg, hh
                 ! CANNOT DO
                 !integer, dimension(2,3)::ff = [10,20,30,40,50,60 ]                                     ! 2d by columns
                 ! BUT CAN DO
                 data ff/10,20,30,40,50,60/  ! fill 2D with simple data statement
                 ! AND CAN DO
                 ! multi-dimensional by equivalence
                 integer                :: ee(2,3)
                 integer                :: e(size(ee))=xx
                 equivalence               (e(1),ee(1,1))
                 ! CANNOT DO
                 !integer, dimension(2,3)::gg = [10,20,30] , [40,50,60]
                 !integer, dimension(2,3)::gg = [[10,20,30] , [40,50,60]]
                 ! BUT CAN DO
                 data gg(1,:)/ 10, 20, 30 /     ! fill rows with data statements
                 data gg(2,:)/ 40, 50, 60 /

                 data hh(:,1)/ 10, 40 /         ! fill columns with data statements
                 data hh(:,2)/ 20, 50 /
                 data hh(:,3)/ 30, 60 /
                    call disp_set(style='left & number')

                    write(*,*)'SIZE(aa)=',size(aa)
                    write(*,*)'SHAPE(aa)=',shape(aa)
                    write(*,*)'xx=',xx
                    write(*,*)'yy=',yy

                    call disp('aa=',aa)
                    call disp('bb=',bb)
                    call disp('cc=',cc)
                    call disp('dd=',dd)

                    call disp('ee=',ee)

                    call disp('ff=',ff)
                    call disp('gg=',gg)
                    call disp('hh=',hh)

                    write(*,*)repeat('=',80)
                    write(*,*)hh
                    write(*,*)repeat('=',80)
                    call print_buildfmt(hh)
                    write(*,*)repeat('=',80)
                    call print_fixedfmt(hh)

                 contains

                 subroutine print_buildfmt(arr)
                 use M_strings, only : v2s
                 implicit none
                 integer,intent(in) :: arr(:,:)
                 integer :: i
                 character(len=:),allocatable :: fmt
                    fmt='("> [",'//v2s(size(arr,dim=2))//'(i0:,","),"]")'
                    write(*,*)'FMT=',fmt
                    write(*,fmt)(arr(i,:),i=1,size(arr,dim=1))
                 end subroutine print_buildfmt

                 subroutine print_fixedfmt(arr)
                 implicit none
                 integer,intent(in) :: arr(:,:)
                 integer :: i
                    do i=1,size(arr,dim=1)
                       write(*, '("> [",*(i0:,","))' ,advance='no')arr(i,:)
                       write(*,'("]")')
                    enddo
                 end subroutine print_fixedfmt

                 end program demo_array_constructors ! initializing small arrays
