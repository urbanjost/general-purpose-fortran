           program demo_remove
           use M_sort, only : sort_shell
           use M_list, only : locate, remove
           implicit none
           character(len=:),allocatable :: arr(:)
           integer                       :: i
           integer                       :: end

           arr=[character(len=20) :: '', 'ZZZ', 'Z', 'aaa', 'b', 'b', 'ab', 'bb', 'xxx' ]
           ! make sure sorted in descending order
           call sort_shell(arr,order='d')

           end=size(arr)
           write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
           call remove(arr,1)
           end=size(arr)
           write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
           call remove(arr,4)
           end=size(arr)
           write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)

           end program demo_remove
