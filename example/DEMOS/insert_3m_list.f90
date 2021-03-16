           program demo_insert
           use M_sort, only : sort_shell
           use M_list, only : locate, insert
           implicit none
           character(len=:),allocatable :: arr(:)
           integer                       :: i

           arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
           ! make sure sorted in descending order
           call sort_shell(arr,order='d')
           ! add or replace values
           call update(arr,'b')
           call update(arr,'[')
           call update(arr,'c')
           call update(arr,'ZZ')
           call update(arr,'ZZZ')
           call update(arr,'ZZZZ')
           call update(arr,'')
           call update(arr,'z')

           contains
           subroutine update(arr,string)
           character(len=:),allocatable :: arr(:)
           character(len=*)             :: string
           integer                      :: place, end

           end=size(arr)
           ! find where string is or should be
           call locate(arr,string,place)
           ! if string was not found insert it
           if(place.lt.1)then
              call insert(arr,string,abs(place))
           endif
           ! show array
           end=size(arr)
           write(*,'("array is now SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)

           end subroutine update
           end program demo_insert
