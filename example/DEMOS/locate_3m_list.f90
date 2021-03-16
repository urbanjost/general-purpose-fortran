           program demo_locate
           use M_sort, only : sort_shell
           use M_list, only : locate
           implicit none
           character(len=:),allocatable  :: arr(:)
           integer                       :: i

           arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
           ! make sure sorted in descending order
           call sort_shell(arr,order='d')

           call update(arr,'b')
           call update(arr,'[')
           call update(arr,'c')
           call update(arr,'ZZ')
           call update(arr,'ZZZZ')
           call update(arr,'z')

           contains
           subroutine update(arr,string)
           character(len=:),allocatable :: arr(:)
           character(len=*)             :: string
           integer                      :: place, plus, ii, end
           ! find where string is or should be
           call locate(arr,string,place)
           write(*,*)'for "'//string//'" index is ',place, size(arr)
           ! if string was not found insert it
           if(place.lt.1)then
              plus=abs(place)
              ii=len(arr)
              end=size(arr)
              ! empty array
              if(end.eq.0)then
                 arr=[character(len=ii) :: string ]
              ! put in front of array
              elseif(plus.eq.1)then
                 arr=[character(len=ii) :: string, arr]
              ! put at end of array
              elseif(plus.eq.end)then
                 arr=[character(len=ii) :: arr, string ]
              ! put in middle of array
              else
                 arr=[character(len=ii) :: arr(:plus-1), string,arr(plus:) ]
              endif
              ! show array
              write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
           endif
           end subroutine update
           end program demo_locate
