           program demo_replace
           use M_list, only  : insert, locate, replace
           ! Find if a key is in a list and insert it
           ! into the key list and value list if it is not present
           ! or replace the associated value if the key existed
           implicit none
           character(len=20)            :: key
           character(len=100)           :: val
           character(len=:),allocatable :: keywords(:)
           character(len=:),allocatable :: values(:)
           integer                      :: i
           integer                      :: place
           call update('b','value of b')
           call update('a','value of a')
           call update('c','value of c')
           call update('c','value of c again')
           call update('d','value of d')
           call update('a','value of a again')
           ! show array
           write(*,'(*(a,"==>",a,/))')(trim(keywords(i)),trim(values(i)),i=1,size(keywords))

           call locate(keywords,'a',place)
           if(place.gt.0)then
              write(*,*)'The value of "a" is',trim(values(place))
           else
              write(*,*)'"a" not found'
           endif

           contains
           subroutine update(key,val)
           character(len=*),intent(in)  :: key
           character(len=*),intent(in)  :: val
           integer                      :: place

           ! find where string is or should be
           call locate(keywords,key,place)
           ! if string was not found insert it
           if(place.lt.1)then
              call insert(keywords,key,abs(place))
              call insert(values,val,abs(place))
           else ! replace
              call replace(values,val,place)
           endif

           end subroutine update
       end program demo_replace
