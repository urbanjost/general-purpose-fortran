          program demo_M_list
          use M_list, only : insert, locate, replace, remove
          ! create a dictionary with character keywords, values, and value lengths
          ! using the routines for maintaining a list

           use M_list, only : locate, insert, replace
           implicit none
           character(len=:),allocatable   :: keywords(:)
           character(len=:),allocatable   :: values(:)
           integer,allocatable            :: counts(:)
           integer                        :: i
           ! insert and replace entries
           call update('b','value of b')
           call update('a','value of a')
           call update('c','value of c')
           call update('c','value of c again')
           call update('d','value of d')
           call update('a','value of a again')
           ! show array
           write(*,'(*(a,"==>","[",a,"]",/))')(trim(keywords(i)),values(i)(:counts(i)),i=1,size(keywords))
           ! remove some entries
           call update('a')
           call update('c')
           write(*,'(*(a,"==>","[",a,"]",/))')(trim(keywords(i)),values(i)(:counts(i)),i=1,size(keywords))
           ! get some values
           write(*,*)'get b=>',get('b')
           write(*,*)'get d=>',get('d')
           write(*,*)'get notthere=>',get('notthere')

           contains
           subroutine update(key,valin)
           character(len=*),intent(in)           :: key
           character(len=*),intent(in),optional  :: valin
           integer                               :: place
           integer                               :: ilen
           character(len=:),allocatable          :: val
           if(present(valin))then
              val=valin
              ilen=len_trim(val)
              ! find where string is or should be
              call locate(keywords,key,place)
              ! if string was not found insert it
              if(place.lt.1)then
                 call insert(keywords,key,iabs(place))
                 call insert(values,val,iabs(place))
                 call insert(counts,ilen,iabs(place))
              else
                 call replace(values,val,place)
                 call replace(counts,ilen,place)
              endif
           else
              call locate(keywords,key,place)
              if(place.gt.0)then
                 call remove(keywords,place)
                 call remove(values,place)
                 call remove(counts,place)
              endif
           endif
           end subroutine update
           function get(key) result(valout)
           character(len=*),intent(in)   :: key
           character(len=:),allocatable  :: valout
           integer                       :: place
              ! find where string is or should be
              call locate(keywords,key,place)
              if(place.lt.1)then
                 valout=''
              else
                 valout=values(place)(:counts(place))
              endif
           end function get
       end program demo_M_list
