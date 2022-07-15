program runtest
use M_msg
use M_verify
use M_verify, only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done
use M_verify, only : unit_check_level, unit_check_stop
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
   call test_suite_M_list()
   call unit_check_stop()
end program runtest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_M_list  
use M_sort, only : sort_shell
use M_verify, only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done, unit_check_level
use M_list, only : locate, insert, remove, replace, dictionary
character(len=*),parameter   :: share=' -library libGPF -filename `pwd`/M_list.FF -documentation y -ufpp y -ccall n -archive GPF.a'
integer                      :: place
   ! list
   call test_locate()        ! finds the index where a string is found or should be in a sorted array 
   call test_insert()        ! insert entry into a string array at specified position 
   call test_remove()        ! remove entry from an allocatable array at specified position 
   call test_replace()       ! replace entry in a string array at specified position 
   ! dictionary
   call test_dict_set()      ! add or replace a key-value pair in a dictionary 
   call test_dict_delete()   ! delete entry by key name from a basic dictionary
   call test_dict_get()      ! value of key-value pair in a dictionary given key 
   call test_dict_ifdef()    ! return whether name is present in dictionary or not 
   call test_dict_clr()      ! clear basic dictionary 

   !
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dict_set
type(dictionary) :: dict
   call unit_check_start('dict%set',   &
   & ' -description "add string into allocatable string array by name" '//share)
   call  dict%set('A','value for a')

   call unit_check('dict%set',all(dict%key.eq.[character(len=20) :: 'A']),msg='array should be A')

   call dict%set('b','value for b')
   call dict%set('c','value for c')
   call dict%set('z','value for z')
   call unit_check('dict%set',all(dict%key.eq.[character(len=20) :: 'z','c','b','A']),'array should be z c b A')

   call dict%set('ZZ','value for ZZ')
   call dict%set('NOT','not this one')
   call dict%set('ZZZ','value for ZZZ')
   call dict%set('Z','value for Z')
   call unit_check('dict%set',all(dict%key.eq.[character(len=20) :: 'z','c','b','ZZZ','ZZ','Z','NOT','A']),'strings ok')

   call unit_check_done('dict%set') 
end subroutine test_dict_set
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dict_delete
type(dictionary) :: dict
   call unit_check_start('dict%del',&
   & ' -description "delete string by name from allocatable string array" '//share)

    call dict%set('A','some A')
   call dict%set('a','some a')
   call dict%set('b','some b')
    call dict%set('ZZ','some ZZ')
    call dict%set('ZZZ','some ZZZ')
    call dict%set('ZZ','some ZZ')
    call dict%set('Z','some Z')
    call dict%set('z','some z')
   call dict%set('c','some c')
   !write(*,'("built ",a)')dict%key

   call dict%del('A')
   call dict%del('Z')
   call dict%del('X')
   call dict%del('ZZZ')
   call dict%del('ZZ')
   call dict%del('z')
   !write(*,'("remaining ",a)')dict%key
   call unit_check('dict%del',all(dict%key.eq.[character(len=20) :: 'c','b','a']),'string deletes keys')
   !write(*,'("remaining key ",a)')dict%key
   !write(*,'("remaining val ",a)')dict%value
   call unit_check('dict%del',all(dict%value.eq.[character(len=20) :: 'some c','some b','some a']),'string deletes values')

   call unit_check_done('dict%del')
end subroutine test_dict_delete
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dict_ifdef
type(dictionary)     :: tt
logical,allocatable  :: answers(:)
   call unit_check_start('dict%ifdef',&
   & ' -description "return whether name is present in dictionary or not" '//share)
   call tt%set('A','value for A')
   call tt%set('B','value for B')
   call tt%set('C','value for C')
   call tt%set('D','value for D')
   call tt%set('E','value for E')
   call tt%set('F','value for F')
   call tt%set('G','value for G')
   call tt%del('F')
   call tt%del('D')

   answers=[tt%ifdef('A'), tt%ifdef('B'), tt%ifdef('C'), tt%ifdef('D'), tt%ifdef('E'), tt%ifdef('F'), tt%ifdef('G'), tt%ifdef('H')]

   call unit_check('dict%',all(answers.eqv. [.true., .true., .true., .false., .true., .false., .true., .false.]),'ifdef tests')
   call unit_check_done('dict%ifdef')

end subroutine test_dict_ifdef
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dict_clr
type(dictionary)     :: tt
logical,allocatable  :: answers(:)
   call unit_check_start('dict%ifdef',&
   & ' -description "return whether name is present in dictionary or not" '//share)
   call tt%set('A','value for A')
   call tt%set('B','value for B')
   call tt%set('C','value for C')
   call tt%set('D','value for D')
   call tt%set('E','value for E')
   call tt%set('F','value for F')
   call tt%set('G','value for G')
   call tt%del('F')
   call tt%del('D')
   call tt%clr()

   answers=[tt%ifdef('A'), tt%ifdef('B'), tt%ifdef('C'), tt%ifdef('D'), tt%ifdef('E'), tt%ifdef('F'), tt%ifdef('G'), tt%ifdef('H')]

   call unit_check('dict%',all(answers.eqv. [.false., .false., .false., .false., .false., .false., .false., .false.]),'clr tests')
   call unit_check_done('dict%clr')

end subroutine test_dict_clr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dict_get
type(dictionary)             :: dict
character(len=20),allocatable :: val(:)
   call unit_check_start('dict%get',&
   & ' -description "locate and get value from key-value pair by key name from dictionary" '//share)
   call dict%set('A','some A')
   call dict%set('Z','some Z')
   call dict%set('X','some X')
   val=[dict%get('Z'), dict%get('A'), dict%get('X')]
   !write(*,'("remaining ",a)')dict%key
   call unit_check('dict%get',all(dict%key.eq.[character(len=20) :: 'Z','X','A']),'string get keys')
   call unit_check('dict%get',all(dict%value.eq.[character(len=20) :: 'some Z','some X','some A']),'string get values ')

   call unit_check_done('dict%get')
end subroutine test_dict_get
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_locate
character(len=:),allocatable :: lst(:)
   lst=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
   ! make sure sorted in descending order
   call sort_shell(lst,order='d')
   call unit_check_start('locate',&
   & ' -description "locate string in allocatable string array sorted in descending order" '//share)
   call locate(lst,'ZZZ',place)
   call unit_check('locate',place.eq.4,'ZZZ',place,'should be ',4)
   call locate(lst,'zqj',place)
   call unit_check('locate',place.eq.-1,'zqj',place,'should be ',-1)
   call unit_check_done('locate')
end subroutine test_locate
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_insert
implicit none
character(len=:),allocatable :: arr(:)
integer                      :: place
character(len=:),allocatable :: newkey
integer                      :: i1,i2
   call unit_check_start('insert',&
   & ' -description "insert value into allocatable array by index"  '//share)
   ! make sure sorted in descending order
   arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
   call sort_shell(arr,order='d')
   newkey='NEW'
   i1=size(arr)
   call locate(arr,newkey,place)         ! find where string is or should be
   call unit_check('insert',place.lt.0,'should not be located',place)
   if(place.lt.1)then                    ! if string was not found insert it
      call insert(arr,newkey,abs(place)) ! not found so insert
      call locate(arr,newkey,place)      ! find where string is or should be
      if(place.gt.0)then
         call unit_check('insert',arr(place).eq.'NEW',arr(place),'should be "NEW"')
      else
         call unit_check('insert',.false.,arr(place),'should be positive for "NEW"')
      endif
   else
      call unit_check('insert',place.le.0,'found but should not have been',place)
   endif
   i2=size(arr)
   call unit_check('insert',i1+1.eq.i2,'array now bigger',i1,'to',i2)
   call unit_check_done('insert')
end subroutine test_insert
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_remove
use M_sort, only : sort_shell
character(len=:),allocatable  :: arr(:)
integer                       :: place
integer                       :: isize
   call unit_check_start('remove',' -description "remove value from allocatable array by index"  '//share)
   arr=[character(len=20) :: '', 'ZZZ', 'Z', 'aaa', 'b', 'x', 'ab', 'bb', 'xxx' ]
   call sort_shell(arr,order='d')     ! make sure sorted in descending order

   isize=size(arr)

   call locate(arr,'ab',place)        ! find where string is or should be
   call unit_check('remove',place.gt.0,'found the element to remove',place)
   call remove(arr,place)
   call locate(arr,'ab',place)        ! find where string is or should be
   call unit_check('remove',place.lt.0,'did not find the element to remove',place)

   call locate(arr,'bb',place)        ! find where string is or should be
   call remove(arr,place)
   call unit_check('remove',isize-2.eq.size(arr),'shrunk by two')

   call unit_check_done('remove')
end subroutine test_remove
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_replace
use M_sort, only : sort_shell
character(len=:),allocatable  :: arr(:)
integer                       :: place1 
integer                       :: isize
   call unit_check_start('replace',&
   & ' -description "replace value from allocatable array by index"  '//share)
   arr=[character(len=20) :: '', 'ZZZ', 'Z', 'aaa', 'b', 'x', 'ab', 'bb', 'xxx' ]
   call sort_shell(arr,order='d')     ! make sure sorted in descending order
   isize=size(arr)
   call locate(arr,'ab',place1) ! find where string is or should be
   call unit_check('replace',place1.gt.0,'location=',place1)
   call replace(arr,'new value for ab',place1)
   call unit_check('replace',size(arr).eq.isize,'no change in size',size(arr))
   if(place1.gt.0.and.place1.le.isize)then
      call unit_check('replace',arr(place1).eq.'new value for ab',arr(place1))
   else
      call unit_check('replace',.false.,'bad location',place1)
   endif
   call unit_check_done('replace')
end subroutine test_replace
!===================================================================================================================================
end subroutine test_suite_M_list  
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
