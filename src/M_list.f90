










!>
!!##NAME
!!    M_list(3f) - [M_list::INTRO] maintain simple lists
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    use M_list, only : insert, replace, remove, locate
!!    use M_list, only : dictionary
!!
!!##DESCRIPTION
!!
!!    The M_list(3fm) module allows for maintaining an allocatable array of
!!    intrinsic type (REAL, INTEGER, CHARACTER) as a sorted list. An example
!!    is given that creates a keyword-value dictionary using the lists.
!!
!!    The lists are maintained as simple allocatable arrays. Each time an
!!    entry is added or deleted the array is re-allocated. Because of the
!!    expense of reallocating the data these routines are best suited for
!!    maintaining small lists that do not change size frequently.
!!
!!    The advantage of this simplistic approach is that the dictionary
!!    components are simple arrays of intrinsic types which can be easily
!!    accessed with standard routines. It is easy to understand, as it
!!    works with simple arrays. For more demanding applications this would
!!    be implemented as a linked list, which there are a number of freely
!!    available examples of; several are listed on the Fortran Wiki.
!!
!!    BASIC LIST
!!
!!    subroutine locate(list,value,place,ier,errmsg)  finds the index where a
!!                                                    value is found or should
!!                                                    be in a sorted array and
!!                                                    flag if the value exists
!!                                                    already
!!    subroutine insert(list,value,place)     insert entry into an allocatable
!!                                            array at specified position
!!    subroutine replace(list,value,place)    replace entry in an allocatable
!!                                            array at specified position
!!    subroutine remove(list,place)           remove entry from an allocatable
!!                                            array at specified position
!!
!!##EXAMPLES
!!
!!   Sample program
!!
!!    program demo_M_list
!!    use M_list, only : insert, locate, replace, remove
!!    ! create a dictionary with character keywords, values, and value lengths
!!    ! using the routines for maintaining a list
!!
!!     use M_list, only : locate, insert, replace
!!     implicit none
!!     character(len=:),allocatable   :: keywords(:)
!!     character(len=:),allocatable   :: values(:)
!!     integer,allocatable            :: counts(:)
!!     integer                        :: i
!!     ! insert and replace entries
!!     call update('b','value of b')
!!     call update('a','value of a')
!!     call update('c','value of c')
!!     call update('c','value of c again')
!!     call update('d','value of d')
!!     call update('a','value of a again')
!!     ! show array
!!     write(*,'(*(a,"==>","[",a,"]",/))')&
!!      & (trim(keywords(i)),values(i)(:counts(i)),i=1,size(keywords))
!!     ! remove some entries
!!     call update('a')
!!     call update('c')
!!     write(*,'(*(a,"==>","[",a,"]",/))')&
!!      & (trim(keywords(i)),values(i)(:counts(i)),i=1,size(keywords))
!!     ! get some values
!!     write(*,*)'get b=>',get('b')
!!     write(*,*)'get d=>',get('d')
!!     write(*,*)'get notthere=>',get('notthere')
!!     !
!!     contains
!!     subroutine update(key,valin)
!!     character(len=*),intent(in)           :: key
!!     character(len=*),intent(in),optional  :: valin
!!     integer                               :: place
!!     integer                               :: ilen
!!     character(len=:),allocatable          :: val
!!     if(present(valin))then
!!        val=valin
!!        ilen=len_trim(val)
!!        ! find where string is or should be
!!        call locate(keywords,key,place)
!!        ! if string was not found insert it
!!        if(place.lt.1)then
!!           call insert(keywords,key,iabs(place))
!!           call insert(values,val,iabs(place))
!!           call insert(counts,ilen,iabs(place))
!!        else
!!           call replace(values,val,place)
!!           call replace(counts,ilen,place)
!!        endif
!!     else
!!        call locate(keywords,key,place)
!!        if(place.gt.0)then
!!           call remove(keywords,place)
!!           call remove(values,place)
!!           call remove(counts,place)
!!        endif
!!     endif
!!     end subroutine update
!!     function get(key) result(valout)
!!     character(len=*),intent(in)   :: key
!!     character(len=:),allocatable  :: valout
!!     integer                       :: place
!!        ! find where string is or should be
!!        call locate(keywords,key,place)
!!        if(place.lt.1)then
!!           valout=''
!!        else
!!           valout=values(place)(:counts(place))
!!        endif
!!     end function get
!!     end program demo_M_list
!!
!!   Results
!!
!!       >  d==>[value of d]
!!       >  c==>[value of c again]
!!       >  b==>[value of b]
!!       >  a==>[value of a again]
!!       >
!!       > d==>[value of d]
!!       > b==>[value of b]
!!       >
!!       >  get b=>value of b
!!       >  get d=>value of d
!!       >  get notthere=>
!!
!!
!!    BASIC DICTIONARY
!!
!!    A basic dictionary that uses the basic M_list functions.
!!
!!    Consider using generic linked-list based dictionaries when heavy
!!    usage is required, now that that is available in more recent versions
!!    of Fortran.
!!
!!    Note: this does not work with gfortran(1) up to at least 7.4.0 but
!!    works from at least 10.3.0 and onward.
!!
!!    Dictionary type definition:
!!
!!       type dictionary
!!          character(len=:),allocatable :: key(:)
!!          character(len=:),allocatable :: value(:)
!!          integer,allocatable          :: count(:)
!!          contains
!!             procedure,public :: get => dict_get
!!             procedure,public :: set => dict_add
!!             procedure,public :: del => dict_delete
!!             procedure,public :: clr => dict_clear
!!       end type dictionary
!!
!!       %get      get value from type(dictionary) given an existing key
!!       %set      set or replace value for type(dictionary) given a key
!!       %del      delete an existing key from type(dictionary)
!!       %clr      empty a type(dictionary)
!!       %ifdef    test if name is defined
!!
!!##EXAMPLES
!!
!!   Sample program
!!
!!       program test_dictionary
!!       use M_list, only : dictionary
!!       implicit none
!!       type(dictionary)             :: table
!!         !
!!         ! create a character string dictionary
!!         !
!!         call table%set('A','aye')
!!         call table%set('B','bee')
!!         call table%set('C','see')
!!         call table%set('D','dee')
!!         !
!!         write(*,*)'A=',table%get('A')
!!         write(*,*)'C=',table%get('C')
!!         write(*,*)'notthere=',table%get('notthere')
!!         !
!!         call print_dict()
!!         !
!!         ! delete dictionary entries
!!         !
!!         call  table%del('A')
!!         call  table%del('C')
!!         call  table%del('z') ! a noop as there is no key of 'z'
!!         !
!!         call print_dict()
!!         !
!!         ! clear dictionary
!!         !
!!         call  table%clr()
!!         !
!!         call print_dict()
!!       !
!!       contains
!!       !
!!       subroutine print_dict()
!!       integer :: i
!!          ! the dictionary is just three arrays
!!          write(*,'("DICTIONARY:")')
!!          write(*,'(*(a,"==>","[",a,"]",/))') &
!!          & (trim(table%key(i)),               &
!!          & table%value(i)(:table%count(i)),    &
!!          & i=1,size(table%key))
!!          !
!!       end subroutine print_dict
!!       !
!!       end program test_dictionary
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_list
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,stdout=>OUTPUT_UNIT    ! access computing environment
implicit none
private

public locate        ! [M_list] find PLACE in sorted character array where value can be found or should be placed
   private locate_c
   private locate_d
   private locate_r
   private locate_i
public insert        ! [M_list] insert entry into a sorted allocatable array at specified position
   private insert_c
   private insert_d
   private insert_r
   private insert_i
   private insert_l
public replace       ! [M_list] replace entry by index from a sorted allocatable array if it is present
   private replace_c
   private replace_d
   private replace_r
   private replace_i
   private replace_l
public remove        ! [M_list] delete entry by index from a sorted allocatable array if it is present
   private remove_c
   private remove_d
   private remove_r
   private remove_i
   private remove_l

! ident_1="@(#)M_list::locate(3f): Generic subroutine locates where element is or should be in sorted allocatable array"
interface locate
   module procedure locate_c, locate_d, locate_r, locate_i
end interface

! ident_2="@(#)M_list::insert(3f): Generic subroutine inserts element into allocatable array at specified position"
interface insert
   module procedure insert_c, insert_d, insert_r, insert_i, insert_l
end interface

! ident_3="@(#)M_list::replace(3f): Generic subroutine replaces element from allocatable array at specified position"
interface replace
   module procedure replace_c, replace_d, replace_r, replace_i, replace_l
end interface

! ident_4="@(#)M_list::remove(3f): Generic subroutine deletes element from allocatable array at specified position"
interface remove
   module procedure remove_c, remove_d, remove_r, remove_i, remove_l
end interface

!-----------------------------------------------------------------------------------------------------------------------------------
public dictionary

type dictionary
   character(len=:),allocatable :: key(:)
   character(len=:),allocatable :: value(:)
   integer,allocatable          :: count(:)
   contains
      procedure,public :: get   => dict_get    ! get value associated with a key in a dictionary or return blank
      procedure,public :: set   => dict_add    ! insert or replace entry by name into a dictionary
      procedure,public :: del   => dict_delete ! delete entry by name from a dictionary if entry is present
      procedure,public :: clr   => dict_clear  ! clear dictionary
      procedure,public :: ifdef => dict_ifdef  ! return if defined or not
end type dictionary

logical,save :: debug=.false.
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    locate(3f) - [M_list] finds the index where a string is found or should be in a sorted array
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine locate(list,value,place,ier,errmsg)
!!
!!    character(len=:)|doubleprecision|real|integer,allocatable :: list(:)
!!    character(len=*)|doubleprecision|real|integer,intent(in)  :: value
!!    integer, intent(out)                  :: PLACE
!!
!!    integer, intent(out),optional         :: IER
!!    character(len=*),intent(out),optional :: ERRMSG
!!
!!##DESCRIPTION
!!
!!    LOCATE(3f) finds the index where the VALUE is found or should
!!    be found in an array. The array must be sorted in descending
!!    order (highest at top). If VALUE is not found it returns the index
!!    where the name should be placed at with a negative sign.
!!
!!    The array and list must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL,INTEGER)
!!
!!##OPTIONS
!!
!!    VALUE         the value to locate in the list.
!!    LIST          is the list array.
!!
!!##RETURNS
!!    PLACE         is the subscript that the entry was found at if it is
!!                  greater than zero(0).
!!
!!                  If PLACE is negative, the absolute value of
!!                  PLACE indicates the subscript value where the
!!                  new entry should be placed in order to keep the
!!                  list alphabetized.
!!
!!    IER           is zero(0) if no error occurs.
!!                  If an error occurs and IER is not
!!                  present, the program is stopped.
!!
!!    ERRMSG        description of any error
!!
!!##EXAMPLES
!!
!!   Find if a string is in a sorted array, and insert the string into
!!   the list if it is not present ...
!!
!!     program demo_locate
!!     use M_sort, only : sort_shell
!!     use M_list, only : locate
!!     implicit none
!!     character(len=:),allocatable  :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     call update(arr,'b')
!!     call update(arr,'[')
!!     call update(arr,'c')
!!     call update(arr,'ZZ')
!!     call update(arr,'ZZZZ')
!!     call update(arr,'z')
!!
!!     contains
!!     subroutine update(arr,string)
!!     character(len=:),allocatable :: arr(:)
!!     character(len=*)             :: string
!!     integer                      :: place, plus, ii, end
!!     ! find where string is or should be
!!     call locate(arr,string,place)
!!     write(*,*)'for "'//string//'" index is ',place, size(arr)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        plus=abs(place)
!!        ii=len(arr)
!!        end=size(arr)
!!        ! empty array
!!        if(end.eq.0)then
!!           arr=[character(len=ii) :: string ]
!!        ! put in front of array
!!        elseif(plus.eq.1)then
!!           arr=[character(len=ii) :: string, arr]
!!        ! put at end of array
!!        elseif(plus.eq.end)then
!!           arr=[character(len=ii) :: arr, string ]
!!        ! put in middle of array
!!        else
!!           arr=[character(len=ii) :: arr(:plus-1), string,arr(plus:) ]
!!        endif
!!        ! show array
!!        write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     endif
!!     end subroutine update
!!     end program demo_locate
!!
!!   Results
!!
!!       >  for "b" index is            2           5
!!       >  for "[" index is           -4           5
!!       > SIZE=5 xxx,b,aaa,[,ZZZ,
!!       >  for "c" index is           -2           6
!!       > SIZE=6 xxx,c,b,aaa,[,ZZZ,
!!       >  for "ZZ" index is           -7           7
!!       > SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!       >  for "ZZZZ" index is           -6           8
!!       > SIZE=8 xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!       >  for "z" index is           -1           9
!!       > SIZE=9 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine locate_c(list,value,place,ier,errmsg)

! ident_5="@(#)M_list::locate_c(3f): find PLACE in sorted character array where VALUE can be found or should be placed"

character(len=*),intent(in)             :: value
integer,intent(out)                     :: place
character(len=:),allocatable            :: list(:)
integer,intent(out),optional            :: ier
character(len=*),intent(out),optional   :: errmsg
integer                                 :: i
character(len=:),allocatable            :: message
integer                                 :: arraysize
integer                                 :: maxtry
integer                                 :: imin, imax
integer                                 :: error
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   arraysize=size(list)
   if(debug)write(stderr,*)'*locate_c* START ARRAYSIZE=',size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',trim(value)//' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
   if(debug)write(stderr,*)'*locate_c* END PLACE=',place,' ARRAYSIZE=',size(list),' LENGTH=',len(list)
end subroutine locate_c
subroutine locate_d(list,value,place,ier,errmsg)

! ident_6="@(#)M_list::locate_d(3f): find PLACE in sorted doubleprecision array where VALUE can be found or should be placed"

! Assuming an array sorted in descending order
!
!  1. If it is not found report where it should be placed as a NEGATIVE index number.

doubleprecision,allocatable            :: list(:)
doubleprecision,intent(in)             :: value
integer,intent(out)                    :: place
integer,intent(out),optional           :: ier
character(len=*),intent(out),optional  :: errmsg

integer                                :: i
character(len=:),allocatable           :: message
integer                                :: arraysize
integer                                :: maxtry
integer                                :: imin, imax
integer                                :: error

   message=''
   if(.not.allocated(list))then
      list=[doubleprecision :: ]
   endif
   arraysize=size(list)
   if(debug)write(stderr,*)'*locate_d* START ARRAYSIZE=',size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',value,' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
   if(debug)write(stderr,*)'*locate_d* END PLACE=',place,' ARRAYSIZE=',size(list)
end subroutine locate_d
subroutine locate_r(list,value,place,ier,errmsg)

! ident_7="@(#)M_list::locate_r(3f): find PLACE in sorted real array where VALUE can be found or should be placed"

! Assuming an array sorted in descending order
!
!  1. If it is not found report where it should be placed as a NEGATIVE index number.

real,allocatable                       :: list(:)
real,intent(in)                        :: value
integer,intent(out)                    :: place
integer,intent(out),optional           :: ier
character(len=*),intent(out),optional  :: errmsg

integer                                :: i
character(len=:),allocatable           :: message
integer                                :: arraysize
integer                                :: maxtry
integer                                :: imin, imax
integer                                :: error

   if(.not.allocated(list))then
      list=[real :: ]
   endif
   arraysize=size(list)
   if(debug)write(stderr,*)'*locate_r* START ARRAYSIZE=',size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',value,' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
   if(debug)write(stderr,*)'*locate_r* END PLACE=',place,' ARRAYSIZE=',size(list)
end subroutine locate_r
subroutine locate_i(list,value,place,ier,errmsg)

! ident_8="@(#)M_list::locate_i(3f): find PLACE in sorted integer array where VALUE can be found or should be placed"

! Assuming an array sorted in descending order
!
!  1. If it is not found report where it should be placed as a NEGATIVE index number.

integer,allocatable                    :: list(:)
integer,intent(in)                     :: value
integer,intent(out)                    :: place
integer,intent(out),optional           :: ier
character(len=*),intent(out),optional  :: errmsg

integer                                :: i
character(len=:),allocatable           :: message
integer                                :: arraysize
integer                                :: maxtry
integer                                :: imin, imax
integer                                :: error

   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   arraysize=size(list)
   if(debug)write(stderr,*)'*locate_i* START ARRAYSIZE=',size(list)

   error=0
   if(arraysize.eq.0)then
      maxtry=0
      place=-1
   else
      maxtry=int(log(float(arraysize))/log(2.0)+1.0)
      place=(arraysize+1)/2
   endif
   imin=1
   imax=arraysize
   message=''

   LOOP: block
   do i=1,maxtry

      if(value.eq.list(PLACE))then
         exit LOOP
      else if(value.gt.list(place))then
         imax=place-1
      else
         imin=place+1
      endif

      if(imin.gt.imax)then
         place=-imin
         if(iabs(place).gt.arraysize)then ! ran off end of list. Where new value should go or an unsorted input array'
            exit LOOP
         endif
         exit LOOP
      endif

      place=(imax+imin)/2

      if(place.gt.arraysize.or.place.le.0)then
         message='*locate* error: search is out of bounds of list. Probably an unsorted input array'
         error=-1
         exit LOOP
      endif

   enddo
   message='*locate* exceeded allowed tries. Probably an unsorted input array'
   endblock LOOP
   if(present(ier))then
      ier=error
   else if(error.ne.0)then
      write(stderr,*)message//' VALUE=',value,' PLACE=',place
      stop 1
   endif
   if(present(errmsg))then
      errmsg=message
   endif
   if(debug)write(stderr,*)'*locate_i* END PLACE=',place,' ARRAYSIZE=',size(list)
end subroutine locate_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    remove(3f) - [M_list] remove entry from an allocatable array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine remove(list,place)
!!
!!    character(len=:)|doubleprecision|real|integer,intent(inout) :: list(:)
!!    integer, intent(out) :: PLACE
!!
!!##DESCRIPTION
!!
!!    Remove a value from an allocatable array at the specified index.
!!    The array is assumed to be sorted in descending order. It may be of
!!    type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER.
!!
!!##OPTIONS
!!
!!    list    is the list array.
!!    PLACE   is the subscript for the entry that should be removed
!!
!!##EXAMPLES
!!
!!   Sample program
!!
!!     program demo_remove
!!     use M_sort, only : sort_shell
!!     use M_list, only : locate, remove
!!     implicit none
!!     character(len=:),allocatable :: arr(:)
!!     integer                       :: i
!!     integer                       :: end
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'Z', 'aaa', 'b', 'b', 'ab', 'bb', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove(arr,1)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!     call remove(arr,4)
!!     end=size(arr)
!!     write(*,'("SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end program demo_remove
!!
!!   Results
!!
!!       > SIZE=9 xxx,bb,b,b,ab,aaa,ZZZ,Z,,
!!       > SIZE=8 bb,b,b,ab,aaa,ZZZ,Z,,
!!       > SIZE=7 bb,b,b,aaa,ZZZ,Z,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine remove_c(list,place)

! ident_9="@(#)M_list::remove_c(3fp): remove string from allocatable string array at specified position"

character(len=:),allocatable :: list(:)
integer,intent(in)           :: place
integer                      :: ii, end
   if(debug) write(stderr,*)'*remove_c* START PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[character(len=2) :: ]
   endif
   ii=len(list)
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[character(len=ii) :: list(:place-1) ]
   else
      list=[character(len=ii) :: list(:place-1), list(place+1:) ]
   endif
   if(debug)write(stderr,*)'*remove_c* END PLACE=',place,' NEWSIZE=',size(list),' LENGTH=',len(list)
end subroutine remove_c
subroutine remove_d(list,place)

! ident_10="@(#)M_list::remove_d(3fp): remove doubleprecision value from allocatable array at specified position"

doubleprecision,allocatable  :: list(:)
integer,intent(in)           :: place
integer                      :: end
   if(debug) write(stderr,*)'*remove_d* START PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
           list=[doubleprecision :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif
   if(debug)write(stderr,*)'*remove_d* END PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_d
subroutine remove_r(list,place)

! ident_11="@(#)M_list::remove_r(3fp): remove value from allocatable array at specified position"

real,allocatable    :: list(:)
integer,intent(in)  :: place
integer             :: end
   if(debug) write(stderr,*)'*remove_r* START PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[real :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif
   if(debug)write(stderr,*)'*remove_r* END PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_r
subroutine remove_l(list,place)

! ident_12="@(#)M_list::remove_l(3fp): remove value from allocatable array at specified position"

logical,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(debug) write(stderr,*)'*remove_l* START PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif
   if(debug)write(stderr,*)'*remove_l* END PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_l
subroutine remove_i(list,place)

! ident_13="@(#)M_list::remove_i(3fp): remove value from allocatable array at specified position"
integer,allocatable    :: list(:)
integer,intent(in)     :: place
integer                :: end

   if(debug) write(stderr,*)'*remove_i* START PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(place.le.0.or.place.gt.end)then                       ! index out of bounds of array
   elseif(place.eq.end)then                                 ! remove from array
      list=[ list(:place-1)]
   else
      list=[ list(:place-1), list(place+1:) ]
   endif
   if(debug)write(stderr,*)'*remove_i* END PLACE=',place,' NEWSIZE=',size(list)

end subroutine remove_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    replace(3f) - [M_list] replace entry in a string array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine replace(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer, intent(out)          :: PLACE
!!
!!##DESCRIPTION
!!
!!    replace a value in an allocatable array at the specified index. Unless the
!!    array needs the string length to increase this is merely an assign of a value
!!    to an array element.
!!
!!    The array may be of type CHARACTER, DOUBLEPRECISION, REAL, or INTEGER>
!!    It is assumed to be sorted in descending order without duplicate values.
!!
!!    The value and list must be of the same type.
!!
!!##OPTIONS
!!
!!    VALUE         the value to place in the array
!!    LIST          is the array.
!!    PLACE         is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!   Replace key-value pairs in a dictionary
!!
!!     program demo_replace
!!     use M_list, only  : insert, locate, replace
!!     ! Find if a key is in a list and insert it
!!     ! into the key list and value list if it is not present
!!     ! or replace the associated value if the key existed
!!     implicit none
!!     character(len=20)            :: key
!!     character(len=100)           :: val
!!     character(len=:),allocatable :: keywords(:)
!!     character(len=:),allocatable :: values(:)
!!     integer                      :: i
!!     integer                      :: place
!!     call update('b','value of b')
!!     call update('a','value of a')
!!     call update('c','value of c')
!!     call update('c','value of c again')
!!     call update('d','value of d')
!!     call update('a','value of a again')
!!     ! show array
!!     write(*,'(*(a,"==>",a,/))')(trim(keywords(i)),trim(values(i)),i=1,size(keywords))
!!
!!     call locate(keywords,'a',place)
!!     if(place.gt.0)then
!!        write(*,*)'The value of "a" is ',trim(values(place))
!!     else
!!        write(*,*)'"a" not found'
!!     endif
!!
!!     contains
!!     subroutine update(key,val)
!!     character(len=*),intent(in)  :: key
!!     character(len=*),intent(in)  :: val
!!     integer                      :: place
!!
!!     ! find where string is or should be
!!     call locate(keywords,key,place)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        call insert(keywords,key,abs(place))
!!        call insert(values,val,abs(place))
!!     else ! replace
!!        call replace(values,val,place)
!!     endif
!!
!!     end subroutine update
!!    end program demo_replace
!!
!!   Results
!!
!!    > d==>value of d
!!    > c==>value of c again
!!    > b==>value of b
!!    > a==>value of a again
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine replace_c(list,value,place)

! ident_14="@(#)M_list::replace_c(3fp): replace string in allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: tlen
integer                      :: end
   if(debug) write(stderr,*)'*replace_c* START VALUE=',trim(value),' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif
   tlen=len_trim(value)
   end=size(list)
   if(place.lt.0.or.place.gt.end)then
           write(stderr,*)'*replace_c* error: index out of range. end=',end,' index=',place
   elseif(len_trim(value).le.len(list))then
      list(place)=value
   else  ! increase length of variable
      ii=max(tlen,len(list))
      kludge=[character(len=ii) :: list ]
      list=kludge
      list(place)=value
   endif
   if(debug)write(stderr,*)'*replace_c* END VALUE=',trim(value),' PLACE=',place,' NEWSIZE=',size(list),' LENGTH=',len(list)
end subroutine replace_c
subroutine replace_d(list,value,place)

! ident_15="@(#)M_list::replace_d(3fp): place doubleprecision value into allocatable array at specified position"

doubleprecision,intent(in)   :: value
doubleprecision,allocatable  :: list(:)
integer,intent(in)           :: place
integer                      :: end
   if(debug) write(stderr,*)'*replace_d* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
           list=[doubleprecision :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_d* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(stderr,*)'*replace_d* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_d
subroutine replace_r(list,value,place)

! ident_16="@(#)M_list::replace_r(3fp): place value into allocatable array at specified position"

real,intent(in)       :: value
real,allocatable      :: list(:)
integer,intent(in)    :: place
integer               :: end
   if(debug) write(stderr,*)'*replace_r* START REPLACE_R VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[real :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_r* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(stderr,*)'*replace_r* END REPLACE_R VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_r
subroutine replace_l(list,value,place)

! ident_17="@(#)M_list::replace_l(3fp): place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(debug)write(stderr,*)'*replace_l* START REPLACE_L VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_l* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(stderr,*)'*replace_l* END REPLACE_L VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_l
subroutine replace_i(list,value,place)

! ident_18="@(#)M_list::replace_i(3fp): place value into allocatable array at specified position"

integer,intent(in)    :: value
integer,allocatable   :: list(:)
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(debug)write(stderr,*)'*replace_i* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.gt.0.and.place.le.end)then
      list(place)=value
   else                                                      ! put in middle of array
      write(stderr,*)'*replace_i* error: index out of range. end=',end,' index=',place
   endif
   if(debug)write(stderr,*)'*replace_i* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine replace_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    insert(3f) - [M_list] insert entry into a string array at specified position
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine insert(list,value,place)
!!
!!    character(len=*)|doubleprecision|real|integer,intent(in) :: value
!!    character(len=:)|doubleprecision|real|integer,intent(in) :: list(:)
!!    integer,intent(in)    :: place
!!
!!##DESCRIPTION
!!
!!    Insert a value into an allocatable array at the specified index.
!!    The list and value must be of the same type (CHARACTER, DOUBLEPRECISION,
!!    REAL, or INTEGER)
!!
!!##OPTIONS
!!
!!    list    is the list array. Must be sorted in descending order.
!!    value   the value to place in the array
!!    PLACE   is the subscript that the entry should be placed at
!!
!!##EXAMPLES
!!
!!   Find if a string is in a sorted array, and insert the string into
!!   the list if it is not present ...
!!
!!     program demo_insert
!!     use M_sort, only : sort_shell
!!     use M_list, only : locate, insert
!!     implicit none
!!     character(len=:),allocatable :: arr(:)
!!     integer                       :: i
!!
!!     arr=[character(len=20) :: '', 'ZZZ', 'aaa', 'b', 'xxx' ]
!!     ! make sure sorted in descending order
!!     call sort_shell(arr,order='d')
!!     ! add or replace values
!!     call update(arr,'b')
!!     call update(arr,'[')
!!     call update(arr,'c')
!!     call update(arr,'ZZ')
!!     call update(arr,'ZZZ')
!!     call update(arr,'ZZZZ')
!!     call update(arr,'')
!!     call update(arr,'z')
!!
!!     contains
!!     subroutine update(arr,string)
!!     character(len=:),allocatable :: arr(:)
!!     character(len=*)             :: string
!!     integer                      :: place, end
!!
!!     end=size(arr)
!!     ! find where string is or should be
!!     call locate(arr,string,place)
!!     ! if string was not found insert it
!!     if(place.lt.1)then
!!        call insert(arr,string,abs(place))
!!     endif
!!     ! show array
!!     end=size(arr)
!!     write(*,'("array is now SIZE=",i0,1x,*(a,","))')end,(trim(arr(i)),i=1,end)
!!
!!     end subroutine update
!!     end program demo_insert
!!
!!   Results
!!
!!        > array is now SIZE=5 xxx,b,aaa,ZZZ,,
!!        > array is now SIZE=6 xxx,b,aaa,[,ZZZ,,
!!        > array is now SIZE=7 xxx,c,b,aaa,[,ZZZ,,
!!        > array is now SIZE=8 xxx,c,b,aaa,[,ZZZ,ZZ,,
!!        > array is now SIZE=9 xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!        > array is now SIZE=10 z,xxx,c,b,aaa,[,ZZZZ,ZZZ,ZZ,,
!!
!!##AUTHOR
!!    1989,2017 John S. Urban
!!##LICENSE
!!    Public Domain
subroutine insert_c(list,value,place)

! ident_19="@(#)M_list::insert_c(3fp): place string into allocatable string array at specified position"

character(len=*),intent(in)  :: value
character(len=:),allocatable :: list(:)
character(len=:),allocatable :: kludge(:)
integer,intent(in)           :: place
integer                      :: ii
integer                      :: end
   if(debug) write(stderr,*)'*insert_c* START VALUE=',trim(value),' PLACE=',place,' ORIGINALSIZE=',size(list)

   if(.not.allocated(list))then
      list=[character(len=max(len_trim(value),2)) :: ]
   endif

   ii=max(len_trim(value),len(list),2)
   end=size(list)

   if(end.eq.0)then                                          ! empty array
      list=[character(len=ii) :: value ]
   elseif(place.eq.1)then                                    ! put in front of array
      kludge=[character(len=ii) :: value, list]
      list=kludge
   elseif(place.gt.end)then                                  ! put at end of array
      kludge=[character(len=ii) :: list, value ]
      list=kludge
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      kludge=[character(len=ii) :: list(:place-1), value,list(place:) ]
      list=kludge
   else                                                      ! index out of range
      write(stderr,*)'*insert_c* error: index out of range. end=',end,' index=',place,' value=',value
   endif

   if(debug)write(stderr,*)'*insert_c* END VALUE=',trim(value),' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_c
subroutine insert_r(list,value,place)

! ident_20="@(#)M_list::insert_r(3fp): place real value into allocatable array at specified position"

real,intent(in)       :: value
real,allocatable      :: list(:)
integer,intent(in)    :: place
integer               :: end

   if(debug) write(stderr,*)'*insert_r* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[real :: ]
   endif

   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                   ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_r* error: index out of range. end=',end,' index=',place,' value=',value
   endif

   if(debug)write(stderr,*)'*insert_r* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_r
subroutine insert_d(list,value,place)

! ident_21="@(#)M_list::insert_d(3fp): place doubleprecision value into allocatable array at specified position"

doubleprecision,intent(in)       :: value
doubleprecision,allocatable      :: list(:)
integer,intent(in)               :: place
integer                          :: end
   if(debug) write(stderr,*)'*insert_d* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(.not.allocated(list))then
      list=[doubleprecision :: ]
   endif
   end=size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_d* error: index out of range. end=',end,' index=',place,' value=',value
   endif
   if(debug)write(stderr,*)'*insert_d* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_d
subroutine insert_l(list,value,place)

! ident_22="@(#)M_list::insert_l(3fp): place value into allocatable array at specified position"

logical,allocatable   :: list(:)
logical,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[logical :: ]
   endif
   end=size(list)
   if(debug)write(stderr,*)'*insert_l* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_l* error: index out of range. end=',end,' index=',place,' value=',value
   endif

   if(debug)write(stderr,*)'*insert_l* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_l
subroutine insert_i(list,value,place)

! ident_23="@(#)M_list::insert_i(3fp): place value into allocatable array at specified position"

integer,allocatable   :: list(:)
integer,intent(in)    :: value
integer,intent(in)    :: place
integer               :: end
   if(.not.allocated(list))then
      list=[integer :: ]
   endif
   end=size(list)
   if(debug)write(stderr,*)'*insert_i* START VALUE=',value,' PLACE=',place,' ORIGINALSIZE=',size(list)
   if(end.eq.0)then                                          ! empty array
      list=[value]
   elseif(place.eq.1)then                                    ! put in front of array
      list=[value, list]
   elseif(place.gt.end)then                                  ! put at end of array
      list=[list, value ]
   elseif(place.ge.2.and.place.le.end)then                 ! put in middle of array
      list=[list(:place-1), value,list(place:) ]
   else                                                      ! index out of range
      write(stderr,*)'*insert_i* error: index out of range. end=',end,' index=',place,' value=',value
   endif

   if(debug)write(stderr,*)'*insert_i* END VALUE=',value,' PLACE=',place,' NEWSIZE=',size(list)
end subroutine insert_i
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    del(3f) - [M_list::dictionary::OOPS] delete entry by key name from a basic dictionary
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   type(dictionary) :: dict
!!
!!    character(len=*),intent(in) :: key
!!
!!    dict%del(key)
!!
!!##DESCRIPTION
!!
!!    Delete an entry from a basic dictionary if it is present.
!!
!!##OPTIONS
!!
!!    DICT   the dictionary.
!!    KEY    the key name to find and delete from the dictionary.
!!
!!##EXAMPLES
!!
!!   Delete an entry from a dictionary by key name.
!!
!!     program demo_del
!!     use M_list, only : dictionary
!!     implicit none
!!     type(dictionary) :: caps
!!     integer                       :: i
!!        ! create a character string dictionary
!!        call caps%set('A','aye')
!!        call caps%set('B','bee')
!!        call caps%set('C','see')
!!        call caps%set('D','dee')
!!        ! show current dictionary
!!        write(*,101)(trim(caps%key(i)),trim(caps%value(i)),i=1,size(caps%key)) ! show array
!!        ! delete dictionary entries
!!        call  caps%del('A')
!!        call  caps%del('C')
!!        call  caps%del('z') ! a noop as there is no key of 'z'
!!        ! show current dictionary
!!        write(*,101)(trim(caps%key(i)),trim(caps%value(i)),i=1,size(caps%key)) ! show array
!!
!!     101 format (1x,*(a,"='",a,"'",:,","))
!!     end program demo_del
!!
!!   Results
!!
!!        > D='dee',C='see',B='bee',A='aye'
!!        > D='dee',B='bee'
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine dict_delete(self,key)

! ident_24="@(#)M_list::dict_delete(3f): remove string from sorted allocatable string array if present"

class(dictionary),intent(in) :: self
character(len=*),intent(in)  :: key
integer                      :: place

   call locate(self%key,key,place)
   if(place.ge.1)then
      call remove(self%key,place)
      call remove(self%value,place)
      call remove(self%count,place)
   endif

end subroutine dict_delete
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    get(3f) - [M_list::dictionary::OOPS] get value of key-value pair in a dictionary given key
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   type(dictionary) :: dict
!!
!!    character(len=*),intent(in) :: key
!!    character(len=*),intent(in) :: VALUE
!!
!!    value=dict%get(key)
!!
!!
!!##DESCRIPTION
!!
!!    get a value given a key from a key-value dictionary
!!
!!    If key is not found in dictionary , return a blank
!!
!!##OPTIONS
!!
!!    DICT     is the dictionary.
!!    KEY      key name
!!    VALUE    value associated with key
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_get
!!     use M_list, only : dictionary
!!     implicit none
!!     type(dictionary)             :: table
!!     character(len=:),allocatable :: val
!!     integer                      :: i
!!
!!        call table%set('A','value for A')
!!        call table%set('B','value for B')
!!        call table%set('C','value for C')
!!        call table%set('D','value for D')
!!        call table%set('E','value for E')
!!        call table%set('F','value for F')
!!        call table%set('G','value for G')
!!
!!        write(*,*)'A=',table%get('A')
!!        write(*,*)'B=',table%get('B')
!!        write(*,*)'C=',table%get('C')
!!        write(*,*)'D=',table%get('D')
!!        write(*,*)'E=',table%get('E')
!!        write(*,*)'F=',table%get('F')
!!        write(*,*)'G=',table%get('G')
!!        write(*,*)'H=',table%get('H')
!!
!!      end program demo_get
!!
!!   Results
!!
!!       >  A=value for A
!!       >  B=value for B
!!       >  C=value for C
!!       >  D=value for D
!!       >  E=value for E
!!       >  F=value for F
!!       >  G=value for G
!!       >  H=
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function dict_get(self,key) result(value)

! ident_25="@(#)M_list::dict_get(3f): get value of key-value pair in dictionary, given key"

class(dictionary),intent(in)    :: self
character(len=*),intent(in)     :: key
character(len=:),allocatable    :: value
integer                         :: place
   call locate(self%key,key,place)
   if(place.lt.1)then
      value=''
   else
      value=self%value(place)(:self%count(place))
   endif
end function dict_get
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    set(3f) - [M_list::dictionary::OOPS] add or replace a key-value pair in a dictionary
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   type(dictionary) :: dict
!!
!!    character(len=*),intent(in) :: key
!!    character(len=*),intent(in) :: VALUE
!!
!!    call dict%rep(key,value)
!!
!!##DESCRIPTION
!!    Add or replace a key-value pair in a dictionary.
!!
!!##OPTIONS
!!    DICT     is the dictionary.
!!    key      key name
!!    VALUE    value associated with key
!!
!!##EXAMPLES
!!
!!   Add or replace a key and value pair in a dictionary
!!
!!     program demo_set
!!     use M_list, only : dictionary
!!     implicit none
!!     type(dictionary) :: dict
!!     integer          :: i
!!
!!         call dict%set('A','b')
!!         call dict%set('B','^')
!!         call dict%set('C',' ')
!!         call dict%set('D','c')
!!         call dict%set('E','ZZ')
!!         call dict%set('F','ZZZZ')
!!         call dict%set('G','z')
!!         call dict%set('A','new value for A')
!!
!!         write(*,'(*(a,"==>","[",a,"]",/))') &
!!          & (trim(dict%key(i)),              &
!!          & dict%value(i)(:dict%count(i)),   &
!!          & i=1,size(dict%key))
!!
!!      end program demo_set
!!
!!   Results
!!
!!       > G==>[z]
!!       > F==>[ZZZZ]
!!       > E==>[ZZ]
!!       > D==>[c]
!!       > C==>[]
!!       > B==>[^]
!!       > A==>[new value for A]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine dict_add(self,key,value)

! ident_26="@(#)M_list::dict_add(3f): place key-value pair into dictionary, adding the key if required"

class(dictionary),intent(inout) :: self
character(len=*),intent(in)     :: key
character(len=*),intent(in)     :: value
integer                         :: place
integer                         :: place2
   call locate(self%key,key,place)
   if(place.le.0)then
      place2=iabs(place)
      call insert( self%key,   key,             place2 )
      call insert( self%value, value,           place2 )
      call insert( self%count, len_trim(value), place2 )
   elseif(place.gt.0)then  ! replace instead of insert
      call replace( self%value, value,           place )
      call replace( self%count, len_trim(value), place )
   endif
end subroutine dict_add
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    clr(3f) - [M_list::dictionary::OOPS] clear basic dictionary
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   type(dictionary) :: dict
!!
!!    call dict%clr()
!!
!!##DESCRIPTION
!!
!!    clear a basic dictionary.
!!
!!##OPTIONS
!!
!!    DICT   the dictionary.
!!
!!##EXAMPLES
!!
!!   create and clear a basic dictionary
!!
!!     program demo_clr
!!     use M_list, only : dictionary
!!     implicit none
!!     type(dictionary) :: caps
!!     integer                       :: i
!!        ! create a character string dictionary
!!        call caps%set('A','aye')
!!        call caps%set('B','bee')
!!        call caps%set('C','see')
!!        call caps%set('D','dee')
!!        ! show current dictionary
!!        write(*,'("DICTIONARY BEFORE CLEARED")')
!!        write(*,101)(trim(caps%key(i)),trim(caps%value(i)),i=1,size(caps%key)) ! show array
!!        call  caps%clr()
!!        write(*,'("DICTIONARY AFTER CLEARED")')
!!        ! show current dictionary
!!        write(*,101)(trim(caps%key(i)),trim(caps%value(i)),i=1,size(caps%key)) ! show array
!!
!!     101 format (1x,*(a,"='",a,"'",:,","))
!!     end program demo_clr
!!
!!   Results
!!
!!       > DICTIONARY BEFORE CLEARED
!!       >  D='dee',C='see',B='bee',A='aye'
!!       > DICTIONARY AFTER CLEARED
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine dict_clear(self)

! ident_27="@(#)M_list::dict_clear(3f): clear basic dictionary"

class(dictionary),intent(inout) :: self
integer                         :: i

   do i=size(self%key),1,-1
      call self%del(self%key(i))
   enddo

end subroutine dict_clear
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    ifdef(3f) - [M_list::dictionary::OOPS] return whether name is present in dictionary or not
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   type(dictionary) :: dict
!!
!!    character(len=*),intent(in) :: key
!!    logical :: value
!!
!!    value=dict%ifdef(key)
!!
!!
!!##DESCRIPTION
!!
!!    determine if name is already defined in dictionary or not
!!
!!##OPTIONS
!!
!!    DICT     is the dictionary.
!!    KEY      key name
!!
!!##RETURNS
!!    VALUE    .FALSE. if name not defined, .TRUE if name is defined.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!     program demo_ifdef
!!     use M_list, only : dictionary
!!     implicit none
!!     type(dictionary)             :: table
!!     character(len=:),allocatable :: val
!!     integer                      :: i
!!
!!        call table%set('A','value for A')
!!        call table%set('B','value for B')
!!        call table%set('C','value for C')
!!        call table%set('D','value for D')
!!        call table%set('E','value for E')
!!        call table%set('F','value for F')
!!        call table%set('G','value for G')
!!        call table%del('F')
!!        call table%del('D')
!!
!!        write(*,*)'A=',table%ifdef('A')
!!        write(*,*)'B=',table%ifdef('B')
!!        write(*,*)'C=',table%ifdef('C')
!!        write(*,*)'D=',table%ifdef('D')
!!        write(*,*)'E=',table%ifdef('E')
!!        write(*,*)'F=',table%ifdef('F')
!!        write(*,*)'G=',table%ifdef('G')
!!        write(*,*)'H=',table%ifdef('H')
!!
!!      end program demo_ifdef
!!
!!   Results
!!
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function dict_ifdef(self,key) result(value)

! ident_28="@(#)M_list::dict_ifdef(3f): return whether name is defined or not"

class(dictionary),intent(in)    :: self
character(len=*),intent(in)     :: key
logical                         :: value
integer                         :: place
   call locate(self%key,key,place)
   if(place.lt.1)then
      value=.false.
   else
      value=.true.
   endif
end function dict_ifdef
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_list
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
