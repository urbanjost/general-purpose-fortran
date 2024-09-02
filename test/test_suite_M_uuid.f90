module M_test_suite_M_uuid
use M_framework__msg
use M_framework__verify, only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done
use M_uuid, only : generate_uuid
private
public test_suite_m_uuid
contains
subroutine test_suite_m_uuid()
! this should contains tests for all public procedures in the module
   call test_generate_uuid()
end subroutine test_suite_m_uuid
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine test_generate_uuid()

!   This just checks that we can generate the various types of UUID
!   (without crashing) and checks that they have the correct syntax. We
!   could also check that the UUID changes for each call and I think there
!   is an additional check we could make within the UUID itself. But for
!   now this is enough.

character(len=36) :: uuid
   call unit_check_start('generate_uuid') ! start tests

   uuid = generate_uuid(0)
   call unit_check('generate_uuid',check_uuid(uuid).and.(uuid =='00000000-0000-0000-0000-000000000000'),msg='Version 0 '//uuid)

   uuid = generate_uuid(1)
   call unit_check('generate_uuid',check_uuid(uuid),msg='Version 1 '//uuid)

   uuid = generate_uuid(2)
   call unit_check('generate_uuid',uuid=='',msg='Version 2 (NOT IMPLEMENTED)')

   uuid = generate_uuid(3)
   call unit_check('generate_uuid',uuid=='',msg='Version 3 (NOT IMPLEMENTED)')

   uuid = generate_uuid(4)
   call unit_check('generate_uuid',check_uuid(uuid),msg='Version 4 '//uuid)

   uuid = generate_uuid(5)
   call unit_check('generate_uuid',uuid=='',msg='Version 5 (NOT IMPLEMENTED)')

   call unit_check('compare',exercise(1000000),msg='test for duplicates in 1000000 values')
   call unit_check_done('generate_uuid')
!==================================================================================================================================!
contains
!==================================================================================================================================!
function check_uuid(chars) result(lout)

! Return true if the string is permitted by the UUID BFN in RFC

character(len=*) :: chars
character(len=22), parameter :: hex = '0123456789abcdefABCDEF'
logical :: lout

   lout = (len_trim(chars) == 36)
   if (lout) then
       lout = lout.and.(verify(chars(1:8), hex) == 0)
       lout = lout.and.(verify(chars(9:9), '-') == 0)
       lout = lout.and.(verify(chars(10:13), hex) == 0)
       lout = lout.and.(verify(chars(14:14), '-') == 0)
       lout = lout.and.(verify(chars(15:18), hex) == 0)
       lout = lout.and.(verify(chars(19:19), '-') == 0)
       lout = lout.and.(verify(chars(20:23), hex) == 0)
       lout = lout.and.(verify(chars(24:24), '-') == 0)
       lout = lout.and.(verify(chars(25:36), hex) == 0)
   endif

end function check_uuid
!==================================================================================================================================!
end subroutine test_generate_uuid
!==================================================================================================================================!
function exercise(sz)
logical :: exercise
integer :: sz
character(len=36),allocatable :: uuid(:)
integer :: i,j
integer :: icount
   exercise=.true.
   TYPES: do j=1,4,3
      if(allocated(uuid))deallocate(uuid)
      allocate(uuid(sz))
      do i=1,sz
         uuid(i)=generate_uuid(j)
      enddo
      call sort_shell_strings_lh(uuid)
      call unique_strings(uuid,icount)

      if(icount.ne.size(uuid))then
         exercise=.false.
         exit TYPES
      endif
   enddo TYPES
end function exercise
!==================================================================================================================================!
subroutine sort_shell_strings_lh(lines)
! ident_5="@(#)M_sort::sort_shell_strings_lh(3fp):sort strings(a-z) over specified field using shell sort"
character(len=*) :: lines(:)
   character(len=:),allocatable :: ihold
   integer           :: n, igap, i,j,k, jg
   n=size(lines)
   if(n.gt.0)then
      allocate(character(len=len(lines(1))) :: ihold)
   else
      ihold=''
   endif
   igap=n
   INFINITE: do
      igap=igap/2
      if(igap.eq.0) exit INFINITE
      k=n-igap
      i=1
      INNER: do
         j=i
         INSIDE: do
            jg=j+igap
            if(lle(lines(j),lines(jg)))exit INSIDE
            ihold=lines(j)
            lines(j)=lines(jg)
            lines(jg)=ihold
            j=j-igap
            if(j.lt.1) exit INSIDE
         enddo INSIDE
         i=i+1
         if(i.gt.k) exit INNER
      enddo INNER
   enddo INFINITE
end subroutine sort_shell_strings_lh

subroutine unique_strings(array,ivals)
character(len=*),intent(inout),allocatable  :: array(:)
integer,intent(out)                         :: ivals
   integer                                  :: i,isize
   isize=size(array)
   if(isize.ge.2)then
      ivals=1
      do i=2,isize
        if(array(i).ne.array(i-1))then
           ivals=ivals+1
        else
           write(*,*)'<ERROR> at ',i,' and ', i+1,' duplicates ',array(i)
        endif
      enddo
   else
      ivals=isize
   endif
end subroutine unique_strings
!==================================================================================================================================!
end module M_test_suite_M_uuid
!==================================================================================================================================!
program runtest
use M_framework__msg
use M_framework__verify, only : unit_check_stop
use M_test_suite_M_uuid
implicit none
   call test_suite_M_uuid()
   call unit_check_stop()
end program runtest
!==================================================================================================================================!
