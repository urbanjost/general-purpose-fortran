!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
module M_testsuite_M_sort
use M_framework__verify, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
use M_framework__msg, only : str
use M_sort
implicit none
private
public test_suite_m_sort
integer,parameter            :: dp=kind(0.0d0)
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_m_sort()

   call test_sort_shell()
   call test_sort_quick_rx_r()
   call test_sort_quick_rx_i()
   call test_sort_quick_rx_c()
   call test_sort_quick_rx_d()
   call test_sort_quick_compact()

   call test_unique()
   call test_swap()

   call test_tree_insert()
   call test_tree_print()

   call test_sort_heap()

end subroutine test_suite_m_sort
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tree_insert()

   call unit_test_start('tree_insert',msg='')
   !!call unit_test('tree_insert', 0.eq.0, 'checking',100)
   call unit_test_done('tree_insert',msg='')
end subroutine test_tree_insert
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tree_print()

   call unit_test_start('tree_print',msg='')
   !!call unit_test('tree_print', 0.eq.0, 'checking',100)
   call unit_test_done('tree_print',msg='')
end subroutine test_tree_print
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sort_quick_compact()
integer,parameter            :: isz=10000
real                         :: rn(isz), rn2(isz)
complex(kind=dp)             :: cd(isz)
complex                      :: cc(isz)
doubleprecision              :: dd(isz)
real                         :: rr(isz)
integer                      :: ii(isz)
character(len=:),allocatable :: array(:)
integer                      :: csz
integer                      :: i
!-----------------------------------------------------------------------------------------------------------------------------------
call unit_test_start('sort_quick_compact','') ! start tests
!-----------------------------------------------------------------------------------------------------------------------------------
array= [ 'red    ','green  ','blue   ','yellow ','orange ','black  ','white  ','brown  ','gray   ','cyan   ','magenta','purple ']
array=sort_quick_compact(array)
csz=size(array)
call unit_test('sort_quick_compact',all(array(1:csz-1) .ge. array(2:csz)),msg='sort string array')  ! verify in ascending order
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(Rn)                                           ! Rn contains uniformly distributed random numbers from 0.0 to <1.0
CALL RANDOM_NUMBER(Rn2)
!-----------------------------------------------------------------------------------------------------------------------------------
ii=sort_quick_compact(int(Rn*HUGE(0)))                                                   ! spread values out along range of INTEGER
call unit_test('sort_quick_compact',all(ii(1:isz-1) .ge. ii(2:isz)),msg='sort integer') ! verify in descending order
rr=sort_quick_compact(Rn)
call unit_test('sort_quick_compact',all(rr(1:isz-1) .ge. rr(2:isz)),msg='sort real')
dd=sort_quick_compact(Rn*20000.0d0)
call unit_test('sort_quick_compact',all(dd(1:isz-1) .ge. dd(2:isz)),msg='sort doubleprecision')

cc=sort_quick_compact(cmplx(Rn*20000.0,Rn2*20000.0))
write(10,'(7(g0,1x))')(i,Rn(i)*20000.0,Rn2(i)*20000.0,cc(i),abs(cc(i)),abs(cc(i)).gt.abs(cc(i+1)),i=1,size(cc)-1)
call unit_test('sort_quick_compact', all(abs(cc(1:isz-1)) .ge. abs(cc(2:isz))), msg='sort complex array by magnitude, single')

cd=sort_quick_compact(cmplx(Rn*20000.0d0,Rn2*20000.0d0,kind=dp))
call unit_test('sort_quick_compact', all(abs(cd(1:isz-1)) .ge. abs(cd(2:isz))), msg='sort double complex by magnitude, double')

!-----------------------------------------------------------------------------------------------------------------------------------
call unit_test_done('sort_quick_compact') ! assume if got here passed checks
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine test_sort_quick_compact
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sort_shell()
integer,parameter            :: isz=10000
real                         :: rn(isz), rn2(isz)
complex(kind=dp)             :: ccdd(isz)
complex                      :: cc(isz)
doubleprecision              :: dd(isz)
real                         :: rr(isz)
integer                      :: ii(isz)
character(len=:),allocatable :: array(:)
integer                      :: csz
!-----------------------------------------------------------------------------------------------------------------------------------
call unit_test_start('sort_shell','-library libGPF') ! start tests
!-----------------------------------------------------------------------------------------------------------------------------------
array= [ 'red    ','green  ','blue   ','yellow ','orange ','black  ','white  ','brown  ','gray   ','cyan   ','magenta','purple ']
call sort_shell(array,order='a')
csz=size(array)
call unit_test('sort_shell',all(array(1:csz-1) .le. array(2:csz)),msg='sort string array, ascending')  ! verify in ascending order

array= [ 'RED    ','GREEN  ','BLUE   ','YELLOW ','ORANGE ','BLACK  ','WHITE  ','BROWN  ','GRAY   ','CYAN   ','MAGENTA','PURPLE ']
call sort_shell(array,order='d')
csz=size(array)
call unit_test('sort_shell',all(array(1:csz-1) .ge. array(2:csz)),msg='sort string array, descending') ! verify in descending order
!-----------------------------------------------------------------------------------------------------------------------------------
CALL RANDOM_NUMBER(Rn)                                           ! Rn contains uniformly distributed random numbers from 0.0 to <1.0
CALL RANDOM_NUMBER(Rn2)
!-----------------------------------------------------------------------------------------------------------------------------------
II=Rn*HUGE(1)                                                    ! spread values out along range of INTEGER
call sort_shell(ii,order='a')
call unit_test('sort_shell',all(ii(1:isz-1) .le. ii(2:isz)),msg='sort integer, ascending array')  ! verify in ascending order

II=Rn*HUGE(1)
call sort_shell(ii,order='d')
call unit_test('sort_shell',all(ii(1:isz-1) .ge. ii(2:isz)),msg='sort integer, descending array')
!-----------------------------------------------------------------------------------------------------------------------------------
rr=rn
call sort_shell(rr,order='a')
call unit_test('sort_shell',all(rr(1:isz-1) .le. rr(2:isz)),msg='sort real, ascending')

rr=rn
call sort_shell(rr,order='d')
call unit_test('sort_shell',all(rr(1:isz-1) .ge. rr(2:isz)),msg='sort real, descending')
!-----------------------------------------------------------------------------------------------------------------------------------
dd=Rn*2000.0d0
call sort_shell(dd,order='a')
call unit_test('sort_shell',all(dd(1:isz-1) .le. dd(2:isz)),msg='sort doubleprecision, ascending')
dd=Rn*2000.0d0
call sort_shell(dd,order='d')
call unit_test('sort_shell',all(dd(1:isz-1) .ge. dd(2:isz)),msg='sort doubleprecision, descending')
!-----------------------------------------------------------------------------------------------------------------------------------
cc=cmplx(Rn*20000.0,Rn2*20000.0)
call sort_shell(cc,order='a',type='real')
call unit_test('sort_shell',all(real(cc(1:isz-1)) .le. real(cc(2:isz))),msg='sort complex by real component, ascending')

cc=cmplx(Rn*20000.0,Rn2*20000.0)
call sort_shell(cc,order='d',type='real')
call unit_test('sort_shell',all(real(cc(1:isz-1)) .ge. real(cc(2:isz))),msg='sort complex by real component, descending')

cc=cmplx(Rn*20000.0,Rn2*20000.0)
call sort_shell(cc,order='a',type='imaginary')
call unit_test('sort_shell',all(aimag(cc(1:isz-1)).le.aimag(cc(2:isz))),msg='sort complex by imaginary component, ascending')

cc=cmplx(Rn*20000.0,Rn2*20000.0)
call sort_shell(cc,order='d',type='imaginary')
call unit_test('sort_shell',all(aimag(cc(1:isz-1)) .ge. aimag(cc(2:isz))),msg='sort complex by imaginary component, descending')

cc=cmplx(Rn*20000.0,Rn2*20000.0)
call sort_shell(cc,order='a',type='size')
call unit_test('sort_shell', all(abs(cc(1:isz-1)) .le. abs(cc(2:isz))), msg='sort complex array by magnitude, ascending')

cc=cmplx(Rn*20000.0,Rn2*20000.0)
call sort_shell(cc,order='d',type='size')
call unit_test('sort_shell', all(abs(cc(1:isz-1)) .ge. abs(cc(2:isz))), msg='sort complex array by magnitude, descending')
!-----------------------------------------------------------------------------------------------------------------------------------
ccdd=cmplx(Rn*20000.0,Rn2*20000.0)
call sort_shell(ccdd,order='a',type='real')
call unit_test('sort_shell',all(real(ccdd(1:isz-1)).le.real(ccdd(2:isz))), msg='sort double complex by real component, ascending')

ccdd=cmplx(Rn*20000.0,Rn2*20000.0)
call sort_shell(ccdd,order='d',type='real')
call unit_test('sort_shell',all(real(ccdd(1:isz-1)).ge.real(ccdd(2:isz))), msg='sort double complex by real component, descending')

ccdd=cmplx(Rn*20000.0,Rn2*20000.0)
call sort_shell(ccdd,order='a',type='imaginary')
call unit_test('sort_shell', &
   all(aimag(ccdd(1:isz-1)).le.aimag(ccdd(2:isz))), msg='sort double complex by imaginary component, ascending')

ccdd=cmplx(Rn*20000.0,Rn2*20000.0)
call sort_shell(ccdd,order='d',type='imaginary')
call unit_test('sort_shell', &
   all(aimag(ccdd(1:isz-1)).ge.aimag(ccdd(2:isz))), msg='sort double complex by imaginary component, descending')

ccdd=cmplx(Rn*20000.0,Rn2*20000.0)
call sort_shell(ccdd,order='a',type='size')
call unit_test('sort_shell', all(abs(ccdd(1:isz-1)) .le. abs(ccdd(2:isz))), msg='sort double complex by magnitude, ascending')

ccdd=cmplx(Rn*20000.0,Rn2*20000.0)
call sort_shell(ccdd,order='d',type='size')
call unit_test('sort_shell', all(abs(ccdd(1:isz-1)) .ge. abs(ccdd(2:isz))), msg='sort double complex by magnitude, descending')
!-----------------------------------------------------------------------------------------------------------------------------------
call unit_test_done('sort_shell') ! assume if got here passed checks
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine test_sort_shell
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sort_quick_rx_c()
integer,parameter            :: isz=100
real                         :: rr(isz)
character(len=10)            :: jj(isz)
integer                      :: ii(isz)
integer                      :: i
logical                      :: gb
call unit_test_start('sort_quick_rx', '-library libGPF') ! start tests

CALL RANDOM_NUMBER(RR)
do i=1,isz
   jj(i) = random_string('abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ',10)
enddo

gb=.true.
call sort_quick_rx(jj,ii)
do i=1,isz-1
   if(jj(ii(i)).gt.jj(ii(i+1)))then
      call unit_test_bad('sort_quit_rx_c',msg='Error in sorting strings from small to large')
      gb=.false.
   endif
enddo
if(gb)call unit_test_good('sort_quick_rx',msg='sort string array')

end subroutine test_sort_quick_rx_c
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
function random_string(chars,length) result(out)

!$@(#) M_random::random_string(3f): create random string composed of provided characters of specified length

character(len=*),intent(in)     :: chars
integer,intent(in)              :: length
character(len=:),allocatable    :: out
   real                         :: x
   integer                      :: ilen   ! length of list of characters
   integer                      :: which
   integer                      :: i
   ilen=len(chars)
   out=''
   if(ilen.gt.0)then
      do i=1,length
         call random_number(x)
         which=nint(real(ilen-1)*x)+1
         out=out//chars(which:which)
      enddo
   endif
end function random_string
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sort_quick_rx_i()
integer,parameter            :: isz=10000
integer                      :: first,last ! lowest and highest integer in range of integers to get
real                         :: rr(isz)
real                         :: jj(isz)
integer                      :: ii(isz)
integer                      :: i
logical                      :: gb
first=-(huge(0)-1)
last=huge(0)
call unit_test_start('sort_quick_rx', '-library libGPF') ! start tests

CALL RANDOM_NUMBER(RR)
jj = first + floor((last+1-first)*rr)
gb=.true.
call sort_quick_rx(jj,ii)
do i=1,isz-1
   if(jj(ii(i)).gt.jj(ii(i+1)))then
      call unit_test_bad('sort_quit_rx_i',msg='Error in sorting integers from small to large')
      gb=.false.
   endif
enddo
if(gb)call unit_test_good('sort_quick_rx',msg='sort integer array')

end subroutine test_sort_quick_rx_i
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sort_quick_rx_d()
integer,parameter            :: isz=10000
doubleprecision              :: rr(isz)
integer                      :: ii(isz)
integer                      :: i
logical                      :: gb
call unit_test_start('sort_quick_rx', '-library libGPF') ! start tests

CALL RANDOM_NUMBER(RR)
rr=rr*45000
gb=.true.
call sort_quick_rx(rr,ii)
do i=1,isz-1
   if(rr(ii(i)).gt.rr(ii(i+1)))then
      call unit_test_bad('sort_quit_rx_d',msg='Error in sorting doubleprecision values  from small to large')
      gb=.false.
   endif
enddo
if(gb)call unit_test_good('sort_quick_rx',msg='sort doubleprecision array')

end subroutine test_sort_quick_rx_d
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sort_quick_rx_r()
integer,parameter            :: isz=10000
real                         :: rr(isz)
integer                      :: ii(isz)
integer                      :: i
logical                      :: gb
call unit_test_start('sort_quick_rx', '-library libGPF') ! start tests

CALL RANDOM_NUMBER(RR)
rr=rr*45000
gb=.true.
call sort_quick_rx(rr,ii)
do i=1,isz-1
   if(rr(ii(i)).gt.rr(ii(i+1)))then
      call unit_test_bad('sort_quit_rx_r',msg='Error in sorting reals from small to large')
      gb=.false.
   endif
enddo
if(gb)call unit_test_good('sort_quick_rx',msg='sort real array')

end subroutine test_sort_quick_rx_r
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unique
integer,allocatable :: ints(:)
integer             :: ic
character(len=:),allocatable :: string
call unit_test_start('unique', '-library libGPF') ! start tests

ints=[1,1,2,3,4,4,10,20,20,30]
call unique(ints,ic)
string=str(ints(:ic))
call unit_test('unique',ic.eq.7.and.all(ints(:ic).eq.[1,2,3,4,10,20,30]),'expect 7 ic=',ic, 'ints=',string)

ints=[integer ::]
call unique(ints,ic)
call unit_test('unique',ic.eq.0 .and. all(ints.eq.[integer::]),msg='check empty array ')

ints=[10]
call unique(ints,ic)
call unit_test('unique',ic.eq.1 .and. all(ints(:ic).eq.[10]),msg='check array of one element')

ints=[10,10,10,10]
call unique(ints,ic)
call unit_test('unique',ic.eq.1 .and. all(ints(:ic).eq.[10]),msg='all duplicates')

ints=[10,20,30,40]
call unique(ints,ic)
call unit_test('unique',ic.eq.4 .and. all(ints(:ic).eq.[10, 20, 30, 40]),msg='no change required')

call unit_test_done('unique',msg='test of unique(3f) completed') ! assume if got here passed checks
end subroutine test_unique
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_swap
integer             :: iarray2(2)=[20,10],iarray(2)=[10,20]
real                :: rarray2(2)=[22.22,11.11],rarray(2)=[11.11,22.22]
doubleprecision     :: darray2(2)=[9876.54321d0,1234.56789d0],darray(2)=[1234.56789d0,9876.54321d0]
complex             :: carray2(2)=[(9876,54321),(1234,56789)],carray(2)=[(1234,56789),(9876,54321)]
logical             :: larray2(2)=[.false.,.true.],larray(2)=[.true.,.false.]
character(len=16)   :: string2(2)=["The other string","First string    "],string(2)=["First string    ", "The other string"]

   call unit_test_start('swap',' -library libGPF') ! start tests
   call swap (iarray(1), iarray(2)); call unit_test('swap',all(iarray.eq.iarray2),'integer test')
   call swap (rarray(1), rarray(2)); call unit_test('swap',all(rarray.eq.rarray2),'real test')
   call swap (darray(1), darray(2)); call unit_test('swap',all(darray.eq.darray2),'double test')
   call swap (carray(1), carray(2)); call unit_test('swap',all(carray.eq.carray2),'complex test')
   call swap (larray(1), larray(2)); call unit_test('swap',all(larray.eqv.larray2),'logical test')
   call swap (string(1), string(2)); call unit_test('swap',all(string.eq.string2),'string test')
   call unit_test_done('swap')

end subroutine test_swap
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sort_heap()
implicit none
integer,parameter            :: isz=10000
real                         :: rr(isz)
integer                      :: ii(isz)
character(len=63)            :: cc(isz)
integer                      :: indx(isz)
integer                      :: i
integer                      :: errorcount
   call unit_test_start('sort_heap',' -library libGPF') ! start tests
   ! initializing array with random numbers
   CALL RANDOM_NUMBER(RR)
   rr=rr*450000.0
   ii=rr
   do i=1,size(cc)
      cc(i)=random_string('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ',len(cc))
   enddo


   ! checking if real values are sorted(3f)
   call sort_heap(rr,indx)
   ! use the index array to actually move the input array into a sorted order
   rr=rr(indx)
   errorcount=0
   do i=1,isz-1
      if(rr(i).gt.rr(i+1))then
         call unit_test_msg('sort_heap','Error in sorting reals small to large ',i,rr(i),rr(i+1))
         errorcount=errorcount+1
      endif
   enddo
   call unit_test('sort_heap',errorcount.eq.0,'real errors is ',errorcount,'out of',isz,'values')

   ! checking if integer values are sorted(3f)
   call sort_heap(ii,indx)
   ! use the index array to actually move the input array into a sorted order
   ii=ii(indx)
   errorcount=0
   do i=1,isz-1
      if(ii(i).gt.ii(i+1))then
         call unit_test_msg('sort_heap','Error in sorting integers small to large ',i,rr(i),rr(i+1))
         errorcount=errorcount+1
      endif
   enddo
   call unit_test('sort_heap',errorcount.eq.0,'integer errors is ',errorcount,'out of',isz,'values')

   ! checking if character values are sorted(3f)
   call sort_heap(cc,indx)
   ! use the index array to actually move the input array into a sorted order
   cc=cc(indx)
   errorcount=0
   do i=1,isz-1
      if(cc(i).gt.cc(i+1))then
         call unit_test_msg('sort_heap','Error in sorting characters small to large ',i,rr(i),rr(i+1))
         errorcount=errorcount+1
      endif
   enddo
   call unit_test('sort_heap',errorcount.eq.0,'character errors is ',errorcount,'out of',isz,'values')

   call unit_test_done('sort_heap')

contains

function random_string(chars,length) result(out)

!$@(#) M_random::random_string(3f): create random string composed of provided characters of specified length

character(len=*),intent(in)  :: chars
integer,intent(in)           :: length
character(len=:),allocatable :: out
real                         :: x
integer                      :: ilen   ! length of list of characters
integer                      :: which
integer                      :: i
   ilen=len(chars)
   out=''
   if(ilen.gt.0)then
      do i=1,length
         call random_number(x)
         which=nint(real(ilen-1)*x)+1
         out=out//chars(which:which)
      enddo
   endif
end function random_string

end subroutine test_sort_heap
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end module M_testsuite_M_sort
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
program runtest
use M_framework__msg
use M_framework__verify
use M_framework__verify, only : unit_test, unit_test_start, unit_test_good, unit_test_bad, unit_test_done
use M_framework__verify, only : unit_test_stop
use M_testsuite_M_sort
implicit none
   call test_suite_M_sort()
   call unit_test_stop()
end program runtest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
