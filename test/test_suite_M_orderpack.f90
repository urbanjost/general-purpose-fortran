!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
program runtest
use M_framework__msg
use M_framework__msg, only : str
use M_framework__verify
use M_framework__verify, only : unit_test_start, unit_test, unit_test_done, unit_test_good, unit_test_bad, unit_test_msg
use M_framework__verify, only : unit_test_stop
! full ranking
use M_orderpack__mrgref, only : mrgref
use M_orderpack__mrgrnk, only : mrgrnk
! full sorting
use M_orderpack__inssor, only : inssor
use M_orderpack__refsor, only : refsor
! pertubation
use M_orderpack__ctrper, only : ctrper
! fractile (nth value)
use M_orderpack__fndnth, only : fndnth
use M_orderpack__indnth, only : indnth
use M_orderpack__valnth, only : valnth
! median
use M_orderpack__indmed, only : indmed
use M_orderpack__valmed, only : valmed
use M_orderpack__median, only : median
!
use M_orderpack__refpar, only : refpar
use M_orderpack__rinpar, only : rinpar
use M_orderpack__rnkpar, only : rnkpar
use M_orderpack__inspar, only : inspar
use M_orderpack__rapknr, only : rapknr
use M_orderpack__unipar, only : unipar
use M_orderpack__mulcnt, only : mulcnt
use M_orderpack__unirnk, only : unirnk
use M_orderpack__unista, only : unista
use M_orderpack__uniinv, only : uniinv
implicit none
character(len=*),parameter :: g='(*(g0,1x))'
integer,parameter          :: dp=kind(0.0d0)

   unit_test_level=0

   call test_gen('mrgref')
   call test_gen('mrgrnk')
   call test_gen('inssor')
   call test_gen('refsor')
   call test_ctrper()
   call test_fndnth()
   call test_indnth()
   call test_valnth()
   call test_indmed()
   call test_valmed()
   call test_median()
   call test_refpar()
   call test_rinpar()
   call test_rnkpar()
   call test_rapknr()
   call test_inspar()
   call test_unipar()
   call test_mulcnt()
   call test_unirnk()
   call test_unista()
   call test_uniinv()

   call unit_test_stop()

contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_gen(name)
character(len=*),intent(in)   :: name
integer,parameter             :: isz=1000
real                          :: rr(isz)
real(kind=dp)                 :: dd(isz)
integer                       :: ii(isz)
character(len=10)             :: cc(isz)
integer                       :: indx(isz)
integer                       :: i

   call unit_test_start(name, '-library orderpack') ! start tests

   CALL RANDOM_NUMBER(RR)
   rr = rr*huge(0.0)
   select case(name)
   case('inssor');call inssor(rr)
   case('refsor');call refsor(rr)
   case('mrgrnk');call mrgrnk(rr,indx); rr=rr(indx)
   case('mrgref');call mrgref(rr,indx); rr=rr(indx)
   endselect
   call unit_test(name,all(rr(1:isz-1) .le. rr(2:isz)),'real test',isz,'values')

   CALL RANDOM_NUMBER(RR)
   ii = rr*huge(0)
   select case(name)
   case('inssor');call inssor(ii)
   case('refsor');call refsor(ii)
   case('mrgrnk');call mrgrnk(ii,indx); ii=ii(indx)
   case('mrgref');call mrgref(ii,indx); ii=ii(indx)
   endselect
   call unit_test(name,all(ii(1:isz-1) .le. ii(2:isz)),'integer test',isz,'values')

   CALL RANDOM_NUMBER(DD)
   dd = dd*huge(0.0_dp)
   select case(name)
   case('inssor');call inssor(dd)
   case('refsor');call refsor(dd)
   case('mrgrnk');call mrgrnk(dd,indx); dd=dd(indx)
   case('mrgref');call mrgref(dd,indx); dd=dd(indx)
   endselect
   call unit_test(name,all(dd(1:isz-1) .le. dd(2:isz)),'double test',isz,'values')

   do i=1,isz
      cc(i) = random_string('abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ',10)
   enddo
   select case(name)
   case('inssor');call inssor(cc)
   case('refsor');call refsor(cc)
   case('mrgrnk');call mrgrnk(cc,indx); cc=cc(indx)
   case('mrgref');call mrgref(cc,indx); cc=cc(indx)
   endselect
   call unit_test(name,all(cc(1:isz-1) .le. cc(2:isz)),'string test, random',isz,'values')

   call unit_test_done(name,msg='test completed')

end subroutine test_gen
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ctrper
implicit none
integer,allocatable :: row(:)
integer,allocatable :: xout(:,:)
integer             :: isz, i, j, jsz
real,allocatable    :: perturb(:)
   call unit_test_start('ctrper', '-library orderpack') ! start tests
        perturb=[0.0,0.1,1.0]
        jsz=size(perturb)
        isz=200
        if(allocated(xout))deallocate(xout)
        allocate(xout(3,isz))
        allocate(row(isz))
        ! make each row the same initially
        row=[(i,i=1,isz)]*10

        do j=1,3
           xout(j,:)=row
           call ctrper(xout(j,:),perturb(j))
        enddo

        !write(*,'(a)')'count    unchanged  perturbed  random'
        !do i=1,size(row)
        !   write(*,'(*(i8,1x))')i,xout(:,i)
        !enddo

   call unit_test('ctrper',all(xout(1,:) .eq. row),'perturb 0 should not change')
   call unit_test('ctrper',.not.(all(xout(3,:) .eq. row)),'perturb 1 should be random,unlikely not changed')

     char: block
      character(len=:),allocatable :: xdont(:)
      xdont=[character(len=20) :: 'a','be','car','dam','fan','gas','egg']
      isz=size(xdont)
      !write(*,g)'Original.................:',(trim(xdont(i)),i=1,isz)
      call ctrper(xdont,1.0)
      !write(*,g)'Perturbed ...............:',(trim(xdont(i)),i=1,isz)
      !write(*,g)
     endblock char
   call unit_test_done('ctrper',msg='test completed')

end subroutine test_ctrper
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_valnth
use M_orderpack__valnth, only : valnth
implicit none
integer,allocatable :: iarr(:)
integer :: i
integer :: imiddle
   call unit_test_start('valnth', '-library orderpack') ! start tests
   ! find Nth lowest value in an array without sorting entire array
   iarr=[80,70,30,40,50,60,20,10]
   ! can return the same values as intrinsics minval() and maxval()
   call unit_test('valnth',valnth(iarr,1).eq.minval(iarr),'like minval()')
   call unit_test('valnth',valnth(iarr,size(iarr)).eq.maxval(iarr),'like maxval()')
   ! but more generally it can return the Nth lowest value.
   call unit_test('valnth',valnth(iarr,8).eq.80,'Nth value')
   ! so a value at the middle would be
   imiddle=(size(iarr)+1)/2
   call unit_test('valnth',valnth(iarr,imiddle).eq.40,'find median')
   ! sort the hard way, one value at a time
   call unit_test('valnth', all([(valnth(iarr,i),i=1,size(iarr))].eq.[10,20,30,40,50,60,70,80]),'sort hard way')
   call unit_test_done('valnth',msg='test completed')
   if(allocated(iarr))deallocate(iarr)
end subroutine test_valnth
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_indnth
use M_orderpack__indnth, only : indnth
implicit none
integer,allocatable :: iarr(:)
integer :: i
integer :: imiddle
   call unit_test_start('indnth', '-library orderpack') ! start tests
   ! find Nth lowest value in an array without sorting entire array
   iarr=[80,70,30,40,50,60,20,10]
   ! can return the same values as intrinsics minloc() and maxloc()
   call unit_test('indnth',all(indnth(iarr,1         ).eq.minloc(iarr)),'like minloc()')
   call unit_test('indnth',all(indnth(iarr,size(iarr)).eq.maxloc(iarr)),'like maxloc()')

   ! but more generally it can return the Nth lowest value.
   call unit_test('indnth',iarr(indnth(iarr,8)).eq.80,'Nth value')
   ! so a value at the middle would be
   imiddle=(size(iarr)+1)/2
   call unit_test('indnth',iarr(indnth(iarr,imiddle)).eq.40,'find median')
   ! sort the hard way, one value at a time
   call unit_test('indnth', all([(iarr(indnth(iarr,i)),i=1,size(iarr))].eq.[10,20,30,40,50,60,70,80]),'sort hard way')

   call unit_test_done('indnth',msg='test completed')
end subroutine test_indnth
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fndnth
use M_orderpack__fndnth, only : fndnth
implicit none
integer,allocatable :: iarr(:)
integer :: i
integer :: imiddle
   call unit_test_start('fndnth', '-library orderpack') ! start tests
   ! find Nth lowest value in an array without sorting entire array
   iarr=[80,70,30,40,50,60,20,10]
   ! can return the same values as intrinsics minval() and maxval()
   call unit_test('fndnth',fndnth(iarr,1).eq.minval(iarr),'like minval()')
   call unit_test('fndnth',fndnth(iarr,size(iarr)).eq.maxval(iarr),'like maxval()')
   ! but more generally it can return the Nth lowest value.
   call unit_test('fndnth',fndnth(iarr,8).eq.80,'Nth value')
   ! so a value at the middle would be
   imiddle=(size(iarr)+1)/2
   call unit_test('fndnth',fndnth(iarr,imiddle).eq.40,'find median')
   ! sort the hard way, one value at a time
   call unit_test('fndnth', all([(fndnth(iarr,i),i=1,size(iarr))].eq.[10,20,30,40,50,60,70,80]),'sort hard way')
   call unit_test_done('fndnth',msg='test completed')
end subroutine test_fndnth
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_indmed
use M_orderpack__indmed, only : indmed
implicit none
real,allocatable :: xdont(:)
real(kind=dp),allocatable :: ddont(:)
integer,allocatable :: idont(:)
character(len=:),allocatable :: cdont(:)
integer :: ii
   call unit_test_start('indmed', '-library orderpack') ! start tests

   xdont=[80.0,70.0,20.0,10.0,1000.0]
   call indmed(xdont,ii)
   call unit_test('indmed', ii.eq.2.and.xdont(ii).eq.70.0, 'real median',ii,xdont(ii))
   !
   idont=[11, 22, 33, 44, 55, 66, 77, 88]
   call indmed(idont,ii)
   call unit_test('indmed', ii.eq.4.and.idont(ii).eq.44, 'integer median',ii,idont(ii))
   !
   ddont=[11.0d0,77.0d0,22.0d0,66.0d0,33.0d0,88.0d0]
   call indmed(ddont,ii)
   call unit_test('indmed', ii.eq.5.and.ddont(ii).eq.33.0d0, 'doubleprecision median',ii,ddont(ii))
   !
   cdont=[character(len=20) :: 'apple','bee','cherry','duck','elephant','finger','goose','h','insect','j']
   call indmed(cdont,ii)
   call unit_test('indmed', ii.eq.5.and.cdont(ii).eq.'elephant', 'character median',ii,cdont(ii))
   !
   call unit_test_done('indmed',msg='test completed')
end subroutine test_indmed
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_valmed
use M_orderpack__valmed, only : valmed
implicit none
real,allocatable :: xdont(:)
real(kind=dp),allocatable :: ddont(:)
integer,allocatable :: idont(:)
   call unit_test_start('valmed', '-library orderpack') ! start tests

   xdont=[80.0,70.0,20.0,10.0,1000.0]
   call unit_test('valmed', valmed(xdont).eq.70.0, 'real valmed',valmed(xdont),70.0)
   !
   idont=[11, 22, 33, 44, 55, 66, 77, 88]
   call unit_test('valmed', valmed(idont).eq.44, 'integer valmed',valmed(idont),44)
   !
   ddont=[11.0d0, 77.0d0, 22.0d0, 66.0d0, 33.0d0, 88.0d0]
   call unit_test('valmed', valmed(ddont).eq.33.0d0, 'doubleprecision valmed',valmed(ddont),33.0)
   !
   call unit_test_done('valmed',msg='test completed')
end subroutine test_valmed
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_median
use M_orderpack__median, only : median
implicit none
real,allocatable :: xdont(:)
real(kind=dp),allocatable :: ddont(:)
integer,allocatable :: idont(:)
   call unit_test_start('median', '-library orderpack') ! start tests

   xdont=[80.0,70.0,20.0,10.0,1000.0]
   call unit_test('median', median(xdont).eq.70.0, 'real median',median(xdont),70.0)
   !
   idont=[11, 22, 33, 44, 55, 66, 77, 88]
   call unit_test('median', median(idont).eq.49, 'integer median',median(idont),49)
   !
   ddont=[11.0d0,77.0d0,22.0d0,66.0d0,33.0d0,88.0d0]
   call unit_test('median', median(ddont).eq.49.5d0, 'doubleprecision median',median(ddont),49.5)
   !
   call unit_test_done('median',msg='test completed')
end subroutine test_median
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_inspar()
implicit none
integer,parameter :: big=2000, little=300
real              :: valsr(big)
   call unit_test_start('inspar', '-library orderpack') ! start tests
   call random_seed()
   call random_number(valsr)
   valsr=valsr*1000000.0-500000.0
   call inspar(valsr,little)
   call unit_test('inspar',all(valsr(1:little-1) .le. valsr(2:little)),'real test',little,'out of',big,'values')
   call unit_test_done('inspar',msg='test completed')
end subroutine test_inspar
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rapknr()
implicit none
integer,parameter :: big=2000, little=300
real              :: valsr(big)
integer           :: indx(little)
   call unit_test_start('rapknr', '-library orderpack') ! start tests
   call random_seed()
   call random_number(valsr)
   valsr=valsr*1000000.0-500000.0
   call rapknr(valsr,indx,little)
   valsr(:300)=valsr(indx(:little))
   call unit_test('rapknr',all(valsr(1:little-1) .ge. valsr(2:little)),'real test',little,'out of',big,'values')
   call unit_test_done('rapknr',msg='test completed')
end subroutine test_rapknr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rnkpar()
implicit none
integer,parameter :: big=2000, little=300
real              :: valsr(big)
integer           :: indx(little)
   call unit_test_start('rnkpar', '-library orderpack') ! start tests
   call random_seed()
   call random_number(valsr)
   valsr=valsr*1000000.0-500000.0
   call rnkpar(valsr,indx,little)
   valsr(:300)=valsr(indx(:little))
   call unit_test('rnkpar',all(valsr(1:little-1) .le. valsr(2:little)),'real test',little,'out of',big,'values')
   call unit_test_done('rnkpar',msg='test completed')
end subroutine test_rnkpar
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rinpar()
implicit none
integer,parameter :: big=2000, little=300
real              :: valsr(big)
integer           :: indx(little)
   call unit_test_start('rinpar', '-library orderpack') ! start tests
   call random_seed()
   call random_number(valsr)
   valsr=valsr*1000000.0-500000.0
   call rinpar(valsr,indx,little)
   valsr(:300)=valsr(indx(:little))
   call unit_test('rinpar',all(valsr(1:little-1) .le. valsr(2:little)),'real test',little,'out of',big,'values')
   call unit_test_done('rinpar',msg='test completed')
end subroutine test_rinpar
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_refpar()
implicit none
integer,parameter :: big=2000, little=300
real              :: valsr(big)
integer           :: indx(little)
   call unit_test_start('refpar', '-library orderpack') ! start tests
   call random_seed()
   call random_number(valsr)
   valsr=valsr*1000000.0-500000.0
   call refpar(valsr,indx,little)
   valsr(:300)=valsr(indx(:little))
   call unit_test('refpar',all(valsr(1:little-1) .le. valsr(2:little)),'real test',little,'out of',big,'values')
   call unit_test_done('refpar',msg='test completed')
end subroutine test_refpar
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unipar()
use M_orderpack__unipar, only : unipar
implicit none
integer,allocatable :: xdont(:)
integer,allocatable :: irngt(:)
integer :: nord
   call unit_test_start('unipar', '-library orderpack') ! start tests
   !
   xdont=[10,5,7,1,4,5,6,8,9,10,1]
   nord=5
   if(allocated(irngt))deallocate(irngt)
   allocate(irngt(nord))
   !
   call unipar(xdont,irngt,nord)

   call unit_test('unipar',nord.eq.5,'number of unique values found',nord,5)
   call unit_test('unipar',all(irngt(1:nord) .eq. [11,5,2,7,3]) ,'returned indices')
   call unit_test('unipar',all(xdont(irngt(1:nord)) .eq.[1,4,5,6,7]) ,'returned values')

   call unit_test_done('unipar',msg='test completed')
end subroutine test_unipar
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mulcnt()
character(len=20),allocatable :: strings(:)
integer,allocatable :: cindx(:)
integer :: csz
   call unit_test_start('mulcnt', '-library orderpack') ! start tests
   !
   strings= [ character(len=20) ::                   &
    & 'two  ',  'four ', 'three', 'five',   'five',  &
    & 'two  ',  'four ', 'three', 'five',   'five',  &
    & 'four ',  'four ', 'three', 'one  ',  'five']
   csz=size(strings)
   if(allocated(cindx))deallocate(cindx)
   allocate(cindx(csz))
   call mulcnt(strings,cindx)
   call unit_test('mulcnt',all(cindx .eq.  [2,4,3,5,5,2,4,3,5,5,4,4,3,1,5]) ,'returned values')
   call unit_test_done('mulcnt',msg='test completed')
end subroutine test_mulcnt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unirnk()
integer,allocatable :: xvalt(:)
integer,allocatable :: irngt(:)
integer :: nuni
   call unit_test_start('unirnk', '-library orderpack') ! start tests
   xvalt=[10,5,7,1,4,5,6,8,9,10,1]
   if(allocated(irngt))deallocate(irngt)
   allocate(irngt(size(xvalt)))
   call unirnk(xvalt,irngt,nuni)
   call unit_test('unirnk',nuni.eq.8,'number of indices. got',nuni,'expected',8)
   call unit_test('unirnk',all(irngt(:nuni) .eq.  [ 4,5,2,7,3,8,9,1 ]) ,'returned indices')
   call unit_test('unirnk',all(xvalt(irngt(:nuni)) .eq.  [ 1,4,5,6,7,8,9,10 ]) ,'sorted data')
   call unit_test_done('unirnk',msg='test completed')
end subroutine test_unirnk
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unista()
integer,allocatable :: xdont(:)
integer :: nuni
   call unit_test_start('unista', '-library orderpack') ! start tests
   xdont=[44,33,33,33,22,11,33,44,55,33]
   call unista(xdont,nuni)
   call unit_test('unista',nuni.eq.5,'number of indices. got',nuni,'expected',5)
   call unit_test('unista',all(xdont(:nuni) .eq.  [ 44,33,22,11,55 ]) ,'unique values')
   call unit_test_done('unista',msg='test completed')
end subroutine test_unista
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_uniinv()
integer,allocatable :: xdont(:)
integer,allocatable :: igoest(:)
integer,allocatable :: out(:)
integer :: imx
integer :: i
   xdont=[10,20,30,10,20,30,10,20,30]
   if(allocated(igoest))deallocate(igoest)
   allocate(igoest(size(xdont)))

   call uniinv(xdont,igoest)

   call unit_test('uniinv',all(igoest .eq.  [ 1,2,3,1,2,3,1,2,3 ]) ,'returned indices')
   imx=maxval(igoest)
   call unit_test('unista',imx.eq.3,'unique indices. got',imx,'expected',3)
   if(allocated(out))deallocate(out)
   allocate(out(imx))
   do i=1,imx
           out(igoest(i))=xdont(i)
   enddo
   call unit_test('uniinv',all(out .eq.  [ 10,20,30 ]) ,'sorted unique values')
   call unit_test_done('uniinv',msg='test completed')
end subroutine test_uniinv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
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
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end program runtest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
