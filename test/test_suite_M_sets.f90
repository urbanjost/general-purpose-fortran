program runtest
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use,intrinsic :: iso_fortran_env, only : real32, real64
use M_framework
use M_sets, only: unique, intersect, union, setdiff, ismember, setxor, issorted
!
implicit none
integer,allocatable           :: a(:)
integer,allocatable           :: b(:)
integer,allocatable           :: c(:)
integer,allocatable           :: expected(:)
real,allocatable              :: fltA(:)
real,allocatable              :: fltB(:)
real,allocatable              :: fltC(:)
real,allocatable              :: fltexpected(:)
character(len=:),allocatable  :: strA(:)
character(len=:),allocatable  :: strB(:)
character(len=:),allocatable  :: strC(:)
character(len=:),allocatable  :: strexpected(:)

   unit_check_level=0

   call test_unique()
   call test_intersect()
   call test_union()
   call test_setdiff()
   call test_ismember()
   call test_setxor()
   call test_issorted()

   call unit_check_stop()

contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unique
   call unit_check_start('unique', 'find the unique elements of vactor A') ! start tests

   A = [10, -10, 0, 1, 2, 3, 3, 2, 1, -10]
   strA = ['ab', 'AB', 'zz', 'mq', 'qm', 'mq', 'Za', 'zz', 'bb', 'qm']

   C = unique(A)
   expected=[-10, 0, 1, 2, 3, 10]
   call unit_check('unique',size(C).eq.size(expected),'sorted size')
   if(size(C).eq.size(expected))then
      call unit_check('unique', all(C .eq. expected),'sorted, expected',str(expected),'got',str(C))
   endif

   C = unique(A, setOrder='stable')
   expected=[10, -10, 0, 1, 2, 3]
   call unit_check('unique',size(C).eq.size(expected),'stable size')
   if(size(C).eq.size(expected))then
      call unit_check('unique', all(C .eq. expected),'stable, expected',str(expected),'got',str(C))
   endif

   strC = unique(strA,setorder='stable')
   strexpected=['ab','AB','zz','mq','qm','Za','bb']
   call unit_check('unique',size(strC).eq.size(strexpected),'sorted size')
   if(size(strC).eq.size(strexpected))then
      call unit_check('unique', all(strC .eq. strexpected),'stable, expected',str(strexpected),'got',str(strC))
   endif

   strC = unique(strA,setorder='sorted')
   strexpected=["AB", "Za", "ab", "bb", "mq", "qm", "zz"]
   call unit_check('unique',size(strC).eq.size(strexpected),'sorted size')
   if(size(strC).eq.size(strexpected))then
      call unit_check('unique', all(strC .eq. strexpected),'sorted, expected',str(strexpected),'got',str(strC))
   endif

   fltA = [10.1, -10.0, 0.0, 1.11, 2.22, 3.33, 3.33, 2.22, 1.11, -10.0]

   fltC = unique(fltA)
   fltexpected=[-10.0, 0.0, 1.11, 2.22, 3.33, 10.1]
   call unit_check('unique',size(fltC).eq.size(fltexpected),'sorted size')
   if(size(fltC).eq.size(fltexpected))then
      call unit_check('unique', all(fltC .eq. fltexpected),'sorted, expected',str(fltexpected),'got',str(fltC))
   endif

   fltC = unique(fltA, setOrder='stable')
   fltexpected=[10.1, -10.0, 0.0, 1.11, 2.22, 3.33]
   call unit_check('unique',size(fltC).eq.size(fltexpected),'stable size')
   if(size(fltC).eq.size(fltexpected))then
      call unit_check('unique', all(fltC .eq. fltexpected),'stable, expected',str(fltexpected),'got',str(fltC))
   endif

   call unit_check_done('unique',msg='test completed')
end subroutine test_unique
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_union
   call unit_check_start('union', 'find the unique elements of vactors A and B') ! start tests

   A = [5, 7, 1]
   B = [3, 1, 1]

   C = union(A,B)
   expected=[1, 3, 5, 7]
   call unit_check('union',size(C).eq.size(expected),'sorted size')
   if(size(C).eq.size(expected))then
      call unit_check('union', all(C .eq. expected),'default, expected',str(expected),'got',str(C))
   endif

   A = [5, 5, 3]
   B = [1, 2, 5]

   C = union(A, B, 'sorted')
   expected=[1,2,3,5]
   call unit_check('union',size(C).eq.size(expected),'sorted size')
   if(size(C).eq.size(expected))then
      call unit_check('union', all(C .eq. expected),'sorted, expected',str(expected),'got',str(C))
   endif

   C = union(A, B, 'stable')
   expected=[5,3,1,2 ]
   call unit_check('union',size(C).eq.size(expected),'stable size')
   if(size(C).eq.size(expected))then
      call unit_check('union', all(C .eq. expected),'stable, expected',str(expected),'got',str(C))
   endif

   strA = ["5", "7", "1"]
   strB = ["3", "1", "1"]

   strC = union(strA,strB)
   strexpected=["1", "3", "5", "7"]
   call unit_check('union',size(strC).eq.size(strexpected),'default size','expected',size(strexpected),'got',size(strC))
   if(size(strC).eq.size(strexpected))then
      call unit_check('union', all(strC .eq. strexpected),'default, expected',str(strexpected),'got',str(strC))
   endif

   strA = ["5", "5", "3"]
   strB = ["1", "2", "5"]

   strC = union(strA, strB, 'sorted')
   strexpected=["1","2","3","5"]
   call unit_check('union',size(strC).eq.size(strexpected),'sorted size','expected',size(strexpected),'got',size(strC))
   if(size(strC).eq.size(strexpected))then
      call unit_check('union', all(strC .eq. strexpected),'sorted, expected',str(strexpected),'got',str(strC))
   endif

   strC = union(strA, strB, 'stable')
   strexpected=["5","3","1","2" ]
   call unit_check('union',size(strC).eq.size(strexpected),'stable size','expected',size(strexpected),'got',size(strC))
   if(size(strC).eq.size(strexpected))then
      call unit_check('union', all(strC .eq. strexpected),'stable, expected',str(strexpected),'got',str(strC))
   endif

   fltA = [5.5, 7.0, 1.2345]
   fltB = [3.333, 1.2345, 1.2345]

   fltC = union(fltA,fltB)
   fltexpected=[1.2345, 3.333, 5.5, 7.0]
   call unit_check('union',size(fltC).eq.size(fltexpected),'sorted size')
   if(size(fltC).eq.size(fltexpected))then
      call unit_check('union', all(fltC .eq. fltexpected),'default, expected',str(fltexpected),'got',str(fltC))
   endif

   fltA = [5.5, 5.5, 3.333]
   fltB = [1.234, 2.0, 5.5]

   fltC = union(fltA, fltB, 'sorted')
   fltexpected=[1.234,2.0,3.333,5.5]
   call unit_check('union',size(fltC).eq.size(fltexpected),'sorted size')
   if(size(fltC).eq.size(fltexpected))then
      call unit_check('union', all(fltC .eq. fltexpected),'sorted, expected',str(fltexpected),'got',str(fltC))
   endif

   fltC = union(fltA, fltB, 'stable')
   fltexpected=[5.5,3.333,1.234,2.0 ]
   call unit_check('union',size(fltC).eq.size(fltexpected),'stable size')
   if(size(fltC).eq.size(fltexpected))then
      call unit_check('union', all(fltC .eq. fltexpected),'stable, expected',str(fltexpected),'got',str(fltC))
   endif

   call unit_check_done('union',msg='test completed')

end subroutine test_union
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_intersect
   call unit_check_start('intersect', 'find the values common to both A and B') ! start tests

   A = [7, 1, 7, 7, 4]
   B = [7, 0, 4, 4, 0]

   C = intersect(A,B)
   expected=[4, 7]
   call unit_check('intersect',size(C).eq.size(expected),'sorted size','expected',size(expected),'got',size(c))
   if(size(C).eq.size(expected))then
      call unit_check('intersect', all(C .eq. expected),'sorted, expected',str(expected),'got',str(C))
   endif

   C = intersect(A, B, 'stable')
   expected=[7, 4 ]
   call unit_check('intersect',size(C).eq.size(expected),'stable size','expected',size(expected),'got',size(c))
   if(size(C).eq.size(expected))then
      call unit_check('intersect', all(C .eq. expected),'stable, expected',str(expected),'got',str(C))
   endif

   strA = [ "7", "1", "7", "7", "4" ]
   strB = [ "7", "0", "4", "4", "0" ]

   strC = intersect(strA,strB)
   strexpected=["4", "7"]
   call unit_check('intersect',size(strC).eq.size(strexpected),'sorted size',&
   'expected',size(strexpected),'got',size(strc),'output=',str(strC))
   if(size(C).eq.size(strexpected))then
      call unit_check('intersect', all(strC .eq. strexpected),'sorted, expected',str(strexpected),'got',str(strC))
   endif

   strC = intersect(strA, strB, 'stable')
   strexpected=["7", "4" ]
   call unit_check('intersect',size(strC).eq.size(strexpected),'stable size',&
   'expected',size(strexpected),'got',size(strc),'output=',str(strc))
   if(size(C).eq.size(strexpected))then
      call unit_check('intersect', all(strC .eq. strexpected),'stable, expected',str(strexpected),'got',str(strC))
   endif

   fltA = [7.777, 1.111, 7.777, 7.777, 4.444]
   fltB = [7.777, 0.000, 4.444, 4.444, 0.000]

   fltC = intersect(fltA,fltB)
   fltexpected=[4.444, 7.777]
   call unit_check('intersect',size(fltC).eq.size(fltexpected),'sorted size',&
   & 'expected',size(fltexpected),'got',size(fltc))
   if(size(fltC).eq.size(fltexpected))then
      call unit_check('intersect', all(fltC .eq. fltexpected),'sorted, expected',str(fltexpected),'got',str(fltC))
   endif

   fltC = intersect(fltA, fltB, 'stable')
   fltexpected=[7.777, 4.444 ]
   call unit_check('intersect',size(fltC).eq.size(fltexpected),'stable size',&
   &'expected', size(fltexpected),'got',size(fltc))
   if(size(fltC).eq.size(fltexpected))then
      call unit_check('intersect', all(fltC .eq. fltexpected),'stable, expected',str(fltexpected),'got',str(fltC))
   endif

   call unit_check_done('intersect',msg='test completed')
end subroutine test_intersect
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_setdiff
   call unit_check_start('setdiff', 'find the values in A that are not in B') ! start tests

   A = [3, 6, 2, 1, 5, 1, 1]
   B = [2, 4, 6]

   C = setdiff(A,B)
   expected=[1,3,5]
   call unit_check('setdiff',size(C).eq.size(expected),'sorted size')
   if(size(C).eq.size(expected))then
      call unit_check('setdiff', all(C .eq. expected),'sorted, expected',str(expected),'got',str(C))
   endif

   A = [4, 1, 3, 2, 5]
   B = [2, 1]

   C = setdiff(A, B, 'sorted')
   expected=[3, 4, 5]
   call unit_check('setdiff',size(C).eq.size(expected),'stable size')
   if(size(C).eq.size(expected))then
      call unit_check('setdiff', all(C .eq. expected),'stable, expected',str(expected),'got',str(C))
   endif

   C = setdiff(A, B, 'stable')
   expected=[4, 3, 5 ]
   call unit_check('setdiff',size(C).eq.size(expected),'stable size')
   if(size(C).eq.size(expected))then
      call unit_check('setdiff', all(C .eq. expected),'stable, expected',str(expected),'got',str(C))
   endif
   
   strA = ['3', '6', '2', '1', '5', '1', '1']
   strB = ['2', '4', '6']

   strC = setdiff(strA,strB)
   strexpected=['1','3','5']
   call unit_check('setdiff',size(strC).eq.size(strexpected),'sorted size')
   if(size(C).eq.size(strexpected))then
      call unit_check('setdiff', all(strC .eq. strexpected),'sorted, expected',str(strexpected),'got',str(strC))
   endif

   strA = ['4', '1', '3', '2', '5']
   strB = ['2', '1']

   strC = setdiff(strA, strB, 'sorted')
   strexpected=['3', '4', '5']
   call unit_check('setdiff',size(strC).eq.size(strexpected),'stable size')
   if(size(C).eq.size(strexpected))then
      call unit_check('setdiff', all(strC .eq. strexpected),'stable, expected',str(strexpected),'got',str(strC))
   endif

   strC = setdiff(strA, strB, 'stable')
   strexpected=['4', '3', '5' ]
   call unit_check('setdiff',size(strC).eq.size(strexpected),'stable size')
   if(size(C).eq.size(strexpected))then
      call unit_check('setdiff', all(strC .eq. strexpected),'stable, expected',str(strexpected),'got',str(strC))
   endif

   fltA = [3.456, 6.54321, 2.01, 1.008, 5.35, 1.008, 1.008]
   fltB = [2.01, 4.6853, 6.54321]

   fltC = setdiff(fltA,fltB)
   fltexpected=[1.008, 3.456, 5.35]
   call unit_check('setdiff',size(fltC).eq.size(strexpected),'sorted size')
   if(size(fltC).eq.size(strexpected))then
      call unit_check('setdiff', all(fltC .eq. fltexpected),'sorted, expected',str(fltexpected),'got',str(fltC))
   endif

   fltA = [4.6853, 1.008, 3.456, 2.01, 5.35]
   fltB = [2.01, 1.008]

   fltC = setdiff(fltA, fltB, 'sorted')
   fltexpected=[3.456, 4.6853, 5.35]
   call unit_check('setdiff',size(fltC).eq.size(fltexpected),'stable size')
   if(size(fltC).eq.size(fltexpected))then
      call unit_check('setdiff', all(fltC .eq. fltexpected),'stable, expected',str(fltexpected),'got',str(fltC))
   endif

   fltC = setdiff(fltA, fltB, 'stable')
   fltexpected=[4.6853, 3.456, 5.35 ]
   call unit_check('setdiff',size(fltC).eq.size(fltexpected),'stable size')
   if(size(fltC).eq.size(fltexpected))then
      call unit_check('setdiff', all(fltC .eq. fltexpected),'stable, expected',str(fltexpected),'got',str(fltC))
   endif

   call unit_check_done('setdiff',msg='test completed')
end subroutine test_setdiff
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_setxor
   call unit_check_start('setxor', 'find values two vectors do not share') ! start tests

   A = [5,1,3,3,3]
   B = [4,1,2]

   C = setxor(A,B)
   expected=[2,3,4,5]
   call unit_check('setxor',size(C).eq.size(expected),'sorted size')
   if(size(C).eq.size(expected))then
      call unit_check('setxor', all(C .eq. expected),'sorted, expected',str(expected),'got',str(C))
   endif

   C = setxor(A, B, 'stable')
   expected=[5, 3, 4, 2 ]
   call unit_check('setxor',size(C).eq.size(expected),'stable size')
   if(size(C).eq.size(expected))then
      call unit_check('setxor', all(C .eq. expected),'stable, expected',str(expected),'got',str(C))
   endif

   strA = ["5","1","3","3","3"]
   strB = ["4","1","2"]

   strC = setxor(strA,strB)
   strexpected=["2","3","4","5"]
   call unit_check('setxor',size(strC).eq.size(strexpected),'sorted size')
   if(size(strC).eq.size(strexpected))then
      call unit_check('setxor', all(strC .eq. strexpected),'sorted, expected',str(strexpected),'got',str(strC))
   endif

   strC = setxor(strA, strB, 'stable')
   strexpected=["5", "3", "4", "2" ]
   call unit_check('setxor',size(strC).eq.size(strexpected),'sorted size')
   if(size(strC).eq.size(strexpected))then
      call unit_check('setxor', all(strC .eq. strexpected),'stable, expected',str(strexpected),'got',str(strC))
   endif

   fltA = [5.4321, 1.11111, 3.55555, 3.55555, 3.55555]
   fltB = [4.9087, 1.11111, 2.0000]

   fltC = setxor(fltA,fltB)
   fltexpected=[2.0000, 3.55555, 4.9087, 5.4321]
   call unit_check('setxor',size(fltC).eq.size(fltexpected),'sorted size')
   if(size(fltC).eq.size(fltexpected))then
      call unit_check('setxor', all(fltC .eq. fltexpected),'sorted, expected',str(fltexpected),'got',str(fltC))
   endif

   fltC = setxor(fltA, fltB, 'stable')
   fltexpected=[5.4321, 3.55555, 4.9087, 2.0000 ]
   call unit_check('setxor',size(fltC).eq.size(fltexpected),'sorted size')
   if(size(fltC).eq.size(fltexpected))then
      call unit_check('setxor', all(fltC .eq. fltexpected),'stable, expected',str(fltexpected),'got',str(fltC))
   endif

   call unit_check_done('setxor',msg='test completed')
end subroutine test_setxor
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ismember
call unit_check_start('ismember', 'report which values in A are also in B') ! start tests
! ! Create two vectors with values in common.

   A = [5,3,4,2]
   B = [2,4,4,4,6,8]

   C = ismember(A,B)
   expected=[0,0,1,1]
   call unit_check('ismember',size(C).eq.size(expected),'sorted size')
   if(size(C).eq.size(expected))then
      call unit_check('ismember', all(C .eq. expected),'sorted, expected',str(expected),'got',str(C))
   endif

   strA = ["5","3","4","2"]
   strB = ["2","4","4","4","6","8"]

   C = ismember(strA,strB)
   expected=[0,0,1,1]
   call unit_check('ismember',size(C).eq.size(expected),'sorted size')
   if(size(C).eq.size(expected))then
      call unit_check('ismember', all(C .eq. expected),'sorted, expected',str(expected),'got',str(C))
   endif

   fltA = [5.5421,3.5421,4.5421,2.5421]
   fltB = [2.5421,4.5421,4.5421,4.5421,6.5421,8.5421]

   C = ismember(fltA,fltB)
   expected=[0,0,1,1]
   call unit_check('ismember',size(C).eq.size(expected),'sorted size')
   if(size(C).eq.size(expected))then
      call unit_check('ismember', all(C .eq. expected),'sorted, expected',str(expected),'got',str(C))
   endif

   call unit_check_done('ismember',msg='test completed')
end subroutine test_ismember
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_issorted
use M_sets, only: issorted

   call unit_check_start( 'issorted','Find the issorted elements of vector A.')

   A = [10, -10, 0, 1, 2, 3, 3, 2, 1, -10]
   call unit_check('issorted', issorted(A).eq.0,'sorted, expected 0 got',issorted(A))

   A = [-10, 10, 100, 201]
   call unit_check('issorted', issorted(A).eq.1,'sorted, expected 1 got',issorted(A))

   strA = ["0", "0", "1", "2", "3", "3", "2", "1" ]
   call unit_check('issorted', issorted(strA).eq.0,'sorted, expected 0 got',issorted(strA))

   strA = [ "10", "20", "30"]
   call unit_check('issorted', issorted(strA).eq.1,'sorted, expected 1 got',issorted(strA))

   fltA = [ 0.5469, 0.5469, 1.5469, 2.5469, 3.5469, 3.5469, 2.5469, 1.5469 ]
   call unit_check('issorted', issorted(fltA).eq.0,'sorted, expected 0 got',issorted(fltA))

   fltA = [ 10.5469, 20.5469, 30.5469]
   call unit_check('issorted', issorted(fltA).eq.1,'sorted, expected 1 got',issorted(fltA))

end subroutine test_issorted
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end program runtest
