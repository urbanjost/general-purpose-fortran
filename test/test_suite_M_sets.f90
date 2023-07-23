program runtest
use M_framework
use M_sets, only: unique, intersect, union, setdiff, ismember, setxor
!
implicit none
integer, allocatable       :: A(:)
integer, allocatable       :: B(:)
integer, allocatable       :: C(:)
integer,allocatable        :: expected(:)

   unit_check_level=0

   call test_unique()
   call test_intersect()
   call test_union()
   call test_setdiff()
   call test_ismember()
   call test_setxor()

   call unit_check_stop()

contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unique
implicit none
   call unit_check_start('unique', 'find the unique elements of vactor A') ! start tests

   A = [10, -10, 0, 1, 2, 3, 3, 2, 1, -10]

   C = unique(A)
   expected=[-10, 0, 1, 2, 3, 10]
   call unit_check('unique', all(C .eq. expected),'sorted, expected',str(expected),'got',str(C))
   call unit_check('unique',size(C).eq.6,'sorted size')

   C = unique(A, setOrder='stable')
   expected=[10, -10, 0, 1, 2, 3]
   call unit_check('unique', all(C .eq. expected),'stable, expected',str(expected),'got',str(C))
   call unit_check('unique',size(C).eq.6,'stable size')

   call unit_check_done('unique',msg='test completed')
end subroutine test_unique
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_union
implicit none
   call unit_check_start('union', 'find the unique elements of vactors A and B') ! start tests

   A = [5, 7, 1]
   B = [3, 1, 1]

   C = union(A,B)
   expected=[1, 3, 5, 7]
   call unit_check('union', all(C .eq. expected),'sorted, expected',str(expected),'got',str(C))
   call unit_check('union',size(C).eq.4,'sorted size')

   A = [5, 5, 3]
   B = [1, 2, 5]

   C = union(A, B, 'sorted')
   expected=[1,2,3,5]
   call unit_check('union', all(C .eq. expected),'sorted, expected',str(expected),'got',str(C))
   call unit_check('union',size(C).eq.4,'sorted size')

   C = union(A, B, 'stable')
   expected=[5,3,1,2 ]
   call unit_check('union', all(C .eq. expected),'stable, expected',str(expected),'got',str(C))
   call unit_check('union',size(C).eq.4,'stable size')


   call unit_check_done('union',msg='test completed')
end subroutine test_union
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_intersect
implicit none
   call unit_check_start('intersect', 'find the values common to both A and B') ! start tests

   A = [7, 1, 7, 7, 4]
   B = [7, 0, 4, 4, 0]

   C = intersect(A,B)
   expected=[4, 7]
   call unit_check('intersect', all(C .eq. expected),'sorted, expected',str(expected),'got',str(C))
   call unit_check('intersect',size(C).eq.2,'sorted size')

   C = intersect(A, B, 'stable')
   expected=[7, 4 ]
   call unit_check('intersect', all(C .eq. expected),'stable, expected',str(expected),'got',str(C))
   call unit_check('intersect',size(C).eq.2,'stable size')

   call unit_check_done('intersect',msg='test completed')
end subroutine test_intersect
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_setdiff
implicit none
   call unit_check_start('setdiff', 'find the values in A that are not in B') ! start tests

   A = [3, 6, 2, 1, 5, 1, 1]
   B = [2, 4, 6]

   C = setdiff(A,B)
   expected=[1,3,5]
   call unit_check('setdiff', all(C .eq. expected),'sorted, expected',str(expected),'got',str(C))
   call unit_check('setdiff',size(C).eq.3,'sorted size')

   A = [4, 1, 3, 2, 5]
   B = [2, 1]

   C = setdiff(A, B, 'sorted')
   expected=[3, 4, 5]
   call unit_check('setdiff', all(C .eq. expected),'stable, expected',str(expected),'got',str(C))
   call unit_check('setdiff',size(C).eq.3,'stable size')

   C = setdiff(A, B, 'stable')
   expected=[4, 3, 5 ]
   call unit_check('setdiff', all(C .eq. expected),'stable, expected',str(expected),'got',str(C))
   call unit_check('setdiff',size(C).eq.3,'stable size')

   call unit_check_done('setdiff',msg='test completed')
end subroutine test_setdiff
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_setxor
implicit none
   call unit_check_start('setxor', 'find values two vectors do not share') ! start tests

   A = [5,1,3,3,3]
   B = [4,1,2]

   C = setxor(A,B)
   expected=[2,3,4,5]
   call unit_check('setxor', all(C .eq. expected),'sorted, expected',str(expected),'got',str(C))
   call unit_check('setxor',size(C).eq.4,'sorted size')

   C = setxor(A, B, 'stable')
   expected=[5, 3, 4, 2 ]
   call unit_check('setxor', all(C .eq. expected),'stable, expected',str(expected),'got',str(C))
   call unit_check('setxor',size(C).eq.4,'stable size')

   call unit_check_done('setxor',msg='test completed')
end subroutine test_setxor
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ismember
implicit none
call unit_check_start('ismember', 'report which values in A are also in B') ! start tests
! ! Create two vectors with values in common.

   A = [5,3,4,2]
   B = [2,4,4,4,6,8]

   C = ismember(A,B)
   expected=[0,0,1,1]
   call unit_check('ismember', all(C .eq. expected),'sorted, expected',str(expected),'got',str(C))
   call unit_check('ismember',size(C).eq.4,'sorted size')

   call unit_check_done('ismember',msg='test completed')
end subroutine test_ismember
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end program runtest
