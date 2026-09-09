        ! program demo_same_type_as
        module M_ether
        implicit none
        private

        type   :: dot
          real :: x=0
          real :: y=0
        end type dot

        type, extends(dot) :: point
          real :: z=0
        end type point

        type something_else
        end type something_else

        public :: dot
        public :: point
        public :: something_else

        end module M_ether

        program demo_same_type_as
        use M_ether, only : dot, point, something_else
        implicit none
        type(dot) :: dad, mom
        type(point) :: me
        type(something_else) :: alien

         write(*,*)same_type_as(me,dad),'I am descended from Dad, but equal?'
         write(*,*)same_type_as(me,me) ,'I am what I am'
         write(*,*)same_type_as(dad,mom) ,'what a pair!'

         write(*,*)same_type_as(dad,me),'no paradox here'
         write(*,*)same_type_as(dad,alien),'no relation'

         call pointers()
         contains
         subroutine pointers()
         ! Given the declarations and assignments
         type t1
            real c
         end type
         type, extends(t1) :: t2
         end type
         class(t1), pointer :: p, q, r
            allocate (p, q)
            allocate (t2 :: r)
            ! the result of SAME_TYPE_AS (P, Q) will be true, and the result
            ! of SAME_TYPE_AS (P, R) will be false.
            write(*,*)'(P,Q)',same_type_as(p,q),"mind your P's and Q's"
            write(*,*)'(P,R)',same_type_as(p,r)
         end subroutine pointers

        end program demo_same_type_as
