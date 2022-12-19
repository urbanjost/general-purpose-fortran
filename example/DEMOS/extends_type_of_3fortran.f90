        ! program demo_extends_type_of
        module M_demo_extends_type_of
        implicit none
        private

        type nothing
        end type nothing

        type, extends(nothing) :: dot
          real :: x=0
          real :: y=0
        end type dot

        type, extends(dot) :: point
          real :: z=0
        end type point

        type something_else
        end type something_else

        public :: nothing
        public :: dot
        public :: point
        public :: something_else

        end module M_demo_extends_type_of

        program demo_extends_type_of
        use M_demo_extends_type_of, only : nothing, dot, point, something_else
        implicit none
        type(nothing) :: grandpa
        type(dot) :: dad
        type(point) :: me
        type(something_else) :: alien

         write(*,*)'these should all be true'
         write(*,*)extends_type_of(me,grandpa),'I am descended from Grandpa'
         write(*,*)extends_type_of(dad,grandpa),'Dad is descended from Grandpa'
         write(*,*)extends_type_of(me,dad),'Dad is my ancestor'

         write(*,*)'is an object an extension of itself?'
         write(*,*)extends_type_of(grandpa,grandpa) ,'self-propagating!'
         write(*,*)extends_type_of(dad,dad) ,'clone!'

         write(*,*)' you did not father your grandfather'
         write(*,*)extends_type_of(grandpa,dad),'no paradox here'

         write(*,*)extends_type_of(dad,me),'no paradox here'
         write(*,*)extends_type_of(grandpa,me),'no relation whatsoever'
         write(*,*)extends_type_of(grandpa,alien),'no relation'
         write(*,*)extends_type_of(me,alien),'not what everyone thinks'

         call pointers()
         contains

         subroutine pointers()
         ! Given the declarations and assignments
         type t1
         real c
         end type
         type, extends(t1) :: t2
         end type
         class(t1), pointer :: p, q
            allocate (p)
            allocate (t2 :: q)
            ! the result of EXTENDS_TYPE_OF (P, Q) will be false, and the result
            ! of EXTENDS_TYPE_OF (Q, P) will be true.
            write(*,*)'(P,Q)',extends_type_of(p,q),"mind your P's and Q's"
            write(*,*)'(Q,P)',extends_type_of(q,p)
         end subroutine pointers

        end program demo_extends_type_of
