         program demo_implicit
         ! everything accessed via USE already has a type and comes
         ! before an implicit statement; but implicit rules are not
         ! inherited from modules
         use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT
         !
         ! the implicit statement must come before other declarations
         ! in new code using this turns on strong typing (that is,every
         ! variable has to have its type declared in a statement). This
         ! is generally highly recommended for new code.
         implicit none
         ! it is still a convention used by many programmers to reserve
         ! starting letters of I to N for integers.
         integer    :: i, j, k
         type(real) :: x,y,z
         intrinsic sin,cos ! intrinsic types are already specified
         integer,external :: zzz ! but external functions need declared
                                 ! if they do not have an interface
         call sub1()
         call sub2()
         contains
         subroutine sub1()
         ! the implicit none above became the default for contained
         ! procedures so no reason to repeat it. So only required once
         ! in main procedure or once in top of a module to change the
         ! default of all procedures defined after a CONTAINS statement
         integer :: i=10,j=20
            write(*,*)'I=',i,'J=',j
         end subroutine sub1
         subroutine sub2()
         ! a contained subroutine can override the default created in the
         ! containing scope though
         implicit complex(a-z)
            A=(10,20)
            write(*,*)'A=',a
         end subroutine sub2
         end
         integer function zzz()
             zzz=1234
         end function zzz
         !end program demo_implicit
