      program demo_cmplx
      implicit none
      integer,parameter :: dp=kind(0.0d0)
      real(kind=dp)     :: precise
      complex(kind=dp)  :: z8
      complex           :: z4, zthree(3)
         precise=1.2345678901234567d0

         ! basic
         z4 = cmplx(-3)
         print *, 'Z4=',z4
         z4 = cmplx(1.23456789, 1.23456789)
         print *, 'Z4=',z4
         ! with a format treat a complex as two real values
         print '(1x,g0,1x,g0,1x,g0)','Z4=',z4

         ! working with higher precision values
         ! using kind=dp makes it keep DOUBLEPRECISION precision
         ! otherwise the result would be of default kind
         z8 = cmplx(precise, -precise )
         print *, 'lost precision Z8=',z8
         z8 = cmplx(precise, -precise ,kind=dp)
         print *, 'kept precision Z8=',z8

         ! assignment of constant values does not require cmplx(3)00
         ! The following is intuitive and works without calling cmplx(3)
         ! but does not work for variables just constants
         z8 = (1.1111111111111111d0, 2.2222222222222222d0 )
         print *, 'Z8 defined with constants=',z8

         ! what happens when you assign a complex to a real?
         precise=z8
         print *, 'LHS=',precise,'RHS=',z8

         ! elemental
         zthree=cmplx([10,20,30],-1)
         print *, 'zthree=',zthree

         ! descriptors are an alternative
         zthree(1:2)%re=[100,200]
         print *, 'zthree=',zthree

         TEST: block
            complex :: x
            real,parameter:: y=sin(3.0)
            ! TEST I
            ! x%re|mi can appear on the left-hand side of an assignment
            x%re=100.0
            x%im=sin(3.0)
            write(*,*)'LHS:',x, &
            merge('PASSED','FAILED',x.eq.cmplx(100.0,y) )

            ! TEST II
            ! it can be passed as a subroutine argument and be changed
            call trivial(x%re)
            write(*,*)'RETURNED:',x, &
            merge('PASSED','FAILED', x.eq.(200.0,y) )
         endblock TEST
      contains
         subroutine trivial(r)
            real,intent(inout) :: r
            r=r*2
         end subroutine trivial

      end program demo_cmplx
