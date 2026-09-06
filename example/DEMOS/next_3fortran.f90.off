      !program demo_next
      module enumeration_mod

      enumeration type :: v_value
         enumerator :: v_one, v_two, v_three
         enumerator v_four
      end enumeration type

      enumeration type :: w_value
         enumerator :: w1, w2, w3, w4, w5, w_endsentinel
      end enumeration type

      contains

      subroutine sub(a)
      type(v_value),intent(in) :: a
         print 1,a ! Acts similarly to Print *,Int(a).
      1  format('A has ordinal value ',i0)
      end subroutine

      subroutine wcheck(w)
      type(w_value),intent(in) :: w
         select case(w)
          case(w1)
            print *,'w1 selected'
          case (w2:w4)
            print *,'One of w2...w4 selected'
          case (w_endsentinel)
            stop 'Invalid w selected'
          case default
            stop 'Unrecognized w selected'
         end select
      end subroutine

      end module
      program demo_next
      ! Here is an example of a program using that module.
      use enumeration_mod
      type(v_value) :: x = v_one
      type(v_value) :: y = v_value(2)  ! Explicit constructor producing v_two.
      type(v_value) :: z,nz            ! Initially undefined.
         call sub(x)
         call sub(v_three)
         z = v_value(1)                ! First value.
         do
            if (z==huge(x)) write (*,'(A)',advance='No') ' Huge:'
            call sub(z)
            nz = next(z)
            if (z==nz) exit
            z = nz
         end do

      end program demo_next
