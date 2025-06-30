      program demo_logical
      use iso_fortran_env, only : logical_kinds
      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      use,intrinsic :: iso_fortran_env, only : real32, real64, real128
      implicit none
      character(len=*),parameter :: g='(*(g0))'
      integer :: i, i1, i2
      logical :: l1, l2
        !
        ! list kind values supported on this platform
        !
         do i =1, size(logical_kinds)
            write(*,'(*(g0))')'integer,parameter :: boolean', &
            & logical_kinds(i),'=', logical_kinds(i)
         enddo
        ! for performance and storage purposes you generally want
        ! to use the smallest storage size supported when using
        ! large arrays, but some existing routines may require
        ! the default kind. LOGICAL(3f) can change the kind of
        ! the variables.
        !
        ! But converting a logical to an integer is not done
        ! with LOGICAL(3f); but can be down with MERGE(3f).
        !
         l1=.true.
         l2=.false.
         i1=merge(0,1,l1)
         i2=merge(0,1,l2)
         write(*,g)'L1=',l1,' L2=',l2,' I1=',i1,' I2=',i2
        !
        ! show type and kind of default logicals
         call showme(.true.)
         call showme(l1)
        ! show logical() changing type and kind
         call showme(logical(l1))
        ! you may have to delete unsupported kinds from this example

        ! this is probably the default
         call showme(logical(l1,kind=4))
        ! note how showme shows different kinds are being passed to it
         call showme(logical(l1,kind=8))
         call showme(logical(l1,kind=2))
        ! this is probably the smallest storage size supported
        ! on this platform; but kind values are platform-specific
         call showme(logical(l1,kind=1))
      contains
      subroutine showme(val)
      ! @(#) showme(3f) - display type and kind of intrinsic value
      class(*),intent(in) :: val
         select type(val)
            type is (integer(kind=int8))
              write(*,'("integer(kind=int8) ",i0)') val
            type is (integer(kind=int16))
               write(*,'("integer(kind=int16) ",i0)') val
            type is (integer(kind=int32))
               write(*,'("integer(kind=int32) ",i0)') val
            type is (integer(kind=int64))
               write(*,'("integer(kind=int64) ",i0)') val
            type is (real(kind=real32))
               write(*,'("real(kind=real32) ",1pg0)') val
            type is (real(kind=real64))
               write(*,'("real(kind=real64) ",1pg0)') val
            type is (real(kind=real128))
              write(*,'("real(kind=real128) ",1pg0)') val
            type is (logical(kind=1))
                  write(*,'("logical(kind=1) ",l1,a,i0)') val, &
              & 'storage=',storage_size(val)
            type is (logical(kind=2))
                  write(*,'("logical(kind=2) ",l1,a,i0)') val, &
              & 'storage=',storage_size(val)
            type is (logical(kind=4))
                  write(*,'("logical(kind=4) ",l1,a,i0)') val, &
              & 'storage=',storage_size(val)
            type is (logical(kind=8))
                  write(*,'("logical(kind=8) ",l1,a,i0)') val, &
              & 'storage=',storage_size(val)
            type is (character(len=*))
                write(*,'("character ",a)') trim(val)
            type is (complex)
                         write(*,'("","(",1pg0,",",1pg0,")")') val
            class default
            stop 'crud. showme() does not know about this type'
         end select
      end subroutine showme
      end program demo_logical
