      program demo_logical
      use iso_fortran_env, only : logical_kinds
      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      use,intrinsic :: iso_fortran_env, only : real32, real64, real128
      !
      ! The standard only requires one default logical kind to be supported
      ! of the same storage size as a default INTEGER and REAL but the
      ! following kind names are standard. The kind may not be
      ! supported (in which case the value of the kind name will be a
      ! negative integer value) and additional kinds may be available as well.
      use,intrinsic :: iso_fortran_env, only : &
       & LOGICAL8, LOGICAL16, LOGICAL32, LOGICAL64
      !
      ! C_BOOL is a kind compatible with C interfaces
      use,intrinsic :: iso_c_binding,   only : C_BOOL
      !
      implicit none
      character(len=*),parameter            :: all='(*(g0))'
      integer                               :: i, i1, i2
      ! make T and F abbreviations for .TRUE. and .FALSE.
      logical,parameter                     :: T=.true., F=.false.
      logical                               :: l1, l2
      ! potentially save space and improve performance by using the
      ! smallest available kind
      logical(kind=selected_logical_kind(1)) :: smallest_storage(10,20)
      logical(kind=c_bool)                   :: boolean=.TRUE.
        !
        print all, 'list LOGICAL kind values available on this platform'
         do i =1, size(logical_kinds)
            write(*,all)'   integer,parameter :: boolean', &
            & logical_kinds(i),'=', logical_kinds(i)
         enddo

        print all, '   LOGICAL8  ==> KIND=',LOGICAL8
        print all, '   LOGICAL16 ==> KIND=',LOGICAL16
        print all, '   LOGICAL32 ==> KIND=',LOGICAL32
        print all, '   LOGICAL64 ==> KIND=',LOGICAL64
        print all, '   C_BOOL    ==> KIND=',C_BOOL

        print all, 'MERGE() is one method for transposing logical and integer'
        ! converting a logical to an integer is not done
        ! with LOGICAL(3f) and INT(3f) or promotion by assignment;
        ! but can be done with MERGE(3f) with scalars or arrays.
         i1=merge(0,1,T)
         i2=merge(0,1,F)
         write(*,all)'   T-->',i1,' F-->',I2
         l1=merge(T,F,i1.eq.0)
         l2=merge(T,F,i2.eq.0)
         write(*,all)'   0-->',l1,' 1-->',l2
        !
        ! Note the standard specifies the default INTEGER, REAL, and LOGICAL
        ! types have the same storage size, but compiler options often allow
        ! changing that. STORAGE_SIZE() can be used to confirm that.
        !
        print all, 'show kind and storage size of default logical'
         call showme(.true.)
         call showme(l1)
        ! A method to portably request the smallest storage size is
        !    logical(kind=selected_logical_kind(1) :: array(1000,1000)
        print all, 'storage size of smallest logical kind'
         call showme(logical(l1,kind=selected_logical_kind(1)))

        ! you may have to delete unsupported kinds from this example
        print all, 'different kinds are being passed because of LOGICAL() call'
        print all,'KIND values are platform-specific'
         call showme(logical(l1,kind=1))
         call showme(logical(l1,kind=2))
         call showme(logical(l1,kind=4))
         call showme(logical(l1,kind=8))
        print all,'kind=C_BOOL'
         call showme(logical(l1,kind=c_bool))
         call showme(boolean)
        print all,'SELECTED_LOGICAL_KIND() is more portable than KIND values'
        ! you might want to check the resulting kind
         call showme(logical(l1,kind=selected_logical_kind(1))) ! smallest
         call showme(logical(l1,kind=kind(.true.)))             ! default
         call showme(logical(l1,kind=selected_logical_kind(8)))
         call showme(logical(l1,kind=selected_logical_kind(16)))
         call showme(logical(l1,kind=selected_logical_kind(32)))
         call showme(logical(l1,kind=selected_logical_kind(64)))
         call showme(smallest_storage(1,1))

      contains
      subroutine showme(val)
      ! @(#) showme(3f) - display type and kind of intrinsic value
      ! this is an example of how to accept any logical kind as a parameter,
      ! but this is often done with a generic procedure.
      class(*),intent(in) :: val
         select type(val)
            type is (logical(kind=logical8))
                  write(*,'("   logical(kind=1) ",l1,a,i0)') val, &
                  & ' storage=',storage_size(val)
            type is (logical(kind=logical16))
                  write(*,'("   logical(kind=2) ",l1,a,i0)') val, &
                  & ' storage=',storage_size(val)
            type is (logical(kind=logical32))
                  write(*,'("   logical(kind=4) ",l1,a,i0)') val, &
                  & ' storage=',storage_size(val)
            type is (logical(kind=logical64))
                  write(*,'("   logical(kind=8) ",l1,a,i0)') val, &
                  & ' storage=',storage_size(val)
            class default
            stop 'crud. showme() does not know about this type'
         end select
      end subroutine showme
      end program demo_logical
