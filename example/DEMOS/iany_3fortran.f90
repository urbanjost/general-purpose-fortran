      program demo_iany
      use, intrinsic :: iso_fortran_env, only : compiler_version
         use, intrinsic :: iso_fortran_env, only : integer_kinds, &
         & int8, int16, int32, int64
         implicit none
         logical,parameter :: T=.true., F=.false.
         integer(kind=int8) :: a(3)
         integer(kind=int8) :: answer

         print '(2a)', 'This file was compiled by ', compiler_version()

         ! set some values to exercise with
         a(1) = int(b'00100100',int8)
         a(2) = int(b'01101010',int8)
         a(3) = int(b'10101010',int8)
         answer=int(b'11101110',int8)
         ! if any bit on in any element of A it should be on in answer

         ! basic call
         print '("A=")'
         print '("  ",1x,b8.8)', a
         print '("IANY(A)=",1x,b8.8,/)', iany(a)
         print '("is it the expected value? ",1x,l1,/)', iany(a)==answer

         ! select values with a mask
         write(*,*)'IANY(A) with a mask'
         write(*,*)'these values should be equivalent'
         print '(1x,b8.8)', iany(a,mask=[T,F,T])
         print '(1x,b8.8)', iany(a,[T,F,T])
         print '(1x,b8.8)', iany(a,dim=1,mask=[T,F,T])

         print *
         print '("the answer should match",1x,b8.8)', iany([a(1),a(3)])
         write(*,*)'does it?'
         write(*,*)iany(a,[T,F,T]) == iany([a(1),a(3)])
         write(*,*)iany(a,[T,F,T]) == int(b'10101110',int8)

      end program demo_iany
