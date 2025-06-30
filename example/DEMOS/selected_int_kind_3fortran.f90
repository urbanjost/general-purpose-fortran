      program demo_selected_int_kind
      use,intrinsic :: iso_fortran_env, only : integer_kinds
      use,intrinsic :: iso_fortran_env, only : compiler_version
      implicit none
      character(len=*),parameter :: all='(*(g0))'
      integer,parameter :: k5 = selected_int_kind(5)
      integer,parameter :: k15 = selected_int_kind(15)
      integer           :: i, ii
      integer(kind=k5)  :: i5
      integer(kind=k15) :: i15
         print all,'program kinds'
         print all, &
            '! This file was compiled by ', compiler_version()
         do i=1,size(INTEGER_KINDS)
            ii=integer_kinds(i)
            print all,'integer(kind=',ii,') :: i',ii
         enddo
         do i=1,size(INTEGER_KINDS)
            ii=integer_kinds(i)
            print all, &
            'write(*,*)"huge(i', &
            ii, &
            ')=",huge(i', &
            ii, &
            ')'

         enddo
         print all,'end program kinds'

         print *
         print *, huge(i5), huge(i15)
         ! the following inequalities are always true
         print *, huge(i5) >= 10_k5**5-1
         print *, huge(i15) >= 10_k15**15-1

      end program demo_selected_int_kind
