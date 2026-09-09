      program demo_selected_int_kind
      use iso_fortran_env, only: output_unit, INTEGER_KINDS
      use,intrinsic :: iso_fortran_env, only : compiler_version
      implicit none
      character(len=*),parameter :: all='(*(g0))'
      integer,parameter :: k5 = selected_int_kind(5)
      integer,parameter :: k15 = selected_int_kind(15)
      integer           :: i, ii
      integer(kind=k5)  :: i5
      integer(kind=k15) :: i15
         ! write a program that can print attributes about each available kind
         print all,'program kinds'
         print all, &
            '! This file was written by ', compiler_version()
         do i=1,size(INTEGER_KINDS)
            ii=integer_kinds(i)
            print all,'integer,parameter :: i',ii,'=',ii
         enddo
         do i=1,size(INTEGER_KINDS)
            ii=integer_kinds(i)
            print all, &
            'write(*,*)"huge(0_i', &
            ii, &
            ')=",huge(0_i', &
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
