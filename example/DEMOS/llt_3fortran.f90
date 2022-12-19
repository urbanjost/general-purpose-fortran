      program demo_llt
      implicit none
      integer :: i

         print *,'the ASCII collating sequence for printable characters'
         write(*,'(1x,19a)')(char(i),i=32,126) ! ASCII order

        ! basics
         print *,'case matters'
         write(*,*) llt('abc','ABC')           ! [F] lowercase is > uppercase
         write(*,*) llt('abc','abc  ')         ! [F] trailing spaces
         ! If both strings are of zero length the result is false.
         write(*,*) llt('','')                 ! [F]
         write(*,*) llt('','a')                ! [T] the null string is padded
         write(*,*) llt('a','')                ! [F]
         print *,'elemental'
         write(*,*) llt('abc',['abc','123'])   ! [F F]  scalar and array
         write(*,*) llt(['cba', '123'],'abc')  ! [F T]
         write(*,*) llt(['abc','123'],['cba','123']) ! [T F]  both arrays
      end program demo_llt
