      program demo_lge
      implicit none
      integer :: i
         print *,'the ASCII collating sequence for printable characters'
         write(*,'(1x,19a)')(char(i),i=32,126) ! ASCII order
         write(*,*) lge('abc','ABC')           ! [T] lowercase is > uppercase
         write(*,*) lge('abc','abc  ')         ! [T] trailing spaces
         ! If both strings are of zero length the result is true
         write(*,*) lge('','')                 ! [T]
         write(*,*) lge('','a')                ! [F] the null string is padded
         write(*,*) lge('a','')                ! [T]
         ! elemental
         write(*,*) lge('abc',['abc','123'])   ! [T T]  scalar and array
         write(*,*) lge(['cba', '123'],'abc')  ! [T F]
         write(*,*) lge(['abc','123'],['cba','123']) ! [F T]  both arrays
      end program demo_lge
