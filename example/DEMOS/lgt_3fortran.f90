        program demo_lgt
        implicit none
        integer :: i
           write(*,'(*(a))')(char(i),i=32,126)  ! ASCII order
           write(*,*) lgt('abc','ABC')          ! [T] lowercase is > uppercase
           write(*,*) lgt('abc','abc  ')        ! [F] trailing spaces
           ! If both strings are of zero length the result is false.
           write(*,*) lgt('','')                ! [F]
           write(*,*) lgt('','a')               ! [F] the null string is padded
           write(*,*) lgt('a','')               ! [T]
           write(*,*) lgt('abc',['abc','123'])  ! [F T]  scalar and array
           write(*,*) lgt(['cba', '123'],'abc') ! [T F]
           write(*,*) lgt(['abc','123'],['cba','123']) ! [F F]  both arrays
        end program demo_lgt
