          program demo_lle
          implicit none
          integer             :: i
             write(*,'(*(a))')(char(i),i=32,126)
               write(*,*) lle('abc','ABC')              ! F lowercase is > uppercase
               write(*,*) lle('abc','abc  ')            ! T trailing spaces
               ! If both strings are of zero length the result is true.
               write(*,*) lle('','')                    ! T
               write(*,*) lle('','a')                   ! T the null string is padded
               write(*,*) lle('a','')                   ! F
               write(*,*) lle('abc',['abc','123'])      ! [T,F] scalar and array
               write(*,*) lle(['cba', '123'],'abc')     ! [F,T]
               write(*,*) lle(['abc','123'],['cba','123']) ! [T,T] both arrays
          end program demo_lle
