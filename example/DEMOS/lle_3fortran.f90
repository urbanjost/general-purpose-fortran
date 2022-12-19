      program demo_lle
      implicit none
      integer :: i
         print *,'the ASCII collating sequence for printable characters'
         write(*,'(1x,19a)')(char(i),i=32,126)
        ! basics

         print *,'case matters'
         write(*,*) lle('abc','ABC')          ! F lowercase is > uppercase

         print *,'a space is the lowest printable character'
         write(*,*) lle('abcd','abc')         ! F  d > space
         write(*,*) lle('abc','abcd')         ! T  space < d

         print *,'leading spaces matter, trailing spaces do not'
         write(*,*) lle('abc','abc  ')        ! T trailing spaces
         write(*,*) lle('abc',' abc')         ! F leading spaces are significant

         print *,'even null strings are padded and compared'
         ! If both strings are of zero length the result is true.
         write(*,*) lle('','')                ! T
         write(*,*) lle('','a')               ! T the null string is padded
         write(*,*) lle('a','')               ! F
         print *,'elemental'
         write(*,*) lle('abc',['abc','123'])  ! [T,F] scalar and array
         write(*,*) lle(['cba', '123'],'abc') ! [F,T]
         ! per the rules for elemental procedures arrays must be the same size
         write(*,*) lle(['abc','123'],['cba','123']) ! [T,T] both arrays
      end program demo_lle
