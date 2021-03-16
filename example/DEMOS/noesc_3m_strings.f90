          program demo_noesc

           use M_strings, only : noesc
           implicit none
           character(len=128) :: ascii
           character(len=128) :: cleared
           integer            :: i
           ! fill variable with base ASCII character set
           do i=1,128
              ascii(i:i)=char(i-1)
           enddo
           cleared=noesc(ascii)
           write(*,*)'characters and their ADE (ASCII Decimal Equivalent)'
           call ade(ascii)
           write(*,*)'Cleared of non-printable characters'
           call ade(cleared)
           write(*,*)'Cleared string:'
           write(*,*)cleared
           contains
             subroutine ade(string)
             implicit none
             ! the string to print
             character(len=*),intent(in) :: string
             ! number of characters in string to print
             integer :: ilen
             ! counter used to step thru string
             integer :: i
                ! get trimmed length of input string
                ilen=len_trim(string(:len(string)))

                ! replace lower unprintable characters with spaces
                write(*,101)(merge(string(i:i),' ',&
                & ichar(string(i:i)).ge.32         &
                & .and.                            &
                & ichar(string(i:i)).le.126)       &
                & ,i=1,ilen)

                ! print ADE value of character underneath it
                write(*,202)     (ichar(string(i:i))/100,    i=1,ilen)
                write(*,202)(mod( ichar(string(i:i)),100)/10,i=1,ilen)
                write(*,202)(mod((ichar(string(i:i))),10),   i=1,ilen)
             ! format for printing string characters
             101   format(*(a1:))
             ! format for printing ADE values
             202   format(*(i1:))
             end subroutine ade
           end program demo_noesc
