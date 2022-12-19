     program demo_matching_delimiter
        use M_strings, only : matching_delimiter
        implicit none
        character(len=128)  :: str
        integer             :: imatch

        str=' a [[[[b] and ] then ] finally ]'
        write(*,*)'string=',str
        call matching_delimiter(str,1,imatch)
        write(*,*)'location=',imatch
        call matching_delimiter(str,4,imatch)
        write(*,*)'location=',imatch
        call matching_delimiter(str,5,imatch)
        write(*,*)'location=',imatch
        call matching_delimiter(str,6,imatch)
        write(*,*)'location=',imatch
        call matching_delimiter(str,7,imatch)
        write(*,*)'location=',imatch
        call matching_delimiter(str,32,imatch)
        write(*,*)'location=',imatch

     end program demo_matching_delimiter
