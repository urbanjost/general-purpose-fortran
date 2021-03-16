          program demo_transliterate

           use M_strings, only : transliterate
           implicit none
           character(len=80)   :: STRING

           STRING='aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ'
           write(*,'(a)') STRING

           ! convert a string to uppercase:
           write(*,*) TRANSLITERATE(STRING, &
           & 'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')

           ! change all miniscule letters to a colon (":"):
           write(*,*) TRANSLITERATE(STRING, &
           & 'abcdefghijklmnopqrstuvwxyz',':')

           ! delete all miniscule letters
           write(*,*) TRANSLITERATE(STRING, &
           & 'abcdefghijklmnopqrstuvwxyz','')

              end program demo_transliterate
