          program demo_selected_char_kind
            use iso_fortran_env
            implicit none
            integer, parameter :: ascii = selected_char_kind ("ascii")
            integer, parameter :: ucs4  = selected_char_kind ('ISO_10646')

            character(kind=ascii, len=26) :: alphabet
            character(kind=ucs4,  len=30) :: hello_world

            alphabet = ascii_"abcdefghijklmnopqrstuvwxyz"
            hello_world = ucs4_'Hello World and Ni Hao -- ' &
                          // char (int (z'4F60'), ucs4)     &
                          // char (int (z'597D'), ucs4)

            write (*,*) alphabet

            open (output_unit, encoding='UTF-8')
            write (*,*) trim (hello_world)
          end program demo_selected_char_kind
