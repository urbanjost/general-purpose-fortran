          program demo_crop
          use M_strings, only: crop
          implicit none
          character(len=20) ::  untrimmed = '   ABCDEFG abcdefg  '
             write(*,*) 'untrimmed string=[',untrimmed,']'
             write(*,*) 'cropped string=[',crop(untrimmed),']'
          end program demo_crop
