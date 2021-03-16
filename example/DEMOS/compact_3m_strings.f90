          program demo_compact
           use M_strings, only : compact
           implicit none
           ! produces 'This is a test               '
           write(*,*)compact('  This     is      a     test  ')
           ! produces 'Thisisatest                  '
           write(*,*)compact('  This     is      a     test  ',char='')
           ! produces 'This:is:a:test               '
           write(*,*)compact('  This     is      a     test  ',char=':')
           ! note CHAR is used to replace the whitespace, but if CHAR is
           ! in the original string it is just copied
           write(*,*)compact('A  AA    A   AAAAA',char='A')
           ! produces (original A characters are left as-is) 'AAAAAAAAAAAA'
           ! not 'A'
          end program demo_compact
