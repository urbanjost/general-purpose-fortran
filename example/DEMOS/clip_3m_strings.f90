     program demo_clip
     use M_strings, only: clip
     implicit none
     character(len=20) ::  untrimmed = '   ABCDEFG abcdefg  '
        write(*,*) 'untrimmed string=[',untrimmed,']'
        write(*,*) 'clipped string=[',clip(untrimmed),']'
        ! which is equivalent to
        write(*,*) 'clipped string=[',trim(adjustl(untrimmed)),']'
        write(*,*)'non-space:'
        write(*,*) '['//clip('----single-character----',set='-')//']'
        write(*,*) '['//clip('  ... . .multi-character . ...',set='. ')//']'
     end program demo_clip
