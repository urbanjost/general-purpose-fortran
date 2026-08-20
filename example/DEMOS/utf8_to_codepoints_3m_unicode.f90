     program demo_utf8_to_codepoints
     use m_unicode, only : utf8_to_codepoints
     implicit none
     character(len=*),parameter   :: string ='Noho me ka hau’oli' !(Be happy)
     character(len=1),allocatable :: bytes(:)
     character(len=*),parameter   :: solid='(*(g0))'
     character(len=*),parameter   :: space='(*(g0,1x))'
     character(len=*),parameter   :: z='(a,*(z0,1x))'
     integer,allocatable          :: codepoints(:)
     integer                      :: nerr
     integer                      :: i
     ! BASIC USAGE: SCALAR CHARACTER VARIABLE
       write(*,solid)'STRING:',string
       call utf8_to_codepoints(string,codepoints,nerr)
       write(*,space)'CODEPOINTS:', codepoints
       write(*,z)'HEXADECIMAL CODEPOINTS:', codepoints
     !
       write(*,space)'How long is this string in glyphs? '
       write(*,space)size(codepoints)
       write(*,space)'How long is this string in bytes? '
       write(*,space)len(string)
     !
     ! BASIC USAGE: ARRAY OF BYTES
       bytes=[(string(i:i),i=1,len(string))]
       write(*,solid)'STRING:',bytes
       call utf8_to_codepoints(bytes,codepoints,nerr)
       write(*,space)'CODEPOINTS:', codepoints
       write(*,z)'HEXADECIMAL CODEPOINTS:', codepoints
     !
       write(*,space)'How long is this string in glyphs? '
       write(*,space)size(codepoints)
       write(*,space)'How long is this string in bytes? '
       write(*,space)size(bytes)
     !
     end program demo_utf8_to_codepoints
