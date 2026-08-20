     program demo_codepoints_to_utf8
     use m_unicode, only : codepoints_to_utf8
     implicit none
     !'Noho me ka hau’oli' !(Be happy)
     integer,parameter :: codepoints(*)=[ &
        & 78,111,104,111,&
        & 32,109,101, &
        & 32,107,97, &
        & 32,104,97,117,8217,111,108,105]
     character(len=:),allocatable :: string
     character(len=1),allocatable :: bytes(:)
     character(len=*),parameter   :: solid='(*(g0))'
     character(len=*),parameter   :: space='(*(g0,1x))'
     character(len=*),parameter   :: z='(a,*(z0,1x))'
     integer                      :: nerr
     ! BASIC USAGE: SCALAR CHARACTER VARIABLE
       write(*,space)'CODEPOINTS:', codepoints
       write(*,z)'HEXADECIMAL CODEPOINTS:', codepoints
       call codepoints_to_utf8(codepoints,string,nerr)
       write(*,solid)'STRING:',string
     !
       write(*,space)'How long is this string in glyphs? '
       write(*,space)size(codepoints)
       write(*,space)'How long is this string in bytes? '
       write(*,space)len(string)
     !
     ! BASIC USAGE: ARRAY OF BYTES
       call codepoints_to_utf8(codepoints,bytes,nerr)
       write(*,solid)'STRING:',bytes
     !
       write(*,space)'How long is this string in glyphs? '
       write(*,space)size(codepoints)
       write(*,space)'How long is this string in bytes? '
       write(*,space)size(bytes)
     !
     end program demo_codepoints_to_utf8
