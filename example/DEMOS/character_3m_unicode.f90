     program demo_character
     use M_unicode, only : ut=>unicode_type, ch=>character, trim, len, pad
     use M_unicode, only : write(formatted), assignment(=)
     type(ut)             :: ustr
     type(ut),allocatable :: array(:)
     integer              :: i
     character(len=*),parameter :: all='(*(g0))'

        ustr=[949, 8021, 961, 951, 954, 945, 33] ! eureka in codepoints
        ! when doing I/O using DT might be the most intuitive
        ! but sometimes converting to intrinsic character variables
        ! is preferred
        write (*,all)  ch(ustr)      ! convert to CHARACTER variable
        write (*,all)  ustr%character()      ! convert to CHARACTER variable
        ! you can select a range of glyphs
        write (*,all)  ustr%character(3,4) ! similar to LINE(3:4) for
                                           ! CHARACTER variables
        ! and even reverse a string
        write (*,all)  ustr%character(len(ustr),1,-1) ! reverse string
        ! note that OOP syntax provides a few other options
        write (*,all)  ustr%byte() ! convert to CHARACTER(LEN=1) type

        ! arrays
        !
        ! using this syntax make sure to make the LEN value large enough
        ! that glyphs can take up to four bytes
        array= ut([ character(len=60) :: &
        'Confucius never claimed to be a prophet, '       ,&
        'but I think he foresaw AI! He said '             ,&
        ''                                                ,&
        ' "学而不思则罔，思而不学则殆"'              ,&
        'or'                                              ,&
        ' (xué ér bù sī zé wǎng, sī ér bù xué zé dài),'   ,&
        'which is also'                                   ,&
        ' "To learn without thinking is to be lost, '     ,&
        ' to think without learning is to be in danger".'])
        !
        write(*,'(*(:,"[",g0,"]",/))')ch(array)
        ! all elements will be the same length in bytes but not necessarily
        !in glyphs
        write(*,'(a,*(i0,1x))')'all elements the same length in BYTES:', &
                & len(ch(array))
        write(*,'(a,*(i0,1x))')'lengths (in glyphs):',len(array)
        array=trim(array)
        write(*,'(a,*(i0,1x))')'lengths after trimming (in glyphs):', &
                & len(array)
        write(*,'(:*(:,"[",g0,"]",/))')ch(array)
        write(*,*)
        !
        ! using this syntax the elements will be of different lengths
        array= [ &
        ut('Confucius never claimed to be a prophet,')      ,&
        ut('but I think he foresaw AI! He said')            ,&
        ut('')                                              ,&
        ut(' "学而不思则罔，思而不学则殆"')                       ,&
        ut('or')                                            ,&
        ut(' (xué ér bù sī zé wǎng, sī ér bù xué zé dài),') ,&
        ut('which is also')                                 ,&
        ut(' "To learn without thinking is to be lost,')    ,&
        ut(' to think without learning is to be in danger".')]
        ! but using the CHARACTER function will still make them the same
        ! length in bytes so you might want to print them individually
        ! for certain effects, subject to font properties such as varying
        ! glyph widths.
        write(*,'(*("[",g0,"]",/))')(ch(array(i)),i=1,size(array))
        write(*,'(*("[",g0,"]",/))')(ch(pad(array(i),60)),i=1,size(array))
        !
     end program demo_character
