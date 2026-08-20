     program demo_ichar
     use M_unicode, only : assignment(=),ch=>character
     use M_unicode, only : ut=>unicode_type, write(formatted)
     use M_unicode, only : ichar, escape, len
     implicit none
     type(ut)             :: string
     type(ut),allocatable :: lets(:)
     integer,allocatable  :: ilets(:)
     integer              :: i
        !
        ! create a string containing multibyte characters
        string=[949, 8021, 961, 951, 954, 945, 33] ! eureka
        write(*,'(*(DT,1x,"(AKA. eureka!)"))')string
        !
        ! call ichar(3) on each glyph of the string to convert
        ! the string to an array of integer codepoints
        ilets=[(ichar(string%sub(i,i)),i=1,len(string))]
        write(*,'(*(z0,1x))')ilets
        !
        ! note that the %codepoint method is commonly used to
        ! convert a string to an integer array of codepoints
        write(*,'(*(z0,1x))')string%codepoint()

        ! elemental
        write(*,'("WRITING ISSUES:")')
        !
        ! define an array LETS with escape codes with one glyph per element
        lets=[ut('\U03B5'),ut('\U1F55'),ut('\U03C1'),ut('\U03B7'), &
            & ut('\U03BA'),ut('\U03B1'),ut('\U0021')]
        lets=escape(lets) ! convert escape codes to glyphs
        !
        ! look at issues with converting to CHARACTER for simple printing
        !
        write(*,'("each element is a single glyph ",*(g0,1x))')len(lets)
        !
        ! notice if you convert to an array of intrinsic CHARACTER type the
        ! strings are all the same length in bytes; but unicode characters
        ! can take various numbers of bytes
        write(*,'(*(g0,":"))')'CHARACTER array elements have same length',&
           & len(ch(lets))
        ! this will not appear correctly because all elements are padded to
        ! the same length in bytes
        write(*,'(*(a,":"))')ch(lets)
        ! one element at a time will retain the size of each element
        write(*,'(*(a,":"))')(ch(lets(i:i)),i=1,size(lets))
        !
        ! the FIRST LETTER of each element is converted to a codepoint so
        ! for the special case where each string element is a single glyph
        ! an elemental approach works
        write(*,'("ELEMENTAL:",*(z0,1x))')ichar(lets)

        ! OOPS
        write(*,'("OOPS:",*(z0,1x))')lets%ichar()
     end program demo_ichar
