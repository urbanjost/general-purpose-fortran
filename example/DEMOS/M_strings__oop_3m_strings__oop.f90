      program demo_M_strings__oop
      !
      ! This is an example using the object-oriented class/type model
      ! defined in M_strings__oop
      !
      ! This is essentially the same functionality as the procedures
      ! combined with several Fortran intrinsics and overloaded operators
      !
      use M_strings__oop,only : string, p
      implicit none
      TYPE(string) :: str1, str2, str3, str4

        write(*,*)'Call methods of type(STRING)'

        ! define TYPE(STRING) with constructor
        str2=string('   This  is  a  String!       ')
        str4=string(' a  String ')

        write(*,101)'str2%str is ................ ', &
         & str2%str                      ! print string member of type
        write(*,202)'len ........................ ', &
         & str2%len()                    ! same as intrinsic LEN()
        write(*,202)'len_trim ................... ', &
         & str2%len_trim()               ! same as intrinsic LEN_TRIM()
        write(*,202)'index("is")................. ', &
         & str2%index("is")              ! same as intrinsic INDEX()
        write(*,202)'index("is",back=.T.) ....... ', &
         & str2%index("is",back=.TRUE.)  ! same as intrinsic INDEX()
        write(*,101)'upper ...................... ', &
         & p(str2%upper())               ! call upper()
        write(*,101)'lower ...................... ', &
         & p(str2%lower())               ! call lower()
        write(*,101)'reverse .................... ', &
         & p(str2%reverse())             ! call reverse()
        write(*,101)'adjustl .................... ', &
         & p(str2%adjustl())             ! same as intrinsic ADJUSTL()
        write(*,101)'adjustr .................... ', &
         & p(str2%adjustr())             ! same as intrinsic ADJUSTR()
        write(*,101)'adjustc .................... ', &
         & p(str2%adjustc())             ! center string in current string length
        write(*,101)'adjustc(40) ................ ', &
         & p(str2%adjustc(40))           ! center string in string length of NN
        write(*,101)'lenset(40) ................. ', &
         & p(str2%lenset(40))            ! call pad() to force minimal string length
        write(*,101)'trim ....................... ', &
         & p(str2%trim())                ! same as intrinsic TRIM()
        write(*,101)'crop ....................... ', &
         & p(str2%crop())                ! trim leading and trailing spaces
        write(*,101)'substitute("This","Here") .. ', &
         & p(str2%substitute("This","Here")) ! call SUBSTITUTE()
        write(*,101)'compact .................... ', &
         & p(str2%compact())                 ! call COMPACT()
        write(*,101)'compact("") ................ ', &
         & p(str2%compact(""))
        write(*,101)'compact(":") ............... ', &
         & p(str2%compact(":"))
        ! calls M_strings procedure TRANSLITERATE()
        write(*,101)'transliterate("aei","VWX") . ', &
         & p(str2%transliterate("aei","VWX"))
        write(*,101)'transliterate("aeiou"," ") . ', &
         & p(str2%transliterate("aeiou"," "))
        write(*,101)'transliterate("aeiou","") .. ', &
         & p(str2%transliterate("aeiou",""))
        write(*,101)'transliterate(" aeiou","") . ', &
         & p(str2%transliterate(" aeiou",""))
        write(*,404)'chars .................... . ', &
         & str4%chars()                   ! call SWITCH()

        str2%str='\t\tSome tabs\t   x\bX '
        write(*,101)'str2%str ................... ',str2%str
        write(*,101)'expand ..................... ', &
         & p(str2%expand())
        str2=str2%expand()
        write(*,101)'notabs ..................... ', &
         & p(str2%notabs())               ! calls NOTABS()
        write(*,101)'noesc ...................... ', &
         & p(str2%noesc())                ! calls NOESC()

        write(*,*)repeat('=',68)
        write(*,*)'Casting to numeric variables'
        str3=string('   12.345678901234567e1        ')
        write(*,101)'str3%str ................... ',str3%str
        ! calls to M_strings procedure STRING_TO_VALUE()
        write(*,*)'int  ....................... ', str3%int()
        write(*,*)'nint ....................... ', str3%nint()
        write(*,*)'real ....................... ', str3%real()
        write(*,*)'dble ....................... ', str3%dble()

        write(*,*)repeat('=',68)
        write(*,*)'Matching simple globbing patterns'
        str3=string('   12.345678901234567e1        ')
        str3=string('Four score and seven years ago')
        write(*,101)'str3%str ................... ',str3%str
        ! %match calls M_strings procedure GLOB
        write(*,*)'match("Fo*") ............... ', str3%match("Fo*")
        write(*,*)'match("and") ............... ', str3%match("and")
        write(*,*)'match("*and*") ............. ', str3%match("*and*")

        101 format(1x,a,"[",a,"]")
        202 format(1x,a,i0)
        303 format(1x,*(l3))
        404 format(1x,a,*("[",a1,"]":))

        write(*,*)repeat('=',68)
        write(*,*)'OVERLOADED OPERATORS (add and subtract,return TYPE(STRING))'
        str1%str='123.456'
        str2%str='AaBbCcDdEeFfGgHhIi AaBbCcDdEeFfGgHhIi'
        write(*,101)'str1%str ................... ',str1%str
        write(*,101)'str2%str ................... ',str2%str
        write(*,*)'str1 + str2 ................ ',p(str1 + str2)
        ! a string that looks like a numeric value can have a value added
        write(*,*)'str1 + 20000 ............... ',p(str1 +20000)
        write(*,*)'str1 - 20.0 ................ ',p(str1 -20.0)
        write(*,*)'str2 - "Aa" (removes ALL) .. ',p(str2 - 'Aa')

        write(*,*)repeat('=',68)
        write(*,*)'OVERLOADED OPERATORS (multiply,return TYPE(STRING))'
        str1%str='AaBbCcDdEeFfGgHhIi'
        write(*,101)'str1%str ................... ',str1%str
        write(*,*)'str1 * 2 ................... ',p(str1 * 2)

        write(*,*)repeat('=',68)
        write(*,*)'OVERLOADED OPERATORS (//,return TYPE(STRING))'
        str1%str='String one:'
        str2%str='String two:'
        write(*,101)'str1%str ................... ',str1%str
        write(*,101)'str2%str ................... ',str2%str
        write(*,*)'str1 // str2 ................ ',p(str1 // str2)
        ! numeric values are converted to strings
        write(*,*)'str1 // 20000 ............... ',p(str1 // 20000)
        write(*,*)'str1 // 20.0 ................ ',p(str1 // 20.0)

        write(*,*)repeat('=',68)
        write(*,*)'OVERLOADED OPERATORS (logical comparisons,return logical)'
        ! NOTE: comparisons are performed on the character variable members
        !       of the type(string)
        str1%str='abcdefghij'
        str2%str='klmnopqrst'
        write(*,101)'str1%str ................... ',str1%str
        write(*,101)'str2%str ................... ',str2%str
        write(*,*)': EQ LT GT LE GE NE'
        write(*,*)'compare str1 to str1'
        write(*,303)str1 == str1  ,str1 < str1  ,str1 > str1  ,str1 <= str1 &
                   & ,str1 >= str1  ,str1 /= str1
        write(*,*)'compare str1 to str2'
        write(*,303)str1 == str2  ,str1 < str2  ,str1 > str2  ,str1 <= str2 &
                   & ,str1 >= str2  ,str1 /= str2
        write(*,*)'compare str2 to str1'
        write(*,303)str2 == str1  ,str2 < str1  ,str2 > str1  ,str2 <= str1 &
                   & ,str2 >= str1  ,str2 /= str1

        write(*,*)repeat('=',68)

      end program demo_M_strings__oop
