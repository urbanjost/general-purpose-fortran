           program demo_M_strings_oop
           !
           ! This is an example using the object-oriented class/type model
           ! defined in M_strings_oop
           ! This is essentially the same functionality as the procedures
           ! combined with several Fortran intrinsics and overloaded operators
           !
           use M_strings_oop,only : string, p
           implicit none
           TYPE(string) :: str1
           TYPE(string) :: str2
           TYPE(string) :: str3
           TYPE(string) :: str4
           !==============================================================================
             write(*,*)'exercise the M_STRING_OOP module interface'
             ! draw a break line in the output
             write(*,*)repeat('=',78)
             write(*,*)'Call methods of type(STRING)'
             ! define TYPE(STRING) with constructor
             str2=string('   This  is  a  String!       ')
             str4=string(' a  String ')
             write(*,*)repeat('=',78)
             ! print members of type
             write(*,101)'str2%str is ................ ',str2%str
             ! same as intrinsic LEN()
             write(*,202)'len ........................ ',str2%len()
             ! same as intrinsic INDEX()
             write(*,202)'len_trim ................... ',str2%len_trim()
             ! same as intrinsic INDEX()
             write(*,202)'index("is")................. ',str2%index("is")
             ! same as intrinsic INDEX()
             write(*,202)'index("is",back=.T.) ....... ',str2%index("is",back=.TRUE.)
             ! output TYPE(STRING) with %str all uppercase
             write(*,101)'upper ...................... ',p(str2%upper())
             ! output TYPE(STRING) with %str all miniscule
             write(*,101)'lower ...................... ',p(str2%lower())
             ! output TYPE(STRING) with %str reversed
             write(*,101)'reverse .................... ',p(str2%reverse())
             ! same as intrinsic ADJUSTL()
             write(*,101)'adjustl .................... ',p(str2%adjustl())
             ! same as intrinsic ADJUSTR()
             write(*,101)'adjustr .................... ',p(str2%adjustr())
             ! center string in current string length
             write(*,101)'adjustc .................... ',p(str2%adjustc())
             ! center string in string length of NN
             write(*,101)'adjustc(49) ................ ',p(str2%adjustc(49))
             ! force %str to be NN characters long
             write(*,101)'lenset(49) ................. ',p(str2%lenset(49))
             ! same as intrinsic TRIM()
             write(*,101)'trim ....................... ',p(str2%trim())
             ! trim leading and trailing spaces
             write(*,101)'crop ....................... ',p(str2%crop())
             ! calls M_strings procedure SUBSTITUTE()
             write(*,101)'substitute("This","Here") .. ',p(str2%substitute("This","Here"))
             ! calls M_strings procedure COMPACT()
             write(*,101)'compact .................... ',p(str2%compact())
             write(*,101)'compact("") ................ ',p(str2%compact(""))
             write(*,101)'compact(":") ............... ',p(str2%compact(":"))
             ! calls M_strings procedure TRANSLITERATE()
             write(*,101)'transliterate("aei","VWX") . ',p(str2%transliterate("aei","VWX"))
             write(*,101)'transliterate("aeiou"," ") . ',p(str2%transliterate("aeiou"," "))
             write(*,101)'transliterate("aeiou","") .. ',p(str2%transliterate("aeiou",""))
             write(*,101)'transliterate(" aeiou","") . ',p(str2%transliterate(" aeiou",""))
             ! calls M_strings procedure SWITCH()
             write(*,404)'chars .................... . ',str4%chars()

             write(*,*)repeat('=',78)
             str2%str='\t\tSome tabs\t   x\bX '
             write(*,101)'str2%str ................... ',str2%str
             write(*,101)'expand ..................... ',p(str2%expand())
             str2=str2%expand()
             ! calls M_strings procedure NOTABS()
             write(*,101)'notabs ..................... ',p(str2%notabs())
             ! calls M_strings procedure NOESC()
             write(*,101)'noesc ...................... ',p(str2%noesc())

             write(*,*)repeat('=',78)
             write(*,*)'Casting to numeric variables'
             str3=string('   12.345678901234567e1        ')
             write(*,101)'str3%str ................... ',str3%str
             ! calls M_strings procedure STRING_TO_VALUE()
             write(*,*)'int  ....................... ', str3%int()
             ! calls M_strings procedure STRING_TO_VALUE()
             write(*,*)'real ....................... ', str3%real()
             ! calls M_strings procedure STRING_TO_VALUE()
             write(*,*)'dble ....................... ', str3%dble()

             write(*,*)repeat('=',78)
             write(*,*)'Matching simple globbing patterns'
             str3=string('   12.345678901234567e1        ')
             str3=string('Four score and seven years ago')
             write(*,101)'str3%str ................... ',str3%str
             ! calls M_strings procedure MATCHW
             write(*,*)'match("Fo*") ............... ', str3%match("Fo*")
             ! calls M_strings procedure MATCHW
             write(*,*)'match("and") ............... ', str3%match("and")
             ! calls M_strings procedure MATCHW
             write(*,*)'match("*and*") ............. ', str3%match("*and*")

             101 format(1x,a,"[",a,"]")
             202 format(1x,a,i0)
             303 format(1x,*(l3))
             404 format(1x,a,*("[",a1,"]":))

             write(*,*)repeat('=',78)
             write(*,*)'OVERLOADED OPERATORS (add and subtract,return TYPE(STRING))'
             str1%str='123.456'
             str2%str='AaBbCcDdEeFfGgHhIiJj AaBbCcDdEeFfGgHhIiJj'
             write(*,101)'str1%str ................... ',str1%str
             write(*,101)'str2%str ................... ',str2%str
             write(*,*)'str1 + str2 ................ ',p(str1 + str2)
             ! a string that looks like a numeric value can have a value added
             write(*,*)'str1 + 20000 ............... ',p(str1 +20000)
             write(*,*)'str1 - 20.0 ................ ',p(str1 -20.0)
             write(*,*)'str2 - "Aa" (removes ALL) .. ',p(str2 - 'Aa')

             write(*,*)repeat('=',78)
             write(*,*)'OVERLOADED OPERATORS (multiply,return TYPE(STRING))'
             str1%str='AaBbCcDdEeFfGgHhIiJj'
             write(*,101)'str1%str ................... ',str1%str
             write(*,*)'str1 * 3 ................... ',p(str1 * 3)

             write(*,*)repeat('=',78)
             write(*,*)'OVERLOADED OPERATORS (//,return TYPE(STRING))'
             str1%str='String one:'
             str2%str='String two:'
             write(*,101)'str1%str ................... ',str1%str
             write(*,101)'str2%str ................... ',str2%str
             write(*,*)'str1 // str2 ................ ',p(str1 // str2)
             ! numeric values are converted to strings
             write(*,*)'str1 // 20000 ............... ',p(str1 // 20000)
             write(*,*)'str1 // 20.0 ................ ',p(str1 // 20.0)

             write(*,*)repeat('=',78)
             write(*,*)'OVERLOADED OPERATORS (logical comparisons,return logical)'
             ! NOTE: comparisons are performed on the character variable members
             !       of the type(string)
             str1%str='abcdefghij'
             str2%str='klmnopqrst'
             write(*,101)'str1%str ................... ',str1%str
             write(*,101)'str2%str ................... ',str2%str
             write(*,*)': EQ LT GT LE GE NE'
             write(*,*)'compare str1 to str1'
             write(*,303)str1.eq.str1  ,str1.lt.str1  ,str1.gt.str1  ,str1.le.str1 &
                        & ,str1.ge.str1  ,str1.ne.str1
             write(*,*)'compare str1 to str2'
             write(*,303)str1.eq.str2  ,str1.lt.str2  ,str1.gt.str2  ,str1.le.str2 &
                        & ,str1.ge.str2  ,str1.ne.str2
             write(*,*)'compare str2 to str1'
             write(*,303)str2.eq.str1  ,str2.lt.str1  ,str2.gt.str1  ,str2.le.str1 &
                        & ,str2.ge.str1  ,str2.ne.str1

             write(*,*)repeat('=',78)

           end program demo_M_strings_oop
