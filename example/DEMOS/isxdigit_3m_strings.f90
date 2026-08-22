     program demo_isxdigit
     use M_strings, only : isxdigit
     implicit none
     integer                    :: i
     character(len=*),parameter :: g='(*(g0,1x))'
     character(len=*),parameter :: c='(40(g0))'
     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]

        print g, 'basics'
        print g, isxdigit('a'),isxdigit(g)
        print *
        print g, 'elemental'
        print g, isxdigit(['a','b',char(8),'c','d',char(10)])
        print *
        print g, 'print all the hexadecimal digit characters'
        print c, pack( string, isxdigit(string) )
        print *
        print g, 'strings return false if any character is not in set'
        print g, isxdigit('abcd')
        print g, isxdigit('ab'//char(0)//'cd')

     end program demo_isxdigit
