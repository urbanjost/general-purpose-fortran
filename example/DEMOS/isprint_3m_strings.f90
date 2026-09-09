     program demo_isprint
     use M_strings, only : isprint
     implicit none
     integer                    :: i
     character(len=*),parameter :: g='(*(g0,1x))'
     character(len=*),parameter :: c='(40(g0))'
     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]

        print *
        print g, 'basics'
        print g, isprint('a'),isprint(achar(9))
        print *
        print g, 'elemental'
        print g, isprint(['a','b',char(8),'c','d',char(10)])
        print *
        print g, 'print all the printable characters'
        print c, pack( string, isprint(string) )
        print *
        print g, 'return false if any character is not printable'
        print g, isprint('abcd')
        print g, isprint('ab'//char(0)//'cd')

     end program demo_isprint
