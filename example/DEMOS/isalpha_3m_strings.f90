     program demo_isalpha
     use M_strings, only : isalpha
     implicit none
     integer                    :: i
     character(len=*),parameter :: g='(*(g0,1x))'
     character(len=*),parameter :: c='(40(g0))'
     character(len=1),parameter :: string(*)=[(char(i),i=0,127)]

        print g, 'basics'
        print g, isalpha('a'),isalpha(achar(9))
        print *
        print g, 'elemental'
        print g, isalpha(['a','b',char(8),'c','d',char(10)])
        print *
        print g, 'print all the alphanumeric characters'
        print c, pack( string, isalpha(string) )
        print *
        print g, 'return false if any character is not printable'
        print g,' using ISALPHA(3):'
        print g, isalpha('abcd')
        print g, isalpha('ab'//char(0)//'cd')

        ALTERNATIVE : block
        ! ALTERNATIVE using VERIFY(3)
        ! the Fortran intrinsic function VERIFY(3) returns a position just
        ! not a logical like C, which can be useful for complex comparisons
        character(len=*),parameter :: low='abcdefghijklmnopqrstuvwxyz'
        character(len=*),parameter :: up='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
           print g,' using VERIFY(3):'
           print g, verify('abcd', up//low) == 0
           print g, verify('ab'//char(0)//'cd', up//low) == 0
        endblock ALTERNATIVE

     end program demo_isalpha
