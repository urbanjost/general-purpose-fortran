     program demo_cc
     use M_strings, only: cc
     implicit none
        print "(*('""',a,'""':,',',1x))", cc("one")
        print "(*('""',a,'""':,',',1x))", cc("one","two")
        print "(*('""',a,'""':,',',1x))", cc("one","two","three")
        print "(*('""',a,'""':,',',1x))", cc("one","two","three",&
                & "four","five","six","seven","eight","nine","ten")
     end program demo_cc
