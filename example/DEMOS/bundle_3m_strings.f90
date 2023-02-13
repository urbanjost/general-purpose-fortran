     program demo_bundle
     use M_strings, only: bundle
     implicit none
        print "(*('""',a,'""':,',',1x))", bundle("one")
        print "(*('""',a,'""':,',',1x))", bundle("one","two")
        print "(*('""',a,'""':,',',1x))", bundle("one","two","three")
        print "(*('""',a,'""':,',',1x))", bundle("one","two","three",&
                & "four","five","six","seven")
     end program demo_bundle
