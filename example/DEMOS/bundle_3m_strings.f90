     program demo_bundle
     use M_strings, only: bundle
     implicit none
     character(len=*),parameter :: fmt= "(*('""',a,'""':,',',1x))"
     character(len=:),allocatable :: array(:)
        print fmt, bundle("one")
        print fmt, bundle("one","two")
        print fmt, bundle("one","two","three")
        array=bundle("one","two","three","four","five","six","seven")
        write(*,'(*(g0))')'size=',size(array),',len=',len(array)
        write(*,'("[",a,"]")')array
     end program demo_bundle
