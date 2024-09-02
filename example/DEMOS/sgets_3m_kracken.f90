     program demo_sgets
     use M_kracken, only : kracken, sgets
     implicit none
     character(len=:),allocatable :: strings(:)
     integer :: i
        call kracken('cmd',' -string    This   is  a sentence ')
        strings= sgets("cmd_string")            ! get -strings words
        print *, "string=",('['//trim(strings(i))//']',i=1,size(strings))
        print *, "len= ",len(strings)
        print *, "size=",size(strings)
     end program demo_sgets
