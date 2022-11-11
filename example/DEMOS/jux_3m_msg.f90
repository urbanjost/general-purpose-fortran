     program demo_jux
     use M_msg, only : jux
     character(len=:),allocatable :: text
     text=jux('it is',.true.,'I like to add some numbers like',10,'and',30.4,'and',(30.0,40.0),'to the message')
     print *, text
     end program demo_jux
