     program demo_decode_base64
     use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
     use M_strings, only : switch, encode_base64, decode_base64
     implicit none
     integer                      :: i
     character(len=1),parameter   :: nl=new_line('a')
     character(len=1),allocatable :: textin(:), textout(:)
     character(len=*),parameter   :: data(*)=[ &
     'This is some sample data          ',  &
     'To encode. Should make it long    ',  &
     'enough to generate multiple lines ',  &
     'of output so can check line wrap  ',  &
     'functionality as well.            '   &
     ]
     ! make a file-like byte stream by trimming lines and adding newlines
        textin=[(switch(trim(data(i))),new_line('a'),i=1,size(data))]
        write(*,'(*(a))')'input:',nl,textin
     !
        textout=encode_base64(textin,width=50)
        write(*,'(*(a))')'result:',nl, textout
     !
        write(*,'(*(a))')'decode result:',nl, decode_base64(textout)
     !
     end program demo_decode_base64
