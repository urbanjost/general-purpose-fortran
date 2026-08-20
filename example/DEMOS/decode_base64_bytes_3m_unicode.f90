     program demo_decode_base64_bytes
     use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
     use M_unicode, only : encode_base64_bytes, decode_base64_bytes
     implicit none
     character(len=*),parameter   :: g='(*(g0))'
     character(len=*),parameter   :: nl=new_line('a')
     character(len=1),allocatable :: textin(:), textout(:)
     character(len=*),parameter   :: line= &
        & 'This is some sample data'            //nl// &
        & 'To encode. Should make it long'      //nl// &
        & 'enough to generate multiple lines'   //nl// &
        & 'of output so can check line wrap'    //nl// &
        & 'functionality as well.'              //nl
        ! convert variable to array of bytes for use as input
        textin=transfer(source=line,mold=['+'],size=len(line))
        write(*,g)'input:',nl,textin
        textout=encode_base64_bytes(textin,width=50)
        write(*,g)'encoded:',nl, textout
        write(*,g)'decode result:',nl, decode_base64_bytes(textout)
     end program demo_decode_base64_bytes
