      program demo_decode_base64
      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      use M_unicode, only : encode_base64, decode_base64
      use M_unicode, only : ut=>unicode_type, assignment(=), operator(//)
      use M_unicode, only : trim
      implicit none
      integer                    :: i
      character(len=1),parameter :: nl=new_line('a')
      type(ut),allocatable       :: textin(:), textout(:), textroundtrip(:)

      textin=[ &
       ut('This is some sample data'), &
       ut('To encode. Should make it long'), &
       ut('enough to generate multiple lines'), &
       ut('of output so can check line wrap'), &
       ut('functionality as well.') ]
      ! make a file-like byte stream by trimming lines and adding newlines
       textin=trim(textin)//nl
       write(*,'(*(a))')'input:',nl,(textin(i)%character(),i=1,size(textin))
      !
       textout=encode_base64(textin,width=50)
       write(*,'(*(a))')'result:',nl, textout(1)%character()

       textroundtrip=decode_base64(textout)
       write(*,'(*(a))')'decode result:',nl, textroundtrip(1)%character()

       write(*,*)'SHAPES=',shape(textin),shape(textout),shape(textroundtrip)

      end program demo_decode_base64
