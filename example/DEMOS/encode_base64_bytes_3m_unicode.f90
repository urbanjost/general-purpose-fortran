       program demo_encode_base64_bytes
       use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
       use M_unicode, only : encode_base64_bytes, decode_base64_bytes
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
          textin=[(s2a(trim(data(i))),new_line('a'),i=1,size(data))]
          write(*,'(*(a))')'input:',nl,textin
       !
          textout=encode_base64_bytes(textin,width=50)
          write(*,'(*(a))')'result:',nl, textout
       !
          write(*,'(*(a))')'decode result:',nl, decode_base64_bytes(textout)
       !
       ! one way to encode non-byte data
          call other()
       contains
       subroutine other()
       real                         :: arr1(100)
       character(len=1),allocatable :: in(:)
       character(len=1),allocatable :: out(:)
       real,allocatable             :: arr2(:)
          ! fill a real array with some values
          arr1=[(sqrt(real(i)),i=1,size(arr1))]
          ! use TRANSFER() to convert data to bytes
          in=transfer(source=arr1,mold=['+'])
          ! encode the bytes
          out=encode_base64_bytes(in)
          ! decode the bytes
          out=decode_base64_bytes(out)
          ! store the bytes back into arr1
          arr2=transfer(source=out,mold=[0.0])
          write(*,'(*(g0,1x))') 'are arr1 and arr2 the same?',all(arr1.eq.arr2)
       end subroutine other
       pure function s2a(string)  RESULT (array)
       !$@(#) M_unicode::s2a(3fp): function to copy string to char array
       character(len=*),intent(in) :: string
       character(len=1)            :: array(len(string))
       integer                     :: i
          forall(i=1:len(string)) array(i) = string(i:i)
       !  array=transfer(string,array)
       !  array=transfer(string,mold='a',size=len(string))
       end function s2a
       end program demo_encode_base64_bytes
