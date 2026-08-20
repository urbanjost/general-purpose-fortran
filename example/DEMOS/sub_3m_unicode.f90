      program demo_sub
       use M_unicode, only : sub, assignment(=)
       use M_unicode, only : len
       use M_unicode, only : ut=> unicode_type
       implicit none
       type(ut)                   :: string
       type(ut)                   :: piece
          !
          string='abcdefghij'
          !
          piece=sub(string,3,5)
          call printme('selected range:')
          piece=sub(string,6)
          call printme('from character to end:')
          piece=sub(string,5,5)
          call printme('single character:')
          piece=sub(string,step=-1)
          call printme('reverse string:')
       contains
       subroutine printme(label)
       character(len=*),intent(in) :: label
          write(*,'(a,"[",g0,"]",/)') label, piece%character()
       end subroutine printme
       end program demo_sub
