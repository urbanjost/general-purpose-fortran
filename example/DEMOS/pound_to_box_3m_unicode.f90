     program demo_pound_to_box
     use M_unicode, only : ut=>unicode_type
     use M_unicode, only : operator(//)
     use M_unicode, only : assignment(=)
     use M_unicode, only : character, pound_to_box
     implicit none
     type(ut),allocatable       :: textout(:)
     character(len=*),parameter :: text(*)=[character(len=80) :: &
     '############################################', &
     '#abcdefg# What about #        #       #    #', &
     '#hijklmn# this text? #        #       ######', &
     '###############################       #    #', &
     '#              #     #        #       ######', &
     '#              #     #        #       #    #', &
     '############################################', &
     '', &
     '   ###################################', &
     '   # WARNING, WARNING, Will Robinson #', &
     '   ###################################']
        textout=text
        call write_text()
        textout=pound_to_box(text)
        call write_text()
        textout=pound_to_box(text,style='light')
        call write_text()
        textout=pound_to_box(text,style='double')
        call write_text()

     contains
     subroutine write_text()
     integer :: i
        write(*,'(*(a:))',advance='no') &
        & (trim(textout(i)%character()), &
        & new_line('a'), &
        & i=1,size(textout))
     end subroutine write_text

     end program demo_pound_to_box
