     program demo_add_border
     use M_unicode, only : ut=>unicode_type, assignment(=)
     use M_unicode, only : character, add_border, trim
     implicit none
     type(ut),allocatable       :: textout(:)
     type(ut)                   :: uline
     type(ut),allocatable       :: uparagraph(:)
     character(len=*),parameter :: paragraph(*)=[character(len=10) :: &
     &'one',&
     &'two',&
     &'three',&
     &'four']

        ! show original text
        textout=paragraph
        call write_text()

        ! character array
        textout=add_border(paragraph)
        call write_text()

        ! ragged string array
        uparagraph=paragraph
        uparagraph=trim(uparagraph)
        textout=add_border(uparagraph)
        call write_text()

        ! add another border and specify style
        textout=add_border(textout,style='DOUBLE')
        call write_text()

        ! scalar character
        textout=add_border("To be or not to be!",style='DOUBLE')
        call write_text()

        ! scalar string
        uline="To be or not to be!"
        textout=add_border(uline,style='light')
        call write_text()

     contains
     subroutine write_text()
     integer :: i
        write(*,'(*(a:))',advance='no') &
        & (trim(textout(i)%character()), &
        & new_line('a'), &
        & i=1,size(textout))
     end subroutine write_text

     end program demo_add_border
