      program demo_attr_mode
      use M_framework__attr, only : attr, attr_mode
      implicit none
      character(len=:),allocatable :: lines(:)
      character(len=:),allocatable :: outlines(:)
      integer :: i
         lines=[character(len=110):: &
         &'<M><y>',&
         &'<M><y>  Suffice it to say that black and white are also colors',&
         &'<M><y>  for their simultaneous contrast is as striking as that ',&
         &'<M><y>  of green and red, for instance. &
         & --- <y><bo>Vincent van Gogh</bo></y>',&
         &' ']

         outlines=attr(lines,chars=57)
         write(*,'(a)')(trim(outlines(i)),i=1,size(outlines))

         call attr_mode(manner='plain') ! write as plain text
         write(*,'(a)')attr(lines)

         call attr_mode(manner='raw')   ! write as-is
         write(*,'(a)')attr(lines)

         call attr_mode(manner='ansi')  ! return to default mode

      end program demo_attr_mode
