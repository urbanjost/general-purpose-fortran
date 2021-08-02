           program demo_attr_mode
           use M_attr, only : attr, attr_mode
           implicit none
           character(len=:),allocatable :: lines(:)
           character(len=:),allocatable :: outlines(:)
           integer :: i
              lines=[character(len=110):: &
              '<B><y>',&
              '<B><y>  Suffice it to say that <W><e>black</e></W><B><y>&
              & and <E><w>white</w></E><B><y> are also colors',&
              '<B><y>  for their simultaneous contrast is as striking as that ',&
              '<B><y>  of <R><g>green</g></R><B><y> and <G><r>red</r></G><B><y>,&
              & for instance. --- <bo>Vincent van Gogh',&
              '<B><y>',&
              ' ']

              outlines=attr(lines,chars=57)
              write(*,'(a)')(trim(outlines(i)),i=1,size(outlines))

              call attr_mode(manner='plain') ! write as plain text
              write(*,'(a)')attr(lines)
              call attr_mode(manner='raw')   ! write as-is
              write(*,'(a)')attr(lines)

              call attr_mode(manner='ansi')  ! return to default mode
              outlines=attr(lines,chars=80)
              write(*,'(a)')(trim(outlines(i)),i=1,size(outlines))

           end program demo_attr_mode
