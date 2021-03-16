             program demo_call_draw
                use M_drawplus, only : call_draw
                use M_io, only : read_line
                implicit none
                character(len=:),allocatable :: line
                logical                      :: found
                integer                      :: iend
                INFINITE: do while (read_line(line)==0)
                   line=adjustl(line)
                   iend=scan(line,' #;')-1
                   if(iend.le.0)iend=len_trim(line)
                   if(iend.ne.0)then
                      line=line//' '
                      call call_draw(line(:iend),line(iend+2:),found)
                      if(.not.found)then
                         write(*,*)'ERROR: ',line(:iend),'[',line(iend+1:),']',' not found'
                      endif
                   endif
                enddo INFINITE
             end program demo_call_draw
