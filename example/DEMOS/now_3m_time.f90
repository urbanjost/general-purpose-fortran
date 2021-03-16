           program demo_now
           use M_time, only : now
           implicit none
              write(*,*)now("The current date is %w, %l %d, %Y %H:%m:%s %N")
              call showme()
           contains
           subroutine showme() ! see all formatting options
           use M_time, only : fmtdate_usage
              call fmtdate_usage() ! see all formatting options
           end subroutine
           end program demo_now
