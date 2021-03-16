           program demo_fmtdate
           use M_time, only : fmtdate
           implicit none
           integer :: dat(8)
              call date_and_time(values=dat)
              write(*,*)fmtdate(dat,"current date: %w, %l %d, %Y %H:%m:%s %N")
              call showme()
           contains
           subroutine showme()
              use M_time, only : fmtdate_usage
              call fmtdate_usage() ! see all formatting options
           end subroutine showme
           end program demo_fmtdate
