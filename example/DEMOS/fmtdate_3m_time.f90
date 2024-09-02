      program demo_fmtdate
      use M_time, only : fmtdate
      implicit none
      integer :: dat(8)
         call date_and_time(values=dat)
         write(*,*)fmtdate(dat,"current date: %w, %l %d, %Y %H:%m:%s %N")
      end program demo_fmtdate
