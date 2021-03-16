           program demo_o2d
           use M_time, only : o2d,fmtdate
           implicit none
           integer :: year
              do year=2004,2008
                 write(*,*)&
                 & '100th day of ',year,' is ',fmtdate(o2d(100,year))
              enddo
              write(*,*)'100th day of this year is ',fmtdate(o2d(100))
           end program demo_o2d
