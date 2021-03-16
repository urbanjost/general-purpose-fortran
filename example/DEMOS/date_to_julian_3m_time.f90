           program demo_date_to_julian
           use M_time, only : date_to_julian,realtime
           implicit none
           integer             :: dat(8)
           real(kind=realtime) :: juliandate
           integer             :: ierr
              ! generate DAT array
              call date_and_time(values=dat)
              ! show DAT array
              write(*,'(" Today is:",*(i0:,":"))')dat
              ! convert DAT to Julian Date
              call date_to_julian(dat,juliandate,ierr)
              write(*,*)'Julian Date is ',juliandate
              write(*,*)'ierr is ',ierr
           end program demo_date_to_julian
