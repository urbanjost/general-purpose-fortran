      program demo_date_to_bas
      use M_time, only : date_to_bas, realtime, BAStime
      use M_time, only : date_to_julian
      implicit none
      integer                    :: dat(8)
      type(BAStime)              :: bas
      real(kind=realtime)        :: juliandate
      integer                    :: ierr
      character(len=*),parameter :: g='(*(g0,1x))'
         !
         write(*,g)'date_to_bas:'
         ! generate DAT array
         call date_and_time(values=dat)
         !
         ! show DAT array
         write(*,'("Today is:",*(i0:,":"))')dat
         !
         ! convert DAT to Julian
         call date_to_julian(dat,juliandate,ierr)
         ! show as Modified Julian Date
         write(*,g) 'Expecting Modified Julian Date:', &
         & juliandate - 2400000.5_realtime
         !
         ! convert DAT to BAS
         call date_to_bas(dat,bas,ierr)
         write(*,g)'Baseday and Seconds is ', bas
         write(*,g)'converted to Modified Julian Date:', &
         & bas%base_day +  bas%secs/86400.0d0

      end program demo_date_to_bas
