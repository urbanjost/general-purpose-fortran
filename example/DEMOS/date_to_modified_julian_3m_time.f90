      program demo_date_to_modified_julian
      use M_time, only : date_to_modified_julian
      use M_time, only : date_to_julian, realtime
      implicit none
      integer                    :: dat(8)
      real(kind=realtime)        :: modified_juliandate
      real(kind=realtime)        :: juliandate
      integer                    :: ierr
      character(len=*),parameter :: g='(*(g0,1x))'
         !
         ! generate DAT array
         call date_and_time(values=dat)
         !
         ! show DAT array
         write(*,'("Today is:",*(i0:,":"))')dat
         !
         ! convert DAT to Julian Date
         call date_to_julian(dat,juliandate,ierr)
         write(*,g) 'Expecting:', juliandate - 2400000.5_realtime
         !
         ! convert DAT to Modified Julian Date
         call date_to_modified_julian(dat,modified_juliandate,ierr)
         write(*,g)'Modified Julian Date is ', modified_juliandate

      end program demo_date_to_modified_julian
