      program demo_locale
      use M_time, only : locale, now
      implicit none
         call locale('POSIX')
         write(*,*)now()
         call locale('french')
         write(*,*)now()
         call mine()
         write(*,*)now()
      contains
      subroutine mine()
      character(len=*),parameter :: months(12)=[ character(len=9) :: &
      &'JANUARY','FEBRUARY','MARCH    ','APRIL  ','MAY     ','JUNE    ', &
      &'JULY   ','AUGUST  ','SEPTEMBER','OCTOBER','NOVEMBER','DECEMBER']
      character(len=*),parameter :: weekdays(7)=[character(len=9) :: &
      &'MONDAY','TUESDAY','WEDNESDAY','THURSDAY','FRIDAY','SATURDAY','SUNDAY']
      character(len=3),parameter :: short_months(12)=months(:)(1:3)
      character(len=3),parameter :: short_weekdays(7)=weekdays(:)(1:3)
      integer :: ierr
        call locale('user',months,short_months,weekdays,short_weekdays,ierr)
      end subroutine mine
      end program demo_locale
