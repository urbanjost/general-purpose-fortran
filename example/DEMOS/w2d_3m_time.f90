         program demo_w2d
         use M_time, only : w2d, fmtdate
         implicit none
           write(*,'(a)')&
           & 'Given Monday 29 December 2008 is written "2009-W01-1"'
           call printit(2009,1,1)
           write(*,'(a)')&
           & 'Given Sunday 3 January 2010 is written "2009-W53-7"'
           call printit(2009,53,7)
           write(*,'(a)')&
           & 'Given the Gregorian date Sun 31 December 2006 &
           &is written 2006-W52-7'
           call printit(2006,52,7)
           write(*,'(a)')&
           & 'Given 27 September 2008 is 2008-W39-6'
           call printit(2008,39,6)
         contains
         subroutine printit(iso_year,iso_week,iso_weekday)
         ! ISO-8601 Week: 2016-W29-1
         integer  :: iso_year, iso_week, iso_weekday
         ! input date array
         integer  :: dat(8)
          call w2d(iso_year,iso_week,iso_weekday,dat)
          write(*,'(a,i0)')'GIVEN:           '
          write(*,'(a,i0)')'ISO-8601 year    ',iso_year
          write(*,'(a,i0)')'ISO-8601 week    ',iso_week
          write(*,'(a,i0)')'ISO-8601 weekday ',iso_weekday
          write(*,'(a,i0)')'RESULT:          '
          write(*,'(a,*(i0:,","))')'   DAT array        ',dat
          write(*,'(a,/,67("="))')'    '//fmtdate(dat,'long')
         end subroutine printit
         end program demo_w2d
