      program demo_days2sec
      use M_time, only : days2sec
      implicit none
         write(*,*)days2sec('1-12:04:20')
         write(*,*)'one second ',days2sec('1')
         write(*,*)'one minute ',days2sec('1:00')
         write(*,*)'one hour ',days2sec('1:00:00')
         write(*,*)'one day ',days2sec('1-00:00:00')
         write(*,*)nint(days2sec(' 1-12:04:20              ')) == 129860
         write(*,*)nint(days2sec(' 1.5 days                ')) == 129600
         write(*,*)nint(days2sec(' 1.5 days 4hrs 30minutes ')) == 145800
         write(*,*)nint(days2sec(' 1.5d                    ')) == 129600
         write(*,*)nint(days2sec(' 1d2h3m4s                ')) == 93784
         ! duplicates
         write(*,*)nint(days2sec(' 1d1d1d                  ')) == 259200
         ! negative values
         write(*,*)nint(days2sec(' 4d-12h                  ')) == 302400
      end program demo_days2sec
