           program demo_days2sec
           use M_time, only : days2sec
           implicit none
              write(*,*)days2sec('1-12:04:20')
              write(*,*)'one second ',days2sec('1')
              write(*,*)'one minute ',days2sec('1:00')
              write(*,*)'one hour ',days2sec('1:00:00')
              write(*,*)'one day ',days2sec('1-00:00:00')
              write(*,*)nint(days2sec(' 1-12:04:20              ')) .eq. 129860
              write(*,*)nint(days2sec(' 1.5 days                ')) .eq. 129600
              write(*,*)nint(days2sec(' 1.5 days 4hrs 30minutes ')) .eq. 145800
              write(*,*)nint(days2sec(' 1.5d                    ')) .eq. 129600
              write(*,*)nint(days2sec(' 1d2h3m4s                ')) .eq. 93784
              ! duplicates
              write(*,*)nint(days2sec(' 1d1d1d                  ')) .eq. 259200
              ! negative values
              write(*,*)nint(days2sec(' 4d-12h                  ')) .eq. 302400
           end program demo_days2sec
