           program demo_d2o
           use M_time, only : d2o
           implicit none
           integer :: dat(8)
              call date_and_time(values=dat)
              write(*,'(" Today is:",*(i0:,":"))')dat
              write(*,*)'Day of year is:',d2o(dat)

              ! year,month,day,timezone,hour,minute,seconds,milliseconds
              dat=[2020,12,31,-240,12,0,0,0]
              write(*,*)dat(1),' Days in year is:',d2o(dat)

              dat=[2021,12,31,-240,12,0,0,0]
              write(*,*)dat(1),' Days in year is:',d2o(dat)

              dat=[2022,12,31,-240,12,0,0,0]
              write(*,*)dat(1),' Days in year is:',d2o(dat)

              dat=[2023,12,31,-240,12,0,0,0]
              write(*,*)dat(1),' Days in year is:',d2o(dat)

              dat=[2024,12,31,-240,12,0,0,0]
              write(*,*)dat(1),' Days in year is:',d2o(dat)

           end program demo_d2o
