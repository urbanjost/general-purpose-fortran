           program demo_moon_fullness
           use M_time, only : now
           use M_time, only : phase_of_moon
           use M_time, only : moon_fullness
           implicit none
           integer             :: dat(8)
              ! generate DAT array
              call date_and_time(values=dat)
              ! show DAT array
              write(*,'(" Today is:",*(i0:,":"))')dat
              ! the %p and %P fields are supported by fmtdate(3f)
              write(*,*)&
              &now('The phase of the moon is %p, with a fullness of %P')
              write(*,'(1x,*(a))',advance='no')&
              &'The phase of the moon is ',trim( phase_of_moon(dat)),','
              write(*,'(1x,a,i0,a)')&
              &'with a fullness of ', moon_fullness(dat),'%'
           end program demo_moon_fullness
