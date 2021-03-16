           program demo_guessdate
           use M_time, only : guessdate, fmtdate
           implicit none
           character(len=20),allocatable :: datestrings(:)
           character(len=:),allocatable  :: answer
           integer                       :: dat(8)
           integer                       :: i
              datestrings=[ &
              & 'January 9th, 2001   ',&
              & ' Tue Jul 19 2016    ',&
              & ' 21/12/2016         ',&
              & ' 4th of Jul 2004    ' ]
              do i=1,size(datestrings)
                 write(*,'(a)')repeat('-',80)
                 write(*,*)'TRYING ',datestrings(i)
                 call guessdate(datestrings(i),dat)
                 write(*,*)'DAT ARRAY ',dat
                 answer=fmtdate(dat)
                 write(*,*)'FOR '//datestrings(i)//' GOT '//trim(answer)
              enddo
           end program demo_guessdate
