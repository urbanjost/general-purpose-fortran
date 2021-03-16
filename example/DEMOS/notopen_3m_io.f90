           program demo_notopen ! test the NOTOPEN(3f) function
           use m_io, only: notopen
           implicit none
           integer :: ii, ierr, igot

           write(*,*)'check for preassigned files from unit 0 to unit 1000'
           write(*,*)'(5 and 6 always return -1)'

           do ii=0,1000
              if(notopen(ii,ii,ierr) .ne. ii)then
                 write(*,*)'INUSE:',ii, notopen(ii,ii,ierr)
              endif
           enddo

           ! open all files from UNIT=10 to UNIT=30 so have used units
           do ii=10,30,1
             open(unit=ii,status="scratch")
           enddo
           ! close UNIT=25
           close(25)

           ! find open file in range 10 to 30
           write(*,*)'Should get 25 for this ..',notopen(10,30,ierr)

           close(18)
           do ii=10,32
             igot=notopen(ii,ii,ierr)
             write(*,*)'For unit ',ii,' I got ',igot,' with ERR=',ierr
           enddo

           end program demo_notopen
