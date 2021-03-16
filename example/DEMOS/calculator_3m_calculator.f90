             program demo_calculator
             !compute(1f): line mode calculator program (that calls calculator(3f))
             use M_calculator, only: calculator,iclen_calc
             ! iclen_calc : max length of expression or variable value as a string
             implicit none
             integer,parameter         :: dp=kind(0.0d0)
             character(len=iclen_calc) :: line
             character(len=iclen_calc) :: outlin
             character(len=iclen_calc) :: event
             real(kind=dp)             :: rvalue
             integer                   :: ierr
             ierr=0
             call calculator('ownmode(1)',outlin,event,rvalue,ierr)
             ! activate user-defined function interface
             INFINITE: do
                read(*,'(a)',end=999)line
                if(line.eq.'.')stop
                call calculator(line,outlin,event,rvalue,ierr)
                select case (ierr)
                ! several different meanings to the error flag returned by calculator
                case(0)
                ! a numeric value was returned without error
                  write(*,'(a,a,a)')trim(outlin),' = ',trim(line)
                case(2)
                ! a string value was returned without error
                  write(*,'(a)')trim(event(:int(rvalue)))
                case(1)
                ! a request for a message has been returned (from DUMP or FUNC)
                  write(*,'(a,a)')'message===>',trim(event(:len_trim(event)))
                case(-1)
                ! an error has occurred
                  write(*,'(a,a)')'error===>',trim(event(:len_trim(event)))
                case default
                ! this should not occur
                  WRITE(6,'(A,i10)')'*CALCULATOR* UNEXPECTED IERR VALUE ',IERR
                end select
             enddo INFINITE
             999 continue
             end program demo_calculator
