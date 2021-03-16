             program demo_uniq
             use M_io, only : uniq
             implicit none
             character(len=4096) :: myname
             integer             :: i
                myname=uniq('does_not_exist')
                open(unit=10,file='does_exist')
                write(*,*)'name stays the same ',trim(myname)
                myname=uniq('does_exist')
                write(*,*)'name has suffix added ',trim(myname)
                do i=1,10
                   myname=uniq('does_exist')
                   write(*,*) 'FILENAME:',trim(myname)
                   open(unit=20+i,file=myname)
                enddo
             end program demo_uniq
