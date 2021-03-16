          program demo_rewind
          implicit none
          character(len=256) :: line
          character(len=256) :: mssge
          integer            :: i
          integer            :: ios
             open(10,file='demo_rewind.txt') ! open a file
             do i=1,100                      ! write lines to it
                write(10,'(a,i0)') 'line ',i
             enddo
             rewind(10, iostat=ios,iomsg=mssge)
             if(ios.ne.0)then
                write(*,*)'*error* ',trim(mssge)
                stop
             endif
             write(*,*)'wrote 100 lines, but now at line ...'
             read(10,'(a)')line
             write(*,'(a)')line
             read(10)
             read(10)
             read(10)
             write(*,*)'skipped a few lines, now at ...'
             read(10,'(a)')line
             write(*,'(a)')line
             close(10,status='delete')
          end program demo_rewind
