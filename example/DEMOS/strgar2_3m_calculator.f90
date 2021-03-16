           program demo_strgar2
           use M_calculator, only : strgar2
           integer             :: ios
           integer             :: i
           integer             :: ifound
           integer             :: ierr
           real                :: vals(1000)
           character(len=4096) :: line

           write(*,'(80("-"))')
           call strgar2('10;2/3;sin(4.314)',4,vals,ifound,' ;',ierr)
           write(*,*)'should find three values in 10;2/3;sin(4.314)'
           write(*,*)'ifound=',ifound
           write(*,*)'values are',(vals(i),i=1,ifound)

           write(*,'(80("-"))')
           write(*,*)'should find three values in 10;2/3;sin(4.314)'
           write(*,*)'ifound=',ifound
           call strgar2('10;2/3;sin(4.314) ',3,vals,ifound,' ;',ierr)
           write(*,*)'ifound=',ifound
           write(*,*)'values are',(vals(i),i=1,ifound)

           write(*,'(80("-"))')
           write(*,*)'should stop at two values in 10;2/3;sin(4.314)'
           call strgar2('10;2/3;sin(4.314)',2,vals,ifound,' ;',ierr)
           write(*,*)'ifound=',ifound
           write(*,*)'values are',(vals(i),i=1,ifound)

           write(*,'(80("-"))')
           write(*,*)'should stop at one values in 10;2/3;sin(4.314)'
           call strgar2('10;2/3;sin(4.314)',1,vals,ifound,' ;',ierr)
           write(*,*)'ifound=',ifound
           write(*,*)'values are',(vals(i),i=1,ifound)

           write(*,'(80("-"))')
           write(*,*)'should find three values in 10;2/3;sin(4.314) ; ; ;   ;; '
           call strgar2('10;2/3;sin(4.314) ; ; ;   ;; ',1000,vals,ifound,' ;',ierr)
           write(*,*)'ifound=',ifound
           write(*,*)'values are',(vals(i),i=1,ifound)

           write(*,'(80("-"))')
           write(*,*)'should find an error in  values in 10;20/3O;sin(4.314) ; ; ;   ;; '
           call strgar2('10;20/3O;sin(4.314) ; ; ;   ;; ',1000,vals,ifound,' ;',ierr)
           write(*,*)'ifound=',ifound,' error=',ierr
           write(*,*)'values are',(vals(i),i=1,ifound)

           write(*,'(80("-"))')
           write(*,*)'Enter strings delimited by spaces or semicolons'
              do
                 read(*,'(a)',iostat=ios)line
                 if(ios.ne.0)then
                    stop
                 endif
                 call strgar2(line,1000,vals,ifound,' ;',ierr)
                 write(*,*)'ifound=',ifound
                 write(*,*)'values are',(vals(i),i=1,ifound)
              enddo
           end program demo_strgar2
