            program demo_strgar3
            use M_hybrid,     only : strgar3
            use M_calculator, only : rnum0
            character(len=90) :: string
            real              :: values(10,4)
            do
               values(:,:)=-123
               write(*,*)'*strgar3* Enter string like 10:1 20 30 40:50'
               read(*,'(a)',iostat=ios)string
               if(ios.ne.0)stop
               call strgar3(string,10,-1.0,values,inums,' ',' ',ierr)
               write(*,*)'inums=',inums
               write(*,*)'ierr=',ierr
               write(*,*)'values(:,1)=',values(:inums,1)
               write(*,*)'values(:,2)=',values(:inums,2)
               write(*,*)'values(:,3)=',values(:inums,3)
               write(*,*)'values(:,4)=',values(:inums,4)
            enddo
            end program demo_strgar3
