          program demo_strgarr
          use M_kracken, only: sget, kracken, lget
          use M_calculator, only : strgarr
          real vals(41)
          character(len=80) :: line=' '
          character(len=10) :: delims=' ;'
          !  define command arguments, default values and crack command line
          call kracken('cmd','-d " ;" -test .false. -help .false. -begin -end')
          !----------------------------------------------------------
          write(*,*)'SGET',trim(sget('cmd_test'))
          write(*,*)'LGET',lget('cmd_test')
          if(lget('cmd_test'))then   ! cursory test
             call strgarr("10;2/3;sin(4.314)",41,vals,ifound,' ;',ierr)
             write(*,*)'values are',(vals(i),i=1,ifound)
             sumtarget= 9.74497986
             tol=       0.00000001
             sumup=sum(vals(:ifound))
             ipass=0
             if(ifound.ne.3) ipass=ipass+1
             if(ierr.ne.0)   ipass=ipass+2
             if( sumup >= (sumtarget-tol) .and. sumup <= (sumtarget+tol) ) then
             else
                ipass=ipass+4
             endif
             if(ipass.eq.0)then
                write(*,*)'sum is ',sumup
                write(*,*)'number of values is',ifound
                write(*,*)'error flag is',ierr
                write(*,*)'STRGARR*: PASSED'
                stop 0
             else
                write(*,*)'IFOUND:',ifound
                write(*,*)'IERR  :',ierr
                write(*,*)'SUM   :',sumup
                write(*,*)'STRGARR*: FAILED',ipass
                stop -1
             endif
          endif
          !----------------------------------------------------------
          delims=sget('cmd_d')
          write(*,*)'DELIMS=[',trim(delims),']'
          !----------------------------------------------------------
          line=sget('cmd_begin')
          write(*,*)'BEGIN:',trim(line)
          if(line.ne.' ')then
             call strgarr(line,41,vals,ifound,delims,ierr)
          endif
          !----------------------------------------------------------
          line=sget('cmd_oo')
          write(*,*)'LINE:',trim(line)
          if(line.ne.' ')then
             call strgarr(line,41,vals,ifound,delims,ierr)
             write(*,*)(VALS(I),I=1,IFOUND)
          else
             INFINITE: do
                read(*,'(a)',iostat=ios)line
                if(ios.ne.0)then
                   exit INFINITE
                endif
                call strgarr(line,41,vals,ifound,delims,ierr)
                write(*,*)IERR,IFOUND,':',(VALS(I),I=1,IFOUND)
             enddo INFINITE
          endif
          !----------------------------------------------------------
          line=sget('cmd_end')
          write(*,*)'END',trim(line)
          if(line.ne.' ')then
             call strgarr(line,41,vals,ifound,delims,ierr)
             write(*,*)'END:',(VALS(I),I=1,IFOUND)
          endif
          !----------------------------------------------------------
          end program demo_strgarr
