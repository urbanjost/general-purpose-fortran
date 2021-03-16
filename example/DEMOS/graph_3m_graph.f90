          program demo_graph
          use m_graph, only : graph, graph_init
          use M_draw

          integer,parameter            :: numlines=3
          integer,parameter            :: numpts=25
          integer,parameter            :: nf=255
          real                         :: x(numpts),y(numpts,numlines)
          character(len=80)            :: c(numlines+3)
          integer                      :: nc(numlines+3)
          real                         :: f(nf)
          character(len=20)            :: device
          integer                      :: ixsize
          integer                      :: iysize
          character(len=:),allocatable :: filename
          integer                      :: w
             device='x11'
             ixsize=1200*.75*0.5
             iysize=900*.75*0.5
             w=40
             if(device.eq.'x11')then
                call prefposition(0,0)
                call prefsize(ixsize,iysize)
             else
                call prefsize(ixsize,iysize)
                !!call voutput(str(filename,'_',int(a),'x',int(b),'.',device,nospace=.true.))
             endif
             call vinit(device)
             call vsetflush(.false.)
             call vflush()
             call linewidth(w)

          !     fill some arrays with data we can plot
             DO i20=1,25
                X(i20)=i20
                Y(i20,1)=i20**2+5.0
                Y(i20,2)=i20*20.0
                Y(i20,3)=(-3.0)*(i20/4.0)**3
             enddo

             f=0.0 !     zero out option array
          !     set up color and linetype in option array
             do i70=55,nf,2
                f(i70)=mod(i70,7)
                f(i70-1)=mod(i70,7)
             enddo
             f(52)=1
             f(53)=1

             ndp=numpts! number of data points per line
             ndl=3     ! number of data lines
             !c        ! array of strings dimensioned c(3+numpts)
             c(1)='X-AXIS'  !  c(1) : x axis title
             c(2)='Y-AXIS'  !  c(2) : y axis title
             c(3)='TITLE'  !  c(3) : top title
             c(4)='LABEL 1'  !  c(n) : line N-3 legend (optionally used)
             c(5)='LABEL 2'  !  c(n) : line N-3 legend (optionally used)
             c(6)='LABEL 3'  !  c(n) : line N-3 legend (optionally used)
             !nc       ! array of string lengths in c() dimensioned nc(3+numpts)
             do i60=1,6
                nc(i60)=len_trim(c(i60))
             enddo
             !f(39)= 0.25
             !f(40)= 0.25
             !f(42)= 3.00
             !f(43)=-1.00
          !  initialize graphics
             call graph_init(12.0,9.0, 1.00,1.00, 1.0)
             INFINITE: do
                CALL graph(X,Y,NDP,NDL,F,C,NC)
                call vflush()
          888   continue
                write(*,*)'Enter index and value for graph(3f) option array:'
                read(*,*,end=999,err=888)index,value
                if(index.lt.1)then
                   exit INFINITE
                elseif(index.gt.nf)then
                   goto 888
                else
                   f(index)=value
                   write(*,*)'index ',index,' now ',value
                endif
                call vflush()              ! flush graphics buffers
                call color(7)
                call clear()
                call color(0)
                call vflush()              ! flush graphics buffers
             enddo INFINITE
          999 continue
             call vflush()              ! flush graphics buffers
             call vexit()               ! close up plot package
             stop
          end program demo_graph
