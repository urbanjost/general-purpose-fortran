            program demo_tabgraph
            use M_messages, only : tabgraph
            implicit none
            character(len=500) :: ctmp
            integer            :: i10, i20
            real               :: array(4)
            integer            :: ilen
            do i20=1,4
               do i10=1,400
                  array(1)=i10*12/100.0
                  array(2)=sin(array(1))
                  array(3)=cos(array(1))
                  if(i20.eq.1)then
                  ! fixed width of 50 for scale
                     call tabgraph(ctmp,array,3,-1.0,1.0,' ',50)
                     ! ctmp  --> CTMP string to fill
                     ! array --> ARRAY data
                     ! 3     --> IVALS
                     !-1     --> RMIN
                     ! 1     --> RMAX
                     !' '    --> CFILL
                     !50     --> ILEN
                  elseif(i20.eq.2)then
                  ! fixed width of 90 for scale with a non-blank fill character
                     call tabgraph(ctmp,array,3,-1.0,1.0,'.',90)
                  elseif(i20.eq.3)then
                  ! 0 len auto-sizes scale region
                     call tabgraph(ctmp,array,3,-1.0,1.0,' ',0)
                  elseif(i20.eq.4)then
                  ! number of values less than or equal to 1
                     call tabgraph(ctmp,array,1,0.0,48.0,' ',0)
                  endif
                  if(i10.eq.1)then
                     ilen=len_trim(ctmp)
                     ilen=max(ilen,1)
                  endif
                  write(*,'(a)')ctmp(1:ilen) ! tabgraph test program
                  ! write(*,'(i5,a)')i10,ctmp(1:ilen) write with a number count
               enddo
            enddo
            end program demo_tabgraph
