         program demo_system_cmd
         use M_system, only : system_cmd
         implicit none
         logical,allocatable :: status(:)
            status=system_cmd([character(len=1024) :: 'date','pwd','logname'])
            write(*,*)'status=',status
         end program demo_system_cmd
