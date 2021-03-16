           program demo_listout
           use M_strings, only : listout
           implicit none
           integer,allocatable :: icurve_lists(:)
           integer :: icurve_expanded(1000)
           ! icurve_lists is input array
           integer :: inums
           ! icurve_expanded is output array
           integer :: i
           ! number of icurve_lists values on input,
           ! number of icurve_expanded numbers on output
           integer :: ierr
              icurve_lists=[1, 20, -30, 101, 100, 99, 100, -120, 222, -200]
              inums=size(icurve_lists)
              call listout(icurve_lists,icurve_expanded,inums,ierr)
              if(ierr.eq.0)then
                 write(*,'(i0)')(icurve_expanded(i),i=1,inums)
              else
                 write(*,'(a,i0)')'error occurred in *listout* ',ierr
                 write(*,'(i0)')(icurve_expanded(i),i=1,inums)
              endif
           end program demo_listout
