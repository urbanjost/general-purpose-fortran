           program demo_ordinal_seconds
           use M_time, only : ordinal_seconds
           implicit none
           character(len=1) :: paws
           integer          :: ios
           integer          :: istart, iend
           istart=ordinal_seconds()
           write(*,'(a)',advance='no')'now pause. Enter return to continue ...'
           read(*,'(a)',iostat=ios) paws
           iend=ordinal_seconds()
           write(*,*)'that took ',iend-istart,'seconds'
           write(*,*)istart,iend
           end program demo_ordinal_seconds
