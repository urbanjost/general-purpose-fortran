           program demo_achar
           implicit none
           character(len=1) :: c
           integer,parameter :: blank=32
           integer,parameter :: horizonal_tab=11
           integer,parameter :: escape=27
           integer :: i
             c = achar(blank)
             write(*,'(i0,1x,a,1x,b0,1x,o0,1x,z0)')blank,c,c,c,c
             write(*,'(32(a))') (achar(i),i=32,126)
           end program demo_achar
