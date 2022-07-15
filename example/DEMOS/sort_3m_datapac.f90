     program demo_sort
     use M_datapac, only : sort
     implicit none
     integer,parameter            :: isz=20
     real                         :: aa(isz)
     real                         :: bb(isz)
     integer                      :: i
        write(*,*)'initializing array with ',isz,' random numbers'
        call random_seed()
        CALL RANDOM_NUMBER(aa)
        aa=aa*450000.0
        bb=real([(i,i=1,isz)])

        call sort(aa,isz,bb) ! sort data

        write(*,*)'checking if real values are sorted(3f)'
        do i=1,isz-1
           if(bb(i).gt.bb(i+1))then
              write(*,*)'Error in sorting reals small to large ',i,bb(i),bb(i+1)
           endif
        enddo
       write(*,'(2(g0,1x))')'ORIGINAL','SORTED',(aa(i),bb(i),i=1,isz)

     end program demo_sort
