     program demo_sortc
     use M_datapac, only : sortc, label
     implicit none
     integer,parameter            :: isz=20
     real                         :: aa(isz)
     real                         :: bb(isz)
     real                         :: cc(isz)
     real                         :: dd(isz)
     integer                      :: i
       call label('sortc')
       write(*,*)'initializing array with ',isz,' random numbers'
       call random_seed()
       CALL RANDOM_NUMBER(aa)
       aa=aa*450000.0
       bb=real([(i,i=1,isz)])
       call sortc(aa,bb,size(aa),cc,dd)

       write(*,*)'checking if real values are sorted(3f)'
       do i=1,isz-1
          if(cc(i).gt.cc(i+1))then
             write(*,*)'Error in sorting reals small to large ',i,cc(i),cc(i+1)
          endif
       enddo
       write(*,*)'test of sortc(3f) complete'
       write(*,'(4(g0,1x))')(aa(i),bb(i),cc(i),dd(i),i=1,isz)
       write(*,'(*(g0,1x))')sum(aa),sum(cc) ! should be the same if no truncation
       write(*,'(*(g0,1x))')sum(bb),sum(dd)

     end program demo_sortc
