         program demo_associate
         implicit none
         character(len=*),parameter :: g='(*(g0,1x))'
         character :: array(-5:5,-5:5)      ! custom non-normal bounds
         ! note the different between queries of ARRAY versus ARRAY(:,:)
           write(*,g)'array:     ',  'lbound=',lbound(array), &
                                     'ubound=',ubound(array)
           write(*,g)'array(:,:): ', 'lbound=',lbound(array(:,:)), &
                                     'ubound=',ubound(array(:,:))
         ! the bounds assigned to the identifiers are what UBOUND(3)
         ! and LBOUND(3) return given the selector as an argument
           associate ( &
            alias=>   array,              & ! keeps the custom bounds
            normal=>  array(:,:),         & ! gets normal bounds
            quadI=>   array(+1:+5,-5:-1), & ! quad* will have normal bounds
            quadII=>  array(-5:-1,-5:-1), & !
            quadIII=> array(-5:-1,+1:+5), & !
            quadIV=>  array(+1:+5,+1:+5), & !
            xaxis=>array(:,0), &
            yaxis=>array(0,:) &
            )
            array='.' ! selector name is still valid in the block
            xaxis='-'
            yaxis='|'
            alias(0,0)='+' ! uses non-normal bounds, equivalent to array(0,0)='+'
            write(*,'(11(g0,1x))') alias
            ! the quads have normalized dimension bounds (1:5,1:5):
            quadI    =  '1';  quadI(1,1)    =  'a';  quadI(5,5)    =  'A'
            quadII   =  '2';  quadII(1,1)   =  'b';  quadII(5,5)   =  'B'
            quadIII  =  '3';  quadIII(1,1)  =  'c';  quadIII(5,5)  =  'C'
            quadIV   =  '4';  quadIV(1,1)   =  'd';  quadIV(5,5)   =  'D'
            write(*,'(11(g0,1x))') alias
            write(*,g)'array:  lbound=',lbound(array), 'ubound=',ubound(array)
            write(*,g)'alias:  lbound=',lbound(alias), 'ubound=',ubound(alias)
            write(*,g)'normal: lbound=',lbound(normal),'ubound=',ubound(normal)
            write(*,g)'quadI:  lbound=',lbound(quadI), 'ubound=',ubound(quadI)
            write(*,g)'quadII: lbound=',lbound(quadII),'ubound=',ubound(quadII)
            write(*,g)'quadIV: lbound=',lbound(quadIV),'ubound=',ubound(quadIV)
           end associate
         end program demo_associate
