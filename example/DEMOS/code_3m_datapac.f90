     program demo_code
     use M_datapac, only : code
     implicit none
     integer,parameter            :: isz=20
     real                         :: vals(isz)
     real                         :: rndx(isz)
     integer                      :: i
        write(*,*)' initializing array with ',isz,' random numbers'
        call random_seed()
        CALL RANDOM_NUMBER(vals)
        vals=vals*450000.0
        ! make sure some duplicates
        vals(3)=vals(6)
        vals(4)=vals(15)

        call code(vals,isz,rndx) ! code data
        ! check order
        write(*,*)
        write(*,'(2(5x,g0.10,1x))')'Values','Code',(vals(i),nint(rndx(i)),i=1,isz)

     end program demo_code
