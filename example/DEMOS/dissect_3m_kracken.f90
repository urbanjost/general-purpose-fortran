      program demo_dissect
      use M_kracken, only : kracken,iget,rget,sget,dissect
      implicit none
      integer :: ierr

      call dissect('demo',' -int 1000 -float 1234.567 -str CHARACTER value','-int 456 -float 50.00 ',ierr)
      write(*,'(a,i0)')'INTEGER IS ',iget('demo_int')
      write(*,'(a,g0)')'REAL IS ',rget('demo_float')
      write(*,'(a,a)')'STRING IS '//trim(sget('demo_str'))

      end program demo_dissect
