             program demo_inum0
             use M_calculator, only : inum0
             i=inum0('20/3.4')
             j=inum0('CI = 13 * 3.1')
             k=inum0('CI')
             write(*,*)'Answers are ',I,J,K
             end program demo_inum0
