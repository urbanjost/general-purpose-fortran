          program demo_lcm
          use M_factor, only : lcm=>least_common_multiple
          implicit none
             write(*,*)'SCALAR:'
                call writeit(10,24,120)
                call writeit(15,30,30)
                call writeit(-15,-30,30)
                call writeit(15,-30,30)
                call writeit(-15,30,30)

             write(*,*)'VECTOR:'
                call writeit_v([10,24],120)
                call writeit_v([15,30],30)
                call writeit_v([-15,-30],30)
                call writeit_v([5,-15,-40],120)
                call writeit_v([2,3,4,5],60)
             write(*,*)'Special cases:'
                call writeit_v([15,0],0)
                call writeit_v([-15,0],0)
                call writeit_v([0],0)
                call writeit_v([-10],10)
                call writeit_v([22],22)
                call writeit_v([0,0],0)
                call writeit_v([0,0,0,0,0],0)
                call writeit_v([0,0,0,-1,0],0)
                call writeit_v([0,0,0,33,0,3,11],0)
             contains

             subroutine writeit(ii,jj,answer)
             integer,intent(in) :: ii,jj
             integer,intent(in) :: answer
                write(*,'("  For lcm(",I0,",",I0,") the value is ",I0," which is ",L1)')&
                   & ii,jj,lcm(ii,jj),lcm(ii,jj).eq.answer
             end subroutine writeit

             subroutine writeit_v(array,answer)
             integer,intent(in) :: array(:)
             integer,intent(in) :: answer
                write(*,'("  For lcm([",*(i0:,1x))',advance='no')array
                write(*,'("]) the value is ",i0," which is ",L1)') &
                   & lcm(array),lcm(array).eq.answer
             end subroutine writeit_v

          end program demo_lcm
