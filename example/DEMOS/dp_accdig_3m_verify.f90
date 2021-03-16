          program demo_dp_accdig ! fortran 90 example
          use M_verify, only : dp_accdig
          implicit none
          integer         :: digi
          doubleprecision :: a, b
          integer         :: i10, i20, i30
          integer         :: ind, ind1, ind2
          real            :: acurcy, acurcy1, acurcy2
          doubleprecision :: vals(9)
          data vals/ &
            &1.234680d0,   1.2345378d0,  2.2234568d0, 1.2345678d0, &
            &1.2345679d0, -1.2345678d0, 76.234567d0,  2.4691356d0, &
            &0.0d0/
             write(*,*)'========================='
             do i10=0,16
                a=1.0d0
                b=a+1.0d0/(10**i10)
                call dp_accdig(a,b,8.0,acurcy,ind)
                write(*,*)i10,a,b,acurcy,ind
             enddo
             write(*,*)'========================='
             digi=16
             do i20=0,digi
                a=1.0d0
                b=a+1.0d0/(10**i20)
                call dp_accdig(a,b,dble(digi),acurcy,ind)
                write(*,*)i20,a,b,acurcy,ind
             enddo
             write(*,*)'========================='
             do i30=1,9
                call dp_accdig(1.2345678d0,vals(i30),8.0,acurcy1,ind1)
                call dp_accdig(vals(i30),1.2345678d0,8.0,acurcy2,ind2)
                write(*,*)i30,vals(i30),acurcy1,acurcy2,ind1,ind2
             enddo
          end program demo_dp_accdig
