          program demo_accdig ! fortran 90 example
          use M_verify, only : accdig
          implicit none
          integer :: digi
          integer :: i10, i20, i30
          integer :: ind, ind1, ind2
          real    :: acurcy, acurcy1, acurcy2
          real    :: a, b
          real    :: vals(9)
          data vals/ &
            &1.234680,   1.2345378,  2.2234568, 1.2345678, &
            &1.2345679, -1.2345678, 76.234567,  2.4691356, &
            &0.0/
             write(*,*)'========================='
             do i10=0,16
                a=1.0
                b=a+1.0/(10**i10)
                call accdig(a,b,8.0,acurcy,ind)
                write(*,*)i10,a,b,acurcy,ind
             enddo
             write(*,*)'========================='
             digi=16
             do i20=0,digi
                a=1.0
                b=a+1.0/(10**i20)
                call accdig(a,b,real(digi),acurcy,ind)
                write(*,*)i20,a,b,acurcy,ind
             enddo
             write(*,*)'========================='
             do i30=1,9
                call accdig(1.2345678,vals(i30),8.0,acurcy1,ind1)
                call accdig(vals(i30),1.2345678,8.0,acurcy2,ind2)
                write(*,*)i30,vals(i30),acurcy1,acurcy2,ind1,ind2
             enddo
          end program demo_accdig
