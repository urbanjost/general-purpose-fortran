          program demo_rd
          use M_io, only : rd
          implicit none
          character(len=:),allocatable :: mystring
          doubleprecision              :: d
          real                         :: r
          integer                      :: i
          logical                      :: l

          INFINITE: do
             mystring=rd('Enter string or "STOP":',default='Today')
             if(mystring.eq.'STOP')stop
             i=rd('Enter integer:',default=huge(0))
             r=rd('Enter real:',default=huge(0.0))
             d=rd('Enter double:',default=huge(0.0d0))
             l=rd('Enter logical:',default=.false.)

             write(*,*)'I=', i, 'R=', r, 'D=',d,  'MYSTRING=', mystring
             write(*,*)'L=', l
          enddo INFINITE

          end program demo_rd
