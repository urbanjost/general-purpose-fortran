      program demo_btest
      implicit none
      integer :: i, j, pos, a(2,2)
      logical :: bool
      character(len=*),parameter :: g='(*(g0))'

           i = 32768 + 1024 + 64
          write(*,'(a,i0,"=>",b32.32,/)')'Looking at the integer: ',i

          ! looking one bit at a time from LOW BIT TO HIGH BIT
          write(*,g)'from bit 0 to bit ',bit_size(i),'==>'
          do pos=0,bit_size(i)-1
              bool = btest(i, pos)
              write(*,'(l1)',advance='no')bool
          enddo
          write(*,*)

          ! a binary format the hard way.
          ! Note going from bit_size(i) to zero.
          write(*,*)
          write(*,g)'so for ',i,' with a bit size of ',bit_size(i)
          write(*,'(b32.32)')i
          write(*,g)merge('^','_',[(btest(i,j),j=bit_size(i)-1,0,-1)])
          write(*,*)
          write(*,g)'and for ',-i,' with a bit size of ',bit_size(i)
          write(*,'(b32.32)')-i
          write(*,g)merge('^','_',[(btest(-i,j),j=bit_size(i)-1,0,-1)])

          ! elemental:
          !
          a(1,:)=[ 1, 2 ]
          a(2,:)=[ 3, 4 ]
          write(*,*)
          write(*,'(a,/,*(i2,1x,i2,/))')'given the array a ...',a
          ! the second bit of all the values in a
          write(*,'(a,/,*(l2,1x,l2,/))')'the value of btest (a, 2)',btest(a,2)
          ! bits 1,2,3,4 of the value 2
          write(*,'(a,/,*(l2,1x,l2,/))')'the value of btest (2, a)',btest(2,a)
      end program demo_btest
