      program demo_min
      implicit none
      integer :: i
      integer :: rectangle(3,4)=reshape([(-6+i,i=0,11)],[3,4])
      character(len=:),allocatable :: answer
      character(len=:),allocatable :: aanswer(:)
          print *, 'basics'
          print *, min(10.0,11.0,30.0,-100.0)
          print *, min(-200.0,-1.0)
          print *, 'elemental'
          print *, min(1,[2,3,4])
          print *, min(5,[2,3,4])

          print *, 'box:'
          do i=1,size(rectangle,dim=1)
             write(*,'(*(i3,1x))')rectangle(i,:)
          enddo
          print *, 'make all values 0 or less:'
          do i=1,size(rectangle,dim=1)
             write(*,'(*(i3,1x))')min(rectangle(i,:),0)
          enddo

          write(*,*)'test1 ',merge('PASSED','FAILED', &
          MIN(-9.0, 7.0, 2.0) == -9.0)

          write(*,*)'test2A ',merge('PASSED','FAILED', &
          & MIN('A', 'YY') == 'A ' .and. len(MIN('A','YY')).eq.2)
          write(*,*)'test2B ',merge('PASSED','FAILED', &
          & MIN('AA', 'Y') == 'AA' .and. len(MIN('AA','Y')).eq.2)
          write(*,*)'test2C ',merge('PASSED','FAILED', &
          & MIN('Y', 'AA') == 'AA' .and. len(MIN('Y','AA')).eq.2)
          write(*,*)'test2D ',merge('PASSED','FAILED', &
          & MIN('YY', 'A') == 'A ' .and. len(MIN('YY','A')).eq.2)

          aanswer=MIN(['Z', 'A'], ['YY', 'B '])
          write(*,'(1x,*(g0,1x))') "MIN(['Z', 'A'], ['YY', 'B ']): ",aanswer
          write(*,*)'test3 ',merge('PASSED','FAILED', &
          all(aanswer.eq. ['YY', 'A ']) .and. len(aanswer).eq.2)

      end program demo_min
