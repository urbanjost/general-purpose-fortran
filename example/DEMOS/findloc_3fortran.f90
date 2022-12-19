      program demo_findloc
      logical,parameter :: T=.true., F=.false.
      integer,allocatable :: ibox(:,:)
      logical,allocatable :: mask(:,:)
        ! basics
         ! the first element matching the value is returned AS AN ARRAY
         call printi('== 6',findloc ([2, 6, 4, 6], value = 6))
         call printi('== 6',findloc ([2, 6, 4, 6], value = 6,back=.true.))
         ! the first element matching the value is returned AS A SCALAR
         call printi('== 6',findloc ([2, 6, 4, 6], value = 6,dim=1))
         call printi('== 6',findloc ([2, 6, 4, 6], value = 6,back=.true.,dim=1))

         ibox=reshape([ 0,-5,  7, 7, &
                        3, 4, -1, 2, &
                        1, 5,  6, 7] ,shape=[3,4],order=[2,1])

         mask=reshape([ T, T, F, T, &
                        T, T, F, T, &
                        T, T, F, T] ,shape=[3,4],order=[2,1])

         call printi('array is', ibox )
         call printl('mask  is', mask )
         print *, 'so for == 7 and back=.false.'
         call printi('so for == 7 the address of the element is', &
                 & findloc (ibox, 7, mask = mask) )
         print *, 'so for == 7 and back=.true.'
         call printi('so for == 7 the address of the element is', &
                 & findloc (ibox, 7, mask = mask, back=.true.) )

         print *,'This is independent of declared lower bounds for the array'

         print *, ' using dim=N'
         ibox=reshape([ 1,  2, -9,  &
                        2,  2,  6 ] ,shape=[2,3],order=[2,1])

         call printi('array is', ibox )
         ! has the value [2, 1, 0] and
         call printi('',findloc (ibox, value = 2, dim = 1) )
         ! has the value [2, 1].
         call printi('',findloc (ibox, value = 2, dim = 2) )
      contains
      ! GENERIC ROUTINES TO PRINT MATRICES
      subroutine printl(title,a)
      implicit none
      !@(#) print small 2d logical scalar, vector, matrix in row-column format
      character(len=*),intent(in)  :: title
      logical,intent(in)           :: a(..)

      character(len=*),parameter   :: row='(" > [ ",*(l1:,","))'
      character(len=*),parameter   :: all='(" ",*(g0,1x))'
      logical,allocatable          :: b(:,:)
      integer                      :: i
         write(*,all,advance='no')trim(title)
         ! copy everything to a matrix to keep code simple
         select rank(a)
         rank (0); write(*,'(a)')' (a scalar)'; b=reshape([a],[1,1])
         rank (1); write(*,'(a)')' (a vector)'; b=reshape(a,[size(a),1])
         rank (2); write(*,'(a)')' (a matrix)'; b=a
         rank default; stop '*printl* unexpected rank'
         end select
         do i=1,size(b,dim=1)
            write(*,fmt=row,advance='no')b(i,:)
            write(*,'(" ]")')
         enddo
         write(*,all) '>shape=',shape(a),',rank=',rank(a),',size=',size(a)
         write(*,*)
      end subroutine printl

      subroutine printi(title,a)
      implicit none
      !@(#) print small 2d integer scalar, vector, matrix in row-column format
      character(len=*),intent(in)  :: title
      integer,intent(in)           :: a(..)
      character(len=*),parameter   :: all='(" ",*(g0,1x))'
      character(len=20)            :: row
      integer,allocatable          :: b(:,:)
      integer                      :: i
         write(*,all,advance='no')trim(title)
         ! copy everything to a matrix to keep code simple
         select rank(a)
         rank (0); write(*,'(a)')' (a scalar)'; b=reshape([a],[1,1])
         rank (1); write(*,'(a)')' (a vector)'; b=reshape(a,[size(a),1])
         rank (2); write(*,'(a)')' (a matrix)'; b=a
         rank default; stop '*printi* unexpected rank'
         end select
         ! find how many characters to use for integers
         write(row,'(i0)')ceiling(log10(real(maxval(abs(b)))))+2
         ! use this format to write a row
         row='(" > [",*(i'//trim(row)//':,","))'
         do i=1,size(b,dim=1)
            write(*,fmt=row,advance='no')b(i,:)
            write(*,'(" ]")')
         enddo
         write(*,all) '>shape=',shape(a),',rank=',rank(a),',size=',size(a)
         write(*,*)
      end subroutine printi
      end program demo_findloc
