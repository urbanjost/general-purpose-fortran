      program demo_count
      implicit none
      character(len=*),parameter :: ints='(*(i2,1x))'
      ! two arrays and a mask all with the same shape
      integer, dimension(2,3) :: a, b
      logical, dimension(2,3) :: mymask
      integer :: i
      integer :: c(2,3,4)

         print *,'the numeric arrays we will compare'
         a = reshape( [ 1, 2, 3, 4, 5, 6 ], [ 2, 3 ])
         b = reshape( [ 0, 7, 3, 4, 5, 8 ], [ 2, 3 ])
         c = reshape( [( i,i=1,24)], [ 2, 3 ,4])
         print '(3i3)', a(1,:)
         print '(3i3)', a(2,:)
         print *
         print '(3i3)', b(1,:)
         print '(3i3)', b(2,:)

        ! basic calls
         print *, 'count a few basic things creating a mask from an expression'
         print *, 'count a>b',count(a>b)
         print *, 'count b<a',count(a<b)
         print *, 'count b==a',count(a==b)
         print *, 'check sum = ',count(a>b) + &
                               & count(a<b) + &
                               & count(a==b).eq.size(a)

         ! The common usage is just getting a count, but if you want
         ! to specify the DIM argument and get back reduced arrays
         ! of counts this is easier to visualize if we look at a mask.
         print *, 'make a mask identifying unequal elements ...'
         mymask = a.ne.b
         print *, 'the mask generated from a.ne.b'
         print '(3l3)', mymask(1,:)
         print '(3l3)', mymask(2,:)

         print *,'count total and along rows and columns ...'

         print '(a)', 'number of elements not equal'
         print '(a)', '(ie. total true elements in the mask)'
         print '(3i3)', count(mymask)

         print '(a)', 'count of elements not equal in each column'
         print '(a)', '(ie. total true elements in each column)'
         print '(3i3)', count(mymask, dim=1)

         print '(a)', 'count of elements not equal in each row'
         print '(a)', '(ie. total true elements in each row)'
         print '(3i3)', count(mymask, dim=2)

         ! working with rank=3 ...
         print *, 'lets try this with c(2,3,4)'
         print *,'  taking the result of the modulo   '
         print *,'   z=1      z=2      z=3      z=4   '
         print *,'  1 3 0 || 2 4 1 || 3 0 2 || 4 1 3 |'
         print *,'  2 4 1 || 3 0 2 || 4 1 3 || 0 2 4 |'
         print *,'                                    '
         print *,'  would result in the mask ..       '
         print *,'  F F T || F F F || F T F || F F F |'
         print *,'  F F F || F T F || F F F || T F F |'
         print *,'                                    '
         print *,' the total number of .true.values is'
         print ints, count(modulo(c,5).eq.0)
         call printi('counting up along a row and removing rows',&
         count(modulo(c,5).eq.0,dim=1))
         call printi('counting up along a column and removing columns',&
         count(modulo(c,5).eq.0,dim=2))
         call printi('counting up along a depth and removing depths',&
         count(modulo(c,5).eq.0,dim=3))

      contains

         ! CONVENIENCE ROUTINE FOR PRINTING SMALL INTEGER MATRICES
         subroutine printi(title,arr)
         implicit none

         !@(#) print small 2d integer arrays in row-column format

         character(len=*),parameter :: all='(*(g0,1x))' ! a handy format
         character(len=*),intent(in)  :: title
         integer,intent(in)           :: arr(:,:)
         integer                      :: i
         character(len=:),allocatable :: biggest

            print all
            print all, trim(title),':(',shape(arr),')'  ! print title
            biggest='           '  ! make buffer to write integer into
            ! find how many characters to use for integers
            write(biggest,'(i0)')ceiling(log10(real(maxval(abs(arr)))))+2
            ! use this format to write a row
            biggest='(" > [",*(i'//trim(biggest)//':,","))'
            ! print one row of array at a time
            do i=1,size(arr,dim=1)
               write(*,fmt=biggest,advance='no')arr(i,:)
               write(*,'(" ]")')
            enddo

         end subroutine printi
      end program demo_count
