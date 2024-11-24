      program demo_maxval
      implicit none
      integer,save :: ints(3,5)= reshape([&
         1,  2,  3,  4,  5, &
        10, 20, 30, 40, 50, &
        11, 22, 33, 44, 55  &
      ],shape(ints),order=[2,1])
      character(len=:),allocatable :: strs(:)
      integer :: i
      character(len=*),parameter :: gen='(*(g0,1x))'
      character(len=*),parameter :: ind='(3x,*(g0,1x))'

         print gen,'Given the array'
         write(*,'(1x,*(g4.4,1x))') &
         & (ints(i,:),new_line('a'),i=1,size(ints,dim=1))
         print gen,'Basics:'
         print ind, 'biggest value in array'
         print ind, maxval(ints)
         print ind, 'biggest value in each column'
         print ind, maxval(ints,dim=1)
         print ind, 'biggest value in each row'
         print ind,  maxval(ints,dim=2)

         print gen,'With a mask:'
         print ind, ' find biggest number less than 30 with mask'
         print ind, maxval(ints,mask=ints.lt.30)

         print gen,'If zero size considered:'
         print ind, 'if zero size numeric array'
         print ind, maxval([integer :: ]),'and -huge(0) is',-huge(0),&
         & '(often not the same!)'
         print ind, 'if zero-size character array all nulls'
         strs=[character(len=5)::]
         strs=maxval(strs)
         print ind, ichar([(strs(i),i=1,len(strs))])
         print ind, 'if everything is false,'
         print ind, 'same as zero-size array for each subarray'
         print ind, maxval(ints,mask=.false.)
         print ind, maxval(ints,mask=.false.,dim=1)
      end program demo_maxval
