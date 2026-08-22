      program demo_maxval
      implicit none
      integer,save                 :: ints(3,5)= reshape([&
         1,  2,  3, -4,  5, &
        10, 20,-30, 40, 50, &
        11,-22, 33, 44, 55  &
      ],shape(ints),order=[2,1])
      character(len=:),allocatable :: strs(:)
      character(len=:),allocatable :: answer
      integer                      :: i
      character(len=*),parameter   :: gen='(*(g0,1x))'
      character(len=*),parameter   :: ind='(3x,*(g0,1x))'

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
         print ind
         print ind, 'find biggest number less than 30 with mask'
         print ind
         print ind, 'find biggest negative value'
         print ind, '(closest to zero, not biggest magnitude)'
         print ind, maxval(ints,mask=ints.lt.0)
         print ind
         print ind, 'DEALING WITH ZERO-LENGTH STRINGS AND ZERO-SIZE ARRAYS'
         print ind
         print ind, 'if zero size numeric array:'
         print ind, maxval([integer :: ]),'and -huge(0) is',-huge(0),&
         & '(often not the same!)'
         print ind
         print ind, maxval([real :: ]),'and -huge(0.0) is',-huge(0.0)
         print ind
         print ind, 'if zero-size character array all nulls'
         if(allocated(strs))deallocate(strs)
         allocate(character(len=0) :: strs(5))
         print ind, 'STRS() has a length of:', len(strs), &
          & 'a SHAPE of:',shape(strs), &
          & ':a SIZE of:',size(strs)
         print ind, &
          & 'is maxval of null length strings a null character? ',&
          & ichar(maxval(strs))==0
         print ind
         if(allocated(strs))deallocate(strs)
         allocate(character(len=5) :: strs(0))
         print ind, 'STRS() has a length of:', len(strs), &
          & 'a SHAPE of:',shape(strs), &
          & ':a SIZE of:',size(strs)
          answer=maxval(strs)
         print ind, 'is maxval of strings all null characters? ', &
          & [(answer(i:i),i=1,len(answer))].eq.char(0)
         print ind
         print ind, 'if everything in mask is false,'
         print ind, 'same as zero-size array for each subarray'
         print ind, maxval(ints,mask=.false.)
         print ind, maxval(ints,mask=.false.,dim=1)
      end program demo_maxval
