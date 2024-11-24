      program demo_minval
      implicit none
      integer :: i
      character(len=:),allocatable :: strs(:)
      character(len=*),parameter :: g='(3x,*(g0,1x))'

      integer,save :: ints(3,5)= reshape([&
             1,  -2,   3,   4,   5,  &
            10,  20, -30,  40,  50,  &
            11,  22,  33, -44,  55  &
      ],shape(ints),order=[2,1])

      integer,save :: box(3,5,2)

         box(:,:,1)=ints
         box(:,:,2)=-ints

         write(*,*)'Given the array'
         write(*,'(1x,*(g4.4,1x))') &
         & (ints(i,:),new_line('a'),i=1,size(ints,dim=1))

         write(*,*)'What is the smallest element in the array?'
         write(*,g) minval(ints),'at <',minloc(ints),'>'

         write(*,*)'What is the smallest element in each column?'
         write(*,g) minval(ints,dim=1)

         write(*,*)'What is the smallest element in each row?'
         write(*,g) minval(ints,dim=2)

         ! notice the shape of the output has less columns
         ! than the input in this case
         write(*,*)'What is the smallest element in each column,'
         write(*,*)'considering only those elements that are'
         write(*,*)'greater than zero?'
         write(*,g) minval(ints, dim=1, mask = ints > 0)

         write(*,*)&
         & 'if everything is false a zero-sized array is NOT returned'
         write(*,*) minval(ints, dim=1, mask = .false.)
         write(*,*)'even for a zero-sized input'
         write(*,g) minval([integer ::], dim=1, mask = .false.)

         write(*,*)'a scalar answer for everything false is huge()'
         write(*,g) minval(ints, mask = .false.)
         write(*,g) minval([integer ::], mask = .false.)

         print *, 'if zero-size character array all dels if ASCII'
         strs=[character(len=5)::]
         strs=minval(strs)
         print g, ichar([(strs(i),i=1,len(strs))])

         write(*,*)'some calls with three dimensions'
         write(*,g) minval(box, mask = .true. )
         write(*,g) minval(box, dim=1, mask = .true. )

         write(*,g) minval(box, dim=2, mask = .true. )
         write(*,g) 'shape of answer is ', &
         & shape(minval(box, dim=2, mask = .true. ))

      end program demo_minval
