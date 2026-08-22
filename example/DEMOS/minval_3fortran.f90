      program demo_minval
      implicit none
      integer,parameter :: ucs4=selected_char_kind('ISO_10646')
      integer,parameter :: ascii=selected_char_kind('ascii')
      integer,parameter :: default=selected_char_kind('default')
      integer                      :: i
      character(len=:,kind=ascii),allocatable :: strs(:)
      character(len=*),parameter   :: g='(3x,*(g0,1x))'

      integer,save                 :: ints(3,5)= reshape([&
             1,  -2,   3,   4,   5, &
            10,  20, -30,  40,  50, &
            11,  22,  33, -44,  55  &
      ],shape(ints),order=[2,1])

      integer,save                 :: box(3,5,2)
      character(len=:,kind=ascii),allocatable :: answer

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
         write(*,*) minval(ints, dim=1, mask = ints > ints)
         write(*,*)'even for a zero-sized input'
         write(*,g) minval([integer ::], dim=1 )

         write(*,*)'a scalar answer for everything false is huge()'
         write(*,g) minval(ints, mask = ints > ints)
         write(*,g) minval([integer ::] )

         strs=[character(len=5)::]
         if(len(strs).eq.0)then
            write(*,g)'<WARNING> compensating for bug defining zero-size arrays'
            if(allocated(strs))deallocate(strs)
            allocate(character(len=5) :: strs(0))
         endif

         answer=minval(strs)
         print g, 'is minval of strings all del characters? ', &
          & [(answer(i:i),i=1,len(answer))].eq.char(127)
         print g, 'is minval of strings all del characters? ', &
          & ichar([(answer(i:i),i=1,len(answer))])

         write(*,*)'some calls with three dimensions'
         write(*,g) minval(box, mask = .true. )
         write(*,g) minval(box, dim=1, mask = .true. )

         write(*,g) minval(box, dim=2, mask = .true. )
         write(*,g) 'shape of answer is ', &
         & shape(minval(box, dim=2, mask = .true. ))

      end program demo_minval
