      program demo_sum
      implicit none
      integer :: vector(5) , matrix(3,4), box(5,6,7)

         vector = [ 1, 2, -3, 4, 5 ]

         matrix(1,:)=[  -1,   2,    -3,   4    ]
         matrix(2,:)=[  10,   -20,  30,   -40  ]
         matrix(3,:)=[  100,  200, -300,  400  ]

         box=11

        ! basics
         print *, 'sum all elements:',sum(vector)
         print *, 'real :',sum([11.0,-5.0,20.0])
         print *, 'complex :',sum([(1.1,-3.3),(4.0,5.0),(8.0,-6.0)])
        ! with MASK option
         print *, 'sum odd elements:',sum(vector, mask=mod(vector, 2)==1)
         print *, 'sum positive values:', sum(vector, mask=vector>0)

         call printi('the input array', matrix )
         call printi('sum of all elements in matrix', sum(matrix) )
         call printi('sum of positive elements', sum(matrix,matrix>=0) )
        ! along dimensions
         call printi('sum along rows', sum(matrix,dim=1) )
         call printi('sum along columns', sum(matrix,dim=2) )
         call printi('sum of a vector is always a scalar', sum(vector,dim=1) )
         call printi('sum of a volume by row', sum(box,dim=1) )
         call printi('sum of a volume by column', sum(box,dim=2) )
         call printi('sum of a volume by depth', sum(box,dim=3) )

      contains
      ! CONVENIENCE ROUTINE; NOT DIRECTLY CONNECTED TO SPREAD(3)
      subroutine printi(title,a)
      use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,&
       & stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
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
         write(row,'(i0)')ceiling(log10(max(1.0,real(maxval(abs(b))))))+2
         ! use this format to write a row
         row='(" > [",*(i'//trim(row)//':,","))'
         do i=1,size(b,dim=1)
            write(*,fmt=row,advance='no')b(i,:)
            write(*,'(" ]")')
         enddo
         write(*,all) '>shape=',shape(a),',rank=',rank(a),',size=',size(a)
         write(*,*)
      end subroutine printi
      end program demo_sum
