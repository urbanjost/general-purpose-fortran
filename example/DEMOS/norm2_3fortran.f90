      program demo_norm2
      implicit none
      integer :: i
      real :: x(2,3) = reshape([ &
         1, 2, 3, &
         4, 5, 6  &
         ],shape(x),order=[2,1])

        write(*,*) 'input in row-column order'
        write(*,*) 'x='
        write(*,'(4x,3f4.0)')transpose(x)
        write(*,*)
        write(*,*) 'norm2(x)=',norm2(x)
        write(*,*) 'which is equivalent to'
        write(*,*) 'sqrt(sum(x**2))=',sqrt(sum(x**2))
        write(*,*)
        write(*,*) 'for reference the array squared is'
        write(*,*) 'x**2='
        write(*,'(4x,3f4.0)')transpose(x**2)
        write(*,*)
        write(*,*) 'norm2(x,dim=1)=',norm2(x,dim=1)
        write(*,*) 'norm2(x,dim=2)=',norm2(x,dim=2)
        write(*,*) '(sqrt(sum(x(:,i)**2)),i=1,3)=',(sqrt(sum(x(:,i)**2)),i=1,3)
        write(*,*) '(sqrt(sum(x(i,:)**2)),i=1,2)=',(sqrt(sum(x(i,:)**2)),i=1,2)

      end program demo_norm2
