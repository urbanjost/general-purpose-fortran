          program demo_greatest_common_divisor
          use M_factor, only : gcd=>greatest_common_divisor
          implicit none
          integer, allocatable :: matrix(:,:)
             ! SCALAR:
                call writeit(26,130,26)
                call writeit(91,390,13)
                call writeit(-91,390,13)
                call writeit(91,-390,13)
                call writeit(-41,-43,1)
                call writeit(-20,-10,10)
                call writeit(20,10,10)
             ! VECTOR:
                call writeit_v([26,130,91,390],13)
                call writeit_v([5,7,11,13,17,19,23,29,31,37,41,43,47],1)
                call writeit_v([-20,-10,0],10)
                call writeit_v([20,10,0],10)
                call writeit_v([26,130],26)
                call writeit_v([91,390],13)
                call writeit_v([-91,390],13)
                call writeit_v([91,-390],13)
                call writeit_v([-41,-43],1)
                call writeit_v([-20,-10],10)
                call writeit_v([20,10],10)
             ! MATRIX:
                matrix=reshape([ 11,22,33,44,55,66],[2,3])
                call write_matrix(matrix,11)
                matrix=reshape([5,7,11,13,17,19,23,29,31,37,41,43,47],[13,1])
                call write_matrix(matrix,1)
                matrix=reshape([40,80,120,160],[2,2])
                call write_matrix(matrix,40)

             contains

             subroutine writeit(ii,jj,answer)
             integer,intent(in) :: ii,jj
             integer,intent(in) :: answer
                write(*,'("gcd([",i0,",",i0,"]) produces ",i0," which is ",l1)') &
                     & ii,jj,gcd(ii,jj),gcd(ii,jj).eq.answer
             end subroutine writeit

             subroutine writeit_v(vector,answer)
             integer,intent(in) :: vector(:)
             integer,intent(in) :: answer
                write(*,'("gcd([",*(i0:,","))',advance='no')vector
                write(*,'("]) produces ",i0," which is ",l1)') &
                     & gcd(vector),gcd(vector).eq.answer
             end subroutine writeit_v

             subroutine write_matrix(matrix,answer)
             integer,intent(in) :: matrix(:,:)
             integer,intent(in) :: answer
                write(*,*)'MATRIX SHAPE:',size(matrix,dim=1),size(matrix,dim=2)
                write(*,'("gcd([",*(i0:,","))',advance='no')matrix
                write(*,'("]) produces ",i0," which is ",l1)') &
                     & gcd(matrix),gcd(matrix).eq.answer
             end subroutine write_matrix

          end program demo_greatest_common_divisor
