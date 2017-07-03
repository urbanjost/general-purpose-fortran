[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                       Manual Reference Pages  - greatest_common_divisor (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    greatest_common_divisor(3f) - [M_factor]calculate greatest common divisor of two integers or vector m(:), matrix m(:,:) or
    cuboid m(:,:,:)

CONTENTS

    Synopsis
    Description
    Example

SYNOPSIS

    The function is generic and may take either two integers or an integer
    vector, matrix, or cuboid.


       integer function greatest_common_divisor(i,j)
        integer,intent(in)::  i,j
         or
       integer function greatest_common_divisor(m)
        integer,intent(in)::  m(:)
         or
        integer,intent(in)::  m(:,:)
         or
        integer,intent(in)::  m(:,:,:)



DESCRIPTION

    The method used is the Euler algorithm; that for two integers ...

        1.    Subtract the 2nd number (N) as many times as possible from the 1st one (M) and save remainder using FORTRAN function
              MOD.

        2.    Test if remainder is equal to zero, if so GCD = N. If not replace M with N and N with remainder and proceed with step
              1.

        3.    Repeat both steps until remainder becomes zero.

EXAMPLE

    Sample program:

       program demo_greatest_common_divisor
       use M_factor, only : gcd=>greatest_common_divisor
       implicit none
       integer, allocatable :: matrix(:,:)
          write(*,*) SCALAR: 
             call writeit(26,130,26)
             call writeit(91,390,13)
             call writeit(-91,390,13)
             call writeit(91,-390,13)
             call writeit(-41,-43,1)
             call writeit(-20,-10,10)
             call writeit(20,10,10)
          write(*,*) VECTOR: 
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
          write(*,*) MATRIX: 
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
             write(*, ("gcd([",i0,",",i0,"]) produces ",i0," which is ",l1) ) &
                  & ii,jj,gcd(ii,jj),gcd(ii,jj).eq.answer
          end subroutine writeit


          subroutine writeit_v(vector,answer)
          integer,intent(in) :: vector(:)
          integer,intent(in) :: answer
             write(*, ("gcd([",*(i0:,",")) ,advance= no )vector
             write(*, ("]) produces ",i0," which is ",l1) ) &
                  & gcd(vector),gcd(vector).eq.answer
          end subroutine writeit_v


          subroutine write_matrix(matrix,answer)
          integer,intent(in) :: matrix(:,:)
          integer,intent(in) :: answer
             write(*,*) MATRIX SHAPE: ,size(matrix,dim=1),size(matrix,dim=2)
             write(*, ("gcd([",*(i0:,",")) ,advance= no )matrix
             write(*, ("]) produces ",i0," which is ",l1) ) &
                  & gcd(matrix),gcd(matrix).eq.answer
          end subroutine write_matrix


       end program demo_greatest_common_divisor

    Expected Output:

       >  SCALAR:
       > gcd([26,130]) produces 26 which is T
       > gcd([91,390]) produces 13 which is T
       > gcd([-91,390]) produces 13 which is T
       > gcd([91,-390]) produces 13 which is T
       > gcd([-41,-43]) produces 1 which is T
       > gcd([-20,-10]) produces 10 which is T
       > gcd([20,10]) produces 10 which is T
       >  VECTOR:
       > gcd([26,130,91,390]) produces 13 which is T
       > gcd([5,7,11,13,17,19,23,29,31,37,41,43,47]) produces 1 which is T
       > gcd([-20,-10,0]) produces 10 which is T
       > gcd([20,10,0]) produces 10 which is T
       > gcd([26,130]) produces 26 which is T
       > gcd([91,390]) produces 13 which is T
       > gcd([-91,390]) produces 13 which is T
       > gcd([91,-390]) produces 13 which is T
       > gcd([-41,-43]) produces 1 which is T
       > gcd([-20,-10]) produces 10 which is T
       > gcd([20,10]) produces 10 which is T
       >  MATRIX:
       >  MATRIX SHAPE:           2           3
       > gcd([11,22,33,44,55,66]) produces 11 which is T
       >  MATRIX SHAPE:          13           1
       > gcd([5,7,11,13,17,19,23,29,31,37,41,43,47]) produces 1 which is T
       >  MATRIX SHAPE:           2           2
       > gcd([40,80,120,160]) produces 40 which is T



-----------------------------------------------------------------------------------------------------------------------------------

                                                    greatest_common_divisor (3)                                       July 02, 2017

Generated by manServer 1.08 from e2510e01-7f85-46a0-8139-e641826ac3e6 using man macros.
                                                           [greatest_c]
