[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                                Manual Reference Pages  - swap (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    swap(3f) - [M_swap]elemental subroutine swaps two standard type variables of like type

CONTENTS

    Synopsis
    Description
    Example

SYNOPSIS

    subroutine swap(X,Y)

DESCRIPTION

    Generic subroutine SWAP(GEN1,GEN2) swaps two variables of like type (real, integer, complex, character, double, logical).

    On output, the values of X and Y have been interchanged. Swapping is commonly required in procedures that sort data.

    SWAP(3f) is elemental, so it can operate on vectors and arrays as well as scalar values.

EXAMPLE

    Example program:

       program try_swap
       use M_swap, only : swap
       integer             :: iarray(2)=[10,20]
       real                :: rarray(2)=[11.11,22.22]
       doubleprecision     :: darray(2)=[1234.56789d0,9876.54321d0]
       complex             :: carray(2)=[(1234,56789),(9876,54321)]
       logical             :: larray(2)=[.true.,.false.]
       character(len=16)   :: string(2)=["First string    ","The other string"]


       integer             :: one(13)=1
       integer             :: two(13)=2


       integer             :: one2(3,3)=1
       integer             :: two2(3,3)=2


          print *, "integers before swap ", iarray
          call swap (iarray(1), iarray(2))
          print *, "integers after swap  ", iarray


          print *, "reals before swap ", rarray
          call swap (rarray(1), rarray(2))
          print *, "reals after swap  ", rarray


          print *, "doubles before swap ", darray
          call swap (darray(1), darray(2))
          print *, "doubles after swap  ", darray


          print *, "complexes before swap ", carray
          call swap (carray(1), carray(2))
          print *, "complexes after swap  ", carray


          print *, "logicals before swap ", larray
          call swap (larray(1), larray(2))
          print *, "logicals after swap  ", larray


          print *, "strings before swap ", string
          call swap (string(1), string(2))
          print *, "strings after swap ", string


          write(*,*) swap two vectors 
          write(*, ("one before: ",*(i0,:",")) ) one
          write(*, ("two before: ",*(i0,:",")) ) two
          call swap(one,two)
          write(*, ("one after: ",*(i0,:",")) ) one
          write(*, ("two after: ",*(i0,:",")) ) two


          write(*,*) given these arrays initially each time  
          one2=1
          two2=2
          call printarrays()


          write(*,*) swap two rows 
          one2=1
          two2=2
          call swap(one2(2,:),two2(3,:))
          call printarrays()


          write(*,*) swap two columns 
          one2=1
          two2=2
          call swap(one2(:,2),two2(:,2))
          call printarrays()


          write(*,*) swap two arrays with same number of elements 
          one2=1
          two2=2
          call swap(one2,two2)
          call printarrays()


          contains
          subroutine printarrays()
          integer :: i
          do i=1,size(one2(1,:))
             write(*, (*(i0,:",")) ) one2(i,:)
          enddo
          write(*,*)
          do i=1,size(two2(1,:))
             write(*, (*(i0,:",")) ) two2(i,:)
          enddo
          end subroutine printarrays



        end program try_swap

    Expected Results:

       > integers before swap           10          20
       > integers after swap            20          10
       > reals before swap    11.1099997       22.2199993
       > reals after swap     22.2199993       11.1099997
       > doubles before swap    1234.5678900000000        9876.5432099999998
       > doubles after swap     9876.5432099999998        1234.5678900000000
       > complexes before swap  (  1234.00000    ,  56789.0000    ) (  9876.00000    ,  54321.0000    )
       > complexes after swap   (  9876.00000    ,  54321.0000    ) (  1234.00000    ,  56789.0000    )
       > logicals before swap  T F
       > logicals after swap   F T
       > strings before swap First string    The other string
       > strings after swap The other stringFirst string
       > swap two vectors
       >one before: 1,1,1,1,1,1,1,1,1,1,1,1,1
       >two before: 2,2,2,2,2,2,2,2,2,2,2,2,2
       >one after: 2,2,2,2,2,2,2,2,2,2,2,2,2
       >two after: 1,1,1,1,1,1,1,1,1,1,1,1,1
       > given these arrays initially each time
       >1,1,1
       >1,1,1
       >1,1,1
       >
       >2,2,2
       >2,2,2
       >2,2,2
       > swap two rows
       >1,1,1
       >2,2,2
       >1,1,1
       >
       >2,2,2
       >2,2,2
       >1,1,1
       > swap two columns
       >1,2,1
       >1,2,1
       >1,2,1
       >
       >2,1,2
       >2,1,2
       >2,1,2
       > swap two arrays with same number of elements
       >2,2,2
       >2,2,2
       >2,2,2
       >
       >1,1,1
       >1,1,1
       >1,1,1



-----------------------------------------------------------------------------------------------------------------------------------

                                                             swap (3)                                                 July 02, 2017

Generated by manServer 1.08 from f88ecf43-b5fe-427f-bd9c-ae4f7b290ef5 using man macros.
                                                              [swap]
