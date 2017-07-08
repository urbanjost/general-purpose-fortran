!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
Module M_swap
implicit none
integer,parameter :: cd=kind(0.0d0)
private
public swap
!===================================================================================================================================
character(len=*),parameter :: ident1='@(#)M_swap::swap(3f): swap two variables of like type (real,integer,complex,character,double)'
interface swap
   module procedure r_swap, i_swap, c_swap, s_swap, d_swap, l_swap, cd_swap
end interface
!   SWAP is a Generic Interface in a module with PRIVATE specific procedures.
!   This means the individual subroutines cannot be called from outside of the M_sort(3fm) module.
!
!    o  procedure names are declared private in this module so they are not accessable except by their generic name
!    o  procedures must include a "use M_swap" to access the generic name "swap"
!    o  if these routines are recompiled, routines with the USE statement should then be recompiled and reloaded.
!===================================================================================================================================
!>
!!##NAME
!!      swap(3f) - [M_swap]elemental subroutine swaps two standard type variables of like type
!!##SYNOPSIS
!!
!!      subroutine swap(X,Y)
!!##DESCRIPTION
!!    Generic subroutine SWAP(GEN1,GEN2) swaps two variables of like type
!!    (real, integer, complex, character, double, logical).
!!
!!    On output, the values of X and Y have been interchanged.
!!    Swapping is commonly required in procedures that sort data.
!!
!!    SWAP(3f) is elemental, so it can operate on vectors and arrays as well
!!    as scalar values.
!!
!!##EXAMPLE
!!
!!   Example program:
!!
!!    program try_swap
!!    use M_swap, only : swap
!!    integer             :: iarray(2)=[10,20]
!!    real                :: rarray(2)=[11.11,22.22]
!!    doubleprecision     :: darray(2)=[1234.56789d0,9876.54321d0]
!!    complex             :: carray(2)=[(1234,56789),(9876,54321)]
!!    logical             :: larray(2)=[.true.,.false.]
!!    character(len=16)   :: string(2)=["First string    ","The other string"]
!!
!!    integer             :: one(13)=1
!!    integer             :: two(13)=2
!!
!!    integer             :: one2(3,3)=1
!!    integer             :: two2(3,3)=2
!!
!!       print *, "integers before swap ", iarray
!!       call swap (iarray(1), iarray(2))
!!       print *, "integers after swap  ", iarray
!!
!!       print *, "reals before swap ", rarray
!!       call swap (rarray(1), rarray(2))
!!       print *, "reals after swap  ", rarray
!!
!!       print *, "doubles before swap ", darray
!!       call swap (darray(1), darray(2))
!!       print *, "doubles after swap  ", darray
!!
!!       print *, "complexes before swap ", carray
!!       call swap (carray(1), carray(2))
!!       print *, "complexes after swap  ", carray
!!
!!       print *, "logicals before swap ", larray
!!       call swap (larray(1), larray(2))
!!       print *, "logicals after swap  ", larray
!!
!!       print *, "strings before swap ", string
!!       call swap (string(1), string(2))
!!       print *, "strings after swap ", string
!!
!!       write(*,*)'swap two vectors'
!!       write(*,'("one before: ",*(i0,:","))') one
!!       write(*,'("two before: ",*(i0,:","))') two
!!       call swap(one,two)
!!       write(*,'("one after: ",*(i0,:","))') one
!!       write(*,'("two after: ",*(i0,:","))') two
!!
!!       write(*,*)'given these arrays initially each time '
!!       one2=1
!!       two2=2
!!       call printarrays()
!!
!!       write(*,*)'swap two rows'
!!       one2=1
!!       two2=2
!!       call swap(one2(2,:),two2(3,:))
!!       call printarrays()
!!
!!       write(*,*)'swap two columns'
!!       one2=1
!!       two2=2
!!       call swap(one2(:,2),two2(:,2))
!!       call printarrays()
!!
!!       write(*,*)'swap two arrays with same number of elements'
!!       one2=1
!!       two2=2
!!       call swap(one2,two2)
!!       call printarrays()
!!
!!       contains
!!       subroutine printarrays()
!!       integer :: i
!!       do i=1,size(one2(1,:))
!!          write(*,'(*(i0,:","))') one2(i,:)
!!       enddo
!!       write(*,*)
!!       do i=1,size(two2(1,:))
!!          write(*,'(*(i0,:","))') two2(i,:)
!!       enddo
!!       end subroutine printarrays
!!
!!    end program try_swap
!!
!!   Expected Results:
!!
!!    > integers before swap           10          20
!!    > integers after swap            20          10
!!    > reals before swap    11.1099997       22.2199993
!!    > reals after swap     22.2199993       11.1099997
!!    > doubles before swap    1234.5678900000000        9876.5432099999998
!!    > doubles after swap     9876.5432099999998        1234.5678900000000
!!    > complexes before swap  (  1234.00000    ,  56789.0000    ) (  9876.00000    ,  54321.0000    )
!!    > complexes after swap   (  9876.00000    ,  54321.0000    ) (  1234.00000    ,  56789.0000    )
!!    > logicals before swap  T F
!!    > logicals after swap   F T
!!    > strings before swap First string    The other string
!!    > strings after swap The other stringFirst string
!!    > swap two vectors
!!    >one before: 1,1,1,1,1,1,1,1,1,1,1,1,1
!!    >two before: 2,2,2,2,2,2,2,2,2,2,2,2,2
!!    >one after: 2,2,2,2,2,2,2,2,2,2,2,2,2
!!    >two after: 1,1,1,1,1,1,1,1,1,1,1,1,1
!!    > given these arrays initially each time
!!    >1,1,1
!!    >1,1,1
!!    >1,1,1
!!    >
!!    >2,2,2
!!    >2,2,2
!!    >2,2,2
!!    > swap two rows
!!    >1,1,1
!!    >2,2,2
!!    >1,1,1
!!    >
!!    >2,2,2
!!    >2,2,2
!!    >1,1,1
!!    > swap two columns
!!    >1,2,1
!!    >1,2,1
!!    >1,2,1
!!    >
!!    >2,1,2
!!    >2,1,2
!!    >2,1,2
!!    > swap two arrays with same number of elements
!!    >2,2,2
!!    >2,2,2
!!    >2,2,2
!!    >
!!    >1,1,1
!!    >1,1,1
!!    >1,1,1
!===================================================================================================================================
!===================================================================================================================================
!>
!! DESCRIPTION: swap(3f):subroutine swaps two variables of like type (real,integer,complex,character,double)
!!##VERSION:     1.0 19970201
!! AUTHOR:      John S. Urban
!===================================================================================================================================
!===================================================================================================================================
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
elemental subroutine d_swap(x,y)
character(len=*),parameter::ident="@(#)M_swap::d_swap(3fp): swap two double variables"
doubleprecision, intent(inout) :: x,y
doubleprecision                :: temp
   temp = x; x = y; y = temp
end subroutine d_swap
!===================================================================================================================================
elemental subroutine r_swap(x,y)
character(len=*),parameter::ident="@(#)M_swap::d_swap(3fp): swap two real variables"
real, intent(inout) :: x,y
real                :: temp
   temp = x; x = y; y = temp
end subroutine r_swap
!===================================================================================================================================
elemental subroutine i_swap(i,j)
character(len=*),parameter::ident="@(#)M_swap::d_swap(3fp): swap two integer variables"
integer, intent(inout) :: i,j
integer                :: itemp
   itemp = i; i = j; j = itemp
end subroutine i_swap
!===================================================================================================================================
elemental subroutine l_swap(l,ll)
character(len=*),parameter::ident="@(#)M_swap::d_swap(3fp): swap two logical variables"
logical, intent(inout) :: l,ll
logical                :: ltemp
   ltemp = l; l = ll; ll = ltemp
end subroutine l_swap
!===================================================================================================================================
elemental subroutine c_swap(xx,yy)
character(len=*),parameter::ident="@(#)M_swap::d_swap(3fp): swap two complex variables"
complex, intent(inout) :: xx,yy
complex                :: tt
   tt = xx; xx = yy; yy = tt
end subroutine c_swap
!===================================================================================================================================
elemental subroutine cd_swap(xx,yy)
character(len=*),parameter::ident="@(#)M_swap::cd_swap(3fp): swap two double complex variables"
complex(kind=cd), intent(inout) :: xx,yy
complex(kind=cd)                :: tt
   tt = xx; xx = yy; yy = tt
end subroutine cd_swap
!===================================================================================================================================
elemental subroutine s_swap(string1,string2)

!>
!!   F90 NOTE:
!!    string_temp is an automatic character object whose size is not a constant expression.
!!    Automatic objects cannot be saved or initialized.
!!    Note that the len of a dummy argument can be used to calculate the automatic variable length.
!!    Therefore, you can make sure len is at least max(len(string1),len(string2)) by adding the two lengths together:
!===================================================================================================================================

character(len=*),parameter::ident="@(#)M_swap::d_swap(3fp): swap two double variables"
character(len=*), intent(inout)             :: string1,string2
!character( len=len(string1) + len(string2)) :: string_temp
character( len=max(len(string1),len(string2))) :: string_temp
   string_temp = string1; string1 = string2; string2 = string_temp
end subroutine s_swap
!===================================================================================================================================
end Module M_swap
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
