subroutine help_usage(l_help)
implicit none
! @(#)help_usage(3f): prints help information
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   model(1f) - [FORTRAN:iso_fortran_env] fortran numeric model information      ',&
'   (LICENSE:PD)                                                                 ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   model                                                                        ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   The following routines are called for various types:                         ',&
'                                                                                ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    model(1f) - [FORTRAN:iso_fortran_env] fortran numeric model information
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    model
!!
!!##DESCRIPTION
!!    The following routines are called for various types:
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
program model
!!use,intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
use,intrinsic :: iso_fortran_env, only : ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT
use,intrinsic :: iso_fortran_env, only : real32, real64, real128, int8, int16, int32, int64
use,intrinsic :: iso_fortran_env, only : integer_kinds, real_kinds

integer, parameter :: io=OUTPUT_UNIT

   write(*,*)'ERROR_UNIT=',error_unit
   write(*,*)'INPUT_UNIT=',input_unit
   write(*,*)'OUTPUT_UNIT=',output_unit

   write(*,*)'INTEGER_KINDS()=',INTEGER_KINDS
   write(*,*)'REAL_KINDS()=',REAL_KINDS

write(io,'(a)')'   huge(x)   returns the largest number that is not an infinity in the model of the type of X.'
write(io,'(a)')'   tiny(x)'

   print *, huge(0)
   print *, huge(0.0),tiny(0.0)
   print *, huge(0.0d0),tiny(0.0d0)

   print *, huge(0_int8)
   print *, huge(0_int16)
   print *, huge(0_int32)
   print *, huge(0_int64)

   print *, huge(0.0_real32),tiny(0.0_real32)
   print *, huge(0.0_real64),tiny(0.0_real64)
   print *, huge(0.0_real128),tiny(0.0_real128)

end program model
