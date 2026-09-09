module M_datapac
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit,  stdout=>output_unit, stderr=>error_unit
use,intrinsic :: iso_fortran_env, only : real32, real64
! create generics
use M_datapac__s
use M_datapac__d
public :: label
integer,parameter,private :: G_IO=stdout  ! IO LUN for all write statements

contains

subroutine label(string)
character(len=*),intent(in) :: string
integer                     :: more
integer                     :: slen
intrinsic                   :: max, trim, repeat

   slen=len_trim(string)
   more=max(0,80-slen-2)/2
   write(g_io,'(1x,a)')repeat('=',79)
   write(g_io,'(1x,a)')repeat('=',more)//' '//string(:slen)//' '//repeat('=',80-more-3-slen)
   write(g_io,'(1x,a)')repeat('=',79)

end subroutine label

end module M_datapac
