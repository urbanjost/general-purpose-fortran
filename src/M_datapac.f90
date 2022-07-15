module M_datapac
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit,  stdout=>output_unit, stderr=>error_unit
use M_datapac_s
use M_datapac_d
public :: label
private :: G_io
integer, save           :: G_IO=stdout  ! IO LUN for all write statements
contains

subroutine label(string)
character(len=*),intent(in) :: string
integer                     :: more
integer                     :: slen
intrinsic                   :: max, trim, repeat

   slen=len_trim(string)
   more=max(0,80-slen-2)/2
   write(*,'(1x,a)')repeat('=',79)
   write(*,'(1x,a)')repeat('=',more)//' '//string(:slen)//' '//repeat('=',80-more-3-slen)
   write(*,'(1x,a)')repeat('=',79)

end subroutine label

end module M_datapac
