module M_compiler
use, intrinsic :: iso_fortran_env, only : compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_options
implicit none
private
public :: say_hello
character(len=*),parameter :: all='(*(g0,1x))'
contains

subroutine say_hello
integer                      :: argument_length, istat
character(len=:),allocatable :: progname, options
   call get_command_argument(number=0,length=argument_length)
   if(allocated(progname))deallocate(progname)
   allocate(character(len=argument_length) :: progname)
   call get_command_argument (0, progname, status=istat)
   if (istat == 0) then
      print all, "The program name is " // trim (progname)
   else
      print all, "Could not get the program name " // trim (progname)
   endif
   print all
   print all, 'This file was compiled by', compiler_version()
   print all
   options=replace(' '//compiler_options(),' -',char(10)//' -')
   write(*,all)'using the options', options
   write(*,all)
end subroutine say_hello

! stripped-down REPLACE(3f) from M_strings(3f)
function replace(targetline,old,new,ierr) result (newline)
character(len=*),intent(in)            :: targetline
character(len=*),intent(in)            :: old, new
integer,intent(out),optional           :: ierr
character(len=:),allocatable           :: newline, old_local_for_comparison
character(len=:),allocatable           :: targetline_for_comparison, targetline_local
integer                                :: icount,ichange, ic, ichr ,original_input_length
integer                                :: len_old, len_new, ladd,right_margin,ind
logical                                :: flip
   flip=.false.
   original_input_length=len_trim(targetline)
   targetline_for_comparison=targetline
   old_local_for_comparison=old
   targetline_local=targetline
   icount=0
   ichange=0
   len_old=len(old)
   len_new=len(new)
   right_margin=len(targetline)
   newline=''
   if(len_old == 0)then
      ichr=len_new + original_input_length
      if(len_new > 0)then
         newline=new(:len_new)//targetline_local(1:original_input_length)
      else
         newline=targetline_local(1:original_input_length)
      endif
      ichange=1
      if(present(ierr))ierr=ichange
      if(flip) newline=reverse(newline)
      return
   endif
   ichr=1
   ic=1
   loop: do
      ind=index(targetline_for_comparison(ic:),old_local_for_comparison(:len_old))+ic-1
      if(ind == ic-1.or.ind > right_margin)then
         exit loop
      endif
      icount=icount+1
      if(ind > ic)then
         ladd=ind-ic
         newline=newline(:ichr-1)//targetline_local(ic:ind-1)
         ichr=ichr+ladd
      endif
      if(icount >= 1.and.icount <= original_input_length )then
         ichange=ichange+1
         if(len_new /= 0)then
            newline=newline(:ichr-1)//new(:len_new)
            ichr=ichr+len_new
         endif
      else
         if(len_old /= 0)then
            newline=newline(:ichr-1)//old(:len_old)
            ichr=ichr+len_old
         endif
      endif
      ic=ind+len_old
   enddo loop
   select case (ichange)
   case (0)
      newline=targetline_local
   case default
      if(ic <= len(targetline))then
         newline=newline(:ichr-1)//targetline_local(ic:max(ic,original_input_length))
      endif
   end select
   if(present(ierr))ierr=ichange
   if(flip) newline=reverse(newline)
end function replace

elemental function reverse(string) result (rev)
character(len=*),intent(in)  :: string
character(len=len(string))   :: rev
integer                      :: length
integer                      :: i
   length = len(string)
   do i = 1,length
      rev(i:i)=string(length-i+1:length-i+1)
   enddo
end function reverse

end module M_compiler
