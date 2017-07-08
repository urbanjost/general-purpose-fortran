SUPPLEMENTAL_SOURCE(){
#   create user-supplied procedures "c" and "juown1" required for the calculator module
echo 'create c.f90' 1>&2
cat >tmp/c.f90 <<\EOF
!===================================================================================================================================
real function c(fval,n) ! curve number, data point subscript, file number
   c=0.0
end function c
!===================================================================================================================================
EOF
#----------------------------------------------------------------------------------------------------------------------------------#
echo 'create juown1.f90' 1>&2
cat > tmp/juown1.f90 <<\EOF
!===================================================================================================================================
subroutine juown1(func,iflen,args,iargstp,n,fval,ctmp,ier)
! if the function owncode(1) is called this subroutine can be accessed to do user-written functions.
use M_journal, only     : journal
use m_calculator, only  : x, y, icname_calc, valuer, values
!use m_calculator, only : iclen_calc, stuff, getvalue
use M_strings, only     : lower
use M_process, only     : process_readall
implicit none
character(len=*),parameter :: ident="@(#)juown1(3f): extend functions available to the calculator routine "
integer,parameter          :: dp=kind(0.0d0)
character(len=*)           :: func
integer                    :: iflen
real(kind=dp)              :: args(100)
integer                    :: iargstp(100)
integer                    :: n
real(kind=dp)              :: fval
character(len=*)           :: ctmp
integer                    :: ier
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                    :: i10, ilen, i, ii
   character(len=icname_calc) :: func2
   integer                    :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   fval=0
!-----------------------------------------------------------------------------------------------------------------------------------
   func2(1:iflen)=lower(func(:iflen))
   call calc_NC(func2(:iflen),args,iargstp,n,fval,i,ier) ! look this name up to see if it is a ncurses function
   ! func2(:iflen)   --> function name to look for
   ! args            --> parameters for function
   ! iargstp         --> type of parameters
   ! n               --> number of arguments to use in args(:)
   ! fval           <--  calculated value if function found
   ! i              <--> i=0 if function not found
   ! ier            <--> ier=-1 if name not found or wrong number of parameters
   !                         -2 values out of range
   if(i.gt.0)then                                ! match found
      if(ier.eq.-1)goto 9991                     ! improper number of parameters or function name not matched
      return                                     ! match found and value calculated
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   select case(func2)
!-----------------------------------------------------------------------------------------------------------------------------------
   case('$sh')! $sh gets one line of output from a shell command
      ctmp=' '
      if(n.gt.0)then
         ii=int(args(1)+0.5)
         ilen=valuer(ii)
         ctmp=process_readall( values(ii)(:ilen),ierr=ier)
      else
         ilen=0
      endif
      fval=len_trim(ctmp)
      fval=max(1.0d0,fval)
      ier=2
      goto 9999
!-----------------------------------------------------------------------------------------------------------------------------------
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
9991 continue
   call journal('*juown1* unknown function')
   call journal('function name is ........'//func(1:iflen)) ! some machines cannot concatenate a string being passed as an argument
   call journal('sc','function name length is..',iflen)
   call journal('sc','number of arguments .....',n)
   write(*,*)(args(i10),i10=1,n,1)
   return
9999 continue
end subroutine juown1
EOF
}
SUPPLEMENTAL_SOURCE
