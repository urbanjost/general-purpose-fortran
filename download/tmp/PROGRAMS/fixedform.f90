!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
program fifo
   use M_kracken
   use M_fixedform
   character(255) filename
   character(len=20) :: device
   logical lval
!  define command arguments, default values
!  and crack command line
   call kracken('cmd','-i 10 -r 10e3 -l .false. -f test.dat -d x11')
!  get values. Allow filename as -oo or -f
   call retrev('cmd_oo',filename,iflen,ier) ! get FILENAME
   if(filename.eq.' ')then
      call retrev('cmd_f',filename,iflen,ier)                      ! get -f FILENAME
   endif
   lval = lget('cmd_l')                    ! get -l present?
   rval = rget('cmd_r')                    ! get -r RVAL
   ival = iget('cmd_i')                    ! get -i IVAL
   device = sget('cmd_i')                  ! get -d STRING
!  all done parsing; do something with the values
!  print *, "filename=",filename(:iflen)
!  print *, " i=",ival, " r=",rval, " l=",lval
!  print *, " d="//device
   page_ptr=>page_pd
   icount_ptr=>icount_pd
   call loaddata(filename)      ! fill the page(*) with user data
   call fixedform()
end program fifo
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
