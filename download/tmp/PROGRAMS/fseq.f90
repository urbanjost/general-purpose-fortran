subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   fseq - print a sequence of numbers, optionally appending file lines.         ',&
'SYNOPSIS                                                                        ',&
'   fseq [OPTION]...                                                             ',&
'DESCRIPTION                                                                     ',&
'  A test program for testing M_kracken(3fm) module. Also useful for experimenting',&
'  with Fortran FORMAT statements and observing  round-off issues.               ',&
'                                                                                ',&
'  Sort of parts of "cat -n"  and "seq" and "head" mixed together.               ',&
'  But awk(1) or sed(1) or shells would do it better.                            ',&
'  Logic has gotten very messy, and has not been made fault tolerant.            ',&
'                                                                                ',&
'   Print numbers from START to END, in steps of DELTA.                          ',&
'       -s START     # start value (defaults to 1)                               ',&
'       -d DELTA     # increment value (defaults to 1)                           ',&
'       -e END       # end value (required)                                      ',&
'       -f FORMAT    # use Fortran floating-point FORMAT                         ',&
'       -i FORMAT    # use Fortran integer FORMAT (defaults to "i0,/")           ',&
'       -h           # display this help and exit                                ',&
'       -v           # output version information and exit                       ',&
'       -oo          # filename (ignored if active FORMAT is *)                  ',&
'                    # lines are read from this file and appended to output      ',&
'                    # lines.                                                    ',&
'   FIRST, DELTA, and END are interpreted as floating point values when the      ',&
'   format from -f is used, they are interpreted as integers when -i is used.    ',&
'   -f FORMAT must be suitable for printing one argument of type "REAL",         ',&
'   -i FORMAT must be suitable for printing one argument of type "INTEGER".      ',&
'   If -f is present, -i is ignored.                                             ',&
'EXAMPLES                                                                        ',&
'   fseq -e 20                                                                   ',&
'   fseq -s 10 -e 100 -d 10 -f "g5.1'''':''''"                                   ',&
'   # show first 20 lines of file "filename"                                     ',&
'   fseq filename -e 20 -i "i10,'''':'''',1x"                                    ',&
'   # show                                                                       ',&
'   fseq filename -e 1000000000 -i "i0,t5,'''':'''',1x"                          ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    fseq - print a sequence of numbers, optionally appending file lines.
!!##SYNOPSIS
!!
!!    fseq [OPTION]...
!!##DESCRIPTION
!!   A test program for testing M_kracken(3fm) module. Also useful for experimenting
!!   with Fortran FORMAT statements and observing  round-off issues.
!!
!!   Sort of parts of "cat -n"  and "seq" and "head" mixed together.
!!   But awk(1) or sed(1) or shells would do it better.
!!   Logic has gotten very messy, and has not been made fault tolerant.
!!
!!    Print numbers from START to END, in steps of DELTA.
!!        -s START     # start value (defaults to 1)
!!        -d DELTA     # increment value (defaults to 1)
!!        -e END       # end value (required)
!!        -f FORMAT    # use Fortran floating-point FORMAT
!!        -i FORMAT    # use Fortran integer FORMAT (defaults to "i0,/")
!!        -h           # display this help and exit
!!        -v           # output version information and exit
!!        -oo          # filename (ignored if active FORMAT is *)
!!                     # lines are read from this file and appended to output
!!                     # lines.
!!    FIRST, DELTA, and END are interpreted as floating point values when the
!!    format from -f is used, they are interpreted as integers when -i is used.
!!    -f FORMAT must be suitable for printing one argument of type "REAL",
!!    -i FORMAT must be suitable for printing one argument of type "INTEGER".
!!    If -f is present, -i is ignored.
!!##EXAMPLES
!!
!!    fseq -e 20
!!    fseq -s 10 -e 100 -d 10 -f "g5.1'':''"
!!    # show first 20 lines of file "filename"
!!    fseq filename -e 20 -i "i10,'':'',1x"
!!    # show
!!    fseq filename -e 1000000000 -i "i0,t5,'':'',1x"
!===================================================================================================================================
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        CLI library utilities and examples>',&
'@(#)PROGRAM:        fseq(1f)>',&
'@(#)DESCRIPTION:    generate a sequence of numbers, optionally concatenated to a file>',&
'@(#)VERSION:        1.0, 2011-01-30>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:03:18 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
program fseq
use m_kracken
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=255)  :: format   ! format statement to write real numbers with
   character(len=255)  :: iformat  ! format statement to write integers with
   character(len=255)  :: filename ! append lines of this file after numbers
   character(len=1024) :: line     ! assume input file lines fit in this
   logical help                    ! see if -h flag was used
   logical version                 ! see if -v flag was used
!-----------------------------------------------------------------------------------------------------------------------------------
!  define command and default argument values and read command line arguments
   call kracken('fseq', '-s 1 -d 1 -e 0 -i "i0,/" -f BLANK -help .false. -version .false.')
   call help_usage(lget('fseq_help'))
   call help_version(lget('fseq_version'))
!-----------------------------------------------------------------------------------------------------------------------------------
!  get desired argument values
   start   = rget('fseq_s')
   end     = rget('fseq_e')
   delta   = rget('fseq_d')

!  put parenthesis around the formats

   format  = '('//sget('fseq_f')
   iend=min(len(format),len_trim(format)+1) ! optimistically assume it fits
   format(iend:iend)=')'

   iformat  = '('//sget('fseq_i')
   iiend=min(len(iformat),len_trim(iformat)+1) ! optimistically assume it fits
   iformat(iiend:iiend)=')'

!  could do internal write with the format and check error status to verify it

!  get an optional filename to read lines from and append to the numbers

   filename  = sget('fseq_oo')
   iin=5
   if(filename.eq.'-')then
     iin=5
   elseif(filename.ne.' ')then
     iin=10
     open(unit=10,file=filename(:len_trim(filename))) ! optimistically assume it opens
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(format.eq.'(BLANK)')then                  ! integer format
      istart=int(start)
      iend=int(end)
      idelta=int(delta)
      do i=istart,iend,idelta
         if(filename.ne.'')then
            read(iin,'(a)',iostat=ios)line
            if(ios.ne.0)exit
            write(*,iformat,advance='no')i
            write(*,'(a)',advance='yes')line(:len_trim(line))
         else
            write(*,iformat,advance='no')i
         endif
      enddo
      n2l=len_trim(iformat)-1              ! always at least two
      if(iformat(n2l:n2l).ne.'/'.and.filename.eq.' ')write(*,*)
!-----------------------------------------------------------------------------------------------------------------------------------
!  a DO statement with REAL parameters is frowned upon
   elseif(delta.gt.0)then
      do
         if(start.gt.end+delta/2)exit  ! round-off problem here
         if(format.eq.'()')format='(i0,t7)'

         if(filename.ne.'')then
            read(iin,'(a)',iostat=ios)line
            if(ios.ne.0)exit
            write(*,format,advance='no')start
            write(*,'(a)',advance='yes')line(:len_trim(line))
         else
            write(*,format,advance='no')start
         endif
         start=start+delta
      enddo
      n2l=len_trim(format)-1
      if(format(n2l:n2l).ne.'/'.and.format.ne.'()'.and.filename.eq.' ')write(*,*)
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(delta.lt.0)then
      do
         if(start.lt.end+delta/2)exit  ! round-off problem here
         if(format.eq.'()') format='(g20.13)'

         if(filename.ne.'')then
            read(iin,'(a)',iostat=ios)line
            if(ios.ne.0)exit
            write(*,format,advance='no')start
            write(*,'(a)',advance='yes')line(:len_trim(line))
         else
            write(*,format,advance='no')start
         endif
         start=start+delta
      enddo
      n2l=len_trim(format)-1
      if(format(n2l:n2l).ne.'/'.and.format.ne.'()'.and.filename.eq.' ')write(*,*)
!-----------------------------------------------------------------------------------------------------------------------------------
   else ! that would have taken a long time
      write(*,*)'$fseq# error: I do not have that much time'
      write(*,*)'-h (help)           ',help
      write(*,*)'-v (version)        ',version
      write(*,*)'-s (start)          ',start
      write(*,*)'-e (end)            ',end
      write(*,*)'-d (delta)          ',delta
      write(*,*)'-f (real format)    ',format(:len_trim(format))
      write(*,*)'-i (integer format) ',iformat(:len_trim(iformat))
!-----------------------------------------------------------------------------------------------------------------------------------
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end program fseq
