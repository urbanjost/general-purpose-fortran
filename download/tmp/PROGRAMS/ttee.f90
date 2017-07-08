!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program ttee ! @(#) ttee(1) writes stdin to stdout and another file with an optional timestamp prefix
   use iso_fortran_env, only : iostat_end
   use m_kracken,only        : kracken,lget,sget,retrev,IPvalue   ! command line parameter cracking module
   use m_debug, only         : stderr
   use m_time, only          : now,fmtdate_usage
   use m_strings, only       : v2s, chomp
   implicit none
   character(len=IPvalue)       :: string                    ! hold line read from input
   character(len=IPvalue)       :: timestamp                 ! hold value of --timestamp option
   character(len=IPvalue)       :: prefix                    ! prefix string
   character(len=10)            :: access                    ! whether to append or overwrite output file
   character(len=IPvalue)       :: file                      ! output filenames
   integer                      :: outfile                   ! unit number for output file
   integer                      :: ios                       ! value of iostat on i/o errors
   integer                      :: ilen,ier
   logical                      :: stamp_stdout,stamp_output
   integer                      :: len1,len2,len3            ! scratch variables for accumulating output filenames
   integer                      :: i10                       ! counter for looping through file names
   character(len=IPvalue)       :: format                    ! alternate format for the time stamp using the now(3f) function
   integer                      :: ii                        ! length of trimmed format
   character(len=*),parameter   :: delimiters=' '//char(9)//char(13)//char(10) ! token delimiters (space, tab, return, line feed)
   character(len=:),allocatable :: token
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  crack command line
   call kracken('ttee','            &
   & -o -output                     &
   & -a .F. -append .F.             &
   & -t "#N#" -timestamp "#N#"      &
   & -help .F.                      &
   & -fmt %Y/%M/%D %h:%m:%s.%x>     &
   & -version .F.                   &
   & ')
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   call help_version(lget('ttee_version')) !  display version number if --version is present
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  display help text and exit if --help is present
   if(lget('ttee_help'))then
      call usage()
      stop
   endif
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  get time format
   call retrev('ttee_fmt', format, ii, ier)
   ! assuming timestamp has same length as current timestamp , which might not be if change now(3f)
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  decide whether to append to output file or overwrite it if -a or --append is present
   access='sequential'
   if(lget('ttee_a'))then
      access='append'
   endif
   if(lget('ttee_append'))then
      access='append'
   endif
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  open optional output file ... simply append FILENAME, -o FILENAME, -output FILENAME
   file=' '
   call retrev('ttee_oo', file, len1, ier)              ! get any filename before any keywords
   len2=min(IPvalue,len1+2)
   call retrev('ttee_o', file(len2:), len1, ier)        ! append any filenames after -o keyword
   len2=min(IPvalue,len2+2+len1+2)
   call retrev('ttee_output', file(len2:), len1, ier)   ! append any filenames after -output keyword
   len3=len_trim(file)                                  ! length of appended filenames
   if(len3.ne.0)then
      outfile=9                                         ! initialize value used to get unit numbers for output files
                                                        ! get list of filename separators
      do while ( chomp(file,token,delimiters).ge. 0)  ! open each filename
         outfile=outfile+1
         open(unit=outfile,file=token,access=access,iostat=ios)
         if(ios.ne.0)then
            call stderr('error opening output,iostat='//v2s(ios))
            exit
         endif
      enddo
   else
      outfile=-1                                        ! flag there is no output file specified
   endif
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  set prefix length to 0 or PLEN depending on whether --timestamp value flags file to have timestamp prefix
   stamp_stdout=.true. ! prefix stdout
   stamp_output=.true. ! prefix outfiles
   prefix=''    ! initialize prefix string

   call retrev('ttee_t', timestamp, ilen, ier)
   if(timestamp.eq.'#N#')then
      call retrev('ttee_timestamp', timestamp, ilen, ier)
   endif
   select case(timestamp(:ilen))
   case('all','','#N#')
      stamp_stdout=.true.
      stamp_output=.true.
   case('stdout')
      stamp_stdout=.true.
      stamp_output=.false.
   case('output')
      stamp_stdout=.false.
      stamp_output=.true.
   case('none')
      stamp_stdout=.false.
      stamp_output=.false.
   case default
      call stderr('unknown timestamp value [stdout|all|output|none]')
      stop
   end select
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  loop reading stdin till end-of-file or error and write to stdout and output file with optional timestamp prefix
   infinite: do
      prefix=now(format(:ii))
      ios=0
      read(*,'(a)',iostat=ios) string
      if(ios.ne.0)then
         if(ios.ne.iostat_end)then
            call stderr('error reading from stdin, iostat='//v2s(ios))
         endif
         exit infinite
      endif

      ios=0
      ilen=len_trim(string)
      if(stamp_stdout)then
         write(*,'(a,a)',iostat=ios)   trim(prefix),string(:ilen)
      else
         write(*,'(a)',iostat=ios)   string(:ilen)
      endif
      if(ios.ne.0)then
         call stderr('error writing to stdout,iostat='//v2s(ios))
         exit infinite
      endif

      ios=0
      if(outfile.ge.0)then
         if(stamp_output)then
            do i10=10,outfile
               write(i10,'(a,a)',iostat=ios)  trim(prefix),string(:ilen)
            enddo
         else
            do i10=10,outfile
               write(i10,'(a)',iostat=ios) string(:ilen)
            enddo
         endif
      endif
      if(ios.ne.0)then
         call stderr('error writing to output,iostat='//v2s(ios))
         exit infinite
      endif

   enddo infinite
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   stop
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine usage() ! "@(#) usage(3f90) writes program help to stdout and exits
!character(len=132),parameter :: text(:)= [&
character(len=132),allocatable :: text(:)
integer                        :: i
text= [&
&'NAME                                                                            ', &
&'   ttee(1f) - [TIME] write input to stdout and a file with timing info.         ', &
&'                                                                                ', &
&'SYNOPSIS                                                                        ', &
&'   ttee [[-o|--output] filename(s)] [-a|--append] [--timestamp FLAG]] ...       ', &
&'        [-fmt FORMAT] [--help |--version]                                       ', &
&'                                                                                ', &
&'DESCRIPTION                                                                     ', &
&'   Read from standard input and write to standard output and files              ', &
&'   with an optional timestamp in front of each line.                            ', &
&'                                                                                ', &
&'   -o|--output FILENAME(S)                                                      ', &
&'         specify name of output file(s). If the filenames are first the         ', &
&'         keyword -o|--output is optional.                                       ', &
&'                                                                                ', &
&'   -a|--append                                                                  ', &
&'         append to the given output file(s), do not overwrite                   ', &
&'                                                                                ', &
&'   -t|--timestamp FLAG                                                          ', &
&'         which files to add the timestamp to. Default is "all"                  ', &
&'         Allowed values are stdout, output, all, none.                          ', &
&'                                                                                ', &
&'   -fmt FORMAT                                                                  ', &
&'         Change format for timestamp prefix using a call to now(3f).            ', &
&'                                                                                ', &
&'CALL FMTDATE_USAGE                                                              ', &
&'   --help     display this help and exit                                        ', &
&'   --version  output version information and exit                               ', &
&'LIMITS                                                                          ', &
&'    Program limits:                                                             ', &
&'                                                                                ', &
&'    o  Input line width maximum is 1024 characters.                             ', &
&'    o  Maximum length of output filenames is 4098, individual filename is 1024. ', &
&'    o  Minimum number of output files is probably at least 90; but is           ', &
&'       system dependent.                                                        ', &
&'                                                                                ', &
&'EXAMPLES                                                                        ', &
&'   Basic command usage:                                                         ', &
&'                                                                                ', &
&'    # write stdout of "program" to ttee.out with a timestamp and stdout         ', &
&'    program|ttee --output ttee.out --timestamp output|grep -i iteration         ', &
&'                                                                                ', &
&'    # write stdout of "program" to log.txt and stdout with a Julian Day         ', &
&'    program|ttee log.txt -fmt "%J :"                                            ', &
&'    2457565.488 :Iteration 1 : Error: 1.20                                      ', &
&'    2457565.558 :Iteration 2 : Error: 0.08                                      ', &
&'    2467569.684 :Iteration 3 : Error: 1.2e-3                                    ', &
&'                                                                                ']
do i=1,SIZE(text)
   select case (text(i))
   case('CALL FMTDATE_USAGE')
      call fmtdate_usage(10)
   case default
      write(*,'(a)')trim(text(i))
   end select
enddo
end subroutine usage
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
'@(#)PROGRAM:        ttee(1f)>',&
'@(#)DESCRIPTION:    split stdout to a file with optional timestamp labeling>',&
'@(#)VERSION:        1.0, 20150913>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COPYRIGHT:      Copyright (C) 2009 John S. Urban>',&
'@(#)LICENSE:        This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:06:47 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
end program ttee
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
