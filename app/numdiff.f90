! (Regression testing)
!  numdiff(1): Detect numeric changes in output files
!
!  A simple Fortran program for comparing two files for numeric differences.
!  numdiff(1) is typically used to detect changes in numeric results in ported or recompiled routines.
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
program numdiff
! These routines are available for general use. If you change this code, please acknowledge the original author
      use M_CLI2, only : set_args,dget,lget,iget,sget,rget     ! module for cracking command line parameters
      implicit none
      character(len=:),allocatable :: filein_old              ! name of template file
      character(len=:),allocatable :: filein_new              ! name of new file to test against template
      logical                :: verbose=.false.               ! flag if -verbose option has been specified
      integer                :: idigits                       ! number of significant digits to require as equal
      doubleprecision        :: tolerance                     ! percentage delta to report if idigits is 0
      integer                :: ier                           ! flag indicating if an error occurred
      integer                :: significant_digits            ! number of significant digits in a dble
      character(len=132)     :: warning                       ! string used for writing error messages
      integer                :: GLOBAL_COUNT=0
      real                   :: margin
      character(len=:),allocatable :: help_text(:)
      character(len=:),allocatable :: version_text(:)
!-----------------------------------------------------------------------------------------------------------------------------------
!     define the command options and default values and apply arguments from user command line
      call setup()
      call set_args(' --percent 0.0001d0 --digits 0 --margin 0 --old " " --new " "',help_text,version_text)

      filein_old=sget("old")                                           ! get -old filein_old
      filein_new=sget("new")                                           ! get -new filein_new

      verbose=lget("verbose")                                          ! get -verbose

      tolerance=dget("percent")                                        ! get -percent TOLERANCE
      idigits=iget("digits")                                           ! get -digits NNN
      margin=rget("margin")                                            ! get -margin XXX.XX

!-----------------------------------------------------------------------------------------------------------------------------------
      if(filein_old.eq.' '.or.filein_new.eq.' ')then                           ! if filenames are not specified stop
         print *, "*numdiff* error: old and new filenames are required."       ! report missing filename
         stop 10
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if(idigits.ne.0)then                                     ! check for inappropriate number of digits to test that is too small
         significant_digits=int(log10(2.0**digits(0.0d0)))     ! MAXIMUM NUMBER OF SIGNIFICANT DIGITS IN A DOUBLE NUMBER.
         if(idigits.lt.0)then                                  ! check for inappropriate number of digits to test that is too small
            write(warning,'(a,i0)')'*numdiff* bad number of significant digits=',idigits
            call remark(warning)
            idigits=significant_digits                         ! set bad value to maximum allowed for double precision number
         else if(idigits .gt. significant_digits)then          ! check for inappropriate number of digits to test  that is too big
            write(warning,'(a,i0)')'*numdiff* significant digit requested too high=',idigits
            call remark(warning)
            idigits=significant_digits                         ! set bad value to maximum allowed for double precision number
         endif
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      write(*,'(132("-"))')
      print *, "       | *numdiff* numerical differences"                      ! display command options
      print *, "       | old file=",trim(filein_old)
      print *, "       | new file=",trim(filein_new)
      if(idigits.ne.0)then
         write(*,'(a,i0)')"        | digits of precision=",idigits
      elseif(margin.ne.0)then
         write(*,'(a,g0)')"        | relative margin =",margin
      else
         write(*,'(a,g0)')"        | percent threshold =",tolerance,"(%)"
      endif
      write(*,'(132("-"))')
      call num_diff(filein_old,filein_new,tolerance,idigits,margin)            ! compare the files
      write(*,'(132("-"))')
      write(*,*)'Numerical Differences=',GLOBAL_COUNT
      write(*,'(132("-"))')
      if(GLOBAL_COUNT.ne.0)then    ! return command exit value to system where supported
         STOP 1
      endif
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
subroutine num_diff(file_old,file_new,percent_tolerance,idigits,margin)
!     num_diff: compare numbers in otherwise identical files
!
!     Author: John S. Urban
!     Date:   1990 ,2009, 2013
!
!     given two files (width less than 264 characters) who are assumed to be
!     identical except for some numeric values, find matching lines that
!     have values which differ by a certain percentage. assume numbers are
!     delimited by spaces. not totally generic but has potential.
!     originally made to check steam table qa program output
!
!     make sure all numbers are delimited by spaces. "x=10" or "10,20"
!     will not work! could sometimes change "=" to "= ", "," to ", ".
!
!     having some problems with a generic definition of percent change
!     when one of the values is at or near 0.
!
implicit none
! ident_1="@(#)num_diff(3f): compare numbers in otherwise identical files"
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=*),intent(in)    :: file_old
      character(len=*),intent(in)    :: file_new
      doubleprecision,intent(in)     :: percent_tolerance         ! percent threshold
      integer,intent(in)             :: idigits                   ! number of digits  to match
      real,intent(in)                :: margin                    ! relative margin
!-----------------------------------------------------------------------------------------------------------------------------------
      doubleprecision                :: per
      doubleprecision                :: digirank
      doubleprecision                :: marginrank
      character(len=264)             :: lline
      character(len=264)             :: rline
      character(len=264)             :: dline
      logical                        :: not_end_of_file_1
      logical                        :: not_end_of_file_2
      integer                        :: lcount
      integer                        :: ilen
      integer                        :: ilen2
      integer                        :: ios
      integer                        :: ind
      doubleprecision                :: permax
      doubleprecision                :: digirankmax
      doubleprecision                :: marginrankmax
!-----------------------------------------------------------------------------------------------------------------------------------
      open(unit=21,file=file_old(:len_trim(file_old)),status="old",iostat=ios)            ! open template file
      if(ios.ne.0)then
         call remark('*numdiff* error: could not open file specified with -old')
         stop 20
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      open(unit=22,file=file_new(:len_trim(file_new)),status="old",iostat=ios)            ! open file of new values
      if(ios.ne.0)then
         call remark('*numdiff* error: could not open file specified with -new')
         stop 30
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      not_end_of_file_1=.true.                                                            ! set flag not at end of input on old file
      not_end_of_file_2=.true.                                                            ! set flag not at end of input on new file
      lcount=0                                                                            ! number of lines read
      permax=0.0d0                                                                        ! highest percentage difference found
      digirankmax=dble(idigits)                                                           ! highest digit difference found
!-----------------------------------------------------------------------------------------------------------------------------------
      INFINITE: do
         !--------------------------------------------------------------------------------------------------------------------------
         if(not_end_of_file_1)then                                     ! get next line from file 1
            read(21,"(a)",iostat=ios)lline
            if(ios.ne.0)then
               not_end_of_file_1=.false.
               lline=" "
            endif
         endif
         !--------------------------------------------------------------------------------------------------------------------------
         if(not_end_of_file_2)then                                     ! get next line from file 2
            read(22,"(a)",iostat=ios)rline
            if(ios.ne.0)then
               not_end_of_file_2=.false.
               rline=" "
            endif
         endif
         !--------------------------------------------------------------------------------------------------------------------------
         if(.not.(not_end_of_file_1.or.not_end_of_file_2))then         ! hit end of file on input, exit program
            if(idigits.eq.0)then
               write(*,*)"       |maximum difference=",permax,"(%)"
            else
               write(*,*)"       |minimum match=",digirankmax,"(digits)"
            endif
            exit INFINITE
         endif
         !--------------------------------------------------------------------------------------------------------------------------
         lcount=lcount+1                                                  ! count of lines of input successfully read
         if(lline.ne.rline)then                                           ! if lines are not equal parse them and check differences
            if(idigits.ne.0)then
               call digits_diff(lline,rline,dline,digirank,idigits,ier,ilen,ilen2,ind)
               if(ind.ne.0)then
                  digirankmax=min(digirankmax,digirank)
                  write(*,'("--------|",a)')repeat('-',132-9)
                  write(*,"('old     |',a)")trim(lline(:ilen))
                  write(*,"(i8,     '|',a)")lcount,trim(dline(:max(ilen,ilen2)))
                  write(*,"('new     |',a)")trim(rline(:ilen2))
                  write(*,"('>>>>>>>>|minimum match =',g0,'(digits)')")digirank
                  write(*,'("--------|",a)')repeat('-',132-9)
               elseif(verbose)then
                  write(*,"(i8,     '|',a)")lcount,lline(:ilen)
               endif
            elseif(margin.ne.0)then
               call margin_diff(lline,rline,dline,marginrank,margin,ier,ilen,ilen2,ind)
               if(ind.ne.0)then
                  marginrankmax=min(marginrankmax,marginrank)
                  write(*,'("--------|",a)')repeat('-',132-9)
                  write(*,"('old     |',a)")trim(lline(:ilen))
                  write(*,"(i8,     '|',a)")lcount,trim(dline(:max(ilen,ilen2)))
                  write(*,"('new     |',a)")trim(rline(:ilen2))
                  write(*,"('>>>>>>>>|minimum match =',g0,'(digits)')")marginrank
                  write(*,'("--------|",a)')repeat('-',132-9)
               elseif(verbose)then
                  write(*,"(i8,     '|',a)")lcount,lline(:ilen)
               endif
            else
               ! note that PERCENT_DIFF(3f) errors come out before printout of line
               call percent_diff(lline,rline,dline,per,percent_tolerance,ier,ilen,ilen2)
               permax=max(permax,per)                                     ! if a new maximum percent difference was found record it
               if(per.ge.percent_tolerance)then                           ! print lines over percentage tolerance
                  write(*,'("--------|",a)')repeat('-',132-9)
                  write(*,"('old     |',a)")trim(lline(:ilen))
                  write(*,"(i8,     '|',a)")lcount,dline(:max(ilen,ilen2))
                  write(*,"('new     |',a)")trim(rline(:ilen2))
                  write(*,"('>>>>>>>>|maximum difference =',g0,'(%)')")per
                  write(*,'("--------|",a)')repeat('-',132-9)
               elseif(verbose)then
                  write(*,"(i8,     '|',a)")lcount,lline(:ilen)
               endif
            endif
         elseif(verbose)then
            write(*,"(i8,     '=',a)")lcount,trim(lline)
         endif
      enddo INFINITE
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine num_diff
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
subroutine margin_diff(line,line2,dline,per,margin,ier,ilen,ilen2,ind_line)
!     Author: John S. Urban
!     Date:   1989,1990, 2013
!  assuming same number of strings on two lines to be compared, find strings that do not match, then if both strings are numbers
!  find if they are equal within a specified relative margin.  makes assumption that only difference in lines is values of numbers
!  that string word differences can be ignored, and same number of words on each line
!-----------------------------------------------------------------------------------------------------------------------------------
use :: M_verify, only : in_margin
implicit none
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in)    :: line                 ! line from file 1
   character(len=*),intent(in)    :: line2                ! line from file 2
   character(len=*),intent(out)   :: dline                ! line showing difference
   doubleprecision, intent(out)   :: per                  ! maximum difference
   real, intent(in)               :: margin               ! relative margin
   integer,intent(out)            :: ier                  ! returned error code; 0 is no error
   integer,intent(out)            :: ilen                 ! length of line trimmed
   integer,intent(out)            :: ilen2                ! length of line2 trimmed
   integer,intent(out)            :: ind_line
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                        :: ind
   integer                        :: i
   integer                        :: ic
   integer                        :: ic2
   doubleprecision                :: acurcy
   doubleprecision                :: value
   doubleprecision                :: value2
   doubleprecision                :: diff
   integer,parameter              :: ip=264/2
   integer                        :: iflag
   integer                        :: iflag2
   integer                        :: ii
   integer is(ip),ie(ip),is2(ip),ie2(ip)
!-----------------------------------------------------------------------------------------------------------------------------------
   dline=' '                                               ! line filled with # where differences occur
   per=0.0d0                                               !
   ier=0                                                   ! error flag: 0 ok, -1 error
   acurcy=0.0d0
   call parse(line ,ic ,is ,ie ,ilen )                     ! fill array with tokens from line 1
   call parse(line2,ic2,is2,ie2,ilen2)                     ! fill array with tokens from line 2
   ind_line=0
   if(ic.ne.0)then                                         ! ic=number of strings found
     do i=1,ic
        ind=0
        call getnum(line(is(i):ie(i)),value,iflag)         ! try to convert substring line(is(i):ie(i) to number
        if(iflag.eq.0)then                                 ! value = real value of string
           if(i.gt.ic2)then
              ier=-1
              write(*,*)"[error]"," no words left to compare"
              exit
           else
              call getnum(line2(is2(i):ie2(i)),value2,iflag2) ! try to convert substing from line2 to number
           endif
           if(iflag2.ne.0)then                             ! line1 was number but line2 was not
              ier=-1

              write(*,*)"[error]",line2(is2(i):ie2(i))," non-numeric"
           else
              if(value.eq.value2)then
                 diff=0.0d0
              else
                 if(in_margin(value,value2,margin))then ! compare two double numbers
                 else
                    ind_line=max(ind,ind_line)
                    ! if found a difference over limit, fill dline with # characters
                    GLOBAL_COUNT=GLOBAL_COUNT+1
                    do ii=is(i),ie(i)
                       dline(ii:ii)='#'
                    enddo
                 endif
              endif
           endif
        elseif(line(is(i):ie(i)).ne.line2(is2(i):ie2(i)))then  ! non-numeric stings
           ! first string was not numeric, but strings are not supposed to differ
           write(*,*)"[error]",line(is(i):ie(i)),"><",line2(is2(i):ie2(i))
        endif
     enddo
   endif
end subroutine margin_diff
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
subroutine digits_diff(line,line2,dline,per,idigits,ier,ilen,ilen2,ind_line)
!     Author: John S. Urban
!     Date:   1989,1990, 2013
!  assuming same number of strings on two lines to be compared, find strings that do not match, then if both strings are numbers
!  find if they have IDIGITS matching significant digits.  makes assumption that only difference in lines is values of numbers
!  that string word differences can be ignored, and same number of words on each line
!-----------------------------------------------------------------------------------------------------------------------------------
   implicit none
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in)    :: line                 ! line from file 1
   character(len=*),intent(in)    :: line2                ! line from file 2
   character(len=*),intent(out)   :: dline                ! line showing difference
   doubleprecision, intent(out)   :: per                  ! maximum difference
   integer, intent(in)            :: idigits              ! number of digits to match
   integer,intent(out)            :: ier                  ! returned error code; 0 is no error
   integer,intent(out)            :: ilen                 ! length of line trimmed
   integer,intent(out)            :: ilen2                ! length of line2 trimmed
   integer,intent(out)            :: ind_line
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                        :: ind
   integer                        :: i
   integer                        :: ic
   integer                        :: ic2
   doubleprecision                :: value
   doubleprecision                :: value2
   doubleprecision                :: diff
   doubleprecision                :: acurcy
   integer,parameter              :: ip=264/2
   integer                        :: iflag
   integer                        :: iflag2
   integer                        :: ii
   integer is(ip),ie(ip),is2(ip),ie2(ip)
!-----------------------------------------------------------------------------------------------------------------------------------
   dline=' '                                               ! line filled with # where differences occur
   per=0.0d0                                               !
   ier=0                                                   ! error flag: 0 ok, -1 error
   acurcy=0.0d0
   call parse(line ,ic ,is ,ie ,ilen )                     ! fill array with tokens from line 1
   call parse(line2,ic2,is2,ie2,ilen2)                     ! fill array with tokens from line 2
   ind_line=0
   if(ic.ne.0)then                                         ! ic=number of strings found
     do i=1,ic
        ind=0
        call getnum(line(is(i):ie(i)),value,iflag)         ! try to convert substring line(is(i):ie(i) to number
        if(iflag.eq.0)then                                 ! value = real value of string
           if(i.gt.ic2)then
              ier=-1
              write(*,*)"[error]"," no words left to compare"
              exit
           else
              call getnum(line2(is2(i):ie2(i)),value2,iflag2) ! try to convert substing from line2 to number
           endif
           if(iflag2.ne.0)then                             ! line1 was number but line2 was not
              ier=-1

              write(*,*)"[error]",line2(is2(i):ie2(i))," non-numeric"
           else
              if(value.eq.value2)then
                 diff=0.0d0
              else
                 call same8(value,value2,idigits,ACURCY,IND) ! compare two double numbers up to a specified number of digits
                 ind_line=max(ind,ind_line)
              endif
              per=max(per,dble(acurcy))
              if(ind.ne.0)then  ! if found a difference over limit, fill dline with # characters
                 GLOBAL_COUNT=GLOBAL_COUNT+1
                 do ii=is(i),ie(i)
                    dline(ii:ii)='#'
                 enddo
              endif
           endif
        elseif(line(is(i):ie(i)).ne.line2(is2(i):ie2(i)))then  ! non-numeric stings
           ! first string was not numeric, but strings are not supposed to differ
           write(*,*)"[error]",line(is(i):ie(i)),"><",line2(is2(i):ie2(i))
        endif
     enddo
   endif
end subroutine digits_diff
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
subroutine percent_diff(line,line2,dline,per,percent_tolerance,ier,ilen,ilen2)
!     Author: John S. Urban
!     Date:   1989,1990
!  assuming same number of strings on two lines to be compared, find
!  strings that do not match, then if both strings are numbers find
!  the maximum percentage difference.
!  makes assumption that only difference in lines is values of numbers
!  that string word differences can be ignored,
!  and same number of words on each line
!-----------------------------------------------------------------------------------------------------------------------------------
   implicit none
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in)    :: line                 ! line from file 1
   character(len=*),intent(in)    :: line2                ! line from file 2
   character(len=*),intent(out)   :: dline                ! line showing difference
   doubleprecision, intent(out)   :: per                  ! maximum percentage difference
   doubleprecision, intent(in)    :: percent_tolerance    ! percentage limit
   integer,intent(out)            :: ier                  ! returned error code; 0 is no error
   integer,intent(out)            :: ilen                 ! length of line trimmed
   integer,intent(out)            :: ilen2                ! length of line2 trimmed
!-----------------------------------------------------------------------------------------------------------------------------------
   integer                        :: i
   integer                        :: ic
   integer                        :: ic2
   doubleprecision                :: value
   doubleprecision                :: value2
   doubleprecision                :: x1
   doubleprecision                :: x2
   doubleprecision                :: bot
   doubleprecision                :: diff
   integer,parameter              :: ip=264/2
   integer                        :: iflag
   integer                        :: iflag2
   integer                        :: ii
   integer is(ip),ie(ip),is2(ip),ie2(ip)
!-----------------------------------------------------------------------------------------------------------------------------------
   dline=' '                                               ! line filled with # where differences occur
   per=0.0d0                                               !
   ier=0                                                   ! error flag: 0 ok, -1 error
   call parse(line ,ic ,is ,ie ,ilen )                     ! fill array with tokens from line 1
   call parse(line2,ic2,is2,ie2,ilen2)                     ! fill array with tokens from line 2
   if(ic.ne.0)then                                         ! ic=number of strings found
     do i=1,ic
        call getnum(line(is(i):ie(i)),value,iflag)         ! try to convert substring line(is(i):ie(i) to number
        if(iflag.eq.0)then                                 ! value = real value of string
           call getnum(line2(is2(i):ie2(i)),value2,iflag2) ! try to convert substing from line2 to number
           if(iflag2.ne.0)then                             ! line1 was number but line2 was not
              ier=-1
              write(*,*)"[error]",line2(is2(i):ie2(i))," non-numeric"
           else
              x1=abs(value)
              x2=abs(value2)
              if(x1.eq.x2)then
                 diff=0.0d0
              else
                 bot=min(x1,x2)
                 if(bot.eq.0.0)bot=max(x1,x2)
                 ! if smaller value is zero, per is always 100,
                 ! could possibly get an overflow/underflow condition here
                 diff=abs(value-value2)/bot ! maximized percentage difference
              endif
              per=max(per,diff*100.0)
              if(diff*100.0.ge.percent_tolerance)then  ! if found a difference over limit, fill dline with # characters
                 GLOBAL_COUNT=GLOBAL_COUNT+1
                 do ii=is(i),ie(i)
                    dline(ii:ii)='#'
                 enddo
              endif
           endif
        elseif(line(is(i):ie(i)).ne.line2(is2(i):ie2(i)))then  ! non-numeric stings
           ! first string was not numeric, but strings are not supposed to differ
           write(*,*)"[error]",line(is(i):ie(i)),"><",line2(is2(i):ie2(i))
        endif
     enddo
   endif
end subroutine percent_diff
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
   subroutine parse(input_line,inotnull,ibegin,iterm,ilen)
!-----------------------------------------------------------------------------------------------------------------------------------
!$@(#) M_strings::parse(3f): parse string on delimiter characters and store token extents into arrays
!  Author: John S. Urban
!  Date:   1986,1990,2013
!
!  given a line of up to 264 characters of structure " par1 par2 par3 ... par(n) " where par(i) are separated by a delimiter
!  find how many pars and beginning and ending column of each, ignoring leading and trailing blanks
!  output array sizes should be istart(132), iterm(132)
!
!  adjacent delimiters in the input string do not create an empty string in the output array
!  no quoting of delimiters is supported
!
!  also return position of last non-blank character (even if more than 132 elements were found).
!
!  delimiters are from set ",; |".
!
!  no checking for more than ipars parameters, if any more they are ignored
!-----------------------------------------------------------------------------------------------------------------------------------
   integer,parameter             :: ipars=264              ! max number of strings INPUT_LINE could split into if all delimiter
   character(len=*),intent(in)   :: input_line             ! input string to tokenize
   integer,intent(out)           :: ibegin((ipars+1)/2)    ! positions in input string where tokens start
   integer,intent(out)           :: iterm((ipars+1)/2)     ! positions in input string where tokens end
   integer,intent(out)           :: inotnull               ! count strings not composed of delimiters
   integer,intent(out)           :: ilen                   ! length of input string with trailing spaces trimmed
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=:),allocatable  :: dlim                   ! string containing delimiter characters
   integer                       :: icount                 ! number of tokens found
   integer                       :: i10,i30                ! loop counters
   integer                       :: icol                   ! pointer into input string as it is being parsed
   integer                       :: idlim                  ! number of delimiter characters
   integer                       :: ifound                 ! where next delimiter character is found in remaining input string data
   integer                       :: imax                   ! length of longest token
!-----------------------------------------------------------------------------------------------------------------------------------
   dlim=' ;,|'                                             ! decide on value for optional DELIMITERS parameter
   idlim=len(dlim)                                         ! dlim a lot of blanks on some machines if dlim is a big string
!-----------------------------------------------------------------------------------------------------------------------------------
   ibegin(:)=1
   iterm(:)=1
!-----------------------------------------------------------------------------------------------------------------------------------
   ilen=len_trim(input_line)                                      ! ILEN is the column position of the last non-blank character
   icount=0                                                       ! how many tokens found
   inotnull=0                                                     ! how many tokens found not composed of delimiters
   imax=0                                                         ! length of longest token found
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ilen.eq.0)return                                            ! command was totally blank
!-----------------------------------------------------------------------------------------------------------------------------------
                                                                  ! there is at least one non-delimiter in INPUT_LINE if get here
      icol=1                                                      ! initialize pointer into input line
      INFINITE: do i30=1,ilen,1                                   ! store into each array element
         ibegin(inotnull+1)=icol                                  ! assume start new token on the character
         if(index(dlim(1:idlim),input_line(icol:icol)).eq.0)then  ! if current character is not a delimiter
            iterm(inotnull+1)=ilen                                ! initially assume no more tokens
            do i10=1,idlim                                        ! search for next delimiter
               ifound=index(input_line(ibegin(inotnull+1):ilen),dlim(i10:i10))
               IF(ifound.gt.0)then
                  iterm(inotnull+1)=min(iterm(inotnull+1),ifound+ibegin(inotnull+1)-2)
               endif
            enddo
            icol=iterm(inotnull+1)+2                              ! next place to look as found end of this token
            inotnull=inotnull+1                                   ! increment count of number of tokens not composed of delimiters
         else                                                     ! character is a delimiter for a null string
            iterm(inotnull+1)=icol-1                              ! record assumed end of string. Will be less than beginning
            icol=icol+1                                           ! advance pointer into input string
         endif
         imax=max(imax,iterm(inotnull+1)-ibegin(inotnull+1)+1)
         icount=inotnull                                          ! increment count of number of tokens found
         if(icol.gt.ilen)then                                     ! text left
            exit INFINITE
         endif
      enddo INFINITE
      ilen=iterm(inotnull)
   end subroutine parse
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
subroutine parse1(line,ic,is,ie,ilen)
!  Author: John S. Urban
!  Date:   1986,1990,2013
!
!  given a line of structure " par1 par2 par3 ... par(n) "
!  find how many pars and beginning and ending column of each ignoring leading and trailing blanks
!
!  also return position of last non-blank character (even if more than 132 elements were found).
!
!  spaces are only legal delimiters.
!  no checking for more than ipars parameters, if any more they are ignored
!-----------------------------------------------------------------------------------------------------------------------------------
   implicit none
   integer,parameter              :: ipars=264/2   ! maximum number of values expected in a 264-character line
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=*),intent(in)    :: line          ! input line to parse
   integer,intent(out)            :: ic            ! how many tokens were found
   integer,intent(out)            :: is(ipars)     ! start position of tokens
   integer,intent(out)            :: ie(ipars)     ! end position of tokens
   integer,intent(out)            :: ilen          ! length of input line; or last position parsed
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=1),parameter     :: delim=" "
   integer                        :: icol
   integer                        :: iarray
   integer                        :: istart
   integer                        :: iend
!-----------------------------------------------------------------------------------------------------------------------------------
   ic=0                                                               ! initialize count of number of tokens found
   ilen=len_trim(line)                                                ! position of last non-blank character in input line
!-----------------------------------------------------------------------------------------------------------------------------------
!  command was totally blank
   if(ilen.eq.0)return
!-----------------------------------------------------------------------------------------------------------------------------------
!  there is at least one non-blank character in the command
!  find next non-delimiter
   icol=1
   do iarray=1,ipars,1
      !-----------------------------------------------------------------------------------------------------------------------------
      INFINITE: do
         if(line(icol:icol).ne.delim)then
            istart=icol
            is(iarray)=icol
            iend=index(line(istart:ilen),delim)
            if(iend.le.0)then
               ie(iarray)=ilen
               ic=iarray
               return
            else
               iend=iend+istart-2
               ie(iarray)=iend
            endif
            icol=iend+2
            exit INFINITE
         else
            icol=icol+1
         endif
      enddo INFINITE
      !-----------------------------------------------------------------------------------------------------------------------------
      !  last character in line was a delimiter, so no text left
      !  (should not happen where blank=delimiter)
      if(icol.gt.ilen)then
         ic=iarray
         return
      endif
   enddo
!  more than ipars elements
   ic=ipars
end subroutine parse1
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
subroutine getnum(chars,value,iflag)
!  Author: John S. Urban
!  Date:   07/15/1986
!  o  works with any g-format input, including integer, real, and exponential.
!  o  prints an error message to output and returns a value of zero if an error occurs.
!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
! ident_2="@(#)getnum(3f): returns a real value from a numeric character string"
      character(len=*),intent(in)    ::  chars
      doubleprecision,intent(out)    ::  value
      integer,intent(out)            ::  iflag
!-----------------------------------------------------------------------------------------------------------------------------------
      character(len=13)              ::  frmt
      integer                        :: ios
!-----------------------------------------------------------------------------------------------------------------------------------
      write(frmt,'("(bn,g",i5,".0)")')len(chars)
      read(chars,fmt=frmt,iostat=ios)value
      if(ios.ne.0)then
         value=0.0d0
         iflag=1
         !write(*,*)"error occurred in read in function getnum, iostat=",ierr
         !write(*,*)"input string=",chars
      else
         iflag=0
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine getnum
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
subroutine remark(string)
use iso_fortran_env, only : error_unit ! access computing environment
implicit none
! ident_3="@(#)remark(3f): writes a message to standard error using a standard f2003 method"
character(len=*),intent(in) :: string
      write(error_unit,*)trim(string)
end subroutine remark
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
SUBROUTINE same8(X,Y,IDIGIT,ACURCY,IND)
!-----------------------------------------------------------------------------------------------------------------------------------
!     Compare two double numbers only up to a specified number of digits
!
!     If two numbers agree to IDIGIT digits of accuracy return IND=0 else return IND=1.
!     Also return a number ACURCY that indicates how many digits do agree.
!
!     Tolerance ...
!        X and Y are considered equal within IDIGIT relative tolerance,
!        if ACURCY is greater than IDIGIT.
!
!     Based on ...
!     ** NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. ACCDIG V 7.00  2/14/90. **
!        DAVID HOGBEN,
!        STATISTICAL ENGINEERING DIVISION,
!        CENTER FOR COMPUTING AND APPLIED MATHEMATICS,
!        A337 ADMINISTRATION BUILDING,
!        NATIONAL INSTITUTE OF STANDARDS AND TECHNOLOGY,
!        GAITHERSBURG, MD 20899
!           TELEPHONE 301-975-2845
!           ORIGINAL VERSION - OCTOBER,  1969.
!           CURRENT VERSION  - FEBRUARY, 1990.
!           CURRENT VERSION  - FEBRUARY, 1991. JSU
!           CURRENT VERSION  - FEBRUARY, 2013. JSU
!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
! ident_4="@(#)same8(3f): compare two double numbers only up to a specified number of digits"
!-----------------------------------------------------------------------------------------------------------------------------------
!     INPUT ...
      doubleprecision,intent(in)  :: x                         ! FIRST  OF TWO DOUBLE NUMBERS TO BE COMPARED.
      doubleprecision,intent(in)  :: y                         ! SECOND OF TWO DOUBLE NUMBERS TO BE COMPARED.
      integer,intent(in)          :: idigit                    ! NUMBER OF DIGITS TO BE SATISFIED IN RELATIVE TOLERANCE.
!-----------------------------------------------------------------------------------------------------------------------------------
!     OUTPUT ...
      integer,intent(out)         :: ind                       ! = 0, IF TOLERANCE IS     SATISFIED.
                                                               ! = 1, IF TOLERANCE IS NOT SATISFIED.
      doubleprecision,intent(out) :: acurcy                    ! = - LOG10 (ABS((X-Y)/Y)))
!-----------------------------------------------------------------------------------------------------------------------------------
      doubleprecision             :: diff                      ! delta of X and Y values
      doubleprecision             :: digi
      integer                     :: ireal_significant_digits  ! maximum number of significant digits for double precision values
      character(len=132)          :: warning                   ! string user for writing error messages
!-----------------------------------------------------------------------------------------------------------------------------------
      ireal_significant_digits=int(log10(2.0**digits(0.0d0)))  ! MAXIMUM NUMBER OF SIGNIFICANT DIGITS IN A DOUBLE NUMBER.
      digi=idigit                                              ! floating value copy of IDIGIT
!-----------------------------------------------------------------------------------------------------------------------------------
      if(digi.le.0)then                                        ! check for inappropriate number of digits to test that is too small
         write(warning,'(a,i0)')'*same8* bad number of significant digits=',idigit; call remark(warning)
         call remark(warning)                                  ! write warning to stderr
         digi=ireal_significant_digits                         ! set bad value to maximum allowed for double precision number
      else if(digi .gt. ireal_significant_digits)then          ! check for inappropriate number of digits to test  that is too big
         write(warning,'(a,i0)')'*same8* significant digit requested too high=',idigit; call remark(warning)
         call remark(warning)                                  ! write warning to stderr
         digi=real(ireal_significant_digits)                   ! set bad value to maximum allowed for double precision number
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      diff = x - y                                             ! find difference between to values to be compared
!-----------------------------------------------------------------------------------------------------------------------------------
      if (diff .eq. 0.0) then                                  ! no difference
         acurcy = digi
      else if (y .eq. 0.0) then                                ! special case where Y is zero
         acurcy = - log10 (abs (x))                            ! X cannot be zero so get measure of X
      else
         acurcy = - log10 ( abs(diff) ) + log10 ( abs(y) )     ! get measure
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      if (acurcy .lt. digi ) then
         ind = 1                                               ! not equal to desired number of digits
      else
         ind = 0                                               ! equal to desired number of digits
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine same8
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
' numdiff(1f) - [DEVELOPER] Compare numeric differences between two files',&
' (LICENSE:PD)',&
'',&
'SYNOPSIS',&
' numdiff',&
'    -old FILENAME -new FILENAME',&
'    [ -percent REAL_VALUE|-digits N|-margin XXX.XX]',&
'    [ -verbose]|',&
'    [ --help|--version]',&
'',&
'DESCRIPTION',&
' NUMDIFF assumes two files are basically identical except for numeric',&
' differences, and finds values whose differences exceed a specified',&
' tolerance.',&
' o file widths are required to be less than 264 characters.',&
' o numbers are assumed to be delimited by spaces, commas, semi-colons,',&
'   or vertical line characters (" ,;|"). Adjacent delimiters are ignored.',&
'',&
' This program was originally written to simplify the comparison of values',&
' generated by new versions of numeric libraries to previous versions',&
' of the libraries.',&
'',&
'OPTIONS',&
'    The options are',&
'',&
'    -old FILENAME',&
'         name of file containing template values',&
'',&
'    -new FILENAME',&
'         name of file containing new values',&
'',&
'    -percent REAL_VALUE',&
'             set threshold at which to report values as a percentage of the',&
'             template value',&
'',&
'    -digits N',&
'            set threshold at which to report values as a number of digits',&
'            if -digits is specified -percent is ignored.',&
'',&
'    -margin XXX.XX',&
'            set threshold to a relative margin of the magnitude of the values',&
'',&
'    -verbose',&
'       shows the lines that pass the criteria from OLDFILE as well.',&
'',&
'    --help',&
'       display this help text and exit',&
'',&
'    --version',&
'       display information on the code version and exit',&
'',&
'USAGE',&
' 1. GENERATE TEMPLATE:  call all your numeric procedures over their',&
'    allowed ranges and print the values to a file on your original system.',&
'    Save this file as your master QA template.',&
' 2. GENERATE TRIAL DATA:  When you port the procedures to another system or',&
'    recompile using a new compiler run your QA program again and save',&
'    the second file.',&
' 3. TEST/COMPARE:  run the numdiff(1) program:',&
'',&
'      numdiff -old MASTER_TEMPLATE_FILE -new NEW_OUTPUT_FILE -percent 0.0001',&
'',&
'EXAMPLE',&
'',&
'   We will assume we have two files meeting the above criteria called',&
'   "cray_results.txt" and "cygwin_results.txt". To compare the values',&
'   we enter',&
'',&
'     numdiff -old cray_results.txt -new cygwin_results.txt -percent 0.00001',&
'',&
'   A diff(1) of the following input files would show every line, as one',&
'   uses the "E" prefix for exponents, while the other uses "D". Even when a',&
'   diff(1) would show few lines, you have to inspect each difference to see',&
'   how large a difference in value was found. Using numdiff(1) you can',&
'   ignore most insignificant differences.',&
'',&
'   OUTPUT',&
'',&
'   Results from the run are',&
'',&
'     >        | *numdiff* numerical differences',&
'     >        | old file=in1',&
'     >        | new file=in2',&
'     >        | percent threshold =  0.1E-006 (%)',&
'     >--------|------------------------------------------------------------------------------------------------------------',&
'     >old     |    0.32000000E+02    0.88589141E-01   -0.17863993E-01   -0.36879559E-04    0.16022057E-01    0.31990327E+02',&
'     >       8|                                       ###############   ###############',&
'     >new     |    0.32000000D+02    0.88589141D-01   -0.17863994D-01   -0.36879562D-04    0.16022057D-01    0.31990327D+02',&
'     >>>>>>>>>|maximum difference =.81345875138179356E-005(%)',&
'     >--------|------------------------------------------------------------------------------------------------------------',&
'',&
'   This indicates values did not pass on line 8. "#" characters underline the values.',&
'',&
'',&
'Input file "cray_results.txt"',&
'',&
'   1TESTS STARTED.',&
'',&
'',&
'   1          T,PSL(T),HSL(T),SSL(T),VSL(T),TSLH(H)',&
'   0         PMIN=    0.885891E-01    PMAX=    0.320823E+04    DELP=    0.100000E+03',&
'             TMIN=    0.320000E+02    TMAX=    0.705470E+03    DELT=    0.200000E+02',&
'   0',&
'       0.32000000E+02    0.88589141E-01   -0.17863993E-01   -0.36879559E-04    0.16022057E-01    0.31990327E+02',&
'       0.40000000E+02    0.12163360E+00    0.80272719E+01    0.16194175E-01    0.16018940E-01    0.40003431E+02',&
'       0.60000000E+02    0.25611401E+00    0.28059533E+02    0.55503370E-01    0.16033165E-01    0.60005984E+02',&
'       0.80000000E+02    0.50682853E+00    0.48036541E+02    0.93222577E-01    0.16071928E-01    0.79998151E+02',&
'       0.10000000E+03    0.94923553E+00    0.67998872E+02    0.12954114E+00    0.16129956E-01    0.99994830E+02',&
'       0.12000000E+03    0.16927366E+01    0.87966229E+02    0.16459153E+00    0.16204270E-01    0.11999684E+03',&
'       0.14000000E+03    0.28891787E+01    0.10794963E+03    0.19847772E+00    0.16293096E-01    0.14000080E+03',&
'       0.16000000E+03    0.47413557E+01    0.12795766E+03    0.23128914E+00    0.16395272E-01    0.16000351E+03',&
'       0.18000000E+03    0.75110274E+01    0.14799946E+03    0.26310734E+00    0.16510005E-01    0.18000351E+03',&
'       0.20000000E+03    0.11526035E+02    0.16808606E+03    0.29400880E+00    0.16636808E-01    0.20000116E+03',&
'       0.22000000E+03    0.17186197E+02    0.18823058E+03    0.32406594E+00    0.16775474E-01    0.21999785E+03',&
'       0.24000000E+03    0.24967794E+02    0.20844802E+03    0.35334719E+00    0.16926045E-01    0.23999513E+03',&
'       0.26000000E+03    0.35426601E+02    0.22875486E+03    0.38191689E+00    0.17088786E-01    0.25999384E+03',&
'       0.28000000E+03    0.49199533E+02    0.24916873E+03    0.40983516E+00    0.17264170E-01    0.27999361E+03',&
'       0.30000000E+03    0.67005075E+02    0.26970823E+03    0.43715817E+00    0.17452875E-01    0.29999280E+03',&
'       0.32000000E+03    0.89642735E+02    0.29039297E+03    0.46393855E+00    0.17655789E-01    0.31998899E+03',&
'       0.34000000E+03    0.11799178E+03    0.31124389E+03    0.49022610E+00    0.17874025E-01    0.33998018E+03',&
'       0.36000000E+03    0.15300954E+03    0.33228370E+03    0.51606862E+00    0.18108958E-01    0.35996652E+03',&
'       0.38000000E+03    0.19572950E+03    0.35353750E+03    0.54151277E+00    0.18362259E-01    0.38000205E+03',&
'       0.40000000E+03    0.24725940E+03    0.37509279E+03    0.56667770E+00    0.18637646E-01    0.40003002E+03',&
'       0.42000000E+03    0.30877960E+03    0.39689616E+03    0.59150311E+00    0.18935141E-01    0.41998032E+03',&
'       0.44000000E+03    0.38154169E+03    0.41902722E+03    0.61609004E+00    0.19258996E-01    0.43998928E+03',&
'       0.46000000E+03    0.46686778E+03    0.44153583E+03    0.64049662E+00    0.19613160E-01    0.46000991E+03',&
'       0.48000000E+03    0.56615075E+03    0.46448172E+03    0.66478705E+00    0.20002609E-01    0.48002037E+03',&
'       0.50000000E+03    0.68085599E+03    0.48793750E+03    0.68903400E+00    0.20433708E-01    0.50001633E+03',&
'       0.52000000E+03    0.81252603E+03    0.51199311E+03    0.71332237E+00    0.20914751E-01    0.52000390E+03',&
'       0.54000000E+03    0.96279001E+03    0.53676266E+03    0.73775486E+00    0.21456790E-01    0.53999326E+03',&
'       0.56000000E+03    0.11333816E+04    0.56239502E+03    0.76246065E+00    0.22074947E-01    0.55999280E+03',&
'       0.58000000E+03    0.13261708E+04    0.58909059E+03    0.78760893E+00    0.22790623E-01    0.58000418E+03',&
'       0.60000000E+03    0.15432192E+04    0.61712901E+03    0.81343122E+00    0.23635444E-01    0.60001858E+03',&
'       0.62000000E+03    0.17868695E+04    0.64691984E+03    0.84026231E+00    0.24659110E-01    0.62001710E+03',&
'       0.64000000E+03    0.20598878E+04    0.67911317E+03    0.86863104E+00    0.25947611E-01    0.63998803E+03',&
'       0.66000000E+03    0.23656783E+04    0.71493433E+03    0.89954128E+00    0.27678770E-01    0.66001396E+03',&
'       0.68000000E+03    0.27085898E+04    0.75845725E+03    0.93651903E+00    0.30369395E-01    0.68000016E+03',&
'       0.70000000E+03    0.30943291E+04    0.82243999E+03    0.99006883E+00    0.36618048E-01    0.70000112E+03',&
'       0.70547000E+03    0.32082348E+04    0.90600741E+03    0.10611600E+01    0.50778529E-01    0.00000000E+00',&
'   0         SUM=  0.6353636479732371E+05',&
'',&
'',&
'Input file "cygwin_results.txt"',&
'',&
'   1TESTS STARTED.',&
'',&
'',&
'   1          T,PSL(T),HSL(T),SSL(T),VSL(T),TSLH(H)',&
'   0         PMIN=    0.885891E-01    PMAX=    0.320823E+04    DELP=    0.100000E+03',&
'             TMIN=    0.320000E+02    TMAX=    0.705470E+03    DELT=    0.200000E+02',&
'   0',&
'       0.32000000D+02    0.88589141D-01   -0.17863994D-01   -0.36879562D-04    0.16022057D-01    0.31990327D+02',&
'       0.40000000D+02    0.12163360D+00    0.80272719D+01    0.16194175D-01    0.16018940D-01    0.40003431D+02',&
'       0.60000000D+02    0.25611401D+00    0.28059533D+02    0.55503370D-01    0.16033165D-01    0.60005984D+02',&
'       0.80000000D+02    0.50682853D+00    0.48036541D+02    0.93222577D-01    0.16071928D-01    0.79998151D+02',&
'       0.10000000D+03    0.94923553D+00    0.67998872D+02    0.12954114D+00    0.16129956D-01    0.99994830D+02',&
'       0.12000000D+03    0.16927366D+01    0.87966229D+02    0.16459153D+00    0.16204270D-01    0.11999684D+03',&
'       0.14000000D+03    0.28891787D+01    0.10794963D+03    0.19847772D+00    0.16293096D-01    0.14000080D+03',&
'       0.16000000D+03    0.47413557D+01    0.12795766D+03    0.23128914D+00    0.16395272D-01    0.16000351D+03',&
'       0.18000000D+03    0.75110274D+01    0.14799946D+03    0.26310734D+00    0.16510005D-01    0.18000351D+03',&
'       0.20000000D+03    0.11526035D+02    0.16808606D+03    0.29400880D+00    0.16636808D-01    0.20000116D+03',&
'       0.22000000D+03    0.17186197D+02    0.18823058D+03    0.32406594D+00    0.16775474D-01    0.21999785D+03',&
'       0.24000000D+03    0.24967794D+02    0.20844802D+03    0.35334719D+00    0.16926045D-01    0.23999513D+03',&
'       0.26000000D+03    0.35426601D+02    0.22875486D+03    0.38191689D+00    0.17088786D-01    0.25999384D+03',&
'       0.28000000D+03    0.49199533D+02    0.24916873D+03    0.40983516D+00    0.17264170D-01    0.27999361D+03',&
'       0.30000000D+03    0.67005075D+02    0.26970823D+03    0.43715817D+00    0.17452875D-01    0.29999280D+03',&
'       0.32000000D+03    0.89642735D+02    0.29039297D+03    0.46393855D+00    0.17655789D-01    0.31998899D+03',&
'       0.34000000D+03    0.11799178D+03    0.31124389D+03    0.49022610D+00    0.17874025D-01    0.33998018D+03',&
'       0.36000000D+03    0.15300954D+03    0.33228370D+03    0.51606862D+00    0.18108958D-01    0.35996652D+03',&
'       0.38000000D+03    0.19572950D+03    0.35353750D+03    0.54151277D+00    0.18362259D-01    0.38000205D+03',&
'       0.40000000D+03    0.24725940D+03    0.37509279D+03    0.56667770D+00    0.18637646D-01    0.40003002D+03',&
'       0.42000000D+03    0.30877960D+03    0.39689616D+03    0.59150311D+00    0.18935141D-01    0.41998032D+03',&
'       0.44000000D+03    0.38154169D+03    0.41902722D+03    0.61609004D+00    0.19258996D-01    0.43998928D+03',&
'       0.46000000D+03    0.46686778D+03    0.44153583D+03    0.64049662D+00    0.19613160D-01    0.46000991D+03',&
'       0.48000000D+03    0.56615075D+03    0.46448172D+03    0.66478705D+00    0.20002609D-01    0.48002037D+03',&
'       0.50000000D+03    0.68085599D+03    0.48793750D+03    0.68903400D+00    0.20433708D-01    0.50001633D+03',&
'       0.52000000D+03    0.81252603D+03    0.51199311D+03    0.71332237D+00    0.20914751D-01    0.52000390D+03',&
'       0.54000000D+03    0.96279001D+03    0.53676266D+03    0.73775486D+00    0.21456790D-01    0.53999326D+03',&
'       0.56000000D+03    0.11333816D+04    0.56239502D+03    0.76246065D+00    0.22074947D-01    0.55999280D+03',&
'       0.58000000D+03    0.13261708D+04    0.58909059D+03    0.78760893D+00    0.22790623D-01    0.58000418D+03',&
'       0.60000000D+03    0.15432192D+04    0.61712901D+03    0.81343122D+00    0.23635444D-01    0.60001858D+03',&
'       0.62000000D+03    0.17868695D+04    0.64691984D+03    0.84026231D+00    0.24659110D-01    0.62001710D+03',&
'       0.64000000D+03    0.20598878D+04    0.67911317D+03    0.86863104D+00    0.25947611D-01    0.63998803D+03',&
'       0.66000000D+03    0.23656783D+04    0.71493433D+03    0.89954128D+00    0.27678770D-01    0.66001396D+03',&
'       0.68000000D+03    0.27085898D+04    0.75845725D+03    0.93651903D+00    0.30369395D-01    0.68000016D+03',&
'       0.70000000D+03    0.30943291D+04    0.82243999D+03    0.99006883D+00    0.36618048D-01    0.70000112D+03',&
'       0.70547000D+03    0.32082348D+04    0.90600741D+03    0.10611600D+01    0.50778529D-01    0.00000000D+00',&
'   0         SUM=  0.6353636479675987E+05',&
'',&
'AUTHOR',&
'   John S. Urban',&
'',&
'LICENSE',&
'   Public Domain',&
'']
!>
!!##NAME
!!  numdiff(1f) - [DEVELOPER] Compare numeric differences between two files
!!  (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!  numdiff
!!     -old FILENAME -new FILENAME
!!     [ -percent REAL_VALUE|-digits N|-margin XXX.XX]
!!     [ -verbose]|
!!     [ --help|--version]
!!
!!##DESCRIPTION
!!  NUMDIFF assumes two files are basically identical except for numeric
!!  differences, and finds values whose differences exceed a specified
!!  tolerance.
!!  o file widths are required to be less than 264 characters.
!!  o numbers are assumed to be delimited by spaces, commas, semi-colons,
!!    or vertical line characters (" ,;|"). Adjacent delimiters are ignored.
!!
!!  This program was originally written to simplify the comparison of values
!!  generated by new versions of numeric libraries to previous versions
!!  of the libraries.
!!
!!##OPTIONS
!!     The options are
!!
!!     -old FILENAME
!!          name of file containing template values
!!
!!     -new FILENAME
!!          name of file containing new values
!!
!!     -percent REAL_VALUE
!!              set threshold at which to report values as a percentage of the
!!              template value
!!
!!     -digits N
!!             set threshold at which to report values as a number of digits
!!             if -digits is specified -percent is ignored.
!!
!!     -margin XXX.XX
!!             set threshold to a relative margin of the magnitude of the values
!!
!!     -verbose
!!        shows the lines that pass the criteria from OLDFILE as well.
!!
!!     --help
!!        display this help text and exit
!!
!!     --version
!!        display information on the code version and exit
!!
!!##USAGE
!!  1. GENERATE TEMPLATE:  call all your numeric procedures over their
!!     allowed ranges and print the values to a file on your original system.
!!     Save this file as your master QA template.
!!  2. GENERATE TRIAL DATA:  When you port the procedures to another system or
!!     recompile using a new compiler run your QA program again and save
!!     the second file.
!!  3. TEST/COMPARE:  run the numdiff(1) program:
!!
!!       numdiff -old MASTER_TEMPLATE_FILE -new NEW_OUTPUT_FILE -percent 0.0001
!!
!!##EXAMPLE
!!
!!
!!    We will assume we have two files meeting the above criteria called
!!    "cray_results.txt" and "cygwin_results.txt". To compare the values
!!    we enter
!!
!!      numdiff -old cray_results.txt -new cygwin_results.txt -percent 0.00001
!!
!!    A diff(1) of the following input files would show every line, as one
!!    uses the "E" prefix for exponents, while the other uses "D". Even when a
!!    diff(1) would show few lines, you have to inspect each difference to see
!!    how large a difference in value was found. Using numdiff(1) you can
!!    ignore most insignificant differences.
!!
!!    OUTPUT
!!
!!    Results from the run are
!!
!!      >        | *numdiff* numerical differences
!!      >        | old file=in1
!!      >        | new file=in2
!!      >        | percent threshold =  0.1E-006 (%)
!!      >--------|------------------------------------------------------------------------------------------------------------
!!      >old     |    0.32000000E+02    0.88589141E-01   -0.17863993E-01   -0.36879559E-04    0.16022057E-01    0.31990327E+02
!!      >       8|                                       ###############   ###############
!!      >new     |    0.32000000D+02    0.88589141D-01   -0.17863994D-01   -0.36879562D-04    0.16022057D-01    0.31990327D+02
!!      >>>>>>>>>|maximum difference =.81345875138179356E-005(%)
!!      >--------|------------------------------------------------------------------------------------------------------------
!!
!!    This indicates values did not pass on line 8. "#" characters underline the values.
!!
!!
!! Input file "cray_results.txt"
!!
!!    1TESTS STARTED.
!!
!!
!!    1          T,PSL(T),HSL(T),SSL(T),VSL(T),TSLH(H)
!!    0         PMIN=    0.885891E-01    PMAX=    0.320823E+04    DELP=    0.100000E+03
!!              TMIN=    0.320000E+02    TMAX=    0.705470E+03    DELT=    0.200000E+02
!!    0
!!        0.32000000E+02    0.88589141E-01   -0.17863993E-01   -0.36879559E-04    0.16022057E-01    0.31990327E+02
!!        0.40000000E+02    0.12163360E+00    0.80272719E+01    0.16194175E-01    0.16018940E-01    0.40003431E+02
!!        0.60000000E+02    0.25611401E+00    0.28059533E+02    0.55503370E-01    0.16033165E-01    0.60005984E+02
!!        0.80000000E+02    0.50682853E+00    0.48036541E+02    0.93222577E-01    0.16071928E-01    0.79998151E+02
!!        0.10000000E+03    0.94923553E+00    0.67998872E+02    0.12954114E+00    0.16129956E-01    0.99994830E+02
!!        0.12000000E+03    0.16927366E+01    0.87966229E+02    0.16459153E+00    0.16204270E-01    0.11999684E+03
!!        0.14000000E+03    0.28891787E+01    0.10794963E+03    0.19847772E+00    0.16293096E-01    0.14000080E+03
!!        0.16000000E+03    0.47413557E+01    0.12795766E+03    0.23128914E+00    0.16395272E-01    0.16000351E+03
!!        0.18000000E+03    0.75110274E+01    0.14799946E+03    0.26310734E+00    0.16510005E-01    0.18000351E+03
!!        0.20000000E+03    0.11526035E+02    0.16808606E+03    0.29400880E+00    0.16636808E-01    0.20000116E+03
!!        0.22000000E+03    0.17186197E+02    0.18823058E+03    0.32406594E+00    0.16775474E-01    0.21999785E+03
!!        0.24000000E+03    0.24967794E+02    0.20844802E+03    0.35334719E+00    0.16926045E-01    0.23999513E+03
!!        0.26000000E+03    0.35426601E+02    0.22875486E+03    0.38191689E+00    0.17088786E-01    0.25999384E+03
!!        0.28000000E+03    0.49199533E+02    0.24916873E+03    0.40983516E+00    0.17264170E-01    0.27999361E+03
!!        0.30000000E+03    0.67005075E+02    0.26970823E+03    0.43715817E+00    0.17452875E-01    0.29999280E+03
!!        0.32000000E+03    0.89642735E+02    0.29039297E+03    0.46393855E+00    0.17655789E-01    0.31998899E+03
!!        0.34000000E+03    0.11799178E+03    0.31124389E+03    0.49022610E+00    0.17874025E-01    0.33998018E+03
!!        0.36000000E+03    0.15300954E+03    0.33228370E+03    0.51606862E+00    0.18108958E-01    0.35996652E+03
!!        0.38000000E+03    0.19572950E+03    0.35353750E+03    0.54151277E+00    0.18362259E-01    0.38000205E+03
!!        0.40000000E+03    0.24725940E+03    0.37509279E+03    0.56667770E+00    0.18637646E-01    0.40003002E+03
!!        0.42000000E+03    0.30877960E+03    0.39689616E+03    0.59150311E+00    0.18935141E-01    0.41998032E+03
!!        0.44000000E+03    0.38154169E+03    0.41902722E+03    0.61609004E+00    0.19258996E-01    0.43998928E+03
!!        0.46000000E+03    0.46686778E+03    0.44153583E+03    0.64049662E+00    0.19613160E-01    0.46000991E+03
!!        0.48000000E+03    0.56615075E+03    0.46448172E+03    0.66478705E+00    0.20002609E-01    0.48002037E+03
!!        0.50000000E+03    0.68085599E+03    0.48793750E+03    0.68903400E+00    0.20433708E-01    0.50001633E+03
!!        0.52000000E+03    0.81252603E+03    0.51199311E+03    0.71332237E+00    0.20914751E-01    0.52000390E+03
!!        0.54000000E+03    0.96279001E+03    0.53676266E+03    0.73775486E+00    0.21456790E-01    0.53999326E+03
!!        0.56000000E+03    0.11333816E+04    0.56239502E+03    0.76246065E+00    0.22074947E-01    0.55999280E+03
!!        0.58000000E+03    0.13261708E+04    0.58909059E+03    0.78760893E+00    0.22790623E-01    0.58000418E+03
!!        0.60000000E+03    0.15432192E+04    0.61712901E+03    0.81343122E+00    0.23635444E-01    0.60001858E+03
!!        0.62000000E+03    0.17868695E+04    0.64691984E+03    0.84026231E+00    0.24659110E-01    0.62001710E+03
!!        0.64000000E+03    0.20598878E+04    0.67911317E+03    0.86863104E+00    0.25947611E-01    0.63998803E+03
!!        0.66000000E+03    0.23656783E+04    0.71493433E+03    0.89954128E+00    0.27678770E-01    0.66001396E+03
!!        0.68000000E+03    0.27085898E+04    0.75845725E+03    0.93651903E+00    0.30369395E-01    0.68000016E+03
!!        0.70000000E+03    0.30943291E+04    0.82243999E+03    0.99006883E+00    0.36618048E-01    0.70000112E+03
!!        0.70547000E+03    0.32082348E+04    0.90600741E+03    0.10611600E+01    0.50778529E-01    0.00000000E+00
!!    0         SUM=  0.6353636479732371E+05
!!
!!
!! Input file "cygwin_results.txt"
!!
!!    1TESTS STARTED.
!!
!!
!!    1          T,PSL(T),HSL(T),SSL(T),VSL(T),TSLH(H)
!!    0         PMIN=    0.885891E-01    PMAX=    0.320823E+04    DELP=    0.100000E+03
!!              TMIN=    0.320000E+02    TMAX=    0.705470E+03    DELT=    0.200000E+02
!!    0
!!        0.32000000D+02    0.88589141D-01   -0.17863994D-01   -0.36879562D-04    0.16022057D-01    0.31990327D+02
!!        0.40000000D+02    0.12163360D+00    0.80272719D+01    0.16194175D-01    0.16018940D-01    0.40003431D+02
!!        0.60000000D+02    0.25611401D+00    0.28059533D+02    0.55503370D-01    0.16033165D-01    0.60005984D+02
!!        0.80000000D+02    0.50682853D+00    0.48036541D+02    0.93222577D-01    0.16071928D-01    0.79998151D+02
!!        0.10000000D+03    0.94923553D+00    0.67998872D+02    0.12954114D+00    0.16129956D-01    0.99994830D+02
!!        0.12000000D+03    0.16927366D+01    0.87966229D+02    0.16459153D+00    0.16204270D-01    0.11999684D+03
!!        0.14000000D+03    0.28891787D+01    0.10794963D+03    0.19847772D+00    0.16293096D-01    0.14000080D+03
!!        0.16000000D+03    0.47413557D+01    0.12795766D+03    0.23128914D+00    0.16395272D-01    0.16000351D+03
!!        0.18000000D+03    0.75110274D+01    0.14799946D+03    0.26310734D+00    0.16510005D-01    0.18000351D+03
!!        0.20000000D+03    0.11526035D+02    0.16808606D+03    0.29400880D+00    0.16636808D-01    0.20000116D+03
!!        0.22000000D+03    0.17186197D+02    0.18823058D+03    0.32406594D+00    0.16775474D-01    0.21999785D+03
!!        0.24000000D+03    0.24967794D+02    0.20844802D+03    0.35334719D+00    0.16926045D-01    0.23999513D+03
!!        0.26000000D+03    0.35426601D+02    0.22875486D+03    0.38191689D+00    0.17088786D-01    0.25999384D+03
!!        0.28000000D+03    0.49199533D+02    0.24916873D+03    0.40983516D+00    0.17264170D-01    0.27999361D+03
!!        0.30000000D+03    0.67005075D+02    0.26970823D+03    0.43715817D+00    0.17452875D-01    0.29999280D+03
!!        0.32000000D+03    0.89642735D+02    0.29039297D+03    0.46393855D+00    0.17655789D-01    0.31998899D+03
!!        0.34000000D+03    0.11799178D+03    0.31124389D+03    0.49022610D+00    0.17874025D-01    0.33998018D+03
!!        0.36000000D+03    0.15300954D+03    0.33228370D+03    0.51606862D+00    0.18108958D-01    0.35996652D+03
!!        0.38000000D+03    0.19572950D+03    0.35353750D+03    0.54151277D+00    0.18362259D-01    0.38000205D+03
!!        0.40000000D+03    0.24725940D+03    0.37509279D+03    0.56667770D+00    0.18637646D-01    0.40003002D+03
!!        0.42000000D+03    0.30877960D+03    0.39689616D+03    0.59150311D+00    0.18935141D-01    0.41998032D+03
!!        0.44000000D+03    0.38154169D+03    0.41902722D+03    0.61609004D+00    0.19258996D-01    0.43998928D+03
!!        0.46000000D+03    0.46686778D+03    0.44153583D+03    0.64049662D+00    0.19613160D-01    0.46000991D+03
!!        0.48000000D+03    0.56615075D+03    0.46448172D+03    0.66478705D+00    0.20002609D-01    0.48002037D+03
!!        0.50000000D+03    0.68085599D+03    0.48793750D+03    0.68903400D+00    0.20433708D-01    0.50001633D+03
!!        0.52000000D+03    0.81252603D+03    0.51199311D+03    0.71332237D+00    0.20914751D-01    0.52000390D+03
!!        0.54000000D+03    0.96279001D+03    0.53676266D+03    0.73775486D+00    0.21456790D-01    0.53999326D+03
!!        0.56000000D+03    0.11333816D+04    0.56239502D+03    0.76246065D+00    0.22074947D-01    0.55999280D+03
!!        0.58000000D+03    0.13261708D+04    0.58909059D+03    0.78760893D+00    0.22790623D-01    0.58000418D+03
!!        0.60000000D+03    0.15432192D+04    0.61712901D+03    0.81343122D+00    0.23635444D-01    0.60001858D+03
!!        0.62000000D+03    0.17868695D+04    0.64691984D+03    0.84026231D+00    0.24659110D-01    0.62001710D+03
!!        0.64000000D+03    0.20598878D+04    0.67911317D+03    0.86863104D+00    0.25947611D-01    0.63998803D+03
!!        0.66000000D+03    0.23656783D+04    0.71493433D+03    0.89954128D+00    0.27678770D-01    0.66001396D+03
!!        0.68000000D+03    0.27085898D+04    0.75845725D+03    0.93651903D+00    0.30369395D-01    0.68000016D+03
!!        0.70000000D+03    0.30943291D+04    0.82243999D+03    0.99006883D+00    0.36618048D-01    0.70000112D+03
!!        0.70547000D+03    0.32082348D+04    0.90600741D+03    0.10611600D+01    0.50778529D-01    0.00000000D+00
!!    0         SUM=  0.6353636479675987E+05
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
version_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        numdiff(1f)>',&
'@(#)DESCRIPTION:    compare otherwise essentially identical files for numeric differences.>',&
'@(#)VERSION:        v3.0.0, 20131201>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COPYRIGHT:      1985, 1986, 1989, 1990, 20090501, 20131129 John. S. Urban>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'                    There is NO WARRANTY, to the extent permitted by law.>',&
'']
end subroutine setup
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
end program numdiff
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()(
!===================================================================================================================================
