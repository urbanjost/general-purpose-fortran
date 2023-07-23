program sub
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
use M_CLI2,    only : set_args, lget, sget, filenames=>unnamed
use M_strings, only : replace
use M_io,      only : read_line, separator, splitpath, get_tmp
use M_system,  only : system_rename, system_remove, system_perror
use M_uuid,    only : generate_uuid
implicit none
logical                       :: verbose, dryrun, case, ask
character(len=1)              :: answer
character(len=:),allocatable  :: old, new
integer                       :: i
character(len=:),allocatable  :: help_text(:)
character(len=:),allocatable  :: version_text(:)

   call setup()
   ! define command arguments,default values and crack command line
   call set_args('--ignorecase:i -dryrun:d F -ask:a F',help_text,version_text)

   case=lget('ignorecase')
   dryrun=lget('dryrun')
   ask=lget('ask')
   verbose=lget('verbose')

   if(dryrun)verbose=.true.

   if(size(filenames).ge.3)then
      old=trim(filenames(1))
      new=trim(filenames(2))
      do i=3,size(filenames)
         call dofile()
      enddo
   else
      write(*,'(a)')'<ERROR> insufficient arguments: sub OLD NEW [OPTIONS] FILE(S)'
   endif

contains

subroutine dofile()
integer                      :: lun_in,lun_out
character(len=256)           :: msg
integer                      :: ios
character(len=:),allocatable :: line
character(len=:),allocatable :: maybe
integer                      :: ierr
integer                      :: icount
integer                      :: ilines
character(len=:),allocatable :: scratch_file
logical                      :: exists, open

   OKBLOCK: block
   icount=0
   scratch_file=''
   lun_in=-1        ! should be a bad value for a LUN
   lun_out=-1       ! should be a bad value for a LUN

   open(newunit=lun_in,        &
      & file=filenames(i),     &
      & iostat=ios,            &
      & status='old',          &
      & form='formatted',      &
      & action='read',         &
      & access='sequential',   &
      & iomsg=msg)
   if(ios.ne.0)then
      if(lun_in.ne.-1)close(lun_in,iostat=ios)
      write(stderr,*)'*sub* ERROR : '//msg
      exit OKBLOCK
   endif

   scratch_file=scratch('./')
   open(newunit=lun_out,      &
      & file=scratch_file,    &
      & iostat=ios,           &
      & status='new',         &
      & form='formatted',     &
      & action='write',       &
      & access='sequential',  &
      & iomsg=msg)
   if(ios.ne.0)then
      if(lun_out.ne.-1)close(lun_out,iostat=ios)
      write(stderr,*)trim(filenames(i))//' : '//msg
      exit OKBLOCK
   endif

   ilines=0
   INFINITE: do while (read_line(line,lun_in)==0)
      ilines=ilines+1
      if(ask)then
         maybe=replace(line,old,new,ierr=ierr,ignorecase=case)
         if(maybe.ne.line)then
            write(*,*)'OLD:'//line
            write(*,*)'NEW:'//maybe
            write(*,'(a)',advance='no')'CHANGE?'
            read(*,'(a)')answer
            if(answer.eq.'y'.or.answer.eq.'Y')then
               line=maybe
            endif
         else
            line=maybe
         endif
      else
         line=replace(line,old,new,ierr=ierr,ignorecase=case)
      endif
      if(ierr.gt.0)then !
         if(icount.eq.0)write(*,'(a)')'==>'//trim(filenames(i))
         if(verbose)then
            write(*,'(i0.4," : ",a)')ilines,line
         endif
         icount=icount+1
      elseif(ierr.lt.0)then
         write(stderr,*)'error changing string'//old//' to '//new
         exit OKBLOCK
      endif
      write(lun_out,'(a)',iostat=ios,iomsg=msg)line
      if(ios.ne.0)then
         write(stderr,*)msg
         exit OKBLOCK
      endif
   enddo INFINITE
   if(icount.ne.0.and..not.dryrun)then                 ! if a change was made move scratch file back to original filename
      ierr=system_rename(scratch_file,filenames(i))
      if(ierr.ne.0)then
         write(*,*)'ERROR RENAMING FILE ',ierr
         call system_perror('*sub*')
      endif
   endif
   endblock OKBLOCK

   if(lun_out.ne.-1)then
      inquire(unit=lun_out, exist=exists, opened=open)
      if(open)then
         close(unit=lun_out,iostat=ios)               ! ensure output file closed so a system command can be performed
      endif
   endif
   if(scratch_file.ne.'')then
      ierr=system_remove(scratch_file)             ! unconditionally remove scratch file
   endif

   if(lun_in.ne.-1)then
      inquire(unit=lun_in, exist=exists, opened=open)
      if(open)then
         close(unit=lun_in,iostat=ios)                ! unconditionally close input file
      endif
   endif

end subroutine dofile

!>
!!##NAME
!!      scratch(3f) - [M_io:QUERY] Return the name of a scratch file
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!     function scratch(prefix) result(tname)
!!
!!      character(len=:),allocatable         :: tname
!!      character(len=*),intent(in),optional :: prefix
!!##DESCRIPTION
!!
!!    Fortran supports non-retainable scratch files via
!!    OPEN(STATUS='SCRATCH',...) . There are circumstances where a file with
!!    a unique name is required instead. Specifying the pathname of a file
!!    can be required for performance reasons, file space limitations, or
!!    to support the ability for other processes or subprocesses to access
!!    the file.
!!
!!    SCRATCH(3f) Return the name of a scratch file in the scratch directory
!!    set by the most common environment variables used to designate a
!!    scratch directory unless the prefix contains the character "/" or "\".
!!
!!    $TMPDIR is the canonical environment variable in Unix and POSIX[1]
!!    used to specify a temporary directory for scratch space. If $TMPDIR
!!    is not set, $TEMP, $TEMPDIR, and $TMP are examined in that order. If
!!    nothing is set "/tmp/" is used.
!!
!!##OPTIONS
!!    prefix  an optional prefix for the leaf of the filename. A suffix
!!            created by genuuid(3) is used to make the name unique. The
!!            prefix is used as-is if it contains the character "/" or
!!            "\". Otherwise, the prefix is prefixed by the first value
!!            that is not blank from the set
!!            {$TMPDIR, $TEMP, $TEMPDIR, $TMP, /tmp}.
!!
!!            The default prefix is the basename of the program that
!!            called the procedure (the name trimmed of directories and
!!            anything from the right-most period in the name to the end
!!            of the name).
!!##EXAMPLE
!!
!!   Sample:
!!
!!     program demo_scratch
!!     use M_io, only : scratch
!!     implicit none
!!     write(*,*)'find good scratch file name candidates; one should test if writable'
!!     call printit('JUNK:')
!!     call printit('./')
!!     call printit('/var/tmp/')
!!     call printit('')
!!     call printit()
!!     contains
!!     subroutine printit(NAME)
!!     character(len=*),intent(in),optional :: NAME
!!     if(present(NAME))then
!!        write(*,'(a,t20,a)')NAME,scratch(NAME)
!!     else
!!        write(*,'(a,t20,a)')'*NOT PRESENT*',scratch()
!!     endif
!!     end subroutine printit
!!     end program demo_scratch
!!
!!   Results:
!!
!!    >  find good scratch file name candidates; one should test if writable
!!    > JUNK:              /tmp/JUNK:405d766e-1320-4405-50e1-5d88fffbee9a.scr
!!    > ./                 ./xx-901606b1-6ad2-4e96-6b17-e8bffedf2452.scr
!!    > /var/tmp/          /var/tmp/xx-3f5c55fa-17ca-4020-4a05-a9d9cfad8dbe.scr
!!    >                    /tmp/f10e0491-a2ff-4455-5ff6-55d7dfe7fa8c.scr
!!    > *NOT PRESENT*      /tmp/xx-f4fed5f7-3694-4609-5af4-8902ffa75839.scr
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function scratch(prefix) result(tname)

! ident_1="@(#) M_io scratch(3f) Return the name of a scratch file"

character(len=*),intent(in),optional :: prefix
character(len=:),allocatable         :: tname

integer,parameter                    :: maxlen=4096
character(len=maxlen)                :: path
character(len=maxlen)                :: bname
integer                              :: ilen
character(len=1)                     :: sep
   sep=separator()

   if(present(prefix))then
      ilen=len_trim(prefix)
      if(index(prefix,'/')+index(prefix,'\') /= 0)then  ! if contains with / or \ do not use current temp directory but use as-is
         if(prefix(ilen:ilen) == '/'.or.prefix(ilen:ilen) == '\')then ! assumed that the prefix is a directory name
            call get_command_argument(number=0,value=path)
            call splitpath(path,basename=bname)
            tname=trim(prefix)//trim(bname)//'-'//generate_uuid()//'.scr'
         else
            tname=trim(prefix)//generate_uuid()//'.scr'
         endif
      else
         tname=get_tmp()//trim(prefix)//generate_uuid()//'.scr'
      endif
   else
      call get_command_argument(number=0,value=path)
      call splitpath(path,basename=bname)
      tname=get_tmp()//trim(bname)//'-'//generate_uuid()//'.scr'
   endif
end function scratch

subroutine setup()

! @(#)help_usage(3f): sets help information

version_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        sub(1)>                                              ',&
'@(#)DESCRIPTION:    replace old string with new string in files)>        ',&
'@(#)VERSION:        1.1, 20210224>                                       ',&
'@(#)AUTHOR:         John S. Urban>                                       ',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>      ',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/                 ',&
'@(#)LICENSE:        MIT. This is free software: you are free to change and redistribute it.',&
'']
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'   sub(1f) - [FILE EDIT] replace fixed strings in files',&
'   (LICENSE:MIT)                                       ',&
'SYNOPSIS                                               ',&
'    sub [ --ignorecase][ --dryrun][ --verbose] old new filename(s)',&
'                                                                  ',&
'     or                                                           ',&
'                                                                  ',&
'    sub --help| --version| --usage                                ',&
'                                                                  ',&
'DESCRIPTION                                                       ',&
'   The sub(1) utility changes strings in-place in files.          ',&
'                                                                  ',&
'   trailing spaces on OLD and NEW are ignored.                    ',&
'                                                                  ',&
'   TABS are expanded.                                             ',&
'                                                                  ',&
'   files named on the command are modified in-place, so you may want',&
'   to make a copy of the original before converting it. sub(1) prints',&
'   a message indicating any of the input files it actually modifies. ',&
'                                                                     ',&
'   Do not change binary files with this program, which uses sequential',&
'   access to read and write the files. It can corrupt binary files.   ',&
'                                                                      ',&
'OPTIONS                                                               ',&
'   old              string to look for                                ',&
'   new              the replacement string                            ',&
'   filenames        list of files to replace strings in               ',&
'   --ignorecase,-i  ignore case of input                              ',&
'   --ask,-a         interactively confirm each change                 ',&
'   --dryrun,-d      does all file operations except for moving the    ',&
'                    changed file back to the original. Implies --verbose.',&
'                                                                         ',&
'   --help,-h        display a help message and exit.                     ',&
'   --usage,-u       display options table                                ',&
'   --version,-v     display version information and exit.                ',&
'   --verbose,-V     print information about what the program changes.    ',&
'                                                                         ',&
'AUTHOR                                                                   ',&
'   John S. Urban                                                         ',&
'                                                                         ',&
'LICENSE                                                                  ',&
'   MIT                                                                   ',&
'']
end subroutine setup

end program sub
