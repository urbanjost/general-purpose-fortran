program swap
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
use M_CLI2,    only : set_args, lget, sget, filenames=>unnamed
use M_strings, only : replace
use M_io,      only : scratch, read_line
use M_system,  only : system_rename, system_remove, system_perror
implicit none
logical                       :: verbose, dryrun, case, ask
character(len=1)              :: answer
character(len=:),allocatable  :: old, new
integer                       :: i
character(len=:),allocatable  :: help_text(:)
character(len=:),allocatable  :: version_text(:)

   call setup()
   ! define command arguments,default values and crack command line
   call set_args('-i -dryrun:d F -ask:a F',help_text,version_text)

   case=lget('i')
   ask=lget('ask')
   verbose=lget('verbose')
   dryrun=lget('dryrun')
   if(dryrun)verbose=.true.

   if(size(filenames).ge.3)then
      old=trim(filenames(1))
      new=trim(filenames(2))
      do i=3,size(filenames)
         call dofile()
      enddo
   else
      write(*,'(a)')'<ERROR> insufficient arguments: swap OLD NEW [OPTIONS] FILE(S)'
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

   open(newunit=lun_in,                  &
      & file=filenames(i),               &
      & iostat=ios,                      &
      & status='old',                    &
      & form='formatted',                &
      & action='read',                   &
      & access='sequential',             &
      & iomsg=msg)
   if(ios.ne.0)then
      if(lun_in.ne.-1)close(lun_in,iostat=ios)
      write(stderr,*)'*swap* ERROR : '//msg
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
         call system_perror('*swap*')
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

subroutine setup()

! @(#)help_usage(3f): sets help information

version_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        swap(1)>',&
'@(#)DESCRIPTION:    replace old string with new string in files)>',&
'@(#)VERSION:        1.0, 20180427>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.',&
'']
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'   swap(1f) - [FILE FILTER] replace fixed strings in files',&
'   (LICENSE:PD)',&
'SYNOPSIS',&
'   swap  [ -i][ -verbose][ -dryrun]|[ -help| -version] old new filename(s)',&
'   (LICENSE:PD)',&
'',&
'DESCRIPTION',&
'   ** This is an example program for the REPLACE(3f) function. It',&
'   still has some flaws.',&
'',&
'   The swap(1) utility changes strings in-place in files.',&
'',&
'   trailing spaces on OLD and NEW are ignored.',&
'   TABS are expanded.',&
'',&
'   files named on the command are modified in-place, so you may want',&
'   to make a copy of the original before converting it. swap(1) prints',&
'   a message indicating any of the input files it actually modifies.',&
'',&
'   Do not change binary files with this program, which uses sequential',&
'   access to read and write the files. It can corrupt binary files.',&
'',&
'OPTIONS',&
'   old           string to look for',&
'   new           the replacement string',&
'   filenames     list of files to replace strings in',&
'   -i            ignore case of input',&
'   -ask          interactively confirm each change',&
'   --dryrun      Does all file operations except for moving the',&
'                 changed file back to the original. Implies --verbose.',&
'   -verbose      Print information about what the program changes.',&
'   --help        Display a help message and exit.',&
'   --version     Display version information and exit.',&
'',&
'AUTHOR',&
'   John S. Urban',&
'',&
'LICENSE',&
'   Public Domain',&
'']
!>
!!##NAME
!!    swap(1f) - [FILE FILTER] replace fixed strings in files
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    swap  [ -i][ -verbose][ -dryrun]|[ -help| -version] old new filename(s)
!!    (LICENSE:PD)
!!
!!##DESCRIPTION
!!    ** This is an example program for the REPLACE(3f) function. It
!!    still has some flaws.
!!
!!    The swap(1) utility changes strings in-place in files.
!!
!!    trailing spaces on OLD and NEW are ignored.
!!    TABS are expanded.
!!
!!    files named on the command are modified in-place, so you may want
!!    to make a copy of the original before converting it. swap(1) prints
!!    a message indicating any of the input files it actually modifies.
!!
!!    Do not change binary files with this program, which uses sequential
!!    access to read and write the files. It can corrupt binary files.
!!
!!##OPTIONS
!!    old           string to look for
!!    new           the replacement string
!!    filenames     list of files to replace strings in
!!    -i            ignore case of input
!!    -ask          interactively confirm each change
!!    --dryrun      Does all file operations except for moving the
!!                  changed file back to the original. Implies --verbose.
!!    -verbose      Print information about what the program changes.
!!    --help        Display a help message and exit.
!!    --version     Display version information and exit.
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
end subroutine setup

end program swap
