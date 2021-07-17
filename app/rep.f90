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
'       rep(1f) - [FILE FILTER] replace fixed strings in files                   ',&
'       (LICENSE:PD)                                                             ',&
'SYNOPSIS                                                                        ',&
'       rep filenames -c /from/to/ [ -verbose][ -dryrun]|[ -help| -version]      ',&
'       (LICENSE:PD)                                                             ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'       This is an example program for the REPLACE(3f) function. It              ',&
'       still has some flaws.                                                    ',&
'                                                                                ',&
'       The rep(1) utility changes strings in-place in files.                    ',&
'                                                                                ',&
'       files named on the command are modified in-place, so you may want        ',&
'       to make a copy of the original before converting it. rep(1) prints       ',&
'       a message indicating which of the input files it actually modifies.      ',&
'                                                                                ',&
'       Do not change binary files with this program, which uses sequential      ',&
'       access to read and write the files. It can corrupt binary files.         ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'       -c /from/to/  "from" represents a string to look for and "to" represents ',&
'                     its replacement.                                           ',&
'       -verbose      Print information about what the program does.             ',&
'       --help        Display a help message and exit.                           ',&
'       --version     Display version information and exit.                      ',&
'       --dryrun      Does all file operations except for moving the             ',&
'                     changed file back to the original. Implies --version.      ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!        rep(1f) - [FILE FILTER] replace fixed strings in files
!!        (LICENSE:PD)
!!##SYNOPSIS
!!
!!        rep filenames -c /from/to/ [ -verbose][ -dryrun]|[ -help| -version]
!!        (LICENSE:PD)
!!
!!##DESCRIPTION
!!        This is an example program for the REPLACE(3f) function. It
!!        still has some flaws.
!!
!!        The rep(1) utility changes strings in-place in files.
!!
!!        files named on the command are modified in-place, so you may want
!!        to make a copy of the original before converting it. rep(1) prints
!!        a message indicating which of the input files it actually modifies.
!!
!!        Do not change binary files with this program, which uses sequential
!!        access to read and write the files. It can corrupt binary files.
!!
!!##OPTIONS
!!        -c /from/to/  "from" represents a string to look for and "to" represents
!!                      its replacement.
!!        -verbose      Print information about what the program does.
!!        --help        Display a help message and exit.
!!        --version     Display version information and exit.
!!        --dryrun      Does all file operations except for moving the
!!                      changed file back to the original. Implies --version.
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
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
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        rep(1)>',&
'@(#)DESCRIPTION:    replace fixed strings in files>',&
'@(#)VERSION:        1.0, 20171113>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       2021-06-26 18:31:01 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program rep
use M_strings, only : replace
use M_kracken, only : kracken, lget, sget, sgets
use M_kracken, only : IPvalue ! length of keyword value
use M_kracken, only : kracken_comment
use M_io,      only : scratch
implicit none
logical                            :: verbose
logical                            :: dryrun
character(len=IPvalue),allocatable :: filenames(:)
character(len=:),allocatable       :: chng
integer                 :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   kracken_comment=char(0)
   ! define command arguments,default values and crack command line
   call kracken('rep','-help .false. -version .false. -c -verbose .false. -dryrun .false.')
!-----------------------------------------------------------------------------------------------------------------------------------
   call help_usage(lget('rep_help'))                                ! if -help option is present, display help text and exit
   call help_version(lget('rep_version'))                           ! if -version option is present, display version text and exit
   filenames=sgets('rep_oo')
   chng=trim(sget('rep_c'))
   verbose=lget('rep_verbose')
   dryrun=lget('rep_dryrun')
   if(dryrun)verbose=.true.
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=1,size(filenames)
      call dofile()
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine dofile()
use M_io,    only  : read_line
use M_verify, only  : stderr
use M_system, only : system_rename, system_remove, system_perror
integer                      :: lun_in,lun_out
character(len=256)           :: msg
integer                      :: ios
character(len=:),allocatable :: line
integer                      :: ierr
integer                      :: icount
integer                      :: ilines
character(len=:),allocatable :: scratch_file
logical                      :: exists, open
!-----------------------------------------------------------------------------------------------------------------------------------
   OKBLOCK: block
   icount=0
   scratch_file=''
   lun_in=-1        ! should be a bad value for a LUN
   lun_out=-1       ! should be a bad value for a LUN
!-----------------------------------------------------------------------------------------------------------------------------------
   open(newunit=lun_in,                  &
      & file=filenames(i),               &
      & iostat=ios,                      &
      & status='old',                    &
      & form='formatted',                &
      & action='read',                   &
      & access='sequential',             &
      & iomsg=msg)
   if(ios.ne.0)then
      close(lun_in,iostat=ios)
      call stderr('*rep* ERROR : '//msg)
      exit OKBLOCK
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
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
      close(lun_out,iostat=ios)
      call stderr(trim(filenames(i))//' : '//msg)
      exit OKBLOCK
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ilines=0
   INFINITE: do while (read_line(line,lun_in)==0)
      ilines=ilines+1
      line=replace(line,ierr=ierr,cmd='c'//chng)
      if(ierr.gt.0)then !
         if(icount.eq.0)write(*,'(a)')'==>'//trim(filenames(i))
         if(verbose)then
            write(*,'(i0.4," : ",a)')ilines,line
         endif
         icount=icount+1
      elseif(ierr.lt.0)then
         call stderr('error changing string'//chng)
         exit OKBLOCK
      endif
      write(lun_out,'(a)',iostat=ios,iomsg=msg)line
      if(ios.ne.0)then
         call stderr(msg)
         exit OKBLOCK
      endif
   enddo INFINITE
   if(icount.ne.0.and..not.dryrun)then                 ! if a change was made move scratch file back to original filename
      ierr=system_rename(scratch_file,filenames(i))
      if(ierr.ne.0)then
         write(*,*)'ERROR RENAMING FILE ',ierr
         call system_perror('*rep*')
      endif
   endif
   endblock OKBLOCK
!-----------------------------------------------------------------------------------------------------------------------------------

   inquire(unit=lun_out, exist=exists, opened=open)
   if(open)then
      close(unit=lun_out,iostat=ios)               ! ensure output file closed so a system command can be performed
   endif
   if(scratch_file.ne.'')then
      ierr=system_remove(scratch_file)             ! unconditionally remove scratch file
   endif

   inquire(unit=lun_in, exist=exists, opened=open)
   if(open)then
      close(unit=lun_in,iostat=ios)                ! unconditionally close input file
   endif

end subroutine dofile
!-----------------------------------------------------------------------------------------------------------------------------------
end program rep
!-----------------------------------------------------------------------------------------------------------------------------------
