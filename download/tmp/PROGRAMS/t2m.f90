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
'   t2m - [DEVELOPER] basic markup of text to a man(1) page                      ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   t2m FILE -cmd -section 1 -product "" -help .F. -version .F.                  ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Minimalist conversion of a text file to a man page. Lines that are all upper-case',&
'   starting in column 1 start a section. Otherwise the sections are printed essentially',&
'   as-is.                                                                       ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'     FILE                                                                       ',&
'        The input filename                                                      ',&
'                                                                                ',&
'     -section N  N is the man(1) section number. Typically the following categories',&
'                 are used:                                                      ',&
'                                                                                ',&
'                  User Commands       1  Executable programs or shell commands  ',&
'                  System Calls        2  System calls (functions provided by the kernel)',&
'                  Library Calls       3  Library calls (functions within program libraries)',&
'                  Special Files       4  Special files (usually found in /dev)  ',&
'                  File Formats        5  File formats and conventions (eg. /etc/passwd)',&
'                  Games               6  Games                                  ',&
'                  Miscellanous        7  Miscellaneous (including macro packages and conventions), e.g. man(7), groff(7)',&
'                  System Admin.       8  System administration commands (usually only for root)',&
'                  Kernel Extensions   9  Kernel routines [Non standard]         ',&
'                                                                                ',&
'                 See the man(1) page for man(1) for further details.            ',&
'                                                                                ',&
'     -cmd                                                                       ',&
'     -product                                                                   ',&
'     -help       display help and exit                                          ',&
'     -version    display version information and exit                           ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    t2m - [DEVELOPER] basic markup of text to a man(1) page
!!
!!##SYNOPSIS
!!
!!    t2m FILE -cmd -section 1 -product "" -help .F. -version .F.
!!
!!##DESCRIPTION
!!    Minimalist conversion of a text file to a man page. Lines that are all upper-case
!!    starting in column 1 start a section. Otherwise the sections are printed essentially
!!    as-is.
!!
!!##OPTIONS
!!      FILE
!!         The input filename
!!
!!      -section N  N is the man(1) section number. Typically the following categories
!!                  are used:
!!
!!                   User Commands       1  Executable programs or shell commands
!!                   System Calls        2  System calls (functions provided by the kernel)
!!                   Library Calls       3  Library calls (functions within program libraries)
!!                   Special Files       4  Special files (usually found in /dev)
!!                   File Formats        5  File formats and conventions (eg. /etc/passwd)
!!                   Games               6  Games
!!                   Miscellanous        7  Miscellaneous (including macro packages and conventions), e.g. man(7), groff(7)
!!                   System Admin.       8  System administration commands (usually only for root)
!!                   Kernel Extensions   9  Kernel routines [Non standard]
!!
!!                  See the man(1) page for man(1) for further details.
!!
!!      -cmd
!!      -product
!!      -help       display help and exit
!!      -version    display version information and exit
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
'@(#)PROGRAM:        t2m(1)>',&
'@(#)DESCRIPTION:    convert text into a man(1) page>',&
'@(#)VERSION:        1.0, 2016-05-14>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 9:59:09 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program test_t2m
use M_kracken, only: kracken, sget, sgets, iget, IPvalue, lget
implicit none
   character(len=1024)      :: alllines(10000)
   character(len=IPvalue)   :: cmd
   integer                  :: section
   character(len=IPvalue)   :: product
   character(len=IPvalue),allocatable :: filename
   integer                            :: inunit,ios,i
!-----------------------------------------------------------------------------------------------------------------------------------
!  parse command line arguments
   call kracken('t2m',' -cmd -section 3 -product " " -help .F. -version .F.' )
   call help_usage(lget('t2m_help'))       ! process -help switch
   call help_version(lget('t2m_version'))  ! process -version switch
   cmd=sget('t2m_cmd')
   product=sget('t2m_product')
   section=iget('t2m_section')
   i=1

   filename=sget('t2m_oo')
   if(filename.eq.'')then
      inunit=5
   else
      inunit=10
      open(unit=inunit,file=trim(filename))
   endif

   INFINITE: do i=1,size(alllines)
      read(inunit,'(a)',iostat=ios)alllines(i)
      if(ios.ne.0)then
         exit INFINITE
      endif
   enddo INFINITE

   alllines(i)='END_OF_MAN_PAGE'  ! assumes file did not fill array
   call t2m(alllines(:i),cmd,section,product)

   if(inunit.eq.10)then
      close(unit=inunit,iostat=ios)
   endif

contains
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
subroutine t2m(table,title,section,product)
!-----------------------------------------------------------------------------------------------------------------------------------
! o Line all in uppercase starting in left margin becomes a section header (.SH)
! o Line starting with . is assumed to be a raw *roff directive and is passed on as-is
!
! First line not starting in column 1 and less than column 5 establishes default paragraph indent
! if indented .ge. 5 some other amount left as-is unless in a list
!-----------------------------------------------------------------------------------------------------------------------------------
use M_strings, only : substitute, indent, s2v, v2s, notabs, upper, switch
use M_time,    only : now
implicit none
character(len=*),parameter  :: ident="@(#)t2m(3f): given a character array markup a simple man(1) page"
character(len=*),intent(in) :: table(:)
character(len=*),intent(in) :: title
character(len=*),intent(in) :: product
integer,intent(in)          :: section
integer                     :: isection
   character(len=1024)      :: line
   integer                  :: ios
   integer                  :: leading_spaces
   integer                  :: i
   integer                  :: ilen
   integer                  :: ileft
   integer                  :: iend
!-----------------------------------------------------------------------------------------------------------------------------------
   character(len=:),allocatable :: global_section
!-----------------------------------------------------------------------------------------------------------------------------------
!  The table below shows the section numbers of the manual followed by the types of pages they contain.
!
   character(len=*),parameter  :: sections(10)= [&
   &'User Commands       ',& !       1   Executable programs or shell commands
   &'System Calls        ',& !       2   System calls (functions provided by the kernel)
   &'Library Calls       ',& !       3   Library calls (functions within program libraries)
   &'Special Files       ',& !       4   Special files (usually found in /dev)
   &'File Formats        ',& !       5   File formats and conventions (eg. /etc/passwd)
   &'Games               ',& !       6   Games
   &'Miscellanous        ',& !       7   Miscellaneous (including macro packages and conventions), e.g. man(7), groff(7)
   &'System Admin.       ',& !       8   System administration commands (usually only for root)
   &'Kernel Extensions   ',& !       9   Kernel routines [Non standard]
   &'                    ']
!-----------------------------------------------------------------------------------------------------------------------------------
   if(section.le.1.or.section.gt.9)then
      isection=10
   else
      isection=section
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   write(*,'(a)')'." -----------------------------------------------------------------'
   write(*,'(a)')'.ll 78n /" set line length (must be set before .TH.'
   ! place a comment at the top of the file
   write(*,'(*(a))')'.\"manpage'
   write(*,'(a)')'.\" t'
   write(*,'(a)')'.\" ** The above line should force tbl to be a preprocessor **'
   write(*,'(*(a))')'.\"DO NOT MODIFY THIS FILE!  It was generated by t2m 1.0 at '//now('%w, %l %D, %Y %h:%m:%s')
   !  .TH title section [extra1] [extra2] [extra3]
   write(*,'(*(a))')'.TH "',trim(title),'" "',v2s(section),'" "',now('%L %Y'),'" "',trim(product),'" "',trim(sections(section)),'"'
   write(*,'(a)')'." * set default formatting'
   write(*,'(a)')'." disable hyphenation'
   write(*,'(a)')'.nh'
   write(*,'(a)')'." disable justification (adjust text to left margin only)'
   write(*,'(a)')'.ad l'
   write(*,'(a)')'." -----------------------------------------------------------------'
!-----------------------------------------------------------------------------------------------------------------------------------
   global_section=''
   ileft=0
   INFINITE: do i=1,size(table)
      call notabs(table(i),line,ilen)                            ! remove tab characters, which can be problematic
      leading_spaces=indent(line)                                ! find amount of leading white space
      !-----------------------------------------------------------------------------------------------------------------------------
      iend=len_trim(line)
      if(line(1:1).eq.'.')then
         write(*,'(a)',iostat=ios)line(:iend)                    ! assume embedded *roff commands. Write as-is
         cycle INFINITE
      endif
      !-----------------------------------------------------------------------------------------------------------------------------
      if(leading_spaces.eq.0.and.iend.ne.0.and.upper(line).eq.line)then ! all upper-case non-blank starting in column 1

         if(global_section.ne.'NAME'.and.global_section.ne.'')then
               write(*,'(a)')'.fi'
         endif

         global_section=line(:iend)
         if(all(switch(line(:iend)).eq.'-'))then                   ! if all characters in line are a dash
         elseif(all(switch(line(:iend)).eq.'='))then               ! if all characters in line are an equal sign
         else
            call substitute(line,'-','\-')                        ! escape any minus character found in input
            write(*,'(a)',iostat=ios)'.\" ****************************************************************************'
            write(*,'(a)',iostat=ios)'.SH "'//line(:iend)//'"'
            if(line(:iend).ne.'NAME')then
               write(*,'(a)')'.nf'
            endif
         endif
         cycle INFINITE
      elseif(iend.eq.0)then
         write(*,'(a)',iostat=ios)
      else
         write(*,'(a)',iostat=ios)line(:iend)
      endif
   enddo INFINITE
   write(*,'(a)')'.fi'
end subroutine t2m
!----------------------------------------------------------------------------------------------------------------------------------!
end program test_t2m
!----------------------------------------------------------------------------------------------------------------------------------!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!----------------------------------------------------------------------------------------------------------------------------------!
