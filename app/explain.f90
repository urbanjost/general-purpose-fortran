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
'   explain(1) - [HELP] reads and navigates a VMS-style help file.               ',&
'   (LICENSE:GNU LGPL)                                                           ',&
'SYNOPSIS                                                                        ',&
'  explain [HELP_FILENAME]                                                       ',&
'DESCRIPTION                                                                     ',&
'   explain(1) reads and navigates a VMS-style help file.                        ',&
'   This shows a simple help file:                                               ',&
'                                                                                ',&
'    >1 Help_File_Format                                                         ',&
'    >                                                                           ',&
'    >This is a brief suggestion of how a help file is laid out. A line          ',&
'    >that begins with a number marks the beginning of a topic. The text         ',&
'    >on that line is the label for the topic.                                   ',&
'    >                                                                           ',&
'    >2 What_the_User_Sees                                                       ',&
'    >                                                                           ',&
'    >On issuing the help command, the user sees the main topic printed out.     ',&
'    >Following this, a list of the immediate subtopics is presented.            ',&
'    >The user can proceed to a subtopic by typing its label.                    ',&
'    >                                                                           ',&
'    >3 Sub_Sub_Topics                                                           ',&
'    >                                                                           ',&
'    >The arrangement of topics is a little like the grouping of parentheses.    ',&
'    >                                                                           ',&
'    >2 Suggestions                                                              ',&
'    >                                                                           ',&
'    >You might want to keep each subtopic short, certainly no more than         ',&
'    >a page in length.                                                          ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'    HELP_FILENAME  Name of help file. That is, a text file marked up            ',&
'                   using the VMS help file format. In this format,              ',&
'                   a number in column 1 is a label, and indicates the           ',&
'                   beginning of a help topic. Label 1 is reserved for           ',&
'                   the main help topic; subtopics of the main topic have        ',&
'                   a label of 2, and so on.                                     ',&
'LICENSING                                                                       ',&
'  This code is distributed under the GNU LGPL license.                          ',&
'AUTHOR                                                                          ',&
' Lifecycle Information:                                                         ',&
'                                                                                ',&
'  Author:   John Burkardt, 2001-03-06, version 1.06                             ',&
'  Modified: John S. Urban, 2019-01-25, version 1.07                             ',&
'            Integrated into the GPF(General Purpose Fortran) format. The        ',&
'            basic login remains the same, sans some utility routines            ',&
'            that duplicated GPF routines; added the command --help              ',&
'            and --version switches; allowed abbreviated case-insensitive        ',&
'            topics.                                                             ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!    explain(1) - [HELP] reads and navigates a VMS-style help file.
!!    (LICENSE:GNU LGPL)
!!##SYNOPSIS
!!
!!   explain [HELP_FILENAME]
!!##DESCRIPTION
!!    explain(1) reads and navigates a VMS-style help file.
!!    This shows a simple help file:
!!
!!     >1 Help_File_Format
!!     >
!!     >This is a brief suggestion of how a help file is laid out. A line
!!     >that begins with a number marks the beginning of a topic. The text
!!     >on that line is the label for the topic.
!!     >
!!     >2 What_the_User_Sees
!!     >
!!     >On issuing the help command, the user sees the main topic printed out.
!!     >Following this, a list of the immediate subtopics is presented.
!!     >The user can proceed to a subtopic by typing its label.
!!     >
!!     >3 Sub_Sub_Topics
!!     >
!!     >The arrangement of topics is a little like the grouping of parentheses.
!!     >
!!     >2 Suggestions
!!     >
!!     >You might want to keep each subtopic short, certainly no more than
!!     >a page in length.
!!
!!##OPTIONS
!!     HELP_FILENAME  Name of help file. That is, a text file marked up
!!                    using the VMS help file format. In this format,
!!                    a number in column 1 is a label, and indicates the
!!                    beginning of a help topic. Label 1 is reserved for
!!                    the main help topic; subtopics of the main topic have
!!                    a label of 2, and so on.
!!##LICENSING
!!   This code is distributed under the GNU LGPL license.
!!##AUTHOR
!!  Lifecycle Information:
!!
!!   Author:   John Burkardt, 2001-03-06, version 1.06
!!   Modified: John S. Urban, 2019-01-25, version 1.07
!!             Integrated into the GPF(General Purpose Fortran) format. The
!!             basic login remains the same, sans some utility routines
!!             that duplicated GPF routines; added the command --help
!!             and --version switches; allowed abbreviated case-insensitive
!!             topics.
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
'@(#)PROGRAM:        explain(1)>',&
'@(#)DESCRIPTION:    reads and navigates a VMS-style help file.>',&
'@(#)VERSION:        1.06, 20010306>',&
'@(#)AUTHOR:         John Burkardt>',&
'@(#)VERSION:        1.07, 20190125>',&
'@(#)MODIFIED:       John S. Urban>',&
'@(#)COMPILED:       2025-06-29 08:20:34 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
program explain
use M_time, only    : now
use M_kracken, only : kracken, sget, lget
implicit none
character(len=4096) :: help_file
integer             :: ios
  call kracken('explain','-help .false. -version .false.') ! define command arguments,default values and crack command line
   call help_usage(lget('explain_help'))                   ! if -help option is present, display help text and exit
   call help_version(lget('explain_version'))              ! if -version option is present, display version text and exit
   help_file=sget('explain_oo')                            ! Get the input file name.
  write ( *, '(2a)' ) 'EXPLAIN version 1.07: A VMS-style help facility: ',now("%w, %l %d, %Y %H:%m:%s.%x %N")
  if(help_file.eq.'')then
    write ( *, '(a)',advance='no') 'What is the name of the help file to be read?'
    read ( *, '(a)' ,iostat=ios) help_file
    if( ios.ne.0.or.help_file == ' ' )then
      stop
    endif
  endif
  call hlpvms ( help_file )
  write ( *, '(2a)' ) 'EXPLAIN: Normal end of execution:', now("%w, %l %d, %Y %H:%m:%s.%x %N")
end program explain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine hlpvms ( help_file )
use M_strings, only : upper, isdigit, compact
use M_io,      only : notopen
  implicit none

! ident_1="@(#) HLPVMS(3f) provides extensive help from a VMS help file."

!  LICENSING:    This code is distributed under the GNU LGPL license.
!  MODIFIED:     06 March 2001
!  AUTHOR:       John Burkardt
!  PARAMETERS:   Input, character ( len=* ) HELP_FILE, the name of the help file.
  integer, parameter        :: maxtop = 10000
  character(len=80)         :: choice
  character(len=*)          :: help_file
  integer                   :: i
  integer                   :: ierror
  integer                   :: iline
  character(len=80)         :: inline
  integer                   :: ios
  integer                   :: itop
  integer                   :: jerror
  character(len=1)          :: lab
  integer                   :: lenc
  integer                   :: level
  character(len=80)         :: levelc(maxtop)
  integer                   :: levelm(10)
  integer                   :: levelo
  integer                   :: levelt(maxtop)
  integer                   :: lhunit
  integer                   :: move
  integer                   :: nline
  integer                   :: ntop
  integer                   :: num
  integer                   :: abbrv
  character(len=80)         :: output
  character(len=80)         :: prompt

  ierror = 0
  nline = 0
  lhunit=notopen(err=ierror)
  if ( ierror .ne. 0 ) then
    write ( *, '(a)' ) 'HLPVMS - Fatal error! Could not get a free FORTRAN unit.'
    ierror = 1
    return
  endif
  open ( unit = lhunit, file = help_file, status = 'old', iostat = ios )             ! Open help file
  if ( ios /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'HLPVMS - Fatal error! Could not open the help file.'
    ierror = 2
    return
  endif
  levelo = 0
  level = 1
  iline = 1
!  Move to beginning of current topic by reading MOVE lines from
!  the top of the file.  Record this position, corresponding to
!  the current LEVEL, in LEVELM, in case we later want to back up.
!
!  Print out the heading line of this topic.
  do
    jerror = 0
    move = iline
    levelm(level) = iline
    do i = 1, move-1
      read ( lhunit, '(1x)', iostat = ios )
      if ( ios /= 0 ) then
        ierror = 4
        write ( *, '(/,a)' ) 'HLPVMS - Fatal error!  Unexpected end of file, or other I/O error.'
        return
      endif
    end do
    read ( lhunit,'(a1,a75)', iostat = ios ) lab, inline
    if ( ios /= 0 ) then
      ierror = 4
      write ( *, '(/,a)' ) 'HLPVMS - Fatal error!  Unexpected end of file, or other I/O error.'
      return
    endif
    write ( *, '(/,a,/)' ) trim ( inline )
    nline = 3
!  o  If 'going down' or redisplaying, (as opposed to backing up), display information available under the current topic.
!  o  We stop printing when we hit a numeric label.
!  o  If this label is less than or equal to current level, there are no subtopics.
!  o  Otherwise, we now move ahead to print out the list of subtopics available for this topic.
    if ( level >= levelo ) then
      ntop = -1
      do
        read ( lhunit, '(a1,a75)', end=50 ) lab, inline
        move = move + 1
        if ( isdigit ( lab ) ) then
          read ( lab, '(i1)' ) num
          if ( num <= level ) then
            goto 50
          endif
          ntop = 0
          exit
        endif
        if ( nline >= 24 ) then
          read ( *, * )
          nline = 0
        endif
        nline = nline + 1
        write ( *, '(a)' ) trim ( inline )
      end do
    else
      ntop = 0
      inline = ' '
      lab = ' '
    endif
!  Locate each subtopic by examining column 1, searching for integer label.
!
!  Assuming we are at level LEVEL, we are searching for labels equal to LEVEL+1.
!  As we encounter each such label, we want to store the rest of the line as a subtopic.
!  We ignore labels greater than LEVEL+1 because these are sub-subtopics,
!  and we cease our search when we reach a label less than or equal to LEVEL.
    do
      if ( isdigit ( lab ) ) then
        read ( lab, '(i1)' ) num
        if ( num <= level ) then
          exit
        endif
        if ( num == level+1 ) then
          if ( ntop == maxtop+1 ) then
            write ( *, '(/,a)' ) 'EXPLAIN - Warning! Maximum number of topics reached!'
          else if ( ntop > maxtop ) then
          else
            ntop = ntop + 1
            if ( ntop == 1 ) then
              write ( *, '(/,a,/)' ) 'Help is available on:'
            endif
            inline=compact(inline)
            write ( *, '(a)' ) trim(inline)
            levelt(ntop) = move
            levelc(ntop) = inline
          endif
        endif
      endif
      read ( lhunit,'(a1,a75)', iostat = ios ) lab, inline
      if ( ios /= 0 ) then
        exit
      endif
      move = move + 1
    end do
50  continue                                                   !  Display subtopics.
    write ( *, '(/,a)' ) 'RETURN to back up, ? to redisplay.'
60  continue                                                   !  Prompt for user choice of new topic, exit, or back up.
    nline = 0
    if ( ntop > 0 ) then
      prompt = 'Enter topic you want help on, or RETURN or ?.'
    else
      prompt = 'Enter RETURN or ?.'
    endif
    write ( *, '(/,a)' ) trim ( prompt )
    nline = 0
    read ( *, '(a)', iostat = ios ) choice
    if ( ios /= 0 ) then
      ierror = 3
      close ( unit = lhunit )
      return
    endif
    choice=compact(choice)
    lenc = len_trim(choice)
    if ( lenc <= 0 ) then
      choice = '!'
    endif
    if ( choice == '!' .and. level == 1 ) then             !  Consider ending this help session.
      close ( unit = lhunit )
      return
    endif
    if ( ierror /= 0 ) then                                !  Two errors in a row, OK, but three suggests that something is wrong.
      jerror = jerror + 1
      if ( jerror <= 4 ) then
        goto 60
      endif
      write ( *, '(/,a)' ) 'HLPVMS - Fatal error!  Too many input errors in a row!'
      close ( unit = lhunit )
      return
    endif
    rewind lhunit                                           !  User wants to back up to a supertopic.  We must rewind.
    levelo = level
    if ( choice == '!' ) then
      level = level - 1
      iline = levelm(level)
    else if ( choice == '?' ) then                          !  Redisplay current topic.
    else                                                    !  User wants to go down to a subtopic.
      itop = 0
      abbrv= min(len_trim(choice), len_trim(levelc(i)))
      do i = 1, ntop
        if (  upper( choice(:abbrv)).eq. upper(levelc(i)(:abbrv))  ) then
          itop = i
          exit
        endif
      end do
      if ( itop == 0 ) then
        output = 'Sorry, no help available on "'// trim ( choice ) // '".'
        output=compact(output)
        write ( *, '(a)' ) trim(output)
        jerror = jerror + 1
        goto 60
      else
        level = level + 1
        iline = levelt(itop)
      endif
    endif
  end do
end subroutine hlpvms
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
