!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
program topic
   use M_journal, only: journal
   use M_kracken,   only: kracken, iget, sget, lget
   implicit none
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('help','     &
      & -oo                  &
      & -t                   &
      & -e                   &
      & -f                   &
      & --help .f.           &
      & --version .f.        &
      & -topics .false.      &
      & -all .false.         &
      & -summaries .false.   &
      &') ! crack command line
!-----------------------------------------------------------------------------------------------------------------------------------
   call help_usage(lget('help_help'))                                ! if -help option is present, display help text and exit
   call help_version(lget('help_version'))                           ! if -version option is present, display version text and exit
!-----------------------------------------------------------------------------------------------------------------------------------
   call helpg(sget('help_oo'),sget('help_t'),sget('help_f'),sget('help_e'))
   contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine helpg(topic0,search0,helpdat0,expr0)
use M_regex,   only : regex_type, regcomp, regexec, regmatch, regfree
use M_strings,only : upper
implicit none

character(len=*),parameter::ident_1="@(#)helpg(3f): a simple help utility"

character(len=*),intent(in)   :: topic0      ! topic name to location
character(len=*),intent(in)   :: search0     ! search body of text for this string
character(len=*),intent(in)   :: helpdat0    ! name of file containing help text. If blank use environment variable
                                                ! or "help.txt"
character(len=*),intent(in)   :: expr0       ! search body of string for this regular expression

character(len=250)            ::  s,topicx,topic,search

character(len=79)             :: columns
character(len=255)            :: helpdat=' '

integer,parameter             :: IIN=1     ! unit number to use for reading
integer,parameter             :: IMAX=240  ! it is time. Even 4010 emulators seem to support at least 80; Tektronix 4010 was 74
integer                       :: i300
integer                       :: icol
integer                       :: istart
integer                       :: iend
integer                       :: ios
integer                       :: ilen
integer                       :: ishowed
integer                       :: iwide
integer                       :: j
integer                       :: status=0
logical                       :: match=.false.
integer                       :: nmatch
integer,allocatable           :: matches(:,:)
type(regex_type)              :: regex
character(len=:),allocatable  :: options
!-----------------------------------------------------------------------------------------------------------------------------------
   topic=topic0(:min(len(topic),len(topic0)))                  ! topic can be changed by this procedure, so make copy
   search=search0(:min(len(search),len(search0)))              ! search can be changed by this procedure, so make copy
   helpdat=helpdat0(:min(len(helpdat),len(helpdat0)))          ! helpdat can be changed by this procedure, so make copy
!-----------------------------------------------------------------------------------------------------------------------------------
   if(expr0.ne.'')then
      write(*,*)'EXPRESSION A=',trim(expr0)
      options='in'
      call regcomp(regex,   & ! new regex object
      & expr0,              & ! regex pattern string
      & options,            & ! flag characters:
                              !   x = extended regex (REG_EXTENDED)
                              !   m = multi-line     (REG_NEWLINE)
                              !   i = case-insensitive (REG_ICASE)
                              !   n = no MATCH required (REG_NOSUB)
      & nmatch,             & ! number of subexpressions in regular expression
      & status)               ! If absent, errors are fatal
      if(status.ne.0)then                        ! Leave program if regex is faulty.  we could use regerror to decode the error ...
         write(*,*) "*regcomp* ERROR. status=", status
      endif
      allocate(matches(2,nmatch))
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   iend=len_trim(helpdat)                                      ! find last non-blank character of filename
   if(iend.eq.0)then                                           ! if blank name, look in environment variable or use default name
      call get_environment_variable('USHHELP',helpdat)         ! set default name for helpfile
      if(helpdat.eq.' ')then
         helpdat='/usr/share/ush/help.txt'                     ! if default name if everything else resulted in blanks
      endif
      iend=len_trim(helpdat)                                   ! get length of filename
   endif
   close(IIN)
   open(unit=IIN,status='old',file=helpdat(:iend),iostat=ios)
   if(ios.ne.0)then
      call journal('sc','*help* unexpected i/o problems opening the help file')
      call journal('sc',helpdat)
      goto 999
   endif
   rewind IIN
!-----------------------------------------------------------------------------------------------------------------------------------
   if(topic.eq.' '.and.search.eq.' ')then
      topic='novice'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(search.ne.' ')then                                       ! show topics that have a certain string in them
      ilen=len_trim(search)
      search=upper(search)
      call journal('s','TOPIC')
      topicx=' '
      do
         read(IIN,'(a)',end=999,err=888)s
         if(s(1:6).eq.'TOPIC:'.and.s(7:9).ne.'...')topicx=s    ! hold  the topic line
         j=index(upper(s),search(:ilen))
         if(j.ne.0)then
            s(j:j+ilen-1)='#########################################'
            if(topicx.ne.' ')call journal('s',trim(topicx))
            call journal('s','      '//trim(s))
            topicx=' '
         endif
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   elseif(expr0.ne.' ')then                                       ! show topics that have a certain string in them
      write(*,*)'EXPRESSION=',trim(expr0)
      call journal('s','TOPIC')
      topicx=' '
      do
         read(IIN,'(a)',end=999,err=888)s
         if(s(1:6).eq.'TOPIC:'.and.s(7:9).ne.'...')topicx=s    ! hold  the topic line
         match=regexec(regex,'('//trim(s)//')',matches)
         if(match)then
            if(topicx.ne.' ')call journal('s',trim(topicx))
            call journal('s','      '//trim(s))
            topicx=' '
         endif
      enddo
      call regfree(regex)
   else
!-----------------------------------------------------------------------------------------------------------------------------------
      select case (topic)
!-----------------------------------------------------------------------------------------------------------------------------------
      case ('???')                                             ! dump all of file
         do
            read(IIN,'(a)',end=999,err=888)s
            ilen=len_trim(s)
            ilen=min(ilen,IMAX)
            call journal('s',s(:ilen))
         enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      case ('??')
         call journal('s','Available topics include:')
            READLINE: do
               read(IIN,'(a)',end=999,err=888)s                   ! look for topic line
               NEXT: do
                  if(s(1:6).eq.'TOPIC:')then
                     call journal('s','=========================================================================')
                     call journal('s',s(7:len_trim(s)))
                  else                                            ! show next paragraph after topic line
                     cycle READLINE
                  endif
                  do
                     read(IIN,'(a)',end=999,err=888)s             ! skip blank lines if any after TOPIC lines
                     if(s(1:6).eq.'TOPIC:')cycle NEXT             ! another TOPIC: line right after the one that began this block
                     if(s.eq.' ')then
                     elseif(s(1:4).eq.'====')then
                     else
                        exit
                     endif
                  enddo
                  do
                     ilen=len_trim(s)
                     ilen=min(ilen,IMAX)
                     call journal('s','-'//s(1:ilen))                   ! echo lines from detailed paragraph
                     read(IIN,'(a)',end=999,err=888)s             ! read and echo detailed description
                     if(s(1:6).eq.'TOPIC:')cycle NEXT             ! another TOPIC: line right after the one that began this block
                     if(s.eq.' ')then                             ! end of detailed paragraph, go get next TOPIC:
                        cycle READLINE
                     endif
                  enddo
                  exit NEXT
               enddo NEXT
            enddo READLINE
!-----------------------------------------------------------------------------------------------------------------------------------
      case ('?')                                                      ! dump all topic lines
         iwide=24
         OUTER: do
            columns='|'
            columns(79:79)='|'
            icol=3
            L300: do i300=1,3                                         ! look for topics in sets of up to three
               L350: do
                  read(IIN,'(a)',iostat=ios)s                         ! read a line
                  if(is_iostat_end(ios))then                          ! if hit end (or error) exit
                     if(columns(2:78).ne.' ') call journal('s',columns)
                     call journal('s','*-----------------------------------------------------------------------------*')
                     goto 999
                  endif
                  if(ios.ne.0) goto 888                               ! some other error
                  if(s(1:6).eq.'TOPIC:')then                          ! found a topic line
                     if(upper(s).eq.s)then                            ! start a new line if topic is all capitals
                        if(columns(2:78).ne.' ') call journal('s',columns)
                        columns='| '//s(7:7+79-1)
                        columns(79:79)='|'
                        call journal('s','*-----------------------------------------------------------------------------*')
                        call journal('s',columns)
                        cycle OUTER
                     endif
                     columns(icol:icol+iwide)=s(7:7+iwide)            ! a TOPIC: line that is not all uppercase letters
                     icol=icol+iwide
                     columns(icol:icol)='|'
                     icol=icol+2
                     cycle L300                                       ! see about adding another topic
                  endif
               enddo L350
            enddo L300
            if(columns(2:78).ne.' ') call journal('s',columns)
         enddo OUTER
!-----------------------------------------------------------------------------------------------------------------------------------
      case default                                             ! display help text for a topic
         ilen=len_trim(topic)
         do
            read(IIN,'(a)',iostat=ios)s
            if(ios.ne.0)then                                            ! reached end of file or error on reading
               write(s,'(3a)')'*help* no entry for /',topic(:ilen),'/'
               ilen=len_trim(s)
               ilen=min(ilen,IMAX)
               call journal('s',s(:ilen))
               call journal('s','*help* maybe try help -t TOPIC?')
               goto 999
            endif
            if(s(7:ilen+6).eq.topic(:ilen).and.s(1:6).eq.'TOPIC:')then  ! found the topic line you were searching for
               call journal('s',trim(s(7:)))                  ! echo topic without TOPIC: field
               ishowed=0                                ! number of lines showed (if 0, its an adjacent ? line, so show it)
               do
                  read(IIN,'(a)',end=999,err=888)s      ! read a potential description, end of description, or continued topic line
                  if(s(1:6).eq.'TOPIC:'.and.ishowed.ne.0)then  ! end of description
                     goto 999
                  elseif(s(1:6).ne.'TOPIC:')then               ! regular line of text
                     ishowed=ishowed+1
                     istart=1
                  else
                     istart=7
                  endif
                  iend=istart+IMAX-1
                  iend=min(iend,IMAX)
                  iend=len_trim(s(:iend))
                  call journal('s',s(istart:iend))
               enddo
            endif
         enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      end select
!-----------------------------------------------------------------------------------------------------------------------------------
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call journal('sc','*help* ended without finding end')
   goto 999
 888  continue
   call journal('sc','*help* unexpected i/o problems on the help file')
   call journal('sc',helpdat)
   goto 999
!-----------------------------------------------------------------------------------------------------------------------------------
 999  continue
   close(IIN)
!-----------------------------------------------------------------------------------------------------------------------------------
   end subroutine helpg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
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
'     topic(1) - [HELP] Display specially formatted help text files.             ',&
'SYNOPSIS                                                                        ',&
'     topic [TOPIC | -t| -e SEARCH_STRING] [ -f INPUT_FILE]]|[ -all| -topics| -summaries]',&
'     (LICENSE:PD)                                                               ',&
'DESCRIPTION                                                                     ',&
'   This utility program is used to read topics from a specially formatted       ',&
'   help text file. It is often called from a program as a subprocess.           ',&
'                                                                                ',&
'   start at the beginning of the description file and read until the            ',&
'   desired topic is reached; then display help until another topic begins       ',&
'                                                                                ',&
'   format of file is                                                            ',&
'                                                                                ',&
'      TOPIC:topic                                                               ',&
'      TOPIC:other optional topic line(s)                                        ',&
'      syntax line(s) (assumed not to exist if first letter of topic is uppercase)',&
'                                                                                ',&
'      detailed topic description for commands displayed as-is until a new TOPIC:',&
'      line is encountered.                                                      ',&
'                                                                                ',&
'      and then repeat starting with TOPIC: line                                 ',&
'         :                                                                      ',&
'         :                                                                      ',&
'         :                                                                      ',&
'                                                                                ',&
'   EXAMPLE FILE:                                                                ',&
'                                                                                ',&
'      TOPIC:COLORS                                                              ',&
'                                                                                ',&
'      The color-related commands background(1) and foreground(1) are            ',&
'      used to define plot page colors                                           ',&
'                                                                                ',&
'      TOPIC: background                                                         ',&
'      background -name color                                                    ',&
'                                                                                ',&
'         This command sets the background color                                 ',&
'                                                                                ',&
'      TOPIC: foreground                                                         ',&
'      foreground -name color                                                    ',&
'                                                                                ',&
'         This command sets the foreground color                                 ',&
'                                                                                ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
' TOPIC  The name of the topic to get help for                                   ',&
'                                                                                ',&
'        * if topic is ? show all topic lines                                    ',&
'          topics all in caps go on new line                                     ',&
'          display other topics three to a line in a table, like:                ',&
'                                                                                ',&
'           #--------------------------------------------------------------------#',&
'           | INTRO                                                              |',&
'           #--------------------------------------------------------------------#',&
'           | STARTING UP USH                                                    |',&
'           | SUMMARY of basic USH |                                             |',&
'           #--------------------------------------------------------------------#',&
'           | COMMANDS                                                           |',&
'           | novice               | aspect               | attach               |',&
'           | dirf                 | exact                | f                    |',&
'           #--------------------------------------------------------------------#',&
'           | CALCULATOR                                                         |',&
'           | ANSI functions       | String functions     | Bessel functions     |',&
'           | Operators            | intg()               | dif()                |',&
'           | convert()            |                                             |',&
'           #--------------------------------------------------------------------#',&
'           | CHANGE REQUEST                                                     |',&
'           | version2 changes     | version3 changes     | version3.02 changes  |',&
'           | version4 changes     | V4 Obsolete Usage    | version5 changes     |',&
'           |                      |                                             |',&
'           #--------------------------------------------------------------------#',&
'                                                                                ',&
'        * if topic is ?? show all topic lines and syntax lines                  ',&
'        * if topic is ??? show all lines                                        ',&
'                                                                                ',&
' -all               list entire help file                                       ',&
' -topics            list topics found in help file                              ',&
' -summaries         list summaries for each topic                               ',&
' -t SEARCH_STRING   find topics that contain the specified string.              ',&
' -e REGULAR_EXPRESSION   find topics that contain the specified                 ',&
'                         regular expression                                     ',&
' -f INPUT_FILE   Set the filename to read descriptions from. The                ',&
'                 default input file is the value of the environment             ',&
'                 variable $USHHELP if it is set. The built-in                   ',&
'                 default is /usr/share/ush/help.txt                             ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'   topic -topics | more     # show all topic names                              ',&
'   topic -summaries | more  # Show all topics and short abstracts               ',&
'   topic -all | more        # display entire help file                          ',&
'                                                                                ',&
'DEPENDENCIES                                                                    ',&
'  * M_strings(3f)                                                               ',&
'  * M_regexp(3f)                                                                ',&
'  * M_kracken(3f)                                                               ',&
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
!!      topic(1) - [HELP] Display specially formatted help text files.
!!##SYNOPSIS
!!
!!      topic [TOPIC | -t| -e SEARCH_STRING] [ -f INPUT_FILE]]|[ -all| -topics| -summaries]
!!      (LICENSE:PD)
!!##DESCRIPTION
!!    This utility program is used to read topics from a specially formatted
!!    help text file. It is often called from a program as a subprocess.
!!
!!    start at the beginning of the description file and read until the
!!    desired topic is reached; then display help until another topic begins
!!
!!    format of file is
!!
!!       TOPIC:topic
!!       TOPIC:other optional topic line(s)
!!       syntax line(s) (assumed not to exist if first letter of topic is uppercase)
!!
!!       detailed topic description for commands displayed as-is until a new TOPIC:
!!       line is encountered.
!!
!!       and then repeat starting with TOPIC: line
!!          :
!!          :
!!          :
!!
!!    EXAMPLE FILE:
!!
!!       TOPIC:COLORS
!!
!!       The color-related commands background(1) and foreground(1) are
!!       used to define plot page colors
!!
!!       TOPIC: background
!!       background -name color
!!
!!          This command sets the background color
!!
!!       TOPIC: foreground
!!       foreground -name color
!!
!!          This command sets the foreground color
!!
!!
!!##OPTIONS
!!  TOPIC  The name of the topic to get help for
!!
!!         * if topic is ? show all topic lines
!!           topics all in caps go on new line
!!           display other topics three to a line in a table, like:
!!
!!            #--------------------------------------------------------------------#
!!            | INTRO                                                              |
!!            #--------------------------------------------------------------------#
!!            | STARTING UP USH                                                    |
!!            | SUMMARY of basic USH |                                             |
!!            #--------------------------------------------------------------------#
!!            | COMMANDS                                                           |
!!            | novice               | aspect               | attach               |
!!            | dirf                 | exact                | f                    |
!!            #--------------------------------------------------------------------#
!!            | CALCULATOR                                                         |
!!            | ANSI functions       | String functions     | Bessel functions     |
!!            | Operators            | intg()               | dif()                |
!!            | convert()            |                                             |
!!            #--------------------------------------------------------------------#
!!            | CHANGE REQUEST                                                     |
!!            | version2 changes     | version3 changes     | version3.02 changes  |
!!            | version4 changes     | V4 Obsolete Usage    | version5 changes     |
!!            |                      |                                             |
!!            #--------------------------------------------------------------------#
!!
!!         * if topic is ?? show all topic lines and syntax lines
!!         * if topic is ??? show all lines
!!
!!  -all               list entire help file
!!  -topics            list topics found in help file
!!  -summaries         list summaries for each topic
!!  -t SEARCH_STRING   find topics that contain the specified string.
!!  -e REGULAR_EXPRESSION   find topics that contain the specified
!!                          regular expression
!!  -f INPUT_FILE   Set the filename to read descriptions from. The
!!                  default input file is the value of the environment
!!                  variable $USHHELP if it is set. The built-in
!!                  default is /usr/share/ush/help.txt
!!
!!##EXAMPLES
!!
!!    topic -topics | more     # show all topic names
!!    topic -summaries | more  # Show all topics and short abstracts
!!    topic -all | more        # display entire help file
!!
!!##DEPENDENCIES
!!   * M_strings(3f)
!!   * M_regexp(3f)
!!   * M_kracken(3f)
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
'@(#)PROGRAM:        topic(1)>',&
'@(#)DESCRIPTION:    create a simple help utility that reads a simply formatted help file>',&
'@(#)VERSION:        1.0, 19841208>',&
'@(#)VERSION:        1.1, 19920525>',&
'@(#)VERSION:        2.0, 19950611  Complete rewrite so help can be generated from HTML 2.0 only>',&
'@(#)VERSION:        2.5, 20130818  Updated and made a seperate program instead of a subroutine>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2021-07-01 09:04:20 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program topic
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
