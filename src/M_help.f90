module M_help
use,intrinsic :: iso_fortran_env, only : stderr=>error_unit, stdin=>input_unit, stdout=>output_unit
use M_journal, only : journal
implicit none
private
public help_command
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    help_command(3f) - [M_help] uses a specially formatted text array to
!!    provide a HELP interface
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    Syntax:
!!
!!      function help_command(text_array,topic,position)
!!
!!##DESCRIPTION
!!    This routine, when given a CHARACTER array displays the text
!!    interactively. The special topics "manual","topics", and "search"
!!    are reserved. "manual" causes the entire array to be displayed.
!!    "topics" displays all lines not beginning with a space or three or
!!    more equal signs, and "search" must be followed by a string to search
!!    for in the manual.
!!
!!    A line beginning with a non-blank character in column one is a topic
!!
!!    A topic with the preceding line beginning with "===" is a special
!!    topic that will be displayed up to the next line beginning with "==="
!!
!!    The special topic "manual" displays the entire help text
!!
!!    The help text is paged based on the values in the position() array. The
!!    first value is the current line count on the current page, and the
!!    second value is how many lines should be displayed as a page before
!!    a paging prompt is produced. POSITION(2) is typically set to 23.
!!    POSITION(1) can be set to zero, especially if the calling page is
!!    not tracking paging itself.
!!
!!    Entering a "q" at the prompt exits the help text. To see other options
!!    enter an "h" at the prompt.
!!
!!       h
!!       #----------------------------------------------------# PAGING
!!       | f b        forward or backward one page            |
!!       | u d        redraw up or down one-half page         |
!!       | r          refresh page                            |
!!       | e y | j k  refresh page moving up or down one line |
!!       #----------------------------------------------------# JUMPING
!!       | g          go to top of manual                     |
!!       | NNN        go to line number NNN. Use a sign (+-)  |
!!       |            for a relative move.                    |
!!       | .          toggle line numbering                   |
!!       #----------------------------------------------------# SEARCHING
!!       | /STRING    advance to next line containing string  |
!!       | ?STRING    search for string above current line    |
!!       | n N        find next occurrence up or down in file |
!!       | \STRING    show all lines with specified string.   |
!!       | t          displays topic lines.                   |
!!       #----------------------------------------------------#
!!       | w FILENAME write entire user guide to local file   |
!!       | h          display this help                       |
!!       | q          quit                                    |
!!       #----------------------------------------------------#
!!       A blank repeats last positioning command. Anything else is ignored.
!!       Line count is 25 out of 54 . Page size is 23 (see "lines")
!!       help:
!!
!!
!!    A normal topic is displayed until another topic line (line beginning
!!    with a non-blank) is encountered
!!
!!    The help text must begin with a line starting with "==="
!!
!!    If a line is preceded by an "===" line it is considered a section
!!    instead of a topic, and all lines between that line and the next line
!!    beginning with "===" are displayed.
!!##OPTIONS
!!    help_text  The block of text to treat as the input document
!!
!!    topic      What topic or section to search for (case sensitive). A blank
!!               is equivalent to "SUMMARY". There are several reserved names.
!!               "manual" means the entire help text, and "topics" shows only
!!               lines not beginning with a blank, and "search" does a
!!               case-insensitive search for a string.
!!
!!    position   A small array with two values. The second value is the size
!!               of the page to be used between pauses. The first one indicates
!!               how many lines on the current page have been displayed.
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_help_command
!!    use M_help, only : help_command
!!    character(len=:),allocatable :: help_text(:)
!!    integer                      :: position(2)
!!    position=[0,23]
!!    help_text=[character(len=80) :: &
!!    '==============================================',&
!!    '   A sample help text file.                   ',&
!!    '   Note the first line MUST start with "==="  ',&
!!    '==============================================',&
!!    'SUMMARY                                       ',&
!!    '  This is usually a crib sheet                ',&
!!    '==============================================',&
!!    'SECTION1                                      ',&
!!    'topic1                                        ',&
!!    '   A description of topic 1                   ',&
!!    '                                              ',&
!!    '   and any general text you want              ',&
!!    '                                              ',&
!!    'topic2  A description of topic 2              ',&
!!    'topic3                                        ',&
!!    '   A description of topic 3                   ',&
!!    '   more  description of topic 3               ',&
!!    '   and more description of topic 3 a          ',&
!!    '   and more description of topic 3 b          ',&
!!    '   and more description of topic 3 c          ',&
!!    '   and more description of topic 3 d          ',&
!!    '   and more description of topic 3 e          ',&
!!    '   and more description of topic 3 f          ',&
!!    '   and more description of topic 3 g          ',&
!!    '   and more description of topic 3 h          ',&
!!    '   and more description of topic 3 i          ',&
!!    '   and more description of topic 3 j          ',&
!!    '   and more description of topic 3 k          ',&
!!    '   and more description of topic 3 l          ',&
!!    '   and more description of topic 3 m          ',&
!!    '   and more description of topic 3 n          ',&
!!    '   and more description of topic 3 o          ',&
!!    '   and more description of topic 3 p          ',&
!!    '   and more description of topic 3 q          ',&
!!    '   and more description of topic 3 r          ',&
!!    '   and more description of topic 3 s          ',&
!!    '   and more description of topic 3 t          ',&
!!    '   and more description of topic 3 u          ',&
!!    '   and more description of topic 3 v          ',&
!!    '   and more description of topic 3 w          ',&
!!    '   and more description of topic 3 x          ',&
!!    '   and more description of topic 3 y          ',&
!!    '   and more description of topic 3 z          ',&
!!    '==============================================',&
!!    'SECTION2                                      ',&
!!    'topic4  A description of topic 4              ',&
!!    '   this is the last part of SECTION1          ',&
!!    'topic5                                        ',&
!!    '  This is all about the fifth topic and is    ',&
!!    '  just displayed as-is. The text cannot start ',&
!!    '  in column one or it will be seen as the     ',&
!!    '  beginning of a topic.                       ',&
!!    '==============================================',&
!!    '                                              ' ]
!!
!!    write(*,*)'>>>>>'
!!    call help_command(help_text,'',position)
!!    write(*,*)'>>>>>topic1'
!!    call help_command(help_text,'topic1',position)
!!    write(*,*)'>>>>>topics'
!!    call help_command(help_text,'topics',position)
!!    write(*,*)'>>>>>manual'
!!    call help_command(help_text,'manual',position)
!!    end program demo_help_command
subroutine help_command(help_text,topic_name,position)

! ident_1="@(#) M_help help_command(3f) interactively display help text"

character(len=*),intent(in)            :: help_text(:)
character(len=*),intent(in)            :: topic_name
integer                                :: position(2)
integer                                :: end_of_first_word
integer                                :: start_of_topic
integer                                :: ios
character(len=:),allocatable           :: topic, old_topic, string
logical                                :: block_topic
integer                                :: i, j, k, jj, ii
logical                                :: numbered
character(len=len(help_text))          :: last_response
integer                                :: toomany
integer,parameter                      :: max_toomany=2000
integer                                :: howbig
integer                                :: old_position

   howbig=size(help_text)
   toomany=1
   last_response='f'
   numbered=.false.
   topic=trim(topic_name)
   old_topic=''
   old_position=0
   if(index(topic,'search ') == 1)then
      topic='search'
   endif
   INFINITE: do

      if (topic == ' ') then                                           ! if no topic
         call journal('Type "help" followed by a case-sensitive topic name ...')
         topic='SUMMARY'
      endif
      select case(topic)
      case('manual')                                  ! show all the help text
         i=0
         do
            i=i+1
            if(i > howbig)exit
            if(help_text(i)(1:3) == '===')then
               if(numbered)then
                  call journal('sc',i,' ')
               else
                  call journal(' ')
               endif
            else
               if(numbered)then
                  call journal('sc',i,help_text(i))
               else
                  call journal('sc',help_text(i))
               endif
            endif
            if(want_to_stop())exit INFINITE
            if(old_topic /= '')cycle INFINITE
            if(i >= howbig) then
               if(position(2)>howbig)exit INFINITE
               do j=1,max_toomany
                  call journal('sc','[end-of-file] (line',i,')')
                  position(1)=position(2)+1
                  if(want_to_stop())exit INFINITE
                  if(old_topic /= '')cycle INFINITE
                  if(i < howbig)exit
               enddo
               if(i >= howbig)exit
            endif
         enddo
         exit INFINITE
      case('topics')                         ! go through all the text showing lines not starting with a a space or equal
         i=1                                 ! display topic starting at start_of_topic
         do
            i=i+1
            if(i > howbig) exit
            if(help_text(i)(1:1) == '   ')cycle
            if(help_text(i)(1:3) == '===')cycle
            jj=merge(0,3,help_text(i-1)(1:3) == '===')
            if(numbered)then
               call journal('sc',i,'>',repeat(' ',jj)//help_text(i))
            else
               call journal('sc','>',repeat(' ',jj)//help_text(i))
            endif
            if(want_to_stop())then
               if(old_topic /= '')then
                  topic=old_topic
                  old_topic=''
                  i=old_position
                  call pageback(1)
                  i=max(1,i)
                  position(1)=position(2)+1
                  cycle INFINITE
               endif
               exit INFINITE
            endif
         enddo
         if(old_topic /= '')then
            topic=old_topic
            old_topic=''
            i=old_position
            call pageback(1)
            i=max(1,i)
            position(1)=position(2)+1
            cycle INFINITE
         endif
         exit INFINITE
      case('search')                         ! go through all the text showing lines matching string
         position(1) = 0
         string=topic_name//'        '
         string=trim(lower(adjustl(string(8:))))
         i=0
         do
            i=i+1
            if(i > howbig)exit
            if(help_text(i)(1:1) /= ' '.and.help_text(i)(1:3) /= '===')then
               old_topic=help_text(i)//' '
               ii=index(old_topic,' ')
               old_topic=old_topic(:ii)
            endif
            if(index(lower(help_text(i)),string) /= 0)then
               if(numbered)then
                  call journal('sc',i,help_text(i))
               else
                  call journal('sc',old_topic,'>',help_text(i))
               endif
               if(want_to_stop())exit INFINITE
               if(i >= howbig) then
                  if(position(2)>howbig)exit INFINITE
                  do j=1,max_toomany
                     call journal('sc','[end-of-file] (line',i,')')
                     position(1)=position(2)+1
                     if(want_to_stop())exit INFINITE
                     if(i < howbig)exit
                  enddo
                  if(i >= howbig)exit
               endif
            endif
         enddo
         exit INFINITE
      case default ! find the line that starts with the topic
         start_of_topic=0
         ! find the line to start with by finding a line that starts with the given topic ( ASSUMING FIRST LINE is ===)
         FINDIT: do j=1,len(help_text)
            do i=2, howbig                                          ! get first word of lines not starting with a blank
               if(help_text(i)(1:1) /= ' ')then                              ! only topic lines start in column one so skip these
                  end_of_first_word=index(help_text(i),' ')-1
                  if(end_of_first_word == 0)end_of_first_word=len(help_text) ! if line is filled and does not have a blank
                  end_of_first_word=end_of_first_word-j+1
                  if(end_of_first_word <= 0)cycle
                  !x!write(*,*)'['//topic(:end_of_first_word)//']['//help_text(i)(:end_of_first_word)//']'
                  if(topic == help_text(i)(:end_of_first_word))then      ! find a line that matches topic
                     exit FINDIT
                  endif
               endif
            enddo
         enddo FINDIT
         start_of_topic=i

         if(i == 0)then
            call journal('<ERROR> internal error. First line of text must start with "==="')
            !!help_text=[character(len=len(help_text)) :: repeat("=",80),help_text]
            start_of_topic=start_of_topic+1
         endif

         if(help_text(i-1)(1:3) == '===')then  ! if the line above the start started with "===" it is a block comment
            block_topic=.true.
         else
            block_topic=.false.
         endif

         if(start_of_topic > howbig.or.start_of_topic == 0)then
            call journal('sc','SORRY, No help on ',topic)
         else
            position(1) = 0
            if(numbered)then
               call journal('sc',i,help_text(start_of_topic))                       ! show the start line
            else
               call journal('sc',help_text(start_of_topic))                       ! show the start line
            endif

            i=start_of_topic+1                                              ! display topic starting at start_of_topic
            do
               if(help_text(i)(1:1) /= ' '.and. .not.block_topic )then       ! stop at next topic if not a block of help
                  exit
               elseif(block_topic .and. help_text(i)(1:3) == '===')then
                  exit
               endif
               if(numbered)then
                  call journal('sc',i,help_text(i))
               else
                  call journal('sc',help_text(i))
               endif
               if(want_to_stop())exit INFINITE
               if(old_topic /= '')cycle INFINITE
               toomany=toomany+1
               if(toomany >= max_toomany)exit INFINITE    ! to prevent infinite loops in batch mode
               i=max(start_of_topic-1,i)
               i=i+1
               if(i > howbig) exit
            enddo
         endif
         exit INFINITE
      end select
      if(want_to_stop())exit INFINITE
   enddo INFINITE
contains

function want_to_stop()
character(len=len(help_text))        :: response
character(len=1)                     :: letter
logical                              :: want_to_stop
integer                              :: j
integer                              :: jj
doubleprecision                      :: val
integer                              :: ierr
   position(1) = position(1) + 1
   want_to_stop=.false.
   PROMPT: do
      if(position(1)  >  position(2)) then
         call journal('+sc','help:')
         read(stdin,'(a)',iostat=ios) response                ! read letter to pause from standard input
         if(response.eq.' ')response=last_response
         response=adjustl(response)
         letter=response(1:1)
         select case(letter)
         case('f')                                            ! next page
            position(1) = 0                                   ! start new page
            last_response='f'
         case('b')                                            ! back one page
            call pageback(2)
            position(1) = 0
            last_response='b'
         case('0':'9')                                        ! assumed to be a number
            call a2d(response,val,ierr)
            i=nint(val)-1
            i=max(i,1)
            i=min(i,howbig-1)
            position(1) = 0
            last_response=last_response
         case('-','+')                                        ! assumed to be a number
            call pageback(1)
            call a2d(response,val,ierr)
            i=i+nint(val)
            i=max(i,1)
            i=min(i,howbig-1)
            position(1) = 0
            last_response=last_response
         case('t')                                            ! topics
            old_topic=topic
            old_position=i
            topic='topics'
            position(1)=0
            exit PROMPT
            !do j=2,howbig
            !   if(help_text(j)(1:1) == '   ')cycle
            !   if(help_text(j)(1:3) == '===')cycle
            !   jj=merge(0,3,help_text(j-1)(1:3) == '===')
            !   if(numbered)then
            !      call journal('sc',j,'>',repeat(' ',jj)//help_text(j))
            !   else
            !      call journal('sc','>',repeat(' ',jj)//help_text(j))
            !   endif
            !enddo
            !call pageback(1)
            !i=max(1,i)
            !position(1)=position(2)+1
            !cycle PROMPT
         case('u')                                            ! back one-half page
            call pageback(1)
            i=max(1,i-position(2)/2-1)
            position(1) = 0
            last_response='u'
         case('e','k')                                        ! back one line page
            call pageback(1)
            i=max(1,i-1)
            position(1) = 0
            last_response='e'
         case('y','j')                                        ! down one line page
            call pageback(1)
            i=max(1,i+1)
            position(1) = 0
            last_response='y'
         case('w')
            WRITEFILE: block
            character(len=1000) :: errmsg
            integer :: temp_lun
               response=adjustl(response(2:))
               if(response == '')response='userguide.txt'
               open(newunit=temp_lun,file=response,status='new',iostat=ios,iomsg=errmsg) ! open help file
               if(ios == 0)then
                  write(temp_lun,'(a)',iostat=ios)( trim(help_text(k)),k=1,howbig )
                  call journal('sc','<INFO> user guide is on file',response )
                  close(unit=temp_lun,iostat=ios)
               else
                  call journal(errmsg)
               endif
            endblock WRITEFILE
            i=max(1,i-1)
            last_response=last_response
         case('d')                                            ! down one-half page
            i=min(howbig-1,i-position(2)/2-1)
            position(1) = 0
            last_response='d'
         case('r')                                            ! repaint page
            call pageback(1)
            position(1) = 0
            last_response=last_response
         case('/','n')                                        ! find string below
            j=i ! hold
            if(letter == 'n')response=last_response
            if(response(2:) == '')response=last_response
            i=i+1
            do
               if(index(lower(help_text(i)),trim(response(2:))) /= 0)then
                  i=max(1,i-1)

                  exit
               else
                  i=i+1
               endif
               if(i > howbig) exit
            enddo
            if(i > howbig)i=j
            position(1) = 0
            last_response=response
         case('\') ! find string
            response=lower(adjustl(response(2:)))
            if(response == ' ')response=last_response
            jj=len_trim(response)
            do j=1,howbig
               if(index(lower(help_text(j)),response(:jj)) /= 0)then
                  call journal('sc',j,help_text(j))
               endif
            enddo
            i=i-1
            call pageback(1)
            last_response='/'//response
         case('?','N','p')                                            ! find string above
            j=i ! hold
            if(letter == 'N'.or.letter == ' ')response=last_response
            if(response(2:) == '')response=last_response
            i=i-1
            do
               if(index(lower(help_text(i)),trim(response(2:))) /= 0)then
                  exit
               else
                  i=i-1
               endif
               if(i <= 1) then
                  i=j
                  exit
               endif
            enddo
            call pageback(1)
            position(1) = 0
            last_response=response
         case('g')                                            ! repaint page
            i=1
            position(1) = 0
            last_response=last_response
         case('.')                                            ! help
            position(1) = 0
            numbered=.not.numbered
            last_response=last_response
         case('h')                                            ! help
            call journal('sc','#----------------------------------------------------# PAGING')
            call journal('sc','| f|SPACE b  forward or backward one page            |')
            call journal('sc','| u d        redraw up or down one-half page         |')
            call journal('sc','| r          refresh page                            |')
            call journal('sc','| e y | j k  refresh page moving up or down one line |')
            call journal('sc','#----------------------------------------------------# JUMPING')
            call journal('sc','| g          go to top of manual                     |')
            call journal('sc','| NNN        go to line number NNN. Use a sign (+-)  |')
            call journal('sc','|            for a relative move.                    |')
            call journal('sc','| .          toggle line numbering                   |')
            call journal('sc','#----------------------------------------------------# SEARCHING')
            call journal('sc','| /STRING    advance to next line containing string  |')
            call journal('sc','| ?STRING    search for string above current line    |')
            call journal('sc','| n N        find next occurrence up or down in file |')
            call journal('sc','| \STRING    show all lines with specified string.   |')
            call journal('sc','| t          displays topic lines.                   |')
            call journal('sc','#----------------------------------------------------#')
            call journal('sc','| w FILENAME write entire user guide to local file   |')
            call journal('sc','| h          display this help                       |')
            call journal('sc','| q          quit                                    |')
            call journal('sc','#----------------------------------------------------#')
            call journal('sc','A blank repeats last positioning command. Anything else is ignored.')
            call journal('sc','Line count is ',i,'out of',howbig,'. Page size is',position(2),'(see "lines")')
            last_response=last_response
            cycle
         case('q')
            position(1) = -1
            want_to_stop=.true.
         case default
            call pageback(2)
            call journal('sc','unknown option -- enter "h" for assistance or "q" to quit')
         end select
      endif
      exit
   enddo PROMPT
end function want_to_stop

subroutine pageback(loops)
integer,intent(in) :: loops
integer            :: j
   do j=1,loops
      i=max(1,i-position(2)-1)
   enddo
end subroutine pageback

end subroutine help_command

subroutine a2d(chars,valu,ierr)

! ident_2="@(#) M_strings a2d(3fp) subroutine returns double value from string"

!     1989,2016 John S. Urban.
!
!  o works with any g-format input, including integer, real, and exponential.
!  o if an error occurs in the read, iostat is returned in ierr and value is set to zero. If no error occurs, ierr=0.
!  o if the string happens to be 'eod' no error message is produced so this string may be used to act as an end-of-data.
!    IERR will still be non-zero in this case.

character(len=*),intent(in)  :: chars                        ! input string
character(len=:),allocatable :: local_chars
doubleprecision,intent(out)  :: valu                         ! value read from input string
integer,intent(out)          :: ierr                         ! error flag (0 == no error)

character(len=*),parameter   :: fmt="('(bn,g',i5,'.0)')"     ! format used to build frmt
character(len=15)            :: frmt                         ! holds format built to read input string
character(len=256)           :: msg                          ! hold message from I/O errors
character(len=3),save        :: nan_string='NaN'

   ierr=0                                                    ! initialize error flag to zero
   local_chars=chars
   msg=''
   if(len(local_chars) == 0)local_chars=' '
   write(frmt,fmt)len(local_chars)                        ! build format of form '(BN,Gn.0)'
   read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu   ! try to read value from string
   if(ierr /= 0)then                                         ! if an error occurred ierr will be non-zero.
      read(nan_string,'(g3.3)')valu
      call journal('sc','*a2d* - cannot produce number from string [',chars,']')
      if(msg /= '')then
         call journal('sc','*a2d* - [',msg,']')
      endif
   endif
end subroutine a2d

elemental pure function lower(str,begin,end) result (string)

! ident_3="@(#) M_strings lower(3f) Changes a string to lowercase over specified range"

character(*), intent(in)     :: str
character(len(str))          :: string
integer,intent(in),optional  :: begin, end
integer                      :: i
integer                      :: ibegin, iend
integer,parameter             :: diff = iachar('A')-iachar('a')
   string = str
   ibegin=1
   iend=len_trim(str)

   if (present(begin))then
      ibegin = min(max(1,begin),iend)
   endif

   if (present(end))then
      iend= max(1,min(iend,end))
   endif

   do concurrent (i = ibegin:iend)                   ! step thru each letter in the string in specified range
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = char(iachar(str(i:i))-diff)   ! change letter to miniscule
      case default
      end select
   enddo

end function lower

end module M_help
