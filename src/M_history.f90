










!>
!!##NAME
!!    redo(3f) - [M_history] Fortran-based Input History Editor
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine redo(inputline,r)
!!
!!      character(len=*) :: inputline
!!      character(len=1),intent(in),optional :: r
!!
!!##DESCRIPTION
!!    the redo(3f) routine lets you recall, list, save, and modify previously
!!    entered program input. Built-in help is included.
!!
!!    The redo(3f) input history editor is a simple-to-use input history
!!    editor interface modeled on the CDC NOS command REDO. It uses a
!!    line editor model that means no special escape characters or control
!!    characters are required. Typically, only a few minutes are required
!!    to master usage.
!!
!!    When using redo(3f) input lines are usually first read into a character
!!    variable and then passed to the routine. The returned string can then
!!    be parsed or read from with an internal READ(3f). So, for example,
!!    if you have an existing READ(3f) such as
!!
!!       READ(*,101) A,I,K
!!
!!    replace it with something similar to
!!
!!      USE M_HISTORY,ONLY : REDO
!!      CHARACTER(LEN=255) :: LINE ! make variable big enough to read a line
!!            :
!!            :
!!      READ(*,'(A)') LINE   ! read line into character variable
!!      CALL REDO(LINE)      ! pass line to REDO(3f). This is a no-op except
!!                           ! for storing the line into the input history
!!                           ! unless the input line is the "r" command
!!      READ(LINE,101)A,I,K  ! read from variable like you did from file
!!##OPTIONS
!!      inputline    line to record into history buffer file or to edit.
!!
!!      r            Optional character to use as command to invoke editing.
!!                   Defaults to 'r'.
!!
!!##USAGE
!!    When prompted for an input line by your program you may at any time
!!    enter "r" on a line by itself, or a line beginning with "r r_command"
!!    and you will enter the command history edit mode. Now you can recall
!!    and edit previous input or compose an input line using the editor
!!    commands.
!!
!!    By default, you will be editing the last line you entered, shifted
!!    one character to the right by an exclamation character.
!!
!!    The character you respond with in column one controls what happens next.
!!
!!    o If you enter "?" while in command edit mode, help is displayed.
!!
!!    o If the last input line is not the desired line to edit,
!!      select the line to edit by entering its line number or by
!!      using the /,l,u, and d commands (see below for details) to find the desired input line.
!!    o Next enter an editing directive (c,m) to edit the selected line. The
!!      "change" command will change all occurrences of an old string to a
!!      new string ...
!!
!!       c/old/new/
!!
!!    o or the "modify" command can be used with the special characters # &amp; and ^ ...
!!        o A # under a character will delete a character.
!!        o An "&" (ampersand) will cause the character above it to be replaced with a space.
!!        o  To insert a string enter ^string#.
!!        o Otherwise, enter a character under one in the currently displayed command and it will replace it.
!!        o hit RETURN to start another edit of the line
!!    o Once the change is executed you will be prompted for another edit
!!      directive
!!    o You will stay in edit mode until you enter a return on a
!!      blank line to feed your line to your program; or enter "." or
!!      "q" (which means cancel changes and return a blank line).
!!
!!    A detailed summary of the main edit-mode commands follows. In the
!!    descriptions, N stands for a number ...
!!
!!  LISTING COMMAND HISTORY
!!     l|p N      list from line N. -N shows N last lines
!!     L|P N      same as 'l' except no line numbers (for pasting)
!!     /string    search for simple string in all history lines
!!
!!  Note that the buffer is set to the last line displayed
!!
!!  POSITIONING TO PREVIOUS COMMANDS
!!     u N        up through buffer
!!     d N        down through buffer
!!     N          load line number
!!
!!  EDITING THE CURRENT BUFFER LINE
!!     c/oldstring/newstring/   change all occurrences of old string
!!                              to new string. Note that s
!!                              (for substitute) is a synonym for c
!!                              (for change).
!!
!!                              For the "c" directive the second character
!!                              becomes the delimiter. Traditionally one
!!                              usually uses a delimiter of / unless the
!!                              string you are editing contains /.
!!
!!     mmod_string    If the first character of your entry is m or blank,
!!              o REPLACE a string by entering a replacement character under it
!!              o LEAVE a character alone by leaving a space under it
!!              o DELETE a character by putting a # character under it
!!              o BLANK OUT a character by putting an & under it
!!              o INSERT A STRING by entering ^STRING#
!!
!!       The "modify" directive takes a little practice but this single
!!       directive accommodates positionally deleting, replacing, and
!!       inserting text. it is hardest using "modify" to put the strings
!!       "&" and "#" into your lines. to put a # or & character into a
!!       string use the 'c' command instead or ^&# or ^##.
!!
!!     ;N N N N ...  Append specified lines to current line
!!
!!  HELP
!!        h|?    display help text
!!
!!  SYSTEM COMMANDS
!!        !cmd   execute system command
!!
!!  DUMPING AND LOADING THE COMMAND HISTORY
!!
!!        w FILENAME   write entire command history to specified file
!!        r FILENAME   replace command history with file contents
!!        a FILENAME   append lines from file onto command history
!!
!!##EXAMPLE PROGRAM
!!   Sample program
!!
!!       program demo_redo
!!       use M_history, only : redo
!!       implicit none
!!       character(len=1024) ::  line
!!       integer             :: ios
!!       integer             :: cstat
!!       character(len=256)  :: sstat
!!       write(*,'(a)')                                             &
!!       & 'REDO(3f) COMMAND INPUT EDITOR',                         &
!!       & 'enter "r" or "r r_command" on the input line to go',    &
!!       & 'into history edit mode. Once in history edit mode you', &
!!       & 'may enter "?" to get some help. Enter "quit" to exit',  &
!!       & 'the program.'
!!       do
!!          write(*,'(a)',advance='no')'>->'    ! write prompt
!!          read(*,'(a)',iostat=ios) line       ! read new input line
!!          ! if "r", edit and return a line from the history editor
!!          call redo(line) ! store into history if not "r".
!!          if(line.eq.'quit')stop ! exit program if user enters "quit"
!!          ! now call user code to process new line of data
!!          ! As an example, call the system shell
!!          call execute_command_line(trim(line),cmdstat=cstat,cmdmsg=sstat)
!!       enddo
!!       end program demo_redo
!!
!!##SAMPLE USAGE
!!
!!    The example program is basically a loop that reads a command from
!!    standard input and then executes it as a subshell unless the "r"
!!    command is entered.
!!
!!    Now, we will enter an echo(1) command followed by a few other lines
!!    of input. Then we recall the echo(1) command and use a few of the
!!    features of redo(3) to change and then re-execute the command.
!!
!!       >echo This isss a Test
!!       This isss a Test
!!       >date
!!       Sun May 31 23:54:09 EDT 2009
!!       >pwd
!!       /cygdrive/c/urbanjs/MYCYGWIN/DISKA/public_html/public/CLONE/REDO
!!       >r                            ! enter edit mode
!!       00001 echo This isss a Test   ! last commands are displayed
!!       00002 date
!!       00003 pwd
!!       !pwd
!!       >1                            ! go to first line in history
!!       !echo This isss a Test
!!                    ##   t           ! delete and replace characters
!!       !echo This is a test          ! insert a string
!!                       ^new #
!!       !echo This is a new test
!!       c/test/TEST/                  ! change a substring
!!       !echo This is a new TEST
!!                          &          | replace character with spaces
!!       !echo This is a newTEST
!!                                     ! a blank line ends editing
!!       This is a newTEST
!!       >quit
!!
!!##AUTHOR
!!    John S. Urban, 1988,2009,2011,2015 (last change: Nov 2019)
!!##LICENSE
!!    Public Domain
module M_history
!
!  Acting much like a line-mode editor, the REDO(3f) procedure lets
!  you list, edit, save, and modify your interactively entered program
!  input. Built-in help and no dependence on terminal control sequences
!  makes this a simple-to-master and portable input history editor.
!
use M_journal, only : journal
use M_strings, only : change, modif, notabs, s2v, v2s, s2vs
implicit none
private

public  :: redo                    !  copy a line into history file or edit history if command is "r" and return line

private :: open_history_           !  open history file
private :: redol_                  !  edit history
private :: help_                   !  produce help text for redo(3f) usage

!  should use unused file, not just unit 1071 for history
!  add option to read in and replace history file

integer,parameter :: READLEN=1024  ! width of history file

contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine redo(inputline,r,lun)
!      if line starts with r word call redol_()
!      uses unit 1071
!       r
!       r string

! ident_1="@(#)M_history::redo(3f): open binary direct access file for keeping history"

character(len=*),intent(inout) :: inputline                ! user string
character(len=1),intent(in),optional :: r                  ! character to use to trigger editing
integer,intent(in),optional          :: lun
character(len=1)                     :: r_local            ! character to use to trigger editing
integer,save                         :: iobuf=1071         ! unit number to use for redo history buffer
integer,save                         :: iredo              ! number of lines read from standard input into redo file
logical,save                         :: lcalled=.false.    ! flag whether first time this routine called or not
character(len=READLEN)               :: onerecord
integer                              :: ioparc
integer                              :: ilast
!-----------------------------------------------------------------------------------------------------------------------------------
if(present(r))then
   r_local=r
else
   r_local='r'
endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  open history file and initialize
   if(.not.lcalled)then                                     ! open the redo buffer file
      lcalled=.true.
      iredo=0   ! number of lines in redo buffer
      call open_history_(iobuf,' ','scratch',ioparc)        ! redo buffer
      if(ioparc.ne.0)then
         call journal('sc','error creating history file')
         return
      endif
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ilast=len_trim(inputline)

   if(ilast.eq.1.and.inputline(1:1).eq.r_local)then                             ! redo command
      call redol_(inputline,iobuf,iredo,READLEN,' ',lun)
      ilast=len_trim(inputline)
   elseif(inputline(1:min(2,len(inputline))).eq.r_local//' ')then               ! redo command with a string following
      call redol_(inputline,iobuf,iredo,READLEN,inputline(3:max(3,ilast)),lun)
      ilast=len_trim(inputline)
   endif

   if(ilast.ne.0)then                                                           ! put command into redo buffer
      iredo=iredo+1
      onerecord=inputline                ! make string the correct length; ASSUMING inputline IS NOT LONGER THAN onerecord
      write(iobuf,rec=iredo)onerecord
   endif
end subroutine redo
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine open_history_(iunit,fname,sname,ierr)
implicit none
!-----------------------------------------------------------------------------------------------------------------------------------

! ident_2="@(#)M_history::open_history_(3fp): open history file for REDO(3f) procedure"

integer,intent(in)          :: iunit   ! Fortran unit to open
character(len=*),intent(in) :: fname   ! filename to open
character(len=*),intent(in) :: sname   ! flag. If "scratch" ignore FNAME and open a scratch file
integer,intent(out)         :: ierr    ! error code returned by opening file
character(len=1024)         :: msg
!-----------------------------------------------------------------------------------------------------------------------------------
  if(sname.eq.'scratch')then
     open(unit=iunit,status='scratch',form='unformatted',access='direct',recl=READLEN,iostat=ierr,iomsg=msg,action='readwrite')
  else
     open(unit=iunit,file=trim(fname),status=trim(sname),form='unformatted',access='direct', &
     & recl=READLEN,iostat=ierr,iomsg=msg,action='readwrite')
  endif
!-----------------------------------------------------------------------------------------------------------------------------------
  if(ierr.ne.0)then
     call journal('sc','*open_history_* open error ',ierr,'=',msg)
  endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine open_history_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine redol_(redoline,iobuf,iredo,ibuf0,init,lun)
!
!  to do:
!  might want to support a count on change to do the Nth to the Mth occurrence
!  support edit window in change
!  prompt to verify each change made with change()
!  maybe make .NAME stick into variable $NAME in the calculator
!  allow changing the edit characters in a modify

! ident_3="@(#)M_history::redoline(3fp): redo a previous input line"

character(len=*),intent(out)   :: redoline    ! edited command line to be returned
integer,intent(in)             :: iobuf       ! history file unit to read old commands from
integer                        :: iredo       !iredo ......  (i) number of lines in history file
character(len=*),intent(in)    :: init        ! initial command string
integer,intent(in)             :: ibuf0       ! the width of the history file in characters; <= len(redoline)
integer,intent(in),optional    :: lun         ! LUN to read history commands from

doubleprecision                :: val8
integer                        :: i10, i15, i20, i30
integer                        :: iounit
integer                        :: idump
integer                        :: idown
integer                        :: lun_local
integer                        :: ipoint
integer                        :: iread
integer                        :: istart
integer                        :: ios
integer                        :: ii
integer                        :: ilen
integer                        :: icall
integer                        :: iup
integer                        :: ix
integer                        :: ibuf
integer                        :: ilast
integer                        :: cstat
character                      :: cmd
character(:),allocatable       :: cmdline
character(len=len(redoline)+1) :: cin, cinbuf ! 1 greater than length of redoline
character(len=1024),save       :: numbers
character(len=1024),save       :: msg
integer,allocatable            :: ivals(:)
integer                        :: iend
integer                        :: i
integer                        :: ierr
integer                        :: ier
logical,save                   :: ddd=.false.
data numbers/'123456789012345678901234567890123456789012345678901234567890&
   &12345678901234567890123456789012345678901234567890123456789012345678901234&
   &56789012345678901234567890123456789012345678901234567890123456789012345678&
   &90123456789012345678901234567890123456789012345678901234567890123456789012&
   &34567890123456789012345678901234567890123456789012345678901234567890123456&
   &78901234567890123456789012345678901234567890123456789012345678901234567890&
   &12345678901234567890123456789012345678901234567890123456789012345678901234&
   &56789012345678901234567890123456789012345678901234567890123456789012345678&
   &90123456789012345678901234567890123456789012345678901234567890123456789012&
   &34567890123456789012345678901234567890123456789012345678901234567890123456&
   &78901234567890123456789012345678901234567890123456789012345678901234567890&
   &'/
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(lun))then
      lun_local=lun
   else
      lun_local=5
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   ipoint=iredo                                          ! initial line in history file to start with
   icall=0                                               ! flag if have been thru loop or just got here
   cin=init                                              ! initialize the directive
   ibuf=min(ibuf0,len(redoline))
   if(ibuf.le.0)return
!-----------------------------------------------------------------------------------------------------------------------------------
1  continue
   if(ipoint.le.0)then                                   ! if no lines in redo history file
      redoline=' '                                       ! make command to 'redo' a blank line since no commands entered
   else
      read(iobuf,rec=ipoint,err=999)redoline(1:ibuf)     ! get last line in history file as line to redo
      ! WARNING: OSF1 DIGITAL Fortran 77 Driver V5.2-10 DIGITAL Fortran 77 V5.2-171-428BH
      ! after this read the following storage was corrupted; switched declaration of
      ! init and redoline and problem cleared but it is probably corrupting cin and
      ! doesn't show because of logic.
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   READLINE: do                                             ! display buffer and decide on command on first call or read command
      ilen=max(1,len_trim(redoline(1:ibuf)))                ! find length of command to redo
      write(*,'(a,a)')'!',redoline(:ilen)                   ! show old command
      if(icall.ne.0)then                                    ! if not first call read the directive
         read(lun_local,'(a)',iostat=ios)cinbuf
         if(ios.ne.0)then                                   ! if there was an I/O error reread line
            exit READLINE
         endif
         call notabs(cinbuf,cin,ilast)
      elseif(cin.eq.' ')then                                ! first call and no initial command passed in
         cin='l -5'                                         ! on first call do this default command if init is blank
         ilast=4
      else                                                  ! if initial command was not blank do it instead of default
         ilast=len_trim(cin)
      endif
      icall=icall+1
!-----------------------------------------------------------------------------------------------------------------------------------
      if(ilast.eq.0)then                                                 ! blank command line; return and execute
         return
      endif
!-----------------------------------------------------------------------------------------------------------------------------------
      cmd=cin(1:1)
      if(ddd)call journal('d','*redol* cmd=',cmd,'options=',cin)
      select case(cmd)                                                   ! first character defines edit action
!-----------------------------------------------------------------------------------------------------------------------------------
       case(' ')                                                         ! modify the string
         call modif(redoline,cin(2:))
!-----------------------------------------------------------------------------------------------------------------------------------
       case('m')                                                         ! modify the string with line number header
         write(*,'(1x,a)',iostat=ios)numbers(:len_trim(redoline))
         call modif(redoline,cin(2:))
!-----------------------------------------------------------------------------------------------------------------------------------
       case('c','s')                                                     ! change old string to new
         call change(redoline,trim(cin(1:255)),ier)                      ! xedit-like change command
!     C/STRING1/STRING2/    OR CW/STRING1/STRING2/  (CHANGE IN WINDOW)
!     WHERE / MAY BE ANY CHARACTER OTHER THAN W OR BLANK, WHICH IS NOT
!     INCLUDED IN STRING1 OR STRING2
!-----------------------------------------------------------------------------------------------------------------------------------
       case('u','b')                                                     ! up or back through buffer
         if(cin(2:).eq.' ')then
            iup=1
         else
            iup=int(s2v(cin(2:),ierr,onerr=0))
         endif
         ipoint=max(ipoint-iup,1)
         goto 1
!-----------------------------------------------------------------------------------------------------------------------------------
       case('d','f')                                                     ! down or forward through buffer
         if(cin(2:).eq.' ')then
            idown=1
         else
            idown=int(s2v(cin(2:),ierr,onerr=0))
         endif
         ipoint=min(ipoint+idown,iredo)
         goto 1
!-----------------------------------------------------------------------------------------------------------------------------------
       case(';')                                                         ! append lines
         ivals=int(s2vs(cin(2:)))
         if(allocated(ivals))then
            do i=1,size(ivals)
               ii=ivals(i)
               if(ii.ge.1.and.ii.le.iredo)then
                  read(iobuf,rec=ii,err=999)cinbuf(1:ibuf)               ! get last line in history file as line to redo
                  iend=len_trim(redoline)
                  redoline=redoline(:iend)//';'//trim(cinbuf)            !! should warn of truncation
               else
                  call journal('sc','*redol_* line not found in history',ii)
               endif
            enddo
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
       case('?','h')                                                     ! display help
         call help_()
!-----------------------------------------------------------------------------------------------------------------------------------
       case('D')                                                         ! toggle debug mode
         if(ddd .eqv. .false.)then
            ddd=.true.
            call journal('>')
         else
            ddd=.false.
            call journal('<')
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
       case('l','p')                                                     ! display history buffer file with line numbers
         if(cin(2:).eq.' ')then
            istart=iredo+1-20                                            ! default is to back up 20 lines
         else
            istart=int(s2v(cin(2:),ierr,onerr=0))
            if(ddd)call journal('d','*redol* istart=',istart,'ierr=',ierr)
            if(ierr.ne.0)istart=iredo
            if(istart.lt.0)then
               istart=iredo+1+istart
            endif
         endif
         istart=min(max(1,istart),iredo)                                 ! make istart a safe value
         if(ddd)call journal('d','*redol* istart=',istart,'iredo=',iredo)
         do i10=istart,iredo
            read(iobuf,rec=i10,iostat=ios)redoline(1:ibuf)
            if(ios.ne.0)then
               exit READLINE
            endif
            ix=max(1,len_trim(redoline))
            write(*,'(i5.5,1x,a)',iostat=ios)i10,redoline(:ix)
            if(ios.ne.0)then
               exit READLINE
            endif
         enddo
!-----------------------------------------------------------------------------------------------------------------------------------
       case('w')                                                         ! dump to a file
          cin=adjustl(cin(2:))                                           ! eliminate leading spaces and command name
          if(cin.eq.' ')then
             cin='DUMP'                                                  ! set as default and for message
          endif
          call do_w()
!-----------------------------------------------------------------------------------------------------------------------------------
       case('e','E')                                                     ! dump and edit history file and read it back in
          cmdline=cin(2:)                                                ! assume rest of command is a system command
          if(cmdline=='')cmdline='vim'                                   ! if no system command use "vim"
          cin='scratch.tmp'                                              ! assume this is a writable scratch file name
          cmdline=trim(cmdline)//' '//cin                                ! append scratch filename to system command
          call do_w()                                                    ! dump history file
          call execute_command_line(cmdline,cmdstat=cstat,cmdmsg=msg)    ! Execute the command line specified by the string.
          if(cstat.eq.0)then                                             ! rewrite or append to history file
             if(cmd.eq.'e')iredo=0
             call do_ar()
          endif
          open(newunit=iounit,file=cin,iostat=ios)                       ! remove scratch file
          if(ios.ne.0)then
            call journal('sc','*redol_* error opening scratch file file',cin,ios,'=',msg)
          endif
          close(unit=iounit,status='delete',iostat=ios,iomsg=msg)
          if(ios.ne.0)then
            call journal('sc','*redol_* error removing scratch file file',cin,ios,'=',msg)
          endif
!-----------------------------------------------------------------------------------------------------------------------------------
       case('a')                                                         ! append to history from a file
          cin=adjustl(cin(2:))                                           ! eliminate leading spaces and command name
          if(cin.eq.' ')then
             cin='DUMP'                                                  ! set as default and for message
          endif
          call do_ar()
!-----------------------------------------------------------------------------------------------------------------------------------
       case('r')                                                         ! replace history from a file
          iredo=0
          cin=adjustl(cin(2:))                                           ! eliminate leading spaces and command name
          if(cin.eq.' ')then
             cin='DUMP'                                                  ! set as default and for message
          endif
          call do_ar()
!-----------------------------------------------------------------------------------------------------------------------------------
       case('P','L')                                                     ! display history buffer file without line numbers
         if(cin(2:).eq.' ')then                                          ! default is to go back up to 20
            istart=iredo+1-20
         else
            istart=int(s2v(cin(2:),ierr,onerr=0))
            if(istart.lt.0)then
               istart=iredo+1+istart
            endif
         endif
         istart=min(max(1,istart),iredo)                                 ! make istart a safe value
         do i30=istart,iredo                                             ! easier to cut and paste if no numbers
            read(iobuf,rec=i30,iostat=ios)redoline(1:ibuf)
            if(ios.ne.0)then
               goto 999
            endif
            ix=max(1,len_trim(redoline))
            write(*,'(a)',err=999)redoline(:ix)
         enddo
!-----------------------------------------------------------------------------------------------------------------------------------
       case('/')                                                         ! display matches in buffer
         if(ilast.lt.2)then
            cycle
         endif
         do i20=1,iredo
            read(iobuf,rec=i20,err=999,iostat=ios)redoline(1:ibuf)
            if(index(redoline(1:ibuf),cin(2:ilast)).ne.0)then
               ix=max(1,len_trim(redoline))
               write(*,'(i5.5,1x,a)',err=999)i20,redoline(:ix)
               ipoint=i20
            endif
         enddo
         goto 1
!-----------------------------------------------------------------------------------------------------------------------------------
       case('!')                                                              ! external command
         if(ilast.lt.2)then
            cycle
         endif
         call execute_command_line(trim(cin(2:)),cmdstat=cstat,cmdmsg=msg)    ! Execute the command line specified by the string.
         !call system(trim(cin(2:)))                                          ! Execute the command line specified by the string.
!-----------------------------------------------------------------------------------------------------------------------------------
       case('.','q')                                                          ! blank out command and quit
         exit READLINE
!-----------------------------------------------------------------------------------------------------------------------------------
       case default                                                           ! assume anything else is a number
         val8=s2v(cin,ierr,onerr=0)
         if(ierr.eq.0)then
            iread=int(val8)
         else
            iread=0
         endif
         if(iread.gt.0.and.iread.le.iredo)then
            read(iobuf,rec=iread,err=999,iostat=ios)redoline(1:ibuf)
            ipoint=iread
         endif
!-----------------------------------------------------------------------------------------------------------------------------------
      end select
!-----------------------------------------------------------------------------------------------------------------------------------
   enddo READLINE
!-----------------------------------------------------------------------------------------------------------------------------------
999 continue
   redoline=' '
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine do_w()
WRITE: block
   open(newunit=idump,file=cin,iostat=ios,status='UNKNOWN',iomsg=msg)
   if(ios.ne.0)then
      call journal('sc','*redol_* error opening dump file',ios,'=',msg)
      exit WRITE
   endif
   do i15=1,iredo
      read(iobuf,rec=i15,iostat=ios,iomsg=msg)redoline(1:ibuf)
      if(ios.ne.0)then
         call journal('sc','*redol_* error reading history file',ios,'=',msg)
         exit WRITE
      endif
      ix=max(1,len_trim(redoline))
      write(idump,'(a)',iostat=ios,iomsg=msg)redoline(:ix)
      if(ios.ne.0)then
         call journal('sc','*redol_* error writing dump file',ios,'=',msg)
         close(idump,iostat=ios)
         exit WRITE
      endif
   enddo
   call journal('sc','wrote file ',cin)
endblock WRITE
close(idump,iostat=ios)
end subroutine do_w
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine do_ar()
REPLACE: block
   open(newunit=idump,file=cin,iostat=ios,status='OLD',iomsg=msg)
   if(ios.ne.0)then
      call journal('sc','*redol_* error opening file',ios,'=',msg)
      exit REPLACE
   endif
   do
      read(idump,'(a)',iostat=ios,iomsg=msg)redoline(1:ibuf)
      if(ios.ne.0)then
         if(.not.is_iostat_end(ios))then
            call journal('sc','*redol_* error reading file ',cin,ios,'=',msg)
         endif
         exit REPLACE
      endif
      iredo=iredo+1
      write(iobuf,rec=iredo,iostat=ios,iomsg=msg)redoline(1:ibuf)
      if(ios.ne.0)then
         call journal('sc','*redol_* error writing history file',ios,'=',msg)
         exit REPLACE
      endif
   enddo
endblock REPLACE
call journal('sc','read file ',cin)
close(idump,iostat=ios)
end subroutine do_ar
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine redol_
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine help_()

! ident_4="@(#)M_history::help_(3fp): prints help for REDO(3f)"

character(len=80),allocatable :: usage(:)
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
usage=[ &
&' History Edit commands (where N is a number):                                   ',&
&'+______________________________________________________________________________+',&
&'|List History                           |History File:                         |',&
&'| l|p N    # list from line N.          | w   file # write history to a file   |',&
&'!          # -N shows N last lines      | a   file # append file to history    |',&
&'| L|P N    # same as l sans line numbers| r   file # replace history with file |',&
&'| /string  # search for simple string   |Return to Normal Command Mode:        |',&
&'|Position in History File:              |      # return and execute command    |',&
&'| u|b N    # up/back through buffer     | .|q  # quit and return a blank line  |',&
&'| d|f N    # down/forward through buffer|Help:                                 |',&
&'| N        # load line number           |  h|?   # display this help text      |',&
&'|System:                                |Append lines to current line:         |',&
&'| !system_command # execute command     |  ;N N N N ...                        |',&
&'|______________________________________________________________________________|',&
&'|Edit Buffer:                                                                  |',&
&'| c|s/oldstring/newstring/  # change/substitute                                |',&
&'| mmod_string               # Modify with line number header                   |',&
&'|  mod_string               # Modify (replace, delete, insert)                 |',&
&'|    #         -- deletes                                                      |',&
&'|    &         -- replaces with a blank                                        |',&
&'|    ^STRING#  -- inserts a string                                             |',&
&'|              -- blank leaves as-is                                           |',&
&'|    Any other -- replaces character                                           |',&
&'+______________________________________________________________________________+']
!-----------------------------------------------------------------------------------------------------------------------------------
   !WRITE(*,'(a)'),usage(i),i=1,size(usage))
   do i=1,size(usage)
      call journal('sc',usage(i))
   enddo
end subroutine help_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_history
