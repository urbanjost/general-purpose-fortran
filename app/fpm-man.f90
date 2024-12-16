program fman
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdout=>OUTPUT_UNIT, stdin=>INPUT_UNIT
use M_intrinsics, only : help_intrinsics
use M_CLI2,       only : set_args, sget, iget, lget, specified, topics=>unnamed
use M_CLI2,       only : set_mode
use M_match,      only : getpat, match, regex_pattern
use M_match,      only : YES, ERR
use M_strings,    only : lower, indent, atleast, str
use M_attr,       only : attr, attr_update
use M_io,         only : filewrite, fileread, get_env
implicit none
type(regex_pattern)            :: p, start_p, end_p
character(len=*),parameter     :: gen='(*(g0:))'
character(len=:),allocatable   :: help_text(:), version_text(:)
character(len=256),allocatable :: manual(:),section(:),clone_no_color(:)
character(len=:),allocatable   :: doc(:)
character(len=:),allocatable   :: regex, start, end
character(len=:),allocatable   :: query
character(len=:),allocatable   :: filename
character(len=:),allocatable   :: templine
character(len=:),allocatable   :: last
character(len=:),allocatable   :: editor
real                           :: rm
integer                        :: i, j, k, m
integer                        :: ilines
integer                        :: lines
integer                        :: iostat
character(len=256)             :: iomsg
character(len=:),allocatable   :: line
integer                        :: iinf
integer                        :: lun
integer                        :: direction
integer                        :: irestore
integer                        :: search_end
logical                        :: number
logical                        :: topic
logical                        :: cmdmode
logical                        :: prefix, ignorecase, demo, color
character(len=512)             :: paws, remember
character(len=20) ::  &
&  bg=       '<E>                ',  &  ! initial background color
&  fg=       '<w>                ',  &  ! text color
&  prg=      '<c>                ',  &  ! demo program text color
&  head=     '<y></bo>           ',  &  ! header line
&  head_=    '</bo>              ',  &
&  fixed=    '<w>                ',  &  ! color of leading > in demo program output
&  output=   '<y>                ',  &  ! demo program output
&  output_=  '</bo>              '
namelist/fman_colors/bg,fg,prg,head,head_,fixed,output,output_
   ! process command line
   number=.false.
   remember=''
   cmdmode=.false.
   iinf=0
   last='^NAME$'
   call setup()
   call set_mode('auto_response_file',.true.)
   call set_mode('lastonly')
   call set_args(' --regex:e " " --ignorecase:i F --topic_only:t F --demo:d F --color:c F --query:Q " " &
   & -start:S " " --end:E "^[A-Z][A-Z_ ]*$" --filename:f " " &
   & --prefixoff:O F --lines:l '//get_env('LINES','0')//'',&
   & help_text,version_text)
   regex=sget('regex')
   start=sget('start')
   end=sget('end')
   topic=lget('topic_only')
   ignorecase=lget('ignorecase')
   demo=lget('demo')
   color=lget('color')
   query=sget('query')
   lines=iget('lines')
   filename=sget('filename')
   if(filename.ne.'')then
      call fileread(FILENAME,doc)
      if(.not.allocated(doc))then
         write(stdout,*)'*fman* failed to load file',FILENAME
         stop
      else
         manual=doc
         clone_no_color=doc
         if(allocated(doc))deallocate(doc)
      endif
      if(size(topics).eq.0)then
         topics=['manual']
      endif
   elseif(topic)then
      ! if -t then just show topic names and exit
      manual = help_intrinsics('',topic=topic)
      ! could truncate if name is too long, could get a bit fancier or use
      ! M_display(3f) and have default just print one per line
      write(stdout,'(3(g0))') ( [character(len=80/3) :: manual(i)], i=1, size(manual) )
      stop
   endif

   ! compile any regular expression
   ! Also, if doing a regular expression and not the single topic "toc"
   ! add a section prefix when building manual

   ! initially assume prefixing is off unless a regular expression is used
   if(regex.ne.' '.or.start.ne.' ')then
      prefix=.true.
   else
      prefix=.false.
   endif

   ! normalize the topics list
   ! ensure there is at least one topic by applying a default
   if(size(topics).eq.0)then
      topics=['toc']
   endif

   if( ( size(topics).eq.1 .and. topics(1).eq.'toc') )then
      prefix=.false.
      ignorecase=.true.
   endif

   if(specified('prefixoff'))then
      prefix=.not.lget('prefixoff')
   endif

   if(regex.ne.' ')then
      if (getpat(merge(lower(regex),regex,ignorecase), p%pat) .eq. ERR) then
         stop '*fman* Illegal regex pattern.'
      endif
   endif
   if(start.ne.' ')then
      if (getpat(merge(lower(start),start,ignorecase), start_p%pat) .eq. ERR) then
         stop '*fman* Illegal start pattern.'
      endif
      if (getpat(merge(lower(end),end,ignorecase), end_p%pat) .eq. ERR) then
         stop '*fman* Illegal end pattern.'
      endif
   endif

   if(lget('verbose'))then
      write(stdout,gen)attr(str('<INFO>AFTER NORMALIZING:'))
      write(stdout,gen)attr(str('<INFO>REGEX.......',regex))
      write(stdout,gen)attr(str('<INFO>IGNORECASE..',ignorecase))
      write(stdout,gen)attr(str('<INFO>TOPIC_ONLY..',topic))
      write(stdout,gen)attr(str('<INFO>PREFIX......',prefix))
      write(stdout,gen)attr(str('<INFO>DEMO........',demo))
      write(stdout,gen)attr(str('<INFO>TOPICS......',str(topics)))
      write(stdout,gen)attr(str('<INFO>START.......',start))
      write(stdout,gen)attr(str('<INFO>END.........',end))
      write(stdout,gen)attr(str('<INFO>LINES.......',lines))
      write(stdout,gen)attr(str('<INFO>COLOR.......',color))
      write(stdout,gen)attr(str('<INFO>FILENAME....',filename))
      write(stdout,gen,advance='no')'Continue...'
      read(stdin,'(a)',iostat=iostat)paws
   endif
   ! build text to display or search
   if(filename.eq.'')then
      call load_manual()
   endif
   if(color)manual=crayons(manual)

   ! display selected text
   if(size(manual).eq.0)then
      write(stdout,'(g0)')'Sorry. did not find that. Perhaps you should search the TOC. try'
      write(stdout,'(g0)')'   fman -e TOPIC'
      write(stdout,'(g0)')'or search the entire manual:'
      write(stdout,'(g0)')'   fman manual -i -e TOPIC'
      stop 1
   else
      ! display what was found
      ilines=0
      i=1
      INFINITE: do
         if(regex.ne.'')then
            if(ignorecase)then
              templine=lower(trim(clone_no_color(i)))//char(10)
            else
              templine=trim(clone_no_color(i))//char(10)
            endif
            if(match(templine, p%pat) .eq. YES) then
               if(number)then
                    write(stdout,'(i0.6,1x,g0)')i,trim(manual(i))
               else
                    write(stdout,'(g0)')trim(manual(i))
               endif
            endif
         else
            if(number)then
               write(stdout,'(i0.6,1x,g0)')i,trim(manual(i))
            else
               write(stdout,'(g0)')trim(manual(i))
            endif
         endif
         if(lines.gt.0)then
            if(ilines.eq.lines-1)then
               ANOTHER: do
                  write(stdout,gen,advance='no')'[',i,'/',size(manual),']:'
                  read(stdin,'(a)',iostat=iostat)paws
                  if(iostat.ne.0)exit INFINITE
                  if(paws.eq.'')paws=remember
                  select case(paws(1:1))
                  case('b');
                             if(i.ge.size(manual))then
                                i=max(0,i-1*lines) ! back
                             else
                                i=max(0,i-2*lines+2) ! back
                             endif
                             i=i-(len_trim(paws)-1)*lines
                             iinf=0
                             remember=paws
                  case('u');
                             if(i.ge.size(manual))then
                                i=max(0,i-1*lines) ! up
                             else
                                i=max(0,i-2*lines+lines/2+2) ! up
                             endif
                             i=i-(len_trim(paws)-1)*lines/2 ! up
                             i=max(0,i)
                             iinf=0
                             remember=paws
                  case('d'); i=max(0,i-1*lines+len_trim(paws)*lines/2) ! down
                             iinf=0
                              remember=paws
                             iinf=0
                             remember=paws
                  case('/','n','N','\')
                             i=i-1
                             irestore=i
                             if(irestore.ge.size(manual))then
                                irestore=max(0,irestore-1*lines) ! back
                             else
                                irestore=max(0,irestore-1*lines+2) ! back
                             endif
                             regex=last
                             if(regex.eq.'')regex='^NAME$'
                             select case(paws(1:1))
                             case('/','n')
                                i=max(0,i-1*lines+4) ! back
                                if(paws(2:).ne.'') regex=paws(2:)
                                direction=1
                                search_end=size(manual)
                             case('\','N')
                                i=max(0,i-1*lines+3) ! back
                                if(paws(2:).ne.'') regex=paws(2:)
                                direction=-1
                                search_end=1
                                i=i-1
                             end select
                             i=max(0,min(i,size(manual)))
                             if(regex.ne.' ')then
                                if (getpat(merge(lower(regex),regex,ignorecase), p%pat) .eq. ERR) then
                                   write(stdout,'(a)')'*fman* Illegal regex pattern.'
                                   i=irestore
                                   paws='r'
                                else
                                   do m=i,search_end,direction
                                      if(ignorecase)then
                                        templine=lower(trim(clone_no_color(m)))//char(10)
                                      else
                                        templine=trim(clone_no_color(m))//char(10)
                                      endif
                                      if(match(templine, p%pat) .eq. YES) then
                                         i=m-1
                                         exit
                                      endif
                                   enddo
                                   if(m-direction.eq.search_end)then
                                      i=irestore
                                      paws='r'
                                   endif
                                endif
                             endif
                             last=regex
                             regex=''
                             iinf=0
                             remember=paws
                  case('r'); i=i-1                         ! refresh
                             !lines=get_env('LINES',lines)  ! adjust for screen size change if set
                             i=max(0,i-1*lines+2)
                             iinf=0
                             remember=paws
                  case('L')
                             filename=adjustl(trim(paws(2:)))
                              if(filename.ne.'')then
                                 call fileread(FILENAME,doc)
                                 if(.not.allocated(doc))then
                                    write(stdout,*)'*fman* failed to load file',FILENAME
                                    i=i-1                      ! refresh
                                    i=max(0,i-1*lines+2)
                                    iinf=0
                                 else
                                    i=0
                                    manual=doc
                                    clone_no_color=manual
                                    if(allocated(doc))deallocate(doc)
                                    iinf=0
                                    if(color)manual=crayons(manual)
                                 endif
                              endif
                              remember='f'
                  case('l')
                             if(i.ge.size(manual))then
                                i=max(0,i-1*lines) ! back
                             else
                                i=max(0,i-2*lines+2) ! back
                             endif
                             if(paws(2:).eq.'')then
                                !lines=get_env('LINES',lines) ! adjust for screen size change if set
                             else
                                read(paws(2:),'(g80.0)',iostat=iostat)rm
                                if(iostat.eq.0)then
                                   rm=min(real(size(manual)),rm)
                                   m=nint(rm)
                                   lines=max(m,0)
                                endif
                             endif
                             i=i-1
                             iinf=0
                             remember='f'
                  case('y','j','v'); i=max(0,i-1*lines+2) ! down one line
                             i=i+len_trim(paws)-1
                             iinf=0
                              remember=paws
                  case('e','k','^'); i=max(0,i-1*lines-0) ! up one line
                             i=max(0,i-len_trim(paws)+1)
                             iinf=0
                              remember=paws
                  case('s','w'); paws=adjustl(paws(2:)) ! save to file
                             if(paws.eq.'')paws='fman.txt'
                             iostat=filewrite(paws,clone_no_color)
                             i=max(0,i-1)
                             iinf=0
                             remember='f'
                  case('E'); i=max(0,i-1)  ! execute editor command
                             ! vim -c 'set ft=man ts=8 nomod nolist nonu' -c 'nnoremap i <nop>'
                             ! vim -R
                             editor=get_env('FCEDIT', get_env('FCEDIT', get_env('VISUAL','vi')))
                             iostat=filewrite('_scratch_',clone_no_color)
                             call execute_command_line(editor//' '//paws(2:)//' _scratch_')
                             lun=-1
                             open(newunit=lun,file='_scratch_',iostat=iostat,iomsg=iomsg)
                             if(iostat.ne.0)write(*,*)trim(iomsg)
                             close(unit=lun,status='delete',iostat=iostat,iomsg=iomsg)
                             if(iostat.ne.0)write(*,*)trim(iomsg)
                             iinf=0
                             remember='f'
                             cycle ANOTHER
                  case('.','x',' '); i=max(0,i-1)  ! execute command
                             if(paws.eq.'x')then
                                cmdmode=.not.cmdmode
                             elseif(paws(2:).ne.'')then
                                call execute_command_line(paws(2:))
                             endif
                             iinf=0
                             remember='f'
                             cycle ANOTHER
                  case('!',':'); i=max(0,i-1)  ! comment
                             iinf=0
                             remember='f'
                             cycle ANOTHER
                  case('g','0':'9','+','-'); ! goto from top
                             if(paws(1:1).eq.'g') paws=paws(2:)
                             if(paws.eq.'')then
                                i=0
                             else
                                call go_to(1)
                             endif
                             iinf=0
                             remember='f'
                  case('G'); i=size(manual) ! goto from bottom
                             paws=paws(2:)
                             if(paws.eq.'')then
                                i=max(0,i-1*lines+1) ! back
                             else
                                call go_to(-1)
                             endif
                             iinf=0
                             remember='b'
                  case('#'); number=.not.number
                             i=max(0,i-1*lines-1)
                             iinf=0
                             remember='f'
                  case('i'); ignorecase=.not.ignorecase
                             i=max(0,i-1*lines-1)
                             iinf=0
                             remember='f'
                  case('q','Q'); exit INFINITE      ! quit
                  case('f')
                             i=i+(len_trim(paws)-1)*lines ! forward number of characters
                             iinf=0
                             remember='f'
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                  case('S'); paws=adjustl(paws(2:)) ! save to file with color sequences if present
                             if(paws.eq.'')paws='fman.txt'
                             iostat=filewrite(paws,manual)
                             i=max(0,i-1)
                             iinf=0
                             remember='f'
                  case('D'); i=0                       ! developer: toggle demo mode
                             demo=.not.demo
                             call load_manual()
                             if(color)manual=crayons(manual)
                             iinf=0
                             remember='f'
                  case('P'); i=i-1                     ! developer: toggle prefix mode
                             i=max(0,i-1*lines+2)
                             prefix=.not.prefix
                             call load_manual()
                             if(color)manual=crayons(manual)
                             iinf=0
                             remember='f'
                  case('c','C'); i=i-1                      ! developer: display or set colors
                             i=max(0,i-1*lines+2)
                             iinf=0
                             if(adjustl(paws(2:)).eq.'')then ! toggle color mode
                                color=.not.color
                                call load_manual()
                                if(color)manual=crayons(manual)
                                i=0
                                remember='f'
                             elseif(adjustl(paws(2:)).eq.'?')then ! show colors
                                write(stdout,nml=fman_colors,iostat=iostat,iomsg=iomsg,delim='quote')
                                flush(stdout,iostat=iostat)
                                write(stdout,gen,advance='no')'[',i,']Continue...'
                                read(stdin,'(a)')paws
                                remember='f'
                             else  ! change colors
                                line='&FMAN_COLORS '//paws(2:)//' /'
                                read(line,nml=fman_colors,iostat=iostat,iomsg=iomsg)
                                if(iostat.ne.0)then
                                   write(stdout,*)'<ERROR>'//trim(iomsg)
                                   write(stdout,*)'<ERROR> INPUT="'//trim(line)
                                else
                                   call load_manual()
                                   if(color)manual=crayons(manual)
                                   i=0
                                endif
                                remember='f'
                             endif
                  case ('H')
                     i=0
                     topics=['help_text']
                     call load_manual()
                     if(color)manual=crayons(manual)
                     iinf=0
                     remember='f'
                  case ('V') ! display version text
                     write(stdout,'(a)')(trim(version_text(m)),m=1,size(version_text))
                     flush(stdout,iostat=iostat)
                     write(stdout,gen,advance='no')'[',i,']Continue...'
                     read(stdin,'(a)')paws
                     i=max(0,i-2*lines+2) ! back
                     remember='f'
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                  case('t','T')
                     i=0  ! developer: load a topic
                     if(paws.eq.'T')paws(2:)='toc'
                        topics=[adjustl(paws(2:))]
                     if(paws.eq.'t')then
                        call shorttopics()
                        if(color)manual=crayons(manual)
                        topics=['']
                     else
                        topics=[adjustl(paws(2:))]
                        call load_manual()
                        if(color)manual=crayons(manual)
                     endif
                     remember='f'
                  case('h','?')
                     call cribsheet()
                     i=max(0,i-2*lines+2) ! back
                     remember='f'
                  case default
                     if(cmdmode)then
                        call execute_command_line(paws)
                     else
                        call cribsheet()
                        i=max(0,i-2*lines+2) ! back
                        remember='f'
                     endif
                  end select

                  ilines=0
                  exit ANOTHER
               enddo ANOTHER
            endif
            ilines=ilines+1
         endif
      i=i+1
      if(i.gt.size(manual))then
         if(lines.gt.0)then
            i=size(manual)
            iinf=iinf+1  ! too many times, assume need to exit
            if(iinf.gt.10000)exit INFINITE
         else
            exit INFINITE
         endif
      endif

      enddo INFINITE
   endif
contains
subroutine cribsheet()
   ! '123456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 '
   write(stdout,'(a)')[character(len=80) :: &
   & ' +------------+-----+----------------------+------+---------------------------+ ', &
   & ' |POSITIONING:| b   | back one page        | f    | forward one page(default) | ', &
   & ' |            | u   | up 1/2 page          | d    | down 1/2 page             | ', &
   & ' |            | e   | up 1 line, eeeeee... | y    | down 1 line, yyyyyy...    | ', &
   & ' |            | gN  | goto Nth line        | [+-]N| [relative] moveto Nth line| ', &
   & ' +------------+-----+----------------------+------+---------------------------+ ', &
   & ' |SEARCH:     | /RE | search for expression| \RE  | backward search           | ', &
   & ' |            | n   | repeat last search   | N    | repeat last search upward | ', &
   & ' +------------+-----+----------------------+------+---------------------------+ ', &
   & ' |SYSTEM:     | s F | save to filename     | xcmd | execute system_command    | ', &
   & ' +------------+-----+----------------------+------+---------------------------+ ', &
   & ' |OPTIONS:    | #   | toggle line numbers  | lNNN | change lines per page     | ', &
   & ' |            | i   | toggle search by case| c    | toggle color mode         | ', &
   & ' +------------+-----+----------------------+------+---------------------------+ ', &
   & ' |GENERAL:    | q   | quit                 | r    | refresh                   | ', &
   & ' |            | h   | display help         | T    | reload Table Of Contents  | ', &
   & ' |            | tstr| load specified topic |      |                           | ', &
   & ' +------------+-----+----------------------+------+---------------------------+ ', &
   & ' | An empty string repeats the last positioning or toggle command. So if you  | ', &
   & ' | searched for a string or did an "e" or "y" and then just hit return the    | ', &
   & ' | previous command is repeated until a non-blank command like "r" is entered.| ', &
   & ' |                                                                            | ', &
   & ' +----------------------------------------------------------------------------+ ']
   if(paws(1:1).eq.'X')then
   write(stdout,'(a)')[character(len=80) :: &
   & ' +------------+-----+----------------------+------+---------------------------+ ', &
   & ' |DEVELOPER:  | C   | toggle color mode    | D    | toggle demo mode          | ', &
   & ' |            | Cstr| change colors        | P    | toggle prefix mode        | ', &
   & ' |            | C?  | show current colors  | X    | show developer help       | ', &
   & ' |            | H   | command help         | L    | load file                 | ', &
   & ' |            | V   | version information  |      |                           | ', &
   & ' +------------+-----+----------------------+------+---------------------------+ ', &
   & ' a loaded file cannot display a prefix string accept for the filename currently.']
   endif
   flush(stdout,iostat=iostat)
   write(stdout,gen,advance='no')'[',i,']Continue...'
   read(stdin,'(a)',iostat=iostat)paws
end subroutine cribsheet
subroutine load_manual()
! use topics list to load the manual variable
integer :: i
   manual=[character(len=0) ::]
   if(size(topics).eq.1)then
      if(topics(1).eq.'')then
         call shorttopics()
         return
      endif
   endif
   do i=1, size(topics)
      if(topics(i).eq.'help_text')then
         section = help_text
      else
         section = help_intrinsics(topics(i),prefix=prefix)
      endif
      ! extract demo program if found (has to follow specific format)
      if(demo)then
         call find_demo()
      endif
      if(start /= '' )then
         call find_start()
      endif

      manual = [character(len=max(len(manual),len(section))) :: manual,section,'']
      clone_no_color=manual
   enddo
end subroutine load_manual
subroutine shorttopics()
! get list of topics, write 3 per line onto an internal character page
integer :: m
   doc = help_intrinsics('',topic=.true.)
   if(allocated(manual))deallocate(manual)
   allocate(manual((size(doc)+3)/3))
   manual(:)=''
   write(manual,'(3(g0))') ( [character(len=80/3) :: doc(m)], m=1, size(doc) )
   manual=pack(manual,manual.ne.'')
   manual=[character(len=len(manual)) :: manual, &
   '', &
   'Note: ', &
   't manual # load entire set of available topics', &
   't toc    # go to full Table of Contents', &
   't NAME   # go to one of the listed topics', &
   ' ']
   clone_no_color=manual
end subroutine shorttopics

subroutine go_to(direction)
integer,intent(in) :: direction
integer :: plusminus
   paws=adjustl(paws)
   read(paws,'(i80)',iostat=iostat)m
   select case(paws(1:1))
   case('+');m=i+m-lines+2
   case('-');m=i+m-lines+2
   case default
   end select

   if(direction>0)then
      i=merge(m,i,iostat.eq.0)
   else
      i=merge(size(manual)-m,i,iostat.eq.0)
   endif
   i=min(size(manual)-1*lines+2,i)
   i=max(1,i)
   i=i-1
end subroutine go_to

subroutine find_demo()
character(len=256),allocatable :: newsection(:)
integer                        :: ii,jj,kk
integer                        :: start_keep, end_keep
   if(allocated(newsection)) deallocate(newsection)
   allocate(newsection(0))
   if(demo)then
      start_keep=0
      end_keep=0
      jj=0
      do ii=1,size(section)
         jj=jj+1
         if(jj.gt.size(section))exit
         if(index(lower(section(jj)),'program demo_').ne.0)then
            start_keep=jj
            do kk=start_keep+1,size(section)
               if(kk.gt.size(section))exit
               if(index(lower(section(kk)),'end program demo_').ne.0)then
                  end_keep=kk
                  if(start_keep.ne.0 .and. end_keep.ne.0)then
                     newsection=[character(len=max(len(newsection),len(section))) :: newsection,section(start_keep:end_keep)]
                     jj=kk+1
                  endif
                  exit
               endif
            enddo
         endif
      enddo
    endif
    if(size(newsection).eq.0)then
       write(stdout,*)'!<ERROR> *fman* standard demo code format not found for ',trim(topics(i))
       section=['']
    else
       section=newsection
       deallocate(newsection)
    endif
end subroutine find_demo

subroutine find_start()
character(len=256),allocatable :: newsection(:)
integer                        :: ii,jj,kk,ic
integer                        :: start_keep, end_keep
   if(size(section).eq.0)return
   if(allocated(newsection)) deallocate(newsection)
   allocate(newsection(0))
   if(specified('start'))then
      start_keep=0
      end_keep=0
      jj=0
      do
         jj=jj+1
         if(prefix)then
            ic=index(section(jj),':')+1
         else
            ic=1
         endif
         if(jj.gt.size(section))exit
         if(match(trim(section(jj)(ic:))//char(10), start_p%pat) .eq. YES) then
            start_keep=jj
            do kk=start_keep+1,size(section)
               if(kk.gt.size(section))exit
               if (match(trim(section(kk)(ic:))//char(10), end_p%pat) .eq. YES) then
                  end_keep=kk-1
                  if(start_keep.gt.0 .and. end_keep .gt. 0)then
                     newsection=[character(len=max(len(newsection),len(section))) :: newsection,section(start_keep:end_keep)]
                     jj=kk+1
                  endif
                  exit
               endif
            enddo
         endif
         if(jj.ge.size(section))exit
      enddo
    endif
    if(size(newsection).eq.0)then
       write(stdout,*)'!<ERROR> *fman* standard start code format not found for ',trim(topics(i))
       section=['']
    else
       section=newsection
       deallocate(newsection)
    endif
end subroutine find_start

function crayons(oldblock) result(newblock)
! just playing. There is a lot of stuff not done robustly here
character(len=256),intent(in),allocatable :: oldblock(:)
character(len=256),allocatable :: newblock(:)
integer :: ilen, gt, ipad
integer :: lead
integer :: width
logical :: program_text, after_demo
   width=max(80,maxval(len_trim(oldblock)))
   line=get_env('FMAN_COLORS')
   if(line.eq.'?')then
      write(stdout,nml=fman_colors,iostat=iostat,iomsg=iomsg,delim='quote')
      stop
   elseif(line.ne.'')then
      line='&FMAN_COLORS '//line//' /'
      read(line,nml=fman_colors,iostat=iostat,iomsg=iomsg)
      if(iostat.ne.0)then
         write(stdout,*)'<ERROR>'//trim(iomsg)
         write(stdout,*)'<ERROR> INPUT="'//trim(line)
      endif
   endif
   call attr_update('bg',      attr( bg,reset=.false.) )
   call attr_update('fg',      attr( fg,reset=.false.) )

   call attr_update('prg',     attr( prg,reset=.false.) )

   call attr_update('head',    attr( head,reset=.false.) )
   call attr_update('/head',   attr( head_,reset=.false.) )

   call attr_update('fixed',   attr( fixed,reset=.false.) )
   call attr_update('output',  attr( output,reset=.false.) )
   call attr_update('/output', attr( output_,reset=.false.) )

   program_text=.false.
   after_demo=.false.
   newblock= oldblock
   lead=0
   do j=1,size(oldblock)
      ! test if entering demo program text
      if( index(oldblock(j),'end program demo_') .eq. 0 .and. index(oldblock(j),'program demo_') .ne. 0)then
         program_text=.true.
         lead=indent(oldblock(j))
      endif
      ! if in program text
      if(program_text .eqv. .true.)then
        ipad=len_trim(oldblock(j))
        ipad=len_trim(than(oldblock(j)))-ipad
        newblock(j)=attr('<bg>'//repeat(' ',lead)//'<bg><prg>'//atleast(trim(than(oldblock(j)(lead+1:))),width-lead+ipad) )
      ! section header
      elseif(verify(oldblock(j)(1:1), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' ) == 0 .and. &
      & verify(oldblock(j), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ _') == 0 )then
         ilen=len_trim(oldblock(j))
         newblock(j)=attr('<bg><head> '//trim(oldblock(j))//' </head>'//repeat(' ',max(0,width-ilen-2))//'<reset>')
         after_demo=.false. ! started new section so indicate left EXAMPLE section. This is repeated when not needed
      ! demo program results
      elseif(after_demo.and.index(adjustl(oldblock(j)),'>').eq.1)then
         ilen=len_trim(oldblock(j))
         gt=index(oldblock(j),'>')
         newblock(j)=attr('<bg><fixed>'//oldblock(j)(:gt)//&
         & '<bg><output>'//trim(than(oldblock(j)(gt+1:)))//'</output>'//repeat(' ',max(0,width-ilen))//'<reset>')
       ! not header or demo program or demo program results
       else
         ipad=len_trim(oldblock(j))
         ipad=len_trim(than(oldblock(j)))-ipad
          newblock(j)=attr('<bg><fg>'//atleast(than(oldblock(j)),width+ipad)//'<reset>')
       endif
      if( index(oldblock(j),'end program demo_') .ne.0)then
         program_text=.false.
         after_demo=.true.
      endif
   enddo
end function crayons

function than(in) result(out)
character(len=*),intent(in)  :: in
character(len=:),allocatable :: out
integer                      :: i
   out=''
   do i=1,len_trim(in)
      select case(in(i:i))
      case('<')
         out=out//'<lt>'
      case('>')
         out=out//'<gt>'
      case default
         out=out//in(i:i)
      endselect
   enddo
end function than

subroutine setup()

help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'    fman(1f) - [DEVELOPER] output descriptions of Fortran intrinsics',&
'    (LICENSE:PD)                                                    ',&
'                                                                    ',&
'SYNOPSIS                                                            ',&
'    fman NAME(s) [[-ignorecase][--regex Regular_Expression]]|[-topic_only]',&
'                [--color][--demo][-lines LINES_PER_AGE]                   ',&
'                                                                          ',&
'    fman [ --help| --version]                                             ',&
'                                                                          ',&
'DESCRIPTION                                                               ',&
'   fman(1) prints descriptions of Fortran intrinsics as simple flat text. ',&
'                                                                          ',&
'   The text is formatted in the txt2man(1) markdown language so one can easily',&
'   generate man-pages on ULS (Unix-Like Systems).                             ',&
'                                                                              ',&
'OPTIONS                                                                       ',&
'  TOPIC(s)          A list of Fortran intrinsic names or the special names    ',&
'                    "toc" and "manual" (which generate a table of contents    ',&
'                    and the entire set of documents respectively).            ',&
'                    The default is "toc" and to ignore case.                  ',&
'  --regex,-e        Search all output per the provided Regular Expression.    ',&
'                    Output is prefixed with the topic it was found in.        ',&
'  --topic_only,-t   Only show topic names. Other switches are ignored.        ',&
'  --ignorecase,-i   Ignore case when searching for a Regular Expression.      ',&
'  --demo,-d         extract first demo program found for a topic (starting with',&
'                    "program demo_*" and ending with "end program demo_*").    ',&
'  --color           Use ANSI in-line escape sequences to display the text in   ',&
'                    set colors. Does not work with all terminal emulators or   ',&
'                    terminals. Must use the -r switch with less(1) for less(1) ',&
'                    to display colors.                                         ',&
'  --lines N,-l N    pause every N lines. In page mode commands may be entered  ',&
'                    at the prompt. Enter "h" to display available commands.    ',&
'  --help            Display this help and exit                                 ',&
'  --version         Output version information and exit                        ',&
'                                                                               ',&
'ENVIRONMENT                                                                    ',&
'   Allows specifying the strings used by the M_attr module to select colors.   ',&
'   FMAN_COLORS="bg=''<E>'',fg=''<w>'',prg=''<c>'',                             ',&
'      head=''<y></bo>'',head_=''</bo>'',fixed=''<w>'',                         ',&
'      output=''<y>'',output_=''</bo>''"                                        ',&
'   LINES                                                                       ',&
'      use "export LINES" from the bash shell to use the automatically generated',&
'      value. Set to a numeric value it activates paging of the output.         ',&
'EXAMPLES                                                                       ',&
'  Sample commands                                                              ',&
'                                                                               ',&
'   fman tan|less            # display a description of tan(3f)                 ',&
'   fman                     # list table of contents                           ',&
'   fman manual>fortran.txt  # create a copy of all descriptions                ',&
'   fman -e character        # check TOC for string. try "trigo","size","complex"',&
'                                                                                ',&
'   fman --regex ''character''   # look for string in the TOC ignoring case      ',&
'                                                                                ',&
'   # list the topic "scan" if found and lines containing "scan" from the entire ',&
'   # manual, prefixing the lines with the section name, while ignoring case.    ',&
'   fman -e scan -i manual                                                       ',&
'                                                                                ',&
'   fman -d verify >demo_verify.f90 # get demo program to try VERIFY(3f).        ',&
'                                                                                ',&
'   # change background to blue, page every 30 lines                             ',&
'   env FMAN_COLORS="bg=''<B>''" fman --color --lines 30 abs                     ',&
'                                                                                ',&
'   # Interactive mode                                                           ',&
'   export LINES # in bash(1) sense terminal size                                ',&
'   fman --color # bring up Table of Contents                                    ',&
'   t cos        # load description of intrinsic "cos"                           ',&
'   t verify     # load description of intrinsic "verify"                        ',&
'   T            # reload TOC (Table of Contents)                                ',&
'   /trig        # move forward to a line with "trig" in it                      ',&
'   #            # toggle on line numbers                                        ',&
'   h            # display crib sheet of commands                                ',&
'']

version_text=[ CHARACTER(LEN=128) :: &
'@(#) PRODUCT:         GPF (General Purpose Fortran) utilities and examples     >',&
'@(#) PROGRAM:         fman(1)                                                  >',&
'@(#) DESCRIPTION:     output Fortran intrinsic descriptions                    >',&
'@(#) VERSION:         2.0.0, 2024-10-25                                        >',&
'@(#) AUTHOR:          John S. Urban                                            >',&
'@(#) HOME PAGE:       http://www.urbanjost.altervista.org/index.html           >',&
'@(#) LICENSE:         MIT License                                              >',&
'']

end subroutine setup

end program fman
! kludge1: older versions of gfortran do not handle character arrays with both line and size allocatable
! always make non-color and color and toggle between the two
! a search that shows topic prefix and line number in original file
