program testit
use M_intrinsics, only : help_intrinsics
use M_CLI2,       only : set_args, sget, lget, topics=>unnamed
use M_match,      only : getpat, match, regex_pattern
use M_match,      only : YES, ERR
use M_strings,    only : lower, indent, atleast
use M_escape,     only : esc
implicit none
type(regex_pattern)          :: p
character(len=:),allocatable :: help_text(:), version_text(:)
character(len=256),allocatable :: manual(:),section(:)
character(len=:),allocatable :: regex
integer                      :: i, j, k
logical                      :: topic
logical                      :: prefix, ignorecase, demo, color
integer                      :: start_keep, end_keep
    ! process command line
    call setup()
    call set_args(' --regex:e " " --ignorecase:i F --topic_only:t F --demo:d F --color:c',&
    & help_text,version_text)
    regex=sget('regex')
    topic=lget('topic_only')
    ignorecase=lget('ignorecase')
    demo=lget('demo')
    color=lget('color')

    ! if -t then just show topic names and exit
    if(topic)then
       manual = help_intrinsics('',topic=topic)
       ! could truncate if name is too long, could get a bit fancier or use
       ! M_display(3f) and have default just print one per line
       write(*,'(3(g0))') ( [character(len=80/3) :: manual(i)], i=1, size(manual) )
       stop
    endif

    ! compile any regular expression
    ! Also, if doing a regular expression and not the single topic "toc"
    ! add a section prefix when building manual


    ! initially assume prefixing is off unless a regular expression is used
    if(regex.ne.' ')then
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

    if(regex.ne.' ')then
       if (getpat(merge(lower(regex),regex,ignorecase), p%pat) .eq. ERR) then
          stop '*fpm-man* Illegal pattern.'
       endif
    endif

    if(lget('verbose'))then
       write(*,'(*(g0:,1x))')'<INFO>AFTER NORMALIZING:'
       write(*,'(*(g0:,1x))')'<INFO>REGEX       ',regex
       write(*,'(*(g0:,1x))')'<INFO>IGNORECASE  ',ignorecase
       write(*,'(*(g0:,1x))')'<INFO>TOPIC_ONLY  ',topic
       write(*,'(*(g0:,1x))')'<INFO>PREFIX      ',prefix
       write(*,'(*(g0:,1x))')'<INFO>DEMO        ',demo
       write(*,'(*(g0:,1x))')'<INFO>TOPICS      ',topics
    endif
    ! build text to display or search
    manual=[character(len=0) ::]
    do i=1, size(topics)
       section = help_intrinsics(topics(i),prefix=prefix)
       if(color)section=crayons(section)

       ! extract demo program if found (has to follow specific format)
       if(demo)then
          call find_demo()
       endif

       manual = [character(len=max(len(manual),len(section))) :: manual,section]
    enddo

    ! display selected text
    if(size(manual).eq.0)then
       write(*,'(g0)')'Sorry. did not find that. Perhaps you should search the TOC. try'
       write(*,'(g0)')'   fpm-man -e TOPIC'
       write(*,'(g0)')'or search the entire manual:'
       write(*,'(g0)')'   fpm-man manual -i -e TOPIC'
       stop 1
    else
       ! display what was found
       do i=1,size(manual)
          if(regex.ne.'')then
             select case(ignorecase)
             case(.true.)
                if(match(lower(trim(manual(i)))//char(10), p%pat) .eq. YES) then
                  write(*,'(g0)')trim(manual(i))
                endif
             case(.false.)
                if (match(trim(manual(i))//char(10), p%pat) .eq. YES) then
                  write(*,'(g0)')trim(manual(i))
                endif
             end select
          else
             write(*,'(g0)')trim(manual(i))
          endif
       enddo
    endif
contains
subroutine find_demo()
character(len=256),allocatable :: newsection(:)
integer :: i,j,k
   if(allocated(newsection)) deallocate(newsection)
   allocate(newsection(0))
   if(demo)then
      start_keep=0
      end_keep=0
      j=0
      do i=1,size(section)
         j=j+1
         if(j.gt.size(section))exit
         if(index(lower(section(j)),'program demo_').ne.0)then
            start_keep=j
            do k=start_keep+1,size(section)
               if(k.gt.size(section))exit
               if(index(lower(section(k)),'end program demo_').ne.0)then
                  end_keep=k
                  if(start_keep.ne.0 .and. end_keep.ne.0)then
                     newsection=[character(len=max(len(newsection),len(section))) :: newsection,section(start_keep:end_keep)]
                     j=k+1
                  endif
                  exit
               endif
            enddo
         endif
      enddo
    endif
    if(size(newsection).eq.0)then
       write(*,*)'!<ERROR> *fpm-man* standard demo code format not found for ',trim(topics(i))
       section=['']
    else
       section=newsection
       deallocate(newsection)
    endif
end subroutine find_demo
function crayons(oldblock) result(newblock)
! just playing. There is a lot of stuff not done robustly here
character(len=256),intent(in),allocatable :: oldblock(:)
character(len=256),allocatable :: newblock(:)
integer :: ilen
integer :: lead
logical :: program_text
   program_text=.false.
   newblock= oldblock
   do j=1,size(oldblock)
      if( index(oldblock(j),'end program demo_') .eq. 0 .and. index(oldblock(j),'program demo_') .ne. 0)then
         program_text=.true.
         lead=indent(oldblock(j))
      endif
      if(program_text .eqv. .true.)then
        newblock(j)=esc('<E>'//repeat(' ',lead)//'<E><y>'//atleast(oldblock(j)(lead+1:),80-lead) )
      elseif(verify(oldblock(j)(1:1), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' ) == 0 .and. &
      & verify(oldblock(j), 'ABCDEFGHIJKLMNOPQRSTUVWXYZ _') == 0 )then
         ilen=len_trim(oldblock(j))
         newblock(j)=esc('<E><y><bo> '//trim(oldblock(j))//' </bo>'//repeat(' ',max(0,80-ilen-2))//'<reset>')
       else
          ilen=len_trim(oldblock(j))
          ilen=len_trim(than(oldblock(j)))-ilen
          newblock(j)=esc('<E><w>'//atleast(than(oldblock(j)),80+ilen)//'<reset>')
       endif
      if( index(oldblock(j),'end program demo_') .ne.0)then
         program_text=.false.
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
'NAME                                                                            ',&
'    fpm-man(1f) - [DEVELOPER] output descriptions of Fortran intrinsics         ',&
'    (LICENSE:PD)                                                                ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'    fpm-man NAME(s) [[-ignorecase][--regex Regular_Expression]]|[-topic_only]   ',&
'                [--color][--demo]                                               ',&
'                                                                                ',&
'    fpm-man [ --help| --version]                                                ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   fpm-man(1) prints descriptions of Fortran intrinsics as simple flat text.    ',&
'                                                                                ',&
'   The text is formatted in the txt2man(1) markdown language so one can easily  ',&
'   generate man-pages on ULS (Unix-Like Systems).                               ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'  TOPIC(s)          A list of Fortran intrinsic names or the special names      ',&
'                    "toc" and "manual" (which generate a table of contents      ',&
'                    and the entire set of documents respecively).               ',&
'                    The default is "toc" and to ignore case.                    ',&
'  --regex,-e        Search all output per the provided Regular Expression.      ',&
'                    Output is prefixed with the topic it was found in.          ',&
'  --itopic_only,-t  Only show topic names. Other switches are ignored.          ',&
'  --ignorecase,-i   Ignore case when searching for a Regular Expression.        ',&
'  --demo,-d         extract first demo program found for a topic, starting with ',&
'                    "program demo_" and ending with "end program demo_".        ',&
'  --color           Use ANSI in-line escape sequences to display the text in    ',&
'                    set colors. Does not work with all terminal emulators or    ',&
'                    terminals. Must use the -r switch with less(1) for less(1)  ',&
'                    to display colors.                                          ',&
'  --help            Display this help and exit                                  ',&
'  --version         Output version information and exit                         ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'  Sample commands                                                               ',&
'                                                                                ',&
'   fpm-man                 # list table of contents                             ',&
'   fpm-man -e character    # check TOC for string. try "trigo","size","complex" ',&
'   fpm-man tan|less        # display a description of tan(3f)                   ',&
'                                                                                ',&
'   fpm-man --regex ''character'' # look for string in the TOC ignoring case     ',&
'                                                                                ',&
'   fpm-man manual>fortran.txt    # create a copy of all descriptions            ',&
'                                                                                ',&
'   # list the topic "scan" if found and lines containing "scan" from the entire ',&
'   # manual, prefixing the lines with the section name, while ignoring case.    ',&
'   fpm-man -e scan -i manual                                                    ',&
'                                                                                ',&
'   fpm-man -d verify >demo_verify.f90 # get sample program to try VERIFY(3f).   ',&
'']

version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:         GPF (General Purpose Fortran) utilities and examples           ',&
'PROGRAM:         fpm-man(1)                                                     ',&
'DESCRIPTION:     output Fortran intrinsic descriptions                          ',&
!'VERSION:         1.0.0, 20201215                                               ',&
!'VERSION:         1.0.1, 20201217                                               ',&
'VERSION:         1.0.2, 202100108                                               ',&
'AUTHOR:          John S. Urban                                                  ',&
'HOME PAGE:       http://www.urbanjost.altervista.org/index.html                 ',&
'LICENSE:         MIT License                                                    ',&
'']

end subroutine setup
end program testit
! kludge1: older versions of gfortran do not handle character arrays with both line and size allocatable
