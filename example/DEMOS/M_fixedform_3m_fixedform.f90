     program demo_M_fixedform
     use M_fixedform, only : fixedform, loaddata, icount_ptr, page_ptr, page_pd, icount_pd
     use M_strings, only : split, s2v
     implicit none
     character(len=5),parameter :: &
     & names(10)=[character(len=5) :: "RED","WHITE","BLUE","NAME","DATE","VALUE","EAST","WEST","NORTH","SOUTH"]
     character(len=:),allocatable   :: tabs
     character(len=:),allocatable   :: answers(:) ! output array of tokens
     logical                        :: lanswer
     real                           :: ranswer
     integer                        :: i
     ! make a namelist for the form
     logical                        :: red, white, blue
     character(len=80)              :: name
     character(len=80)              :: date
     real                           :: value
     logical                        :: east, west, north, south
     namelist /form/ RED,WHITE,BLUE,NAME,DATE,VALUE,EAST,WEST,NORTH,SOUTH
     call make_data()
     page_ptr=>page_pd
     icount_ptr=>icount_pd
     call loaddata('test.dat')      ! fill the page(*) with user data
     call fixedform(tabs)           ! display the form converted to a TDU

     write(*,*)'The field values are returned left to right, top to bottom'
     write(*,*)'as a tab-delimited string. The returned string is ...'
     write(*,*)tabs
     write(*,'(a)')repeat('=',80)

     write(*,*)'The strings can be read from the string, reformatted as a '
     write(*,*)'NAMELIST string, or parsed.'
     write(*,'(a)')repeat('=',80)

     write(*,*)'parse string into an array of strings using tab delimiter'
     call split(tabs,answers,char(9),nulls='return')
     write(*,*)'ARRAY IS'
     write(*,'(i0,t5,a)')(i,trim(answers(i)),i=1,size(answers))
     write(*,'(a)')repeat('=',80)

     ! different ways of converting the strings to other types

     write(*,*)' convert a T/F string to a logical'
     lanswer=answers(1).eq.'T'
     write(*,*)' RED is ',lanswer
     ! or use an internal read
     read(answers(2),*) lanswer
     write(*,*)' WHITE is ',lanswer
     write(*,'(a)')repeat('=',80)

     write(*,*)' NAME is  ',trim(answers(5))
     write(*,'(a)')repeat('=',80)

     write(*,*)' get a numeric value from a string'
     ranswer=real(s2v(answers(6)))
     write(*,*)' VALUE is ',ranswer
     ! or
     read(answers(6),'(g20.13)') ranswer
     write(*,*)' VALUE is ',ranswer
     write(*,'(a)')repeat('=',80)

     write(*,*)'write out the data in the form of a namelist file'
     NAME='UNKNOWN'
     DATE=''
     VALUE=0.00
     !*!open(11,status='scratch')
     open(11,file='form.txt',action='readwrite')
     write(11,'("&FORM")')
     do i=1,size(answers)
        answers(i)=adjustl(answers(i))
        if(answers(i).eq.'')then
           write(11,'(*(a))')"! ",trim(names(i)),' was blank'
        elseif(index(trim(answers(i)),' ').ne.0)then
           write(11,'(*(a))')trim(names(i)),'="',trim(answers(i)),'",'
        else
           write(11,'(*(a))')trim(names(i)),'=',trim(answers(i)),','
        endif
     enddo
     write(11,'("/")')
     ! read the namelist file (an internal file could have been used instead of a file)
     write(*,*)'read it back in'
     rewind(11)
     read(11,form)
     write(*,*)'write out namelist using namelist output'
     write(*,form)
     write(*,'(a)')repeat('=',80)
     contains
     subroutine make_data()
     integer,parameter :: LUN=10
     integer           :: ios
     open(unit=LUN,file='test.dat')
     write(LUN,'(a)')[character(len=80) ::                                            &
        '@    The simplest use of FIXEDFORM is when a text file is used to define a     @', &
        '@    form to be generated much like it could be drawn on paper:                @', &
        '################################################################################', &
        '#                                                                              #', &
        '#  ~ A basic form definition:         ~  ^ RED                                 #', &
        '#  ~ o Underlines become input fields ~  ^ WHITE                               #', &
        '#  ~ o Up-carets become menu options  ~  ^ BLUE                                #', &
        '#  ~ o Pound characters define boxes  ~                                        #', &
        '#  ~ o Text otherwise displays as-is  ~  Connected by pound characters or      #', &
        '#  ~   for the most part.             ~  adjacent to one another, up-carets    #', &
        '#  Name:  ___________________            form a radio button.                  #', &
        '#  Date:  ___________________            #######################               #', &
        '#  Value: ___________________            ^      ^       ^      ^               #', &
        '#                                       EAST   WEST   NORTH  SOUTH             #', &
        '#                                                                              #', &
        '# When the cursor is over a menu item it is toggled by pressing the space bar. #', &
        '# A tab character moves to the next selectable item. Typing in an input value  #', &
        '# changes the value. When the form is complete use the ctrl-S keys to submit.  #', &
        '################################################################################' ]
     close(unit=LUN,iostat=ios)
     end subroutine make_data
     end program demo_M_fixedform
