          program demo_M_logic
          use M_journal, only : journal  ! for logging messages
          use M_strings, only : lower, delim,v2s ! convert character case; split string
          use M_logic, only : cond
          use M_logic, only : write ! flag whether current data lines should be written
          use M_logic, only : nest_level   ! nesting level for #IF/#ELSEIF/#ELSE/#ENDIF
          use M_calculator, only : rnum0
          character(len=1)    :: prefix              ! directive prefix character
          character(len=1024) :: line                ! input line
          integer,parameter   :: max_words=2  ! maximum number of words allowed on line
          character(len=1024) :: array(max_words)    ! working copy of input line
          ! location where words start and end
          integer             :: ibegin(max_words), iterm(max_words)
          !----------------------------------------------------------------------------
          PREFIX='#'              ! for the example, assume direct lines use a # prefix
          !----------------------------------------------------------------------------
          READLINE: do                                   ! read loop to read input file
             read(*,'(a)',iostat=ios) line
             if(ios.ne.0)then
                if (nest_level.ne.0) then ! check to make sure all if blocks are closed
                   call journal('sc',&
                   &'*logic* error - #IF BLOCK NOT CLOSED WHEN READING FILE FINISHED.')
                endif
                stop
             endif
             ! although delim(3f) can do more
             ! just parsing the first word out and finding where second word starts
             ! make sure array is initialized for when
             ! icount(number of words on line) is zero
             array=' '
             call delim(lower(line),array,max_words,icount,ibegin,iterm,ilen,' ')
             select case(array(1))
             ! find conditional lines
             case('#if','#else','#elseif','#endif')
                ! process conditional directive
                call cond(trim(array(1)(2:)),line(iterm(1)+1:),ierr_logic)
             case('#define')
                ! evaluate expression
                value=rnum0(line(iterm(1)+1:))
             case default
                ! find input lines you want to use, skip others
                if (write) then
                   ! for example, if last conditional was true then write line
                   write(*,'(a)') trim(line)
                   ! write data line
                endif
             end select
          enddo READLINE
          end program demo_M_logic
