           program demo_delim

           use M_strings, only: delim
           implicit none
           character(len=80) :: line
           character(len=80) :: dlm
           integer,parameter :: n=10
           character(len=20) :: array(n)=' '
           integer           :: ibegin(n),iterm(n)
           integer           :: i20, icount, ilen, i10
           line=' first  second 10.3 words_of_stuff  '
           do i20=1,4
              ! change delimiter list and what is calculated or parsed
              if(i20.eq.1)dlm=' '
              if(i20.eq.2)dlm='o'
              if(i20.eq.3)dlm=' aeiou'    ! NOTE SPACE IS FIRST
              if(i20.eq.3)ARRAY(1)='#N#'  ! QUIT RETURNING STRING ARRAY
              if(i20.eq.4)line='AAAaBBBBBBbIIIIIi  J K L'

              ! write out a break line composed of =========== ..
              write(*,'(57("="))')
              ! show line being parsed
              write(*,'(a)')'PARSING=['//trim(line)//'] on '//trim(dlm)
              ! call parsing procedure
              call delim(line,array,n,icount,ibegin,iterm,ilen,dlm)
              write(*,*)'number of tokens found=',icount
              write(*,*)'last character in column ',ilen
              if(icount.gt.0)then
                 if(ilen.ne.iterm(icount))then
                    write(*,*)'ignored from column ',iterm(icount)+1,' to ',ilen
                 endif
                 do i10=1,icount
                    ! check flag to see if ARRAY() was set
                    if(array(1).ne.'#N#')then
                       ! from returned array
                       write(*,'(a,a,a)',advance='no')&
                       &'[',array(i10)(:iterm(i10)-ibegin(i10)+1),']'
                    endif
                 enddo
                 ! using start and end positions in IBEGIN() and ITERM()
                 write(*,*)
                 do i10=1,icount
                    ! from positions in original line
                    write(*,'(a,a,a)',advance='no')&
                    &'[',line(ibegin(i10):iterm(i10)),']'
                 enddo
                 write(*,*)
              endif
           enddo
           end program demo_delim
