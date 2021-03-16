          program demo_substitute
          use M_strings, only : substitute
          implicit none
          ! must be long enough to hold changed line
          character(len=80) :: targetline

          targetline='this is the input string'
          write(*,*)'ORIGINAL    : '//trim(targetline)

          ! changes the input to 'THis is THe input string'
          call substitute(targetline,'th','TH')
          write(*,*)'th => TH    : '//trim(targetline)

          ! a null old substring means "at beginning of line"
          ! changes the input to 'BEFORE:this is the input string'
          call substitute(targetline,'','BEFORE:')
          write(*,*)'"" => BEFORE: '//trim(targetline)

          ! a null new string deletes occurrences of the old substring
          ! changes the input to 'ths s the nput strng'
          call substitute(targetline,'i','')
          write(*,*)'i => ""     : '//trim(targetline)

          end program demo_substitute
