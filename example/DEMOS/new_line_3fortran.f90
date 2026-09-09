      program demo_new_line
      implicit none
      ! Get the system's newline character
      character,parameter          :: nl=new_line('a')
      character(len=:),allocatable :: string
      real                         :: r
      integer                      :: i, count
      integer                      :: u, pos_save
      character(len=256)           :: line_buffer

        ! basics
         ! print a string with a newline embedded in it
         string='This is record 1.'//nl//'This is record 2.'
         write(*,'(a)') string

        ! Non-Advancing I/O with Newline
         ! Combining ADVANCE='NO' with NEW_LINE allows for granular control
         ! over output formatting.
         ! print a newline character string
         write(*,'(*(a))',advance='no') &
            nl,'This is record 1.',nl,'This is record 2.',nl

        ! Stream I/O

          ! 1. Open a file for formatted stream output
          open(newunit=u, file='test_stream.txt', access='stream', &
               form='formatted', status='replace')

          ! 2. Write data with manual newlines
          write(u, '(A)') 'First Line' // nl

          ! Inquire current position (byte offset) before writing second line
          inquire(unit=u, pos=pos_save)

          write(u, '(A)') 'Second Line' // nl
          write(u, '(A)') 'Third Line' // nl

          ! Jump directly to the saved position (start of the second line)
          read(u, '(A)', pos=pos_save) line_buffer
          print *, 'Data read from saved position:', trim(line_buffer)

          close(u)

        ! Extended Example Providing Paragraph Fill
         ! output a number of words of random length as a paragraph
         ! by inserting a new_line before line exceeds 70 characters

        ! simplistic paragraph print using non-advancing I/O
         count=0
         do i=1,100

            ! make some fake word of random length
            call random_number(r)
            string=repeat('x',int(r*10)+1)

            count=count+len(string)+1
            if(count.gt.70)then
               write(*,'(a)',advance='no')nl
               count=len(string)+1
            endif
            write(*,'(1x,a)',advance='no')string
         enddo
         write(*,'(a)',advance='no')nl

      end program demo_new_line
