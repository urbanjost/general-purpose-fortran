      program demo_new_line
      implicit none
      character,parameter :: nl=new_line('a')
      character(len=:),allocatable :: string
      real :: r
      integer :: i, count

        ! basics
         ! print a string with a newline embedded in it
         string='This is record 1.'//nl//'This is record 2.'
         write(*,'(a)') string

         ! print a newline character string
         write(*,'(*(a))',advance='no') &
            nl,'This is record 1.',nl,'This is record 2.',nl

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
