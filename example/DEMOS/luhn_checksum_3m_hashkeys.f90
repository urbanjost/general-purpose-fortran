          program demo_luhn_checksum
          use M_hashkeys, only : luhn_checksum
          implicit none
          character(len=:),allocatable :: ccards(:), string
          integer :: i, j
          write(*,*)'GOOD VALUES'
          ccards=[ character(len=20) :: '79927398713', '49927398716', '1234567812345670' ]
          call checkem()
          write(*,*)'BAD VALUES'
          ccards=[ character(len=20) :: &
             '79927398710','79927398711','79927398712','79927398714', &
             '79927398715','79927398716','79927398717','79927398718','79927398719', &
             '49927398717', '1234567812345678' ]
          call checkem()
          contains
          subroutine checkem
             ! validate these numbers
             do i=1,size(ccards)
                j=len(trim(ccards(i)))
                string=luhn_checksum(ccards(i)(:j-1))
                write(*,'(a,1x,a,1x,l1)')ccards(i),string,ccards(i).eq.string
             enddo

             string='123456 781-234-567'
             write(*,*)'from ',string,' got ',luhn_checksum(string),' which should be 1234567812345670'
          end subroutine checkem
          end program demo_luhn_checksum
