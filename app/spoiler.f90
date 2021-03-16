!>
!!##NAME
!!    spoiler(1) - [CONVERT] A trifling program for reading punchlines that are encoded using the ROT-13 rotation encryption
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    spoiler [string(s)]<filename
!!##DESCRIPTION
!!    Read data from stdin or the command line and encrypt it with the
!!    trifling ROT-13 rotation encryption. A second run using the encrypted
!!    data as input will unencrypt that data. This is not suitable for
!!    secure encryption, but is used for such tasks as hiding a punchline
!!    to a joke in a newsgroup or web page.
!!##EXAMPLE
!!
!!   Sample usage:
!!
!!    echo 'fun but unsecure'|spoiler
!!
!!   Expected output:
!!
!!    sha ohg hafrpher
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
program spoiler
use M_CLI,     only : commandline, check_commandline, unnamed
use M_strings, only : rotate13
implicit none
character(len=256)           :: line
character(len=:),allocatable :: readme  ! stores updated namelist
integer                      :: ios
integer                      :: i
   readme=commandline(' ')
   call check_commandline(0,' ')
   if(size(unnamed).ne.0)then           ! if command arguments write them as one line
      write(*,'(*(a,1x))')(rotate13(trim(unnamed(i))),i=1,size(unnamed))
   else
      do
         read(*,'(a)',iostat=ios)line
         if(ios.ne.0)exit
         write(*,'(a)')rotate13(trim(line))
      enddo
   endif
end program spoiler
