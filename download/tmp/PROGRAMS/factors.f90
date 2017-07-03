program find_prime_factors
Use M_factor, only : prime_factors                                           ! prime number factoring routine
use M_kracken, only : kracken,igets,lget,iget                                ! command line parsing
implicit none
integer             :: iexp(10), iprm(10), nprm, number
integer             :: ios
integer             :: i
integer,allocatable :: values(:)
logical             :: verbose=.true.
integer             :: istart=0
integer             :: iend=0

   call kracken('factors','-help F -version F -verbose F -start 0 -end -1')   ! crack command line options
   call help_usage(lget('factors_help'))                                      ! if help text requested display it and quit
   call help_version(lget('factors_version'))                                 ! if version information requested display it and quit
   values=igets('factors_oo')                                                 ! get numbers from command line
   verbose=lget('factors_verbose')                                            ! get start and end of any range specified
   istart=iget('factors_start')                                               ! get start and end of any range specified
   iend=iget('factors_end')

   if(iend.gt.0.and.istart.le.0)then                        ! if iend specified but not istart set istart to 2
      call printme(2,iend)
   elseif(istart.gt.0.and.iend.le.0)then                    ! if istart is specified but iend is not, set iend to biggest integer
      call printme(istart,huge(0))
   else
      call printme(istart,iend)
   endif

   if(size(values).eq.0.and.(iend-istart.le.0))then         ! if did not print a range and no number specified prompt for values
      do
        !!write(*, '(a)', advance='no') ' Enter number to be factored: '
        read(*, *,iostat=ios) number
        if(is_iostat_end(ios))then
           exit
        elseif(ios.ne.0)then
           cycle
        endif
        call printme(number,number)
      enddo
   else                                                                      ! numbers are on command line
      do i=1,size(values)
         call printme(values(i),values(i))
      enddo
   endif

   contains

   subroutine printme(ii,jj)
   integer,intent(in) :: ii
   integer,intent(in) :: jj
   integer            :: ivalue,j,k
      do ivalue=ii,jj                                                               ! for values from ii to jj print factors
         call prime_factors(ivalue, nprm, iprm, iexp, verbose)                      ! get the factors
         if(.not.verbose)then                                                       ! if not printed yet print the values
            write(*,'(i0,": ",*(i0:,1x))')ivalue,( (iprm(k),j=1,iexp(k)) ,k=1,nprm) ! "VALUE: FACTOR(1) FACTOR(2) FACTOR(3) ..."
         endif
      enddo
   end subroutine printme

end program find_prime_factors
subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'       factors(1f) - [NUMBERS]display prime factors of numbers                  ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'       factors [NUMBER]...                                                      ',&
'       factors -start N -end M                                                  ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Print the prime factors of each specified integer NUMBER. If none are        ',&
'   specified on the command line, read them from standard input.                ',&
'                                                                                ',&
'   Typically, the numbers must be positive integers where                       ',&
'                                                                                ',&
'      2 <= NUMBER <= (2**31)-1 or 2147483647.                                   ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'       -start N   if specified factor a range of numbers starting with this     ',&
'                  value. If -end is specified defaults to 2.                    ',&
'       -end M     if specified factor a range of numbers ending with this       ',&
'                  value. If -start is specified defaults to huge(0).            ',&
'       --help     display this help and exit                                    ',&
'       --version  output version information and exit                           ',&
'       --verbose  verbose output                                                ',&
'EXAMPLE                                                                         ',&
' Sample Usage:                                                                  ',&
'                                                                                ',&
'  factors 512                                                                   ',&
'  512: 2 2 2 2 2 2 2 2 2                                                        ',&
'                                                                                ',&
'  factors 512 -verbose                                                          ',&
'  512 factors as (2**9)                                                         ',&
'                                                                                ',&
'  factors 202023 2147483647 -verbose                                            ',&
'  202023 factors as (3**2)*22447                                                ',&
'  2147483647 IS A PRIME NUMBER                                                  ',&
'                                                                                ',&
'  factors -start 2 -end 12 -verbose                                             ',&
'  2 IS A PRIME NUMBER                                                           ',&
'  3 IS A PRIME NUMBER                                                           ',&
'  4 factors as (2**2)                                                           ',&
'  5 IS A PRIME NUMBER                                                           ',&
'  6 factors as 2*3                                                              ',&
'  7 IS A PRIME NUMBER                                                           ',&
'  8 factors as (2**3)                                                           ',&
'  9 factors as (3**2)                                                           ',&
'  10 factors as 2*5                                                             ',&
'  11 IS A PRIME NUMBER                                                          ',&
'  12 factors as (2**2)*3                                                        ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        factors(1f) - [NUMBERS]display prime factors of numbers
!!
!!##SYNOPSIS
!!
!!        factors [NUMBER]...
!!        factors -start N -end M
!!
!!##DESCRIPTION
!!    Print the prime factors of each specified integer NUMBER. If none are
!!    specified on the command line, read them from standard input.
!!
!!    Typically, the numbers must be positive integers where
!!
!!       2 <= NUMBER <= (2**31)-1 or 2147483647.
!!
!!##OPTIONS
!!        -start N   if specified factor a range of numbers starting with this
!!                   value. If -end is specified defaults to 2.
!!        -end M     if specified factor a range of numbers ending with this
!!                   value. If -start is specified defaults to huge(0).
!!        --help     display this help and exit
!!        --version  output version information and exit
!!        --verbose  verbose output
!!##EXAMPLE
!!
!!  Sample Usage:
!!
!!   factors 512
!!   512: 2 2 2 2 2 2 2 2 2
!!
!!   factors 512 -verbose
!!   512 factors as (2**9)
!!
!!   factors 202023 2147483647 -verbose
!!   202023 factors as (3**2)*22447
!!   2147483647 IS A PRIME NUMBER
!!
!!   factors -start 2 -end 12 -verbose
!!   2 IS A PRIME NUMBER
!!   3 IS A PRIME NUMBER
!!   4 factors as (2**2)
!!   5 IS A PRIME NUMBER
!!   6 factors as 2*3
!!   7 IS A PRIME NUMBER
!!   8 factors as (2**3)
!!   9 factors as (3**2)
!!   10 factors as 2*5
!!   11 IS A PRIME NUMBER
!!   12 factors as (2**2)*3
!===================================================================================================================================
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        CLI library utilities and examples>',&
'@(#)PROGRAM:        factors(1f)>',&
'@(#)DESCRIPTION:    Determine prime factors of numbers>',&
'@(#)VERSION:        1.1, 20161007>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)UUID:           37a84d23-0b17-4cd5-bb09-54e23d2e66a6>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:02:29 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
