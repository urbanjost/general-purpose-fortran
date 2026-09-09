      program demo_len
      implicit none

      ! fixed length
      character(len=40) :: string
      ! allocatable length
      character(len=:),allocatable :: astring
      character(len=:),allocatable :: many_strings(:)
      integer :: ii
        ! BASIC USAGE
         ii=len(string)
         write(*,*)'length =',ii

        ! ALLOCATABLE VARIABLE LENGTH CAN CHANGE
        ! the allocatable string length will be the length of RHS expression
         astring=' How long is this allocatable string? '
         write(*,*)astring, ' LEN=', len(astring)
        ! print underline
         write(*,*) repeat('=',len(astring))
        ! assign new value to astring and length changes
         astring='New allocatable string'
         write(*,*)astring, ' LEN=', len(astring)
        ! print underline
         write(*,*) repeat('=',len(astring))

        ! THE STRING LENGTH WILL BE CONSTANT FOR A FIXED-LENGTH VARIABLE
         string=' How long is this fixed string? '
         write(*,*)string,' LEN=',len(string)
         string='New fixed string '
         write(*,*)string,' LEN=',len(string)

        ! ALL STRINGS IN AN ARRAY ARE THE SAME LENGTH
        ! a scalar is returned for an array, as all values in a Fortran
        ! character array must be of the same length.
         many_strings = [ character(len=7) :: 'Tom', 'Dick', 'Harry' ]
         write(*,*)'length of ALL elements of array=',len(many_strings)

        ! NAME%LEN IS ESSENTIALLY THE SAME AS LEN(NAME)
        ! you can also query the length (and other attributes) of a string
        ! using a "type parameter inquiry" (available since fortran 2018)
         write(*,*)'length from type parameter inquiry=',string%len
        ! %len is equivalent to a call to LEN() except the kind of the integer
        ! value returned is always of default kind.

        ! LOOK AT HOW A PASSED STRING CAN BE USED ...
         call passed(' how long? ')

      contains

         subroutine passed(str)
         character(len=*),intent(in)  :: str
         ! the length of str can be used in the definitions of variables
            ! you can query the length of the passed variable
            write(*,*)'length of passed value is ', LEN(str)
         end subroutine passed

      end program demo_len
