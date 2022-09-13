      program demo_ifdef
      use M_list, only : dictionary
      implicit none
      type(dictionary)             :: table
      character(len=:),allocatable :: val
      integer                      :: i

         call table%set('A','value for A')
         call table%set('B','value for B')
         call table%set('C','value for C')
         call table%set('D','value for D')
         call table%set('E','value for E')
         call table%set('F','value for F')
         call table%set('G','value for G')
         call table%del('F')
         call table%del('D')

         write(*,*)'A=',table%ifdef('A')
         write(*,*)'B=',table%ifdef('B')
         write(*,*)'C=',table%ifdef('C')
         write(*,*)'D=',table%ifdef('D')
         write(*,*)'E=',table%ifdef('E')
         write(*,*)'F=',table%ifdef('F')
         write(*,*)'G=',table%ifdef('G')
         write(*,*)'H=',table%ifdef('H')

       end program demo_ifdef
