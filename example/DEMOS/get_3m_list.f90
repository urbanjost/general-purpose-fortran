      program demo_get
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

         write(*,*)'A=',table%get('A')
         write(*,*)'B=',table%get('B')
         write(*,*)'C=',table%get('C')
         write(*,*)'D=',table%get('D')
         write(*,*)'E=',table%get('E')
         write(*,*)'F=',table%get('F')
         write(*,*)'G=',table%get('G')
         write(*,*)'H=',table%get('H')

       end program demo_get
