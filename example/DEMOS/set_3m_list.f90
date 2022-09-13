      program demo_set
      use M_list, only : dictionary
      implicit none
      type(dictionary) :: dict
      integer          :: i

          call dict%set('A','b')
          call dict%set('B','^')
          call dict%set('C',' ')
          call dict%set('D','c')
          call dict%set('E','ZZ')
          call dict%set('F','ZZZZ')
          call dict%set('G','z')
          call dict%set('A','new value for A')

          write(*,'(*(a,"==>","[",a,"]",/))') &
           & (trim(dict%key(i)),              &
           & dict%value(i)(:dict%count(i)),   &
           & i=1,size(dict%key))

       end program demo_set
