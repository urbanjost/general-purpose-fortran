      program demo_clr
      use M_list, only : dictionary
      implicit none
      type(dictionary) :: caps
      integer                       :: i
         ! create a character string dictionary
         call caps%set('A','aye')
         call caps%set('B','bee')
         call caps%set('C','see')
         call caps%set('D','dee')
         ! show current dictionary
         write(*,'("DICTIONARY BEFORE CLEARED")')
         write(*,101)(trim(caps%key(i)),trim(caps%value(i)),i=1,size(caps%key))
         call  caps%clr()
         write(*,'("DICTIONARY AFTER CLEARED")')
         ! show current dictionary
         write(*,101)(trim(caps%key(i)),trim(caps%value(i)),i=1,size(caps%key))

      101 format (1x,*(a,"='",a,"'",:,","))
      end program demo_clr
