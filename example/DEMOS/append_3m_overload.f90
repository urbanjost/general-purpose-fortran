      program demo_append
      use :: M_overload, only : append
      use :: M_overload, only : operator(.append.)
      use :: M_overload, only : operator( + )
      implicit none
      character(:), allocatable :: str(:)
         !
         ! plus (+) operator overload
         str = "This" + "is" + "an" + "array" + "of" + "characters"
         print "(('[',A,']'))", str
         print '(*(g0))', 'size=',size(str), ',len=',len(str)
         !
         ! .append.
         str = &
         "This" .append. "is" &
         .append. "an" .append. "array" &
         .append. "of" .append. "characters"
         print "(('[',A,']'))", str
         !
         ! append()
         str = append("This","is") + &
               append("an","array") + &
               append("of","characters")
         print "(('[',A,']'))", str

      end program demo_append
