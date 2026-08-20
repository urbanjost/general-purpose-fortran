      program demo_expandtabs
      use M_unicode, only : expandtabs, ch=>character, replace
      use M_unicode, only : assignment(=), ut=> unicode_type
      implicit none
      type(ut)                     :: in
      type(ut)                     :: inexpanded
      character(len=:),allocatable :: dat
      integer                      :: i
         dat='  this is my string  '
         ! change spaces to tabs to make a sample input
         do i=1,len(dat)
            if(dat(i:i) == ' ')dat(i:i)=char(9)
         enddo
         in=dat
         !
         inexpanded=expandtabs(in)
         write(*,'("[",a,"]")')ch(inexpanded)
         inexpanded=replace(inexpanded,ut(' '),ut('_'))
         write(*,'("[",a,"]")')ch(inexpanded)
         !
         write(*,'("[",a,"]")')ch(in%expandtabs())
         write(*,'("[",a,"]")')ch(in%expandtabs(tab_size=8))
         write(*,'("[",a,"]")')ch(in%expandtabs(tab_size=1))
         write(*,'("[",a,"]")')ch(in%expandtabs(tab_size=0))
         !
      end program demo_expandtabs
