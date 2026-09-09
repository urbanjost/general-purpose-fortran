      program demo_isascii
      use M_unicode, only : ut=>unicode_type, assignment(=)
      use M_unicode, only : isascii, ch=>character
      implicit none
      integer                      :: i
      character(len=256)           :: ascii8
      type(ut)                     :: uascii8
      type(ut)                     :: ustring
      character(len=:),allocatable :: astring
         do i=1,256
            ascii8(i:i)=char(i-1)
         enddo
         uascii8=[(i,i=0,255)]

         write(*,*)'CHARACTER:   all of ascii8',isascii(ascii8)
         write(*,*)'CHARACTER:   all of ascii7',isascii(ascii8(1:128))
         write(*,*)'UNICODE TYPE:all of ascii8',isascii(uascii8)
         write(*,*)'UNICODE TYPE:all of ascii7',isascii(uascii8%sub(1,128))

         ! French pangram translates from the French to
         ! "Take this old whisky to the blond judge who is smoking."

         astring='Portez ce vieux whisky au juge blond qui fume.'
         ustring=astring
         write(*,*)'CHARACTER:   ',isascii(astring),astring
         write(*,*)'UNICODE_TYPE:',isascii(ustring),ch(ustring)

         ! (variant with “é”)
         astring='Portez ce vieux whisky au juge blond qui a fumé.'
         ustring=astring
         write(*,*)'CHARACTER    ',isascii(ustring),ch(ustring)
         write(*,*)'UNICODE_TYPE:',isascii(ustring),ch(ustring)

      end program demo_isascii
