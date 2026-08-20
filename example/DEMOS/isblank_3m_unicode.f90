      program demo_isblank
      use M_unicode, only : isblank, unicode, ch=>character, unicode_type
      use M_unicode, only : assignment(=)
      implicit none
      integer                    :: i
      type(unicode_type)         :: string_u
      character(len=1),parameter :: string_a(*)=[(char(i),i=0,127)]

         write(*,'(*(g0,1x))')'ISBLANK PASSED TYPE(CHARACTER) : ',isblank(string_a)

         string_u=unicode%SPACES
         write(*,'(*(g0,1x))')'ISBLANK PASSED TYPE(UNICODE_TYPE): ',isblank(string_u)
         write(*,'(*(g0))')'BLANKS: ',ch(string_u)
         write(*,'(*(g0),1x)')'BLANKS: ',string_u%codepoint()
      end program demo_isblank
