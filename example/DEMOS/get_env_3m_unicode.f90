      program demo_get_env
      use M_unicode, only : get_env, ut=> unicode_type
      use M_unicode, only : assignment(=), operator(//)
      implicit none
      type(ut) :: name
      type(ut) :: default
      type(ut) :: value
      type(ut) :: smiley
      integer  :: i
      character(len=*),parameter :: bracket= '(1x,*("[",a,"]",:))'
         !
         smiley=128515 ! set with Unicode code point
         name='UTF8'   ! set with ASCII
         default='Have a nice day '//smiley//'!' ! set with unicode_type
         !
         ! arguments can be type(unicode_type) or character
         ! but type(unicode_type) is always returned
         value=get_env(name,             default             )
         value=get_env(name%character(), default%character() )
         value=get_env(name,             default%character() )
         value=get_env(name%character(), default             )
         !
         write(*,*)value%character()
         !
         ! print each glyph surrounded by brackets
         write(*,bracket)(value%character(i,i),i=1,value%len())
         !
      end program demo_get_env
