      program demo_get_arg
      use M_unicode, only : get_arg, ut=> unicode_type, ch=>character
      use M_unicode, only : assignment(=), operator(//), write(formatted)
      implicit none
      integer  :: position
      type(ut) :: default
      type(ut) :: value
      type(ut) :: smiley
      integer  :: i
      character(len=*),parameter :: bracket= '(1x,*("[",a,"]",:))'
         !
         smiley=128515 ! set with Unicode code point
         default='Wish I was first '//smiley//'!' ! set with unicode_type
         !
         ! arguments can be type(unicode_type) or character
         ! but type(unicode_type) is always returned
         do position=0,command_argument_count()
            value=get_arg(position, default             )
            value=get_arg(position, default%character() )
            !
            write(*,*)value%character()
            write(*,'(DT)')default%get_arg(position)
            !
            ! print each glyph surrounded by brackets
            write(*,bracket)(value%character(i,i),i=1,value%len())
         enddo
         !
      end program demo_get_arg
