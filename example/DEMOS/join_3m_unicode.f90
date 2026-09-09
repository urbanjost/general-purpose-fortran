     program demo_join
     use M_unicode,  only : join, ut=>unicode_type, ch=>character, assignment(=)
     !use M_unicode, only : write(formatted)
     implicit none
     character(len=*),parameter    :: w='((g0,/,g0))'
     !character(len=*),parameter   :: v='((g0,/,DT))'
     character(len=20),allocatable :: proverb(:)
     type(ut),allocatable          :: s(:)
     type(ut),allocatable          :: sep
       !
       proverb=[ character(len=13) :: &
         & ' United'       ,&
         & '  we'          ,&
         & '   stand,'     ,&
         & '    divided'   ,&
         & '     we fall.' ]
       !
       if(allocated(s))deallocate(s)
       allocate(s(size(proverb))) ! avoid GNU Fortran (GCC) 16.0.0 bug
       s=proverb
       write(*,w) 'SIMPLE JOIN:         ', ch( join(s)                )
       write(*,w) 'JOIN WITH SEPARATOR: ', ch( join(s,sep=ut(' '))    )
       write(*,w) 'CUSTOM SEPARATOR:    ', ch( join(s,sep=ut('<-->')) )
       write(*,w) 'NO TRIMMING:         ', ch( join(s,clip=.false.)   )
       !
       sep=ut()
       write(*,w) 'SIMPLE JOIN:         ', ch(sep%join(s) )
       sep=' '
       write(*,w) 'JOIN WITH SEPARATOR: ', ch(sep%join(s) )
       sep='<-->'
       write(*,w) 'CUSTOM SEPARATOR:    ', ch(sep%join(s) )
       sep=''
       write(*,w) 'NO TRIMMING:         ', ch(sep%join(s,clip=.false.) )
     end program demo_join
