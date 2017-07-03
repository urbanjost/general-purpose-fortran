module M_messages
private
   ! LINEART
   public junbad        ! <a href="junbad.3.html">print eye-catching ASCII graphic (skull)</a>
   public junbat        ! <a href="junbat.3.html">print eye-catching ASCII graphic (bat)</a>
   public junbuster     ! <a href="junbuster.3.html">print eye-catching ASCII graphic (ghostbuster)</a>
   public jundragon     ! <a href="jundragon.3.html">print eye-catching ASCII graphic (dragon) with message</a>
   public junroach      ! <a href="junroach.3.html">print eye-catching ASCII graphic (roach)</a>
   public junsun        ! <a href="junsun.3.html">print eye-catching ASCII graphic (sunshine)</a>
   public juntrolls     ! <a href="juntrolls.3.html">print eye-catching ASCII graphic (trolls) with message</a>

   ! ILLUMINATE
   public tabgraph      ! <a href="tabgraph.3.html">write columns of numbers with a text scale to the right</a>

   ! BLOCK LETTERS
   public blocks        ! <a href="blocks.3.html">write out 132-character string in large block letters</a>
   public signs         ! <a href="signs.3.html">write up to 132 large block letters</a>

   ! PROGRESS AND STATUS
   public percent_done  ! <a href=percent_done.3.html"> place a non-advancing status counter on terminal display </a>
contains

!>
!!##NAME
!!      junbad(3f) - [M_messages]print an eye-catching image of a skull.
!!
!!##SYNOPSIS
!!
!!    SUBROUTINE junbad(where)
!!
!!     character(len=*),intent(in) :: where
!!
!!##DESCRIPTION
!!
!!    Calls JOURNAL(3f) to print an eye-catching lineart image of a skill.
!!    Typically used to indicate an error has occurred in a program.
!!
!!##OPTIONS
!!    where  String used to indicate output disposition. See the description
!!           of JOURNAL() for a meaning for the I/O flag. To write to stdout
!!           use 's'.
!!
!!##EXAMPLE
!!
!!    Example program:
!!
!!       program seebad
!!       use M_messages, only : junbad
!!          call junbad('s')
!!       end program seebad
!!
!!    Expected output:
!!
!!       >           _,.-----.,_
!!       >         ,-~           ~-.
!!       >       ,^___           ___^.
!!       >      /~"   ~"   .   "~   "~!!       >     Y  ,--._    I    _.--.  Y
!!       >     | Y     ~-. | ,-~     Y |
!!       >     | |        }:{        | |
!!       >     j l       / | \       ! l
!!       >  .-~  (__,.--" .^. "--.,__)  ~-.
!!       > (           / / | \ \           )
!!       >  \.____,   ~  \/"\/  ~   .____,/
!!       >   ^.____                 ____.^
!!       >      | |T ~\  !   !  /~ T| |
!!       >      | |l   _ _ _ _ _   !| |
!!       >      | l \/V V V V V V\/ j |
!!       >      l  \ \|_|_|_|_|_|/ /  !
!!       >       \  \[T T T T T TI/  /
!!       >        \  `^-^-^-^-^-^`  /
!!       >         \               /
!!       >          \.           ,/
!!       >            "^-.___,-^"
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine junbad(where)
use M_journal, only: journal
implicit none
character(len=*),parameter :: ident="@(#)M_messages::junbad(3f):print eye-catching ASCII graphic (skull)"
character(len=*),intent(in) :: where
integer                     :: i
!   some compilers use \ as an escape character, so change \ to \\ if have problems
character(len=*),parameter    :: picture(23)=[&
&'           _,.-----.,_              ',&
&'         ,-~           ~-.          ',&
&'       ,^___           ___^.        ',&
&'      /~"   ~"   .   "~   "~\       ',&
&'     Y  ,--._    I    _.--.  Y      ',&
&'     | Y     ~-. | ,-~     Y |      ',&
&'     | |        }:{        | |      ',&
&'     j l       / | \       ! l      ',&
&'  .-~  (__,.--" .^. "--.,__)  ~-.   ',&
&' (           / / | \ \           )  ',&
&'  \.____,   ~  \/"\/  ~   .____,/   ',&
&'   ^.____                 ____.^    ',&
&'      | |T ~\  !   !  /~ T| |       ',&
&'      | |l   _ _ _ _ _   !| |       ',&
&'      | l \/V V V V V V\/ j |       ',&
&'      l  \ \|_|_|_|_|_|/ /  !       ',&
&'       \  \[T T T T T TI/  /        ',&
&'        \  `^-^-^-^-^-^`  /         ',&
&'         \               /          ',&
&'          \.           ,/           ',&
&'            "^-.___,-^"             ',&
&'            "^-.___,-^"             ',&
&'            "^-.___,-^"             ']
do i=1,size(picture)-2
   call journal(where,picture(i))
enddo
end subroutine junbad
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      junbat(3f) - [M_messages]print an eye-catching image of a bat.
!!
!!##SYNOPSIS
!!
!!   SUBROUTINE junbat(where)
!!
!!    character(len=*),intent(in) :: where
!!
!!##DESCRIPTION
!!  Sample output:
!!
!!   >                     .-                    .-
!!   >                _..-'(                       )`-.._
!!   >             ./'. '||\\.       (\_/)       .//||` .`\.
!!   >          ./'.|'.'||||\\|..    )o o(    ..|//||||`.`|.`\.
!!   >       ./'..|'.|| |||||\`````` '`"'` ''''''/||||| ||.`|..`\.
!!   >     ./'.||'.|||| ||||||||||||.     .|||||||||||| ||||.`||.`\.
!!   >    /'|||'.|||||| ||||||||||||{     }|||||||||||| ||||||.`|||`!!   >   '.|||'.||||||| ||||||||||||{     }|||||||||||| |||||||.`|||.`
!!   >  '.||| ||||||||| |/'   ``\||``     ''||/''   `\| ||||||||| |||.`
!!   >  |/' \./'     `\./         \!|\   /|!/         \./'     `\./ `\|
!!   >  V    V         V          }' `\ /' `{          V         V    V
!!   >  `    `         `               V               '         '    '
!!
!!  See the description of JOURNAL() for a meaning for the I/O flag.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!    program seepic
!!    use M_messages, only : junbat
!!       call junbat('s')
!!    end program seepic
!===================================================================================================================================
!>
!! PROCEDURE:   junbat(3f)
!! DESCRIPTION: print eye-catching ASCII graphic (bat)
!! AUTHOR:      John S. Urban
!!##VERSION:     1.0, 20130401
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine junbat(where)
use M_journal, only : journal
character(len=*),parameter :: ident="@(#)M_messages::junbat(3f):print eye-catching ASCII graphic (bat)"
character(len=*),intent(in)   :: where ! the I/O flag to pass on to journal()
call journal(where,"                      .-                    .-                       ")
call journal(where,"                 _..-'(                       )`-.._                 ")
call journal(where,"              ./'. '||\\.       (\_/)       .//||` .`\.              ")
call journal(where,"           ./'.|'.'||||\\|..    )o o(    ..|//||||`.`|.`\.           ")
call journal(where,"        ./'..|'.|| |||||\`````` '`v'` ''''''/||||| ||.`|..`\.        ")
call journal(where,"      ./'.||'.|||| ||||||||||||.     .|||||||||||| ||||.`||.`\.      ")
call journal(where,"     /'|||'.|||||| ||||||||||||{     }|||||||||||| ||||||.`|||`\     ")
call journal(where,"    '.|||'.||||||| ||||||||||||{     }|||||||||||| |||||||.`|||.`    ")
call journal(where,"   '.||| ||||||||| |/'   ``\||``     ''||/''   `\| ||||||||| |||.`   ")
call journal(where,"   |/' \./'     `\./         \!|\   /|!/         \./'     `\./ `\|   ")
call journal(where,"   V    V         V          }' `\ /' `{          V         V    V   ")
call journal(where,"   `    `         `               V               '         '    '   ")
end subroutine junbat
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      junbuster(3f) - [M_messages]call journal(3f) to print eye-catching ASCII graphic (ghostbuster)
!!
!!##SYNOPSIS
!!
!!
!!       SUBROUTINE junbuster(where)
!!
!!        character(len=*),intent(in) :: where
!!
!!##DESCRIPTION
!!
!!  Sample output:
!!
!!   >                        __---__
!!   >                     _-       _--______
!!   >                __--( /     \ )XXXXXXXXXXXXX_
!!   >              --XXX(   O   O  )XXXXXXXXXXXXXXX-
!!   >             /XXX(       U     )        XXXXXXX!!   >           /XXXXX(              )--_  XXXXXXXXXXX!!   >          /XXXXX/ (      O     )   XXXXXX   \XXXXX!!   >          XXXXX/   /            XXXXXX   \__ \XXXXX----
!!   >          XXXXXX__/          XXXXXX         \__----  -
!!   >  ---___  XXX__/          XXXXXX      \__         ---
!!   >    --  --__/   ___/\  XXXXXX            /  ___---=
!!   >      -_    ___/    XXXXXX              '--- XXXXXX
!!   >        --\/XXX\ XXXXXX                      /XXXXX
!!   >          \XXXXXXXXX                        /XXXXX/
!!   >           \XXXXXX                        _/XXXXX/
!!   >             \XXXXX--__/              __-- XXXX/
!!   >              --XXXXXXX---------------  XXXXX--
!!   >                 \XXXXXXXXXXXXXXXXXXXXXXXX-
!!   >                   --XXXXXXXXXXXXXXXXXX-
!!
!!
!!  See the description of JOURNAL() for a meaning for the I/O flag.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!    program seebuster
!!    use M_messages, only : junbuster
!!       call junbuster('s')
!!    end program seebuster
!===================================================================================================================================
!>
!! AUTHORS:     John S. Urban
!!##VERSION:     1.0, 20001130
!! DESCRIPTION: print eye-catching ASCII graphic (ghostbuster)
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine junbuster(where)
use M_journal, only : journal
implicit none
character(len=*),parameter :: ident="@(#)M_messages::junbuster(3f):print eye-catching ASCII graphic (ghostbuster)"
character(len=*),intent(in)   :: where
!   some compilers use \ as an escape character, so change \ to \\ if have problems
character(len=*),parameter :: picture(19)=[&
&'                       __---__                          ',&
&'                    _-       _--______                  ',&
&'               __--( /     \ )XXXXXXXXXXXXX_            ',&
&'             --XXX(   O   O  )XXXXXXXXXXXXXXX-          ',&
&'            /XXX(       U     )        XXXXXXX\         ',&
&'          /XXXXX(              )--_  XXXXXXXXXXX\       ',&
&'         /XXXXX/ (      O     )   XXXXXX   \XXXXX\      ',&
&'         XXXXX/   /            XXXXXX   \__ \XXXXX----  ',&
&'         XXXXXX__/          XXXXXX         \__----  -   ',&
&' ---___  XXX__/          XXXXXX      \__         ---    ',&
&'   --  --__/   ___/\  XXXXXX            /  ___---=      ',&
&'     -_    ___/    XXXXXX              ''--- XXXXXX      ',&
&'       --\/XXX\ XXXXXX                      /XXXXX      ',&
&'         \XXXXXXXXX                        /XXXXX/      ',&
&'          \XXXXXX                        _/XXXXX/       ',&
&'            \XXXXX--__/              __-- XXXX/         ',&
&'             --XXXXXXX---------------  XXXXX--          ',&
&'                \XXXXXXXXXXXXXXXXXXXXXXXX-              ',&
&'                  --XXXXXXXXXXXXXXXXXX-                 ']
integer                    :: i

do i=1,size(picture)
   call journal(where,picture(i))
end do

end subroutine junbuster
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      jundragon - [M_messages]fill in a character array with a message
!!
!!##SYNOPSIS
!!
!!
!!      SUBROUTINE jundragon(where,a)
!!
!!       character(len=*),intent(in) :: where
!!       character(len=32),intent(in) :: a(8)
!!
!!##DESCRIPTION
!!
!! Prints out a pretty message shaped like a dragon puffing smoke
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program seedragon
!!       character(len=32) :: a(8)
!!       a(1)='Puff, the magic dragon----------'
!!       a(2)='lived by the sea----------------'
!!       a(3)='and frolicked in the Autumn mist'
!!       a(4)='in a land called----------------'
!!       a(5)='Honiley-------------------------'
!!       a(6)='--------------------------------'
!!       a(7)='--------------------------------'
!!       a(8)='--------------------------------'
!!       call jundragon('s',a)
!!    end program seedragon
!!
!!
!!     >                 \=/,         _-===-_-====-_-===-_-==========-_-====-_
!!     >                |  @___oo   (  Puff, the magic dragon----------       )_
!!     >      /\  /\   / (___,,,}_--=  lived by the sea----------------       )
!!     >     ) /^\) ^\/ _)        =__ and frolicked in the Autumn mist       )
!!     >     )   /^\/   _)          (_  in a land called----------------      )
!!     >     )   _ /  / _)            (  Honiley-------------------------      )
!!     > /\  )/\/ ||  | )_)            (_ --------------------------------       )
!!     ><  >      |(,,) )__)             (  --------------------------------   )
!!     > ||      /    \)___)\             ( --------------------------------__)
!!     > | \____(      )___) )___           -==-_____-=====-_____-=====-___==
!!     >  \______(_______;;; __;;;
!===================================================================================================================================
!>
!! AUTHOR:      John S. Urban
!!##VERSION:     1.0, 20001130
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine jundragon(where,a)
use M_journal, only : journal
implicit none
character(len=*),parameter :: ident="@(#)M_messages::jundragon(3f):print eye-catching ASCII graphic (dragon) with message"
character(len=*),intent(in)   :: where ! the I/O flag to pass on to JUN()
   character(len=32)          :: a(8)  ! message of 8 32-character strings
   character(len=80)          :: b(11)
   integer                    :: i
   write(b(01),1)'                \=/,          _-===-_-====-_-===-_-==========-_-====-_'
   write(b(02),1)'                |  @___oo   (  ',a(01),'       )_'
   write(b(03),1)'      /\  /\   / (___,,,}_--=  ',a(02),'       )'
   write(b(04),1)'     ) /^\) ^\/ _)        =__ ',a(03),'       )'
   write(b(05),1)'     )   /^\/   _)          (_  ',a(04),'      )'
   write(b(06),1)'     )   _ /  / _)            (  ',a(05),'      )'
   write(b(07),1)' /\  )/\/ ||  | )_)            (_ ',a(06),'      )'
   write(b(08),1)'<  >      |(,,) )__)             (  ',a(07),'   )'
   write(b(09),1)' ||      /    \)___)\             ( ',a(08),'__)'
   write(b(10),1)' | \____(      )___) )___           -==-_____-=====-_____-=====-___=='
   write(b(11),*)' \______(_______;;; __;;;'
1  format(a,a,a)
   do i=1,11
      call journal(where,b(i))
   enddo
end subroutine jundragon
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      junroach(3f) - [M_messages]print an eye-catching image of a roach.
!!
!!##SYNOPSIS
!!
!!    SUBROUTINE junroach(where)
!!
!!     integer,intent(in) :: where
!!
!!##DESCRIPTION
!!    Prints an eye-catching image of a roach. Typically used to mark
!!    an error has occurred in an output file.
!!
!!##OPTIONS
!!    where  the I/O flag to pass on to JOURNAL().
!!           To write to stdout use 's'.
!!           See the description of JOURNAL() for a meaning for the I/O flag.
!!
!!##EXAMPLE
!!
!!    Typical Usage:
!!
!!      program demo_junroach
!!      use M_messages, only : junroach
!!      implicit none
!!      logical :: error=.true.
!!         if (error)then
!!            write(*,*)'ERROR:'
!!            call junroach('s')
!!            write(*,*)'   explanation of error.'
!!         endif
!!      end program demo_junroach
!!
!!    Results:
!!
!!     >ERROR:
!!     >      ,--.     .--.
!!     >     /    \. ./    !!     >    /  /\ / " \ /\  !!     >   / _/  {~~v~~}  \_ !!     >  /     {   |   }     !!     > ;   /\{    |    }/\   !!     > | _/  {    |    }  \_  :
!!     > |     {    |    }      |
!!     > |    /{    |    }\     |
!!     > |   / {    |    } \    |
!!     > |  /  {    |    }  \   |
!!     > |  \  \    |    /  /   |
!!     > |   \  \   |   /  /    |
!!     >  \   \  \  |  /  /    /
!!     >   \  /   ~~~~~   \   /
!!     >   explanation of error.
!===================================================================================================================================
!>
!! DESCRIPTION:  junroach(3f):print eye-catching ASCII graphic (roach)"
!! REFERENCES:   none
!! DEPENDENCIES: none
!! RESTRICTIONS: none
!!##QA:
!! AUTHOR:       John S. Urban
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine junroach(where)
use M_journal, only: journal
character(len=*),parameter :: ident="@(#)M_messages::junroach(3f):print eye-catching ASCII graphic (roach)"
character(len=*),intent(in)   :: where  ! the I/O flag to pass on to JOURNAL()
call journal(where,'       ,--.     .--.        ')
call journal(where,'      /    \. ./    \       ')
call journal(where,'     /  /\ / " \ /\  \      ')
call journal(where,'    / _/  {~~v~~}  \_ \     ')
call journal(where,'   /     {   |   }     \    ')
call journal(where,'  ;   /\{    |    }/\   \   ')
call journal(where,'  | _/  {    |    }  \_  :  ')
call journal(where,'  |     {    |    }      |  ')
call journal(where,'  |    /{    |    }\     |  ')
call journal(where,'  |   / {    |    } \    |  ')
call journal(where,'  |  /  {    |    }  \   |  ')
call journal(where,'  |  \  \    |    /  /   |  ')
call journal(where,'  |   \  \   |   /  /    |  ')
call journal(where,'   \   \  \  |  /  /    /   ')
call journal(where,'    \  /   ~~~~~   \   /    ')
end subroutine junroach
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      junsun(3f) - [M_messages]print an eye-catching image of a smiling sun.
!!
!!##SYNOPSIS
!!
!!
!!       SUBROUTINE junsun(where)
!!
!!##DESCRIPTION
!!  Sample output:
!!
!!   ####################                  #####################
!!   ######################                ###################
!!   ########################              #################
!!   ##########################            ###############                      ##
!!      #########################                 ######                   #######
!!           ######################   ###########   ##                 ###########
!!                ###############  #################              ################
!!                     ########  #####################       #####################
!!                          ##  #######################  #########################
!!                             #####     ###     #######  ########################
!!                            #####   ##  #   ##  #######  #######################
!!   #######################  ########### ###############  #######################
!!   #######################  ######## #   ## ###########
!!   ########################  #####  ########   #######
!!   #########################  ### ##        ### #####  ##
!!   #####################       #####################  ########
!!   ################              #####   #########  ###############
!!   ###########                 ##   ###########   ######################
!!   ######                    ######                 #########################
!!   #                       ###############            ##########################
!!                         #################              ########################
!!                       ###################                ######################
!!                     #####################                  ####################
!!
!!##OPTIONS
!!
!!       where    the I/O flag to pass on to JOURNAL().
!!                See the description of JOURNAL() for a meaning for the I/O flag.
!!
!!##EXAMPLE
!!
!!  Sample program:
!!
!!    program seepic
!!    use M_messages, only : junsun
!!       call junsun('s')
!!    end program seepic
!===================================================================================================================================
!>
!! AUTHOR:     John S. Urban
!!##VERSION:    1.0, 20130401
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine junsun(where)
use M_journal, only: journal
character(len=*),parameter :: ident="@(#)M_messages::junsun(3f):print eye-catching ASCII graphic (sunshine)"
character(len=*),intent(in)   :: where
call journal(where,'#####################                  #####################                   ')
call journal(where,'#######################                ###################                     ')
call journal(where,'#########################              #################                       ')
call journal(where,'###########################            ###############                      ## ')
call journal(where,'    #########################                 ######                   ####### ')
call journal(where,'         ######################   ###########   ##                 ############')
call journal(where,'              ###############  #################              #################')
call journal(where,'                   ########  #####################       ######################')
call journal(where,'                        ##  #######################  ##########################')
call journal(where,'                           #####     ###     #######  #########################')
call journal(where,'                          #####   ##  #   ##  #######  ########################')
call journal(where,'########################  ########### ###############  ########################')
call journal(where,'########################  ######## #   ## ###########                          ')
call journal(where,'#########################  #####  ########   #######                           ')
call journal(where,'##########################  ### ##        ### #####  ##                        ')
call journal(where,'######################       #####################  ########                   ')
call journal(where,'#################              #####   #########  ###############              ')
call journal(where,'############                 ##   ###########   ######################         ')
call journal(where,'#######                    ######                 #########################    ')
call journal(where,'##                       ###############            ###########################')
call journal(where,'                       #################              #########################')
call journal(where,'                     ###################                #######################')
call journal(where,'                   #####################                  #####################')
end subroutine junsun
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!      juntrolls(3f) - [M_messages]print an eye-catching bulletin
!!
!!##SYNOPSIS/USAGE
!!
!!      SUBROUTINE juntrolls(where,a)
!!
!!       character(len=*)  :: where  ! the I/O flag to pass on to JOURNAL()
!!       character(len=21) :: a(4)
!!
!!##DESCRIPTION
!!  Sample output:
!!
!!     >          \|||/
!!     >          (o o)
!!     > ;~~~~ooO~~(_)~~~~~~~~~;
!!     > |                     |
!!     > |                     |
!!     > |                     |
!!     > |_____________________|
!!     > '~~~~~~~~~~~~~~~~Ooo~~'
!!     >         |  |  |
!!     >         ~~~ ~~~
!!     >          || ||
!!     >         ooO Ooo
!!
!!  See the description of JOURNAL() for a meaning for the I/O flag.
!!
!!##EXAMPLE
!!
!!
!!  Sample program:
!!
!!    program seepic
!!    use M_messages, only : juntrolls
!!       call juntrolls('s',[         &
!!          'Please ...           ',  &
!!          "   don't feed        ",  &
!!          '   the               ',  &
!!          '   TROLLS!           '   &
!!          ])
!!    end program seepic
!===================================================================================================================================
!>
!! DESCRIPTION: print eye-catching ASCII graphic (trolls) with message
!! AUTHOR:      John S. Urban
!!##VERSION:     1,0, 201130401
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine juntrolls(where,a)
use M_journal, only: journal
implicit none
character(len=*),parameter :: ident="@(#)M_messages::juntrolls(3f):print eye-catching ASCII graphic (trolls) with message"
character(len=*),intent(in)      :: where
character(len=*),intent(in)      :: a(*)
integer                          :: i
character(len=25),save           :: sign,trolls(12)=[&
'          \|||/          ', &
'          (o o)          ', &
' ;~~~~ooO~~(_)~~~~~~~~~; ', &
' |XXXXXXXXXXXXXXXXXXXXX| ', &
' |XXXXXXXXXXXXXXXXXXXXX| ', &
' |XXXXXXXXXXXXXXXXXXXXX| ', &
' |XXXXXXXXXXXXXXXXXXXXX| ', &
" '~~~~~~~~~~~~~~~~Ooo~~' ", &
'         |  |  |         ', &
'         ~~~ ~~~         ', &
'          || ||          ', &
'         ooO Ooo         ']

do i=1,3
   call journal(where,trolls(i))
enddo

!do i=1,ubound(a,dim=1)
do i=1,4
   sign=trolls(4)
   sign(3:23)=a(i)
   call journal(where,sign)
enddo

do i=8,size(trolls)
   call journal(where,trolls(i))
enddo

end subroutine juntrolls
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================

!>
!!##NAME
!!      tabgraph(3f) - [M_messages]write out a row of numbers and a text-based scaled graph
!!
!!##SYNOPSIS
!!
!!       SUBROUTINE tabgraph(ctmp,array,ivals,rmin,rmax,cfill,ilen)
!!
!!##DESCRIPTION
!!
!!    Given a max and min value for setting up a range
!!    write out a series of real numbers with a text-based graph scale appended.
!!    Each column of numbers is written with a format of "1x,g14.7", taking up
!!    15 characters per number.
!!
!!    Given an array of numbers in ARRAY(IVALS),
!!
!!      1. write the numbers into string CTMP with a scale appended to the right
!!         that depicts the relative ranges of the numbers.
!!      2. RMIN and RMAX are the minimum and maximum numbers to determine
!!         where ARRAY(2) thru ARRAY(IVALS) go on the scale graph.
!!         ARRAY(1) is printed but is not plotted unless IVALS is less than or
!!         equal to one (1), on the assumption that it is the X values and the
!!         other columns are Y values.
!!         ABS(IVALS) is the number of numbers in array. Negative numbers flag
!!         that ARRAY(1) should be plotted too.
!!      3. CFILL is the character to fill the scale with as a background
!!         character
!!      4. ILEN is the number of characters to use for the scale region.
!!         If set to 0, it pads out to 132 columns unless the scale would be
!!         less than 15 characters wide; then it pads out to 255 characters
!!
!!    Each number takes up 15 columns on output.
!!
!!    Could stand being made more generic and/or robust. Maybe label max
!!    and min values, draw a vertical line at 0 or some reference value,
!!    check if I/O errors occur many times and stop putting out message
!!    (some platforms probably won't like really wide lines), and put a
!!    marker for values that should be off scale (and so on).
!!
!!    It is assumed that from this, you can easy make a routine that, given
!!    all of curves at once, finds max and min and loops calling tabgraph.
!!
!!##OPTIONS
!!
!!
!!      CC        is the string to fill and return with the "graph"
!!
!!      ARRAY     is the list of numbers to put into the graph
!!
!!      IVALUES0  is the number of numbers in array; if < 2 then take abs(ivalues0)
!!      and show first column. Else assume first column is X values and should not
!!      be added to scale.
!!
!!      RMIN   is the min value for the scale.
!!      RMAX   is the max value for the scale.
!!      FILL   is the character to fill the scale with as a background.
!!      ILEN0  is the width of the scale in characters.
!!             0=> fit into line of 132 characters unless scale becomes narrower
!!             than ICMIN characters, then fit into 255 character line.)
!!
!!##EXAMPLE
!!
!!
!!   Sample program:
!!
!!        program testit
!!        character*500 ctmp
!!        real array(4)
!!        do 20 i20=1,4
!!        do 10 i10=1,400
!!           array(1)=i10*12/100.0
!!           array(2)=sin(array(1))
!!           array(3)=cos(array(1))
!!           if(i20.eq.1)then
!!              ! fixed width of 50 for scale
!!              call tabgraph(ctmp,array,3,-1.0,1.0,' ',50)
!!              ! ctmp  --> CTMP string to fill
!!              ! array --> ARRAY data
!!              ! 3     --> IVALS
!!              !-1     --> RMIN
!!              ! 1     --> RMAX
!!              !' '    --> CFILL
!!              !50     --> ILEN
!!           elseif(i20.eq.2)then
!!              ! fixed width of 90 for scale with a non-blank fill character
!!              call tabgraph(ctmp,array,3,-1.0,1.0,'.',90)
!!           elseif(i20.eq.3)then
!!              ! 0 len auto-sizes scale region
!!              call tabgraph(ctmp,array,3,-1.0,1.0,' ',0)
!!           elseif(i20.eq.4)then
!!             ! number of values less than or equal to 1
!!             call tabgraph(ctmp,array,1,0.0,48.0,' ',0)
!!           endif
!!           if(i10.eq.1)then
!!              ilen=len_trim(ctmp)
!!              ilen=max(ilen,1)
!!           endif
!!           write(*,'(a)')ctmp(1:ilen) ! tabgraph test program
!!           ! write(*,'(i5,a)')i10,ctmp(1:ilen) write with a number count
!!        10     continue
!!        20     continue
!!        end program testit
!!
!!  The purpose of this routine becomes much clearer when looking at a sample
!!  output. The third pass thru loop 20 in the test code above will produce:
!!
!!
!!   0.1200000      0.1197122      0.9928086    #                                               1                                    2#
!!   0.2400000      0.2377026      0.9713380    #                                                    1                              2 #
!!   0.3600000      0.3522742      0.9358968    #                                                        1                        2   #
!!   0.4800000      0.4617792      0.8869949    #                                                             1                 2     #
!!   0.6000000      0.5646425      0.8253356    #                                                                 1           2       #
!!   0.7200000      0.6593847      0.7518057    #                                                                      1  2           #
!!   0.8400000      0.7446431      0.6674628    #                                                                      2  1           #
!!   0.9600000      0.8191916      0.5735200    #                                                                  2         1        #
!!    1.080000      0.8819578      0.4713283    #                                                              2                1     #
!!    1.200000      0.9320391      0.3623577    #                                                         2                       1   #
!!    1.320000      0.9687151      0.2481754    #                                                    2                              1 #
!!    1.440000      0.9914584      0.1304237    #                                               2                                    1#
!!    1.560000      0.9999417      0.1079617E-01#                                          2                                         1#
!!    1.680000      0.9940432     -0.1089867    #                                     2                                              1#
!!    1.800000      0.9738476     -0.2272020    #                                2                                                  1 #
!!    1.920000      0.9396455     -0.3421496    #                           2                                                     1   #
!!    2.040000      0.8919287     -0.4521761    #                      2                                                        1     #
!!    2.160000      0.8313834     -0.5556992    #                  2                                                          1       #
!!    2.280000      0.7588807     -0.6512296    #              2                                                           1          #
!!    2.400000      0.6754631     -0.7373938    #          2                                                           1              #
!!    2.520000      0.5823306     -0.8129520    #       2                                                          1                  #
!!    2.640000      0.4808225     -0.8768179    #    2                                                         1                      #
!!    2.760000      0.3723991     -0.9280727    #  2                                                      1                           #
!!    2.880000      0.2586192     -0.9659793    #2                                                   1                                #
!!    3.000000      0.1411200     -0.9899925    #2                                              1                                     #
!!    3.120000      0.2159109E-01 -0.9997669    #2                                         1                                          #
!!    3.240000     -0.9824860E-01 -0.9951619    #2                                    1                                               #
!!    3.360000     -0.2166750     -0.9762438    #2                               1                                                    #
!!    3.480000     -0.3319852     -0.9432846    # 2                         1                                                         #
!!    3.600000     -0.4425204     -0.8967584    #   2                   1                                                             #
!!    3.720000     -0.5466911     -0.8373344    #      2           1                                                                  #
!!    3.840000     -0.6429987     -0.7658673    #         2    1                                                                      #
!!    3.960000     -0.7300584     -0.6833848    #          1 2                                                                        #
!!    4.080000     -0.8066177     -0.5910735    #       1        2                                                                    #
!!    4.200000     -0.8715757     -0.4902610    #    1                2                                                               #
!!    4.320000     -0.9239982     -0.3823968    #  1                      2                                                           #
!!    4.440000     -0.9631310     -0.2690330    # 1                            2                                                      #
!!    4.560000     -0.9884112     -0.1517999    #1                                  2                                                 #
!!    4.680000     -0.9994755     -0.3238349E-01#1                                       2                                            #
!!    4.800000     -0.9961646      0.8749917E-01#1                                            2                                       #
!!    4.920000     -0.9785261      0.2061229    #1                                                 2                                  #
!!    5.040000     -0.9468138      0.3217820    # 1                                                     2                             #
!!    5.160000     -0.9014837      0.4328130    #   1                                                        2                        #
!!    5.280000     -0.8431876      0.5376194    #      1                                                         2                    #
!!    5.400000     -0.7727644      0.6346930    #         1                                                          2                #
!!    5.520000     -0.6912268      0.7226379    #            1                                                           2            #
!!    5.640000     -0.5997474      0.8001894    #                1                                                           2        #
!!    5.760000     -0.4996417      0.8662322    #                    1                                                         2      #
!!    5.880000     -0.3923501      0.9198160    #                         1                                                       2   #
!!    6.000000     -0.2794155      0.9601703    #                              1                                                   2  #
!!    6.120000     -0.1624621      0.9867148    #                                   1                                               2 #
!!    6.240000     -0.4317211E-01  0.9990677    #                                        1                                           2#
!!    6.360000      0.7673930E-01  0.9970512    #                                             1                                      2#
!!    6.480000      0.1955465      0.9806944    #                                                  1                                2 #
!!    6.600000      0.3115413      0.9502326    #                                                       1                          2  #
!!    6.720000      0.4230552      0.9061039    #                                                           1                    2    #
!!    6.840000      0.5284849      0.8489427    #                                                                1             2      #
!!    6.960000      0.6263130      0.7795717    #                                                                    1      2         #
!!    7.080000      0.7151332      0.6989882    #                                                                       21            #
!!    7.200000      0.7936677      0.6083515    #                                                                   2       1         #
!!    7.320000      0.8607875      0.5089645    #                                                               2              1      #
!!    7.440000      0.9155264      0.4022577    #                                                           2                    1    #
!!    7.560000      0.9570977      0.2897654    #                                                      2                           1  #
!!    7.680000      0.9849033      0.1731054    #                                                 2                                 1 #
!!    7.800000      0.9985434      0.5395523E-01#                                            2                                       1#
!!    7.920000      0.9978216     -0.6597050E-01#                                       2                                            1#
!!    8.040000      0.9827484     -0.1849474    #                                  2                                                1 #
!!    8.160000      0.9535407     -0.3012642    #                             2                                                    1  #
!!    8.280000      0.9106185     -0.4132481    #                        2                                                       1    #
!!    8.400000      0.8545991     -0.5192883    #                   2                                                          1      #
!!    8.520000      0.7862877     -0.6178606    #               2                                                           1         #
!!    8.640000      0.7066678     -0.7075455    #           2                                                            1            #
!!       :
!!       :
!!       :
!!
!!##NOTES
!!
!! TABGRAPH makes it very easy to find values in particular ranges in printed output.
!! works particularly well with the once ubiquitous fan-fold paper.
!===================================================================================================================================
!>
!! PRODUCT:            CLI library utilities and examples
!! PROCEDURE:          tabgraph(3f)
!! DESCRIPTION:        write columns of numbers with a text scale to the right"
!!##VERSION:            1.0, 20150508
!! AUTHOR:             John S. Urban
!! REPORTING BUGS:     http://www.urbanjost.altervista.org/
!! HOME PAGE:          http://www.urbanjost.altervista.org/index.html
!! LICENSE:            Public Domain. This is free software: you are free to change and redistribute it.
!!                     There is NO WARRANTY, to the extent permitted by law.
!! LANGUAGE:           Fortran
!! REFERENCES:         none
!! DEPENDENCIES:       journal(3f)
!! LEGAL RESTRICTIONS: none
!! QA:                 Test program in source
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine tabgraph(cc,array,ivalues0,rmin,rmax,fill,ilen0)
      use M_journal, only : journal
      character(len=*),parameter :: ident="@(#)M_messages::tabgraph(3f): write columns of numbers with a text scale to the right"
      character*(*) cc ! assumed big enough
      real array(*)
      character yvalue*1000, format*80
      character*1 fill
      parameter(icmin=15)
      delta=rmax-rmin
      if(delta.eq.0)then
        rmin=-1000.0
        rmax=10000.0
        call journal('*tabgraph* bad range')
      endif
      ilet=49
      ivalues=abs(ivalues0)
      !================================================================--------
      ! if ilen0 = 0, make fit in 132 column output
      if(ilen0.le.1)then
         ! 15 characters per number, 2 border characters
         ilen=132-15*ivalues-2
         ! if 132 comes out too poor, try for 255-column output
         if(ilen.le.ICMIN)then    ! ICMIN is totally arbitrary
            ilen=255-15*ivalues-2
         endif
         if(ilen.le.ICMIN)then
            ilen=20
         endif
      else
         ilen=ilen0
      endif
      !================================================================--------
      if(ilen+2+15*ivalues.gt.len(cc))then
         call journal('*tabgraph* string too small')
      endif
      !================================================================--------
      write(format,101)ivalues
101   format("(",i10,"(1x,g14.7)1h#,a,1h#)")
      !================================================================--------
      if(fill.eq.' ')then
         yvalue=' '
      else
         write(yvalue,'(1000a1)')(fill,i=1,min(1000,ilen))
      endif
      !================================================================--------
      if(ivalues0.lt.2)then
         istart=1
      else
         istart=2
      endif
      do i10=istart,ivalues
         ipos=ilen*(array(i10)-rmin)/delta+0.5
         ipos=min(ilen,ipos)
         ipos=max(1,ipos)
         yvalue(ipos:ipos)=char(ilet)
         ilet=ilet+1
         if(ilet.gt.127)ilet=127
      enddo
      !================================================================--------
      write(cc,fmt=format,err=999)(array(i),i=1,ivalues),yvalue(1:ilen)
      return
      !================================================================--------
999   continue
      call journal('*tabgraph* write error')
end subroutine tabgraph
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================

!>
!!##NAME
!!    blocks(1f) - [M_messages]write out 132-character string in large block letters
!!
!!##SYNOPSIS
!!
!!    subroutine blocks(string,iounit)
!!
!!     character(len=132)  ::  string
!!     integer iounit
!!
!!##DESCRIPTION
!!    Given a string up to 132 characters long, BLOCKS() writes out the string
!!    left-justified in large block letters 10 lines tall between columns 2 and
!!    131 for a string up to 10 characters.
!!
!!    This can be used to make banners in program output files; it is also handy
!!    for making attention-catching notices in interactive programs. The routine
!!    has been used to make large delivery banners on fixed-type printers and to
!!    make eye-readable headers on microfiche.
!!
!!    If the output unit number is negative, JOURNAL() is called instead
!!    of writing to a unit
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo
!!       call blocks('NOTICE',6)
!!    end program demo
!!
!!    would produce:
!!
!!     >nn      nn    oooooooo    tttttttttt   iiiiiiiiii    cccccccc    eeeeeeeeee
!!     >nnn     nn   oooooooooo   tttttttttt   iiiiiiiiii   cccccccccc   eeeeeeeeee
!!     >nnnn    nn   oo     ooo       tt           ii       cc       c   ee
!!     >nn nn   nn   oo    o oo       tt           ii       cc           ee
!!     >nn  nn  nn   oo   o  oo       tt           ii       cc           eeeee
!!     >nn  nn  nn   oo  o   oo       tt           ii       cc           eeeee
!!     >nn   nn nn   oo o    oo       tt           ii       cc           ee
!!     >nn    nnnn   ooo     oo       tt           ii       cc       c   ee
!!     >nn     nnn   oooooooooo       tt       iiiiiiiiii   cccccccccc   eeeeeeeeee
!!     >nn      nn    oooooooo        tt       iiiiiiiiii    cccccccc    eeeeeeeeee
!===================================================================================================================================
!>
!! PROGRAM:     blocks(1F)
!! DESCRIPTION: print alphabet in big block letters using blocks(3f)
!! AUTHOR:      John S. Urban
!!##VERSION:     2.0, 20160624
!! COPYRIGHT:   Copyright (c) 1984, 1996 John S. Urban
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!subroutine blocks(str,iout,filler,char)
subroutine blocks(str,iout)
!     Things to do:
!      o  make one that writes into a character array instead of to a file
!      o  make an option to print a bitmap-like string for use by atobm(1)
!      o  if bothered to order data in ASCII collating sequence could use ichar(letter) instead of slower index(string,key)
!===================================================================================================================================
use M_journal, only : journal
implicit none
character(len=*),parameter      :: ident="@(#)M_messages::blocks(3f): write out 132-character string in large block letters"
character(len=*),intent(in)     :: str
integer,intent(in)              :: iout
!character,intent(in),optional  :: filler
!character,intent(in),optional  :: char
!character                      :: filler_local
!character                      :: char_local
   character(len=12)            :: alf(10,95)
   integer,parameter            :: maxchar=132
   character(len=maxchar*12+1)  :: line
   integer                      :: l(maxchar)
   integer                      :: ilet,irow,ii,kk,ip,lstr,mm
   save
      data ((alf(irow,ilet),ilet=1,5),irow=1,10) /                      &
     &' AAAAAAAA ','BBBBBBBBB ',' CCCCCCCC ','DDDDDDDDD ','EEEEEEEEEE', &
     &'AAAAAAAAAA','BBBBBBBBBB','CCCCCCCCCC','DDDDDDDDDD','EEEEEEEEEE', &
     &'AA      AA','BB      BB','CC       C','DD      DD','EE        ', &
     &'AA      AA','BB      BB','CC        ','DD      DD','EE        ', &
     &'AA      AA','BBBBBBBBB ','CC        ','DD      DD','EEEEE     ', &
     &'AAAAAAAAAA','BBBBBBBBB ','CC        ','DD      DD','EEEEE     ', &
     &'AAAAAAAAAA','BB      BB','CC        ','DD      DD','EE        ', &
     &'AA      AA','BB      BB','CC       C','DD      DD','EE        ', &
     &'AA      AA','BBBBBBBBBB','CCCCCCCCCC','DDDDDDDDDD','EEEEEEEEEE', &
     &'AA      AA','BBBBBBBBB ',' CCCCCCCC ','DDDDDDDDD ','EEEEEEEEEE'/
      DATA ((ALF(IROW,ILET),ILET=6,10),IROW=1,10) /                     &
     &'FFFFFFFFFF',' GGGGGGGG ','HH      HH','IIIIIIIIII','  JJJJJJJJ', &
     &'FFFFFFFFFF','GGGGGGGGGG','HH      HH','IIIIIIIIII','  JJJJJJJJ', &
     &'FF        ','GG        ','HH      HH','    II    ','     JJ   ', &
     &'FF        ','GG        ','HH      HH','    II    ','     JJ   ', &
     &'FFFFF     ','GG   GGGGG','HHHHHHHHHH','    II    ','     JJ   ', &
     &'FFFFF     ','GG   GGGGG','HHHHHHHHHH','    II    ','     JJ   ', &
     &'FF        ','GG      GG','HH      HH','    II    ','JJ   JJ   ', &
     &'FF        ','GG      GG','HH      HH','    II    ','JJ   JJ   ', &
     &'FF        ','GGGGGGGGGG','HH      HH','IIIIIIIIII','JJJJJJJ   ', &
     &'FF        ',' GGGGGGGGG','HH      HH','IIIIIIIIII',' JJJJJ    '/
      data ((alf(irow,ilet),ilet=11,15),irow=1,10) /                    &
     &'KK      KK','LL        ','MM      MM','NN      NN',' OOOOOOOO ', &
     &'KK     KK ','LL        ','MMMM  MMMM','NNN     NN','OOOOOOOOOO', &
     &'KK   KK   ','LL        ','MM MMMM MM','NNNN    NN','OO     OOO', &
     &'KK KK     ','LL        ','MM  MM  MM','NN NN   NN','OO    O OO', &
     &'KKKKK     ','LL        ','MM  MM  MM','NN  NN  NN','OO   O  OO', &
     &'KK  KK    ','LL        ','MM      MM','NN  NN  NN','OO  O   OO', &
     &'KK   KK   ','LL        ','MM      MM','NN   NN NN','OO O    OO', &
     &'KK    KK  ','LL        ','MM      MM','NN    NNNN','OOO     OO', &
     &'KK     KK ','LLLLLLLLLL','MM      MM','NN     NNN','OOOOOOOOOO', &
     &'KK      KK','LLLLLLLLLL','MM      MM','NN      NN',' OOOOOOOO '/
      data ((alf(irow,ilet),ilet=16,20),irow=1,10) /                    &
     &'PPPPPPPPP ',' QQQQQQQQ ','RRRRRRRRR ',' SSSSSSSS ','TTTTTTTTTT', &
     &'PPPPPPPPPP','QQQQQQQQQQ','RRRRRRRRRR','SSSSSSSSSS','TTTTTTTTTT', &
     &'PP      PP','QQ      QQ','RR      RR','SS       S','    TT    ', &
     &'PP      PP','QQ      QQ','RR      RR','SS        ','    TT    ', &
     &'PPPPPPPPPP','QQ      QQ','RRRRRRRRRR','SSSSSSSSS ','    TT    ', &
     &'PPPPPPPPP ','QQ      QQ','RRRRRRRRR ',' SSSSSSSSS','    TT    ', &
     &'PP        ','QQ   QQ QQ','RR   RR   ','        SS','    TT    ', &
     &'PP        ','QQ    QQQQ','RR    RR  ','S       SS','    TT    ', &
     &'PP        ','QQQQQQQQQ ','RR     RR ','SSSSSSSSSS','    TT    ', &
     &'PP        ',' QQQQQQ QQ','RR      RR',' SSSSSSSS ','    TT    '/
      data ((alf(irow,ilet),ilet=21,25),irow=1,10) /                    &
     &'UU      UU','VV      VV','WW      WW','XX      XX','YY      YY', &
     &'UU      UU','VV      VV','WW      WW',' XX    XX ',' YY    YY ', &
     &'UU      UU',' VV    VV ','WW      WW','  XX  XX  ','  YY  YY  ', &
     &'UU      UU',' VV    VV ','WW      WW','   XXXX   ','   YYYY   ', &
     &'UU      UU',' VV    VV ','WW  WW  WW','    XX    ','    YY    ', &
     &'UU      UU',' VV    VV ','WW  WW  WW','   XXXX   ','    YY    ', &
     &'UU      UU','  VV  VV  ','WW  WW  WW','  XX  XX  ','    YY    ', &
     &'UU      UU','  VV  VV  ','WW WWWW WW',' XX    XX ','    YY    ', &
     &'UUUUUUUUUU','   VVVV   ',' WWW  WWW ','XX      XX','    YY    ', &
     &' UUUUUUUU ','    VV    ',' WW    WW ','XX      XX','    YY    '/
      data ((alf(irow,ilet),ilet=26,30),irow=1,10) /                    &
     &'ZZZZZZZZZZ','   0000   ','    11    ',' 22222222 ','3333333333', &
     &'ZZZZZZZZZ ','  000000  ','  1111    ','2222222222','333333333 ', &
     &'      ZZ  ',' 00    00 ',' 11 11    ','2       22','      33  ', &
     &'     ZZ   ','00      00','    11    ','        22','     33   ', &
     &'    ZZ    ','00      00','    11    ','       222','    333   ', &
     &'    ZZ    ','00      00','    11    ','     222  ','      333 ', &
     &'   ZZ     ','00      00','    11    ','   222    ','       33 ', &
     &'  ZZ      ',' 00    00 ','    11    ','  222     ','3       33', &
     &' ZZZZZZZZZ','  000000  ','1111111111','2222222222','3333333333', &
     &'ZZZZZZZZZZ','   0000   ','1111111111','2222222222',' 33333333 '/
      data ((alf(irow,ilet),ilet=31,35),irow=1,10) /                    &
     &'     444  ','5555555555',' 66666666 ','          ','    $$    ', &
     &'    4444  ','5555555555','6666666666','  @@@@@@  ',' $$$$$$$$ ', &
     &'   44 44  ','55        ','66       6',' @@   @@@ ','$$$$$$$$$$', &
     &'  44  44  ','55        ','66        ','       @@ ','$$  $$    ', &
     &' 44   44  ','555555555 ','666666666 ','   @@@@@@ ','$$$$$$$$$ ', &
     &'4444444444','5555555555','6666666666','  @@   @@ ',' $$$$$$$$$', &
     &'4444444444','        55','66      66',' @@    @@ ','    $$  $$', &
     &'      44  ','5       55','66      66',' @@    @@ ','$$$$$$$$$$', &
     &'      44  ','5555555555','6666666666','  @@@@@@  ',' $$$$$$$$ ', &
     &'      44  ',' 55555555 ',' 66666666 ','          ','    $$    '/
      data ((alf(irow,ilet),ilet=36,40),irow=1,10) /                    &
     &' %%%    %%','  &&&&    ','          ','       (  ','  )       ', &
     &'%% %%  %% ',' &&  &&   ','          ','     ((   ','   ))     ', &
     &'%% %% %%  ',' &&  &&   ',' *  *  *  ','    ((    ','    ))    ', &
     &' %%% %%   ','  && &&   ','  * * *   ','   ((     ','     ))   ', &
     &'    %%    ','   &&&    ','   ***    ','   ((     ','     ))   ', &
     &'   %%     ','  && &&   ','********* ','   ((     ','     ))   ', &
     &'  %%  %%% ',' &&   && &','   ***    ','   ((     ','     ))   ', &
     &' %%  %% %%','&&     &&&','  * * *   ','    ((    ','    ))    ', &
     &'%%   %% %%','&&     && ',' *  *  *  ','     ((   ','   ))     ', &
     &'%     %%% ',' &&&&&& &&','          ','       (  ','  )       '/
      data ((alf(irow,ilet),ilet=41,44),irow=1,10) /       &
     &'          ','          ','          ','          ', &
     &'          ','          ','          ','   ++++   ', &
     &'          ','          ','          ','   ++++   ', &
     &'          ','          ','==========','   ++++   ', &
     &'----------','          ','==========','++++++++++', &
     &'----------','          ','          ','++++++++++', &
     &'          ','          ','==========','   ++++   ', &
     &'          ','          ','==========','   ++++   ', &
     &'          ','__________','          ','   ++++   ', &
     &'          ','__________','          ','          '/
! By himself so can expand \ to \\ and not go past column 72; some compilers now treat \ as an escape
! character, Linux is the only one so far that does not have an option to turn that off.
      data ((alf(irow,ilet),ilet=45,45),irow=1,10) / &
     &'\         ', &
     &'\\        ', &
     &' \\       ', &
     &'  \\      ', &
     &'   \\     ', &
     &'    \\    ', &
     &'     \\   ', &
     &'      \\  ', &
     &'       \\ ', &
     &'        \ '/
      data ((alf(irow,ilet),ilet=46,50),irow=1,10) /                    &
     &'    ]]]]] ','   [[[[[  ','          ','         ','           ', &
     &'       ]] ','   [[     ',' >>       ','         ','         <<', &
     &'       ]] ','   [[     ','   >>     ','         ','       <<  ', &
     &'       ]] ','   [[     ','     >>   ','         ','     <<    ', &
     &'       ]] ','   [[     ','       >> ','         ','   <<      ', &
     &'       ]] ','   [[     ','         >>','        ',' <<        ', &
     &'       ]] ','   [[     ','       >> ','         ','   <<      ', &
     &'       ]] ','   [[     ','     >>   ','   ...   ','     <<    ', &
     &'       ]] ','   [[     ','   >>     ','  .....  ','       <<  ', &
     &'    ]]]]] ','   [[[[[  ',' >>       ','   ...   ','         <<'/
      data ((alf(irow,ilet),ilet=51,55),irow=1,10) /                    &
     &'          ','  ?????   ','         /','    !!    ','          ', &
     &'          ',' ???????  ','        //','    !!    ','    ;;;   ', &
     &'          ','??     ?? ','       // ','    !!    ','   ;;;;;  ', &
     &'          ','       ?? ','      //  ','    !!    ','    ;;;   ', &
     &'          ','      ??  ','     //   ','    !!    ','          ', &
     &'   ,,,    ','     ??   ','    //    ','    !!    ','    ;;;   ', &
     &'  ,,,,,   ','   ??     ','   //     ','    !!    ','   ;;;;;  ', &
     &'   ,,,,   ','   ??     ','  //      ','    !!    ','    ;;;;  ', &
     &'     ,    ','          ',' //       ','          ','       ;  ', &
     &'    ,     ','   ??     ',' /        ','    !!    ','      ;   '/
!    NB: takes two single quotes to make a quote
      data ((alf(irow,ilet),ilet=56,60),irow=1,10) /                    &
     &'    ''''''','  ""  ""  ','          ','          ','          ', &
     &'    ''''''','  ""  ""  ','    :::   ','  ##  ##  ','    ^^    ', &
     &'    ''''''','  ""  ""  ','   :::::  ','  ##  ##  ','   ^^^^   ', &
     &'          ','  ""  ""  ','    :::   ','##########','  ^^  ^^  ', &
     &'          ','          ','          ','  ##  ##  ',' ^^    ^^ ', &
     &'          ','          ','    :::   ','  ##  ##  ','^^      ^^', &
     &'          ','          ','   :::::  ','##########','          ', &
     &'          ','          ','    :::   ','  ##  ##  ','          ', &
     &'          ','          ','          ','  ##  ##  ','          ', &
     &'          ','          ','          ','          ','          '/
      data ((alf(irow,ilet),ilet=61,65),irow=1,10) /                    &
     &'7777777777',' 88888888 ',' 99999999 ','          ','          ', &
     &'7777777777','8888888888','9999999999','          ','          ', &
     &'       77 ','88      88','99      99','          ','          ', &
     &'      77  ','88      88','99      99','          ','          ', &
     &'     77   ',' 88888888 ','9999999999','          ','          ', &
     &'    77    ',' 88888888 ',' 999999999','          ','   aaaa   ', &
     &'   77     ','88      88','        99','          ','      aa  ', &
     &'  77      ','88      88','        99','          ','  aaaaaa  ', &
     &' 77       ','8888888888','9999999999','          ',' aa   aa  ', &
     &'77        ',' 88888888 ',' 99999999 ','          ','  aaaaa a '/
      data ((alf(irow,ilet),ilet=66,70),irow=1,10) /                    &
     &' bb       ','          ','       dd ','          ','     ff   ', &
     &' bb       ','          ','       dd ','          ','    ff ff ', &
     &' bb       ','          ','       dd ','          ','    ff    ', &
     &' bb       ','          ','       dd ','          ','  fffff   ', &
     &' bb       ','          ','       dd ','          ','    ff    ', &
     &' bbbbbbb  ','  cccccc  ','  ddddddd ','  eeeeee  ','    ff    ', &
     &' bb    bb ',' cc       ',' d     dd ',' ee     e ','    ff    ', &
     &' bb    bb ',' cc       ',' d     dd ',' eeeeeee  ','    ff    ', &
     &' bb    bb ',' cc       ',' d     dd ',' ee       ','    ff    ', &
     &' bbbbbbb  ','  cccccc  ','  ddddddd ','  eeeeee  ','    ff    '/
      data ((alf(irow,ilet),ilet=71,75),irow=1,10) /                    &
     &'          ',' hh       ','          ','          ','          ', &
     &'          ',' hh       ','          ','          ',' kk       ', &
     &'          ',' hh       ','          ','       jj ',' kk       ', &
     &'  ggggg   ',' hh       ','    ii    ','          ',' kk       ', &
     &' g     gg ',' hh       ','          ','       jj ',' kk   kk  ', &
     &' g     gg ',' hhhhhhh  ','    ii    ','       jj ',' kk  kk   ', &
     &'  ggggggg ',' hh    hh ','    ii    ','       jj ',' kkkk     ', &
     &'       gg ',' hh    hh ','    ii    ','       jj ',' kk  kk   ', &
     &'       gg ',' hh    hh ','    ii    ',' jj    jj ',' kk   kk  ', &
     &'   ggggg  ',' hh    hh ','    ii    ','   jjjjj  ',' kk    kk '/
      data ((alf(irow,ilet),ilet=76,80),irow=1,10) /                    &
     &'   lll    ','          ','          ','          ','          ', &
     &'    ll    ','          ','          ','          ','          ', &
     &'    ll    ','          ','          ','          ','          ', &
     &'    ll    ','          ','          ','          ','   ppppp  ', &
     &'    ll    ','          ','          ','          ',' pp     p ', &
     &'    ll    ',' mmmm mmm ',' n nnnnn  ','  oooooo  ',' pp     p ', &
     &'    ll    ',' mm  m  mm',' nn    nn ',' oo    oo ',' ppppppp  ', &
     &'    ll    ',' mm  m  mm',' nn    nn ',' oo    oo ',' pp       ', &
     &'    ll    ',' mm  m  mm',' nn    nn ',' oo    oo ',' pp       ', &
     &'    ll    ',' mm  m  mm',' nn    nn ','  oooooo  ',' pp       '/
      data ((alf(irow,ilet),ilet=81,85),irow=1,10) /                    &
     &'          ','          ','          ','          ','          ', &
     &'          ','          ','          ','          ','          ', &
     &'          ','          ','          ','   tt     ','          ', &
     &'  qqqqq   ','          ','          ','   tt     ','          ', &
     &' qq    qq ','          ','          ',' ttttttt  ','          ', &
     &' qq    qq ',' r rrrrrr ','  ssssss  ','   tt     ',' uu    uu ', &
     &'  qqqqqqq ',' rrr      ',' ss       ','   tt     ',' uu    uu ', &
     &'       qq ',' rr       ','  ssssss  ','   tt     ',' uu    uu ', &
     &'       qq ',' rr       ','       ss ','   tt     ',' uu    uu ', &
     &'       qq ',' rr       ','  ssssss  ','     ttt  ','  uuuuuu u'/
      data ((alf(irow,ilet),ilet=86,90),irow=1,10) /                    &
     &'          ','          ','          ','          ','          ', &
     &'          ','          ','          ','          ','          ', &
     &'          ','          ','          ','          ','          ', &
     &'          ','          ','          ',' yy    yy ','          ', &
     &'          ','          ','          ','  yy  yy  ','          ', &
     &' vv     vv',' ww     ww',' xx   xx  ','   yyyy   ','  zzzzzz  ', &
     &'  vv   vv ',' ww  w  ww','  xx xx   ','    yy    ','     zz   ', &
     &'   vv vv  ',' ww  w  ww','    x     ','   yy     ','    zz    ', &
     &'    v v   ',' ww  w  ww','  xx xx   ','  yy      ','   zz     ', &
     &'     v    ','  www www ',' xx   xx  ',' yy       ','  zzzzzz  '/
      data ((alf(irow,ilet),ilet=91,95),irow=1,10) /                    &
     &'   |||    ','   {{{{   ','   }}}}   ','  ~       ','```       ', &
     &'   |||    ','  {{      ','      }}  ','  ~      ~',' ```      ', &
     &'   |||    ',' {{       ','       }} ','~ ~ ~   ~ ','  ```     ', &
     &'   |||    ','  {{      ','      }}  ',' ~ ~ ~ ~ ~','   ```    ', &
     &'   |||    ','{{{       ','       }}}','~   ~ ~ ~ ','          ', &
     &'   |||    ','{{{       ','       }}}','     ~ ~  ','          ', &
     &'   |||    ','  {{      ','      }}  ','          ','          ', &
     &'   |||    ',' {{       ','       }} ','          ','          ', &
     &'   |||    ','  {{      ','      }}  ','          ','          ', &
     &'   |||    ','   {{{{   ','   }}}}   ','          ','          '/
!-----------------------------------------------------------------------------------------------------------------------------------
   lstr=min(len_trim(str),maxchar)  ! have space to print maxchar characters at most
   do ii = 1, lstr                 ! find column number for this letter
      ip=index('ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456@$%&*()-_=+\][>.<,?/!;''":#^789 abcdefghijklmnopqrstuvwxyz|{}~`',str(ii:ii))
      if (ip .eq. 0) then      ! if not found, letter is not supported
         ! (note that could just check  31 < ichar(str(ii:ii) < 127 now that all printable ASCII characters are defined )
         l(ii) = 64
         ! actually printing a non-printable character is risky so print ASCII Decimal Equivalent
         call journal('sc','*banner* BAD CHARACTER, ADE=',ichar(STR(ii:ii)))
      else
         l(ii) = ip
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   do kk = 1, 10
      if(iout.ge.0)then
         write (*,'(1x,*(a12):)') (alf(kk,l(mm)), mm = 1, lstr)
      else
         write (LINE, '(1x,132(a12):)') (alf(kk,l(mm)), mm = 1, lstr)
         call journal(LINE)
      endif
   enddo
!===================================================================================================================================
end subroutine blocks
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     signs - [M_messages]write out 132-character string in large block letters
!!
!!##SYNOPSIS
!!
!!     subroutine signs(string,iounit)
!!
!!      character(len=132),intent(in)  :: string
!!      integer,intent(in)             :: iounit
!!
!!##DESCRIPTION
!!     Given a string up to 132 characters long, signs() writes out the string
!!     left-justified in large (13 lines x 8 columns) block letters starting in
!!     column 2.
!!
!!     This can be used to make banners in program output files; it is also handy
!!     for making attention-catching notices in interactive programs.
!!
!!##EXAMPLE
!!
!!     The program:
!!
!!        program demo_signs
!!        use M_messages, only : signs
!!
!!        call signs('NOTICE',6)
!!
!!        end program demo_signs
!!
!!     would produce:
!!
!!      > XX  XXX   XXX   XXXXXXX  XXXXX    XXXX  XXXXXXX
!!      >  X   X   X   X  X  X  X    X     X    X  X    X
!!      >  XX  X  X     X    X       X    X        X
!!      >  XX  X  X     X    X       X    X        X  X
!!      >  X X X  X     X    X       X    X        XXXX
!!      >  X  XX  X     X    X       X    X        X  X
!!      >  X  XX  X     X    X       X    X        X
!!      >  X   X   X   X     X       X     X    X  X    X
!!      > XXX  X    XXX     XXX    XXXXX    XXXX  XXXXXXX
!===================================================================================================================================
!>
!! REFERENCES        : none
!! DEPENDENCIES      : none
!! LEGAL RESTRICTIONS: Copyright (c) 1984, 1996, John S. Urban
!! QA                : banner.f03
!! AUTHORS           : John S. Urban
!!##VERSION           : 1.0, 20110101
!===================================================================================================================================
subroutine signs(str,iout)
!=======================================================================--------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
!=======================================================================--------
implicit none
character(len=*),parameter :: ident="@(#)M_messages::signs(3f):write up to 132 large block letters"
character(len=*),intent(in) :: str       ! string to write
integer,intent(in)          :: iout      ! unit number to write to
!     ******************************************************************
      character(len=8),save       :: alf(13,95) ! store block letters
      integer,parameter           :: maxchars=132
      integer                     :: l(maxchars)! alphabet
      integer                     :: i30, k, mm ! loop counters
      integer                     :: ip
      integer                     :: lstr
      integer                     :: irow, ilet
!     ******************************************************************
      DATA ((ALF(IROW,ILET),ILET=1,5),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ', &
     & '        ' , '        ' , '        ' , '        ' , '        ', &
     & '   XX   ' , ' XXXXXX ' , '   XXXX ' , ' XXXXX  ' , ' XXXXXXX', &
     & '    X   ' , '  X    X' , '  X    X' , '  X   X ' , '  X    X', &
     & '    X   ' , '  X    X' , ' X      ' , '  X    X' , '  X     ', &
     & '   X X  ' , '  X    X' , ' X      ' , '  X    X' , '  X  X  ', &
     & '   X X  ' , '  XXXXX ' , ' X      ' , '  X    X' , '  XXXX  ', &
     & '  X   X ' , '  X    X' , ' X      ' , '  X    X' , '  X  X  ', &
     & '  XXXXX ' , '  X    X' , ' X      ' , '  X    X' , '  X     ', &
     & '  X   X ' , '  X    X' , '  X    X' , '  X   X ' , '  X    X', &
     & ' XXX XXX' , ' XXXXXX ' , '   XXXX ' , ' XXXXX  ' , ' XXXXXXX', &
     & '        ' , '        ' , '        ' , '        ' , '        ', &
     & '        ' , '        ' , '        ' , '        ' , '        '/

      DATA ((ALF(IROW,ILET),ILET=6,10),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ', &
     & '        ' , '        ' , '        ' , '        ' , '        ', &
     & ' XXXXXXX' , '   XXXX ' , ' XXX XXX' , '  XXXXX ' , '   XXXX ', &
     & '  X    X' , '  X    X' , '  X   X ' , '    X   ' , '     X  ', &
     & '  X     ' , ' X      ' , '  X   X ' , '    X   ' , '     X  ', &
     & '  X  X  ' , ' X      ' , '  X   X ' , '    X   ' , '     X  ', &
     & '  XXXX  ' , ' X      ' , '  XXXXX ' , '    X   ' , '     X  ', &
     & '  X  X  ' , ' X   XXX' , '  X   X ' , '    X   ' , '     X  ', &
     & '  X     ' , ' X     X' , '  X   X ' , '    X   ' , ' X   X  ', &
     & '  X     ' , '  X    X' , '  X   X ' , '    X   ' , ' X   X  ', &
     & ' XXXX   ' , '   XXXX ' , ' XXX XXX' , '  XXXXX ' , '  XXX   ', &
     & '        ' , '        ' , '        ' , '        ' , '        ', &
     & '        ' , '        ' , '        ' , '        ' , '        '/

      DATA ((ALF(IROW,ILET),ILET=11,15),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ', &
     & '        ' , '        ' , '        ' , '        ' , '        ', &
     & ' XXX  XX' , ' XXXXX  ' , ' XX   XX' , ' XX  XXX' , '   XXX  ', &
     & '  X   X ' , '   X    ' , '  X   X ' , '  X   X ' , '  X   X ', &
     & '  X  X  ' , '   X    ' , '  XX XX ' , '  XX  X ' , ' X     X', &
     & '  X  X  ' , '   X    ' , '  XX XX ' , '  XX  X ' , ' X     X', &
     & '  X X   ' , '   X    ' , '  X X X ' , '  X X X ' , ' X     X', &
     & '  XXX   ' , '   X    ' , '  X X X ' , '  X  XX ' , ' X     X', &
     & '  X  X  ' , '   X    ' , '  X   X ' , '  X  XX ' , ' X     X', &
     & '  X   X ' , '   X   X' , '  X   X ' , '  X   X ' , '  X   X ', &
     & ' XXX  XX' , ' XXXXXXX' , ' XXX XXX' , ' XXX  X ' , '   XXX  ', &
     & '        ' , '        ' , '        ' , '        ' , '        ', &
     & '        ' , '        ' , '        ' , '        ' , '        '/

      DATA ((ALF(IROW,ILET),ILET=16,20),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ', &
     & '        ' , '        ' , '        ' , '        ' , '        ', &
     & ' XXXXXX ' , '   XXX  ' , ' XXXXXX ' , '  XXXXX ' , ' XXXXXXX', &
     & '  X    X' , '  X   X ' , '  X    X' , ' X     X' , ' X  X  X', &
     & '  X    X' , ' X     X' , '  X    X' , ' X      ' , '    X   ', &
     & '  X    X' , ' X     X' , '  X    X' , ' X      ' , '    X   ', &
     & '  XXXXX ' , ' X     X' , '  XXXXX ' , '  XXXXX ' , '    X   ', &
     & '  X     ' , ' X     X' , '  X  X  ' , '       X' , '    X   ', &
     & '  X     ' , ' X     X' , '  X  X  ' , '       X' , '    X   ', &
     & '  X     ' , '  X   X ' , '  X   X ' , ' X     X' , '    X   ', &
     & ' XXXX   ' , '   XXX  ' , ' XXX  XX' , '  XXXXX ' , '   XXX  ', &
     & '        ' , '   XX XX' , '        ' , '        ' , '        ', &
     & '        ' , '        ' , '        ' , '        ' , '        '/

      DATA ((ALF(IROW,ILET),ILET=21,25),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ', &
     & '        ' , '        ' , '        ' , '        ' , '        ', &
     & ' XXX XXX' , ' XXX XXX' , ' XXX XXX' , ' XXX XXX' , ' XXX XXX', &
     & '  X   X ' , '  X   X ' , '  X   X ' , '  X   X ' , '  X   X ', &
     & '  X   X ' , '  X   X ' , '  X   X ' , '   X X  ' , '  X   X ', &
     & '  X   X ' , '  X   X ' , '  X   X ' , '   X X  ' , '   X X  ', &
     & '  X   X ' , '   X X  ' , '  X X X ' , '    X   ' , '   X X  ', &
     & '  X   X ' , '   X X  ' , '  X X X ' , '   X X  ' , '    X   ', &
     & '  X   X ' , '   X X  ' , '  X X X ' , '   X X  ' , '    X   ', &
     & '  X   X ' , '    X   ' , '   X X  ' , '  X   X ' , '    X   ', &
     & '   XXX  ' , '    X   ' , '   X X  ' , ' XXX XXX' , '   XXX  ', &
     & '        ' , '        ' , '        ' , '        ' , '        ', &
     & '        ' , '        ' , '        ' , '        ' , '        '/

      DATA ((ALF(IROW,ILET),ILET=26,30),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & ' XXXXXXX' , '   XXX  ' , '    X   ' , '   XXX  ' , '   XXX  ' , &
     & ' X    X ' , '  X   X ' , '  XXX   ' , '  X   X ' , '  X   X ' , &
     & '     X  ' , '  X   X ' , '    X   ' , '      X ' , '      X ' , &
     & '     X  ' , '  X   X ' , '    X   ' , '      X ' , '      X ' , &
     & '    X   ' , '  X   X ' , '    X   ' , '     X  ' , '    XX  ' , &
     & '   X    ' , '  X   X ' , '    X   ' , '    X   ' , '      X ' , &
     & '   X    ' , '  X   X ' , '    X   ' , '   X    ' , '      X ' , &
     & '  X    X' , '  X   X ' , '    X   ' , '  X     ' , '  X   X ' , &
     & ' XXXXXXX' , '   XXX  ' , '  XXXXX ' , '  XXXXX ' , '   XXX  ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' /

      DATA ((ALF(IROW,ILET),ILET=31,35),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '     X  ' , '  XXXXX ' , '    XX  ' , '   XXXX ' , '    X   ' , &
     & '    XX  ' , '  X     ' , '   X    ' , '  X    X' , '   XXX  ' , &
     & '    XX  ' , '  X     ' , '  X     ' , ' X  XX X' , '  X   X ' , &
     & '   X X  ' , '  X     ' , '  X     ' , ' X X X X' , '  X     ' , &
     & '   X X  ' , '  XXXX  ' , '  XXXX  ' , ' X X X X' , '   XXX  ' , &
     & '  X  X  ' , '      X ' , '  X   X ' , ' X X X X' , '      X ' , &
     & '  XXXXX ' , '      X ' , '  X   X ' , ' X  XXX ' , '  X   X ' , &
     & '     X  ' , '  X   X ' , '  X   X ' , '  X     ' , '   XXX  ' , &
     & '    XXX ' , '   XXX  ' , '   XXX  ' , '   XXX  ' , '    X   ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' /

      DATA ((ALF(IROW,ILET),ILET=36,40),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '  X     ' , '   XX   ' , '        ' , '     X  ' , '   X    ' , &
     & ' X X   X' , '  X     ' , '        ' , '    X   ' , '    X   ' , &
     & '  X   X ' , '  X     ' , '  XX XX ' , '    X   ' , '    X   ' , &
     & '     X  ' , '   X    ' , '   XXX  ' , '   X    ' , '     X  ' , &
     & '    X   ' , '  XX    ' , ' XXXXXXX' , '   X    ' , '     X  ' , &
     & '   X    ' , ' X  X  X' , '   XXX  ' , '   X    ' , '     X  ' , &
     & '  X   X ' , ' X  X X ' , '  XX XX ' , '   X    ' , '     X  ' , &
     & ' X   X X' , ' X   X  ' , '        ' , '   X    ' , '     X  ' , &
     & '      X ' , '  XXX XX' , '        ' , '    X   ' , '    X   ' , &
     & '        ' , '        ' , '        ' , '    X   ' , '    X   ' , &
     & '        ' , '        ' , '        ' , '     X  ' , '   X    ' /

      DATA ((ALF(IROW,ILET),ILET=41,44),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '    X   ' , &
     & '        ' , '        ' , '  XXXXX ' , '    X   ' , &
     & '        ' , '        ' , '        ' , '    X   ' , &
     & ' XXXXXXX' , '        ' , '  XXXXX ' , ' XXXXXXX' , &
     & '        ' , '        ' , '        ' , '    X   ' , &
     & '        ' , '        ' , '        ' , '    X   ' , &
     & '        ' , '        ' , '        ' , '    X   ' , &
     & '        ' , '        ' , '        ' , '        ' , &
     & '        ' , 'XXXXXXXX' , '        ' , '        ' /

      DATA ((ALF(IROW,ILET),ILET=45,45),IROW=1,13) / &
     & '        ' , &
     & '        ' , &
     & '        ' , &
     & ' X      ' , &
     & '  X     ' , &
     & '   X    ' , &
     & '    X   ' , &
     & '     X  ' , &
     & '      X ' , &
     & '       X' , &
     & '        ' , &
     & '        ' , &
     & '        ' /

      DATA ((ALF(IROW,ILET),ILET=46,50),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '   XXX  ' , '   XXX  ' , '        ' , '        ' , '        ' , &
     & '     X  ' , '   X    ' , '        ' , '        ' , '        ' , &
     & '     X  ' , '   X    ' , '   X    ' , '        ' , '     X  ' , &
     & '     X  ' , '   X    ' , '    X   ' , '        ' , '    X   ' , &
     & '     X  ' , '   X    ' , '     X  ' , '        ' , '   X    ' , &
     & '     X  ' , '   X    ' , '      X ' , '        ' , '  X     ' , &
     & '     X  ' , '   X    ' , '     X  ' , '        ' , '   X    ' , &
     & '     X  ' , '   X    ' , '    X   ' , '        ' , '    X   ' , &
     & '     X  ' , '   X    ' , '   X    ' , '    X   ' , '     X  ' , &
     & '     X  ' , '   X    ' , '        ' , '        ' , '        ' , &
     & '   XXX  ' , '   XXX  ' , '        ' , '        ' , '        ' /

      DATA ((ALF(IROW,ILET),ILET=51,55),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '   XXX  ' , '        ' , '    X   ' , '        ' , &
     & '        ' , '  X   X ' , '       X' , '    X   ' , '        ' , &
     & '        ' , '      X ' , '      X ' , '    X   ' , '        ' , &
     & '        ' , '      X ' , '     X  ' , '    X   ' , '    X   ' , &
     & '        ' , '     X  ' , '    X   ' , '    X   ' , '        ' , &
     & '        ' , '    X   ' , '   X    ' , '    X   ' , '        ' , &
     & '        ' , '    X   ' , '  X     ' , '    X   ' , '        ' , &
     & '        ' , '        ' , ' X      ' , '        ' , '        ' , &
     & '    X   ' , '    X   ' , '        ' , '    X   ' , '    X   ' , &
     & '   X    ' , '        ' , '        ' , '        ' , '   X    ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' /

      DATA ((ALF(IROW,ILET),ILET=56,60),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '    X   ' , &
     & '    X   ' , '   X X  ' , '        ' , '    X X ' , '   X X  ' , &
     & '    X   ' , '   X X  ' , '        ' , '    X X ' , '  X   X ' , &
     & '    X   ' , '   X X  ' , '        ' , '  XXXXXX' , '        ' , &
     & '        ' , '        ' , '    X   ' , '   X X  ' , '        ' , &
     & '        ' , '        ' , '        ' , '   X X  ' , '        ' , &
     & '        ' , '        ' , '        ' , '   X X  ' , '        ' , &
     & '        ' , '        ' , '        ' , ' XXXXXX ' , '        ' , &
     & '        ' , '        ' , '        ' , '  X X   ' , '        ' , &
     & '        ' , '        ' , '    X   ' , '  X X   ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' /

      DATA ((ALF(IROW,ILET),ILET=61,65),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '  XXXXX ' , '   XXX  ' , '   XXX  ' , '        ' , '        ' , &
     & '  X   X ' , '  X   X ' , '  X   X ' , '        ' , '        ' , &
     & '      X ' , '  X   X ' , '  X   X ' , '        ' , '        ' , &
     & '     X  ' , '  X   X ' , '  X   X ' , '        ' , '  XXXX  ' , &
     & '     X  ' , '   XXX  ' , '   XXXX ' , '        ' , '      X ' , &
     & '    X   ' , '  X   X ' , '      X ' , '        ' , '  XXXXX ' , &
     & '    X   ' , '  X   X ' , '      X ' , '        ' , ' X    X ' , &
     & '   X    ' , '  X   X ' , '     X  ' , '        ' , ' X    X ' , &
     & '   X    ' , '   XXX  ' , '   XX   ' , '        ' , '  XXXX X' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' /

      DATA ((ALF(IROW,ILET),ILET=66,70),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & ' XX     ' , '        ' , '     XX ' , '        ' , '    XX  ' , &
     & '  X     ' , '        ' , '      X ' , '        ' , '   X    ' , &
     & '  X     ' , '        ' , '      X ' , '        ' , '   X    ' , &
     & '  XXXXX ' , '  XXXXX ' , '  XXXXX ' , '  XXXXX ' , '  XXXX  ' , &
     & '  X    X' , ' X     X' , ' X    X ' , ' X     X' , '   X    ' , &
     & '  X    X' , ' X      ' , ' X    X ' , ' XXXXXXX' , '   X    ' , &
     & '  X    X' , ' X      ' , ' X    X ' , ' X      ' , '   X    ' , &
     & '  X    X' , ' X     X' , ' X    X ' , ' X     X' , '   X    ' , &
     & ' XXXXXX ' , '  XXXXX ' , '  XXXXXX' , '  XXXXX ' , '  XXXX  ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' /

      DATA ((ALF(IROW,ILET),ILET=71,75),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , ' XX     ' , '    X   ' , '     X  ' , ' XX     ' , &
     & '        ' , '  X     ' , '        ' , '        ' , '  X     ' , &
     & '        ' , '  X     ' , '        ' , '        ' , '  X     ' , &
     & '  XXXXXX' , '  X XX  ' , '  XXX   ' , '  XXXX  ' , '  X  XX ' , &
     & ' X    X ' , '  XX  X ' , '    X   ' , '     X  ' , '  X  X  ' , &
     & ' X    X ' , '  X   X ' , '    X   ' , '     X  ' , '  X X   ' , &
     & ' X    X ' , '  X   X ' , '    X   ' , '     X  ' , '  XXX   ' , &
     & '  XXXXX ' , '  X   X ' , '    X   ' , '     X  ' , '  X  X  ' , &
     & '      X ' , ' XXX XXX' , '  XXXXX ' , '     X  ' , ' XX   XX' , &
     & '      X ' , '        ' , '        ' , '     X  ' , '        ' , &
     & '  XXXX  ' , '        ' , '        ' , '  XXX   ' , '        ' /

      DATA ((ALF(IROW,ILET),ILET=76,80),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '   XX   ' , '        ' , '        ' , '        ' , '        ' , &
     & '    X   ' , '        ' , '        ' , '        ' , '        ' , &
     & '    X   ' , '        ' , '        ' , '        ' , '        ' , &
     & '    X   ' , ' XXX X  ' , ' XX XX  ' , '  XXXXX ' , ' XXXXXX ' , &
     & '    X   ' , '  X X X ' , '  XX  X ' , ' X     X' , '  X    X' , &
     & '    X   ' , '  X X X ' , '  X   X ' , ' X     X' , '  X    X' , &
     & '    X   ' , '  X X X ' , '  X   X ' , ' X     X' , '  X    X' , &
     & '    X   ' , '  X X X ' , '  X   X ' , ' X     X' , '  X    X' , &
     & '  XXXXX ' , ' XX X XX' , ' XXX XXX' , '  XXXXX ' , '  XXXXX ' , &
     & '        ' , '        ' , '        ' , '        ' , '  X     ' , &
     & '        ' , '        ' , '        ' , '        ' , ' XXX    ' /

      DATA ((ALF(IROW,ILET),ILET=81,85),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '   X    ' , '        ' , &
     & '        ' , '        ' , '        ' , '   X    ' , '        ' , &
     & '  XXXXXX' , ' XXX XX ' , '  XXXXX ' , '  XXXX  ' , ' XX  XX ' , &
     & ' X    X ' , '   XX  X' , ' X     X' , '   X    ' , '  X   X ' , &
     & ' X    X ' , '   X    ' , '  XXX   ' , '   X    ' , '  X   X ' , &
     & ' X    X ' , '   X    ' , '     XX ' , '   X    ' , '  X   X ' , &
     & ' X    X ' , '   X    ' , ' X     X' , '   X  X ' , '  X  XX ' , &
     & '  XXXXX ' , ' XXXXX  ' , '  XXXXX ' , '    XX  ' , '   XX XX' , &
     & '      X ' , '        ' , '        ' , '        ' , '        ' , &
     & '     XXX' , '        ' , '        ' , '        ' , '        ' /

      DATA ((ALF(IROW,ILET),ILET=86,90),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & ' XXX XXX' , ' XXX XXX' , ' XXX XXX' , ' XXX XXX' , ' XXXXXX ' , &
     & '  X   X ' , '  X   X ' , '  X   X ' , '  X   X ' , ' X   X  ' , &
     & '  X   X ' , '  X X X ' , '   XXX  ' , '  X   X ' , '    X   ' , &
     & '   X X  ' , '  X X X ' , '   XXX  ' , '   X X  ' , '   X    ' , &
     & '   X X  ' , '   X X  ' , '  X   X ' , '   X X  ' , '  X   X ' , &
     & '    X   ' , '   X X  ' , ' XXX XXX' , '    X   ' , ' XXXXXX ' , &
     & '        ' , '        ' , '        ' , '    X   ' , '        ' , &
     & '        ' , '        ' , '        ' , '  XX    ' , '        ' /

      DATA ((ALF(IROW,ILET),ILET=91,95),IROW=1,13) / &
     & '        ' , '        ' , '        ' , '        ' , '        ' , &
     & '    X   ' , '        ' , '        ' , '        ' , '        ' , &
     & '    X   ' , '     XX ' , '  XX    ' , '        ' , '   X    ' , &
     & '    X   ' , '    X   ' , '    X   ' , '  XX   X' , '    X   ' , &
     & '    X   ' , '    X   ' , '    X   ' , ' X  X  X' , '        ' , &
     & '    X   ' , '    X   ' , '    X   ' , ' X   XX ' , '        ' , &
     & '    X   ' , '    X   ' , '    X   ' , '        ' , '        ' , &
     & '    X   ' , '  XX    ' , '     XX ' , '        ' , '        ' , &
     & '    X   ' , '    X   ' , '    X   ' , '        ' , '        ' , &
     & '    X   ' , '    X   ' , '    X   ' , '        ' , '        ' , &
     & '    X   ' , '    X   ' , '    X   ' , '        ' , '        ' , &
     & '    X   ' , '    X   ' , '    X   ' , '        ' , '        ' , &
     & '    X   ' , '     XX ' , '  XX    ' , '        ' , '        ' /
!-----------------------------------------------------------------------------------------------------------------------------------
!!   NOTE:
!!   rearrange the character definitions in their ASCII decimal equivalent
!!   order and the INDEX() call could be replaced with a simple ICHAR()
!!   call.

   lstr=min(len_trim(str),maxchars)
   do i30 = 1, lstr     ! find column number for this letter
      ip=index('ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456@$%&*()-_=+\][>.<,?/!;''":#^789 abcdefghijklmnopqrstuvwxyz|{}~`',str(i30:i30))
      if (ip .eq. 0) then  ! if not found, letter is not supported
         l(i30) = 64
         write(*,*)'*signs* UNSUPPORTED CHARACTER, ADE=',ichar(str(i30:i30)) ! print ASCII Decimal Equivalent
     else
         l(i30) = ip
     endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   do k = 1, 13
      write (iout, '(1x,132a8:)') (alf(k,l(mm)), mm = 1, lstr)
   enddo
   contains
end subroutine signs
!===================================================================================================================================

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    percent_done(3f) - [M_messages] non-advancing status counter displays percentage done
!!                       on terminal displays
!!
!!##SYNOPSIS
!!
!!   subroutine percent_done(part,whole)
!!
!!    class(*),intent(in) :: part
!!    class(*),intent(in) :: whole
!!
!!##DESCRIPTION
!!    For interactive terminal sessions display the message
!!
!!      " Percent Complete: NNN.NN%"
!!
!!##OPTIONS
!!    part   number of elements completed. Should take any
!!           scalar numeric value.
!!    whole  total number of elements to be completed. Should take any
!!           scalar numeric value.
!!
!!##EXAMPLE
!!
!!
!!   Sample program:
!!
!!    program progress
!!    use m_time, only : system_sleep
!!    use m_messages, only : percent_done
!!    implicit none
!!    integer :: i, nr=10
!!
!!    do i=1,nr
!!       call percent_done(i,nr)
!!       call system_sleep(1)  !give a delay in seconds
!!    enddo
!!
!!    end program progress
!!
!!   Results:
!!
!!    Percent Complete: 100.00%
!===================================================================================================================================
subroutine percent_done(part,whole)
use M_anyscalar, only : anyscalar_to_real
implicit none
character(len=*),parameter::ident="@(#)place a non-advancing status counter on terminal display (not redirected)"
   class(*),intent(in)  :: part
   class(*),intent(in)  :: whole
   real                 :: part_local
   real                 :: whole_local
   part_local=anyscalar_to_real(part)
   whole_local=anyscalar_to_real(whole)
   write(*,fmt="(a1,' Percent Complete: ',t21,f0.2,'%')",ADVANCE="NO") achar(13), (part_local/whole_local)*100.0
end subroutine percent_done
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================

end module M_messages
