          program demo_sundragon
          use M_messages, only : jundragon
          character(len=32) :: a(8)
             a(1)='Puff, the magic dragon----------'
             a(2)='lived by the sea----------------'
             a(3)='and frolicked in the Autumn mist'
             a(4)='in a land called----------------'
             a(5)='Honiley-------------------------'
             a(6)='--------------------------------'
             a(7)='--------------------------------'
             a(8)='--------------------------------'
             call jundragon('s',a)
          end program demo_sundragon
