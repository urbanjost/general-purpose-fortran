      program demo_M_attr
      use M_attr, only : attr, attr_mode, attr_update, alert
      implicit none
      character(len=256) :: line
      character(len=*),parameter :: f='( &
       &"   <bo><w><G> GREAT: </G></w>&
       &The new value <Y><b>",f8.4,1x,"</b></Y> is in range"&
       &)'
      real :: value

         write(*,'(a)')&
         &attr('   <r><W><bo> ERROR: </W>red text on a white background</y>')

         value=3.4567
         write(line,fmt=f) value
         write(*,'(a)')attr(trim(line))

         ! write same string as plain text
         write(*,*)
         call attr_mode(manner='plain')
         write(*,'(a)')attr(trim(line))

         call attr_mode(manner='color')
         ! use pre-defined or user defined strings
         write(*,*)
         write(*,'(a)')attr('<ERROR> Woe is nigh.')
         write(*,'(a)')attr('<WARNING> The night is young.')
         write(*,'(a)')attr('<INFO> It is Monday')

         call alert('<ERROR>', 'Woe is nigh.')
         call alert('<WARNING>', 'The night is young.')
         call alert('<INFO>', 'It is Monday')

         ! create a custom mnemonic
         call attr_update('MYERROR',attr(&
         ' <R><e> E<w>-<e>R<w>-<e>R<w>-<e>O<w>-<e>R: </e></R></bo>'&
         ))
         write(*,*)
         write(*,'(a)')attr('<MYERROR> my custom message style')

    end program demo_M_attr
