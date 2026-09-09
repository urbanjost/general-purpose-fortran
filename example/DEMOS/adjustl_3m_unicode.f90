     program demo_adjustl
     use M_unicode, only : ut=>unicode_type
     use M_unicode, only : ch=>character
     use M_unicode, only : adjustl, trim, len_trim, verify
     use M_unicode, only : write(formatted)
     use M_unicode, only : assignment(=)
     implicit none
     type(ut)                   :: usample, uout
     integer                    :: istart, iend
     character(len=*),parameter :: adt = '(a,"[",DT,"]")'
      !
      ! basic use
        usample='   sample string   '
        write(*,adt) 'original: ',usample
      !
      ! note a string stays the same length
      ! and is not trimmed by just an adjustl(3) call.
        write(*,adt) 'adjusted: ',adjustl(usample)
      !
      ! a fixed‐length string can be trimmed using trim(3)
        uout=trim(adjustl(usample))
        write(*,adt) 'trimmed:  ',uout
      !
      ! or alternatively you can select a substring without adjusting
        istart= max(1,verify(usample, ' ')) ! first non‐blank character
        iend = len_trim(usample)
        write(*,adt) 'substring:',usample%sub(istart,iend)
      !
        write(*,adt) 'substring:',adjustl(usample,30)
        write(*,adt) 'substring:',adjustl(usample,20)
        write(*,adt) 'substring:',adjustl(usample,10)
        write(*,adt) 'substring:',adjustl(usample,0)
     end program demo_adjustl
