     program demo_index
     use M_unicode, only : ut=>unicode_type
     use M_unicode, only : assignment(=)
     use M_unicode, only : index
     implicit none
     type(ut)                   :: str
     character(len=*),parameter :: all='(*(g0))'
     integer                    :: ii
        !
        str='Huli i kēia kaula no kēia ʻōlelo'
        !bug!print all, index(str,'kēia').eq.8
        ii=index(str,'kēia'); print all, ii.eq.8
        !
        ! return value is counted from the left end even if BACK=.TRUE.
        !bug!print all, index(str,'kēia',back=.true.).eq.22
        ii=index(str,'kēia',back=.true.); print all, ii.eq.22
        !
        ! INDEX is case-sensitive
        !bug!print all, index(str,'Kēia').eq.0
        ii=index(str,'Kēia'); print all, ii.eq.0
        !<<<<<<<<<<
        !ifx bug: ifx (IFX) 2024.1.0 20240308
        !
        !example/demo_index.f90(17): error #6766: A binary defined OPERATOR
        !definition is missing or incorrect.   [EQ]
        !        print all, index(str,'k  ia',back=.true.).eq.22
        !--------------------------------------------------^
        !Original works with gfortran and flang_new and this works with ifx
        !        ii=ndex(str,'k  ia',back=.true.)
        !    print all, ii.eq.22
        !>>>>>>>>>>
     end program demo_index
