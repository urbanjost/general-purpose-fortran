       program demo_fmt
       use :: M_unicode, only : fmt, assignment(=)
       use :: M_unicode, only : ut=>unicode_type, ch=>character
       implicit none
       character(len=:),allocatable :: Astr, Aformat
       type(ut) :: Ustr
          ! format can be CHARACTER
          Aformat="('[',i0,']')"
          Astr=fmt(10,Aformat)
          write(*,*)'result is ',Astr
          ! format can be string
          Astr=fmt(10.0/3.0,ut("'[',g0.5,']'"))
          write(*,*)'result is ',Astr
          ! Output is a string, so use ch()
          write(*,*)'result is ', ch(fmt(.true.,"'The answer is [',g0,']'"))
          ! OOP
          Ustr='A B C'
          Ustr=Ustr%fmt("'[',g0,']'")
          write(*,*)'result is ',ch(Ustr)
       end program demo_fmt
