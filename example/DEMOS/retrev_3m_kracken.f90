      program demo_retrev
      use M_kracken, only : kracken, retrev
      use M_kracken, only : IPvalue ! length of keyword value
      implicit none
      character(len=IPvalue) :: val
      integer                :: len, ier

      call kracken('demo', ' -value my default string')
      call retrev('demo_value',val,len,ier)
      write(*,'(a)')'VALUE IS '//trim(val)

      end program demo_retrev
