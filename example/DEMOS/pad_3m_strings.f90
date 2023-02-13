     program demo_pad
      use M_strings, only : pad
      implicit none
      character(len=10)            :: string='abcdefghij'
      character(len=:),allocatable :: answer
      integer                      :: i
      character(len=*),parameter   :: g='(*(g0))'
         answer=pad(string,5)
         write(*,'("[",a,"]")') answer
         answer=pad(string,20)
         write(*,'("[",a,"]")') answer
         i=30
         write(*,g)
         write(*,'(1x,a,1x,i0)') &
          & pad('CHAPTER 1 : The beginning ',i,'.'), 1   , &
          & pad('CHAPTER 2 : The end ',i,'.'),       1234, &
          & pad('APPENDIX ',i,'.'),                  1235
         write(*,*)
         write(*,'(1x,a,i7)') &
          & pad('CHAPTER 1 : The beginning ',i,'.'), 1   , &
          & pad('CHAPTER 2 : The end ',i,'.'),       1234, &
          & pad('APPENDIX ',i,'.'),                  1235

          write(*,g)pad('12',5,'0',right=.false.)

          write(*,g)pad('12345 ',30,'_',right=.false.)
          write(*,g)pad('12345 ',30,'_',right=.false.,clip=.true.)
          write(*,g)pad('12345 ',7,'_',right=.false.)
          write(*,g)pad('12345 ',7,'_',right=.false.,clip=.true.)
          write(*,g)pad('12345 ',6,'_',right=.false.)
          write(*,g)pad('12345 ',6,'_',right=.false.,clip=.true.)
          write(*,g)pad('12345 ',5,'_',right=.false.)
          write(*,g)pad('12345 ',5,'_',right=.false.,clip=.true.)
          write(*,g)pad('12345 ',4,'_',right=.false.)
          write(*,g)pad('12345 ',4,'_',right=.false.,clip=.true.)
     end program demo_pad
