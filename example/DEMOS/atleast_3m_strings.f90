          program demo_atleast
           use M_strings, only : atleast
           implicit none
           character(len=10)            :: string='abcdefghij'
           character(len=:),allocatable :: answer
           integer                      :: i
              answer=atleast(string,5)
              write(*,'("[",a,"]")') answer
              answer=atleast(string,20)
              write(*,'("[",a,"]")') answer
              i=30
              write(*,*)
              write(*,'(1x,a,1x,i0)') &
               & atleast('CHAPTER 1 : The beginning ',i,'.'), 1   , &
               & atleast('CHAPTER 2 : The end ',i,'.'),       1234, &
               & atleast('APPENDIX ',i,'.'),                  1235
              write(*,*)
              write(*,'(1x,a,i7)') &
               & atleast('CHAPTER 1 : The beginning ',i,'.'), 1   , &
               & atleast('CHAPTER 2 : The end ',i,'.'),       1234, &
               & atleast('APPENDIX ',i,'.'),                  1235
          end program demo_atleast
