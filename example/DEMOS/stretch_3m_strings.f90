         program demo_stretch
          use M_strings, only : stretch
          implicit none
          character(len=10)            :: string='abcdefghij'
          character(len=:),allocatable :: answer
          integer                      :: i
             answer=stretch(string,5)
             write(*,'("[",a,"]")') answer
             answer=stretch(string,20)
             write(*,'("[",a,"]")') answer
             i=30
             write(*,*)
             write(*,'(1x,a,i0)') &
              & stretch('CHAPTER 1 : The beginning ',i,'.'), 1    ,&
              & stretch('CHAPTER 2 : The end ',i,'.'),       1234 ,&
              & stretch('APPENDIX ',i,'.'),                  1235
             write(*,*)
             write(*,'(1x,a,i7)') &
              & stretch('CHAPTER 1 : The beginning ',i,'.'), 1    ,&
              & stretch('CHAPTER 2 : The end ',i,'.'),       1234 ,&
              & stretch('APPENDIX ',i,'.'),                  1235
             write(*,*)
             write(*,*) &
              & stretch('CHAPTER 1 : The beginning ',i,suffix=': '), 1
             write(*,*) &
              & stretch('CHAPTER 2 : The end ',i,suffix=': '),1234
             write(*,*) &
              & stretch('APPENDIX ',i,suffix=': '),           1235
         end program demo_stretch
