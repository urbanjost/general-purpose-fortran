          program demo_extremum
          use M_math, only : extremum
          real,allocatable :: arr(:)
          arr=[-10.0,8.8,-5.0,0.0,5.0,10.0,-0.3]
          call extremum(arr,small,big)
          write(*,*)'ARRAY=',arr
          write(*,*)'SMALL=',small
          write(*,*)'BIG=',big
          end program demo_extremum
