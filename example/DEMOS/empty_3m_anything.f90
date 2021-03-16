          program demo_empty_
          use M_anything, only : empty, assignment(=)
          integer, allocatable      :: ints(:)
          character(:), allocatable :: strs(:)
          real, allocatable      :: reals(:)
             ints=empty
             write(*,*)size(ints)

             write(*,*)'give them some size ...'
             reals = [1.0,2.0,3.0]
             ints = [1,2,3]
             strs = [character(len=10) :: "one","two","three","four"]
             write(*,*)size(ints)
             write(*,*)size(reals)
             write(*,*)size(strs)

             ints=empty
             reals=empty
             strs=empty
             write(*,*)'back to empty ...'
             write(*,*)size(ints)
             write(*,*)size(reals)
             write(*,*)size(strs)

          end program demo_empty_
