          program demo_store
          use M_kracken, only : store, show
          implicit none
          integer :: ier
          ! The following should be equivalent to
          ! call kracken('MY',' &
          ! & -STRING My string value &
          ! & -INTEGER 1234 &
          ! & -INTEGER 0987654321 &
          ! & -REAL 1234.5678 &
          ! & -DOUBLE 123.4567d8 &
          ! & -LOGICAL T &
          ! & '
          call store('MY_STRING','My string value','add',ier)
          if(ier.ne.0)write(*,*)'ERROR: could not store MY_STRING ier=',ier
          ! now the verb MY is defined with the option -STRING so the
          ! dictionary has MY_STRING='My string value' defined

          ! this will be an error because MY does not have the -INTEGER
          ! keyword defined
          call store('MY_INTEGER',12345678,'no_add',ier)

          ! now define MY_INTEGER
          call store('MY_INTEGER',1234,'add',ier)
          ! if 'no_add' it will APPEND to current string
          call store('MY_INTEGER',987654321,'add',ier)

          call store('MY_REAL',1234.5678,'add',ier)
          call store('MY_DOUBLE',123.4567d8,'add',ier)
          call store('MY_LOGICAL',.true.,'add',ier)

          call show('MY',.false.,0)
          write(*,*)repeat('=',76)

          ! if 'replace' is used REPLACE instead of APPEND to current value
          call store('MY_INTEGER',987654321,'replace',ier)
          call show('MY',.false.,0)
          write(*,*)repeat('=',76)

          ! 'replace' can only replace an existing entry, not add one
          call store('MY_UNKNOWN',987654321,'replace',ier)
          call show('MY',.false.,0)
          write(*,*)repeat('=',76)

          end program demo_store
