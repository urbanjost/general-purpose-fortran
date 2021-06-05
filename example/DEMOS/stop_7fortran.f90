          program demo_stop
          use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT
          implicit none
          integer :: stopcode
          ! Normal terminations
             ! A STOP with no parameter is a normal termination and generally
             ! returns a zero status value if the system supports return statuses
             stop
             ! All other stops are error stops
             stop 10
             stop 'That is all, folks!'
             stopcode=11
             stop stopcode
          ! Error terminations
             ! ERROR STOP is always an error stop, even without a stop-code
             error stop
             ! ERROR STOP often displays a traceback but that is not required
             error stop 10
             error stop 'That is all, folks!'
             error stop stopcode
          end program demo_stop
