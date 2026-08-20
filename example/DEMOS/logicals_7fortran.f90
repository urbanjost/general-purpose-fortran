         program demo_different_logical_kinds
         use iso_fortran_env, only : logical_kinds
         use,intrinsic :: iso_fortran_env, only : &
          & LOGICAL8, LOGICAL16, LOGICAL32, LOGICAL64
         use,intrinsic :: iso_c_binding,   only : C_BOOL
         implicit none
         character(len=*),parameter             :: all='(*(g0))'
         ! potentially save space and improve performance by using the
         ! smallest available kind
         integer,parameter                      :: lk=selected_logical_kind(1)
         logical(lk)                            :: smallest_storage(10,20)

         ! C_BOOL is a kind compatible with C interfaces
         logical(kind=c_bool)                   :: boolean=.TRUE.

         integer                                :: i
           ! The integer array constant LOGICAL_KINDS() contains the kind
           ! values for supported logical kinds for the current processor
           print all, 'list LOGICAL kind values available on this platform'
            do i =1, size(logical_kinds)
               print all, '   integer,parameter :: boolean', &
               & logical_kinds(i),'=', logical_kinds(i)
            enddo

           print all, '   LOGICAL8  ==> KIND=',LOGICAL8
           print all, '   LOGICAL16 ==> KIND=',LOGICAL16
           print all, '   LOGICAL32 ==> KIND=',LOGICAL32
           print all, '   LOGICAL64 ==> KIND=',LOGICAL64
           print all, '   C_BOOL    ==> KIND=',C_BOOL

           print all, 'storage size of default logical = ', storage_size(.true.)
           print all, 'storage size of smallest logical kind = ', &
            storage_size(smallest_storage)
           print all, 'storage size of C_BOOL= ', storage_size(boolean)

           print all, 'kind of default logical = ', kind(.true.)
           print all, 'kind of smallest logical kind = ', kind(smallest_storage)
           print all, 'kind of C_BOOL= ', kind(.true._c_bool)

         end program demo_different_logical_kinds
