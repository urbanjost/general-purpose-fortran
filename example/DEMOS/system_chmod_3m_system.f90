          program demo_system_chmod
          use M_system, only : system_chmod
          use M_system, only : system_stat
          use M_system, only : R_GRP,R_OTH,R_USR, RWX_G, RWX_U, W_OTH, X_GRP
          !use M_system, only : RWX_O, W_GRP,W_USR,X_OTH,X_USR
          !use M_system, only : DEFFILEMODE, ACCESSPERMS
          use,intrinsic     :: iso_fortran_env, only : int64
          implicit none
          integer         :: ierr
          integer         :: status
          integer(kind=int64) :: buffer(13)
             !Setting Read Permissions for User, Group, and Others
             ! The following example sets read permissions for the owner, group, and others.
             open(file='_test1',unit=10)
             write(10,*)'TEST FILE 1'
             close(unit=10)
             ierr=system_chmod('_test1', IANY([R_USR,R_GRP,R_OTH]))

             !Setting Read, Write, and Execute Permissions for the Owner Only
             ! The following example sets read, write, and execute permissions for the owner, and no permissions for group and others.
             open(file='_test2',unit=10)
             write(10,*)'TEST FILE 2'
             close(unit=10)
             ierr=system_chmod('_test2', RWX_U)

             !Setting Different Permissions for Owner, Group, and Other
             ! The following example sets owner permissions for CHANGEFILE to read, write, and execute, group permissions to read and
             ! execute, and other permissions to read.
             open(file='_test3',unit=10)
             write(10,*)'TEST FILE 3'
             close(unit=10)
             ierr=system_chmod('_test3', IANY([RWX_U,R_GRP,X_GRP,R_OTH]));

             !Setting and Checking File Permissions
             ! The following example sets the file permission bits for a file named /home/cnd/mod1, then calls the stat() function to
             ! verify the permissions.

             ierr=system_chmod("home/cnd/mod1", IANY([RWX_U,RWX_G,R_OTH,W_OTH]))
             call system_stat("home/cnd/mod1", buffer,status)

             ! In order to ensure that the S_ISUID and S_ISGID bits are set, an application requiring this should use stat() after a
             ! successful chmod() to verify this.

             !    Any files currently open could possibly become invalid if the mode
             !    of the file is changed to a value which would deny access to
             !    that process.

          end program demo_system_chmod
