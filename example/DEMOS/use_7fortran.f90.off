         ! program demo_use and module examples
         module example ! example is the namespace name
         use,intrinsic :: iso_fortran_env , only : real64

            type type1 ! type1 is the class prototype name
            contains
               procedure, nopass :: static_method1
            end type type1

            type type2 ! type1 is the class prototype name
            contains
               procedure, nopass :: static_method2
            end type type2

            real(kind=real64),parameter :: &
            pi  = 3.1415926535897932_real64
            ! Napier's constant is the base of the natural logarithm
            ! system. It is often denoted by "e" in honor of Euler.
            real(kind=real64),parameter :: &
            Napier_constant = 2.71828182845904523_real64

         contains

            subroutine static_method1(arg)
               integer :: arg
               ! code to implement method goes here
            end subroutine static_method1

            subroutine static_method2(arg)
               integer :: arg
               ! code to implement method goes here
            end subroutine static_method2

         end module example
         program demo_use
         use example, only: type1 ! class prototype type1 available,
                                  ! but nothing else is made available by this
                                  !
         ! (additionally) within this scoping unit, type1 is referred to
         ! as "mytype"
         use example, mytype => type1
         !
         ! only: is recommended but for long lists importing everything
         !       without listing it is supported:
         use example ! all public objects in namespace example available
         !
         ! some popular intrinsic entities
         !
         use,intrinsic :: iso_fortran_env, only : &
         stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
         ! specifying INTRINSIC or NON_INTRINSIC is typically optional but
         ! indicating INTRINSIC when it is so is the norm.
         use :: iso_fortran_env, only : integer_kinds,int8,int16,int32,int64
         use iso_fortran_env, only : real_kinds,real32,real64,real128
         ! duplicates are OK
         use,intrinsic :: iso_fortran_env, only : sp=>real32,dp=>real64
         use,intrinsic :: iso_fortran_env, only : integer_kinds
         use,intrinsic :: iso_fortran_env, only : compiler_version
         use,intrinsic :: iso_fortran_env, only : compiler_options
         use,intrinsic :: iso_fortran_env, only : iostat_eor, iostat_end
         end program demo_use
