!> This module loads a Lua file to read values and execute functions.
!>
!!##NAME
!!    M_lua(3fm) - [M_lua] Fortran interface to the LUA language
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!
!!
!!    This is a fork of a simple set of Lua bindings for use in Fortran,
!!    tested to be compatible with Lua 5.2.3+, based on FORTLUA 5.2.
!!
!!    Most of the original basis work is fully attributed to @adolgert and
!!    @adambrozier and creators of Aotus (see attribution clause below).
!!
!!    MOTIVATION
!!
!!    This project shows how to call a Lua file from Fortran. You can
!!    also call a Lua file from C, but doing it from an even older language
!!    seems more dramatic. This also demonstrates how to call C libraries
!!    directly from Fortran using the iso_c_binding.
!!
!!    COMPILATION
!!
!!    This project assumes that the Lua library is available on your system.
!!
!!    DISCUSSION
!!
!!    Why call Lua from Fortran? I can think of two good uses for this.
!!
!!    You could use a Lua file as a configuration file. You can query
!!    environment variables and do pre-calculations in the configuration
!!    file, yielding a result for Fortran to use. You can even pass in
!!    functions for Fortran to execute.
!!
!!    You could also write a commonly-modified subroutine in Lua so that
!!    there is no need to recompile the code when you make changes.
!!
!!    Why Lua? The language is relatively simple and has basic math
!!    included. It's made to embed in programs. That said, it is not a
!!    common language. You could do the same exercise with Gnu Guile.
!!
!!    How? Lua is a very small language that is built to be embedded.
!!    It passes all of its state back to the calling program through a
!!    stack (the stack data structure, but stored on the heap) so that it
!!    is simple to retrieve.
!!
!!    We use Fortran's iso_c_binding to call the Lua C library directly
!!    from Fortran. That binding is a relatively new development in Fortran,
!!    but it works fine.
!!
!!       Drew Dolgert
!!       adolgert@cornell.edu
!!       Adam Brazier
!!       brazier@cornell.edu
!!       Kevin Manalo
!!       kmanalo@gmail.com
!!
!!    This modification to FortLua was derived from routines provided by AOTUS, hence
!!    their attribution is listed below:
!!
!!    AOTUS LICENSE
!!
!!    Aotus is licensed under the terms of the MIT license reproduced below.
!!    This means that Aotus is free software and can be used for both academic and
!!    commercial purposes at absolutely no cost. You are free to do with the code
!!    whatever you want.
!!    The only requirement is that some credit to the authors is given by putting this
!!    copyright notice somewhere in your project.
!!    The MIT license is chosen for full compatibility with Lua.
!!
!!    For the license of the underlying Lua library have a look at
!!
!!       http://www.lua.org/license.html.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    ! This program uses a script to configure itself.
!!
!!    PROGRAM calculate
!!    USE M_lua
!!
!!    INTEGER :: argument_count
!!    REAL*8 :: pressure
!!    REAL*8 :: time
!!    INTEGER :: step, stepcount
!!    INTEGER :: status
!!    REAL :: temperature_start
!!    REAL, DIMENSION(1) :: pressure_args
!!    INTEGER :: read_status
!!    INTEGER :: cstatus
!!    integer :: newval
!!
!!    character(50) :: string
!!    character(:), allocatable :: astring
!!
!!    status = config_open('vals.lua')
!!
!!    call config_string('string',read_status, string)
!!    astring = trim(string)
!!
!!    write(*,*) 'string = ', astring
!!
!!    temperature_start = config_real('temperature',read_status)
!!    IF ( read_status .eq. 0 ) THEN
!!      WRITE (*,*) 'temperature = ', temperature_start
!!    ELSE
!!      WRITE (*,*) 'error reading temperature'
!!    ENDIF
!!
!!    newval = config_integer('newval',read_status)
!!    IF ( read_status .eq. 0 ) THEN
!!      WRITE (*,*) 'newval = ', newval
!!    ELSE
!!      WRITE (*,*) 'error reading newval'
!!    ENDIF
!!
!!    stepcount=10
!!
!!    DO step = 1,stepcount
!!      time = step/1.0
!!      pressure_args(1) = time
!!      pressure=config_function('pressure',pressure_args,1,cstatus)
!!      WRITE (*,*) 'pressure = ', pressure
!!    END DO
!!
!!
!!    CALL config_close()
!!
!!    STOP
!!    END PROGRAM calculate
!!
!!   Input file
!!
!!    -- vals.lua
!!    -- This is a configuration file for the Fortran program.
!!    -- Lua is not too complicated. Check out
!!    -- http://lua-users.org/wiki/TutorialDirectory
!!
!!    -- Parameters
!!    if os.getenv("BATCH") then
!!       temperature=3.2
!!    else
!!       temperature=5.0
!!    end
!!
!!    mintemp=3
!!    maxtemp=6
!!    duration=10 -- time to get to 95%
!!    string=10 -- string will be interpreted because that is what this program is looking for
!!    newval=10.5
!!
!!    function pressure(time)
!!      return mintemp+(maxtemp-mintemp)*math.tanh(1.83*time/duration)
!!    end
!!
!>
!!
!!
!! ====================================================================================================================================
!!
!! Copyright (C) 2011-2013 German Research School for Simulation Sciences GmbH,
!!                         Aachen and others.
!!               2013-2015 University of Siegen.
!!
!! Permission is hereby granted, free of charge, to any person obtaining a copy
!! of this software and associated documentation files (the "Software"), to deal
!! in the Software without restriction, including without limitation the rights
!! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
!! copies of the Software, and to permit persons to whom the Software is
!! furnished to do so, subject to the following conditions:
!!
!! The above copyright notice and this permission notice shall be included in
!! all copies or substantial portions of the Software.
!!
!!##THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
!!##IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
!!##FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
!!##AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
!!##LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
!!##OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
!!##THE SOFTWARE.
!!
!! ====================================================================================================================================
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
MODULE M_lua
  ! The USE statement comes first
  USE iso_c_binding, only: C_CHAR, C_NULL_CHAR, C_INT, C_PTR, &
                           C_FUNPTR, C_DOUBLE
  IMPLICIT NONE
  PRIVATE mluastate !< This is an opaque pointer to the Lua interpreter.
  public :: test_suite_M_lua

  ! Module scope variables
  ! The lua_State pointer is stored opaquely in Fortran in this
  ! module-level variable.
  TYPE(c_ptr) :: mluastate
  INTEGER(c_int) :: LUA_IDSIZE = 60

  INTEGER(c_int) :: LUA_TNIL = 0
  INTEGER(c_int) :: LUA_TBOOLEAN = 1
  INTEGER(c_int) :: LUA_TLIGHTUSERDATA = 2
  INTEGER(c_int) :: LUA_TNUMBER = 3
  INTEGER(c_int) :: LUA_TSTRING = 4
  INTEGER(c_int) :: LUA_TTABLE = 5
  INTEGER(c_int) :: LUA_TFUNCTION = 6
  INTEGER(c_int) :: LUA_TUSERDATA = 7
  INTEGER(c_int) :: LUA_TTHREAD = 8


  INTERFACE
    ! This interface is a subset of the Lua interface, but you
    ! get the point.
    !
    ! This uses some of Fortran's ability to bind directly to C.
    ! The value option tells Fortran not to pass the argument
    ! using a pointer to the argument.
    ! For strings, it looks like the proper declaration is
    ! an array of CHARACTER(KIND=c_char) but that Fortran will
    ! happily translate CHARACTER(KIND=c_char,LEN=*) to the
    ! array of single chars.


    FUNCTION luaL_newstate() bind(C,name="luaL_newstate")
      USE iso_c_binding, only: c_ptr, c_funptr
      TYPE(c_ptr) :: luaL_newstate
    END FUNCTION luaL_newstate

    SUBROUTINE lua_close(lstate) bind(C,name="lua_close")
      USE iso_c_binding, only: c_ptr
      TYPE(c_ptr), value :: lstate
    END SUBROUTINE lua_close

    SUBROUTINE luaL_openlibs(lstate) bind(C,name="luaL_openlibs")
      USE iso_c_binding, only: c_ptr
      TYPE(c_ptr), value :: lstate
    END SUBROUTINE luaL_openlibs

    function luaL_loadfilex(L, filename, mode) bind(c, name="luaL_loadfilex")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      character(kind=c_char), dimension(*) :: filename
      character(kind=c_char), dimension(*) :: mode
      integer(kind=c_int) :: luaL_loadfilex
    end function luaL_loadfilex

    FUNCTION lua_gettop(lstate) bind(C, name="lua_gettop")
      USE iso_c_binding, only: c_int, c_ptr
      INTEGER(c_int) :: lua_gettop
      TYPE(c_ptr), value :: lstate
    END FUNCTION lua_gettop

    FUNCTION lua_type(lstate, stackIdx) bind(C,name="lua_type")
      USE iso_c_binding, only: c_int, c_ptr
      INTEGER(c_int) :: lua_type
      TYPE(c_ptr), value :: lstate
      INTEGER(c_int), value :: stackIdx
    END FUNCTION lua_type

    FUNCTION lua_checkstack(lstate, stackIdx) bind(C,name="lua_checkstack")
      USE iso_c_binding, only: c_int, c_ptr
      INTEGER(c_int) :: lua_checkstack
      TYPE(c_ptr), value :: lstate
      INTEGER(c_int), value :: stackIdx
    END FUNCTION lua_checkstack

    subroutine lua_getglobal(L, k) bind(c, name="lua_getglobal")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      character(kind=c_char), dimension(*) :: k
    end subroutine lua_getglobal

    !> Set the top of the stack.
    !! lua_pop is defined as lua_settop(L,-(n)-1) in a macro for C.
    SUBROUTINE lua_settop(lstate,stackIdx) bind(C,name="lua_settop")
      USE iso_c_binding, only: c_ptr, c_int
      TYPE(c_ptr), value :: lstate
      INTEGER(c_int), value :: stackIdx
    END SUBROUTINE lua_settop

    SUBROUTINE lua_pushnumber(lstate,setval) bind(C,name="lua_pushnumber")
      USE iso_c_binding, only: c_ptr, c_double
      TYPE(c_ptr), value :: lstate
      REAL(c_double), value :: setval
    END SUBROUTINE lua_pushnumber

    function lua_pcallk(L, nargs, nresults, errfunc, ctx, k) bind(c, name="lua_pcallk")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: nargs
      integer(kind=c_int), value :: nresults
      integer(kind=c_int), value :: errfunc
      integer(kind=c_int), value :: ctx
      type(c_ptr), value :: k
      integer(kind=c_int) :: lua_pcallk
    end function lua_pcallk

    FUNCTION lua_isfunction(lstate,stackIdx) bind(C,name="lua_isfunction")
      USE iso_c_binding, only: c_ptr, c_int
      INTEGER(c_int) :: lua_isfunction
      TYPE(c_ptr), value :: lstate
      INTEGER(c_int), value :: stackIdx
    END FUNCTION lua_isfunction

    FUNCTION lua_isnumber(lstate,stackIdx) bind(C,name="lua_isnumber")
      USE iso_c_binding, only: c_ptr, c_int
      INTEGER(c_int) :: lua_isnumber
      TYPE(c_ptr), value :: lstate
      INTEGER(c_int), value :: stackIdx
    END FUNCTION lua_isnumber

    function lua_tonumberx(L, index, isnum) bind(c, name="lua_tonumberx")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
      integer(kind=c_int) :: isnum
      real(kind=c_double) :: lua_tonumberx
    end function lua_tonumberx

    function lua_tolstring(L, index, length) bind(c, name="lua_tolstring")
      use, intrinsic :: iso_c_binding
      type(c_ptr), value :: L
      integer(kind=c_int), value :: index
      integer(kind=c_size_t) :: length
      type(c_ptr):: lua_tolstring
    end function lua_tolstring

    FUNCTION lua_tointeger(lstate,stackIdx) bind(C,name="lua_tointeger")
      USE iso_c_binding, only: c_ptr, c_int, c_size_t
      INTEGER(c_size_t) :: lua_tointeger
      TYPE(c_ptr), value :: lstate
      INTEGER(c_int), value :: stackIdx
    END FUNCTION lua_tointeger

  END INTERFACE

  CONTAINS
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
    function luaL_loadfile(lstate, filename) result(errcode)
      use, intrinsic :: iso_c_binding
      TYPE(c_ptr), value :: lstate
      character(len=*) :: filename
      integer :: errcode

      character(len=len_trim(filename)+1) :: c_filename
      character(len=3) :: c_mode
      integer(kind=c_int) :: c_errcode

      c_filename = trim(filename) // c_null_char
      c_mode = "bt" // c_null_char
      c_errcode = luaL_loadfilex(lstate, c_filename, c_mode)
      errcode = c_errcode
    end function luaL_loadfile
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
    function lua_pcall(lstate, nargs, nresults, errfunc) result(errcode)
      use, intrinsic :: iso_c_binding
      TYPE(c_ptr), value :: lstate
      integer :: nargs
      integer :: nresults
      integer :: errfunc
      integer :: errcode

      integer(kind=c_int) :: c_nargs
      integer(kind=c_int) :: c_nresults
      integer(kind=c_int) :: c_errfunc
      integer(kind=c_int) :: c_errcode

      c_nargs = nargs
      c_nresults = nresults
      c_errfunc = errfunc

      c_errcode = lua_pcallk(lstate, c_nargs, c_nresults, c_errfunc, &
        &                    0_c_int, C_NULL_PTR)
      errcode = c_errcode
    end function lua_pcall
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
    function lua_tonumber(lstate, index) result(number)
      use, intrinsic :: iso_c_binding
      TYPE(c_ptr), value :: lstate
      integer :: index
      real :: number

      integer(kind=c_int) :: c_index
      integer(kind=c_int) :: isnum

      c_index = index
      number = real(lua_tonumberx(lstate, c_index, isnum), &
        &           kind=kind(number))
    end function lua_tonumber
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
    function lua_tostring(lstate, index, len) result(string)
      use, intrinsic :: iso_c_binding
      TYPE(c_ptr), value :: lstate
      integer :: index
      integer :: len
      character,pointer,dimension(:) :: string

      integer :: string_shape(1)
      integer(kind=c_int) :: c_index
      integer(kind=c_size_t) :: c_len
      type(c_ptr) :: c_string

      c_index = index
      c_string = lua_tolstring(lstate, c_index, c_len)
      len = int(c_len,kind=kind(len))
      string_shape(1) = len
      call c_f_pointer(c_string, string, string_shape)
    end function lua_tostring
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
  !> Open a Lua configuration file by name.
  !! The state of the Lua file is held in the module and must be
  !! closed when you are done.
  INTEGER FUNCTION config_open(fname)
    CHARACTER(LEN=*) :: fname
    INTEGER(c_int) :: filesuccess, callsuccess

    mluastate=luaL_newstate()
    CALL luaL_openlibs(mluastate)

    filesuccess = luaL_loadfile(mluastate, TRIM(fname)//C_NULL_CHAR)
    IF ( filesuccess .eq. 0 ) THEN
      callsuccess = lua_pcall(mluastate,0,0,0)
      IF ( callsuccess .eq. 0 ) THEN
        ! This is equivalent to the macro lua_pop.
        CALL lua_settop(mluastate,-2)
        config_open=1
      ELSE
         config_open=0
      ENDIF
    ELSE
      config_open=0
    ENDIF

  END FUNCTION config_open
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
  !> Close the Lua configuration, which releases the interpreter.
  SUBROUTINE config_close
    call lua_close(mluastate)
  END SUBROUTINE config_close
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
  !> Retrieve the value of a character array
  subroutine config_string(name,status,string)
    character(len=*), intent(out) :: string
    character, dimension(:), allocatable :: mystring
    CHARACTER(LEN=*) :: name
    INTEGER :: status
    integer :: length, i
    !INTEGER(c_int) :: stackstart
    character, pointer :: cstring(:)

    CALL lua_getglobal(mluastate,TRIM(name)//C_NULL_CHAR)

    !IF ( lua_isnumber(mluastate,-1) .NE. 0 ) THEN
      cstring => lua_tostring(mluastate,-1, length)
      allocate(mystring(length))
      do i=1,length
        string(i:i) = cstring(i)
        !mystring(i:i) = cstring(i)
      end do
      !print *, mystring

      ! This is the same as Lua pop 1.
      CALL lua_settop(mluastate,-2)
      status = 0
    !ELSE
    !  config_string=''
    !  status = -1
    !ENDIF
    !IF (stackstart .ne. lua_gettop(mluastate)) THEN
    !   WRITE(*,*) 'The stack is a different size coming out of config_real'
    !ENDIF

  END subroutine config_string
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
  !> Retrieve the value of a floating point variable.
  FUNCTION config_real(name,status)
    REAL :: config_real
    CHARACTER(LEN=*) :: name
    INTEGER :: status
    INTEGER(c_int) :: stackstart

    ! We compare the stack before and after our work to discover
    ! whether we have corrupted it. Otherwise debugging errors
    ! can be difficult.
    stackstart = lua_gettop(mluastate)

    !DBG CALL lua_getfield(mluastate,LUA_GLOBALSINDEX,TRIM(name)//C_NULL_CHAR)
    CALL lua_getglobal(mluastate,TRIM(name)//C_NULL_CHAR)

    IF ( lua_isnumber(mluastate,-1) .NE. 0 ) THEN
      config_real=lua_tonumber(mluastate,-1)
      ! This is the same as Lua pop 1.
      CALL lua_settop(mluastate,-2)
      status = 0
    ELSE
      config_real=0
      status = -1
    ENDIF
    IF (stackstart .ne. lua_gettop(mluastate)) THEN
       WRITE(*,*) 'The stack is a different size coming out of config_real'
    ENDIF

  END FUNCTION config_real
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
  !> Retrieve the value of an integer variable.
  FUNCTION config_integer(name,status)
    INTEGER :: config_integer
    CHARACTER(LEN=*) :: name
    INTEGER :: status
    INTEGER(c_int) :: stackstart

    stackstart = lua_gettop(mluastate)

    CALL lua_getglobal(mluastate,TRIM(name)//C_NULL_CHAR)

    IF ( lua_isnumber(mluastate,-1) .NE. 0 ) THEN
      config_integer=lua_tonumber(mluastate,-1)
      ! This is the same as Lua pop 1.
      CALL lua_settop(mluastate,-2)
      status = 0
    ELSE
      config_integer=0
      status = -1
    ENDIF
    IF (stackstart .ne. lua_gettop(mluastate)) THEN
       WRITE(*,*) 'The stack is a different size coming out of config_integer'
    ENDIF

  END FUNCTION config_integer
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
  !> Evaluate a function in the config file and get its result.
  FUNCTION config_function(name,args,nargs,status)
    REAL :: config_function
    CHARACTER(LEN=*) :: name
    REAL, DIMENSION(*) :: args
    REAL(KIND=c_double) :: anarg
    INTEGER :: nargs
    INTEGER :: status
    INTEGER :: iargs
    INTEGER(c_int) :: stackstart

    stackstart = lua_gettop(mluastate)

    config_function = 0


    CALL lua_getglobal(mluastate,TRIM(name)//C_NULL_CHAR)
    IF ( lua_type(mluastate,-1) .eq. LUA_TFUNCTION ) THEN
        DO iargs = 1,nargs
          anarg = args(iargs)
          CALL lua_pushnumber(mluastate,anarg)
        ENDDO
        IF (lua_pcall(mluastate,nargs,1,0) .eq. 0) THEN
          if (lua_isnumber(mluastate,-1) .ne. 0) THEN
            config_function = lua_tonumber(mluastate,-1)
            CALL lua_settop(mluastate,-2)
          ELSE
            ! Nothing to pop here
            status=-3
          ENDIF
        ELSE
          CALL lua_settop(mluastate,-2)
          status=-2
        ENDIF
    ELSE
        CALL lua_settop(mluastate,-2)
        status=-1
    ENDIF
    IF (stackstart .ne. lua_gettop(mluastate)) THEN
       WRITE(*,*) 'The stack is a different size coming out of config_function'
    ENDIF

  END FUNCTION config_function
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine test_suite_M_lua()
use M_verify, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
use M_verify, only : unit_test_level
implicit none
!! setup
   call test_config_close()
   call test_config_function()
   call test_config_integer()
   call test_config_open()
   call test_config_real()
   call test_config_string()
   call test_lua_pcall()
   call test_lua_tonumber()
   call test_lua_tostring()
   call test_lual_loadfile()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_config_close()
implicit none
   call unit_test_start('config_close',msg='')
   !!call unit_test('config_close', 0.eq.0, 'checking',100)
   call unit_test_done('config_close',msg='')
end subroutine test_config_close
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_config_function()
implicit none
   call unit_test_start('config_function',msg='')
   !!call unit_test('config_function', 0.eq.0, 'checking',100)
   call unit_test_done('config_function',msg='')
end subroutine test_config_function
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_config_integer()
implicit none
   call unit_test_start('config_integer',msg='')
   !!call unit_test('config_integer', 0.eq.0, 'checking',100)
   call unit_test_done('config_integer',msg='')
end subroutine test_config_integer
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_config_open()
implicit none
   call unit_test_start('config_open',msg='')
   !!call unit_test('config_open', 0.eq.0, 'checking',100)
   call unit_test_done('config_open',msg='')
end subroutine test_config_open
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_config_real()
implicit none
   call unit_test_start('config_real',msg='')
   !!call unit_test('config_real', 0.eq.0, 'checking',100)
   call unit_test_done('config_real',msg='')
end subroutine test_config_real
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_config_string()
implicit none
   call unit_test_start('config_string',msg='')
   !!call unit_test('config_string', 0.eq.0, 'checking',100)
   call unit_test_done('config_string',msg='')
end subroutine test_config_string
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lua_pcall()
implicit none
   call unit_test_start('lua_pcall',msg='')
   !!call unit_test('lua_pcall', 0.eq.0, 'checking',100)
   call unit_test_done('lua_pcall',msg='')
end subroutine test_lua_pcall
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lua_tonumber()
implicit none
   call unit_test_start('lua_tonumber',msg='')
   !!call unit_test('lua_tonumber', 0.eq.0, 'checking',100)
   call unit_test_done('lua_tonumber',msg='')
end subroutine test_lua_tonumber
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lua_tostring()
implicit none
   call unit_test_start('lua_tostring',msg='')
   !!call unit_test('lua_tostring', 0.eq.0, 'checking',100)
   call unit_test_done('lua_tostring',msg='')
end subroutine test_lua_tostring
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lual_loadfile()
implicit none
   call unit_test_start('lual_loadfile',msg='')
   !!call unit_test('lual_loadfile', 0.eq.0, 'checking',100)
   call unit_test_done('lual_loadfile',msg='')
end subroutine test_lual_loadfile
!===================================================================================================================================
end subroutine test_suite_M_lua
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
END MODULE M_lua
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
