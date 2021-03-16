










! generic_list.f90 -- A Generic Linked List Implementation in Fortran 95
!
! Copyright (C) 2009, 2012 Jason R. Blevins
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE.

! Revision History:
!
! 1. July 21, 2012: In the list_free subroutine, line 11 should read
! nullify(current%data) instead of nullify(self%data).  Thanks to
! Michael Quinlan.
!>
!!##NAME
!!    M_generic_list(3f) - [M_generic_list] A Generic Linked List Implementation in Fortran 95
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!   public :: list_node_t, list_data
!!   public :: list_init, list_free
!!   public :: list_insert, list_put, list_get, list_next
!!##DESCRIPTION
!!
!!    A linked list, or more specifically a singly-linked list, is a list
!!    consisting of a series of individual node elements where each
!!    node contains a data member and a pointer that points to the next
!!    node in the list. M_generic_list(3fm) defines a generic linked list
!!    implementation in standard Fortran 95 which is able to store arbitrary
!!    data (and in particular -- pointers to arbitrary data types).
!!
!!##AUTHOR
!!    M_generic_list(3fm) defines a Generic Linked List Implementation in Fortran 95.
!!    This module, described in detail at http://fortranwiki.org is by:
!!
!!     Jason R. Blevins
!!     Department of Economics, Duke University
!!     May18,2009
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     ! program demo_M_generic_list and module
!!     module data
!!       implicit none
!!
!!       private
!!       public :: data_t
!!       public :: data_ptr
!!
!!       ! Data is stored in data_t
!!       type :: data_t
!!          real :: x
!!       end type data_t
!!
!!       ! A container for storing data_t pointers
!!       type :: data_ptr
!!          type(data_t), pointer :: p
!!       end type data_ptr
!!
!!     end module data
!!
!!     program demo_M_generic_list
!!       use M_generic_list
!!       use data
!!       implicit none
!!
!!       type(list_node_t), pointer :: list => null()
!!       type(data_ptr) :: ptr
!!
!!       ! Allocate a new data element
!!       allocate(ptr%p)
!!       ptr%p%x = 2.7183
!!
!!       ! Initialize the list with the first data element
!!       call list_init(list, transfer(ptr, list_data))
!!       print *, 'Initializing list with data:', ptr%p
!!
!!       ! Allocate a second data element
!!       allocate(ptr%p)
!!       ptr%p%x = 0.5772
!!
!!       ! Insert the second into the list
!!       call list_insert(list, transfer(ptr, list_data))
!!       print *, 'Inserting node with data:', ptr%p
!!
!!       ! Retrieve data from the second node and free memory
!!       ptr = transfer(list_get(list_next(list)), ptr)
!!       print *, 'Second node data:', ptr%p
!!       deallocate(ptr%p)
!!
!!       ! Retrieve data from the head node and free memory
!!       ptr = transfer(list_get(list), ptr)
!!       print *, 'Head node data:', ptr%p
!!       deallocate(ptr%p)
!!
!!       ! Free the list
!!       call list_free(list)
!!     end program demo_M_generic_list
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_generic_list
implicit none

private
public :: list_node_t, list_data
public :: list_init, list_free
public :: list_insert, list_put, list_get, list_next

public test_suite_M_generic_list

! A public variable used as a MOLD for transfer()
integer, dimension(:), allocatable :: list_data

! Linked list node
type :: list_node_t
   private
   integer, dimension(:), pointer :: data => null()
   type(list_node_t), pointer :: next => null()
end type list_node_t

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    list_init(3f) - [M_generic_list] Initialize a head node SELF and optionally store the provided DATA.
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!    subroutine list_init(self, data)
!!
!!     type(list_node_t), pointer :: self
!!     integer, dimension(:), intent(in), optional :: data
!!##DESCRIPTION
!!    Initialize a head node SELF and optionally store the provided DATA.
!!
!!##AUTHOR
!!    Fortran 95 Implementation by:
!!
!!     JASON R. BLEVINS
!!     Department of Economics, Duke University
!!     May 18,2009
!!##SEE ALSO
!!   M_generic_list(3fm), list_free(3f)
!!   list_insert(3f), list_put(3f), list_get(3f), list_next(3f)
!!##EXAMPLE
!!
!!    Sample program:
!!
!!      ! program demo_list_init and module
!!      module data
!!        implicit none
!!
!!        private
!!        public :: data_t
!!        public :: data_ptr
!!
!!        ! Data is stored in data_t
!!        type :: data_t
!!           real :: x
!!        end type data_t
!!
!!        ! A container for storing data_t pointers
!!        type :: data_ptr
!!           type(data_t), pointer :: p
!!        end type data_ptr
!!
!!      end module data
!!
!!      program demo_list_init
!!      use M_generic_list
!!      use data
!!      implicit none
!!      type(list_node_t), pointer :: list => null()
!!      type(data_ptr) :: ptr
!!        ! Allocate a new data element
!!        allocate(ptr%p)
!!        ptr%p%x = 2.7183
!!        ! Initialize the list with the first data element
!!        call list_init(list, transfer(ptr, list_data))
!!        print *, 'Initializing list with data:'   , ptr%p
!!        ! Allocate a second data element
!!        allocate(ptr%p)
!!        ptr%p%x = 0.5772
!!        ! Insert the second into the list
!!        call list_insert(list, transfer(ptr, list_data))
!!        print *, 'Inserting node with data:'   , ptr%p
!!        ! Retrieve data from the second node and free memory
!!        ptr = transfer(list_get(list_next(list)), ptr)
!!        print *,    'Second node data:'   , ptr%p
!!        deallocate(ptr%p)
!!        ! Retrieve data from the head node and free memory
!!        ptr = transfer(list_get(list), ptr)
!!        print *, 'Head node data:'   , ptr%p
!!        deallocate(ptr%p)
!!        ! Free the list
!!        call list_free(list)
!!      end program demo_list_init
!!
!!    The test program produces the following output:
!!
!!     Initializing list with data:    2.7183001
!!     Inserting node with data: 0.57720000
!!     Second node data: 0.57720000
!!     Head node data:   2.7183001
subroutine list_init(self, data)

! ident_1="@(#)M_generic_list::list_init(3f): Initialize a head node SELF and optionally store the provided DATA."

type(list_node_t), pointer :: self
integer, dimension(:), intent(in), optional :: data

   allocate(self)
   nullify(self%next)

   if (present(data)) then
      allocate(self%data(size(data)))
      self%data = data
   else
      nullify(self%data)
   end if

end subroutine list_init
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    list_free(3f) - [M_generic_list] Free the entire list and all data, beginning at SELF
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##AUTHOR
!!    Fortran 95 Implementation by:
!!
!!     JASON R. BLEVINS
!!     Department of Economics, Duke University
!!     May 18,2009
!!##SEE ALSO
!!   M_generic_list(3fm), list_init(3f)
!!   list_insert(3f), list_put(3f), list_get(3f), list_next(3f)
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    ! program demo_list_free and module
!!    module data
!!    implicit none
!!
!!    private
!!    public :: data_t
!!    public :: data_ptr
!!
!!    ! Data is stored in data_t
!!    type :: data_t
!!       real :: x
!!    end type data_t
!!
!!    ! A container for storing data_t pointers
!!    type :: data_ptr
!!       type(data_t), pointer :: p
!!    end type data_ptr
!!
!!    end module data
!!
!!    program demo_list_free
!!    use M_generic_list
!!    use data
!!    implicit none
!!
!!    type(list_node_t), pointer :: list => null()
!!    type(data_ptr) :: ptr
!!
!!       ! Allocate a new data element
!!       allocate(ptr%p)
!!       ptr%p%x = 2.7183
!!
!!       ! Initialize the list with the first data element
!!       call list_init(list, transfer(ptr, list_data))
!!       print *,"Initializing list with data:", ptr%p
!!
!!       ! Allocate a second data element
!!       allocate(ptr%p)
!!       ptr%p%x = 0.5772
!!
!!       ! Insert the second into the list
!!       call list_insert(list, transfer(ptr, list_data))
!!       print *,"Inserting node with data:", ptr%p
!!
!!       ! Retrieve data from the second node and free memory
!!       ptr = transfer(list_get(list_next(list)), ptr)
!!       print *,"Second node data:", ptr%p
!!       deallocate(ptr%p)
!!
!!       ! Retrieve data from the head node and free memory
!!       ptr = transfer(list_get(list), ptr)
!!       print *,"Head node data:", ptr%p
!!       deallocate(ptr%p)
!!
!!       ! Free the list
!!       call list_free(list)
!!    end program demo_list_free
!!
!!    !  The test program produces the following output:
!!    !
!!    !   Initializing list with data:    2.7183001
!!    !   Inserting node with data: 0.57720000
!!    !   Second node data: 0.57720000
!!    !   Head node data:   2.7183001
subroutine list_free(self)

! ident_2="@(#)M_generic_list::list_free(3f): Free the entire list and all data, beginning at SELF"

type(list_node_t), pointer :: self
type(list_node_t), pointer :: current
type(list_node_t), pointer :: next

   current => self
   do while (associated(current))
      next => current%next
      if (associated(current%data)) then
         deallocate(current%data)
         nullify(current%data)
      end if
      deallocate(current)
      nullify(current)
      current => next
   end do

end subroutine list_free
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    list_insert(3f) - [M_generic_list] Insert a list node after SELF containing DATA (optional)
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!    subroutine list_insert(self, data)
!!
!!     type(list_node_t), pointer :: self
!!     integer, dimension(:), intent(in), optional :: data
!!     type(list_node_t), pointer :: next
!!
!!##DESCRIPTION
!!##AUTHOR
!!    Fortran 95 Implementation by:
!!
!!     JASON R. BLEVINS
!!     Department of Economics, Duke University
!!     May 18,2009
!!##SEE ALSO
!!   M_generic_list(3fm), list_init(3f), list_free(3f)
!!   list_put(3f), list_get(3f), list_next(3f)
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    ! program demo_list_insert and module
!!    module data
!!    implicit none
!!
!!    private
!!    public :: data_t
!!    public :: data_ptr
!!
!!    ! Data is stored in data_t
!!    type :: data_t
!!       real :: x
!!    end type data_t
!!
!!    ! A container for storing data_t pointers
!!    type :: data_ptr
!!       type(data_t), pointer :: p
!!    end type data_ptr
!!
!!    end module data
!!
!!    program demo_list_insert
!!    use M_generic_list
!!    use data
!!    implicit none
!!
!!    type(list_node_t), pointer :: list => null()
!!    type(data_ptr) :: ptr
!!
!!       ! Allocate a new data element
!!       allocate(ptr%p)
!!       ptr%p%x = 2.7183
!!
!!       ! Initialize the list with the first data element
!!       call list_init(list, transfer(ptr, list_data))
!!       print *,"Initializing list with data:", ptr%p
!!
!!       ! Allocate a second data element
!!       allocate(ptr%p)
!!       ptr%p%x = 0.5772
!!
!!       ! Insert the second into the list
!!       call list_insert(list, transfer(ptr, list_data))
!!       print *,"Inserting node with data:", ptr%p
!!
!!       ! Retrieve data from the second node and free memory
!!       ptr = transfer(list_get(list_next(list)), ptr)
!!       print *,"Second node data:", ptr%p
!!       deallocate(ptr%p)
!!
!!       ! Retrieve data from the head node and free memory
!!       ptr = transfer(list_get(list), ptr)
!!       print *,"Head node data:", ptr%p
!!       deallocate(ptr%p)
!!
!!       ! Free the list
!!       call list_free(list)
!!    end program demo_list_insert
!!
!!    !  The test program produces the following output:
!!    !
!!    !   Initializing list with data:    2.7183001
!!    !   Inserting node with data: 0.57720000
!!    !   Second node data: 0.57720000
!!    !   Head node data:   2.7183001
subroutine list_insert(self, data)

! ident_3="@(#)M_generic_list::list_insert(3f): Insert a list node after SELF containing DATA (optional)"

type(list_node_t), pointer :: self
integer, dimension(:), intent(in), optional :: data
type(list_node_t), pointer :: next

   allocate(next)

   if (present(data)) then
      allocate(next%data(size(data)))
      next%data = data
   else
      nullify(next%data)
   end if

   next%next => self%next
   self%next => next

end subroutine list_insert
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    list_put(3f) - [M_generic_list] Store the encoded DATA in list node SELF
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!    subroutine list_put(self, data)
!!
!!     type(list_node_t), pointer :: self
!!     integer, dimension(:), intent(in) :: data
!!##DESCRIPTION
!!##AUTHOR
!!    Fortran 95 Implementation by:
!!
!!     JASON R. BLEVINS
!!     Department of Economics, Duke University
!!     May 18,2009
!!##SEE ALSO
!!   M_generic_list(3fm), list_init(3f), list_free(3f)
!!   list_insert(3f), list_get(3f), list_next(3f)
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    ! program demo_list_put and module
!!    module data
!!    implicit none
!!
!!    private
!!    public :: data_t
!!    public :: data_ptr
!!
!!    ! Data is stored in data_t
!!    type :: data_t
!!       real :: x
!!    end type data_t
!!
!!    ! A container for storing data_t pointers
!!    type :: data_ptr
!!       type(data_t), pointer :: p
!!    end type data_ptr
!!
!!    end module data
!!
!!    program demo_list_put
!!    use M_generic_list
!!    use data
!!    implicit none
!!
!!    type(list_node_t), pointer :: list => null()
!!    type(data_ptr) :: ptr
!!
!!       ! Allocate a new data element
!!       allocate(ptr%p)
!!       ptr%p%x = 2.7183
!!
!!       ! Initialize the list with the first data element
!!       call list_init(list, transfer(ptr, list_data))
!!       print *,"Initializing list with data:", ptr%p
!!
!!       ! Allocate a second data element
!!       allocate(ptr%p)
!!       ptr%p%x = 0.5772
!!
!!       ! Insert the second into the list
!!       call list_insert(list, transfer(ptr, list_data))
!!       print *,"Inserting node with data:", ptr%p
!!
!!       ! Retrieve data from the second node and free memory
!!       ptr = transfer(list_get(list_next(list)), ptr)
!!       print *,"Second node data:", ptr%p
!!       deallocate(ptr%p)
!!
!!       ! Retrieve data from the head node and free memory
!!       ptr = transfer(list_get(list), ptr)
!!       print *,"Head node data:", ptr%p
!!       deallocate(ptr%p)
!!
!!       ! Free the list
!!       call list_free(list)
!!    end program demo_list_put
!!
!!    !  The test program produces the following output:
!!    !
!!    !   Initializing list with data:    2.7183001
!!    !   Inserting node with data: 0.57720000
!!    !   Second node data: 0.57720000
!!    !   Head node data:   2.7183001
subroutine list_put(self, data)

! ident_4="@(#)M_generic_list::list_put(3f): Store the encoded DATA in list node SELF"

type(list_node_t), pointer :: self
integer, dimension(:), intent(in) :: data

   if (associated(self%data)) then
      deallocate(self%data)
      nullify(self%data)
   end if
   self%data = data

end subroutine list_put
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    list_get(3f) - [M_generic_list] Return the DATA stored in the node SELF
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!    function list_get(self) result(data)
!!
!!     type(list_node_t), pointer :: self
!!     integer, dimension(:), pointer :: data
!!
!!##DESCRIPTION
!!    Return the DATA stored in the node SELF
!!##AUTHOR
!!    Fortran 95 Implementation by:
!!
!!     JASON R. BLEVINS
!!     Department of Economics, Duke University
!!     May 18,2009
!!##SEE ALSO
!!   M_generic_list(3fm), list_init(3f), list_free(3f)
!!   list_insert(3f), list_put(3f), list_next(3f)
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    ! program demo_list_get and module
!!    module data
!!    implicit none
!!
!!    private
!!    public :: data_t
!!    public :: data_ptr
!!
!!    ! Data is stored in data_t
!!    type :: data_t
!!       real :: x
!!    end type data_t
!!
!!    ! A container for storing data_t pointers
!!    type :: data_ptr
!!       type(data_t), pointer :: p
!!    end type data_ptr
!!
!!    end module data
!!
!!    program demo_list_get
!!    use M_generic_list
!!    use data
!!    implicit none
!!
!!    type(list_node_t), pointer :: list => null()
!!    type(data_ptr) :: ptr
!!
!!       ! Allocate a new data element
!!       allocate(ptr%p)
!!       ptr%p%x = 2.7183
!!
!!       ! Initialize the list with the first data element
!!       call list_init(list, transfer(ptr, list_data))
!!       print *,"Initializing list with data:", ptr%p
!!
!!       ! Allocate a second data element
!!       allocate(ptr%p)
!!       ptr%p%x = 0.5772
!!
!!       ! Insert the second into the list
!!       call list_insert(list, transfer(ptr, list_data))
!!       print *,"Inserting node with data:", ptr%p
!!
!!       ! Retrieve data from the second node and free memory
!!       ptr = transfer(list_get(list_next(list)), ptr)
!!       print *,"Second node data:", ptr%p
!!       deallocate(ptr%p)
!!
!!       ! Retrieve data from the head node and free memory
!!       ptr = transfer(list_get(list), ptr)
!!       print *,"Head node data:", ptr%p
!!       deallocate(ptr%p)
!!
!!       ! Free the list
!!       call list_free(list)
!!    end program demo_list_get
!!
!!    !  The test program produces the following output:
!!    !
!!    !   Initializing list with data:    2.7183001
!!    !   Inserting node with data: 0.57720000
!!    !   Second node data: 0.57720000
!!    !   Head node data:   2.7183001
function list_get(self) result(data)

! ident_5="@(#)M_generic_list::list_put(3f): Return the DATA stored in the node SELF"

type(list_node_t), pointer :: self
integer, dimension(:), pointer :: data

    data => self%data

end function list_get
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    list_next(3f) - [M_generic_list] Return the next node after SELF
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!    function list_next(self)
!!
!!     type(list_node_t), pointer :: self
!!     type(list_node_t), pointer :: list_next
!!
!!##DESCRIPTION
!!##AUTHOR
!!    Fortran 95 Implementation by:
!!
!!     JASON R. BLEVINS
!!     Department of Economics, Duke University
!!     May 18,2009
!!##SEE ALSO
!!   M_generic_list(3fm), list_init(3f), list_free(3f)
!!   list_insert(3f), list_put(3f), list_get(3f)
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    ! program demo_list_next and module
!!    module data
!!    implicit none
!!
!!    private
!!    public :: data_t
!!    public :: data_ptr
!!
!!    ! Data is stored in data_t
!!    type :: data_t
!!       real :: x
!!    end type data_t
!!
!!    ! A container for storing data_t pointers
!!    type :: data_ptr
!!       type(data_t), pointer :: p
!!    end type data_ptr
!!
!!    end module data
!!
!!    program demo_list_next
!!    use M_generic_list
!!    use data
!!    implicit none
!!
!!    type(list_node_t), pointer :: list => null()
!!    type(data_ptr) :: ptr
!!
!!       ! Allocate a new data element
!!       allocate(ptr%p)
!!       ptr%p%x = 2.7183
!!
!!       ! Initialize the list with the first data element
!!       call list_init(list, transfer(ptr, list_data))
!!       print *,"Initializing list with data:", ptr%p
!!
!!       ! Allocate a second data element
!!       allocate(ptr%p)
!!       ptr%p%x = 0.5772
!!
!!       ! Insert the second into the list
!!       call list_insert(list, transfer(ptr, list_data))
!!       print *,"Inserting node with data:", ptr%p
!!
!!       ! Retrieve data from the second node and free memory
!!       ptr = transfer(list_get(list_next(list)), ptr)
!!       print *,"Second node data:", ptr%p
!!       deallocate(ptr%p)
!!
!!       ! Retrieve data from the head node and free memory
!!       ptr = transfer(list_get(list), ptr)
!!       print *,"Head node data:", ptr%p
!!       deallocate(ptr%p)
!!
!!       ! Free the list
!!       call list_free(list)
!!    end program demo_list_next
!!
!!    !  The test program produces the following output:
!!    !
!!    !   Initializing list with data:    2.7183001
!!    !   Inserting node with data: 0.57720000
!!    !   Second node data: 0.57720000
!!    !   Head node data:   2.7183001
function list_next(self)

! ident_6="@(#)M_generic_list::list_put(3f): Return the next node after SELF"

type(list_node_t), pointer :: self
type(list_node_t), pointer :: list_next

    list_next => self%next

end function list_next
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_generic_list()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level

!! setup
   call test_list_free()
   call test_list_get()
   call test_list_init()
   call test_list_insert()
   call test_list_next()
   call test_list_put()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_list_free()

   call unit_check_start('list_free',msg='')
   !!call unit_check('list_free', 0.eq.0, 'checking',100)
   call unit_check_done('list_free',msg='')
end subroutine test_list_free
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_list_get()

   call unit_check_start('list_get',msg='')
   !!call unit_check('list_get', 0.eq.0, 'checking',100)
   call unit_check_done('list_get',msg='')
end subroutine test_list_get
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_list_init()

   call unit_check_start('list_init',msg='')
   !!call unit_check('list_init', 0.eq.0, 'checking',100)
   call unit_check_done('list_init',msg='')
end subroutine test_list_init
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_list_insert()

   call unit_check_start('list_insert',msg='')
   !!call unit_check('list_insert', 0.eq.0, 'checking',100)
   call unit_check_done('list_insert',msg='')
end subroutine test_list_insert
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_list_next()

   call unit_check_start('list_next',msg='')
   !!call unit_check('list_next', 0.eq.0, 'checking',100)
   call unit_check_done('list_next',msg='')
end subroutine test_list_next
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_list_put()

   call unit_check_start('list_put',msg='')
   !!call unit_check('list_put', 0.eq.0, 'checking',100)
   call unit_check_done('list_put',msg='')
end subroutine test_list_put
!===================================================================================================================================
end subroutine test_suite_M_generic_list
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_generic_list
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
