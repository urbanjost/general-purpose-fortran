          ! program demo_list_put and module
          module data
          implicit none

          private
          public :: data_t
          public :: data_ptr

          ! Data is stored in data_t
          type :: data_t
             real :: x
          end type data_t

          ! A container for storing data_t pointers
          type :: data_ptr
             type(data_t), pointer :: p
          end type data_ptr

          end module data

          program demo_list_put
          use M_generic_list
          use data
          implicit none

          type(list_node_t), pointer :: list => null()
          type(data_ptr) :: ptr

             ! Allocate a new data element
             allocate(ptr%p)
             ptr%p%x = 2.7183

             ! Initialize the list with the first data element
             call list_init(list, transfer(ptr, list_data))
             print *,"Initializing list with data:", ptr%p

             ! Allocate a second data element
             allocate(ptr%p)
             ptr%p%x = 0.5772

             ! Insert the second into the list
             call list_insert(list, transfer(ptr, list_data))
             print *,"Inserting node with data:", ptr%p

             ! Retrieve data from the second node and free memory
             ptr = transfer(list_get(list_next(list)), ptr)
             print *,"Second node data:", ptr%p
             deallocate(ptr%p)

             ! Retrieve data from the head node and free memory
             ptr = transfer(list_get(list), ptr)
             print *,"Head node data:", ptr%p
             deallocate(ptr%p)

             ! Free the list
             call list_free(list)
       end program demo_list_put
