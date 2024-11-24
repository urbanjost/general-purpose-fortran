module GPF
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, GPF!"
  end subroutine say_hello
end module GPF
