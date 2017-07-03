[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                             Manual Reference Pages  - intrinsics (7)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    intrinsics(7f) - intrinsic man(1) pages

CONTENTS

    Description
    Contents

DESCRIPTION

    Got tired of not having the Fortran intrinsics available as man(1) pages, especially when looking at code in the vim(1) editor
    and wanting to use the "K" command to evoke the man(1) page.

    So, as a start I took the closest thing I knew, which was descriptions on the Fortran Wiki, and am slowly turning them into man
    (1) pages and sometimes altering them.

    *Note*: In many cases the descriptions of these intrinsics were originally taken from the [[GFortran|GNU Fortran]] manual to
    make descriptions on the Fortran Wiki by Jason Blevins (which were then used to start this collection). Like the Fortran Wiki
    itself, the [[GFortran|GNU Fortran]] manual is licensed under the [[GNU Free Documentation License]].

    These are at the state of "good enough considering the alternative is nothing", but are still actively being completed.

CONTENTS

    Inquiry intrinsic functions

    The result of an inquiry function depends on the properties of its principal argument, not on the value of the argument. The
    value of the argument does not have to be defined.

        o  [[allocated]]--Status of an allocatable entity

        o  [[associated]]--Status of a pointer or pointer/target pair

        o  [[bit_size]]--Bit size inquiry function

        o  [[command_argument_count]]--Get number of command line arguments

        o  [[digits]]--Significant digits function

        o  [[epsilon]]--Epsilon function

        o  [[extends_type_of]]--Type extension inquiry

        o  [[huge]]--Largest number of a kind

        o  [[is_contiguous]]--True if and only if an object is contigous

        o  [[kind]]--Kind of an entity

        o  [[lbound]]--Lower dimension bounds of an array

        o  [[len]]--Length of a character entity

        o  [[maxexponent]]--Maximum exponent of a real kind

        o  [[minexponent]]--Minimum exponent of a real kind

        o  [[new_line]]--New line character

        o  [[precision]]--Decimal precision of a real kind

        o  [[present]]--Determine whether an optional dummy argument is specified

        o  [[radix]]--Base of a model number

        o  [[range]]--Decimal exponent range of a real kind

        o  [[same_type_as]]--Query dynamic types for equality

        o  [[shape]]--Determine the shape of an array

        o  [[size]]--Determine the size of an array

        o  [[tiny]]--Smallest positive number of a real kind

        o  [[ubound]]--Upper dimension bounds of an array

    Math functions

        o  [[abs]]--Absolute value

        o  [[acos]]--Arccosine function

        o  [[acosh]]--Inverse hyperbolic cosine function

        o  [[asin]]--Arcsine function

        o  [[asinh]]--Inverse hyperbolic sine function

        o  [[atan]]--Arctangent function

        o  [[atan2]]--Arctangent function

        o  [[atanh]]--Inverse hyperbolic tangent function

        o  [[bessel_j0]]--Bessel function of the first kind of order 0

        o  [[bessel_j1]]--Bessel function of the first kind of order 1

        o  [[bessel_jn]]--Bessel function of the first kind

        o  [[bessel_y0]]--Bessel function of the second kind of order 0

        o  [[bessel_y1]]--Bessel function of the second kind of order 1

        o  [[bessel_yn]]--Bessel function of the second kind

        o  [[cos]]--Cosine function

        o  [[cosh]]--Hyperbolic cosine function

        o  [[erf]]--Error function

        o  [[erfc]]--Complementary error function

        o  [[erfc_scaled]]--Error function

        o  [[gamma]]--Gamma function

        o  [[hypot]]--Euclidean distance function

        o  [[log]]--Logarithm function

        o  [[log10]]--Base 10 logarithm function

        o  [[log_gamma]]--Logarithm of the Gamma function

        o  [[sin]]--Sine function

        o  [[sinh]]--Hyperbolic sine function

        o  [[sqrt]]--Square-root function

        o  [[tan]]--Tangent function

        o  [[tanh]]--Hyperbolic tangent function

        o  [[aimag]]--Imaginary part of complex number

        o  [[aint]]--Truncate to a whole number

        o  [[anint]]--Nearest whole number

    o   [[achar]]--Character in ASCII collating sequence

    o   [[adjustl]]--Left adjust a string

    o   [[adjustr]]--Right adjust a string

    o   [[all]]--All values in MASK along DIM are true

    o   [[any]]--Any value in MASK along DIM is true

    o   [[atomic_add]]--Atomic ADD operation

    o   [[atomic_and]]--Atomic bitwise AND operation

    o   [[atomic_cas]]--Atomic compare and swap

    o   [[atomic_define]]--Setting a variable atomically

    o   [[atomic_fetch_add]]--Atomic ADD operation with prior fetch

    o   [[atomic_fetch_and]]--Atomic bitwise AND operation with prior fetch

    o   [[atomic_fetch_or]]--Atomic bitwise OR operation with prior fetch

    o   [[atomic_fetch_xor]]--Atomic bitwise XOR operation with prior fetch

    o   [[atomic_or]]--Atomic bitwise OR operation

    o   [[atomic_ref]]--Obtaining the value of a variable atomically

    o   [[atomic_xor]]--Atomic bitwise OR operation

    o   [[bge]]--Bitwise greater than or equal to

    o   [[bgt]]--Bitwise greater than

    o   [[ble]]--Bitwise less than or equal to

    o   [[blt]]--Bitwise less than

    o   [[btest]]--Bit test function

    o   [[c_associated]]--Status of a C pointer

    o   [[c_funloc]]--Obtain the C address of a procedure

    o   [[c_f_procpointer]]--Convert C into Fortran procedure pointer

    o   [[c_f_pointer]]--Convert C into Fortran pointer

    o   [[c_loc]]--Obtain the C address of an object

    o   [[c_sizeof]]--Size in bytes of an expression

    o   [[ceiling]]--Integer ceiling function

    o   [[char]]--Character conversion function

    o   [[cmplx]]--Complex conversion function

    o   [[co_broadcast]]--Copy a value to all images the current set of images

    o   [[co_max]]--Maximal value on the current set of images

    o   [[co_min]]--Minimal value on the current set of images

    o   [[co_reduce]]--Reduction of values on the current set of images

    o   [[co_sum]]--Sum of values on the current set of images

    o   [[compiler_options]]--Options passed to the compiler

    o   [[compiler_version]]--Compiler version string

    o   [[conjg]]--Complex conjugate function

    o   [[count]]--Count function

    o   [[cpu_time]]--CPU elapsed time in seconds

    o   [[cshift]]--Circular shift elements of an array

    o   [[date_and_time]]--Date and time subroutine

    o   [[dble]]--Double conversion function

    o   [[dim]]--Positive difference

    o   [[dot_product]]--Dot product function

    o   [[dprod]]--Double product function

    o   [[dshiftl]]--Combined left shift

    o   [[dshiftr]]--Combined right shift

    o   [[eoshift]]--End-off shift elements of an array

    o   [[event_query]]--Query whether a coarray event has occurred

    o   [[execute_command_line]]--Execute a shell command

    o   [[exp]]--Exponential function

    o   [[exponent]]--Exponent function

    o   [[float]]--Convert integer to default real

    o   [[floor]]--Integer floor function

    o   [[fraction]]--Fractional part of the model representation

    o   [[get_command]]--Get the entire command line

    o   [[get_command_argument]]--Get command line arguments

    o   [[get_environment_variable]]--Get an environmental variable

    o   [[iachar]]--Code in ASCII collating sequence

    o   [[iall]]--Bitwise and of array elements

    o   [[iand]]--Bitwise logical and

    o   [[iany]]--Bitwise or of array elements

    o   [[ibclr]]--Clear bit

    o   [[ibits]]--Bit extraction

    o   [[ibset]]--Set bit

    o   [[ichar]]--Character-to-integer conversion function

    o   [[ieor]]--Bitwise logical exclusive or

    o   [[image_index]]--Cosubscript to image index conversion

    o   [[index]]--Position of a substring within a string

    o   [[int]]--Convert to integer type

    o   [[ior]]--Bitwise logical inclusive or

    o   [[iparity]]--Bitwise exclusive or of array elements

    o   [[is_iostat_end]]--Test for end-of-file value

    o   [[is_iostat_eor]]--Test for end-of-record value

    o   [[ishft]]--Shift bits

    o   [[ishftc]]--Shift bits circularly

    o   [[lcobound]]--Lower codimension bounds of an array

    o   [[leadz]]--Number of leading zero bits of an integer

    o   [[len_trim]]--Length of a character entity without trailing blank characters

    o   [[lge]]--Lexical greater than or equal

    o   [[lgt]]--Lexical greater than

    o   [[lle]]--Lexical less than or equal

    o   [[llt]]--Lexical less than

    o   [[logical]]--Convert to logical type

    o   [[maskl]]--Left justified mask

    o   [[maskr]]--Right justified mask

    o   [[matmul]]--matrix multiplication

    o   [[max]]--Maximum value of an argument list

    o   [[maxloc]]--Location of the maximum value within an array

    o   [[maxval]]--Maximum value of an array

    o   [[merge]]--Merge variables

    o   [[merge_bits]]--Merge of bits under mask

    o   [[min]]--Minimum value of an argument list

    o   [[minloc]]--Location of the minimum value within an array

    o   [[minval]]--Minimum value of an array

    o   [[mod]]--Remainder function

    o   [[modulo]]--Modulo function

    o   [[move_alloc]]--Move allocation from one object to another

    o   [[mvbits]]--Move bits from one integer to another

    o   [[nearest]]--Nearest representable number

    o   [[nint]]--Nearest whole number

    o   [[not]]--Logical negation

    o   [[norm2]]--Euclidean vector norm

    o   [[null]]--Function that returns an disassociated pointer

    o   [[num_images]]--Number of images

    o   [[pack]]--Pack an array into an array of rank one

    o   [[parity]]--Reduction with exclusive or

    o   [[popcnt]]--Number of bits set

    o   [[poppar]]--Parity of the number of bits set

    o   [[product]]--Product of array elements

    o   [[random_number]]--Pseudo-random number

    o   [[random_seed]]--Initialize a pseudo-random number sequence

    o   [[rank]]--Rank of a data object

    o   [[real]]--Convert to real type

    o   [[repeat]]--Repeated string concatenation

    o   [[reshape]]--Function to reshape an array

    o   [[rrspacing]]--Reciprocal of the relative spacing

    o   [[scale]]--Scale a real value

    o   [[scan]]--Scan a string for the presence of a set of characters

    o   [[selected_char_kind]]--Choose character kind

    o   [[selected_int_kind]]--Choose integer kind

    o   [[selected_real_kind]]--Choose real kind

    o   [[set_exponent]]--Set the exponent of the model

    o   [[shifta]]--Right shift with fill

    o   [[shiftl]]--Left shift

    o   [[shiftr]]--Right shift

    o   [[sign]]--Sign copying function

    o   [[sngl]]--Convert double precision real to default real

    o   [[spacing]]--Smallest distance between two numbers of a given type

    o   [[spread]]--Add a dimension to an array

    o   [[storage_size]]--Storage size in bits

    o   [[sum]]--Sum of array elements

    o   [[system_clock]]--Time function

    o   [[this_image]]--Cosubscript index of this image

    o   [[trailz]]--Number of trailing zero bits of an integer

    o   [[transfer]]--Transfer bit patterns

    o   [[transpose]]--Transpose an array of rank two

    o   [[trim]]--Remove trailing blank characters of a string

    o   [[ucobound]]--Upper codimension bounds of an array

    o   [[unpack]]--Store the elements of a vector in an array of higher rank

    o   [[verify]]--Scan a string for the absence of a set of characters

-----------------------------------------------------------------------------------------------------------------------------------

                                                          intrinsics (7)                                              July 02, 2017

Generated by manServer 1.08 from 1fc1ffb8-1005-4ad6-bac8-8f3728c7a11c using man macros.
                                                           [intrinsics]
