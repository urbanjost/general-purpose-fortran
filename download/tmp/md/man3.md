[CLI Home Page]

libjust4 man(3) pages

+---------------------------------------------------------------------------------------------------------------------------------+
|       grouping        |            page             |                                description                                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|                       |INTRINSIC                    |intrinsic man(1) pages                                                     |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|                       |getkey                       |read single character from keyboard in hot (raw I/O) mode                  |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|                       |intro_blas1                  |Introduction to vector-vector linear algebra (matrix) subprograms          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|                       |maskit                       |compress array using mask array                                            |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|                       |other2                       |(unknown subject)                                                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|                       |system_getkey                |read single character from keyboard in hot (raw I/O) mode                  |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_getopt     |M_getopt                     |parse command line arguments. similar to those in standard C library.      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_getopt_long|M_getopt_long                |parse command line options like Sun getopt_long, including the Sun CLIP    |
|                       |                             |specifica...                                                               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |M_kracken                    |parse command line options of Fortran programs using Unix-like syntax      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |dget                         |given keyword fetch doubleprecision value from command argument            |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |dgets                        |given keyword fetch doubleprecision array from command arguments           |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |dissect                      |convenient call to parse() - define defaults, then process                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |iget                         |given keyword fetch integer value from command argument                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |igets                        |given keyword fetch integer array from command arguments                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |kracken                      |crack command line options on Fortran programs, using "-KEYWORD VALUE"     |
|                       |                             |syntax                                                                     |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |lget                         |given keyword fetch logical value from command arguments                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |lgets                        |given keyword fetch logical array from command argument                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |parse                        |parse user command and store tokens into Language Dictionary               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |retrev                       |get keyword value as a string from a command's argument list processed by  |
|                       |                             |kracken(3f)                                                                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |rget                         |given keyword fetch real value from command argument                       |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |rgets                        |given keyword fetch real array from command arguments                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |setprompts                   |set explicit prompts for keywords in interactive mode                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |sget                         |given keyword fetch string value and length from command arguments         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |sgets                        |given keyword fetch string value parsed on whitespace into an array        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |show                         |dump dictionary entries                                                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|ARGUMENTS:M_kracken    |store                        |add or replace dict. name's value (if allow='add' add name if necessary)   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_Compare_Float_Numbers|M_Compare_Float_Numbers      |perform relational comparisons on real numbers                             |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_IO                   |readline                     |read a line from specified LUN into allocatable string                     |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_anyscalar            |anyinteger_to_128bit         |convert integer any kind to integer(kind=128)                              |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_anyscalar            |anyscalar_to_real            |convert integer or real parameter of any kind to real                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_calculator           |M_calculator                 |module of routines for parsing expressions and returning values            |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_calculator           |M_calculator_programmer_notes|Programmer notes for module M_calculator                                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_calculator           |getvalue                     |given numeric variable name return doubleprecision value directly from     |
|                       |                             |calculator dictionary...                                                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_calculator           |igetvalue                    |given numeric variable name return integer value directly from calculator  |
|                       |                             |dictionary for eff...                                                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_calculator           |jucalc                       |parse calculator expression and return numeric or string value             |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_calculator           |rgetvalue                    |given numeric variable name return real value directly from calculator     |
|                       |                             |dictionary for effici...                                                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_calculator           |stuff                        |directly store value into calculator directory for efficiency              |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_calculator           |stuffa                       |stuffa(3f): directly store a string into calculator variable name table"   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_calculator_plus      |M_calculator_plus            |convenience routines for calling the M_calculator(3fm) module              |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_calculator_plus      |dnum0                        |return double precision value from string expression using JUCALC          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_calculator_plus      |inum0                        |return integer value from calculator expression                            |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_calculator_plus      |jucalcx                      |return value from a string expression processing messages to simplify call |
|                       |                             |to JUCALC(3f)                                                              |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_calculator_plus      |rnum0                        |returns real number from string expression using JUCALC(3f)                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_calculator_plus      |snum0                        |resolve a calculator expression into a string(return blank on errors)      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_calculator_plus      |strgar2                      |read a string into a real array USING CALCULATOR, allowing quoted strings  |
|                       |                             |in arguments,                                                              |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_calculator_plus      |strgarr                      |read a string into an array using jucalc(3f) calculator                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_color                |M_color                      |a Fortran module that lets you convert between common color models         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_color                |closest_color_name           |returns the closest name for the given RGB values.                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_color                |color_name2rgb               |returns the RGB values in the range 0 to 100 for a given known color name. |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_color                |hue                          |converts a color's components from one color model to another.             |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_color                |rgbmono                      |converts RGB colors to a reasonable grayscale intensity.                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_debug                |M_debug                      |a collection of Fortran routines for supporting the development ofunit     |
|                       |                             |tests, and providing erro...                                               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_debug                |pdec                         |write out string with ASCII decimal equivalent vertically under it         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_debug                |stderr                       |write message to stderr                                                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_debug                |unit_check                   |if logical expression is false, call command "goodbad NAME bad" and stop   |
|                       |                             |program                                                                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_debug                |unit_check_bad               |call command "goodbad NAME bad" and stop program                           |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_debug                |unit_check_good              |call command "goodbad NAME good"                                           |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_debug                |unit_check_start             |call command "goodstart NAME start"                                        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_factor               |M_factor                     |module for least common multiple, greatest common divisor, and prime       |
|                       |                             |factors                                                                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_factor               |greatest_common_divisor      |calculate greatest common divisor oftwo integers or vector m(:), matrix m  |
|                       |                             |(:, :) or cuboid...                                                        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_factor               |i_is_prime                   |Determine if a number is prime using Sieve of Erasthosthenes               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_factor               |least_common_multiple        |Least common multiple of two integersor vector m(:), matrix m(:, :) or     |
|                       |                             |cuboid m(:, :, :)                                                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_factor               |prime_factors                |decompose a number into its prime factors                                  |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_history              |redo                         |Fortran-based Input History Editor                                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_io                   |M_io                         |Fortran I/O module                                                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_io                   |dirname                      |strip last component from filename                                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_io                   |isdir                        |checks if argument is a directory path                                     |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_io                   |notopen                      |Find a FUN/LUN (Fortran-unit-number) that is not in use                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_io                   |print_inquire                |Do INQUIRE on file by name/number and print results                        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_io                   |slurp                        |read a file into a character array                                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_io                   |splitpath                    |split a Unix pathname into components                                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_io                   |uniq                         |append a number to the end of filename to make a unique name if name exists|
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_journal              |M_journal                    |write program messages to stdout and/or a log file                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_journal              |journal                      |provides public message routine, no paging or graphic mode change"         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_logic                |M_logic                      |process input files with embedded if/else/elseif/endif commands            |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_logic                |cond                         |process input files with embedded if/else/elseif/endif commands            |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math:fit             |julfit                       |linear least squares curve fits, destroys input arrays                     |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math:fit             |julfit1                      |internal routine for linear least square fit(y=a*x+b), changes the y array |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math:fit             |linearint                    |interpolates a curve defined by X(i), Y(i) using linear interpolation at   |
|                       |                             |given XI(j) values                                                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math:fit             |lowess                       |procedures for locally weighted regression                                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math:fit             |splift                       |fits a spline to the n data points given in x and yand also returns first  |
|                       |                             |and second derivitives                                                     |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math:fit             |splint                       |interpolates and twice differentiates a cubic spline                       |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math:geometry        |citer                        |determine various geometric properties of circle segmentgiven radius and   |
|                       |                             |area of the segm...                                                        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math:geometry        |envelope                     |Find the vertices (in clockwise order) of a polygon enclosing the points (x|
|                       |                             |(i), y(i), i=1...                                                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math:geometry        |inpolygon                    |determine whether or not an integer point is in an integer polygon         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math:geometry        |locpt                        |find if a point is inside a polygonal path                                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math:geometry        |poly_intercept               |intersection of a straight line and polygonal path                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math:geometry        |polyarea                     |compute the area bounded by a closed polygonal curve                       |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math:statistics      |bds                          |Basic Statistical Measures                                                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math:statistics      |extremum                     |Finds the minimum and maximum value in a REAL array.                       |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math:statistics      |skekur1                      |variant on calculating skewness and kurtosis of an arrray                  |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math:statistics      |skekurx                      |Compute unbiased estimator of the population SKEWNESS and KURTOSIS         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math:statistics      |stddev                       |given a real vector and the vector average calculate the standard deviation|
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math                 |M_math                       |module collecting various general math-related procedures together         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math                 |accdig                       |compare two real numbers only up to a specified number of digits           |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math                 |almost                       |return true or false if two numbers agree up to specified number of digits |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math                 |dp_addig                     |compare two DOUBLEPRECISION numbers only up to a specified number of digits|
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math                 |invert_2x2                   |directly invert 2x2 matrix                                                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math                 |invert_3x3                   |directly invert 3x3 matrix                                                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math                 |invert_4x4                   |directly invert 4x4 matrix                                                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math                 |scale1                       |find new range xminp xmaxp divisible into approximately n linear intervals |
|                       |                             |of size dist                                                               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_math                 |scale3                       |find nice log range                                                        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_messages             |M_messages                   |module collecting various general routines for displaying messages         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_messages             |blocks                       |write out 132-character string in large block letters                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_messages             |junbad                       |print an eye-catching image of a skull.                                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_messages             |junbat                       |print an eye-catching image of a bat.                                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_messages             |junbuster                    |call journal(3f) to print eye-catching ASCII graphic (ghostbuster)         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_messages             |jundragon                    |fill in a character array with a message                                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_messages             |junroach                     |print an eye-catching image of a roach.                                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_messages             |junsun                       |print an eye-catching image of a smiling sun.                              |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_messages             |juntrolls                    |print an eye-catching bulletin                                             |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_messages             |percent_done                 |non-advancing status counter displays percentage doneon terminal displays  |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_messages             |signs                        |write out 132-character string in large block letters                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_messages             |tabgraph                     |write out a row of numbers and a text-based scaled graph                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |M_pixel                      |module for drawing into a pixel array with 2D vector operations            |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |arc                          |draw an arc using current line width and color                             |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |biggest_ortho2               |define the area of the virtual world coordinates to map to the viewport    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |centertext                   |set text centering mode for drawstr(3f) and drawc(3f)                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |circle                       |draw a circle using current line width and color                           |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |circleprecision              |set number of line segments used to approximate a circle                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |clear                        |clear background to current color or specified color index                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |closepoly                    |Terminates a polygon opened by makepoly(3f)                                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |color                        |set current color index                                                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |dl_slices                    |basic 3-d surface plotting routine                                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |draw2                        |draw from current position to given point                                  |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |drawchar                     |Draw a character at the current position                                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |drawstr                      |Draw the text string at the current position                               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |font                         |select font style by name                                                  |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |getdisplaysize               |Returns the width and height of the device in pixels                       |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |getgp2                       |Gets the current graphics position in world coords.                        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |getviewport                  |return viewport in screen pixel coordinates                                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |hershey                      |draw text string as Hershey software vector fonts                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |justfy                       |return lengths used to justify a string when calling hershey               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |line                         |draw line between two points                                               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |linewidth                    |set linewidth                                                              |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |makepoly                     |opens polygon constructed by a series of move-draws and closed by closepoly|
|                       |                             ||                                                                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |mapcolor                     |set a color index using RGB values                                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |move2                        |change current position                                                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |ortho2                       |define the area of the virtual world coordinates to map to the viewport    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |pixel_slice                  |simplified call of DL_SLICES(3f) to make a plot of a 3-d surface           |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |point2                       |Draw a point at x, y                                                       |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |poly2                        |construct a polygon from an array of points                                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |polyline                     |connect points with lines                                                  |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |prefsize                     |specify size of pixel array                                                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |print_ascii                  |print small pixel array as ASCII text                                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |print_ppm                    |print pixel array as a ppm p3 file                                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |rdraw2                       |draw from current position to given point                                  |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |rect                         |draw rectangle given two corners                                           |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |rmove2                       |relative move                                                              |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |state                        |print graphics state of M_pixel graphics module                            |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |strlength                    |return length of string                                                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |textang                      |set text angle                                                             |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |textsize                     |set text size in world units                                               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |vexit                        |exit pixel graphics mode                                                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |viewport                     |Specify which part of the screen to draw in.                               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |vinit                        |initialize pixel graphics module                                           |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |xcentertext                  |set text centering mode on for drawstr(3f) and drawc(3f) in X direction    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_pixel                |ycentertext                  |set text centering mode on for drawstr(3f) and drawc(3f) in Y direction    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_process              |M_process                    |Fortran Module for calling process-related C functions from Fortran        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_process              |process_close                |Fortran Module for calling process-related C functions from Fortran        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_process              |process_open                 |Fortran Module for calling process-related C functions from Fortran        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_process              |process_open_read            |Fortran Module for calling process-related C functions from Fortran        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_process              |process_open_write           |Fortran Module for calling process-related C functions from Fortran        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_process              |process_readall              |read all lines from process into single string                             |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_process              |process_readline             |Fortran Module for calling process-related C functions from Fortran        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_process              |process_writeline            |Fortran Module for calling process-related C functions from Fortran        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_readgif              |readgif                      |read a GIF file                                                            |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_readline             |M_readline                   |Calling readline(3c) from Fortran                                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_readline             |system_readline              |Call readline(3c) from Fortran                                             |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_regex                |regcomp                      |Compile a regex into a regex object                                        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_regex                |regerror                     |Get the string message for a status error value                            |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_regex                |regexec                      |Execute a compiled regex against a string                                  |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_regex                |regfree                      |Release                                                                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_regex                |regmatch                     |                                                                           |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_sort                 |M_sort                       |Fortran module containing sorting algorithms for arrays of standard scalar |
|                       |                             |types                                                                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_sort                 |sort_quick_rx                |indexed hybrid quicksort of a real array                                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_sort                 |sort_shell                   |Generic subroutine sorts the array X using Shell's method                  |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |M_strings                    |Fortran string module                                                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |adjustc                      |center text                                                                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |c2s                          |convert C string pointer to Fortran character string                       |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |change                       |change old string to new string with a directive like a line editor        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |chomp                        |Tokenize a string, consuming it one token per call                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |compact                      |converts contiguous whitespace to a single character (or nothing)          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |crop                         |trim leading blanks and trailing blanks from a string                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |delim                        |parse a string and store tokens into an array                              |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |describe                     |returns a string describing the name of a single character                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |expand                       |expand escape sequences                                                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |indent                       |count number of leading spaces in a string                                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |isalnum                      |test membership in subsets of ASCII character set                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |isalpha                      |test membership in subsets of ASCII character set                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |isascii                      |test membership in subsets of ASCII character set                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |isblank                      |test membership in subsets of ASCII character set                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |iscntrl                      |test membership in subsets of ASCII character set                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |isdigit                      |test membership in subsets of ASCII character set                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |isgraph                      |test membership in subsets of ASCII character set                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |islower                      |test membership in subsets of ASCII character set                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |isprint                      |test membership in subsets of ASCII character set                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |ispunct                      |test membership in subsets of ASCII character set                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |isspace                      |test membership in subsets of ASCII character set                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |isupper                      |test membership in subsets of ASCII character set                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |isxdigit                     |test membership in subsets of ASCII character set                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |len_white                    |get length of string trimmed of whitespace.                                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |lenset                       |return string trimmed or padded to specified length                        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |lower                        |changes a string to lowercase over specified range                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |matchw                       |compare given string for match to pattern which may contain wildcard       |
|                       |                             |characters                                                                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |merge_str                    |pads strings to same length and then calls MERGE(3f)                       |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |modif                        |emulate the MODIFY command from the line editor XEDIT                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |noesc                        |convert non-printable characters to a space.                               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |nospace                      |remove all whitespace from input string                                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |notabs                       |expand tab characters                                                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |reverse                      |Return a string reversed                                                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |s2c                          |convert character variable to array of characters with last element set to |
|                       |                             |null                                                                       |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |s2v                          |function returns doubleprecision numeric value from a string               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |s2vs                         |given a string representing numbers return a numeric array                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |split                        |parse string into an array using specified delimiters                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |string_to_value              |subroutine returns real value from string                                  |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |string_to_values             |read a string representing numbers into a numeric array                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |substitute                   |Globally substitute one substring for another in string                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |switch                       |generic composition of a2s() and s2a() converts between CHARACTER scalar   |
|                       |                             |and array of single ch...                                                  |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |transliterate                |replace characters from old set with new set                               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |upper                        |changes a string to uppercase                                              |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |v2s                          |return numeric string from a numeric value                                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_strings              |value_to_string              |return numeric string from a numeric value                                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_swap                 |swap                         |elemental subroutine swaps two standard type variables of like type        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |M_system                     |Fortran interface to C system interface                                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |fileglob                     |Read output of an ls(1) command from Fortran                               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |set_environment_variable     |call setenv(3c) to set environment variable                                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_chdir                 |call chdir(3c) from Fortran to change working directory                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_chmod                 |call chmod(3c) to change permission mode of a file relative to directory   |
|                       |                             |file descriptor                                                            |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_clearenv              |clear environment by calling clearenv(3c)                                  |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_closedir              |close a directory stream by calling closedir(3c)                           |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_errno                 |XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_getcwd                |call getcwd(3c) to get the pathname of the current working directory       |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_getegid               |get the effective group ID (GID) of current process from Fortran by calling|
|                       |                             |getegid(3c)                                                                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_geteuid               |get effective UID of current process from Fortran by calling geteuid(3c)   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_getgid                |get the real group ID (GID) of current process from Fortran by calling     |
|                       |                             |getgid(3c)                                                                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_gethostname           |get name of current host                                                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_getlogin              |get login name                                                             |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_getpid                |get PID (process ID) of current process from Fortran by calling getpid(3c) |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_getppid               |get parent process ID (PPID) of current process from Fortran by calling    |
|                       |                             |getppid(3c)                                                                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_getsid                |get the process group ID of a session leader                               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_getuid                |get real UID of current process from Fortran by calling getuid(3c)         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_getumask              |get current umask                                                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_initenv               |initialize environment table pointer and size so table can be read by      |
|                       |                             |readenv(3f)                                                                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_kill                  |send a signal to a process or a groupof processes                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_link                  |link one file to another file relative to two directory file descriptors   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_mkdir                 |call mkdir(3c) to create a new directory                                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_mkfifo                |make a FIFO special file relative to directory file descriptor             |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_opendir               |open directory stream by calling opendir(3c)                               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_perror                |print error message for last C error on stderr                             |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_putenv                |set environment variable from Fortran by calling putenv(3c)                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_rand                  |call pseudo-random number generator rand(3c)                               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_readdir               |read a directory using readdir(3c)                                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_readenv               |step thru and read environment table                                       |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_remove                |call remove(3c) to remove file                                             |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_rename                |call rename(3c) to rename a system file                                    |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_rewinddir             |call rewinddir(3c) to rewind directory stream                              |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_rmdir                 |call rmdir(3c) to remove empty directories                                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_setumask              |set the file mode creation umask                                           |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_srand                 |set seed for pseudo-random number generator system_rand(3f)                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_umask                 |XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX                                        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_uname                 |call a C wrapper that calls uname(3c) to get current system information    |
|                       |                             |from Fortran                                                               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_unlink                |remove a directory entry relative to directory file descriptor             |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_system               |system_unsetenv              |change or add an environment variable by calling unsetenv(3c)              |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |M_time                       |Fortran module for manipulating and presenting time and date values        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |box_month                    |create specified month in a character array                                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |d2j                          |given DAT date-time array returns Julian Date                              |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |d2o                          |converts DAT date-time array to Ordinal day                                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |d2u                          |given DAT date-time array returns Unix Epoch Time (UET starts at 0000 on 1 |
|                       |                             |Jan. 1970, UTC)                                                            |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |d2w                          |calculate iso-8601 Week-numbering year date yyyy-Www-d given DAT date-time |
|                       |                             |array                                                                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |date_to_julian               |converts DAT date-time array to Julian Date                                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |date_to_unix                 |converts DAT date-time array to Unix Epoch Time                            |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |days2sec                     |hh:mm:ss.nn to seconds                                                     |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |dow                          |given a date-time array DAT return the day of the week                     |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |fmtdate                      |given DAT date-time array return date as string using specified format     |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |fmtdate_usage                |display macros recognized by fmtdate(3f) and now(3f)                       |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |guessdate                    |reads in a date, in various formats                                        |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |j2d                          |given a JED (Julian Ephemeris Date) returns a date-time array DAT.         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |julian_to_date               |converts a JED(Julian Ephemeris Date) to a DAT date-time array.            |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |mo2d                         |given month name return DAT date-time array for beginning of that month in |
|                       |                             |current year                                                               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |mo2v                         |given month name return month number (1-12) of that month                  |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |moon_fullness                |return percentage of moon phase from new to full                           |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |now                          |return string representing current time given format                       |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |o2d                          |converts Ordinal day to DAT date-time array                                |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |ordinal_to_date              |when given a valid year and day of the year returns the DAT array for the  |
|                       |                             |date                                                                       |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |phase_of_moon                |return name for phase of moon for given date                               |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |sec2days                     |convert seconds to string of form dd-hh:mm:ss                              |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |system_sleep                 |call C sleep(3c) or usleep(3c) procedure                                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |u2d                          |given Unix Epoch Time returns DAT date-time array                          |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |unix_to_date                 |converts Unix Epoch Time to DAT date-time array                            |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |v2mo                         |returns the month name of a Common month number                            |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_time                 |w2d                          |calculate DAT date-time array from iso-8601 Week-numbering year date       |
|                       |                             |yyyy-Www-d                                                                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_units                |M_units                      |convert between various physical units                                     |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_units                |c2f                          |convert Celsius to Fahrenheit                                              |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_units                |cartesian_to_polar           |convert Cartesian coordinates to polar coordinates                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_units                |cartesian_to_spherical       |convert Cartesian coordinates to ISO polar coordinates                     |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_units                |cosd                         |calculate sine of value in degrees                                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_units                |d2r                          |convert degrees to radians                                                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_units                |f2c                          |convert Fahrenheit to Celsius                                              |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_units                |feet_to_meters               |converts a measurement in feet to meters                                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_units                |meters_to_feet               |converts a measurement in meters to feet                                   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_units                |polar_to_cartesian           |convert polar coordinates to Cartesian coordinates                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_units                |r2d                          |convert radians to degrees                                                 |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_units                |sind                         |calculate sine of value in degrees                                         |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_units                |spherical_to_cartesian       |convert ISO polar coordinates to Cartesian coordinates                     |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_units                |tand                         |calculate tangent of value in degrees                                      |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_writegif             |writegif                     |Codes pixel-map with palette into GIF format. Optional transparent color   |
|-----------------------+-----------------------------+---------------------------------------------------------------------------|
|M_writegif_animated    |write_animated_gif           |Codes pixel-maps with palette into animated GIF format. Optional           |
|                       |                             |transparent color                                                          |
+---------------------------------------------------------------------------------------------------------------------------------+
