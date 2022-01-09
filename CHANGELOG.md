## GPF Changelog

The intent of this changelog is to keep everyone in the loop about
what's new in the GPF project. It is a curated, chronologically ordered
list of notable changes including records of change such as bug fixes,
new features, changes, and relevant notifications.

---
**2022-01-09**  John S. Urban<https://github.com/urbanjost>
   - removed creation of temporary arrays from CALL statements to 
     assignment to a TEMP variable to work around ifort 11 segfault

   - production version of prep(1) with $PARCEL/$POST working with
     string substitution; marked with "kludge for bug ...".

   - Mersenne twister routine in M_random.f90 was undersubscripting the
     init_key array creating a working (it was passing tests) but potentially non-repeatable
     initialization

### :green_circle: ADD:
Added the new routine `system_signal` to module `M_system`.

     system_signal(3f) - [M_system:SIGNALS] install a signal handler (LICENSE:PD)

   - [x] manpage
   - [x] demo program
   - [ ] unit test
---
---
**2021-03-01**  Somajit Dey  <https://github.com/SomajitDey>

### :green_circle: ADD:
Added the new routine `system_signal` to module `M_system`.

     system_signal(3f) - [M_system:SIGNALS] install a signal handler (LICENSE:PD)

   - [x] manpage
   - [x] demo program
   - [ ] unit test
---

<!--
**2020-04-01**  John S. Urban  <https://github.com/urbanjost>

### :orange_circle: DIFF:
### :green_circle: ADD:
   + QA: test/test_suite_M_strings.f90
   + MANPAGE: [X]
   + DEMO PROGRAM: [X]
   + COMMIT: [abcdefghij]
### :red_circle: FIX:
---
Geometric
| ico                       | shortcode                   | ico                         | shortcode |
| ----                      | ------                      | -----                       | -------   |
| red_circle                | :red_circle:                |  orange_circle              | :orange_circle:              |
| yellow_circle             | :yellow_circle:             |  green_circle               | :green_circle:               |
| large_blue_circle         | :large_blue_circle:         |  purple_circle              | :purple_circle:              |
| brown_circle              | :brown_circle:              |  black_circle               | :black_circle:               |
| white_circle              | :white_circle:              |  red_square                 | :red_square:                 |
| orange_square             | :orange_square:             |  yellow_square              | :yellow_square:              |
| green_square              | :green_square:              |  blue_square                | :blue_square:                |
| purple_square             | :purple_square:             |  brown_square               | :brown_square:               |
| black_large_square        | :black_large_square:        |  white_large_square         | :white_large_square:         |
| black_medium_square       | :black_medium_square:       |  white_medium_square        | :white_medium_square:        |
| black_medium_small_square | :black_medium_small_square: |  white_medium_small_square  | :white_medium_small_square:  |
| black_small_square        | :black_small_square:        |  white_small_square         | :white_small_square:         |
| large_orange_diamond      | :large_orange_diamond:      |  large_blue_diamond         | :large_blue_diamond:         |
| small_orange_diamond      | :small_orange_diamond:      |  small_blue_diamond         | :small_blue_diamond:         |
| small_red_triangle        | :small_red_triangle:        |  small_red_triangle_down    | :small_red_triangle_down:    |
| diamond_shape_with_a_dot_inside  | :diamond_shape_with_a_dot_inside:  | radio_button               |  :radio_button:        |
| white_square_button              | :white_square_button:              | black_square_button        |  :black_square_button: |

-->
