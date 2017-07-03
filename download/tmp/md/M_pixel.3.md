[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                               Manual Reference Pages  - M_pixel (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    M_pixel(3f) - [M_pixel] module for drawing into a pixel array with 2D vector operations

CONTENTS

    Synopsis
    Description
    Example

SYNOPSIS

    Module procedures


       use M_writegif, only : writegif


       use :: M_pixel, only : drawchar,    rect,            rdraw2,      strlength
       use :: M_pixel, only : color,       mapcolor,        clear,       draw2
       use :: M_pixel, only : circle,      circleprecision, arc,         getviewport
       use :: M_pixel, only : viewport,    ortho2,          rmove2
       use :: M_pixel, only : line,        linewidth,       polyline,    move2
       use :: M_pixel, only : move2,       draw2,           prefsize,    vinit
       use :: M_pixel, only : textang,     textsize,        drawstr,     getgp2
       use :: M_pixel, only : vflush,      biggest_ortho2,  point2,      getdisplaysize
       use :: M_pixel, only : poly2,       centertext,      xcentertext, ycentertext
       use :: M_pixel, only : makepoly,    closepoly,       font


       use :: M_pixel, only : state,       hershey,         justfy
       use :: M_pixel, only : print_ascii, print_ppm


       ! Differences between M_pixel and M_vogle and VOGLE-related procedures:
       !    hershey(3f) and justfy(3f) do not exist in VOGLE and might be replaced
       !    and the same font names are not available
       !    print_ascii(3f) and print_ppm(3f) do not exist in VOGLE
       !    state(3f) does not exist in VOGLE
       !    biggest_ortho2(3f) is like juaspct(3f)
       !    viewport is in terms of pixels, not range -1.0 to 1.0



    Module variables

       use M_pixel,    only :  P_pixel, P_ColorMap, P_debug



DESCRIPTION

    M_pixel(3fm) is intended to produce simple pixel graphics composed of line drawings and polygon fills in two dimensions. It
    handles circles, curves, arcs, polygons, and software text. It is designed to provide a programming interface very similar to a
    subset of the VOGLE graphics library (M_pixel does not support objects, interactive graphics, or 3D vectors).

    It is primarily intended to provide a simple Fortran-based set of routines that can generate simple graphics that can be
    written to a GIF file using the writegif(3f) routine.

    This is a prototype under construction starting 2017-06, but is already useful. Improvements in line width, dashed lines,
    polygon fill and higher level graphing routines are being worked on. If anyone is interested in collaborating on the module,
    contact the author.

EXAMPLE

    Sample program

       program demo_M_pixel


       use M_pixel
       use M_writegif, only :  writegif
       use M_units,    only : cosd, sind
       implicit none


          integer  :: i
          integer  :: j
          integer  :: icolor


          ! initialize image
          call prefsize(400,400)  ! set size before starting
          call vinit()            ! start graphics
          call clear(0)           ! clear to color 0


          ! put some colored boxes into pixmap by address
          ! so show how the pixel map can be editted easily with
          ! other routines that can manipulate a pixel array.
          ! The P_pixel array was created when vinit(3f) was called
          icolor=1
          do i=1,4
             do j=1,4
                P_pixel((i-1)*100+1+3:i*100-3,(j-1)*100+1+3:j*100-3)=icolor
                icolor=icolor+1
             enddo
          enddo


          ! map area of virtual world to pixel array
          ! notice Y-axis for viewport is zero at TOP
             ! viewport(left, right, bottom, top)
          call viewport(0.0,  400.0,  400.0, 0.0)
          ! define the virtual world area we want to work in
              !ortho2(left, right, bottom,   top)
          call ortho2(0.0,  400.0,    0.0, 400.0)
          ! the drawing routines use these world units


          ! draw polar grids
          call linewidth(100)
          call color(14)
          call target(200.0,200.0,200.0)


          call linewidth(75)
          call color(0)
          call target(100.0,200.0,50.0)


          ! draw some lines
          call color(1)
          call linewidth(200)
          call line(1.0,1.0,400.0,400.0)


          call color(4)
          call line(350.0,200.0,350.0,300.0)


          ! print some text
          call color(1)
          !call hershey(x,y,height,itext,theta,ntext)
          call linewidth(125)
          call hershey(40.0, 40.0,35.0, Hello World ,0.0,11)
          call color(7)
          call linewidth(25)
          call hershey(40.0, 80.0,35.0, Hello World ,0.0,11)
          call linewidth(100)
          call hershey(40.0,120.0,35.0, Hello World ,30.0,11)


          call hershey( 40.0,350.0,35.0, \COMPLEX\Hello World ,0.0,20)
          call hershey( 40.0,310.0,35.0, \DUPLEX\Hello World ,0.0,19)
          call hershey( 350.0,400.0,35.0, \ITALIC\Hello World ,90.0,19)
          call linewidth(50)
          call hershey(200.0,120.0,15.0, \SIMPLEX\Hello World ,20.0,20)


          ! change background color directly
          where (P_pixel.eq.0) P_pixel=9
          ! write standard gif file
          call writegif( M_pixel.3.gif ,P_pixel,P_ColorMap)



        contains

              subroutine target(xc,yc,rc)
              use M_units,    only : cosd, sind
              real     :: xc,yc,rc
              integer  :: i
              real     :: x,y
                 do i=0,360,10
                    x=rc*cosd(i)
                    y=rc*sind(i)
                    call line(xc,yc,xc+x,yc+y)
                 enddo
                 do i=1,int(rc),10
                    call circle(xc,yc,real(i))
                 enddo
              end subroutine target

        end program demo_M_pixel

-----------------------------------------------------------------------------------------------------------------------------------

                                                            M_pixel (3)                                               July 02, 2017

Generated by manServer 1.08 from a249b527-5cbf-4070-9a75-7df8e940b97a using man macros.
                                                             [M_pixel]
