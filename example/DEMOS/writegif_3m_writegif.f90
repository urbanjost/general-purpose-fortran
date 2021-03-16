          program demo_writegif
          use M_writegif, only : writegif
          integer  :: Pixel(100,100)
          integer  :: Transparent = 0
          integer  :: ColorMap (3,0:7)
          colormap(:,0)=[255,255,255]
          colormap(:,1)=[255,  0,  0]
          colormap(:,2)=[  0,255,  0]
          colormap(:,3)=[  0,  0,255]
          colormap(:,4)=[255,255,  0]
          colormap(:,5)=[255,  0,255]
          colormap(:,6)=[  0,255,255]
          colormap(:,7)=[  0,  0,  0]

          ! put some colored boxes into pixmap
          pixel(:,:)=0
          pixel(1:80,1:80)=1
          pixel(11:20,11:20)=2
          pixel(21:40,21:40)=3

          ! write gif with a transparent background
          call writegif('boxes_t.gif',pixel,ColorMap,Transparent)

          ! change background color and write standard gif file
          where (pixel.eq.0) pixel=4
          call writegif('boxes.gif',pixel,ColorMap)

          end program demo_writegif
