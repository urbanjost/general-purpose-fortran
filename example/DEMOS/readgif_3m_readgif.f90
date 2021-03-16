             program demo_readgif
             use M_readgif, only : readgif
             use M_writegif, only : writegif
             implicit none
             character(len=*),parameter :: filename='boxes.gif'
             integer                    :: num_image=1
             integer,allocatable        :: image(:,:)
             integer                    :: iostat=0
             real,allocatable           :: color_map(:,:)
             integer,allocatable        :: color_map2(:,:)
             logical                    :: verbose=.true.
             integer                    :: i,ii,jj
             call readgif(filename,num_image,image,iostat,color_map,verbose)
             if(iostat.ne.0)then
                write(*,*)'*demo_readgif* could not read GIF file ',trim(filename)
                stop
             endif

             write(*,*)'SIZE OF IMAGE =',size(image)
             do i=1,rank(image)
                write(*,*)'RANK OF IMAGE=',i,size(image,dim=i)
             enddo

             write(*,*)'SIZE OF COLORMAP=',size(color_map)
             do i=1,rank(color_map)
                write(*,*)'RANK OF COLORMAP=',i,size(color_map,dim=i)
             enddo

             ! convert between colormap types
             ! writegif uses an integer colormap, values 0 to 255
             ! readgif  uses real values 0.0 to 1.0
             ii=size(color_map,dim=1)
             jj=size(color_map,dim=2)
             allocate(color_map2(ii,0:jj-1))
             color_map2=255*color_map

             ! change color and write standard gif file
             where (image.eq.1) image=4
             call writegif('boxes_new.gif',image,color_map2)

             end program demo_readgif
