            program demo_joinpath
            use M_io, only : joinpath
            implicit none
               write(*,*)joinpath('/share/user','/man/','man3','joinpath.3m_io'//'.gz')
            end program demo_joinpath
