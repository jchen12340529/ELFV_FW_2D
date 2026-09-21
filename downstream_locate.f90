subroutine downstream_locate(dt_local, mtotal, dcell)    
      implicit none
      real,intent(in) :: dt_local, dcell
      integer, intent(in) :: mtotal
      integer :: i
    do i = -nd,mtotal+nd
           
    elements(i)%point_left%coor = merged(i)%coor_left+elements(i)%point_left%char*dt_local
    elements(i)%point_right%coor = merged(i)%coor_right+elements(i)%point_right%char*dt_local
           
    elements(i)%int_len = elements(i)%point_right%coor-elements(i)%point_left%coor
            
    elements(i)%point_left%id = ceiling( (elements(i)%point_left%coor-merged(1)%coor_left)/dcell )
    elements(i)%point_right%id = ceiling( (elements(i)%point_right%coor-merged(1)%coor_left)/dcell )
        
  !      write(200,*) i,element_x_star(i)%point_left%coor, element_x_star(i)%point_right%coor,element_x_star(i)%point_left%id,element_x_star(i)%point_right%id
    enddo
     
    end subroutine downstream_locate