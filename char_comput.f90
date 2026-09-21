subroutine char_comput(dcell,n_cell)
    implicit none
    real,intent(in) :: dcell
    integer,intent(in) :: n_cell
    ! see if characteristics has intersections
    do i = 1-nd,n_cell+nd+1
    call roe_speed( merged(i-1)%cell_itg/dcell,merged(i)%cell_itg/dcell,merged(i)%coor_left,speed(i) )
    enddo
          
    do i = 1-nd,n_cell+nd
        merged(i)%left_char = speed(i)
        merged(i)%right_char = speed(i+1)
        
        elements(i)%point_left%char = speed(i)
        elements(i)%point_right%char = speed(i+1)
    enddo

		 end subroutine char_comput    