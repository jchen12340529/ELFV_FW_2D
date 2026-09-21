subroutine reproject(result_out,n_cell,dcell)
implicit none
    integer,intent(in) :: n_cell
    real,intent(in)  ::  dcell
    real,intent(out) :: result_out(1:n_cell)
    type(element1d_downstream), pointer :: p1,p2
    type(element1d_downstream), pointer :: p(:)
    integer :: i,j,j_note
    real :: sum, int_left, int_right,int_cell
    integer :: i0
 
     result_out(:) = 0.
    do i =1,n_cell
!    result_out(i) = 0.
    sum = 0.
    j_note = 0
    int_left = 0.
    int_right = 0.
    int_cell = 0.
    
    do j = 3-nd, mg_total+nd
       p1 => elements(j)
       p2 => elements(j-1)
       if (elements(j)%point_left%id .eq. i) then
       
       if (j_note .eq. 0) then
       j_note = j_note + 1 
! First order
!    sum = sum + (p1%point_left%coor-X(p1%point_left%id))/p2%int_len * p2%up_itg 
       
!-----------------------------------------       
       p => elements(j-3:j+1)
       i0 = 3
       call eno3_reproject(p,i0,X(p1%point_left%id),p1%point_left%coor,int_left)

       sum = sum + int_left*(p2%int_len)
        
        
       endif
       
       if (elements(j)%point_right%id .eq. i) then
       
       sum =  sum + p1%up_itg ! * p1%int_len
        
        
       elseif( elements(j)%point_right%id .gt. i ) then
 
!    sum = sum + (X(p1%point_left%id+1)-p1%point_left%coor)/p1%int_len * p1%up_itg

      p => elements(j-2:j+2) 
       i0 = 3
      call eno3_reproject(p,i0,p1%point_left%coor,X(p1%point_left%id+1),int_right)     

        sum = sum + int_right* p1%int_len
        
       endif
       elseif (elements(j)%point_left%id .lt. i .and. elements(j)%point_right%id .gt. i) then
      
!       p1 => elements(j)
!       sum = sum + dx/p1%int_len * p1%up_itg

       p => elements(j-2:j+2)
       i0 = 3
       call eno3_reproject(p,i0,X(i),X(i+1),int_cell)
      
       sum = sum + int_cell*p1%int_len
        
        
    endif
    enddo
 
    result_out(i) = sum/dcell
    
    
    enddo
    
    end subroutine reproject 