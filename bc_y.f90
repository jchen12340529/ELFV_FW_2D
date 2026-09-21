subroutine bc_y(i1)
    implicit none
    integer,intent(in) :: i1
    integer :: j
   
    if(ibc .eq. 2)then 
    do j=0, nd
        u(i1,-j) = u(i1,1)
        u(i1,ny+1+j) = u(i1,ny)
    enddo
     
    else
        
     do j=0, nd
        u(i1,-j) = u(i1,ny-j)
        u(i1,ny+1+j) = u(i1,j+1)
     enddo
     
    endif
    end subroutine bc_y