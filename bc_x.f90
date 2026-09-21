subroutine bc_x(j1)
    implicit none
    integer,intent(in) :: j1
    integer :: i
    
    if(ibc .eq. 2)then 
    do i=0, nd
 !   write(*,*) 'i=',i,'j1=',j1
        u(-i,j1) = u(1,j1)
        u(nx+1+i,j1) = u(nx,j1)
    enddo
    
    else
    
    do i=0, nd
        u(-i,j1) = u(nx-i,j1)
        u(nx+1+i,j1) = u(i+1,j1)
    enddo
    
    endif
    end subroutine bc_x