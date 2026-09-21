    subroutine init
    implicit none
    real :: utemp
    integer :: lx,i,j
    i_stage = 0
    mass = 0.
!    merged_cell_total_x = nx
!    merged_cell_total_y = ny
    
    dx = (xright-xleft)/nx
    dy = (yright-yleft)/ny
    
    do i = -nd, nx+nd+1
        X(i) = xleft + (i-1)*dx ! X end pts
    enddo

    do i = -nd, ny+nd+1
        Y(i) = yleft + (i-1)*dy ! Y end pts
    enddo
    
    do i =-nd, nx+nd
! mid pts
		Xmid(i) = xleft + i*dx -dx/2.D0
    enddo
    
    do i =-nd, ny+nd
! mid pts
		Ymid(i) = yleft + i*dy -dy/2.D0
      !  write(200,*)   Ymid(i)
    enddo

! interval pts info(in class)*******************
    do i = 1,nx+1
        vertex_x(i)%coor = xleft + (i-1) * dx
        vertex_x(i)%midpt = xleft + i*dx -dx/2.
    enddo
    
    do i = 1,ny+1
        vertex_y(i)%coor = yleft + (i-1) * dy
        vertex_y(i)%midpt = yleft +i*dy -dy/2.
    enddo
 !**********************************************   
    do i = 1, nx
     do j = 1, ny
          utemp = 0.
        do lx = 1, 9
         utemp = utemp+exact(Xmid(i)+xg(lx)*dx/2.,Ymid(j)+yg(lx)*dy/2.,0.)*wg(lx)
        enddo
        u(i,j) = utemp/4.
 !       mass = mass + u(i,0)  
    enddo 
    enddo
    
    u_new(:,:) = u(:,:)
  
    !bdry conditions
    do i = 1,nx 
    call bc_y(i)
    enddo
    
    do j = 1,ny 
    call bc_x(j)
    enddo
    
 !        do i = -nd, nx+nd
 !          merged_x(i)%coor_left = xleft + (i-1) * dx
 !          merged_x(i)%coor_right = xleft + i * dx
 !          merged_x(i)%cell_itg = u(i,0)*dx     
 !        enddo     
		end subroutine init