subroutine update_x
    implicit none
    real, pointer :: pt1(:)
    type(background_cell), pointer :: q
    real :: avg_in(1:nx),avg_out(1:nx)
    integer :: j1,i,k
    
  
    
    do j1=1,ny ! for fixed y
    do i = 1, nx
     mg_total = nx
    call bc_y(i)
    
      pt1 => u(i,j1-2:j1+2) !5th
    q =>  vertex_x(i) ! Store gspt value
    
    q%gspt_value(1)=0.
    q%gspt_value(2)=0.
    q%gspt_value(3)=0.
 
    call gspt_recon_5th(pt1,q%gspt_value(:))
 
    enddo
 !   write(*,*) 'j1=',j1,'gas_recon_x done'
   
    do k=1,3   
    avg_in(1:nx) = 0.
    y_fixed = 0.
    avg_in(1:nx) =  vertex_x(1:nx)%gspt_value(k)

        y_fixed = dy/2.*gs(k)+Ymid(j1)
    !    write(*,*) 'update_x',Ymid(j1),y_fixed
   allocate( merged(-nd:nx+nd+1) )
   allocate( elements(-nd:nx+nd+1) )
   allocate( us_n(1:nx) )
   allocate( speed(-nd:nx+nd+1) )
   allocate(F_tail_1d(1:nx+1))
      mg_total = nx
      
    do i = -nd, nx+nd
           merged(i)%coor_left = xleft + (i-1) * dx
           merged(i)%coor_right = xleft + i * dx
    enddo
    
   call evolve(avg_in(:), avg_out(:),nx,dx)
    !call evolve_1(avg_in(:), avg_out(:),nx,dx)
   vertex_x(1:nx)%gspt_value(k) = avg_out(1:nx)  
    
   deallocate( F_tail_1d )
   deallocate( us_n )
   deallocate( elements )
   deallocate( merged )
   deallocate( speed )
      
 !  if(mg_total .lt. nx)then
 !  write(*,*) 'i_y=',j1,'k=',k
 !  endif
    
   enddo
!   write(*,*) 'j1=',j1,'evolve_x done'
   
    do i=1,nx
    q =>  vertex_x(i)
    call  gssum(q%gspt_value(:), u_new(i,j1))

    enddo
 !   write(*,*) 'j1=',j1,'gssum_x done'
    
    enddo
    
    u(:,:) = u_new(:,:)
    end subroutine update_x