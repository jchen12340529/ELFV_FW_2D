subroutine update_y
    implicit none
    real, pointer :: pt1(:)
    type(background_cell), pointer :: q
    real :: avg_in(1:ny),avg_out(1:ny)
    integer :: i1,j,k
        
    do i1=1,nx ! for fixed x
    do j = 1, ny
     mg_total = ny
    call bc_x(j)
    
    pt1 => u(i1-2:i1+2,j) ! 5th
    q =>  vertex_y(j) ! Store gspt value
    
    q%gspt_value(1)=0.
    q%gspt_value(2)=0.
    q%gspt_value(3)=0.
 
    call gspt_recon_5th(pt1,q%gspt_value(:))
    
    enddo

    do k=1,3   
    !trig = 0 !!!!!!!!!!!!!!!!!!
    avg_in(:) = 0.
    x_fixed = 0.
    avg_in(1:ny) = vertex_y(1:ny)%gspt_value(k)

      x_fixed = dx/2.*gs(k)+Xmid(i1)
    !    write(*,*) 'update_x',Ymid(j1),y_fixed
   allocate( merged(-nd:ny+nd+1) )
   allocate( elements(-nd:ny+nd+1) )
   allocate( us_n(1:ny) )
   allocate( speed(-nd:ny+nd+1) )
   allocate(F_tail_1d(1:ny+1))
      mg_total = ny
      
    do j = -nd, ny+nd
           merged(j)%coor_left = yleft + (j-1) * dy
           merged(j)%coor_right = yleft + j * dy
    enddo
   
    !if(i1.eq.28 .and. k.eq.3) then !!!!!!!!!!!!!!!!!!!!!!!!!
    !trig =1
    !endif
    
    call evolve(avg_in(:), avg_out(:),ny,dy)
    !call evolve_1(avg_in(:), avg_out(:),ny,dy)
    vertex_y(1:ny)%gspt_value(k) = avg_out(1:ny)  
    
   deallocate( F_tail_1d )
   deallocate( us_n )
   deallocate( elements )
   deallocate( merged )
   deallocate( speed )
    
    enddo
    
    do j=1,ny
    q =>  vertex_y(j)
   
    call  gssum(q%gspt_value(:), u_new(i1,j))  
      
    enddo
 !   write(*,*) 'j1=',j1,'gssum_x done'

    enddo

    
    u(:,:) = u_new(:,:)
    end subroutine update_y