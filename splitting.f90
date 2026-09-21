 subroutine splitting
    implicit none
    integer :: i1, j1,knt, i,j
    real :: temp_u_x(1:nx),temp_u,min_x(1:nx),max_x(1:nx)
    real :: sp1,sp2,dt1
      tt = 0.
      dt=0.
    sp1 = 1./(2.-2.**(1./3.))
    sp2 = -(2.**(1./3.))/(2.-2.**(1./3.))
    do i = 1,nx
        temp_u_x(i) = maxval(abs(u(i,:))) 
        max_x(i) = maxval(u(i,:))
        min_x(i) = minval(u(i,:))
    enddo
    
        temp_u = maxval(abs(temp_u_x(:)))
        umax = maxval(max_x(:))
        umin = minval(min_x(:))

    dt = cfl/(temp_u/dx+temp_u/dy)
!     dt = cfl/(1./dx + 1./dy)
!     dt = dx**2.
    nt = ceiling(T/dt)
    
    write(*,*) 'nt=', nt, 'dt/dx=', dt/dx
    pause
 !**********************************************************      1st
   if(isp.eq.1)then
        dt1 = dt 
     do knt=1,nt
      if(tt+dt.gt.T)then
            dt = T - tt
            dt1 = dt 
      endif

       call update_x
       
      write(*,*) 'x-sweep done'
 !      TV = 0.
 !    do j = 1,ny-1
 !       do i =1, nx-1
 !       TV = max(TV, abs(u(i+1,j+1)-u(i,j)))
 !   enddo
 !   enddo
 !    write(600,*) 'TV_u*=', TV  
       
        call update_y
    write(*,*) 'y-sweep done'
   
!    do i =1, nx-1
!    do j = 1,ny-1
!        TV = max(TV, abs(u(i+1,j+1)-u(i,j)))
!    enddo
!    enddo
  !   write(600,*) 'TV_u_n+1=', TV
        
         tt = tt + dt
      write(*,*) 'nt=', nt, knt
      enddo
 !***************************************************************  2nd   
     elseif(isp.eq.2)then
           dt1 = dt 
     do knt=1,nt
        if(tt+dt.gt.T)then
            dt = T - tt
            dt1 = dt 
         endif   
      dt=dt/2. 

        call update_x
      
    write(*,*) 'x-sweep done'
    !   TV = 0.
    ! do j = 1,ny-1
    !    do i =1, nx-1
    !    TV = max(TV, abs(u(i+1,j+1)-u(i,j)))
    !enddo
    !enddo
    ! write(600,*) 'TV_u1=', TV  
     
    dt=dt*2.      

          call update_y

    write(*,*) 'y-sweep done'
   !!  TV = 0.
   ! do i =1, nx-1
   ! do j = 1,ny-1
   !     TV = max(TV, abs(u(i+1,j+1)-u(i,j)))
   ! enddo
   ! enddo
   !  write(600,*) 'TV_u2=', TV
     
     tt=tt+dt/2.
     dt=dt/2. 

        call update_x
    write(*,*) 'x-sweep done'
    !do j = 1,ny-1
    !do i =1, nx-1
    !    TV = max(TV, abs(u(i+1,j+1)-u(i,j)))
    !enddo
    !enddo
    ! write(600,*) 'TV_u*=', TV  
     
     dt=dt*2. 
     tt=tt+dt/2.
       write(*,*) 'nt=', knt
       enddo
!**********************************************************
        elseif(isp.eq.3)then
               dt1 = dt 
     do knt=1,nt
        if(tt+dt.gt.T)then
            dt = T - tt
            dt1 = dt 
         endif
         
     dt  = dt1*sp1/2.
    
        call update_x
      
      
      dt = dt1*sp1 
    
         call update_y
      
      
    tt=tt+dt1*sp1/2.D0
    
    dt  = (sp1+sp2)*dt1/2.
     
        call update_x
      
     tt=tt+dt1*sp1/2.D0  
      
    dt = dt1*sp2
 
         call update_y 
      
    tt = tt+sp2*dt1/2.
    
    dt  = (sp1+sp2)*dt1/2.

        call update_x
      
    tt = tt+sp2*dt1/2.
    dt = dt1*sp1

         call update_y
      
    tt=tt+dt1*sp1/2.
     dt  = dt1*sp1/2.
     
        call update_x   

    dt = dt1 
    tt = tt+dt1*sp1/2.
    write(*,*) 'nt=', knt
    enddo
    
    endif
      end subroutine splitting 
    
    
    
    