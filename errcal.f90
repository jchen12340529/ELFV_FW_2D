 subroutine errcal(l)
    implicit none

    integer :: kkx,ig,kky
    integer,intent(in) :: l
    integer :: i,j

    error1(l)=0.
    error2(l)=0.
    error3(l)=0.
      
! do kkx = 1, nx
!    do kky = 1, ny
!        exact_ave(kkx,kky) = 0.
!        do ig = 1,9
!        if(iexample.eq.3)then
!         exact_ave(kkx,kky) = exact_ave(kkx,kky) &
!         & + exact(Xmid(kkx)+xg(ig)*dx/2.,Ymid(kky)+yg(ig)*dy/2.,T)*wg(ig)
!         endif
!        enddo
        
!        exact_ave(kkx,kky) = exact_ave(kkx,kky)/4.
         
!        error1(l)=error1(l)+abs(exact_ave(kkx,kky)-u(kkx,kky))
!        error3(l)=max(error3(l),abs(exact_ave(kkx,kky)-u(kkx,kky)  ))
!        error2(l)=error2(l)+( exact_ave(kkx,kky)-u(kkx,kky))**2.  
!    enddo
!    enddo
!    error1(l)=error1(l)/nx/ny

    !    error2(l)=sqrt(error2(l)/(nx*ny))
!    L1_norm = 0.
!    do i=1,nx
!    do j =1,ny
!      L1_norm = L1_norm + u(i,j)
!    enddo
!    enddo
    
    if(l.eq.nl)then
    do i=1,nx
    do j =1,ny
!        write(2000, *) Xmid(i), Ymid(j), exact_ave(i,j)
        write(1003,*) Xmid(i), Ymid(j), u(i,j)
 !       write(100,*)  u(i,j) 
!    if(i_init.eq.2 .and. iexample.eq.2)then
!     if(i.eq.nx/2)then
!		 write(1001,*) Ymid(j), u(i,j)
!		 endif
!         if(j.eq.ny*5/8)then
!		 write(1002,*) Xmid(i), u(i,j)
!		 endif
 !        if(j.eq.ny*1/4)then
!		 write(1004,*) Xmid(i), u(i,j)
!		 endif
!    endif
    enddo    
    enddo
    endif

 !   do i=1,nx
 !   write(600,*) X(i) 
 !   enddo
    
 !   do i=1,ny
 !   write(601,*) Y(i) 
  !  enddo
    
        write(*,*) 'NX*NY=',nx,ny, 'error=', error1(l)
!        write(*,*) 'mass_error=',abs(mass-L1_norm)*dx*dy 
    end subroutine errcal