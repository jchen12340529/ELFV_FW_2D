 program ELFV_FW_2D
    use mod_2d
    
    implicit none
   integer, parameter :: nl =1, N_x = 100, N_y=N_x, nd =20, nxyd = nl * max(N_x,N_y)+nd+1
    integer :: i,j,k,l
    integer :: nx,ny,nt
    real :: dx,dy
    real :: pi,eps
    real :: xg(9),yg(9),wg(9), gs(3),wgs(3),gpt
    real :: xleft,xright,yleft,yright
    real :: dt,tt,T,cfl
    real,allocatable :: X(:),Y(:)
    real,allocatable :: Xmid(:),Ymid(:)

    real,allocatable,target :: u(:,:),u_new(:,:)
    real :: exact_ave(1-nd:nxyd-1,1-nd:nxyd-1)
    real :: error1(1:nl),error2(1:nl),error3(1:nl)
    real :: ord(1:nl)

    real :: x_fixed,y_fixed,TV
    real,allocatable :: us_n(:)
    integer :: i_stage,i_init,isp,iexample ,ibc
!    real :: er1,er2,er3
    real :: sum_out, mass,mass_temp,mass_max, L1_norm
    integer :: mg_total
    real,allocatable :: speed(:)
    real :: umax, umin
    !integer :: trig

 !   integer :: iexample
    
    pi = 4.D0 * datan(1.D0)
    eps = 1.e-8
    gpt = sqrt(3./5.)
    tt = 0.
    
    xg(1)=-gpt
    yg(1)=-gpt
    xg(2)=gpt
    yg(2)=-gpt
    xg(3)=gpt
    yg(3)=gpt
    xg(4)=-gpt
    yg(4)=gpt
    xg(5)=0.
    yg(5)=-gpt
    xg(6)=gpt
    yg(6)=0.
    xg(7)=0.
    yg(7)=gpt
    xg(8)=-gpt
    yg(8)=0.
    xg(9)=0.
    yg(9)=0.
    
    wg(1)=25./81.
    wg(2)=wg(1)
    wg(3)=wg(1)
    wg(4)=wg(1)
    
    wg(5)=40./81.
    wg(6)=wg(5)
    wg(7)=wg(5)
    wg(8)=wg(5)
    
    wg(9)=64./81.

    gs(1) = -gpt
    gs(2) = 0.
    gs(3) = gpt
    wgs(1) = 5./9.
    wgs(2) = 8./9.
    wgs(3) = 5./9.
    
     call setup
     do l=1,nl
        nx = l * N_x
        ny = l * N_y
    
!*******************************
!allocate_variables
    allocate( vertex_x(1:nx+1) )
    allocate( vertex_y(1:ny+1) )
    allocate( X(-nd:nx+nd+1) )
    allocate( Y(-nd:ny+nd+1) )
    allocate( Xmid(-nd:nx+nd) )
    allocate( Ymid(-nd:ny+nd) )
    allocate( u(-nd:nx+nd+1,-nd:ny+nd+1) )
    allocate( u_new(-nd:nx+nd+1,-nd:ny+nd+1))
   
!*******************************
    call init
    call splitting
    call errcal(l)
    enddo
    pause
    
!    call err_ord
    !************************************
        !deallocate_variables
        deallocate( vertex_x )
        deallocate( vertex_y )
        deallocate( X )
        deallocate( Y )
        deallocate( Xmid )
        deallocate( Ymid )
        deallocate( u )
        deallocate( u_new )
      
        !end deallocate_variables
!*******************************
 !-----------------------------------------------------------   
    contains
    
    include "init.f90"
    include "setup.f90"
    include "splitting.f90"
    include "errcal.f90"
    include "exact.f90"
    include "gspt_recon_5th.f90"  
    include "merging.f90"
    include "mgnum_criteria.f90"
    include "cross_time.f90"
    include "bc_x.f90"
    include "bc_y.f90"
    include "bc_1d.f90"
    include "downstream_locate.f90"
    include "char_comput.f90"
    !include "evolve_1.f90"
    include "evolve.f90"
    include "flux_comput.f90"
    include "eno3.f90"
    include "eno3_reproject.f90"
    include "reproject.f90"
    include "f.f90"
    include "fp.f90"
    include "gssum.f90"
    include "lambda_local.f90"
    include "roe_speed.f90"
    include "update_x.f90"
    include "update_y.f90"
    
    end program ELFV_FW_2D