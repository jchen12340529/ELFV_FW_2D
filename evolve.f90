subroutine evolve(avg_input,avg_output, n_cell, dcell)
    implicit none

    integer,intent(in) :: n_cell
    real,intent(in) :: avg_input(1:n_cell),dcell
    real,intent(out) :: avg_output(1:n_cell)
    real :: avg_new(1:n_cell)
    type(element1d_downstream), pointer :: p(:)
    integer :: i 
    !real :: dt_global, lend
    
    mg_total = n_cell

    !umax = maxval( avg_input(:) )
    !umin = minval( avg_input(:) )
    
    merged(1:n_cell)%cell_itg = avg_input(1:n_cell)* dcell
    elements(1:n_cell)%up_itg = merged(1:n_cell)%cell_itg 
    
!bdry condition*******************************************    
    call bc_1d
    
    !dt_global = dt
    call char_comput(dcell, n_cell)
    call downstream_locate(0., mg_total, dcell)   
 ! Step 1    
    do i = 1, n_cell
    p => elements(i-3:i+3)
         
    call flux_comput(p, F_tail_1d(i)%flux_left,F_tail_1d(i)%flux_right)  !reconstruction of F_tail at each approx chara line
 !   call flux_comput(p, elements(i)%point_left%flux0,elements(i)%point_right%flux0 )
    enddo     
    
    call downstream_locate(dt,mg_total, dcell) 
    call merging(dcell)
!bdry condition    
    call bc_1d
    do i = 1, mg_total  
    us_n(i) = merged(i)%cell_itg

    elements(i)%up_itg = us_n(i) - dt*(elements(i)%point_right%flux0 - elements(i)%point_left%flux0)
    enddo     
    
 ! Step 2 ---------------------------------------------------------
     call downstream_locate(dt,mg_total,dcell ) 
     call bc_1d
        do i = 1, mg_total
          p => elements(i-3:i+3)
         
		 call flux_comput(p,elements(i)%point_left%flux1,elements(i)%point_right%flux1)  !reconstruction of F_tail at each approx chara line
    
        enddo 
     
        do i = 1, mg_total
        elements(i)%up_itg = us_n(i)-0.25*dt*(elements(i)%point_right%flux1 - elements(i)%point_left%flux1 &
        & + elements(i)%point_right%flux0 - elements(i)%point_left%flux0)
        
        enddo 
! Step 3 ---------------------------------------------------------  
        call downstream_locate(dt/2.,mg_total,dcell) 
         call bc_1d
        do i = 1, mg_total
          p => elements(i-3:i+3)
         
		 call flux_comput(p,elements(i)%point_left%flux2,elements(i)%point_right%flux2)  !reconstruction of F_tail at each approx chara line
         
        enddo 
     
        do i = 1, mg_total
        elements(i)%up_itg = us_n(i)-dt/6.*(elements(i)%point_right%flux1 - elements(i)%point_left%flux1 &
        & + elements(i)%point_right%flux0 - elements(i)%point_left%flux0 &
        & + 4.*(elements(i)%point_right%flux2 - elements(i)%point_left%flux2 ))
        
        enddo 
         
!reaverage -------------------------------
        call downstream_locate(dt,mg_total,dcell ) 
        call bc_1d
        
         call reproject(avg_new(:),n_cell,dcell)
         avg_output(1:n_cell) = avg_new(1:n_cell)
         
end subroutine evolve