 subroutine merging(cell_len)
    implicit none
    integer :: merge_rec(1:nx), i, j, k, m
    real :: cross_t(1:nx)
    integer :: merge_num,cross_num
    type(merged_cell),pointer :: p_merged
    type(mgcell_info_1d), pointer ::  p_cell, p2
    integer :: left_id, right_id
    real,intent(in) :: cell_len

   !     mg_total = n_cell
        merge_num = 2

        do i = 1,mg_total
           merged(i)%left_char = speed(i)
           elements(i)%point_left%char = speed(i)
           merged(i)%right_char = speed(i+1)
           elements(i)%point_right%char = speed(i+1)
           
           merged(i)%left_flux = F_tail_1d(i)%flux_left
           elements(i)%point_left%flux0 = merged(i)%left_flux
           merged(i)%right_flux = F_tail_1d(i)%flux_right
           elements(i)%point_right%flux0 = merged(i)%right_flux
        enddo
        call bc_1d

!100     continue
          call downstream_locate(2.*dt,  mg_total, cell_len)
            cross_t(:) = T*2
            merge_rec(:) = -2*nd
            cross_num = 0

        do j=1,mg_total
        if (elements(j)%point_left%coor .ge. elements(j)%point_right%coor) then
            cross_num = cross_num +1
            merge_rec(cross_num) = j
            p_merged => merged(j) 
            call cross_time(p_merged, cross_t(cross_num))
            !write(*,*) 'the', cross_num,  '-th cross is at', j, 'time=', cross_t(cross_num)
        endif
        enddo
        if(cross_num .eq. 0)then
        !write(*,*) 'No merging'    
        return
        endif
        write(*,*) 'cross num=', cross_num
        
        allocate(mgcell_info(1:cross_num))
        do i = 1, cross_num
        mgcell_info(i)%cell_id = merge_rec(i)
        mgcell_info(i)%xt = cross_t(i)
        mgcell_info(i)%value(1:7) = elements(mgcell_info(i)%cell_id-3:mgcell_info(i)%cell_id+3)%up_itg/cell_len
        p2 => mgcell_info(i)
        call mgnum_criteria(p2)
        Write(2008,*) mgcell_info(i)%cell_id,mgcell_info(i)%numl,mgcell_info(i)%numr
        enddo
        
        call downstream_locate( dt, mg_total, cell_len )    
        
        i = 1
    do while (i .le. cross_num )
        p_cell => mgcell_info(i)
        if ( i .lt. cross_num ) then
        if ( (mgcell_info(i+1)%cell_id - p_cell%cell_id) .le. 1 ) then
            i = i+1   
        endif
        endif
        
            k=0
            left_id = p_cell%cell_id - p_cell%numl 
            right_id = p_cell%cell_id + p_cell%numr 
            k = i+1
  
            do while(k .le. cross_num)
            if ((mgcell_info(k)%cell_id-p_cell%cell_id) .le. (mgcell_info(k)%numl + p_cell%numr))then
            p_cell => mgcell_info(k)
            i = i+1
                if(k .lt. cross_num) then
                if( (mgcell_info(k+1)%cell_id-p_cell%cell_id) .le. 1)then         
                i=i+1
                k=k+1
                endif
                endif
                
            right_id = p_cell%cell_id + p_cell%numr     
            k=k+1
            i=i+1
            else
            exit
            endif
            enddo
            i=i+1
        ! write(*,*)  p_cell%cell_id, left_id, right_id
        if (cross_num .gt. 0 ) then
            merged(left_id)%coor_right = merged(right_id)%coor_right
            elements(left_id)%point_right = elements(right_id)%point_right
            merged(left_id)%right_char = merged(right_id)%right_char  ! characteristic of right end pt
            merged(left_id)%right_flux = merged(right_id)%right_flux
            merged(left_id)%cell_itg = sum(merged(left_id : right_id)%cell_itg)
            elements(left_id)%int_len = elements(right_id)%point_right%coor &
            & -elements(left_id)%point_left%coor
            mg_total = mg_total - (right_id-left_id)
        write(*,*) 'mg_total=',mg_total
        write(2009,*) left_id, right_id  
        
        do j = left_id+1, mg_total+nd 
           merged(j) = merged(j+right_id-left_id)
           elements(j) = elements(j+right_id-left_id)
        enddo 
        
        do m = i,cross_num
            mgcell_info(m)%cell_id = mgcell_info(m)%cell_id - (right_id-left_id)
        enddo
        endif
    enddo 

     deallocate(mgcell_info)
    !if (cross_num .gt. 0)then
    !goto 100
    !endif
end subroutine merging	