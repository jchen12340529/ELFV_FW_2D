subroutine mgnum_criteria(pc)
    implicit none
    type(mgcell_info_1d), pointer :: pc
    !integer,intent(out) :: num_out
    
    if((pc%value(6)+pc%value(7)) .lt. (umax+3.*umin)/2. .and. (pc%value(3)+pc%value(4) &
    & +pc%value(5)) .ge. (7.*umax+5.*umin)/4.)then

    pc%numl = 2
    pc%numr = 3          !  i-2,....,i+3
    elseif((pc%value(1)+pc%value(2)) .gt. (3.*umax+umin)/2. .and. (pc%value(3)+pc%value(4) &
    & +pc%value(5)) .le. (5.*umax+7.*umin)/4.)then
    
    pc%numl = 3
    pc%numr = 2         ! i-3,...,i+2
    else
    
    pc%numl = 2
    pc%numr = 2          ! i-2,...,i+2
    endif
        
    end subroutine mgnum_criteria