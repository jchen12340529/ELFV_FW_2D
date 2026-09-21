subroutine bc_1d
implicit none  
integer :: i

if (ibc .eq. 2)then
    do i=0, nd
    merged(-i)%cell_itg = merged(1)%cell_itg
    merged(mg_total+1+i)%cell_itg = merged(mg_total)%cell_itg  
    
    elements(-i)%up_itg = elements(1)%up_itg
    elements(mg_total+1+i)%up_itg = elements(mg_total)%up_itg    
    enddo
 else
    do i=0, nd
    merged(-i)%cell_itg = merged(mg_total-i)%cell_itg
    merged(mg_total+1+i)%cell_itg = merged(1+i)%cell_itg  
    
    elements(-i)%up_itg = elements(mg_total-i)%up_itg
    elements(mg_total+1+i)%up_itg = elements(1+i)%up_itg    
    enddo
endif
    
end subroutine bc_1d  