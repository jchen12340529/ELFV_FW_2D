subroutine cross_time(pc,i_time)
    implicit none
    type(merged_cell),pointer :: pc
    real,intent(out) :: i_time
        
    i_time=(pc%coor_right-pc%coor_left)/(pc%left_char-pc%right_char)
        
end subroutine 