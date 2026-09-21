  subroutine flux_comput(p,fleft_out,fright_out)
    implicit none
    real,intent(out) :: fleft_out,fright_out
    type(element1d_downstream), pointer :: p(:)
  !  integer :: i0l,i0r,kk
    integer :: i0
    real ::  a3m,b3m,c3m
    real :: a3p,b3p,c3p
    real :: fleft_minus,fleft_plus
    real :: fright_minus,fright_plus
    real :: xi,lambda
    real :: fleft, fright
    
    
!F_{j-1/2}  
    
    i0 = 3  
    call  eno3(p,i0,0.5,fleft_minus)
!    fleft_minus = p(i0)%up_itg/(p(i0)%point_right%coor - p(i0)%point_left%coor)
    i0 = 4
     call  eno3(p,i0,-0.5,fleft_plus)
!    fleft_plus = p(i0)%up_itg/(p(i0)%point_right%coor - p(i0)%point_left%coor)
    
    call lambda_local( fleft_minus,fleft_plus, p(4)%point_left%char, p(4)%point_left%coor,lambda  )
    
    fleft = 0.5*(f(fleft_minus,p(4)%point_left%coor) - p(4)%point_left%char *fleft_minus &
        & + f(fleft_plus,p(4)%point_left%coor)-p(4)%point_left%char *fleft_plus) - 0.5*lambda*(fleft_plus-fleft_minus )
    
    ! F_{j+1/2}
    i0 = 4
     call  eno3(p,i0,0.5,fright_minus)
!    fright_minus = p(i0)%up_itg/(p(i0)%point_right%coor - p(i0)%point_left%coor)
   
    i0 = 5
     call  eno3(p,i0,-0.5,fright_plus)
!    fright_plus = p(i0)%up_itg/(p(i0)%point_right%coor - p(i0)%point_left%coor)
    
    call lambda_local( fright_minus,fright_plus, p(4)%point_right%char, p(4)%point_right%coor,lambda)

    fright = 0.5*(f(fright_minus,p(4)%point_right%coor)-p(4)%point_right%char *fright_minus &
         & + f(fright_plus,p(4)%point_right%coor)-p(4)%point_right%char*fright_plus)-0.5*lambda*(fright_plus-fright_minus)
    
    
    fleft_out = fleft
    fright_out = fright
    
    
    end subroutine flux_comput