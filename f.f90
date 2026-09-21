    real function f(u,x)
    implicit none
    real,intent(in) :: u,x
!    integer, intent(in) :: option
    
        f = 0.5*u**2.
    end function f