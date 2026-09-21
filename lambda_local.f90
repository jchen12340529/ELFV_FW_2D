  subroutine lambda_local(minus,plus,gamma,endpt,lambda_out)
    implicit none
    real,intent(in) :: minus,plus,gamma,endpt
    real,intent(out) :: lambda_out
    real :: local
    integer :: i

     local = 0.

    local = max(abs( fp(minus,endpt)-gamma), abs(fp(plus,endpt)-gamma) )

    lambda_out = local

    end subroutine lambda_local