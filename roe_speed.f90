    subroutine roe_speed( uleft,uright,x,spd )
    implicit none
    real,intent(in) :: uleft,uright,x
    real,intent(out) :: spd

    if( abs(uright - uleft)<eps )then
        spd = fp(uleft*0.5+uright*0.5,x)
    else
        spd = ( f(uright,x) - f(uleft,x) )/(uright-uleft)
    endif

    end subroutine  roe_speed