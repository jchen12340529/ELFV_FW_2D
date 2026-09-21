    !*******************************

    real function exact(x,y,t)
    implicit none
    real,intent(in) :: x,y,t
    real :: value
      
    if(iexample .eq. 2)then
    if(x.gt.0. .and. x.lt. 1. .and. &
    & y.gt.0. .and. y.lt.1.)then
    value = (sin(pi*x)*sin(pi*y))**2.
    else
    value = 0.
    endif
    
    elseif(iexample.eq.1)then
    if(x.ge.0 .and. x.le. 0.5 .and.y.ge.0 .and. y.le. 0.5)then
    value = 4.
    elseif(x.ge.-0.5 .and. x.le. 0 .and. y.ge.0 .and. y.le. 0.5)then
    value = 3.
    elseif(x.ge.-0.5 .and. x.le. 0 .and. y.ge.-0.5 .and. y.le. 0)then
    value = 2.
    else !x.ge.0 .and. x.le. pi .and. y.ge.-pi .and. y.le.0
    value = 1.
    endif
    endif
    
     

    exact = value
    end function exact
    !*******************************