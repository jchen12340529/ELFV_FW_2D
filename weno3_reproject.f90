subroutine weno3_reproject(p,i0,pt_left,pt_right,f_out)
    implicit none
    real,intent(in) :: pt_left,pt_right
    integer,intent(in) :: i0
    real,intent(out) :: f_out
    type(element1d_downstream), pointer :: p(:)
    real :: a3,b3,c3
    real :: al,bl, ar,br
    real :: gl,gr,gc,gamma, w_l,w_r,w_c, wb_l,wb_r,wb_c
    real :: beta_l, beta_r, tao, beta_c
    real :: f_l,f_r,f_c
    real ::  avg_3, avg_l, avg_r
    real :: lenl,lenr,len3
    real ::  kesi_left,kesi_right,pt_mid
    
    gamma = 0.9
    gl = (1.-gamma)/2.
    gr = (1.-gamma)/2.
    gc = gamma
      
    lenl = p(i0-1)%point_right%coor - p(i0-1)%point_left%coor
    lenr = p(i0+1)%point_right%coor - p(i0+1)%point_left%coor
    len3 = p(i0)%point_right%coor - p(i0)%point_left%coor
    
    pt_mid = (p(i0)%point_right%coor + p(i0)%point_left%coor)/2.
    kesi_left = (pt_left - pt_mid)/len3
    kesi_right = (pt_right - pt_mid)/len3
    
    avg_l = p(i0-1)%up_itg/len3
    avg_3 = p(i0)%up_itg/len3   
    avg_r = p(i0+1)%up_itg/len3  
    
    
   a3 = (3.*len3**3.*lenr*(len3+lenr)*avg_l &
        & -3.*lenl*len3**2.*lenr*(lenl+2.*len3 &
        & +lenr)*avg_3 &
        & +3.*lenl*len3**3.*(lenl+len3)*avg_r) &
        & /(lenl*(lenl+len3)*lenr*(len3+lenr) &
        & *(lenl+len3+lenr))

    b3 = (-1.*len3**2.*lenr*(len3**2.+3.*len3*lenr+2.*lenr**2.) &
        & *avg_l-lenl*len3*(lenl-lenr)*lenr*(2.*lenl &
        & +3.*len3+2.*lenr)*avg_3 +lenl*len3**2.*(2.*lenl**2.&
        & +3.*lenl*len3+len3**2.)*avg_r)/(lenl*(lenl &
        & +len3)*lenr*(len3+lenr)*(lenl+len3+lenr))

    c3 = (-1.*len3**3.*lenr*(len3+lenr)*avg_l &
        & +lenl*lenr*(4.*lenl**2.*(len3+lenr)+lenl &
        & *(3.*len3+2.*lenr)**2.+len3*(6.*len3**2.+9.*len3*lenr &
        & +4.*lenr**2.))*avg_3-lenl*len3**3.*(lenl &
        & +len3)*avg_r)/(4.*lenl*(lenl+len3) &
        & *lenr*(len3+lenr)*(lenl+len3+lenr))
    f_c = a3*(kesi_right**3.-kesi_left**3.)/3.+0.5*b3*(kesi_right**2.-kesi_left**2.)+c3*(kesi_right-kesi_left)
    
    al = 2.*(-len3*avg_l*len3+lenl*avg_3*len3)/(len3*(lenl+len3))
    bl = avg_3
    f_l = 0.5*al*(kesi_right**2.-kesi_left**2.)+bl*(kesi_right-kesi_left)
    
    ar = 2.*(-lenr*avg_3*len3+len3*avg_r*len3)/(len3*(lenr+len3))
    br = avg_3
    f_r = 0.5*ar*(kesi_right**2.-kesi_left**2.)+br*(kesi_right-kesi_left)
    
    beta_c = (39.*len3**4.* (-lenl*lenr*(lenl+lenr)*avg_3+len3**2.*(lenr*avg_l+lenl*avg_r)+len3*(lenr**2.*avg_l &
    & -2.*lenl*lenr*avg_3+lenl**2.*avg_r))**2.+ len3**2.*(2*lenl*lenr*(lenl**2.-lenr**2.)*avg_3  &
    & + len3**3.*(lenr*avg_l-lenl*avg_r) + 3.* len3**2.*(lenr**2.*avg_l-lenl**2.*avg_r) &
    & + len3*(2.*lenr**3.*avg_l+3.*lenl**2.*lenr*avg_3-3.*lenl*lenr**2.*avg_3-2.*lenl**3.*avg_r))**2.) &
    & /((lenl*(lenl+len3)*lenr*(len3+lenr)*(lenl+len3+lenr))**2.)
    
    beta_l = (4.*(-len3*avg_l*len3+lenl*avg_3*len3)**2.)/((lenl*(lenl+len3))**2.)
    
    beta_r = (4.*(-lenr*avg_3*len3+len3*avg_r*len3)**2.)/((lenr*(len3+lenr))**2.)

    tao = (0.5*(abs(beta_c-beta_l)+abs(beta_c-beta_r)))
    
    w_c = gc*(1+tao**2./(beta_c+eps)**2.)
    w_l = gl*(1+tao**2./(beta_l+eps)**2.)
    w_r = gr*(1+tao**2./(beta_r+eps)**2.)
    
!     w_c = gc*(1+tao**2./(beta_c+eps))
!     w_l = gl*(1+tao**2./(beta_l+eps))
!     w_r = gr*(1+tao**2./(beta_r+eps))
    
    wb_c = w_c/(w_c+w_l+w_r)
    wb_l = w_l/(w_c+w_l+w_r)
    wb_r = w_r/(w_c+w_l+w_r)
    
       f_out = wb_c/gc*(f_c-gl*f_l-gr*f_r)+wb_l*f_l+wb_r*f_r
 !        f_out = f_c

    end subroutine weno3_reproject