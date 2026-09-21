    subroutine eno3_reproject(p,i0,pt_left,pt_right,f_out)
    implicit none
    real,intent(in) :: pt_left,pt_right
    integer,intent(in) :: i0
    real,intent(out) :: f_out
    type(element1d_downstream), pointer :: p(:)
    real :: avg_l2,avg_l1,avg_c,avg_r1,avg_r2
    real :: a3,b3,c3
    real :: ptl2_mid,ptl1_mid, ptr1_mid,ptr2_mid,ptc_mid
    real ::  avg_m, avg_l, avg_r
    real :: lenl1,lenl2,len3, lenr1,lenr2
    real ::  kesi_left,kesi_right
     
     
    lenl1 = p(i0-1)%point_right%coor - p(i0-1)%point_left%coor
    lenr1 = p(i0+1)%point_right%coor - p(i0+1)%point_left%coor
    len3 = p(i0)%point_right%coor - p(i0)%point_left%coor
    lenl2 = p(i0-2)%point_right%coor - p(i0-2)%point_left%coor
    lenr2 = p(i0+2)%point_right%coor - p(i0+2)%point_left%coor
    
     avg_l2 = p(i0-2)%up_itg/lenl2
     avg_l1 = p(i0-1)%up_itg/lenl1
     avg_c = p(i0)%up_itg/len3
     avg_r1 = p(i0+1)%up_itg/lenr1
     avg_r2 = p(i0+2)%up_itg/lenr2
     
    ptl2_mid = (p(i0-2)%point_right%coor + p(i0-2)%point_left%coor)/2.
    ptl1_mid = (p(i0-1)%point_right%coor + p(i0-1)%point_left%coor)/2.
    ptc_mid = (p(i0)%point_right%coor + p(i0)%point_left%coor)/2.
    ptr1_mid = (p(i0+1)%point_right%coor + p(i0+1)%point_left%coor)/2.
    ptr2_mid = (p(i0+2)%point_right%coor + p(i0+2)%point_left%coor)/2.
    
    kesi_left = (pt_left - ptc_mid)/len3
    kesi_right = (pt_right - ptc_mid)/len3
    
!    !ui-2, ui-1, ui ************************************   
    if(abs((avg_c-avg_l1)/(ptc_mid-ptl1_mid)) .le. abs((avg_c-avg_r1)/(ptc_mid-ptr1_mid))&
    & .and. abs(((avg_l2-avg_l1)/(ptl2_mid-ptl1_mid)-(avg_l1-avg_c)/(ptl1_mid-ptc_mid))/(ptl2_mid-ptc_mid)) &
    & .le. abs(((avg_l1-avg_c)/(ptl1_mid-ptc_mid)-(avg_c-avg_r1)/(ptc_mid-ptr1_mid))/(ptl1_mid-ptr1_mid)))then
    !ui-2, ui-1, ui
    avg_l = p(i0-2)%up_itg/len3
    avg_m = p(i0-1)%up_itg/len3   
    avg_r = p(i0)%up_itg/len3  
    
    a3 = (3.*lenl1*len3**3.*(lenl1+len3)*avg_l-3.*lenl2*len3**3.*(lenl2+2.*lenl1+len3)*avg_m &
    & +3.*lenl2*lenl1*(lenl2+lenl1)*len3**2.*avg_r)/(lenl2*lenl1*(lenl2+lenl1)*(lenl1+len3)*(lenl2+lenl1+len3))
    
    b3 = (lenl1*len3**2.*(2.*lenl1**2.+3.*lenl1*len3+len3**2.)*avg_l-lenl2*len3**2.*(2.*lenl2**2.+6.*lenl1**2.+6.*lenl1*len3 &
    &+len3**2.+3.*lenl2*(2.*lenl1+len3))*avg_m+lenl2*lenl1*(lenl2+lenl1)*len3*(2.*lenl2+4.*lenl1+3.*len3)*avg_r) &
    & /(lenl2*lenl1*(lenl2+lenl1)*(lenl1+len3)*(lenl2+lenl1+len3))
    
    c3 = (-1.*lenl1*len3**3.*(lenl1+len3)*avg_l+lenl2*len3**3.*(lenl2+2.*lenl1+len3)*avg_m &
    & +lenl2*lenl1*(4.*lenl2**2.*(lenl1+len3)+lenl1*(4.*lenl1**2.+8.*lenl1*len3+3.*len3**2.) &
    & +lenl2*(8.*lenl1**2.+12.*lenl1*len3+3.*len3**2.))*avg_r) &
    & /(4.*(lenl2*lenl1*(lenl2+lenl1)*(lenl1+len3)*(lenl2+lenl1+len3)))

   f_out = a3*(kesi_right**3.-kesi_left**3.)/3. + 0.5*b3*(kesi_right**2.-kesi_left**2.) + c3*(kesi_right-kesi_left) 
    !ui, ui+1, ui+2 *****************************************************   
    elseif(abs((avg_c-avg_l1)/(ptc_mid-ptl1_mid)) .ge. abs((avg_c-avg_r1)/(ptc_mid-ptr1_mid))&
    & .and. abs(((avg_l1-avg_c)/(ptl1_mid-ptc_mid)-(avg_r1-avg_c)/(ptr1_mid-ptc_mid))/(ptl1_mid-ptr1_mid)) &
    & .ge. abs(((avg_c-avg_r1)/(ptc_mid-ptr1_mid)-(avg_r1-avg_r2)/(ptr1_mid-ptr2_mid))/(ptc_mid-ptr2_mid)))then
  
    avg_l = p(i0)%up_itg/len3
    avg_m = p(i0+1)%up_itg/len3   
    avg_r = p(i0+2)%up_itg/len3 
    
    a3 = (3.*len3**2.*lenr1*lenr2*(lenr1+lenr2)*avg_l-3.*len3**3.*lenr2*(len3+2.*lenr1+lenr2)*avg_m &
    & +3.*len3**3.*lenr1*(len3+lenr1)*avg_r)/(lenr1*(len3+lenr1)*lenr2*(lenr1+lenr2)*(len3+lenr1+lenr2))
    
    b3 = (-1.*len3*lenr1*lenr2*(lenr1 + lenr2)*(3.*len3+4.*lenr1+2.*lenr2)*avg_l &
    & +len3**2.*lenr2*(len3**2.+6.*len3*lenr1+6.*lenr1**2.+3.*len3*lenr2+6.*lenr1*lenr2 & 
    & +2.*lenr2**2.)*avg_m-len3**2.*lenr1*(len3**2.+3.*len3*lenr1+2.*lenr1**2.)*avg_r) &
    & /(lenr1*(len3+lenr1)*lenr2*(lenr1+lenr2)*(len3+lenr1+lenr2))
    
    c3 = (lenr1*lenr2*(lenr1+lenr2)*(3.*len3**2.+4.*lenr1*(lenr1+lenr2)+4.*len3*(2.*lenr1+lenr2))*avg_l &
    & +len3**3.*lenr2*(len3+2.*lenr1+lenr2)*avg_m-len3**3.*lenr1*(len3+lenr1)*avg_r) &
    & /(lenr1*(len3+lenr1)*lenr2*(lenr1+lenr2)*(len3+lenr1+lenr2)*4.)
    
    f_out = a3*(kesi_right**3.-kesi_left**3.)/3. + 0.5*b3*(kesi_right**2.-kesi_left**2.) + c3*(kesi_right-kesi_left) 
    else
    !ui-1,ui,ui+1**************************************
    avg_l = p(i0-1)%up_itg/len3
    avg_m = p(i0)%up_itg/len3   
    avg_r = p(i0+1)%up_itg/len3  
    
    
   a3 = (3.*len3**3.*lenr1*(len3+lenr1)*avg_l &
        & -3.*lenl1*len3**2.*lenr1*(lenl1+2.*len3 &
        & +lenr1)*avg_m &
        & +3.*lenl1*len3**3.*(lenl1+len3)*avg_r) &
        & /(lenl1*(lenl1+len3)*lenr1*(len3+lenr1) &
        & *(lenl1+len3+lenr1))

    b3 = (-1.*len3**2.*lenr1*(len3**2.+3.*len3*lenr1+2.*lenr1**2.) &
        & *avg_l-lenl1*len3*(lenl1-lenr1)*lenr1*(2.*lenl1 &
        & +3.*len3+2.*lenr1)*avg_m +lenl1*len3**2.*(2.*lenl1**2.&
        & +3.*lenl1*len3+len3**2.)*avg_r)/(lenl1*(lenl1 &
        & +len3)*lenr1*(len3+lenr1)*(lenl1+len3+lenr1))

    c3 = (-1.*len3**3.*lenr1*(len3+lenr1)*avg_l &
        & +lenl1*lenr1*(4.*lenl1**2.*(len3+lenr1)+lenl1 &
        & *(3.*len3+2.*lenr1)**2.+len3*(6.*len3**2.+9.*len3*lenr1 &
        & +4.*lenr1**2.))*avg_m-lenl1*len3**3.*(lenl1 &
        & +len3)*avg_r)/(4.*lenl1*(lenl1+len3) &
        & *lenr1*(len3+lenr1)*(lenl1+len3+lenr1))
    f_out = a3*(kesi_right**3.-kesi_left**3.)/3.+0.5*b3*(kesi_right**2.-kesi_left**2.)+c3*(kesi_right-kesi_left)
    
    endif
   
    end subroutine eno3_reproject