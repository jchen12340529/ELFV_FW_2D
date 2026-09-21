 subroutine gspt_recon_5th(pt1,gspt_out)
    implicit none
    real,pointer :: pt1(:)
    real,intent(out) :: gspt_out(1:3)
    integer :: i0,i
    real :: a5,b5,c5,d5,e5
    real :: a3,b3,c3,al,bl,cl,ar,br,cr,xi
    real :: gl,gr,gc,g5,gamma,w_c,w_l,w_r,w_5,wb_c,wb_l,wb_r,wb_5
    real :: beta_l,beta_r,beta_c,tao, beta_5  
    real :: f_c,f_l,f_r,f_5
   
    i0 = 3      ! mid of the stencils
     
    gamma = 0.95
    gl = (1.-gamma)*(1.-gamma)/2.
    gr = (1.-gamma)*(1.-gamma)/2.
    gc = (1.-gamma)*gamma
    g5 = gamma
    
    a5 = (pt1(i0-2)-4.*pt1(i0-1)+6.*pt1(i0)-4.*pt1(i0+1) +pt1(i0+2) )/24.
    b5 = (2.*pt1(i0-1) -pt1(i0-2) -2.*pt1(i0+1) +pt1(i0+2) )/12.
    c5 = (12.*pt1(i0-1) -pt1(i0-2) -22.*pt1(i0) +12.*pt1(i0+1) -pt1(i0+2) )/16.
    d5 = (5.*pt1(i0-2) -34.*pt1(i0-1) +34.*pt1(i0+1) -5.*pt1(i0+2) )/48.
    e5 = (9.*pt1(i0-2) -116.*pt1(i0-1) +2134.*pt1(i0) -116.*pt1(i0+1) +9.*pt1(i0+2) )/1920.
    
    a3 = 0.5*(pt1(i0-1)-2.* pt1(i0)+pt1(i0+1))
    b3 = 0.5*(pt1(i0+1)-pt1(i0-1))
    c3 = (26.*pt1(i0)-pt1(i0+1)-pt1(i0-1))/24.
    
    al = 0.5*(pt1(i0-2) -2.*pt1(i0-1) +pt1(i0) )
    bl = 0.5*(pt1(i0-2) -4.*pt1(i0-1) +3.*pt1(i0) )
    cl = (2.*pt1(i0-1) -pt1(i0-2) +23.*pt1(i0) )/24.
    
    ar = 0.5*(pt1(i0) -2.*pt1(i0+1) +pt1(i0+2) )
    br = 0.5*(-3.*pt1(i0) +4.*pt1(i0+1) -pt1(i0+2) )
    cr = (23.*pt1(i0) +2.*pt1(i0+1) -pt1(i0+2) )/24.
    
    beta_5 = ((-82.*pt1(i0-1)+11.*pt1(i0-2)+82.*pt1(i0+1)-11.*pt1(i0+2) &
    & +2.*pt1(i0-1)-pt1(i0-2)-2.*pt1(i0+1)+pt1(i0+2))/120.)**2. &
    & +13./3.*((40.*pt1(i0-1)-3.*pt1(i0-2)-74.*pt1(i0)+40.*pt1(i0+1)-3.*pt1(i0+2))/56.&
    & + 123./455.*(-4.*pt1(i0-1)+pt1(i0-2)+6.*pt1(i0)-4.*pt1(i0+1)+pt1(i0+2))/24.)**2. &
    & +781./20.*((2.*pt1(i0-1)-pt1(i0-2)-2.*pt1(i0+1)+pt1(i0+2))/12.)**2. &
    & +1421461./2275.*((-4.*pt1(i0-1)+pt1(i0-2)+6.*pt1(i0)-4.*pt1(i0+1)+pt1(i0+2))/24.)**2.
       
    beta_c = 13./12.*(pt1(i0-1) - 2.* pt1(i0)+pt1(i0+1))**2.&
    & +1./4.*(pt1(i0-1)-pt1(i0+1))**2.
    
    beta_l = 13./12.*(pt1(i0-2)-2.*pt1(i0-1)+pt1(i0))**2. &
    & +1./4.*(pt1(i0-2)-4.*pt1(i0-1)+3.*pt1(i0))**2.
    
    beta_r = 13./12.*(pt1(i0)-2.*pt1(i0+1)+pt1(i0+2))**2. &
    & +1./4.*(3.*pt1(i0)-4.*pt1(i0+1)+pt1(i0+2))**2.
    
    tao = (abs(beta_5-beta_l)+abs(beta_5-beta_c)+abs(beta_5-beta_r))/3.
   
    w_5 = g5*(1.+(tao/(beta_5+eps))**2.)
    w_l = gl*(1.+(tao/(beta_l+eps))**2.)
    w_c = gc*(1.+(tao/(beta_c+eps))**2.)
    w_r = gr*(1.+(tao/(beta_r+eps))**2.)
    
    wb_5 =  w_5/(w_c+ w_l+ w_r+w_5)
    wb_l =  w_l/(w_c+ w_l+ w_r+w_5)
    wb_c =  w_c/(w_c+ w_l+ w_r+w_5)
    wb_r =  w_r/(w_c+ w_l+ w_r+w_5)

   do i=1,3 
    xi = gs(i)/2.
    
    f_5 = a5*xi**4.+b5*xi**3.+c5*xi**2.+d5*xi+e5  
    f_c = a3*xi**2.+b3*xi+c3
    f_l = al*xi**2.+bl*xi+cl
    f_r = ar*xi**2.+br*xi+cr
 
    gspt_out(i) = wb_5/g5*(f_5-gc*f_c-gl*f_l-gr*f_r)+wb_c*f_c+wb_l*f_l+wb_r*f_r
!      gspt_out(i) = f_5
    enddo  
    
 !    write(4000,*) beta_5,beta_c,beta_l,beta_r
  !    write(5000,*) wb_5,wb_c,wb_l,wb_r
 !    write(3000,*) pt1(:)
     
    end subroutine gspt_recon_5th