module mod_2d
! data structure of 2D
!*******************************
    !  Background cell X(i) for gauss point
    type, public :: background_cell
        sequence
        real :: coor,midpt
        real :: gspt_value(1:3)
    end type

    type(background_cell),allocatable,target,public :: vertex_x(:),vertex_y(:)
!*******************************  
  !merge info
    type, public :: mgcell_info_1d
    sequence
        integer :: cell_id              !trouble cell index
        integer :: numl,numr      !merge left right cell indexes
        real :: xt         !cross time 
        real :: value(1:7)    ! 7 values around the trouble cell 
    end type

    type(mgcell_info_1d),allocatable,target,public :: mgcell_info(:)
!*****************************************
! background cell X(i), Y(i) for merging
    type, public :: merged_cell
    sequence
        real :: coor_left,coor_right
        real :: cell_itg
        real :: left_char,right_char
        real :: left_flux,right_flux
    end type

!    type(merged_cell),allocatable,target,public :: merged_x(:),merged_y(:)
    type(merged_cell),allocatable,target,public :: merged(:)
!**********************************************
    ! Downstream cell 
    type, public :: downstream_cell
        sequence
        real :: coor
        integer :: id
        real :: char
        real :: flux0, flux1,flux2
    end type
   ! type(downstream_cell),allocatable,target,public :: ds_x_star(:) 
!**********************************************    
    type, public :: element1d_downstream
    sequence
    type(downstream_cell) :: point_left,point_right
    real :: up_itg
    real :: int_len
  !  real :: coef_a, coef_b, coef_c
    end type

!type(element1d_downstream),allocatable,target,public :: element_x_star(:),element_y_star(:)
type(element1d_downstream),allocatable,target,public :: elements(:)
    !**************************************** 
    type, public :: F_tail
    sequence
     
    real :: flux_left,flux_right
    end type
    
     type(F_tail),allocatable,target,public :: F_tail_1d(:)
 !*************************************************   
    
end module mod_2d