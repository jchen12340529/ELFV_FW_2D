subroutine gssum(gspt_in,gssum_out)
implicit none
real,intent(in) :: gspt_in(1:3)
real,intent(out) :: gssum_out
integer :: i

gssum_out = 0.

do i=1,3
gssum_out = wgs(i)*gspt_in(i)+gssum_out
enddo

gssum_out = gssum_out/2.


!  if (mg_total .lt. ny)then
!  write(3007,*) gssum_out
!  endif


end subroutine