 subroutine setup
     implicit none

		write(*,*)'T'
		read(*,*) T
        !T= 1.
		write(*,*)'cfl'
		read(*,*) cfl

        !cfl = 4.8 !!!!4.8?
        
 !       write(*,*)  'boundary condition: 1,periodic 2, Riemann problem'
 !       read(*,*) ibc
        ibc = 2
 !       write(*,*) 'splitting order: 1) 1st, 2) 2nd, 3)4th ' 
 !       read(*,*) isp       
         isp = 2 

        write(*,*) '1.2D Riemann Problem for Burgers, 2. 2D burgers sin '
        read(*,*) iexample
         !iexample = 2
        
        if(iexample.eq.2)then
        xleft = 0
		xright = 2
        yleft  = 0
        yright = 2
        else
        xleft = -.5
		xright = .5
        yleft  = -.5
        yright = .5
        endif
        end subroutine setup