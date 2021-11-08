!-----------------------------------------------------------------------
! Using 2 side lengths of a triangle, use the Cosine rule to compute the
! third length of the triangle
!-----------------------------------------------------------------------
      program cosineforumula
              implicit none

              real, dimension(3)  :: L ! INPUT: Lengths of triangle and
                                       ! angle
              real, parameter     :: pi = 3.141592
              real                :: L1, L2 ! Lengths of triangle
              real                :: theta ! Angle between L1, L2
              real                :: L3 ! OUTPUT: Third length of
                                        ! triangle
              logical             :: cond1, cond2 ! check L1, L2 positive

              write(*,*) "Enter 2 lengths of triangle and angle between"
              read(*,*) L
              L1  = L(1)
              L2 = L(2)
              theta = L(3)*(pi / 180)

              cond1 = (L1 > 0.) .and. (L2 > 0.)
              cond2 = (theta > 0.) .and. (L(3) < 360)
              if (cond1 .and. cond2) then
                  L3 = sqrt(L1**2 + L2**2 - 2*L1*L2*cos(theta))
                  write (*,*) "Third side has length", L3
              else if (.not. cond1) then
                  write(*,*) "ERROR. Lengths must both be positive"
              else if (.not. cond2) then 
                  write(*,*) "ERROR. Angle between must be in (0,360)"
              else
                  write(*,*) "ERROR. Please try again"
              end if
       end program
