!-----------------------------------
! Serial code for 2D Wave Equation
!-----------------------------------

      program wave2d
          implicit none

          ! Type declare variables
          integer, parameter :: M = 1999
          integer :: i,j, xj, yi, tgap, t, n, rem
          real :: x(M+1), y(M+1)
          real :: dt, dx, V_diff
          real :: v_num(4, (M+1)/20, (M+1)/20)
          real, dimension(M+1, M+1) :: vvold, vv, vvnew, v_exact
          real, dimension(M+1, M+1) :: vvN, vvS, vvE, vvW, del2vv

          ! Define vectors and stepsize
          dt = 1 / float(M)
          do i=0, M
             x(i+1) = -1 + 2*dt*i
          end do
          dx = 2 / float(M)
          y = x

          ! Define exact solution
          do i = 1, M+1
           yi = y(i)
           do j = 1, M+1
            xj = x(j)
            v_exact(j, i) = exp(-40*((xj-.4)**2 + yi**2))
           end do
          end do

          ! Define numerical solution
          vv = v_exact
          vvold = vv
          tgap = nint((1.0/3.0)/dt)
          dt = (1.0/3.0) / tgap
          do n = 0, 1
             !write(*,'("Starting iteration ",I0)') n
             rem = mod(n, tgap)
             t = n*dt

             ! If n is divisble by tgap (i.e. solutions at t=0, 1/3, 2/3, 1) 
             ! then save results for plotting purposes
             if (rem == 0) then
                v_num(nint(real(n/tgap+1)), :, :) = vvold(::20, ::20)
                !write(*,'("Saved results at iteration ",I0)') n
             end if

             ! Initialise first and last columns of components of RHS of PDE 
             vvN(:, M+1) = 0
             vvS(:, 1) = 0
             vvE(M+1, :) = 0
             vvW(1, :) = 0

             ! Compute components of RHS of PDE
             vvN(:, :M) = vv(:, 2:M+1)
             vvS(:, 2:) = vv(:, :M)
             vvE(:M, :) = vv(2:M+1, :)
             vvW(2:, :) = vv(:M, :)

             ! Compute RHS of PDE 
             del2vv = (vvN + vvS + vvE + vvW - 4*vv)
             del2vv = del2vv / dx**2
             !print*, "Computed RHS of PDE"
             
             ! Compute new timestep of solution
             vvnew = 2*vv - vvold + dt**2 * del2vv
             !print*, "Computed new timestep of solution"

             ! Update matrices ready for next timestep
             vvold = vv
             vv = vvnew
          end do
              
          ! Compute numerical error
          !V_diff = sum(V_num - V_exact)
          !write(*,'("Numerical Error: ",F0.4)'), V_diff
          !do i = 1
             do j = 1, (M+1)/20
                print*, v_exact(j, :)
             end do
          !end do

      end program




