!------------------------------------------
! Serial code for solving 1-D heat equation
!------------------------------------------

      function func(x,t) result(u)
              real :: pi, tau, term1, term2, term3
              parameter(pi = 3.1415926535)
              real, intent(in) :: x,t ! Input
              real :: y ! Output

              tau = pi**2
              term1 = exp(-4*tau*t)*sin(2*pi*x)
              term2 = 2*exp(-25*tau*t)*sin(5*pi*x)
              term3 = 3*exp(-400*tau*t)*sin(20*pi*x)
              
              u = term1 + term2 + term3
      end function

      program heat_eqn_serial
              implicit none

              !include "mpif.h"
              ! Define the integer variables necessary for MPI functions
              !integer :: ierr, comm, size, rank
              !comm = MPI_COMM_WORLD
              !call MPI_INIT(ierr)
              !call MPI_COMM_RANK(comm, rank, ierr)
              !call MPI_COMM_SIZE(comm, size, ierr)
              !call MPI_FINALIZE(ierr)

              ! Type declare variables and parameters
              real, parameter :: x0=0.0, xX=1.0, t0=0.0, tT=0.1
              real, parameter :: pi=3.1415926535
              !integer, parameter :: x_grid = 1000, t_grid = 200000
              integer, parameter :: x_grid = 100, t_grid = 10000
              real :: h_x, h_t, hh_frac, x, t
              real :: U_nm, U_n1m, U_nm1, U_nm_1, func, U_diff
              integer :: i, j, errCode
              real, dimension(x_grid+1, t_grid+1) :: U, U_exact 
              
              ! Compute stepsizes for spatial and temporal variables
              h_x = (xX - x0)/x_grid
              h_t = (tT - t0)/t_grid

              ! Ensure Forward Euler is numerically stable
              hh_frac = h_t / (h_x**2)
              !if (hh_frac > 0.5) then
              !   print*, "Numerically unstable choice of parameters"
              !   print*, "Redefining t_grid and x_grid"
              !   x_grid = int(x_grid / 10)
              !   t_grid = int(t_grid * 10)
              !   goto 420
              !end if
              !write(*,'("New x_grid: ", I0)') x_grid
              !write(*,'("New t_grid: ", I0)') t_grid
              !allocate(U(x_grid, t_grid), stat=errCode)
              !allocate(U_exact(x_grid, t_grid), stat=errCode)

              ! Compute the exact solution for comparison
              U_exact(1, :) = 0
              U_exact(x_grid+1, :) = 0
              do j=1, t_grid+1
                 t = 0 + (j-1)*h_t
                 do i=2, x_grid
                    x = 0 + (i-1)*h_x
                    U_exact(i, j) = func(x,t)
                 end do
              end do

              ! Initialise solution U at different spatial positions
              ! when t=0
              U(1,:) = 0
              U(x_grid+1,:) = 0
              do i=2, x_grid
                 x = 0 + (i-1)*h_x
                 U(i, 1) = sin(2*pi*x) + 2*sin(5*pi*x) + 3*sin(20*pi*x)
              end do
              
              ! Numerically solve the 1D heat equation
              do j=2, t_grid+1
                 do i = 2, x_grid
                    U_nm = U(i, j-1)
                    U_nm1 = U(i+1, j-1)
                    U_nm_1 = U(i-1, j-1)
                    U_n1m = hh_frac * (U_nm_1 - 2*U_nm + U_nm1) + u_nm
            
                    U(i, j) = U_n1m
                 end do
              end do

              ! Compute absolute difference between answers
              !do i=1, x_grid+1
              !   print*, U(i, :)
              !end do
              U_diff = sum(U - U_exact)
              write(*,'("Numerical error: ",F0.5)') U_diff
              
      end program
