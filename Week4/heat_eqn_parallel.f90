!------------------------------------------------
! Parallelised code for solving 1-D heat equation
!------------------------------------------------
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

      program heat_eqn_parallel
          implicit none

          include "mpif.h"
              
          ! Define the integer variables necessary for MPI functions
          integer :: ierr, comm, size, rank

          ! Declare parameters: spatial and temporal boundaries and pi
          real, parameter :: x0=0.0, xX=1.0, t0=0.0, tT=0.1
          real, parameter :: pi=3.1415926535

          ! Declare parameters: # processors, number of spatial and temporal
          ! steps, the number of points in each subinterval, J. M and nproc has
          ! been chosen so that (M-2)/nproc is an integer much greater than 3.
          ! Choosing 4 processes, 146 spatial steps means 38 points for each
          ! process
          integer, parameter :: nproc=4, M=144+2, Jj = (M-2)/nproc+2

          ! Declare stepsize in space and time, the multiplicative factor to 
          ! ensure stability in Euler's rule (dt/dx^2 < 0.5(=mu)). Also define
          ! number of timesteps.
          real, parameter :: dx = 1.0/M, mu = 0.25, dt=0.25*(dx**2)
          integer, parameter :: N = ceiling(tT/dt)

          ! Declare variables - real variables
          real :: U_nm, U_n1m, U_nm1, U_nm_1, U_send, U_recv
          real :: func, U_diff, x, t

          ! Declare variables - integer variables
          integer :: i, j, k, r, s, N_subints
          integer :: tag1, tag2, errCode

          ! Declare variables - arrays
          real :: U(M+1, N+1), U_exact(M+1, N+1), U_interim(Jj) 
          integer, allocatable :: subint_idxs(:,:)
              
          ! Call MPI functions to store rank and size
          comm = MPI_COMM_WORLD
          call MPI_INIT(ierr)
          call MPI_COMM_RANK(comm, rank, ierr)
          call MPI_COMM_SIZE(comm, size, ierr)

          ! Ensure the number of processes hired is equal to nproc
          if (size .ne. nproc) then
           write(*,'(I0," processes hired does not match preset value: ",I0)') &
                size, nproc
           MPI_ABORT(comm, ierr)
           MPI_FINALIZE(ierr)
          end if

          ! Compute the number of subintervals: 150 subintervals 
          N_subints = ((M-2*(Jj-1)) / (Jj-2)) + 2
 
          ! Fill array with indexes for each subinterval. Each column
          ! corresponds to the column indexes that constitute that rows
          ! columns subinterval. The array only goes up to x_grid so we
          ! discard the first element of 1 since it's a BC
          allocate(subint_idxs(Jj, N_subints), stat=errCode)
          do k=1, N_subints
             do i=1, N_pts
                subint_idxs(i,k) = i + (Jj-2)*(k-1)
             end do
          end do

          !Compute the exact solution for comparison
          U_exact(1, :) = 0
          U_exact(M+1, :) = 0
          do k=1, N+1
             t = 0 + (k-1)*dt
             do i=2, M
                x = 0 + (i-1)*dt
                U_exact(i, k) = func(x,t)
             end do
          end do

          ! Initialise solution U at different spatial positions
          ! when t=0 at root process
          if (rank == 0) then
             U(1,:) = 0
             U(M+1,:) = 0
             do i=2, M
                x = 0 + (i-1)*dx
                U(i, 1) = sin(2*pi*x) + 2*sin(5*pi*x) + 3*sin(20*pi*x)
             end do
          end if

          ! Broadcast initial condition to other processes
          call MPI_BCAST(U, (M+1)*(N+1), MPI_REAL, 0, &
                        comm, ierr)

          ! Loop temporally and over number of subintervals
          ! IF statement to ensure that a particular process deals
          ! with a particular subinterval
          r = rank
          do j=2, N+1
           do s=1, N_subints
            i = 2
            if (rank == s-1) then
             ! If first subinterval only send and recieve messages at
             ! the end of the interval.
             if (s==1) then

                do while (i <= Jj)
                   k = subint_idxs(i, s)
                   U_nm = U(k, j-1)
                   U_nm_1 = U(k-1, j-1)
                   if (i==Jj) then 
                      tag1 = (j-2)*(2*size)+2*r+1
                      tag2 = (j-2)*(2*size)+2*r
                      U_send = U(k-1, j)
                      call MPI_Bsend(U_send, 1, MPI_REAL, rank+1, &
                               tag1, comm, MPI_STATUS_IGNORE, ierr)
                   
                      call MPI_Recv(U_recv, 1, MPI_REAL, rank+1, &
                               tag2, comm, MPI_STATUS_IGNORE, ierr)
                      U(k, j) = U_recv
                   else
                      U_nm1  = U(k+1, j-1)
                      U_n1m = mu * (U_nm_1 - 2*U_nm + U_nm1) + U_nm
                      U(k, j) = U_n1m
                   end if

                   i = i+1
                end do
             
             ! If last subinterval only send and reiceive messages at
             ! the beginning of the interval
             elseif (s==N_subints) then
                
                do while (i <= Jj)
                   k = subint_idxs(i, s)
                   U_nm = U(k, j-1)
                   U_nm1 = U(k+1, j-1)
                   U_nm_1 = U(k-1,j-1)
                   if (i==2) then
                      tag1 = (j-2)*(2*size)+2*(r-1)+1
                      tag2 = (j-2)*(2*size)+2*(r-1)

                      call MPI_Recv(U_recv, 1, MPI_REAL, rank-1, &
                                   tag1, comm, MPI_STATUS_IGNORE, ierr)
                      U(k-1, j) = U_recv
                      !write(*,'("R: ",I0," T: ",I0," S: ",I0," Recv")') r, j, s

                      U_send = mu * (U_nm_1 - 2*U_nm + U_nm1) + U_nm
                      U(k, j) = U_send
                      call MPI_Bsend(U_send, 1, MPI_REAL, rank-1, &
                                   tag2, comm, MPI_STATUS_IGNORE, ierr)
                      !write(*,'("R: ",I0," T: ",I0," S: ",I0," Sent")') r, j, s

                   else
                      U_nm_1 = U(k-1, j-1)        
                      U_n1m = mu * (U_nm_1 - 2*U_nm + U_nm1) + U_nm
                      U(k, j) = U_n1m
                   end if

                   i = i + 1
                end do
             
             ! If intermediate subinterval send and recieve messages at
             ! the start and end of the interval
             else

                do while (i <= N_pts)
                   k = subint_idxs(i,s)                         
                   U_nm = U(k, j-1)
                   U_nm_1 = U(k-1,j-1)
                   if (i == 2) then 
                      tag1 = (j-2)*(2*size)+2*(r-1)+1
                      tag2 = (j-2)*(2*size)+2*(r-1)

                      U_nm1 = U(k+1, j-1)
                      call MPI_Recv(U_recv, 1, MPI_REAL, rank-1, &
                                   tag1, comm, MPI_STATUS_IGNORE, ierr)
                      U(k-1,j) = U_recv
                      
                      U_send = mu * (U_nm_1 - 2*U_nm + U_nm1) + U_nm
                      U(k,j) = U_send
                      call MPI_Bsend(U_send, 1, MPI_REAL, rank-1, &
                                    tag2, comm, MPI_STATUS_IGNORE, ierr)
                   elseif (i == Jj) then 
                      tag1 = (j-2)*(2*size)+2*r+1
                      tag2 = (j-2)*(2*size)+2*r

                      U_send = U(k-1, j)
                      call MPI_Bsend(U_send, 1, MPI_REAL, rank+1, &
                              tag1, comm, MPI_STATUS_IGNORE, ierr)

                      call MPI_Recv(U_recv, 1, MPI_REAL, rank+1, &
                              tag2, comm, MPI_STATUS_IGNORE, ierr)
                      U(k, j) = U_recv
                   else
                      U_nm1 = U(k+1, j-1)
                      U_n1m = mu * (U_nm_1 - 2*U_nm + U_nm1) + U_nm
                      U(k, j) = U_n1m
                   end if

                   i = i+1
                end do
             end if
             
             ! Send results from other processes to root process
             if (s /= 1) then
                U_interim = U(subint_idxs(1, s):subint_idxs(Jj, s),j)
                call MPI_Ssend(U_interim, Jj, MPI_REAL, 0, &
                         (j-2)*2*size-1, comm, MPI_STATUS_IGNORE,ierr)
             end if
 
            end if

            ! If root process, recieve message and store results in
            ! array
            if (rank==0 .and. s /= 1) then
               call MPI_Recv(U_interim, Jj, MPI_REAL, s-1, &
                       (j-2)*2*size-1, comm, MPI_STATUS_IGNORE, ierr)
               U(subint_idxs(1,s):subint_idxs(Jj,s), j) = U_interim
             end if

           end do
          end do

          ! Compute absolute difference between answers
          if (rank == 0) then
          !   do i=1, t_grid+1
          !      print*, U(:, i)
          !   end do
          
             U_diff = sum(U - U_exact)
             write(*,'("Numerical error: ",F0.5)') U_diff
          end if

          ! Stop MPI procedure
          call MPI_FINALIZE(ierr)

      end program
