!--------------------------------------------------
! Perform matrix multiplication using MPI functions
! 
! This is an extension to mat_mul.f90 where the number
! of processes does NOT equal the size of matrices
! being multiplied together
!--------------------------------------------------
      program mat_mul_ext
          implicit none
        
          ! Include the mpif.h module for MPI function
          include "mpif.h"

          ! Define size of matrices and iterators i, j
          integer :: N, i, j
          parameter(N = 5)
          ! Define the integer variables necessary for MPI functions
          integer :: comm, rank, N_processes, ierr
          
          ! Define matrices A, B, C_builtin, D and vectors
          ! row_c and row_a (to hold interim results of MPI computation
          ! note: C_builtin will hold the computation using matmul()
          ! while D will hold the result computed using MPI functions
          integer, dimension(N, N) :: A, B, C_builtin, CD_diff, D
          integer, dimension(N) :: row_a, row_c

          ! Create allocatble 2D arrays for when N_processes < N
          ! Also create integer status vaiable for allocation
          integer :: errCode
          integer, dimension(:,:), allocatable :: rows_a, rows_c
          integer, dimension(:,:), allocatable :: interim_D

          ! define remainder between the size of matrices and
          ! the number of processors. Also define divisor, the
          ! whole integer number when dividing N by N_processes
          integer :: div, rem
          
          ! Initialise MPI and extract the rank and size of process
          comm = MPI_COMM_WORLD
          call MPI_INIT(ierr)
          call MPI_COMM_RANK(comm, rank, ierr)
          call MPI_COMM_SIZE(comm, N_processes, ierr)

          ! Set up conditions depening on rank (process)
          if (rank == 0) then 
             ! Create matrix B if rank 0 and form 1st row of A
             do i = 1, N
                do j = 1, N
                   B(i,j) = (j + i) * (N - j + 1)
                end do
             end do
          end if 
          
          ! Broadcast B to all other processes
          call MPI_BCAST(B, N**2, MPI_INT, 0, &
                         comm, ierr)
          
          ! Compute divisor between N and N_processes for use 
          ! when number of processes is less than the size of
          ! matrices
          rem = mod(N, N_processes)
          div = floor(float(N)/float(N_processes))       

          ! If the number of processes is greater than or equal
          ! to the size of matrices then only use up to N of the
          ! processes in dealing with the computation
          if (N_processes >= N) then
             if (rank < N) then 
                ! Form rank'th row of A
                do j = 1, N
                   row_a(j) = (N - j + (rank+1) + 1)*(rank + 1)
                end do

                ! Compute vector c, i.e. compute matmul(row_a,B)
                row_c = matmul(row_a, B)
             end if
               
             ! Gather vectors rows_c to put in matrix D
             call MPI_GATHER(row_c, N, MPI_INT, D, N, &
                             MPI_INT, 0, comm, ierr)
          
          ! If the number of the processes are less than N, assign an
          ! equal number of rows of A to each process and populate D.
          else if (N_processes < N) then 
             ! If non-zero remainder (N is not divisble by N_processes
             ! then add 1 to the divisor so compute ceil(N/N_processes)
             if (rem /= 0) then
                div = div + 1
             end if

             ! Allocate the necessary memory to rows_a and rows_c
             ! Also allocate maximum number of columns to interim_D
             allocate(rows_a(div, N), stat = errCode)
             allocate(rows_c(div, N), stat = errCode)
             allocate(interim_D(N, div*N_processes), stat = errCode)

             ! Build rows of A. The row number if maintained by
             ! iterating over the divisor between N and N_processes and
             ! the rank of the process
             do i = 1, div
                do j = 1, N
                   rows_a(i,j) = (N - j + (div*rank+i) + 1)*(div*rank+i)
                end do
             end do

             ! Compute rows of answer and transpose before gathering
             ! them to populate rows of D
             rows_c = matmul(rows_a, B)
             rows_c = transpose(rows_c)
         
             ! Gather vector rows_c to populate D. If N is not divisible
             ! by number of processes then populate interim_D and
             ! discard the last few columns (since FORTRAN is a
             ! column-wise language) which correspond to the extra rows
             ! computed. 
             if (rem /= 0) then
                call MPI_GATHER(rows_c, div*N, MPI_INT, interim_D, &
                                div*N, MPI_INT, 0, comm, ierr)
                D = interim_D(:,:N)
             else
                call MPI_GATHER(rows_c, div*N, MPI_INT, D, div*N, &
                                MPI_INT, 0, comm, ierr) 
             end if
          end if
         
          ! Print results
          if (rank == 0) then
            
             ! Loop over iterators to define matrices A and B
             do i = 1, N
                do j = 1, N
                   A(i, j) = (N - j + i + 1) * i         
                end do
             end do
            
             print*, "Matrix A:"
             do i = 1, N
                print*, A(i, :)
             end do
 
             print*, "Matrix B:"
             do i = 1, N
                print*, B(i, :)
             end do
          
             print*, " "
             C_builtin = matmul(A, B)
             print*,"Multiplying matrices A and B using matmul():"
             do i = 1, N
                print*, C_builtin(i, :)
             end do
  
             print*, " "
             print*, "Multplying matrices A and B using MPI Funcs:"
             D = transpose(D)
             do i = 1, N
                print*, D(i,:)
             end do

             print*, " "
             CD_diff = C_builtin - D
             print*, "Summed difference between answers:" 
             do i = 1, N
                print*, CD_diff(i, :)
             end do

         end if   
          

         ! Stop MPI process
         call MPI_FINALIZE(ierr)
        
      end program mat_mul_ext
