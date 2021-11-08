!
! prints what processor it is out of the total number of processors from all ranks
! and finds the closest multiple of three to its rank
!

program matrix_mult
  implicit none
  include  "mpif.h"

  integer :: comm, rank, size, ierr
  integer :: i, j, k
  integer :: N = 3
  integer, dimension(3) :: A_rank, C_rank
  integer, dimension(3,3) :: B, D
  
  comm = MPI_COMM_WORLD

  call MPI_INIT(ierr)

  call MPI_COMM_RANK(comm, rank, ierr)
  call MPI_COMM_SIZE(comm, size, ierr)
  
  ! build matrix B on process 0
  if (rank.eq.0) then
     do j = 1, N
        do i = 1, N
           B(i,j) = (j+i) * (N-j+1)
        enddo
     enddo
     print*, 'B = '
     call print_matrix(B,N)
  endif

  call MPI_BCAST(B, N*N, MPI_INT, 0, comm,ierr)
        
  do j = 1, N
     A_rank(j) = (N-j+(rank+1)+1)*(rank+1)
  enddo

  do j = 1, N
     C_rank(j) = 0
     do i = 1, N
        C_rank(j) = C_rank(j) + A_rank(i) * B(i,j)
     enddo
  enddo

  call MPI_GATHER(C_rank, N, MPI_INT, D, N, MPI_INT, 0, comm, ierr)

  if (rank.eq.0) then
     print*, 'D = '
     call print_matrix(TRANSPOSE(D),N)
  endif

  call MPI_FINALIZE(ierr)

end program matrix_mult

subroutine print_matrix(B,N)
  integer :: N
  integer  :: B(N,N)
  do i=1,N
     WRITE(*,*) B(i,1:N)
  enddo
endsubroutine print_matrix
