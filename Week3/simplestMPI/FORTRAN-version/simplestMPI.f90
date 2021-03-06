3!
! prints what processor it is out of the total number of processors from all ranks
! and finds the closest multiple of three to its rank
!

program simplestMPI
implicit none
include  "mpif.h"

integer :: comm, rank, size, ierr

comm = MPI_COMM_WORLD

call MPI_INIT(ierr)

call MPI_COMM_RANK(comm, rank, ierr)
call MPI_COMM_SIZE(comm, size, ierr)


! Find the closest multiple of three to your rank below ...

! Each process prints out its rank
print*, 'I am ', rank, 'out of ', size,' processors and closest multiple of three to me is ...'
print*,''

call MPI_FINALIZE(ierr)

end program simplestMPI
