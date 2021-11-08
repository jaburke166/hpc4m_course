!
! prints what processor it is out of the total number of processors from all ranks
! and finds the closest multiple of three to its rank
!

program n_factorial
  implicit none
  include  "mpif.h"

  integer :: my_rank, teammate_rank, size, ierr
  integer :: i
  integer :: N_final = 10
  integer :: n_current = 1
  integer :: n_factrl = 1
  
  call MPI_INIT(ierr)

  call MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)

  teammate_rank = mod((my_rank + 1),2)
  do while (n_current.lt.N_final) 
     if  (my_rank .eq. mod(n_current,2)) then
        ! First update the factorial
        n_factrl = n_factrl*n_current
        ! Increment the current number
        n_current =  n_current + 1
        print*, "I am process rank ", my_rank, " and going to send ", n_factrl
        call MPI_SEND(n_factrl, 1, MPI_INT, teammate_rank, 0, MPI_COMM_WORLD, ierr)
     else
        ! Increment the current number
        n_current =  n_current + 1
        call MPI_RECV(n_factrl, 1, MPI_INT, teammate_rank, 0, &
             MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
        print*, "I am process rank ", my_rank, " and I received  ", n_factrl
        print*, ' '
     endif
  enddo 
  
  call MPI_FINALIZE(ierr)

end program n_factorial

