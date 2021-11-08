//
// prints what processor it is out of the total number of processors from all ranks
// and finds the closest multiple of three to its rank
//

#include <iostream>
#include <mpi.h>
using namespace std;

int main(){
  int rank, size, ierr;
  MPI_Comm comm;

  comm  = MPI_COMM_WORLD;

  MPI_Init(NULL,NULL);
  MPI_Comm_rank(comm, &rank);            
  MPI_Comm_size(comm, &size);
  
  // Find the closest multiple of three to your rank below ...
  
  // Each process prints out its rank
  cout << "I am "<<rank<<" out of "<<size<<" and closest multiple of 3 to me is ..."<<endl;

  MPI_Finalize();

}
