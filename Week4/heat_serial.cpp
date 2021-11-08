#include <iostream>
#include <cmath>

using namespace std;

static const double PI = 3.1415926536; 

int main(int argc, char* argv[]){

int M = 100;  // M length intervals
int N = 10000; // N time intervals
double T = atof(argv[1]);  // get final time from input argument
double U[2][M+1];  // stores the numerical values of function U; two rows to also store values of previous time step 
double Usol[M+1];  // stores true solution 
double dt = T/N;
double dx = 1./M;
double dtdx = dt/(dx*dx);
cout<< "\ndx="<<dx<<", dt="<<dt<<", dt/dx²="<< dtdx<<endl;

// initialize numerical array with given conditions
U[0][0]=0, U[0][M]=0, U[1][0]=0, U[1][M]=0;
for(int m=1; m<M; ++m){
	U[0][m] = sin(2*PI*m*dx) + 2*sin(5*PI*m*dx) + 3*sin(20*PI*m*dx);

}

// use numerical scheme to obtain the future values of U on the M+1 space points
for(int i=1; i<=N; ++i){
	for (int m=1; m<M; ++m){
		U[1][m] = U[0][m] + dtdx * (U[0][m-1] - 2*U[0][m] + U[0][m+1]);	
	}
	// update "old" values	
	for(int m=1; m<M; ++m){
		U[0][m] = U[1][m];
	}
}

// print out array entries of numerical solution next to true solution
cout << "\nTrue and numerical values at M="<<M<<" space points at time T="<<T<<":"<<endl;
cout << "\nTrue values           Numerical solutions\n"<<endl;
for(int m=0; m<=M; ++m){
	Usol[m] = exp(-4*PI*PI*T)*sin(2*PI*m*dx) + 2*exp(-25*PI*PI*T)*sin(5*PI*m*dx) + 3*exp(-400*PI*PI*T)*sin(20*PI*M*dx);		
	cout << Usol[m] << "            " << U[1][m] << endl;
	// note that we did not really need to store the true solution in the array just to print out the values.
}


return 0;

}
