#!/bin/bash

# Slurm job options (name, compute nodes, job time, etc.)
# The tasks-per-node is set as the dimension of the matrices that
# are being multiplied together
#SBATCH --job-name=mat_mul_ext
#SBATCH --time=0:05:00
#SBATCH --exclusive
#SBATCH --nodes=1
#SBATCH --tasks-per-node=3
#SBATCH --cpus-per-task=1

# Define budget code, partition (default as "standard" as we are running on 
# CPU nodes) and quality of service also by default as "standard" since our
# code will not run for long.
#SBATCH --account=dc122
#SBATCH --partition=standard
#SBATCH --qos=standard

# Load the defaut HPE MPI environment and compiler of choice (Intel)
module load mpt
module load intel-compilers-19

# Change to the submission directory
cd $SLURM_SUBMIT_DIR

# Set the number of threads to 1 - preventing any system libraries from 
# automatically using threading.
export OMP_NUM_THREADS=1

# Launch the parallel job - using 3 MPI processes and 36 MPI processes
# per node. srun pickps up the distribution from the sbatch options
srun ./mat_mul_ext.exe > mat_mul_ext_MPI.out

