#!/bin/bash

# Slurm job options (name, compute nodes, job time)
#SBATCH --job-name=JamieBurke_Serial_Job
#SBATCH --time=0:5:0
#SBATCH --ntasks=1

# Replace [budget code] below with your budget code (e.g. t01)
#SBATCH --account=dc122_28
# We use the "standard" partition as we are running on CPU nodes
#SBATCH --partition=standard
# We use the "standard" QoS as our runtime is less than 4 days
#SBATCH --qos=standard

# Change to the submission directory
cd $SLURM_SUBMIT_DIR

# Enforce threading to 1 in case underlying libraries are threaded
export OMP_NUM_THREADS=1

# Launch the serial job
#   Using 1 thread
srun --cpu-bind=cores ./***your_executable*** >  
