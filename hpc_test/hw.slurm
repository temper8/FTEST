#!/bin/bash
#SBATCH -J hw_test          # Name the job as 'MPItest'
#SBATCH -o hw_test-%j.out   # Write the standard output to file named 'jMPItest-<job_number>.out'
#SBATCH -e hw_test-%j.err   # Write the standard error to file named 'jMPItest-<job_number>.err'
#SBATCH -t 0-00:05:00        # Run for a maximum time of 0 days, 12 hours, 00 mins, 00 secs
#SBATCH --nodes=1            # Request N nodes
#SBATCH --ntasks-per-node=1 # Request n cores or task per node
#SBATCH --mem-per-cpu=4000   # Request 4000MB (4GB) RAM per core
echo $0
echo $1
echo $2
echo $3
module list                 # will list modules loaded by default. In our case, it will be GNU8 compilers and OpenMPI3 MPI libraries
pwd                         # prints current working directory
date                        # prints the date and time
srun python hello_world.py $1
date
wait