#!/bin/bash
#
#SBATCH -J comparisons            # A single job name for the array
#SBATCH -n 1                 # Number of cores
#SBATCH -N 1                 # Number of nodes for the cores
#SBATCH -t 0-10:00           # Runtime in D-HH:MM format
#SBATCH -p serial_requeue    # Partition to submit to
#SBATCH --mem=4000            # Memory pool for all CPUs
#SBATCH --mail-type=END      # Type of email notification- BEGIN,END,FAIL,ALL
#SBATCH --mail-user=kpoels@g.harvard.edu  #Email to which notifications will be sent

module load gcc/8.2.0-fasrc01 openmpi/3.1.1-fasrc01 R/3.6.1-fasrc01
export R_LIBS_USER=$HOME/apps/R_3.6.1:$R_LIBS_USER

R CMD BATCH --quiet --no-restore --no-save run_group1.${SLURM_ARRAY_TASK_ID}.R
R CMD BATCH --quiet --no-restore --no-save run_group2.${SLURM_ARRAY_TASK_ID}.R
