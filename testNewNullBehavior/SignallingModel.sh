#!/bin/bash
#SBATCH --job-name=testNewNullBehavior
#SBATCH --mail-user=jacob.chisausky@evobio.eu
#SBATCH --mail-type=ALL
#SBATCH --time=0-12:00:00
#SBATCH --mem=8000
#SBATCH --array=1-5

module load foss/2022a

echo "SLURM_JOBID: " $SLURM_JOBID
echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
echo "SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID

./Program `ls -d *.json | awk NR==$SLURM_ARRAY_TASK_ID`
