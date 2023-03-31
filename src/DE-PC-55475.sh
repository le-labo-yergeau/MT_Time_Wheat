#!/bin/bash
#SBATCH --account=def-yergeaue-ab
#SBATCH --mem-per-cpu=64G
#SBATCH --time=0-24:00
module load gcc/9.3.0 r/4.0.2
Rscript 06-DE.R
