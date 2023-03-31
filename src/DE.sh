#!/bin/bash
#SBATCH --account=def-yergeaue-ab
#SBATCH --mem-per-cpu=32G
#SBATCH --time=0-12:00
module load gcc/9.3.0 r/4.0.2
Rscript 06-DE.R
