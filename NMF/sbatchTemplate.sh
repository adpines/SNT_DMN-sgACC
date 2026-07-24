#!/bin/bash
#
#SBATCH --job-name=Matlab
#SBATCH --time=48:00:00
#SBATCH -n 1
#SBATCH --mem=100G
#SBATCH -p normal,anishm,owners # Queue names you can submit to
# Outputs ----------------------------------
#SBATCH --mail-user=no@stanford.edu
#SBATCH --mail-type=ALL
# ------------------------------------------
module load matlab
module load biology
module load workbench
