#!/bin/bash

#SBATCH -o ./out/%x_%a.out
#SBATCH --chdir=/gscratch/csde/gknowlt/PrEP-Optimize

source /gscratch/csde/gknowlt/PrEP-Optimize/hyaksim/loadR.sh
Rscript /gscratch/csde/gknowlt/PrEP-Optimize/hyaksim/sim.R
