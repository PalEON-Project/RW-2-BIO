#!/bin/bash
#$ -M mkivi@nd.edu
#$ -m abe
#$ -pe smp 8 
#$ -q long 
#$ -N step_run_HF

module load R
module load gcc

R CMD BATCH ~/RW-2-BIO/sites/HARVARD/inst/run_model_HARVARD.R  output_run_HARVARD.out
