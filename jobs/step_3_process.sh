#!/bin/bash
#$ -M mkivi@nd.edu
#$ -m abe
#$ -pe smp 4 
#$ -q long 
#$ -N step_process_RH

module load R
#module load netcdf/4.7.0/intel/18.0

R CMD BATCH ~/RW-2-BIO/sites/ROOSTER/inst/process_model_ROOSTER.R  output_process_ROOSTER.out
