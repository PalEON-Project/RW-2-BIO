#!/bin/bash
#$ -M mkivi@nd.edu
#$ -m abe
#$ -pe smp 4 
#$ -q long 
#$ -N step_build_GE

module load R
#module load netcdf/4.7.0/intel/18.0

R CMD BATCH ~/RW-2-BIO/sites/GOOSE/inst/build_data_GOOSE.R  output_build_GOOSE.out
