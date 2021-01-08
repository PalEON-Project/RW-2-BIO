#!/bin/bash
#$ -M mkivi@nd.edu
#$ -m abe
#$ -pe smp 8 
#$ -q long 
#$ -N step_run_GE

module load R
module load gcc

R CMD BATCH ~/RW-2-BIO/sites/GOOSE/inst/run_model_GOOSE.R  output_run_GOOSE.out
