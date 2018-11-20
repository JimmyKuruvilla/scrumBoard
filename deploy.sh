#!/bin/bash
# requires $GH_TOKEN env variable to fetch new github data

R -e "install.packages(c('packrat'))" 
R -e "packrat::snapshot(); packrat::restore()"
Rscript scripts/getScrumBoardData.R 

scp -r data ubuntu@EC2IP:~/scrumBoard
scp index.Rmd ubuntu@EC2IP:~/scrumBoard/
