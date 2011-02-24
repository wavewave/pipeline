#PBS -q flux
##PBS -l nodes=1:ppn=1
#PBS -N math1
##PBS -o $ofile$
#PBS -S /bin/bash
#PBS -M iankim@umich.edu
#PBS -j oe
#PBS -V


##module load mathematica/7.0
cd $workdir$ 
date
math <  $mfile$  > $ofile$
date
