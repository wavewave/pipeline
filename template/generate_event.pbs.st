#PBS -q flux
#PBS -l nodes=1:ppn=1,walltime=5:00:00,qos=kzurek_flux
#PBS -A kzurek_flux
#PBS -N math1
##PBS -o #ofile
#PBS -S /bin/bash
#PBS -M iankim@umich.edu
#PBS -j oe
#PBS -V



cd $workdir$ 
date
bin/generate_events 2 1 $runname$ 
date
