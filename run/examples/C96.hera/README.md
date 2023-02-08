# instructions for running C96 simulation on Hera

1) to avoid clutter, create an output directory (referenced in namelist) 

`mkdir output`

2) Submit script to run the model

`sbatch spinup_era5_C96_debug.sh`

Default is a five-day simulation with output in the output/ directory.
