# instructions for running C96 simulation on Orion

1) to avoid clutter, create an output directory (referenced in namelist) 

`mkdir output`

note: it seems that the default permissions for an orion directory will not allow writing from a submitted script so you may need to add group write permission:

`chmod g+w output`

2) Submit script to run the model

`sbatch spinup_era5_C96_debug.sh`

Default is a five-day simulation with output in the output/ directory.
