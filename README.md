# ufs-land-driver: a simple land driver for the UFS land models

1) Clone the ufs-land-driver repository: 

`git clone --recurse-submodules https://github.com/barlage/ufs-land-driver.git`

2) Make sure your computer has a Fortran compiler and NetCDF software installed.

3) Navigate to driver

`cd ufs-land-driver`

4) Create a `user_build_config` file

`./configure`

5) Edit the `user_build_config` file to setup compiler, and library paths (to be consistent with your environment if not done by default). 

6) _Optional_ If you'd like to use a different `ccpp-physics` directory from the one automatically downloaded with the clone, set the `PHYSDIR` in `user_build_config` to point to the top of the `ccpp-physics` directory (path relative to the `mod` directory.

7) Compile the code

`make`

All the modules from **ccpp-physics** should be compiled in the `mod` directory, and all the drivers in the `driver` directory, and the executable is in the `run` directory.

**For versions older than 2021-12-06 (commit b3048d3a5), see wiki for instructions**
