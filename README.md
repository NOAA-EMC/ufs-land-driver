# ufs-land-driver

ufs-land-driver: a simple land driver for the UFS land models.

The model forcing and driver are seperated in two repositories: **ufs_land_forcing** and **ufs_land_driver** respectively.

1) First clone these 3 repositories: 

    https://github.com/barlage/ufs-land-driver.git

    https://github.com/barlage/ufs-land-forcing.git

    https://github.com/HelinWei-NOAA/ccpp-physics.git

2) In the "ufs-land-driver" repository, Use the **feature/noahmp** branch, so: 

    `git fetch`

    `git checkout feature/noahmp`

3) Make sure your computer has FORTRAN compiler and NetCDF software installed.

4) Edit the **user_build_config** file to setup compiler, and library paths (to be consistent with your environment).

5) Inside **ccpp-physics/physics** directory we can see the noah-mp model under **module_sf_noahmplsm.f90** name. The **makefile** collect all the codes from **ccpp-physics** directory and compile it. 

    Invoke `make` to compile.
 
    All the modules from **ccpp-physics** should be compiled in **mod** directory, and all the drivers in **driver**      directory, and the executable is in **run** directory.

## Example of single point test:
6) There are 2 namelist files, one for noah and another for noah-mp model in **run/examples/single_point**        directory. To modify and test them, copy one of them to the **run** directory (where the ufsLand.exe is) and update   the “static_file”, init_file” and “forcing_dir” file directories.

7) Run the executable by typing: `./ufsLand.exe`

7) The output NetCDF file will be create. 
