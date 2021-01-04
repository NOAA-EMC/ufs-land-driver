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

3) Copy the **user_build_config** from **ufs_land_forcing** repository, and edit this file to setup compiler, and   library paths (to be consistent with your environment).

4) The **makefile** collect all the codes from **ccpp-physics** directory and compile it. 

    Invoke `make` to compile.
 
    All the modules from **ccpp-physics** should be compiled in **mod** directory, and all the drivers in **driver**      directory, and the executable is in **run** directory.

## Example of single point test:
5) There are 2 namelist files, one for noah and another for noah-mp model in **run/examples/single_point**        directory. To modify and test them, copy one of them to the **run** directory (where the ufsLand.exe is) and update   the “static_file”, init_file” and “forcing_dir” file directories.

6) Run the executable by typing: `./ufsLand.exe`

7) The output NetCDF file will be create. 
