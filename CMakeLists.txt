# (C) Copyright 2022 .
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.


cmake_minimum_required( VERSION 3.12)
project( ufslanddriver VERSION 2022.10 LANGUAGES Fortran )

# ecbuild integration
find_package(ecbuild 3.3.2 REQUIRED)
include( ecbuild_system NO_POLICY_SCOPE )
ecbuild_declare_project()

list( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
include( ufsland_compiler_flags )

################################################################################
# Dependencies
################################################################################

find_package(OpenMP COMPONENTS C Fortran)
find_package(MPI REQUIRED COMPONENTS C Fortran)
find_package(NetCDF REQUIRED COMPONENTS Fortran )

################################################################################
# Sources
################################################################################

set( UFSLAND_LINKER_LANGUAGE Fortran )

list ( APPEND util_files
./util/module_cosine_zenith.f90
./util/module_error_handling.f90
./util/module_interpolation_utilities.f90
./util/module_time_utilities.f90
./ccpp-physics/physics/machine.F
./ccpp-physics/physics/physcons.F90
./ccpp-physics/physics/funcphys.f90
./ccpp-physics/physics/surface_perturbation.F90
./ccpp-physics/physics/namelist_soilveg.f
./ccpp-physics/physics/set_soilveg.f
./ccpp-physics/physics/sflx.f
./ccpp-physics/physics/sfc_drv.f
./ccpp-physics/physics/date_def.f
./ccpp-physics/physics/module_sf_noahmp_glacier.f90
./ccpp-physics/physics/module_sf_noahmplsm.f90
./ccpp-physics/physics/noahmp_tables.f90
./ccpp-physics/physics/sfc_diff.f
./ccpp-physics/physics/sfc_noahmp_drv.F90
./driver/ufsLandForcingModule.f90
./driver/ufsLandInitialModule.f90
./driver/ufsLandIOModule.f90
./driver/ufsLandNamelistRead.f90
./driver/ufsLandNoahDriverModule.f90
./driver/ufsLandNoahMPDriverModule.f90
./driver/ufsLandNoahMPRestartModule.f90
./driver/ufsLandNoahMPType.f90
./driver/ufsLandNoahRestartModule.f90
./driver/ufsLandNoahType.f90
./driver/ufsLandStaticModule.f90
)

ecbuild_add_library( TARGET   ufslanddriver
                     SOURCES  ${util_files}
                     INSTALL_HEADERS LISTED
                     LINKER_LANGUAGE ${UFSLAND_LINKER_LANGUAGE}
                    )


target_link_libraries(ufslanddriver PUBLIC NetCDF::NetCDF_Fortran)
target_link_libraries(ufslanddriver PUBLIC MPI::MPI_Fortran)
target_link_libraries(ufslanddriver PUBLIC OpenMP::OpenMP_C OpenMP::OpenMP_Fortran)

target_compile_options(ufslanddriver PRIVATE  -r8 -init=huge -cpp, -DCCPP=.false. )

# Fortran module output directory for build and install interfaces
set(MODULE_DIR module/${PROJECT_NAME}/${CMAKE_Fortran_COMPILER_ID}/${CMAKE_Fortran_COMPILER_VERSION})
set_target_properties(${PROJECT_NAME} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/${MODULE_DIR})
install(DIRECTORY ${CMAKE_BINARY_DIR}/${MODULE_DIR}/ DESTINATION ${MODULE_DIR})
target_include_directories(${PROJECT_NAME} INTERFACE
                            $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/${MODULE_DIR}>
                            $<INSTALL_INTERFACE:${MODULE_DIR}>)

set( UFSLANDDRIVER_INCLUDE_DIRS  ${CMAKE_Fortran_MODULE_DIRECTORY} )
set( UFSLANDDRIVER_LIBRARIES landdriver )

# ufsLandDriver.exe executable
#-------------------------------------------------------------------------------
set ( exe_files ./driver/ufsLandDriver.f90 )

ecbuild_add_executable( TARGET   ufsLandDriver.exe
                        SOURCES  ${exe_files}
                        LIBS     ufslanddriver
                        LINKER_LANGUAGE ${UFSLAND_LINKER_LANGUAGE}
                       )


################################################################################
# Finalise configuration
################################################################################

ecbuild_install_project( NAME ufslanddriver )
ecbuild_print_summary()
