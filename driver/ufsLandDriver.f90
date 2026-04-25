program ufsLandDriver

  use mpi

  use ufsLandNoahDriverModule
  use ufsLandNoahMPDriverModule
  use ufsLandNoahType, only      : noah_type
  use ufsLandNoahMPType, only    : noahmp_type
  use NamelistRead
  use ufsLandStaticModule, only  : static_type
  use ufsLandForcingModule, only : forcing_type
  use module_mpi_land,      only : mpi_land_init, mpi_land_type, mpi_land_finalize, mpi_land_abort

  implicit none
  
  type (noah_type)     :: noah
  type (noahmp_type)   :: noahmp
  type (namelist_type) :: namelist
  type (static_type)   :: static
  type (forcing_type)  :: forcing
  type (mpi_land_type) :: mpiland
  
  integer, parameter :: NOAH_LAND_SURFACE_MODEL = 1
  integer, parameter :: NOAHMP_LAND_SURFACE_MODEL = 2

  integer            :: ierr, nprocs, myrank
  character(len=3)   :: mem_str

  call mpi_init(ierr) 
  call mpi_comm_size( MPI_COMM_WORLD, nprocs, ierr )
  call mpi_comm_rank( MPI_COMM_WORLD, myrank, ierr )

  if (myrank==0) print*, "starting ufs land driver on ",nprocs," procs"

  call namelist%ReadNamelist()
  
  call mpi_land_init(namelist%ens_size,namelist%location_length,myrank,nprocs,mpiland)
  
  if(namelist%ens_size > 1) then   
    write(mem_str, '(I3.3)') mpiland%group_id + 1
    namelist%restart_dir = trim(namelist%restart_dir)//"/mem"//mem_str//"/"
    namelist%output_dir = trim(namelist%output_dir)//"/mem"//mem_str//"/"
    namelist%forcing_dir = trim(namelist%forcing_dir)//"/mem"//mem_str//"/"
  endif

  namelist%subset_start  = mpiland%location_start + namelist%location_start - 1
  namelist%subset_end    = mpiland%location_end + namelist%location_start - 1
  namelist%subset_length = mpiland%location_end - mpiland%location_start + 1
  
  land_model : select case(namelist%land_model)
  
    case(NOAH_LAND_SURFACE_MODEL)

      call ufsLandNoahDriverInit(namelist, static, forcing, noah)

      call ufsLandNoahDriverRun(namelist, static, forcing, noah, mpiland%comm_group, mpiland%myrank)

      call ufsLandNoahDriverFinalize()

    case(NOAHMP_LAND_SURFACE_MODEL)

      call ufsLandNoahMPDriverInit(namelist, static, forcing, noahmp, mpiland%comm_group, mpiland%myrank)

      call ufsLandNoahMPDriverRun(namelist, static, forcing, noahmp, mpiland%comm_group, mpiland%myrank)

      call ufsLandNoahMPDriverFinalize()

    case default

    if (myrank==0) print*, " stop. no valid land_model set in namelist"
    call mpi_land_abort()

  end select land_model
   
  call mpi_land_finalize()
  
end program

