program ufsLandDriver

  use ufsLandNoahDriverModule
  use ufsLandNoahMPDriverModule
  use ufsLandNoahType, only      : noah_type
  use ufsLandNoahMPType, only    : noahmp_type
  use NamelistRead
  use ufsLandStaticModule, only  : static_type
  use ufsLandForcingModule, only : forcing_type
  use module_mpi_land, only: mpi_land_init, mpi_land_type, mpi_land_finalize

  implicit none
  
  type (noah_type)     :: noah
  type (noahmp_type)   :: noahmp
  type (namelist_type) :: namelist
  type (static_type)   :: static
  type (forcing_type)  :: forcing
  type (mpi_land_type) :: mpi_land
  
  integer, parameter :: NOAH_LAND_SURFACE_MODEL = 1
  integer, parameter :: NOAHMP_LAND_SURFACE_MODEL = 2

  call namelist%ReadNamelist()
  
  call mpi_land_init(namelist%location_length,mpi_land)
  
  namelist%subset_start  = mpi_land%location_start
  namelist%subset_end    = mpi_land%location_end
  namelist%subset_length = mpi_land%location_end - mpi_land%location_start + 1
  
  land_model : select case(namelist%land_model)
  
    case(NOAH_LAND_SURFACE_MODEL)

      call ufsLandNoahDriverInit(namelist, static, forcing, noah)

      call ufsLandNoahDriverRun(namelist, static, forcing, noah)

      call ufsLandNoahDriverFinalize()

    case(NOAHMP_LAND_SURFACE_MODEL)

      call ufsLandNoahMPDriverInit(namelist, static, forcing, noahmp)

      call ufsLandNoahMPDriverRun(namelist, static, forcing, noahmp)

      call ufsLandNoahMPDriverFinalize()

    case default

      stop "no valid land_model set in namelist"

  end select land_model
   
  call mpi_land_finalize()
  
end program

