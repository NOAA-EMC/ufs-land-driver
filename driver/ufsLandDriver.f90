program ufsLandDriver

  use ufsLandNoahDriverModule
  use ufsLandNoahMPDriverModule
  use ufsLandNoahType, only      : noah_type
  use ufsLandNoahMPType, only    : noahmp_type
  use NamelistRead
  use ufsLandStaticModule, only  : static_type
  use ufsLandForcingModule, only : forcing_type

  implicit none
  
  type (noah_type)     :: noah
  type (noahmp_type)   :: noahmp
  type (namelist_type) :: namelist
  type (static_type)   :: static
  type (forcing_type)  :: forcing
  
  integer, parameter :: NOAH_LAND_SURFACE_MODEL = 1
  integer, parameter :: NOAHMP_LAND_SURFACE_MODEL = 2

  call namelist%ReadNamelist()
  
  namelist%subset_start  = namelist%location_start
  namelist%subset_end    = namelist%location_end
  namelist%subset_length = namelist%location_end - namelist%location_start + 1
  
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
   
end program

