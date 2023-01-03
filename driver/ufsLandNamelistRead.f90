module NamelistRead

implicit none
save
private

integer, parameter :: maximum_names = 200

type, public :: namelist_type

  character*128  :: static_file
  character*128  :: init_file
  character*128  :: forcing_dir
  
  logical        :: separate_output
  integer        :: output_frequency_s
  character*128  :: output_dir
  logical        :: output_initial
  
  integer        :: timestep_seconds

  integer        :: restart_frequency_s
  logical        :: restart_simulation
  character*19   :: restart_date
  character*128  :: restart_dir
  
  character*19   :: simulation_start
  character*19   :: simulation_end
  integer        :: run_timesteps
  integer        :: restart_timesteps
  integer        :: output_timesteps
  
  integer        :: location_start
  integer        :: location_end
  integer        :: location_length

  integer        :: subset_start
  integer        :: subset_end
  integer        :: subset_length

  integer        :: land_model

  integer        :: run_days
  integer        :: run_hours
  integer        :: run_minutes
  integer        :: run_seconds

  integer        :: num_soil_levels
  integer        :: num_snow_levels
  real           :: forcing_height

  real, allocatable, dimension(:) ::  soil_level_thickness ! soil level thicknesses [m]
  real, allocatable, dimension(:) ::  soil_level_nodes     ! soil level centroids from surface [m]
  
  double precision :: initial_time

  integer        ::  dynamic_vegetation_option
  integer        ::  canopy_stomatal_resistance_option
  integer        ::  soil_wetness_option
  integer        ::  runoff_option
  integer        ::  surface_exchange_option
  integer        ::  supercooled_soilwater_option
  integer        ::  frozen_soil_adjust_option
  integer        ::  radiative_transfer_option
  integer        ::  snow_albedo_option
  integer        ::  precip_partition_option
  integer        ::  soil_temp_lower_bdy_option
  integer        ::  soil_temp_time_scheme_option
  integer        ::  thermal_roughness_scheme_option
  integer        ::  surface_evap_resistance_option
  integer        ::  glacier_option
  
  integer        ::  forcing_timestep_seconds
  character*128  ::  forcing_regrid
  character*128  ::  forcing_regrid_weights_filename
  character*128  ::  forcing_type
  character*128  ::  forcing_filename
  character*128  ::  forcing_interp_solar
  character*128  ::  forcing_time_solar
  character*128  ::  forcing_name_precipitation
  character*128  ::  forcing_name_sw_radiation
  character*128  ::  forcing_name_lw_radiation
  character*128  ::  forcing_name_pressure
  character*128  ::  forcing_name_specific_humidity
  character*128  ::  forcing_name_wind_speed
  character*128  ::  forcing_name_temperature
  
  character*128, dimension(maximum_names)  ::  output_names
  character*128, dimension(maximum_names)  ::  restart_names
  
  contains

    procedure, public  :: ReadNamelist         

end type namelist_type
     
contains   

  subroutine ReadNamelist(this)
  
    use time_utilities
  
    class(namelist_type) :: this
    
    character*128  :: static_file = ""
    character*128  :: init_file = ""
    character*128  :: forcing_dir = ""
    
    integer        :: output_frequency_s = 0
    logical        :: separate_output = .false.
    character*128  :: output_dir = ""
    logical        :: output_initial = .false.
  
    integer        :: timestep_seconds = -999

    integer        :: restart_frequency_s = 0
    logical        :: restart_simulation = .false.
    character*19   :: restart_date = ""
    character*128  :: restart_dir = ""
  
    character*19   :: simulation_start = ""
    character*19   :: simulation_end = ""

    integer        :: run_days = -999
    integer        :: run_hours = -999
    integer        :: run_minutes = -999
    integer        :: run_seconds = -999
    integer        :: run_timesteps = -999
    
    integer        :: location_start = -999
    integer        :: location_end = -999
    integer        :: location_length = -999
    integer        :: subset_start = -999
    integer        :: subset_end = -999
    integer        :: subset_length = -999
    
    integer        :: land_model = -999

    integer        :: num_soil_levels = -999
    integer        :: num_snow_levels = -999
    real           :: forcing_height = -999.

    real, allocatable, dimension(:) ::  soil_level_thickness ! soil level thicknesses [m]
    real, allocatable, dimension(:) ::  soil_level_nodes     ! soil level centroids from surface [m]
    
    double precision :: run_time = -999.d0
    
    integer        ::  dynamic_vegetation_option         = -999
    integer        ::  canopy_stomatal_resistance_option = -999
    integer        ::  soil_wetness_option               = -999
    integer        ::  runoff_option                     = -999
    integer        ::  surface_exchange_option           = -999
    integer        ::  supercooled_soilwater_option      = -999
    integer        ::  frozen_soil_adjust_option         = -999
    integer        ::  radiative_transfer_option         = -999
    integer        ::  snow_albedo_option                = -999
    integer        ::  precip_partition_option           = -999
    integer        ::  soil_temp_lower_bdy_option        = -999
    integer        ::  soil_temp_time_scheme_option      = -999
    integer        ::  thermal_roughness_scheme_option   = -999
    integer        ::  surface_evap_resistance_option    = -999
    integer        ::  glacier_option                    = -999

    integer        ::  forcing_timestep_seconds = -999
    character*128  ::  forcing_regrid = ""
    character*128  ::  forcing_regrid_weights_filename = ""
    character*128  ::  forcing_type = ""
    character*128  ::  forcing_filename = ""
    character*128  ::  forcing_interp_solar = ""
    character*128  ::  forcing_time_solar = ""
    character*128  ::  forcing_name_precipitation = ""
    character*128  ::  forcing_name_temperature = ""
    character*128  ::  forcing_name_specific_humidity = ""
    character*128  ::  forcing_name_wind_speed = ""
    character*128  ::  forcing_name_pressure = ""
    character*128  ::  forcing_name_sw_radiation = ""
    character*128  ::  forcing_name_lw_radiation = ""

    character*128, dimension(maximum_names)  ::  output_names = ""
    character*128, dimension(maximum_names)  ::  restart_names = ""
  
    integer, parameter :: NOAHMP_LAND_SURFACE_MODEL = 2
  
    namelist / run_setup  / static_file, init_file, forcing_dir, output_dir, timestep_seconds, &
                            simulation_start, simulation_end, run_days, run_hours, run_minutes, &
			    run_seconds, run_timesteps, separate_output, location_start, location_end, &
			    restart_dir, restart_frequency_s, restart_simulation, restart_date, &
                            output_frequency_s, output_initial
    namelist / land_model_option / land_model
    namelist / structure  / num_soil_levels, forcing_height
    namelist / soil_setup / soil_level_thickness, soil_level_nodes
    namelist / noahmp_options /                                                        &
               dynamic_vegetation_option         , canopy_stomatal_resistance_option , &
               soil_wetness_option               , runoff_option                     , &
               surface_exchange_option           , supercooled_soilwater_option      , &
               frozen_soil_adjust_option         , radiative_transfer_option         , &
               snow_albedo_option                , precip_partition_option           , &
               soil_temp_lower_bdy_option        , soil_temp_time_scheme_option      , &
               thermal_roughness_scheme_option   , &
               surface_evap_resistance_option    , glacier_option                    
    namelist / forcing / forcing_timestep_seconds       , forcing_regrid            , &
                         forcing_regrid_weights_filename,                             &
                         forcing_type                   , forcing_filename          , &
			 forcing_interp_solar           , forcing_time_solar        , &
                         forcing_name_precipitation     , forcing_name_temperature  , &
                         forcing_name_specific_humidity , forcing_name_wind_speed   , &
			 forcing_name_pressure          , forcing_name_sw_radiation , &
                         forcing_name_lw_radiation
    namelist / io / output_names, restart_names
    
!---------------------------------------------------------------------
!  read input file, part 1
!---------------------------------------------------------------------

    open(30, file="ufs-land.namelist", form="formatted")
     read(30, run_setup)
     read(30, land_model_option)
     read(30, structure)
     read(30, forcing)
     read(30, io)
    close(30)

    allocate (soil_level_thickness (1:num_soil_levels))   ! soil level thicknesses [m]
    allocate (soil_level_nodes     (1:num_soil_levels))   ! soil level centroids from surface [m]
    
    if(forcing_timestep_seconds < 0) forcing_timestep_seconds = timestep_seconds

!---------------------------------------------------------------------
!  read input file, part 2
!---------------------------------------------------------------------

    open(30, file="ufs-land.namelist", form="formatted")
     read(30, soil_setup)
    close(30)
    
!---------------------------------------------------------------------
!  read input file, part 3
!---------------------------------------------------------------------

    open(30, file="ufs-land.namelist", form="formatted")
     if(land_model == NOAHMP_LAND_SURFACE_MODEL) then
       read(30, noahmp_options)
     end if
    close(30)
    
!---------------------------------------------------------------------
!  transfer to structure
!---------------------------------------------------------------------

    this%land_model           = land_model
    this%static_file          = static_file
    this%init_file            = init_file
    this%forcing_dir          = forcing_dir
    this%output_frequency_s   = output_frequency_s
    this%output_dir           = output_dir
    this%separate_output      = separate_output
    this%output_initial       = output_initial
    this%timestep_seconds     = timestep_seconds
    this%restart_frequency_s  = restart_frequency_s
    this%restart_simulation   = restart_simulation
    this%restart_date         = restart_date
    this%restart_dir          = restart_dir
    this%location_start       = location_start
    this%location_end         = location_end
    this%location_length      = location_end - location_start + 1
    this%subset_start         = this%location_start
    this%subset_end           = this%location_end
    this%subset_length        = this%location_length
    this%simulation_start     = simulation_start
    this%simulation_end       = simulation_end
    this%run_days             = run_days
    this%run_hours            = run_hours
    this%run_minutes          = run_minutes
    this%run_seconds          = run_seconds
    this%num_soil_levels      = num_soil_levels
    this%num_snow_levels      = 3   ! this is currently hard-coded in CCPP sfc_noahmp_drv
    this%forcing_height       = forcing_height
    this%soil_level_thickness = soil_level_thickness
    this%soil_level_nodes     = soil_level_nodes
    this%forcing_timestep_seconds       = forcing_timestep_seconds
    this%forcing_regrid                 = forcing_regrid
    this%forcing_regrid_weights_filename= forcing_regrid_weights_filename
    this%forcing_type                   = forcing_type
    this%forcing_filename               = forcing_filename
    this%forcing_interp_solar           = forcing_interp_solar
    this%forcing_time_solar             = forcing_time_solar
    this%forcing_name_precipitation     = forcing_name_precipitation
    this%forcing_name_temperature       = forcing_name_temperature
    this%forcing_name_specific_humidity = forcing_name_specific_humidity
    this%forcing_name_wind_speed        = forcing_name_wind_speed
    this%forcing_name_pressure          = forcing_name_pressure
    this%forcing_name_sw_radiation      = forcing_name_sw_radiation
    this%forcing_name_lw_radiation      = forcing_name_lw_radiation
    
    this%output_names  = output_names
    this%restart_names = restart_names
    
    if(this%location_start <= 0 .or. this%location_end <= 0) then
      write(*,*) "location_start = ", location_start
      write(*,*) "location_end = ", location_end
      write(*,*) "location_start and location_end need to be >0"
      stop
    end if
    
    if(this%location_length <= 0) then
      write(*,*) "location_start = ", location_start
      write(*,*) "location_end = ", location_end
      write(*,*) "no locations to simulate"
      stop
    end if
    
    if(this%forcing_type /= "single_point" .and. &
       this%forcing_type /= "mm_3h" .and. &
       this%forcing_type /= "mm_1h" .and. &
       this%forcing_type /= "dd_1h" ) then
      write(*,*) this%forcing_type, " namelist%forcing_type not recognized"
      stop
    end if
    
    if(this%forcing_regrid /= "none" .and. &
       this%forcing_regrid /= "esmf" ) then
      write(*,*) this%forcing_regrid, " namelist%forcing_regrid not recognized"
      stop
    end if
    
    if(this%forcing_regrid_weights_filename == "" .and. &
       this%forcing_regrid == "esmf" ) then
      write(*,*) "using esmf regrid weights but no forcing_regrid_weights_filename provided in namelist"
      stop
    end if
    
    if(this%forcing_interp_solar /= "linear" .and. &
       this%forcing_interp_solar /= "zenith" ) then
      write(*,*) this%forcing_interp_solar, " namelist%forcing_interp_solar not recognized"
      stop
    end if
    
    if(this%forcing_time_solar /= "instantaneous" .and. &
       this%forcing_time_solar /= "period_average" ) then
      write(*,*) this%forcing_time_solar, " namelist%forcing_time_solar not recognized"
      stop
    end if
    
    if(restart_simulation) then
      call calc_sec_since("1970-01-01 00:00:00",restart_date,0,run_time)
      call date_from_since("1970-01-01 00:00:00", run_time+timestep_seconds, simulation_start)
      this%simulation_start = simulation_start
    end if
    
    if(simulation_end /= "") then
      call calc_sec_since(simulation_start,simulation_end,0,run_time)
      if(mod(int(run_time),timestep_seconds) /= 0) stop "calculated run time not divisible by timestep"
      this%run_timesteps = int(run_time)/timestep_seconds + 1
    elseif(run_days /= 0 .or. run_hours /= 0 .or. run_minutes /= 0 .or. run_seconds /= 0) then
      run_time = 86400*run_days + 3600*run_hours + 60*run_minutes + run_seconds
      if(mod(int(run_time),timestep_seconds) /= 0) stop "calculated run time not divisible by timestep"
      this%run_timesteps = int(run_time)/timestep_seconds + 1
    elseif(run_timesteps /= 0) then
      this%run_timesteps = run_timesteps
    else
      stop "no valid simulation length in namelist"
    end if
    
    if(restart_frequency_s > 0) then
      if(mod(restart_frequency_s,timestep_seconds) == 0) then
        this%restart_timesteps = restart_frequency_s / timestep_seconds
      else
        stop "restart time not divisible by timestep"
      end if
    end if

    if(output_frequency_s > 0) then
      if(mod(output_frequency_s,timestep_seconds) == 0) then
        this%output_timesteps = output_frequency_s / timestep_seconds
      else
        stop "output time not divisible by timestep"
      end if
    end if

    this%initial_time = huge(1.d0)

    if(land_model == NOAHMP_LAND_SURFACE_MODEL) then
      this%dynamic_vegetation_option         = dynamic_vegetation_option
      this%canopy_stomatal_resistance_option = canopy_stomatal_resistance_option
      this%soil_wetness_option               = soil_wetness_option
      this%runoff_option                     = runoff_option
      this%surface_exchange_option           = surface_exchange_option
      this%supercooled_soilwater_option      = supercooled_soilwater_option
      this%frozen_soil_adjust_option         = frozen_soil_adjust_option
      this%radiative_transfer_option         = radiative_transfer_option
      this%snow_albedo_option                = snow_albedo_option
      this%precip_partition_option           = precip_partition_option
      this%soil_temp_lower_bdy_option        = soil_temp_lower_bdy_option
      this%soil_temp_time_scheme_option      = soil_temp_time_scheme_option
      this%thermal_roughness_scheme_option   = thermal_roughness_scheme_option
      this%surface_evap_resistance_option    = surface_evap_resistance_option
      this%glacier_option                    = glacier_option
    end if

  end subroutine ReadNamelist

end module NamelistRead
