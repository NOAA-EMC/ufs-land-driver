module ufsLandNoahMPDriverModule

implicit none

contains

subroutine ufsLandNoahMPDriverInit(namelist, static, forcing, noahmp)

  use NamelistRead
  use ufsLandNoahMPType
  use ufsLandStaticModule
  use ufsLandInitialModule
  use ufsLandForcingModule
  use ufsLandNoahMPRestartModule

  implicit none

  type (namelist_type)       :: namelist
  type (noahmp_type)         :: noahmp
  type (static_type)         :: static
  type (initial_type)        :: initial
  type (forcing_type)        :: forcing
  type (noahmp_restart_type) :: restart

  call static%ReadStatic(namelist)
  
  call noahmp%Init(namelist,namelist%subset_length)

  if(namelist%restart_simulation) then
    call restart%ReadRestartNoahMP(namelist, noahmp)
  else
    call initial%ReadInitial(namelist)
    call initial%TransferInitialNoahMP(namelist, noahmp)
  end if
  
  call static%TransferStaticNoahMP(noahmp)
  
  call noahmp%TransferNamelist(namelist)
  
  call forcing%ReadForcingInit(namelist)
  
end subroutine ufsLandNoahMPDriverInit
  
subroutine ufsLandNoahMPDriverRun(namelist, static, forcing, noahmp)

use machine , only : kind_phys
use noahmpdrv
use set_soilveg_mod
use funcphys
use namelist_soilveg, only : z0_data
use noahmp_tables
use physcons, only : con_hvap , con_cp, con_jcal, con_eps, con_epsm1,    &
                     con_fvirt, con_rd, con_hfus, con_g  ,               &
		     tfreeze=> con_t0c, rhoh2o => rhowater

use interpolation_utilities
use time_utilities
use cosine_zenith
use NamelistRead, only         : namelist_type
use ufsLandNoahMPType, only    : noahmp_type
use ufsLandStaticModule, only  : static_type
use ufsLandForcingModule
use ufsLandIOModule
use ufsLandNoahMPRestartModule

type (namelist_type)  :: namelist
type (noahmp_type)    :: noahmp
type (forcing_type)   :: forcing
type (static_type)    :: static
type (output_type)    :: output
type (noahmp_restart_type)    :: restart

integer          :: timestep
double precision :: now_time
character*19     :: now_date  ! format: yyyy-mm-dd hh:nn:ss
integer          :: now_yyyy

integer                            :: itime      ! not used
integer                            :: errflg     ! CCPP error flag
character(len=128)                 :: errmsg     ! CCPP error message
   real, allocatable, dimension(:) :: rho        ! density [kg/m3]
   real, allocatable, dimension(:) :: u1         ! u-component of wind [m/s]
   real, allocatable, dimension(:) :: v1         ! v-component of wind [m/s]
   real, allocatable, dimension(:) :: snet       ! shortwave absorbed at surface [W/m2]
   real, allocatable, dimension(:) :: prsl1      ! pressure at forcing height [Pa]
   real, allocatable, dimension(:) :: srflag     ! snow ratio for precipitation [-]
   real, allocatable, dimension(:) :: prslki     ! Exner function at forcing height [-]
   real, allocatable, dimension(:) :: prslk1     ! Exner function at forcing height [-]
   real, allocatable, dimension(:) :: prsik1     ! Exner function at forcing height [-]
   real, allocatable, dimension(:) :: cmm        ! Cm*U [m/s]
   real, allocatable, dimension(:) :: chh        ! Ch*U*rho [kg/m2/s]
   real, allocatable, dimension(:) :: shdmin     ! minimum vegetation fraction(not used) [-]
   real, allocatable, dimension(:) :: smcref2    ! field capacity(not used) [m3/m3]
   real, allocatable, dimension(:) :: smcwlt2    ! wilting point(not used) [m3/m3]
   real, allocatable, dimension(:) :: snohf      ! snow melt energy that exist pack [W/m2]
   real, allocatable, dimension(:) :: sncovr1    !  copy of snow cover(not used)[-]
   real, allocatable, dimension(:) :: snoalb     ! snow-covered-area albedo(not used) [-]
   real, allocatable, dimension(:) :: tsurf      ! copy of tskin(not used) [K]
   real, allocatable, dimension(:) :: wet1       ! top-level soil saturation(not used) [-]
   real, allocatable, dimension(:) :: garea      ! grid cell area [m2]
   real, allocatable, dimension(:) :: rb1        ! composite bulk richardson number
   real, allocatable, dimension(:) :: fm1        ! composite momemtum stability
   real, allocatable, dimension(:) :: fh1        ! composite heat/moisture stability
   real, allocatable, dimension(:) :: stress1    ! composite surface stress
   real, allocatable, dimension(:) :: fm101      ! composite 2-meter momemtum stability
   real, allocatable, dimension(:) :: fh21       ! composite 10-meter heat/moisture stability
   real, allocatable, dimension(:) :: zvfun      ! some function of vegetation used for gfs stability
   real, allocatable, dimension(:) :: rhonewsn1  ! holder for microphysics frozen density
logical, allocatable, dimension(:) :: dry        ! land flag [-]
logical, allocatable, dimension(:) :: flag_iter  ! defunct flag for surface layer iteration [-]
   real, allocatable, dimension(:) :: latitude_radians

logical :: do_mynnsfclay = .false.               ! flag for activating mynnsfclay
logical :: thsfc_loc = .true.                    ! use local theta

integer                         :: lsnowl = -2   ! lower limit for snow vector
real(kind=kind_phys), parameter :: one     = 1.0_kind_phys

associate (                                                      &
   ps         => forcing%surface_pressure                       ,&
   t1         => forcing%temperature                            ,&
   q1         => forcing%specific_humidity                      ,&
   dlwflx     => forcing%downward_longwave                      ,&
   dswsfc     => forcing%downward_shortwave                     ,&
   wind       => forcing%wind_speed                             ,&
   tprcp      => forcing%precipitation                          ,&
   im         => namelist%subset_length                         ,&
   km         => noahmp%static%soil_levels                      ,&
   delt       => noahmp%static%timestep                         ,&
   isot       => noahmp%static%soil_source                      ,&
   ivegsrc    => noahmp%static%veg_source                       ,&
   idveg      => noahmp%options%dynamic_vegetation              ,&
   iopt_crs   => noahmp%options%canopy_stomatal_resistance      ,&
   iopt_btr   => noahmp%options%soil_wetness                    ,&
   iopt_run   => noahmp%options%runoff                          ,&
   iopt_sfc   => noahmp%options%surface_exchange                ,&
   iopt_frz   => noahmp%options%supercooled_soilwater           ,&
   iopt_inf   => noahmp%options%frozen_soil_adjust              ,&
   iopt_rad   => noahmp%options%radiative_transfer              ,&
   iopt_alb   => noahmp%options%snow_albedo                     ,&
   iopt_snf   => noahmp%options%precip_partition                ,&
   iopt_tbot  => noahmp%options%soil_temp_lower_bdy             ,&
   iopt_stc   => noahmp%options%soil_temp_time_scheme           ,&
   iopt_trs   => noahmp%options%thermal_roughness_scheme        ,&
   iopt_rsf   => noahmp%options%surface_evap_resistance         ,&
   iopt_gla   => noahmp%options%glacier                         ,&
   iopt_diag  => noahmp%options%tq_diagnostic                   ,&
   soiltyp    => noahmp%static%soil_category%data               ,&
   vegtype    => noahmp%static%vegetation_category%data         ,&
   slopetyp   => noahmp%static%slope_category%data              ,&
   soilcol    => noahmp%static%soil_color_category%data         ,&
   sigmaf     => noahmp%model%vegetation_fraction%data          ,&
   emiss      => noahmp%diag%emissivity_total%data              ,&
   albdvis    => noahmp%diag%albedo_direct%data(:,1)            ,&
   albdnir    => noahmp%diag%albedo_direct%data(:,2)            ,&
   albivis    => noahmp%diag%albedo_diffuse%data(:,1)           ,&
   albinir    => noahmp%diag%albedo_diffuse%data(:,2)           ,&
   tg3        => noahmp%static%temperature_soil_bot%data        ,&
   cm         => noahmp%model%cm_noahmp%data                    ,&
   ch         => noahmp%model%ch_noahmp%data                    ,&
   shdmax     => noahmp%model%max_vegetation_frac%data          ,&
   sfalb      => noahmp%diag%albedo_total%data                  ,&
   zf         => noahmp%model%forcing_height%data               ,&
   weasd      => noahmp%state%snow_water_equiv%data             ,&
   snwdph     => noahmp%state%snow_depth%data                   ,&
   tskin      => noahmp%state%temperature_radiative%data        ,&
   canopy     => noahmp%diag%canopy_water%data                  ,&
   trans      => noahmp%flux%transpiration_heat%data            ,&
   zorl       => noahmp%diag%z0_total%data                      ,&
   ztmax      => noahmp%diag%z0h_total%data                     ,&
   ustar1     => noahmp%model%friction_velocity%data            ,&
   smc        => noahmp%state%soil_moisture_vol%data            ,&
   stc        => noahmp%state%temperature_soil%data             ,&
   slc        => noahmp%state%soil_liquid_vol%data              ,&
   qsurf      => noahmp%diag%spec_humidity_surface%data         ,&
   gflux      => noahmp%flux%ground_heat_total%data             ,&
   drain      => noahmp%flux%runoff_baseflow%data               ,&
   evap       => noahmp%flux%latent_heat_total%data             ,&
   hflx       => noahmp%flux%sensible_heat_total%data           ,&
   ep         => noahmp%diag%evaporation_potential%data         ,&
   runoff     => noahmp%flux%runoff_surface%data                ,&
   evbs       => noahmp%flux%latent_heat_ground%data            ,&
   evcw       => noahmp%flux%latent_heat_canopy%data            ,&
   sbsno      => noahmp%flux%snow_sublimation%data              ,&
   pah        => noahmp%flux%precip_adv_heat_total%data         ,&
   ecan       => noahmp%flux%evaporation_canopy%data            ,&
   etran      => noahmp%flux%transpiration%data                 ,&
   edir       => noahmp%flux%evaporation_soil%data              ,&
   snowc      => noahmp%diag%snow_cover_fraction%data           ,&
   stm        => noahmp%diag%soil_moisture_total%data           ,&
   xlatin     => noahmp%model%latitude%data                     ,&
   xcoszin    => noahmp%model%cosine_zenith%data                ,&
   iyrlen     => noahmp%model%year_length                       ,&
   julian     => noahmp%model%julian_day                        ,&
   rainn_mp   => noahmp%forcing%precip_non_convective%data      ,&
   rainc_mp   => noahmp%forcing%precip_convective%data          ,&
   snow_mp    => noahmp%forcing%precip_snow%data                ,&
   graupel_mp => noahmp%forcing%precip_graupel%data             ,&
   ice_mp     => noahmp%forcing%precip_hail%data                ,&
   snowxy     => noahmp%model%active_snow_levels%data           ,&
   tvxy       => noahmp%state%temperature_leaf%data             ,&
   tgxy       => noahmp%state%temperature_ground%data           ,&
   canicexy   => noahmp%state%canopy_ice%data                   ,&
   canliqxy   => noahmp%state%canopy_liquid%data                ,&
   eahxy      => noahmp%state%vapor_pres_canopy_air%data        ,&
   tahxy      => noahmp%state%temperature_canopy_air%data       ,&
   cmxy       => noahmp%model%cm_noahmp%data                    ,&
   chxy       => noahmp%model%ch_noahmp%data                    ,&
   fwetxy     => noahmp%diag%canopy_wet_fraction%data           ,&
   sneqvoxy   => noahmp%state%snow_water_equiv_old%data         ,&
   alboldxy   => noahmp%diag%snow_albedo_old%data               ,&
   qsnowxy    => noahmp%forcing%snowfall%data                   ,&
   wslakexy   => noahmp%state%lake_water%data                   ,&
   zwtxy      => noahmp%diag%depth_water_table%data             ,&
   waxy       => noahmp%state%aquifer_water%data                ,&
   wtxy       => noahmp%state%saturated_water%data              ,&
   tsnoxy     => noahmp%state%temperature_snow%data             ,&
   zsnsoxy    => noahmp%model%interface_depth%data              ,&
   snicexy    => noahmp%state%snow_level_ice%data               ,&
   snliqxy    => noahmp%state%snow_level_liquid%data            ,&
   lfmassxy   => noahmp%state%leaf_carbon%data                  ,&
   rtmassxy   => noahmp%state%root_carbon%data                  ,&
   stmassxy   => noahmp%state%stem_carbon%data                  ,&
   woodxy     => noahmp%state%wood_carbon%data                  ,&
   stblcpxy   => noahmp%state%soil_carbon_stable%data           ,&
   fastcpxy   => noahmp%state%soil_carbon_fast%data             ,&
   xlaixy     => noahmp%model%leaf_area_index%data              ,&
   xsaixy     => noahmp%model%stem_area_index%data              ,&
   taussxy    => noahmp%state%snow_age%data                     ,&
   smoiseq    => noahmp%state%eq_soil_water_vol%data            ,&
   smcwtdxy   => noahmp%state%soil_moisture_wtd%data            ,&
   deeprechxy => noahmp%flux%deep_recharge%data                 ,&
   rechxy     => noahmp%flux%recharge%data                      ,&
   pblh       => noahmp%model%pbl_height%data                   ,&
   rmol1      => noahmp%model%mo_length_inverse%data            ,&
   flhc1      => noahmp%model%heat_flux_multiplier%data         ,&
   flqc1      => noahmp%model%moisture_flux_multiplier%data     ,&
   t2mmp      => noahmp%diag%temperature_2m%data                ,&
   q2mp       => noahmp%diag%spec_humidity_2m%data              ,&
   canopy_heat_storage_ccpp   => noahmp%flux%canopy_heat_storage%data   ,&
   rainfall_ccpp              => noahmp%forcing%rainfall%data           ,&
   sw_absorbed_total_ccpp     => noahmp%flux%sw_absorbed_total%data     ,&
   sw_reflected_total_ccpp    => noahmp%flux%sw_reflected_total%data    ,&
   lw_absorbed_total_ccpp     => noahmp%flux%lw_absorbed_total%data     ,&
   temperature_bare_grd_ccpp  => noahmp%state%temperature_bare_grd%data ,&
   temperature_veg_grd_ccpp   => noahmp%state%temperature_veg_grd%data  ,&
   temperature_veg_2m_ccpp    => noahmp%diag%temperature_veg_2m%data    ,&
   temperature_bare_2m_ccpp   => noahmp%diag%temperature_bare_2m%data   ,&
   spec_humidity_veg_2m_ccpp  => noahmp%diag%spec_humidity_veg_2m%data  ,&
   spec_humidity_bare_2m_ccpp => noahmp%diag%spec_humidity_bare_2m%data ,&
   sw_absorbed_veg_ccpp       => noahmp%flux%sw_absorbed_veg%data       ,&
   sw_absorbed_ground_ccpp    => noahmp%flux%sw_absorbed_ground%data    ,&
   snowmelt_out_ccpp          => noahmp%flux%snowmelt_out%data          ,&
   snowmelt_shallow_ccpp      => noahmp%flux%snowmelt_shallow%data      ,&
   albedo_direct_snow_ccpp    => noahmp%diag%albedo_direct_snow%data    ,&
   albedo_diffuse_snow_ccpp   => noahmp%diag%albedo_diffuse_snow%data   ,&
   ch_vegetated_ccpp          => noahmp%model%ch_vegetated%data         ,&
   ch_bare_ground_ccpp        => noahmp%model%ch_bare_ground%data       ,&
   sensible_heat_grd_veg_ccpp => noahmp%flux%sensible_heat_grd_veg%data ,&
   sensible_heat_leaf_ccpp    => noahmp%flux%sensible_heat_leaf%data    ,&
   sensible_heat_grd_bar_ccpp => noahmp%flux%sensible_heat_grd_bar%data ,&
   latent_heat_grd_veg_ccpp   => noahmp%flux%latent_heat_grd_veg%data   ,&
   latent_heat_grd_bare_ccpp  => noahmp%flux%latent_heat_grd_bare%data  ,&
   ground_heat_veg_ccpp       => noahmp%flux%ground_heat_veg%data       ,&
   ground_heat_bare_ccpp      => noahmp%flux%ground_heat_bare%data      ,&
   lw_absorbed_grd_veg_ccpp   => noahmp%flux%lw_absorbed_grd_veg%data   ,&
   lw_absorbed_leaf_ccpp      => noahmp%flux%lw_absorbed_leaf%data      ,&
   lw_absorbed_grd_bare_ccpp  => noahmp%flux%lw_absorbed_grd_bare%data  ,&
   latent_heat_trans_ccpp     => noahmp%flux%latent_heat_trans%data     ,&
   latent_heat_leaf_ccpp      => noahmp%flux%latent_heat_leaf%data      ,&
   ch_leaf_ccpp               => noahmp%model%ch_leaf%data              ,&
   ch_below_canopy_ccpp       => noahmp%model%ch_below_canopy%data      ,&
   ch_vegetated_2m_ccpp       => noahmp%model%ch_vegetated_2m%data      ,&
   ch_bare_ground_2m_ccpp     => noahmp%model%ch_bare_ground_2m%data    ,&
   precip_adv_heat_veg_ccpp   => noahmp%flux%precip_adv_heat_veg%data   ,&
   precip_adv_heat_grd_v_ccpp => noahmp%flux%precip_adv_heat_grd_v%data ,&
   precip_adv_heat_grd_b_ccpp => noahmp%flux%precip_adv_heat_grd_b%data  & 
   )

allocate(       rho(im))
allocate(        u1(im))
allocate(        v1(im))
allocate(      snet(im))
allocate(     prsl1(im))
allocate(    srflag(im))
allocate(    prslki(im))
allocate(    prslk1(im))
allocate(    prsik1(im))
allocate(       cmm(im))
allocate(       chh(im))
allocate(    shdmin(im))
allocate(   smcref2(im))
allocate(   smcwlt2(im))
allocate(     snohf(im))
allocate(   sncovr1(im))
allocate(    snoalb(im))
allocate(     tsurf(im))
allocate(      wet1(im))
allocate(       dry(im))
allocate(flag_iter (im))
allocate(     garea(im))
allocate(       rb1(im))
allocate(       fm1(im))
allocate(       fh1(im))
allocate(   stress1(im))
allocate(     fm101(im))
allocate(      fh21(im))
allocate(     zvfun(im))
allocate( rhonewsn1(im))
allocate(latitude_radians(im))

latitude_radians  = noahmp%model%latitude%data  * 3.14159265/180.

dry        = .true.
  where(static%vegetation_category == static%iswater) dry = .false.
flag_iter  = .true.
garea      = 3000.0 * 3000.0   ! any size >= 3km will give the same answer

call set_soilveg(0,isot,ivegsrc,0,errmsg,errflg)
call gpvs()

call read_mp_table_parameters(errmsg,errflg)

zorl     = z0_data(vegtype) * 100.0   ! at driver level, roughness length in cm

time_loop : do timestep = 1, namelist%run_timesteps

  now_time = namelist%initial_time + timestep * namelist%timestep_seconds

  call date_from_since(namelist%reference_date, now_time, now_date)
  read(now_date(1:4),'(i4)') now_yyyy
  iyrlen = 365
  if(mod(now_yyyy,4) == 0) iyrlen = 366
  
  if(timestep == 1) then
    if(.not.namelist%restart_simulation) call noahmp%InitStates(namelist, now_time)

    if(namelist%output_initial .and. namelist%output_names_count > 0) &
      call output%WriteOutputNoahMP(namelist, noahmp, now_time - timestep * namelist%timestep_seconds)
  end if

  call forcing%ReadForcing(namelist, static, now_time)
   noahmp%forcing%surface_pressure_forcing%data   = forcing%surface_pressure
   noahmp%forcing%temperature_forcing%data        = forcing%temperature
   noahmp%forcing%specific_humidity_forcing%data  = forcing%specific_humidity
   noahmp%forcing%downward_longwave_forcing%data  = forcing%downward_longwave
   noahmp%forcing%downward_shortwave_forcing%data = forcing%downward_shortwave
   noahmp%forcing%wind_speed_forcing%data         = forcing%wind_speed
   noahmp%forcing%precipitation_forcing%data      = forcing%precipitation
  
  call interpolate_monthly(namelist%reference_date, now_time, im, static%gvf_monthly, sigmaf)
  call interpolate_monthly(namelist%reference_date, now_time, im, static%albedo_monthly, sfalb)
  
  call calc_cosine_zenith(namelist%reference_date, now_time, im, static%latitude, static%longitude, xcoszin, julian)
  
  u1       = wind
  v1       = 0.0_kind_phys
  snet     = dswsfc * (1.0_kind_phys - sfalb)
  srflag   = 0.0d0
    where(t1 < tfreeze) srflag = 1.d0
  prsl1    = ps * exp(-1.d0*zf/29.25d0/t1)       !  29.26 [m/K] / T [K] is the atmospheric scale height
  prslki   = (exp(zf/29.25d0/t1))**(2.d0/7.d0)  
  prslk1   = (exp(zf/29.25d0/t1))**(2.d0/7.d0)   !  assuming Exner function is approximately constant
  prsik1   = (exp(zf/29.25d0/t1))**(2.d0/7.d0)   !   for these subtleties
  
  rainn_mp = 1000.0 * tprcp / delt
  rainc_mp = 0.0
  snow_mp = 0.0
  graupel_mp = 0.0
  ice_mp = 0.0
  rhonewsn1 = huge(1.0)
  
  if(iopt_sfc == 4) do_mynnsfclay = .true.

      call noahmpdrv_run                                               &
          ( im, km, lsnowl, itime, ps, u1, v1, t1, q1, soiltyp,soilcol,&
            vegtype,sigmaf, dlwflx, dswsfc, snet, delt, tg3, cm, ch,   &
            prsl1, prslk1, prslki, prsik1, zf,pblh, dry, wind, slopetyp,    &
            shdmin, shdmax, snoalb, sfalb, flag_iter,con_g,            &
            idveg, iopt_crs, iopt_btr, iopt_run, iopt_sfc, iopt_frz,   &
            iopt_inf, iopt_rad, iopt_alb, iopt_snf, iopt_tbot,iopt_stc,&
            iopt_trs,iopt_diag,latitude_radians, xcoszin, iyrlen, julian, garea, &
            rainn_mp, rainc_mp, snow_mp, graupel_mp, ice_mp, rhonewsn1,&
            con_hvap, con_cp, con_jcal, rhoh2o, con_eps, con_epsm1,    &
            con_fvirt, con_rd, con_hfus, thsfc_loc,                    &
            weasd, snwdph, tskin, tprcp, srflag, smc, stc, slc,        &
            canopy, trans, tsurf, zorl,                                &
            rb1, fm1, fh1, ustar1, stress1, fm101, fh21,               &
            rmol1,flhc1,flqc1,do_mynnsfclay,                           &
            snowxy, tvxy, tgxy, canicexy, canliqxy, eahxy, tahxy, cmxy,&
            chxy, fwetxy, sneqvoxy, alboldxy, qsnowxy, wslakexy, zwtxy,&
            waxy, wtxy, tsnoxy, zsnsoxy, snicexy, snliqxy, lfmassxy,   &
            rtmassxy, stmassxy, woodxy, stblcpxy, fastcpxy, xlaixy,    &
            xsaixy, taussxy, smoiseq, smcwtdxy, deeprechxy, rechxy,    &
            albdvis, albdnir,  albivis,  albinir,emiss,                &
            sncovr1, qsurf, gflux, drain, evap, hflx, ep, runoff,      &
            cmm, chh, evbs, evcw, sbsno, pah, ecan, etran, edir, snowc,&
            stm, snohf,smcwlt2, smcref2, wet1, t2mmp, q2mp,zvfun,      &
            ztmax, errmsg, errflg,                                     &
            canopy_heat_storage_ccpp,                                  &
            rainfall_ccpp,                                             &
            sw_absorbed_total_ccpp,                                    &
            sw_reflected_total_ccpp,                                   &
            lw_absorbed_total_ccpp,                                    &
            temperature_bare_grd_ccpp,                                 &
            temperature_veg_grd_ccpp,                                  &
            temperature_veg_2m_ccpp,                                   &
            temperature_bare_2m_ccpp,                                  &
            spec_humidity_veg_2m_ccpp,                                 &
            spec_humidity_bare_2m_ccpp,                                &
            sw_absorbed_veg_ccpp,                                      &
            sw_absorbed_ground_ccpp,                                   &
            snowmelt_out_ccpp,                                         &
            snowmelt_shallow_ccpp,                                     &
            albedo_direct_snow_ccpp,                                   &
            albedo_diffuse_snow_ccpp,                                  &
            ch_vegetated_ccpp,                                         &
            ch_bare_ground_ccpp,                                       &
            sensible_heat_grd_veg_ccpp,                                &
            sensible_heat_leaf_ccpp,                                   &
            sensible_heat_grd_bar_ccpp,                                &
            latent_heat_grd_veg_ccpp,                                  &
            latent_heat_grd_bare_ccpp,                                 &
            ground_heat_veg_ccpp,                                      &
            ground_heat_bare_ccpp,                                     &
            lw_absorbed_grd_veg_ccpp,                                  &
            lw_absorbed_leaf_ccpp,                                     &
            lw_absorbed_grd_bare_ccpp,                                 &
            latent_heat_trans_ccpp,                                    &
            latent_heat_leaf_ccpp,                                     &
            ch_leaf_ccpp,                                              &
            ch_below_canopy_ccpp,                                      &
            ch_vegetated_2m_ccpp,                                      &
            ch_bare_ground_2m_ccpp,                                    &
            precip_adv_heat_veg_ccpp,                                  &
            precip_adv_heat_grd_v_ccpp,                                &
            precip_adv_heat_grd_b_ccpp                                )

  rho = prsl1 / (con_rd*t1*(one+con_fvirt*q1)) 
  hflx = hflx * rho * con_cp
  evap = evap * rho * con_hvap
  
  where(dswsfc>0.0 .and. sfalb<0.0) dswsfc = 0.0

!!! Output section !!!

  if(namelist%output_names_count > 0) then

    output_cases : select case(namelist%output_frequency_s)
  
      case( 1 : )  ! output based on number of timesteps

        if(mod(timestep,namelist%output_timesteps) == 0) &
          call output%WriteOutputNoahMP(namelist, noahmp, now_time)

      case( -1 )  ! output daily at 00Z

        if(now_date(12:19) == "00:00:00") &
          call output%WriteOutputNoahMP(namelist, noahmp, now_time)
      
      case( -2 )  ! output monthly at 00Z on 1st of month

        if(now_date( 9:19) == "01 00:00:00") &
          call output%WriteOutputNoahMP(namelist, noahmp, now_time)
      
    end select output_cases

  end if ! namelist%output_names_count > 0

!!! Daily mean section !!!

  if(namelist%daily_mean_names_count > 0) then

    call output%WriteDailyMeanNoahMP(namelist, noahmp, now_time)
      
  end if ! namelist%daily_mean_names_count > 0

!!! Monthly mean section !!!

  if(namelist%monthly_mean_names_count > 0) then

    call output%WriteMonthlyMeanNoahMP(namelist, noahmp, now_time)
      
  end if ! namelist%monthly_mean_names_count > 0

!!! Solar noon section !!!

  if(namelist%solar_noon_names_count > 0) then

    call output%WriteSolarNoonNoahMP(namelist, noahmp, now_time)
      
  end if ! namelist%solar_noon_names_count > 0

!!! Restart section !!!

  restart_cases : select case(namelist%restart_frequency_s)
  
    case( 1 : )  ! restart based on number of timesteps

      if(mod(timestep,namelist%restart_timesteps) == 0) &
        call restart%WriteRestartNoahMP(namelist, noahmp, now_time)

    case( -1 )  ! restart daily at 00Z

      if(now_date(12:19) == "00:00:00") &
        call restart%WriteRestartNoahMP(namelist, noahmp, now_time)

    case( -2 )  ! restart monthly at 00Z on 1st of month

      if(now_date( 9:19) == "01 00:00:00") &
        call restart%WriteRestartNoahMP(namelist, noahmp, now_time)

  end select restart_cases

  if(errflg /= 0) then
    write(*,*) "noahmpdrv_run reporting an error"
    write(*,*) errmsg
    stop
  end if

end do time_loop

end associate

end subroutine ufsLandNoahMPDriverRun 

subroutine ufsLandNoahMPDriverFinalize()
end subroutine ufsLandNoahMPDriverFinalize

end module ufsLandNoahMPDriverModule
