module ufsLandNoahMPRestartModule

  implicit none
  save
  private

  type, public :: noahmp_restart_type

    character*256    :: filename

  contains

    procedure, public  :: WriteRestartNoahMP
    procedure, public  :: ReadRestartNoahMP

end type noahmp_restart_type
     
contains   

  subroutine WriteRestartNoahMP(this, namelist, noahmp, now_time)
  
  use netcdf
  use time_utilities
  use error_handling, only : handle_err
  use NamelistRead
  use ufsLandNoahMPType

  class(noahmp_restart_type)   :: this  
  type(namelist_type)  :: namelist
  type(noahmp_type)    :: noahmp
  double precision     :: now_time
  character*19     :: nowdate    ! current date
  character*19     :: reference_date 
  integer          :: yyyy,mm,dd,hh,nn,ss
  integer :: ncid, dimid, varid, status
  integer :: dim_id_time, dim_id_loc, dim_id_soil, dim_id_snow, dim_id_snso, dim_id_date
  integer :: outsub
  
  reference_date = namelist%reference_date

  outsub = namelist%begsub - namelist%begloc + 1
  
  call date_from_since(reference_date, now_time, nowdate)
  read(nowdate( 1: 4),'(i4.4)') yyyy
  read(nowdate( 6: 7),'(i2.2)') mm
  read(nowdate( 9:10),'(i2.2)') dd
  read(nowdate(12:13),'(i2.2)') hh
  read(nowdate(15:16),'(i2.2)') nn
  read(nowdate(18:19),'(i2.2)') ss

  write(this%filename,'(a17,i4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a3)') &
    "ufs_land_restart.", yyyy, "-", mm, "-", dd, "_", hh, "-", nn, "-", ss, ".nc"

  this%filename = trim(namelist%restart_dir)//"/"//trim(this%filename)

  write(*,*) "Creating: "//trim(this%filename)

  status = nf90_create(this%filename, NF90_CLOBBER, ncid)
    if (status /= nf90_noerr) call handle_err(status)

! Define dimensions in the file.

  status = nf90_def_dim(ncid, "location"    , noahmp%static%im     , dim_id_loc)
    if (status /= nf90_noerr) call handle_err(status)
  status = nf90_def_dim(ncid, "soil_levels" , noahmp%static%km     , dim_id_soil)
    if (status /= nf90_noerr) call handle_err(status)
  status = nf90_def_dim(ncid, "snow_levels" , 3                    , dim_id_snow)
    if (status /= nf90_noerr) call handle_err(status)
  status = nf90_def_dim(ncid, "snso_levels" , noahmp%static%km + 3 , dim_id_snso)
    if (status /= nf90_noerr) call handle_err(status)
  status = nf90_def_dim(ncid, "time"        , NF90_UNLIMITED       , dim_id_time)
    if (status /= nf90_noerr) call handle_err(status)
  
! Define variables in the file.

  status = nf90_def_var(ncid, "time", NF90_DOUBLE, dim_id_time, varid)
    status = nf90_put_att(ncid, varid, "long_name", "time")
    status = nf90_put_att(ncid, varid, "units", "seconds since "//reference_date)

  status = nf90_def_var(ncid, "delt", NF90_DOUBLE, (/dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "time step")
    status = nf90_put_att(ncid, varid, "units", "seconds")

  status = nf90_def_var(ncid, "sigmaf", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "green vegetation fraction")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "emiss", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface emissivity")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "albdvis", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface albedo - direct visible")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "albdnir", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface albedo - direct NIR")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "albivis", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface albedo - diffuse visible")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "albinir", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface albedo - diffuse NIR")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "snet", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "forcing net shortwave flux")
    status = nf90_put_att(ncid, varid, "units", "W/m2")

  status = nf90_def_var(ncid, "tg3", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "deep soil temperature")
    status = nf90_put_att(ncid, varid, "units", "K")

  status = nf90_def_var(ncid, "cm", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface exchange coeff for momentum")
    status = nf90_put_att(ncid, varid, "units", "m/s")

  status = nf90_def_var(ncid, "ch", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface exchange coeff heat & moisture")
    status = nf90_put_att(ncid, varid, "units", "m/s")

  status = nf90_def_var(ncid, "prsl1", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "sfc layer 1 mean pressure")
    status = nf90_put_att(ncid, varid, "units", "Pa")

  status = nf90_def_var(ncid, "prslki", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "Exner function from layer 1 to sfc")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "zf", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "height of bottom layer")
    status = nf90_put_att(ncid, varid, "units", "m")

  status = nf90_def_var(ncid, "shdmin", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "min fractional coverage of green veg")
    status = nf90_put_att(ncid, varid, "units", "fraction")

  status = nf90_def_var(ncid, "shdmax", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "max fractional coverage of green veg")
    status = nf90_put_att(ncid, varid, "units", "fraction")

  status = nf90_def_var(ncid, "snoalb", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "upper bound on max albedo over deep snow")
    status = nf90_put_att(ncid, varid, "units", "fraction")

  status = nf90_def_var(ncid, "sfalb", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "mean sfc diffuse sw albedo")
    status = nf90_put_att(ncid, varid, "units", "fraction")

  status = nf90_def_var(ncid, "weasd", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "water equivalent accumulated snow depth")
    status = nf90_put_att(ncid, varid, "units", "mm")

  status = nf90_def_var(ncid, "snwdph", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "snow depth (water equiv) over land")
    status = nf90_put_att(ncid, varid, "units", "m")

  status = nf90_def_var(ncid, "tskin", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "ground surface skin temperature")
    status = nf90_put_att(ncid, varid, "units", "K")

  status = nf90_def_var(ncid, "srflag", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "snow/rain flag for precipitation")
    status = nf90_put_att(ncid, varid, "units", "fraction")

  status = nf90_def_var(ncid, "smc", NF90_DOUBLE, (/dim_id_loc,dim_id_soil,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "total soil moisture content")
    status = nf90_put_att(ncid, varid, "units", "m3/m3")

  status = nf90_def_var(ncid, "stc", NF90_DOUBLE, (/dim_id_loc,dim_id_soil,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "soil temperature")
    status = nf90_put_att(ncid, varid, "units", "K")

  status = nf90_def_var(ncid, "slc", NF90_DOUBLE, (/dim_id_loc,dim_id_soil,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "liquid soil moisture")
    status = nf90_put_att(ncid, varid, "units", "m3/m3")

  status = nf90_def_var(ncid, "canopy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "canopy moisture content")
    status = nf90_put_att(ncid, varid, "units", "m")

  status = nf90_def_var(ncid, "trans", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "total plant transpiration")
    status = nf90_put_att(ncid, varid, "units", "m/s")

  status = nf90_def_var(ncid, "tsurf", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface skin temperature (after iteration)")
    status = nf90_put_att(ncid, varid, "units", "K")

  status = nf90_def_var(ncid, "zorl", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface roughness")
    status = nf90_put_att(ncid, varid, "units", "m")

  status = nf90_def_var(ncid, "sncovr1", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "snow cover over land")
    status = nf90_put_att(ncid, varid, "units", "fraction")

  status = nf90_def_var(ncid, "qsurf", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "specific humidity at sfc")
    status = nf90_put_att(ncid, varid, "units", "kg/kg")

  status = nf90_def_var(ncid, "gflux", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "soil heat flux")
    status = nf90_put_att(ncid, varid, "units", "W/m2")

  status = nf90_def_var(ncid, "drain", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "subsurface runoff")
    status = nf90_put_att(ncid, varid, "units", "mm/s")

  status = nf90_def_var(ncid, "evap", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "evaporation from latent heat flux")
    status = nf90_put_att(ncid, varid, "units", "mm/s")

  status = nf90_def_var(ncid, "hflx", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
  status = nf90_put_att(ncid, varid, "long_name", "sensible heat flux")
    status = nf90_put_att(ncid, varid, "units", "W/m2")

  status = nf90_def_var(ncid, "ep", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "potential evaporation")
    status = nf90_put_att(ncid, varid, "units", "?")

  status = nf90_def_var(ncid, "runoff", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "surface runoff")
    status = nf90_put_att(ncid, varid, "units", "m/s")

  status = nf90_def_var(ncid, "cmm", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "ch * rho")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "chh", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "m/s")

  status = nf90_def_var(ncid, "evbs", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "direct soil evaporation")
    status = nf90_put_att(ncid, varid, "units", "m/s")

  status = nf90_def_var(ncid, "evcw", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "canopy water evaporation")
    status = nf90_put_att(ncid, varid, "units", "m/s")

  status = nf90_def_var(ncid, "sbsno", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "sublimation/deposit from snopack")
    status = nf90_put_att(ncid, varid, "units", "m/s")

  status = nf90_def_var(ncid, "snowc", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "fractional snow cover")
    status = nf90_put_att(ncid, varid, "units", "fraction")

  status = nf90_def_var(ncid, "stm", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "total soil column moisture content")
    status = nf90_put_att(ncid, varid, "units", "m")

  status = nf90_def_var(ncid, "snohf", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "snow/freezing-rain latent heat flux ")
    status = nf90_put_att(ncid, varid, "units", "W/m2")

  status = nf90_def_var(ncid, "smcwlt2", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "dry soil moisture threshold")
    status = nf90_put_att(ncid, varid, "units", "m3/m3")

  status = nf90_def_var(ncid, "smcref2", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "soil moisture threshold")
    status = nf90_put_att(ncid, varid, "units", "m3/m3")

  status = nf90_def_var(ncid, "wet1", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "normalized soil wetness")
    status = nf90_put_att(ncid, varid, "units", "-")

  status = nf90_def_var(ncid, "xcoszin", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "snowxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "tvxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "tgxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "canicexy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "canliqxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "eahxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "tahxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "cmxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "chxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "fwetxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "sneqvoxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "alboldxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "qsnowxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "wslakexy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "zwtxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "waxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "wtxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "lfmassxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "rtmassxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "stmassxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "woodxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "stblcpxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "fastcpxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "xlaixy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "xsaixy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "taussxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "smcwtdxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "deeprechxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "rechxy", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "t2mmp", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "q2mp", NF90_DOUBLE, (/dim_id_loc,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "smoiseq", NF90_DOUBLE, (/dim_id_loc,dim_id_soil,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "tsnoxy", NF90_DOUBLE, (/dim_id_loc,dim_id_snow,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "zsnsoxy", NF90_DOUBLE, (/dim_id_loc,dim_id_snso,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "snicexy", NF90_DOUBLE, (/dim_id_loc,dim_id_snow,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_def_var(ncid, "snliqxy", NF90_DOUBLE, (/dim_id_loc,dim_id_snow,dim_id_time/), varid)
    status = nf90_put_att(ncid, varid, "long_name", "")
    status = nf90_put_att(ncid, varid, "units", "")

  status = nf90_enddef(ncid)

! Start writing restart file
  
  status = nf90_inq_varid(ncid, "time", varid)
  status = nf90_put_var(ncid, varid , now_time             )
  
  status = nf90_inq_varid(ncid, "delt", varid)
  status = nf90_put_var(ncid, varid , noahmp%static%delt   )

  status = nf90_inq_varid(ncid, "sigmaf", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%sigmaf        , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "emiss", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%emiss         , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "albdvis", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%albdvis       , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "albdnir", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%albdnir       , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "albivis", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%albivis       , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "albinir", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%albinir       , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "snet", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%snet          , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "tg3", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%tg3           , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "cm", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%cm            , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "ch", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%ch            , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "prsl1", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%prsl1         , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "prslki", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%prslki        , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "zf", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%zf            , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "shdmin", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%shdmin        , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "shdmax", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%shdmax        , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "snoalb", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%snoalb        , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "sfalb", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%sfalb         , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "weasd", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%weasd         , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "snwdph", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%snwdph        , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "tskin", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%tskin         , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "srflag", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%srflag        , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "smc", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%smc , &
      start = (/          outsub,                1, 1/), &
      count = (/noahmp%static%im, noahmp%static%km, 1/))

  status = nf90_inq_varid(ncid, "stc", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%stc , &
      start = (/          outsub,                1, 1/), &
      count = (/noahmp%static%im, noahmp%static%km, 1/))

  status = nf90_inq_varid(ncid, "slc", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%slc , &
      start = (/          outsub,                1, 1/), &
      count = (/noahmp%static%im, noahmp%static%km, 1/))

  status = nf90_inq_varid(ncid, "canopy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%canopy        , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "trans", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%trans         , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "tsurf", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%tsurf         , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "zorl", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%zorl          , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "sncovr1", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%sncovr1       , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "qsurf", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%qsurf         , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "gflux", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%gflux         , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "drain", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%drain         , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "evap", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%evap          , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "hflx", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%hflx          , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "ep", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%ep            , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "runoff", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%runoff        , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "cmm", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%cmm           , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "chh", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%chh           , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "evbs", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%evbs          , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "evcw", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%evcw          , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "sbsno", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%sbsno         , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "snowc", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%snowc         , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "stm", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%stm           , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "snohf", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%snohf         , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "smcwlt2", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%smcwlt2       , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "smcref2", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%smcref2       , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "wet1", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%wet1          , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "xcoszin", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%xcoszin     , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "snowxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%snowxy      , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "tvxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%tvxy        , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "tgxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%tgxy        , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "canicexy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%canicexy    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "canliqxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%canliqxy    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "eahxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%eahxy       , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "tahxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%tahxy       , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "cmxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%cmxy        , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "chxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%chxy        , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "fwetxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%fwetxy      , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "sneqvoxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%sneqvoxy    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "alboldxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%alboldxy    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "qsnowxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%qsnowxy     , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "wslakexy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%wslakexy    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "zwtxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%zwtxy       , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "waxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%waxy        , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "wtxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%wtxy        , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "lfmassxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%lfmassxy    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "rtmassxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%rtmassxy    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "stmassxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%stmassxy    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "woodxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%woodxy      , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "stblcpxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%stblcpxy    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "fastcpxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%fastcpxy    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "xlaixy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%xlaixy      , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "xsaixy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%xsaixy      , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "taussxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%taussxy     , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "smcwtdxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%smcwtdxy    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "deeprechxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model% deeprechxy , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "rechxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%rechxy      , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "t2mmp", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%t2mmp       , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "q2mp", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%q2mp        , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "smoiseq", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%smoiseq , &
      start = (/outsub          ,                1, 1/), &
      count = (/noahmp%static%im, noahmp%static%km, 1/))

  status = nf90_inq_varid(ncid, "zsnsoxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%zsnsoxy , &
      start = (/outsub          ,                  1, 1/), &
      count = (/noahmp%static%im, noahmp%static%km+3, 1/))

  status = nf90_inq_varid(ncid, "tsnoxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%tsnoxy , &
      start = (/outsub          ,              1, 1/), &
      count = (/noahmp%static%im,              3, 1/))

  status = nf90_inq_varid(ncid, "snicexy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%snicexy , &
      start = (/outsub          ,              1, 1/), &
      count = (/noahmp%static%im,              3, 1/))

  status = nf90_inq_varid(ncid, "snliqxy", varid)
  status = nf90_put_var(ncid, varid , noahmp%model%snliqxy , &
      start = (/outsub          ,              1, 1/), &
      count = (/noahmp%static%im,              3, 1/))

  status = nf90_close(ncid)

  end subroutine WriteRestartNoahMP
  
  subroutine ReadRestartNoahMP(this, namelist, noahmp)
  
  use netcdf
  use error_handling, only : handle_err
  use time_utilities
  use NamelistRead
  use ufsLandNoahMPType
  
  class(noahmp_restart_type)  :: this
  type(namelist_type)  :: namelist
  type (noahmp_type)   :: noahmp
  double precision     :: now_time
  
  character*19     :: reference_date 
  integer          :: yyyy,mm,dd,hh,nn,ss
  integer :: ncid, dimid, varid, status
  integer :: dim_id_time, dim_id_loc, dim_id_soil, dim_id_snow, dim_id_snso, dim_id_date
  integer :: outsub
  
  reference_date = namelist%reference_date
  
  call calc_sec_since(reference_date,namelist%restart_date,0,now_time)

  namelist%initial_time = now_time

  read(namelist%restart_date( 1: 4),'(i4.4)') yyyy
  read(namelist%restart_date( 6: 7),'(i2.2)') mm
  read(namelist%restart_date( 9:10),'(i2.2)') dd
  read(namelist%restart_date(12:13),'(i2.2)') hh
  read(namelist%restart_date(15:16),'(i2.2)') nn
  read(namelist%restart_date(18:19),'(i2.2)') ss

  write(this%filename,'(a17,i4,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a1,i2.2,a3)') &
    "ufs_land_restart.", yyyy, "-", mm, "-", dd, "_", hh, "-", nn, "-", ss, ".nc"

  this%filename = trim(namelist%restart_dir)//"/"//trim(this%filename)

  write(*,*) "Reading: "//trim(this%filename)
    
  outsub = namelist%begsub - namelist%begloc + 1
  
  status = nf90_open(this%filename, NF90_NOWRITE, ncid)
   if (status /= nf90_noerr) call handle_err(status)
  
  status = nf90_inq_varid(ncid, "time", varid)
  status = nf90_get_var(ncid, varid , now_time)
  
  status = nf90_inq_varid(ncid, "sigmaf", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%sigmaf  , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "emiss", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%emiss   , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "albdvis", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%albdvis , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "albdnir", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%albdnir , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "albivis", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%albivis , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "albinir", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%albinir , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "snet", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%snet    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "tg3", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%tg3     , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "cm", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%cm      , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "ch", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%ch      , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "prsl1", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%prsl1   , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "prslki", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%prslki  , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "zf", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%zf      , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "shdmin", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%shdmin  , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "shdmax", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%shdmax  , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "snoalb", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%snoalb  , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "sfalb", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%sfalb   , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "weasd", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%weasd   , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "snwdph", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%snwdph  , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "tskin", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%tskin   , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "srflag", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%srflag  , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "smc", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%smc     , &
      start = (/          outsub,                1, 1/)    , &
      count = (/noahmp%static%im, noahmp%static%km, 1/))

  status = nf90_inq_varid(ncid, "stc", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%stc     , &
      start = (/          outsub,                1, 1/)    , &
      count = (/noahmp%static%im, noahmp%static%km, 1/))

  status = nf90_inq_varid(ncid, "slc", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%slc     , &
      start = (/          outsub,                1, 1/)    , &
      count = (/noahmp%static%im, noahmp%static%km, 1/))

  status = nf90_inq_varid(ncid, "canopy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%canopy  , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "trans", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%trans   , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "tsurf", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%tsurf   , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "zorl", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%zorl    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "sncovr1", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%sncovr1 , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "qsurf", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%qsurf   , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "gflux", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%gflux   , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "drain", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%drain   , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "evap", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%evap    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "hflx", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%hflx    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "ep", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%ep      , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "runoff", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%runoff  , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "cmm", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%cmm     , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "chh", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%chh     , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "evbs", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%evbs    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "evcw", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%evcw    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "sbsno", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%sbsno   , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "snowc", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%snowc   , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "stm", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%stm     , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "snohf", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%snohf   , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "smcwlt2", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%smcwlt2 , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "smcref2", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%smcref2 , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "wet1", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%wet1    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "xcoszin", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%xcoszin , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "snowxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%snowxy  , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "tvxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%tvxy    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "tgxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%tgxy    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "canicexy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%canicexy, &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "canliqxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%canliqxy, &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "eahxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%eahxy   , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "tahxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%tahxy   , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "cmxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%cmxy    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "chxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%chxy    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "fwetxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%fwetxy  , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "sneqvoxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%sneqvoxy, &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "alboldxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%alboldxy, &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "qsnowxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%qsnowxy , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "wslakexy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%wslakexy, &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "zwtxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%zwtxy   , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "waxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%waxy    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "wtxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%wtxy    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "lfmassxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%lfmassxy, &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "rtmassxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%rtmassxy, &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "stmassxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%stmassxy, &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "woodxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%woodxy  , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "stblcpxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%stblcpxy, &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "fastcpxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%fastcpxy, &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "xlaixy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%xlaixy  , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "xsaixy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%xsaixy  , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "taussxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%taussxy , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "smcwtdxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%smcwtdxy, &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "deeprechxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model% deeprechxy, &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "rechxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%rechxy  , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "t2mmp", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%t2mmp   , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "q2mp", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%q2mp    , &
      start = (/outsub,1/), count = (/noahmp%static%im, 1/))

  status = nf90_inq_varid(ncid, "smoiseq", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%smoiseq , &
      start = (/outsub          ,                1, 1/)    , &
      count = (/noahmp%static%im, noahmp%static%km, 1/))

  status = nf90_inq_varid(ncid, "tsnoxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%tsnoxy  , &
      start = (/outsub          ,              1, 1/)      , &
      count = (/noahmp%static%im,              3, 1/))

  status = nf90_inq_varid(ncid, "zsnsoxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%zsnsoxy , &
      start = (/outsub          ,                  1, 1/)  , &
      count = (/noahmp%static%im, noahmp%static%km+3, 1/))

  status = nf90_inq_varid(ncid, "snicexy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%snicexy , &
      start = (/outsub          ,              1, 1/)      , &
      count = (/noahmp%static%im,              3, 1/))

  status = nf90_inq_varid(ncid, "snliqxy", varid)
  status = nf90_get_var(ncid, varid , noahmp%model%snliqxy , &
      start = (/outsub          ,              1, 1/)      , &
      count = (/noahmp%static%im,              3, 1/))
 
  status = nf90_close(ncid)

  end subroutine ReadRestartNoahMP

end module ufsLandNoahMPRestartModule
