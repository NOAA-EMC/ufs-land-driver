module ufsLandNoahMPDriverModule

implicit none

contains

subroutine ufsLandNoahMPDriverInit(namelist, static, forcing, noahmp)

  use NamelistRead
  use ufsLandNoahMPType
  use ufsLandStaticModule
  use ufsLandInitialModule
  use ufsLandForcingModule

  implicit none

  type (namelist_type)     :: namelist
  type (noahmp_type)       :: noahmp
  type (static_type)       :: static
  type (initial_type)      :: initial
  type (forcing_type)      :: forcing

  call static%ReadStatic(namelist)
  
  call noahmp%Init(namelist,static%nlocations)

  call initial%ReadInitial(namelist)
  
  call initial%TransferInitialNoahMP(noahmp)
  
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
use physcons, only : con_hvap , con_cp, con_jcal, con_eps, con_epsm1,    &
                     con_fvirt, con_rd, con_hfus,                        &
		     tfreeze=> con_t0c, rhoh2o => rhowater

use interpolation_utilities
use time_utilities
use cosine_zenith
use NamelistRead, only         : namelist_type
use ufsLandNoahMPType, only    : noahmp_type
use ufsLandStaticModule, only  : static_type
use ufsLandForcingModule
use ufsLandIOModule

type (namelist_type)  :: namelist
type (noahmp_type)    :: noahmp
type (forcing_type)   :: forcing
type (static_type)    :: static
type (output_type)    :: output

integer          :: timestep
double precision :: now_time
character*19     :: now_date  ! format: yyyy-mm-dd hh:nn:ss
integer          :: now_yyyy

real, allocatable, dimension(:) :: rho
real(kind=kind_phys), parameter :: one     = 1.0_kind_phys

associate (                               &
   ps         => forcing%surface_pressure    ,&
   t1         => forcing%temperature         ,&
   q1         => forcing%specific_humidity   ,&
   dlwflx     => forcing%downward_longwave   ,&
   dswsfc     => forcing%downward_shortwave  ,&
   wind       => forcing%wind_speed          ,&
   tprcp      => forcing%precipitation       ,&
   im         => noahmp%static%im        ,&
   km         => noahmp%static%km        ,&
   itime      => noahmp%static%itime     ,&
   delt       => noahmp%static%delt      ,&
   isot       => noahmp%static%isot      ,&
   ivegsrc    => noahmp%static%ivegsrc   ,&
   idveg      => noahmp%static%idveg     ,&
   iopt_crs   => noahmp%static%iopt_crs  ,&
   iopt_btr   => noahmp%static%iopt_btr  ,&
   iopt_run   => noahmp%static%iopt_run  ,&
   iopt_sfc   => noahmp%static%iopt_sfc  ,&
   iopt_frz   => noahmp%static%iopt_frz  ,&
   iopt_inf   => noahmp%static%iopt_inf  ,&
   iopt_rad   => noahmp%static%iopt_rad  ,&
   iopt_alb   => noahmp%static%iopt_alb  ,&
   iopt_snf   => noahmp%static%iopt_snf  ,&
   iopt_tbot  => noahmp%static%iopt_tbot ,&
   iopt_stc   => noahmp%static%iopt_stc  ,&
   iopt_rsf   => noahmp%static%iopt_rsf  ,&
   iopt_gla   => noahmp%static%iopt_gla  ,&
   errmsg     => noahmp%static%errmsg    ,& 
   errflg     => noahmp%static%errflg    ,& 
   soiltyp    => noahmp%model%soiltyp    ,&
   vegtype    => noahmp%model%vegtype    ,&
   slopetyp   => noahmp%model%slopetyp   ,&
   u1         => noahmp%model%u1         ,&
   v1         => noahmp%model%v1         ,&
   sigmaf     => noahmp%model%sigmaf     ,&
   emiss      => noahmp%model%emiss      ,&
   albdvis    => noahmp%model%albdvis    ,&
   albdnir    => noahmp%model%albdnir    ,&
   albivis    => noahmp%model%albivis    ,&
   albinir    => noahmp%model%albinir    ,&
   snet       => noahmp%model%snet       ,&
   tg3        => noahmp%model%tg3        ,&
   cm         => noahmp%model%cm         ,&
   ch         => noahmp%model%ch         ,&
   prsl1      => noahmp%model%prsl1      ,&
   shdmin     => noahmp%model%shdmin     ,&
   shdmax     => noahmp%model%shdmax     ,&
   snoalb     => noahmp%model%snoalb     ,&
   sfalb      => noahmp%model%sfalb      ,&
   zf         => noahmp%model%zf         ,&
   flag_iter  => noahmp%model%flag_iter  ,&
   flag_guess => noahmp%model%flag_guess ,&
   dry        => noahmp%model%dry        ,&
   prslki     => noahmp%model%prslki     ,&
   weasd      => noahmp%model%weasd      ,&
   snwdph     => noahmp%model%snwdph     ,&
   tskin      => noahmp%model%tskin      ,&
   srflag     => noahmp%model%srflag     ,&
   canopy     => noahmp%model%canopy     ,&
   trans      => noahmp%model%trans      ,&
   tsurf      => noahmp%model%tsurf      ,&
   zorl       => noahmp%model%zorl       ,&
   smc        => noahmp%model%smc        ,&
   stc        => noahmp%model%stc        ,&
   slc        => noahmp%model%slc        ,&
   sncovr1    => noahmp%model%sncovr1    ,&
   qsurf      => noahmp%model%qsurf      ,&
   gflux      => noahmp%model%gflux      ,&
   drain      => noahmp%model%drain      ,&
   evap       => noahmp%model%evap       ,&
   hflx       => noahmp%model%hflx       ,&
   ep         => noahmp%model%ep         ,&
   runoff     => noahmp%model%runoff     ,&
   cmm        => noahmp%model%cmm        ,&
   chh        => noahmp%model%chh        ,&
   evbs       => noahmp%model%evbs       ,&
   evcw       => noahmp%model%evcw       ,&
   sbsno      => noahmp%model%sbsno      ,&
   snowc      => noahmp%model%snowc      ,&
   stm        => noahmp%model%stm        ,&
   snohf      => noahmp%model%snohf      ,&
   smcwlt2    => noahmp%model%smcwlt2    ,&
   smcref2    => noahmp%model%smcref2    ,&
   wet1       => noahmp%model%wet1       ,&
   xlatin     => noahmp%model%xlatin     ,&
   xcoszin    => noahmp%model%xcoszin    ,&
   iyrlen     => noahmp%model%iyrlen     ,&
   julian     => noahmp%model%julian     ,&
   rainn_mp   => noahmp%model%rainn_mp   ,&
   rainc_mp   => noahmp%model%rainc_mp   ,&
   snow_mp    => noahmp%model%snow_mp    ,&
   graupel_mp => noahmp%model%graupel_mp ,&
   ice_mp     => noahmp%model%ice_mp     ,&
   snowxy     => noahmp%model%snowxy     ,&
   tvxy       => noahmp%model%tvxy       ,&
   tgxy       => noahmp%model%tgxy       ,&
   canicexy   => noahmp%model%canicexy   ,&
   canliqxy   => noahmp%model%canliqxy   ,&
   eahxy      => noahmp%model%eahxy      ,&
   tahxy      => noahmp%model%tahxy      ,&
   cmxy       => noahmp%model%cmxy       ,&
   chxy       => noahmp%model%chxy       ,&
   fwetxy     => noahmp%model%fwetxy     ,&
   sneqvoxy   => noahmp%model%sneqvoxy   ,&
   alboldxy   => noahmp%model%alboldxy   ,&
   qsnowxy    => noahmp%model%qsnowxy    ,&
   wslakexy   => noahmp%model%wslakexy   ,&
   zwtxy      => noahmp%model%zwtxy      ,&
   waxy       => noahmp%model%waxy       ,&
   wtxy       => noahmp%model%wtxy       ,&
   tsnoxy     => noahmp%model%tsnoxy     ,&
   zsnsoxy    => noahmp%model%zsnsoxy    ,&
   snicexy    => noahmp%model%snicexy    ,&
   snliqxy    => noahmp%model%snliqxy    ,&
   lfmassxy   => noahmp%model%lfmassxy   ,&
   rtmassxy   => noahmp%model%rtmassxy   ,&
   stmassxy   => noahmp%model%stmassxy   ,&
   woodxy     => noahmp%model%woodxy     ,&
   stblcpxy   => noahmp%model%stblcpxy   ,&
   fastcpxy   => noahmp%model%fastcpxy   ,&
   xlaixy     => noahmp%model%xlaixy     ,&
   xsaixy     => noahmp%model%xsaixy     ,&
   taussxy    => noahmp%model%taussxy    ,&
   smoiseq    => noahmp%model%smoiseq    ,&
   smcwtdxy   => noahmp%model%smcwtdxy   ,&
   deeprechxy => noahmp%model%deeprechxy ,&
   rechxy     => noahmp%model%rechxy     ,&
   t2mmp      => noahmp%model%t2mmp      ,&
   q2mp       => noahmp%model%q2mp        &
   )

allocate(   rho(im))

flag_iter  = .true.
flag_guess = .false.

call set_soilveg(0,isot,ivegsrc,0)
call gpvs()

zorl     = z0_data(vegtype) * 100.0   ! at driver level, roughness length in cm

time_loop : do timestep = 1, namelist%run_timesteps

  now_time = namelist%initial_time + timestep * namelist%timestep_seconds

  call date_from_since("1970-01-01 00:00:00", now_time, now_date)
  read(now_date(1:4),'(i4)') now_yyyy
  iyrlen = 365
  if(mod(now_yyyy,4) == 0) iyrlen = 366
  
  if(timestep == 1) call noahmp%InitStates(namelist, now_time)

  call forcing%ReadForcing(namelist, now_time)
  
  call interpolate_monthly(now_time, im, static%gvf_monthly, sigmaf)
  call interpolate_monthly(now_time, im, static%albedo_monthly, sfalb)
  
  call calc_cosine_zenith(now_time, im, static%latitude, static%longitude, xcoszin, julian)
  
  u1 = wind
  v1 = 0.0_kind_phys
  snet   = dswsfc * (1.0_kind_phys - sfalb)
  prsl1  = ps * exp(-1.d0*zf/29.25d0/t1)
  srflag = 0.0d0
  where(t1 < tfreeze) srflag = 1.d0
  prslki = (exp(zf/29.25d0/t1))**(2.d0/7.d0)
  
  rainn_mp = 1000.0 * tprcp / delt
  rainc_mp = 0.0
  snow_mp = 0.0
  graupel_mp = 0.0
  ice_mp = 0.0
  
      call noahmpdrv_run                                               &
          ( im, km, itime, ps, u1, v1, t1, q1, soiltyp, vegtype,       &  !  ---  inputs
            sigmaf, dlwflx, dswsfc, snet, delt, tg3, cm, ch,           &
            prsl1, prslki, zf, dry, wind, slopetyp,                    &
            shdmin, shdmax, snoalb, sfalb, flag_iter, flag_guess,      &
            idveg, iopt_crs, iopt_btr, iopt_run, iopt_sfc, iopt_frz,   &
            iopt_inf, iopt_rad, iopt_alb, iopt_snf, iopt_tbot,         &
            iopt_stc, xlatin, xcoszin, iyrlen, julian,                 &
            rainn_mp, rainc_mp, snow_mp, graupel_mp, ice_mp,           &
            con_hvap, con_cp, con_jcal, rhoh2o, con_eps, con_epsm1,    &
            con_fvirt, con_rd, con_hfus,                               &
            weasd, snwdph, tskin, tprcp, srflag, smc, stc, slc,        &  !  ---  in/outs:
            canopy, trans, tsurf, zorl,                                &
            snowxy, tvxy, tgxy, canicexy, canliqxy, eahxy, tahxy, cmxy,&  ! --- Noah MP specific
            chxy, fwetxy, sneqvoxy, alboldxy, qsnowxy, wslakexy, zwtxy,&
            waxy, wtxy, tsnoxy, zsnsoxy, snicexy, snliqxy, lfmassxy,   &
            rtmassxy, stmassxy, woodxy, stblcpxy, fastcpxy, xlaixy,    &
            xsaixy, taussxy, smoiseq, smcwtdxy, deeprechxy, rechxy,    &
            albdvis, albdnir,  albivis,  albinir,emiss,                &
            sncovr1, qsurf, gflux, drain, evap, hflx, ep, runoff,      &  !  ---  outputs:
            cmm, chh, evbs, evcw, sbsno, snowc, stm, snohf,            &
            smcwlt2, smcref2, wet1, t2mmp, q2mp, errmsg, errflg)     

  rho = prsl1 / (con_rd*t1*(one+con_fvirt*q1)) 
  hflx = hflx * rho * con_cp
  evap = evap * rho * con_hvap
  
  where(dswsfc>0.0 .and. sfalb<0.0) dswsfc = 0.0

  call output%WriteOutputNoahMP(namelist, noahmp, forcing, now_time)

end do time_loop

end associate

end subroutine ufsLandNoahMPDriverRun 

subroutine ufsLandNoahMPDriverFinalize()
end subroutine ufsLandNoahMPDriverFinalize

end module ufsLandNoahMPDriverModule
