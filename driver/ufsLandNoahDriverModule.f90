module ufsLandNoahDriverModule

implicit none

contains

subroutine ufsLandNoahDriverInit(namelist, static, forcing, noah)

  use NamelistRead
  use ufsLandNoahType
  use ufsLandStaticModule
  use ufsLandInitialModule
  use ufsLandForcingModule
  use ufsLandNoahRestartModule

  implicit none

  type (namelist_type)     :: namelist
  type (noah_type)         :: noah
  type (static_type)       :: static
  type (initial_type)      :: initial
  type (forcing_type)      :: forcing
  type (noah_restart_type) :: restart

  call static%ReadStatic(namelist)
  
  call noah%Init(namelist,namelist%lensub)

  if(namelist%restart_simulation) then
    call restart%ReadRestartNoah(namelist, noah)
  else
    call initial%ReadInitial(namelist)
    call initial%TransferInitialNoah(namelist, noah)
  end if
  
  call static%TransferStaticNoah(noah)
  
  call noah%TransferNamelist(namelist)
  
  call forcing%ReadForcingInit(namelist)
  
end subroutine ufsLandNoahDriverInit
  
subroutine ufsLandNoahDriverRun(namelist, static, forcing, noah)

use machine , only : kind_phys
use lsm_noah
use sfc_diff
use set_soilveg_mod
use funcphys
use namelist_soilveg, only : z0_data
use physcons, only : grav   => con_g,    cp   => con_cp,          &
                     hvap   => con_hvap, rd   => con_rd,          &
                     eps    => con_eps, epsm1 => con_epsm1,       &
                     rvrdm1 => con_fvirt,                         &
		     tfreeze=> con_t0c

use interpolation_utilities
use NamelistRead, only         : namelist_type
use ufsLandNoahType, only      : noah_type
use ufsLandStaticModule, only  : static_type
use ufsLandForcingModule
use ufsLandIOModule
use ufsLandNoahRestartModule

type (namelist_type)  :: namelist
type (noah_type)      :: noah
type (forcing_type)   :: forcing
type (static_type)    :: static
type (output_type)    :: output
type (noah_restart_type)    :: restart

integer          :: timestep
double precision :: now_time
real, allocatable, dimension(:) :: rho
real, allocatable, dimension(:) :: albdvis_lnd, albdnir_lnd, albivis_lnd, albinir_lnd, adjvisbmd, adjnirbmd, adjvisdfd, adjnirdfd
real, allocatable, dimension(:) :: prsik1,prslk1,ustar,rb,stress,fm,fh,fm10,fh2,z0pert,ztpert,garea,fake  ! some fields needed for sfc_diff
logical, allocatable, dimension(:) :: lfake

real(kind=kind_phys), parameter :: one     = 1.0_kind_phys
real(kind=kind_phys)            :: cpinv, hvapi

associate (                             &
   ps         => forcing%surface_pressure    ,&
   t1         => forcing%temperature         ,&
   q1         => forcing%specific_humidity   ,&
   dlwflx     => forcing%downward_longwave   ,&
   dswsfc     => forcing%downward_shortwave  ,&
   wind       => forcing%wind_speed          ,&
   tprcp      => forcing%precipitation       ,&
   im         => noah%static%im        ,&
   km         => noah%static%km        ,&
   delt       => noah%static%delt      ,&
   isot       => noah%static%isot      ,&
   ivegsrc    => noah%static%ivegsrc   ,&
   pertvegf   => noah%static%pertvegf  ,&  
   lheatstrg  => noah%static%lheatstrg ,& 
   errmsg     => noah%static%errmsg    ,& 
   errflg     => noah%static%errflg    ,& 
   soiltyp    => noah%model%soiltyp    ,&
   vegtype    => noah%model%vegtype    ,&
   slopetyp   => noah%model%slopetyp   ,&
   sigmaf     => noah%model%sigmaf     ,&
   sfcemis    => noah%model%sfcemis    ,&
   snet       => noah%model%snet       ,&
   tg3        => noah%model%tg3        ,&
   cm         => noah%model%cm         ,&
   ch         => noah%model%ch         ,&
   prsl1      => noah%model%prsl1      ,&
   shdmin     => noah%model%shdmin     ,&
   shdmax     => noah%model%shdmax     ,&
   snoalb     => noah%model%snoalb     ,&
   sfalb      => noah%model%sfalb      ,&
   zf         => noah%model%zf         ,&
   bexppert   => noah%model%bexppert   ,&
   xlaipert   => noah%model%xlaipert   ,&
   vegfpert   => noah%model%vegfpert   ,&
   flag_iter  => noah%model%flag_iter  ,&
   flag_guess => noah%model%flag_guess ,&
   land       => noah%model%land       ,&
   prslki     => noah%model%prslki     ,&
   weasd      => noah%model%weasd      ,&
   snwdph     => noah%model%snwdph     ,&
   tskin      => noah%model%tskin      ,&
   srflag     => noah%model%srflag     ,&
   canopy     => noah%model%canopy     ,&
   trans      => noah%model%trans      ,&
   tsurf      => noah%model%tsurf      ,&
   zorl       => noah%model%zorl       ,&
   smc        => noah%model%smc        ,&
   stc        => noah%model%stc        ,&
   slc        => noah%model%slc        ,&
   sncovr1    => noah%model%sncovr1    ,&
   qsurf      => noah%model%qsurf      ,&
   gflux      => noah%model%gflux      ,&
   drain      => noah%model%drain      ,&
   evap       => noah%model%evap       ,&
   hflx       => noah%model%hflx       ,&
   ep         => noah%model%ep         ,&
   runoff     => noah%model%runoff     ,&
   cmm        => noah%model%cmm        ,&
   chh        => noah%model%chh        ,&
   evbs       => noah%model%evbs       ,&
   evcw       => noah%model%evcw       ,&
   sbsno      => noah%model%sbsno      ,&
   snowc      => noah%model%snowc      ,&
   stm        => noah%model%stm        ,&
   snohf      => noah%model%snohf      ,&
   smcwlt2    => noah%model%smcwlt2    ,&
   smcref2    => noah%model%smcref2    ,&
   wet1       => noah%model%wet1        &
   )


allocate(prsik1(im))
allocate(prslk1(im))
allocate( ustar(im))
allocate(    rb(im))
allocate(stress(im))
allocate(    fm(im))
allocate(    fh(im))
allocate(  fm10(im))
allocate(   fh2(im))
allocate(z0pert(im))
allocate(ztpert(im))
allocate(  fake(im))
allocate( garea(im))
allocate( lfake(im))
allocate(   rho(im))
allocate(albdvis_lnd(im))
allocate(albdnir_lnd(im))
allocate(albivis_lnd(im))
allocate(albinir_lnd(im))
allocate(adjvisbmd  (im))
allocate(adjnirbmd  (im))
allocate(adjvisdfd  (im))
allocate(adjnirdfd  (im))

pertvegf = 0.d0
bexppert = 0.d0
xlaipert = 0.d0
vegfpert = 0.d0
z0pert   = 0.d0
ztpert   = 0.d0
fake     = -999._kind_phys
lfake    = .false.
ustar    =  0.1
rb       =  0.1
stress   =  0.1
fm       =  1.0
fh       =  1.0
fm10     =  1.0
fh2      =  1.0
garea    =  3000.0

flag_iter  = .true.
flag_guess = .false.

call set_soilveg(0,isot,ivegsrc,0)
call gpvs()

zorl     = z0_data(vegtype) * 100.0   ! at driver level, roughness length in cm

time_loop : do timestep = 1, namelist%run_timesteps

  now_time = namelist%initial_time + timestep * namelist%timestep_seconds

  call forcing%ReadForcing(namelist, static, now_time)

  call interpolate_monthly(now_time, im, static%gvf_monthly, sigmaf)
  call interpolate_monthly(now_time, im, static%albedo_monthly, sfalb)
  
  albdvis_lnd = sfalb
  albdnir_lnd = sfalb
  albivis_lnd = sfalb
  albinir_lnd = sfalb
  
  adjvisbmd = 0.5 * 0.7 * dswsfc   ! making an assumption about radiation disaggregation
  adjnirbmd = 0.5 * 0.7 * dswsfc   ! 50% VIS, 50% NIR
  adjvisdfd = 0.5 * 0.3 * dswsfc   ! 30% diffuse, 70% direct
  adjnirdfd = 0.5 * 0.3 * dswsfc

  snet   = dswsfc * (1.0_kind_phys - sfalb)
  prsl1  = ps * exp(-1.d0*zf/29.25d0/t1)
  srflag = 0.0d0
  where(t1 < tfreeze) srflag = 1.d0
  prslki = (exp(zf/29.25d0/t1))**(2.d0/7.d0)
  prsik1 = prslki
  prslk1 = prslki
  
      call sfc_diff_run (im,rvrdm1,eps,epsm1,grav,                 &  !intent(in)
                         ps,t1,q1,zf,garea,wind,                   &  !intent(in)
                         prsl1,prslki,prsik1,prslk1,               &  !intent(in)
                         sigmaf,vegtype,shdmax,ivegsrc,            &  !intent(in)
                         z0pert,ztpert,                            &  ! mg, sfc-perts !intent(in)
                         flag_iter,.false.,                        &  !intent(in)
                         fake,fake,-999,                           &  !hafs,z0 type !intent(in)
                         lfake,land,lfake,                         &  !intent(in)
                         .true.,                                   &  !intent(in)
                         fake, tskin,fake,                         &  !intent(in)
                         fake, tsurf,fake,                         &  !intent(in)
                         fake,  zorl,fake,                         &  !intent(inout)
                         fake,                                     &  !intent(inout)
                         fake, ustar,fake,                         &  !intent(inout)
                         fake,    cm,fake,                         &  !intent(inout)
                         fake,    ch,fake,                         &  !intent(inout)
                         fake,    rb,fake,                         &  !intent(inout)
                         fake,stress,fake,                         &  !intent(inout)
                         fake,    fm,fake,                         &  !intent(inout)
                         fake,    fh,fake,                         &  !intent(inout)
                         fake,  fm10,fake,                         &  !intent(inout)
                         fake,   fh2,fake,                         &  !intent(inout)
                         fake,  fake,fake,                         &  !intent(inout)
                         fake,                                     &  !intent(out)
                         errmsg, errflg)                              !intent(out)


      call lsm_noah_run                                               &
         ( im, km, grav, cp, hvap, rd, eps, epsm1, rvrdm1, ps,        &  !  ---  inputs:
           t1, q1, soiltyp, vegtype, sigmaf,                          &
           sfcemis, dlwflx, dswsfc, delt, tg3, cm, ch,                &
           prsl1, prslki, zf, land, wind, slopetyp,                   &
           shdmin, shdmax, snoalb, sfalb, flag_iter, flag_guess,      &
           lheatstrg, isot, ivegsrc,                                  &
           bexppert, xlaipert, vegfpert,pertvegf,                     &  !  ---  in/outs:
           albdvis_lnd, albdnir_lnd, albivis_lnd, albinir_lnd,        &  
           adjvisbmd, adjnirbmd, adjvisdfd, adjnirdfd,                &  
           weasd, snwdph, tskin, tprcp, srflag, smc, stc, slc,        &
           canopy, trans, tsurf, zorl,                                &  !  ---  outputs:
           sncovr1, qsurf, gflux, drain, evap, hflx, ep, runoff,      &
           cmm, chh, evbs, evcw, sbsno, snowc, stm, snohf,            &
           smcwlt2, smcref2, wet1, errmsg, errflg                     &
         )

  rho = prsl1 / (rd*t1*(one+rvrdm1*q1)) 
  hflx = hflx * rho * cp
  evap = evap * rho * hvap

  call output%WriteOutputNoah(namelist, noah, forcing, now_time)
  
  if(namelist%restart_timesteps > 0) then
    if(mod(timestep,namelist%restart_timesteps) == 0) then
      call restart%WriteRestartNoah(namelist, noah, now_time)
    end if
  end if

  if(errflg /= 0) then
    write(*,*) "noahmpdrv_run reporting an error"
    write(*,*) errmsg
    stop
  end if

end do time_loop

end associate

end subroutine ufsLandNoahDriverRun 

subroutine ufsLandNoahDriverFinalize()
end subroutine ufsLandNoahDriverFinalize

end module ufsLandNoahDriverModule
