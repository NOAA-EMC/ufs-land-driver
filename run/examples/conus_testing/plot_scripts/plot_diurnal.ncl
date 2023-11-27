
; This script compares diurnal cycles with observations and a baseline

begin

wks_t2  = gsn_open_wks("png", "t2_diurnal_"+sim2+"_compare_to_"+sim1)
wks_td2 = gsn_open_wks("png","td2_diurnal_"+sim2+"_compare_to_"+sim1)
wks_shf = gsn_open_wks("png","shf_diurnal_"+sim2+"_compare_to_"+sim1)
wks_lhf = gsn_open_wks("png","lhf_diurnal_"+sim2+"_compare_to_"+sim1)
wks_ghf = gsn_open_wks("png","ghf_diurnal_"+sim2+"_compare_to_"+sim1)

path1 = "/scratch2/NCEPDEV/land/data/conus/tests/"+sim1
path2 = "/scratch2/NCEPDEV/land/data/conus/tests/"+sim2

infile_obs = addfile("/scratch2/NCEPDEV/land/data/evaluation/METAR_SYNOP/diurnal/metar_diurnal.nc","r")
infile_lu  = addfile("/scratch2/NCEPDEV/land/data/evaluation/METAR_SYNOP/metar_allgrids_lookup.nc","r")

C96_conus_lookup = infile_lu->C96_conus_lookup

diurnal_obs_t2_in = infile_obs->t2_monthly_diurnal_climo(month | :, hour | :, stations | :)    ; dimensions (stations, month, hour)
diurnal_obs_t2 = diurnal_obs_t2_in(0,:,:)
diurnal_obs_td2_in = infile_obs->td2_monthly_diurnal_climo(month | :, hour | :, stations | :)    ; dimensions (stations, month, hour)
diurnal_obs_td2 = diurnal_obs_td2_in(0,:,:)

diurnal_sim1_t2  = diurnal_obs_t2
diurnal_sim2_t2  = diurnal_obs_t2
diurnal_sim1_td2 = diurnal_obs_t2
diurnal_sim2_td2 = diurnal_obs_t2

var2plot_sim1 = new((/25/),float)
var2plot_sim2 = new((/25/),float)
var2plot_obs  = new((/25/),float)

plots_t2 = new(12,graphic)
plots_td2 = new(12,graphic)
plots_shf = new(12,graphic)
plots_lhf = new(12,graphic)
plots_ghf = new(12,graphic)
time = ispan(0,24,1)
res = True
res@xyLineColors  = (/"red","blue","black","blue","black","black"/)
res@xyMarkLineModes = (/"Lines","Lines","Markers","Lines","Lines","Lines","Lines"/)
res@xyLineThicknessF = 4.0
res@xyMonoDashPattern = False
res@xyDashPatterns = (/0,0,0,0,1,0/)
res@xyMonoMarker = True
res@xyMarker = 16
res@xyMarkerSizeF = 0.01
res@pmLegendDisplayMode = "Always"
res@pmLegendSide           = "Top" 
res@pmLegendParallelPosF   = .25   
res@pmLegendOrthogonalPosF = -0.38
res@pmLegendWidthF         = 0.15
res@pmLegendHeightF        = 0.15
res@lgPerimOn              = False
res@lgLabelFontHeightF     = .018
res@gsnDraw = False
res@gsnFrame = False
res@gsnPaperOrientation = "landscape" ;auto          
res@gsnMaximize         = True        ; must include w/ Paper Orientation

res@trXMinF = 0
res@trXMaxF = 24
;res@trYMinF = -100
;res@trYMaxF = 800
res@xyExplicitLegendLabels = (/" "+sim1," "+sim2," METAR","LWDN","SWUP","SWDN"/)

mmms = (/"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"/)

do imm = 1,12

  diurnal_obs_t2 = diurnal_obs_t2_in(imm-1, :, :)
  diurnal_obs_td2 = diurnal_obs_td2_in(imm-1, :, :)

  infile_sim1 = addfile(path1+"/ufs_land_diurnal.2013-2022."+tostring_with_format(imm,"%2.2d")+".nc","r")

   t2_sim1 = infile_sim1->temperature_2m(0,:,:)            ; dimensions (hour, location)
  td2_sim1 = infile_sim1->dewpoint_2m(0,:,:)
  shf_sim1 = infile_sim1->sensible_heat_total(0,:,:)
  lhf_sim1 = infile_sim1->latent_heat_total(0,:,:)
  ghf_sim1 = infile_sim1->ground_heat_total(0,:,:)

  infile_sim2 = addfile(path2+"/ufs_land_diurnal.2013-2022."+tostring_with_format(imm,"%2.2d")+".nc","r")

   t2_sim2 = infile_sim2->temperature_2m(0,:,:)            ; dimensions (hour, location)
  td2_sim2 = infile_sim2->dewpoint_2m(0,:,:)
  shf_sim2 = infile_sim2->sensible_heat_total(0,:,:)
  lhf_sim2 = infile_sim2->latent_heat_total(0,:,:)
  ghf_sim2 = infile_sim2->ground_heat_total(0,:,:)

  count_good_t2 = 0
  count_good_td2 = 0
  do iloc = 0, 3821
    if(all(.not.ismissing(diurnal_obs_t2(:,iloc)))) then
      count_good_t2 = count_good_t2 + 1
      diurnal_sim1_t2 (:,iloc) = (/  t2_sim1(:,C96_conus_lookup(iloc)) /)   ; do vector-to-obs assignment
      diurnal_sim2_t2 (:,iloc) = (/  t2_sim2(:,C96_conus_lookup(iloc)) /)   ; do vector-to-obs assignment
    end if
    if(all(.not.ismissing(diurnal_obs_td2(:,iloc)))) then
      count_good_td2 = count_good_td2 + 1
      diurnal_sim1_td2(:,iloc) = (/ td2_sim1(:,C96_conus_lookup(iloc)) /)
      diurnal_sim2_td2(:,iloc) = (/ td2_sim2(:,C96_conus_lookup(iloc)) /)
    end if
  end do
  print("Number of good t2 observations locations:"+count_good_t2)
  print("Number of good td2 observations locations:"+count_good_td2)

; temperature plots

  var2plot_sim1(0:23) = dim_avg(diurnal_sim1_t2)
  var2plot_sim2(0:23) = dim_avg(diurnal_sim2_t2)
  var2plot_obs(0:23)  = dim_avg(diurnal_obs_t2)
  var2plot_sim1(24) = var2plot_sim1(0)     ; copy hour 24 from hour 0
  var2plot_sim2(24) = var2plot_sim2(0)
  var2plot_obs(24)  = var2plot_obs(0)

  res@tiMainString = mmms(imm-1)
  plots_t2(imm-1) = gsn_csm_xy(wks_t2,time,(/var2plot_sim1,var2plot_sim2,var2plot_obs/),res)

; dewpoint plots

  var2plot_sim1(0:23) = dim_avg(diurnal_sim1_td2)
  var2plot_sim2(0:23) = dim_avg(diurnal_sim2_td2)
  var2plot_obs(0:23)  = dim_avg(diurnal_obs_td2)
  var2plot_sim1(24) = var2plot_sim1(0)     ; copy hour 24 from hour 0
  var2plot_sim2(24) = var2plot_sim2(0)
  var2plot_obs(24)  = var2plot_obs(0)

  res@tiMainString = mmms(imm-1)
  plots_td2(imm-1) = gsn_csm_xy(wks_td2,time,(/var2plot_sim1,var2plot_sim2,var2plot_obs/),res)

; SHF plots

  var2plot_sim1(0:23) = dim_avg(shf_sim1)
  var2plot_sim2(0:23) = dim_avg(shf_sim2)
  var2plot_sim1(24) = var2plot_sim1(0)     ; copy hour 24 from hour 0
  var2plot_sim2(24) = var2plot_sim2(0)

  res@tiMainString = mmms(imm-1)
  plots_shf(imm-1) = gsn_csm_xy(wks_shf,time,(/var2plot_sim1,var2plot_sim2/),res)

; LHF plots

  var2plot_sim1(0:23) = dim_avg(lhf_sim1)
  var2plot_sim2(0:23) = dim_avg(lhf_sim2)
  var2plot_sim1(24) = var2plot_sim1(0)     ; copy hour 24 from hour 0
  var2plot_sim2(24) = var2plot_sim2(0)

  res@tiMainString = mmms(imm-1)
  plots_lhf(imm-1) = gsn_csm_xy(wks_lhf,time,(/var2plot_sim1,var2plot_sim2/),res)

; LHF plots

  var2plot_sim1(0:23) = -1.0*dim_avg(ghf_sim1)
  var2plot_sim2(0:23) = -1.0*dim_avg(ghf_sim2)
  var2plot_sim1(24) = var2plot_sim1(0)     ; copy hour 24 from hour 0
  var2plot_sim2(24) = var2plot_sim2(0)

  res@tiMainString = mmms(imm-1)
  plots_ghf(imm-1) = gsn_csm_xy(wks_ghf,time,(/var2plot_sim1,var2plot_sim2/),res)

end do

resp = True

resp@gsnPanelMainString = "2-m Temperature"
gsn_panel(wks_t2,plots_t2,(/3,4/),resp)

resp@gsnPanelMainString = "2-m Dewpoint"
gsn_panel(wks_td2,plots_td2,(/3,4/),resp)

resp@gsnPanelMainString = "Sensible Heat Flux"
gsn_panel(wks_shf,plots_shf,(/3,4/),resp)

resp@gsnPanelMainString = "Latent Heat Flux"
gsn_panel(wks_lhf,plots_lhf,(/3,4/),resp)

resp@gsnPanelMainString = "Ground Heat Flux (positive downward)"
gsn_panel(wks_ghf,plots_ghf,(/3,4/),resp)


end
