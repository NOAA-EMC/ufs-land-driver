
; This script compares monthly soil moisture with a baseline

begin

wks  = gsn_open_wks("png", "sm_monthly_region1_"+sim2+"_compare_to_"+sim1)

path1 = "/scratch2/NCEPDEV/land/data/conus/tests/"+sim1
path2 = "/scratch2/NCEPDEV/land/data/conus/tests/"+sim2

infile = addfile("/scratch2/NCEPDEV/land/data/forcing/era5/static/C96_conus/ufs-land_C96_conus_hr3_static_fields.nc","r")

vegetation_category = infile->vegetation_category
num_locations = dimsizes(vegetation_category)
good_mask = new(num_locations,logical)
good_mask = where(vegetation_category.ne.15,True,False)

infile = addfile("/scratch2/NCEPDEV/land/data/evaluation/BUKOVSKY/C96_conus/BUKOVSKY_C96_conus.nc","r")

region = infile->regions
region_names = infile->region_names

; no snow reference for now
;infile = addfile("/scratch2/NCEPDEV/land/data/evaluation/GLEAM/C96_conus/climo/GLEAM_C96_conus_climatology.nc","r")
;tr_gleam_36a_all = infile->GLEAM_36a_Et  ; (month,locations)

sm1_all_sim1  = new((/num_locations/),float)
sm1_all_sim2  = new((/num_locations/),float)
sm2_all_sim1  = new((/num_locations/),float)
sm2_all_sim2  = new((/num_locations/),float)
sm3_all_sim1  = new((/num_locations/),float)
sm3_all_sim2  = new((/num_locations/),float)
sm4_all_sim1  = new((/num_locations/),float)
sm4_all_sim2  = new((/num_locations/),float)

sm1_sim1 = new((/12/),float)
sm1_sim2 = new((/12/),float)
sm2_sim1 = new((/12/),float)
sm2_sim2 = new((/12/),float)
sm3_sim1 = new((/12/),float)
sm3_sim2 = new((/12/),float)
sm4_sim1 = new((/12/),float)
sm4_sim2 = new((/12/),float)

plots = new(9,graphic)

time = ispan(1,12,1)
res = True
res@xyLineColors  = (/"red","blue","red","blue","red","blue","red","blue","black"/)
res@xyMarkLineModes = (/"Lines","Lines","Lines","Lines","Lines","Lines","Lines","Lines","Lines"/)
res@xyLineThicknessF = 2.0
res@xyMonoDashPattern = False
res@xyDashPatterns = (/0,0,1,1,2,2,3,3,0,0/)
res@xyMonoMarker = True
res@xyMarker = 16
res@xyMarkerSizeF = 0.01
res@pmLegendDisplayMode = "Always"
res@pmLegendSide           = "Top" 
res@pmLegendParallelPosF   = .22   
res@pmLegendOrthogonalPosF = -1.12
res@pmLegendWidthF         = 0.1
res@pmLegendHeightF        = 0.20
res@lgPerimOn              = False
res@lgLabelFontHeightF     = .015
res@gsnDraw = False
res@gsnFrame = False
;res@gsnPaperOrientation = "landscape" ;auto          
res@gsnMaximize         = True        ; must include w/ Paper Orientation

res@trXMinF = 1
res@trXMaxF = 12
res@xyExplicitLegendLabels = (/" "+sim1+" SM1"," "+sim2+" SM1"," "+sim1+" SM2"," "+sim2+" SM2", \
                               " "+sim1+" SM3"," "+sim2+" SM3"," "+sim1+" SM4"," "+sim2+" SM4"/)

ivegs = (/   3 ,   4 ,   5 ,   7 ,   8 ,  9 ,  10 ,  11,  12 /)

do iveg = 0,8

do imm = 1,12

  infile_sim1 = addfile(path1+"/ufs_land_monthly_mean.2013-2022."+tostring_with_format(imm,"%2.2d")+".nc","r")
  multiplier = 1.0 ; 3600.0 * infile_sim1->number_in_average
  sm1_all_sim1 = multiplier * infile_sim1->soil_moisture_vol(0,0,:)
   sm1_all_sim1 = mask(sm1_all_sim1,region.eq.ivegs(iveg).and.good_mask,True)
  sm2_all_sim1 = multiplier * infile_sim1->soil_moisture_vol(0,1,:)
   sm2_all_sim1 = mask(sm2_all_sim1,region.eq.ivegs(iveg).and.good_mask,True)
  sm3_all_sim1 = multiplier * infile_sim1->soil_moisture_vol(0,2,:)
   sm3_all_sim1 = mask(sm3_all_sim1,region.eq.ivegs(iveg).and.good_mask,True)
  sm4_all_sim1 = multiplier * infile_sim1->soil_moisture_vol(0,3,:)
   sm4_all_sim1 = mask(sm4_all_sim1,region.eq.ivegs(iveg).and.good_mask,True)

  infile_sim2 = addfile(path2+"/ufs_land_monthly_mean.2013-2022."+tostring_with_format(imm,"%2.2d")+".nc","r")
  multiplier = 1.0 ; 3600.0 * infile_sim2->number_in_average
  sm1_all_sim2 = multiplier * infile_sim2->soil_moisture_vol(0,0,:)
   sm1_all_sim2 = mask(sm1_all_sim2,region.eq.ivegs(iveg).and.good_mask,True)
  sm2_all_sim2 = multiplier * infile_sim2->soil_moisture_vol(0,1,:)
   sm2_all_sim2 = mask(sm2_all_sim2,region.eq.ivegs(iveg).and.good_mask,True)
  sm3_all_sim2 = multiplier * infile_sim2->soil_moisture_vol(0,2,:)
   sm3_all_sim2 = mask(sm3_all_sim2,region.eq.ivegs(iveg).and.good_mask,True)
  sm4_all_sim2 = multiplier * infile_sim2->soil_moisture_vol(0,3,:)
   sm4_all_sim2 = mask(sm4_all_sim2,region.eq.ivegs(iveg).and.good_mask,True)

  sm1_sim1(imm-1) = avg(sm1_all_sim1)
  sm2_sim1(imm-1) = avg(sm2_all_sim1)
  sm3_sim1(imm-1) = avg(sm3_all_sim1)
  sm4_sim1(imm-1) = avg(sm4_all_sim1)

  sm1_sim2(imm-1) = avg(sm1_all_sim2)
  sm2_sim2(imm-1) = avg(sm2_all_sim2)
  sm3_sim2(imm-1) = avg(sm3_all_sim2)
  sm4_sim2(imm-1) = avg(sm4_all_sim2)

end do

  num_veg = num(region.eq.ivegs(iveg))
  res@tiMainString = tostring(region_names(ivegs(iveg)-1,:))+" ("+num_veg+")"
  plots(iveg) = gsn_csm_xy(wks,time,(/sm1_sim1,sm1_sim2,sm2_sim1,sm2_sim2,sm3_sim1,sm3_sim2,sm4_sim1,sm4_sim2/),res)

end do

resp = True

resp@gsnPanelMainString = "Soil Moisture [vol]"
gsn_panel(wks,plots,(/3,3/),resp)

end
