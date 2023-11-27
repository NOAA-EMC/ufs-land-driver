
; This script compares monthly snow depth and SWE with a baseline

begin

wks  = gsn_open_wks("png", "snow_monthly_veg_"+sim2+"_compare_to_"+sim1)

path1 = "/scratch2/NCEPDEV/land/data/conus/tests/"+sim1
path2 = "/scratch2/NCEPDEV/land/data/conus/tests/"+sim2

infile = addfile("/scratch2/NCEPDEV/land/data/forcing/era5/static/C96_conus/ufs-land_C96_conus_hr3_static_fields.nc","r")

vegetation_category = infile->vegetation_category
num_locations = dimsizes(vegetation_category)

; no snow reference for now
;infile = addfile("/scratch2/NCEPDEV/land/data/evaluation/GLEAM/C96_conus/climo/GLEAM_C96_conus_climatology.nc","r")
;sd_snodas_all = infile->snow_depth  ; (month,locations)

sd_all_sim1  = new((/num_locations/),float)
sw_all_sim1  = new((/num_locations/),float)
sd_all_sim2  = new((/num_locations/),float)
sw_all_sim2  = new((/num_locations/),float)

sd_sim1 = new((/12/),float)
sd_sim2 = new((/12/),float)
sw_sim1 = new((/12/),float)
sw_sim2 = new((/12/),float)

plots = new(12,graphic)

time = ispan(1,12,1)
res = True
res@xyLineColors  = (/"red","blue","red","blue","red","blue","black","black"/)
res@xyMarkLineModes = (/"Lines","Lines","Lines","Lines","Lines","Lines","Lines","Lines","Lines"/)
res@xyLineThicknessF = 2.0
res@xyMonoDashPattern = False
res@xyDashPatterns = (/0,0,1,1,2,2,0,0/)
res@xyMonoMarker = True
res@xyMarker = 16
res@xyMarkerSizeF = 0.01
res@pmLegendDisplayMode = "Always"
res@pmLegendSide           = "Top" 
res@pmLegendParallelPosF   = .67   
res@pmLegendOrthogonalPosF = -0.45
res@pmLegendWidthF         = 0.1
res@pmLegendHeightF        = 0.20
res@lgPerimOn              = False
res@lgLabelFontHeightF     = .018
res@gsnDraw = False
res@gsnFrame = False
;res@gsnPaperOrientation = "landscape" ;auto          
res@gsnMaximize         = True        ; must include w/ Paper Orientation

res@trXMinF = 1
res@trXMaxF = 12
res@xyExplicitLegendLabels = (/" "+sim1+" depth"," "+sim2+" depth"," "+sim1+" SWE"," "+sim2+" SWE"/)

nvegs = (/"ENF","DBF","MIX","OSH","WSH","GRA","WET","CRP","URB","CNM","BAR","MTU"/)
lnvegs = (/"Evergreen Needleleaf Forest","Deciduous Broadleaf Forest","Mixed Forest",\
           "Open Shrub","Wooded Shrub","Grassland","Wetland","Cropland","Urban","Crop/Natural",\
           "Bare Ground","Mixed Tundra"/)
ivegs = (/   1 ,   4 ,   5 ,   7 ,   8 ,  10 ,  11 ,  12 ,  13 ,  14 ,  16 ,  19 /)

do iveg = 0,11

do imm = 1,12

  infile_sim1 = addfile(path1+"/ufs_land_monthly_mean.2013-2022."+tostring_with_format(imm,"%2.2d")+".nc","r")
  multiplier = 1.0 ; 3600.0 * infile_sim1->number_in_average
  sd_all_sim1 = multiplier * infile_sim1->snow_depth(0,:)
   sd_all_sim1 = mask(sd_all_sim1,vegetation_category.eq.ivegs(iveg),True)
  sw_all_sim1 = multiplier * infile_sim1->snow_water_equiv(0,:)
   sw_all_sim1 = mask(sw_all_sim1,vegetation_category.eq.ivegs(iveg),True)

  infile_sim2 = addfile(path2+"/ufs_land_monthly_mean.2013-2022."+tostring_with_format(imm,"%2.2d")+".nc","r")
  multiplier = 1.0 ; 3600.0 * infile_sim2->number_in_average
  sd_all_sim2 = multiplier * infile_sim2->snow_depth(0,:)
   sd_all_sim2 = mask(sd_all_sim2,vegetation_category.eq.ivegs(iveg),True)
  sw_all_sim2 = multiplier * infile_sim2->snow_water_equiv(0,:)
   sw_all_sim2 = mask(sw_all_sim2,vegetation_category.eq.ivegs(iveg),True)

   sd_sim1(imm-1) = avg(sd_all_sim1)
   sw_sim1(imm-1) = avg(sw_all_sim1)

   sd_sim2(imm-1) = avg(sd_all_sim2)
   sw_sim2(imm-1) = avg(sw_all_sim2)

end do

  num_veg = num(vegetation_category.eq.ivegs(iveg))
  res@tiMainString = lnvegs(iveg)+" ("+num_veg+")"
  res@gsnXYFillColors = (/"transparent","transparent","transparent","transparent","transparent","transparent","grey80"/)
  plots(iveg) = gsn_csm_xy(wks,time,(/sd_sim1,sd_sim2,sw_sim1,sw_sim2/),res)

end do

resp = True

resp@gsnPanelMainString = "Snow Depth/SWE [mm]"
gsn_panel(wks,plots,(/3,4/),resp)

end
