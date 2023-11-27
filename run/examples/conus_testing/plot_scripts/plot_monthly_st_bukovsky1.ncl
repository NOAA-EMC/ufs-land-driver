
; This script compares monthly soil temperature with a baseline

begin

wks  = gsn_open_wks("png", "st_monthly_region1_"+sim2+"_compare_to_"+sim1)

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

; no soil temperature reference for now
;infile = addfile("/scratch2/NCEPDEV/land/data/evaluation/GLEAM/C96_conus/climo/GLEAM_C96_conus_climatology.nc","r")
;tr_gleam_36a_all = infile->GLEAM_36a_Et  ; (month,locations)

st1_all_sim1  = new((/num_locations/),float)
st1_all_sim2  = new((/num_locations/),float)
st2_all_sim1  = new((/num_locations/),float)
st2_all_sim2  = new((/num_locations/),float)
st3_all_sim1  = new((/num_locations/),float)
st3_all_sim2  = new((/num_locations/),float)
st4_all_sim1  = new((/num_locations/),float)
st4_all_sim2  = new((/num_locations/),float)

st1_sim1 = new((/12/),float)
st1_sim2 = new((/12/),float)
st2_sim1 = new((/12/),float)
st2_sim2 = new((/12/),float)
st3_sim1 = new((/12/),float)
st3_sim2 = new((/12/),float)
st4_sim1 = new((/12/),float)
st4_sim2 = new((/12/),float)

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
res@pmLegendOrthogonalPosF = -0.47
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
res@xyExplicitLegendLabels = (/" "+sim1+" ST1"," "+sim2+" ST1"," "+sim1+" ST2"," "+sim2+" ST2", \
                               " "+sim1+" ST3"," "+sim2+" ST3"," "+sim1+" ST4"," "+sim2+" ST4"/)

ivegs = (/   3 ,   4 ,   5 ,   7 ,   8 ,  9 ,  10 ,  11,  12 /)

do iveg = 0,8

do imm = 1,12

  infile_sim1 = addfile(path1+"/ufs_land_monthly_mean.2013-2022."+tostring_with_format(imm,"%2.2d")+".nc","r")
  multiplier = 1.0 ; 3600.0 * infile_sim1->number_in_average
  st1_all_sim1 = multiplier * infile_sim1->temperature_soil(0,0,:)
   st1_all_sim1 = mask(st1_all_sim1,region.eq.ivegs(iveg).and.good_mask,True)
  st2_all_sim1 = multiplier * infile_sim1->temperature_soil(0,1,:)
   st2_all_sim1 = mask(st2_all_sim1,region.eq.ivegs(iveg).and.good_mask,True)
  st3_all_sim1 = multiplier * infile_sim1->temperature_soil(0,2,:)
   st3_all_sim1 = mask(st3_all_sim1,region.eq.ivegs(iveg).and.good_mask,True)
  st4_all_sim1 = multiplier * infile_sim1->temperature_soil(0,3,:)
   st4_all_sim1 = mask(st4_all_sim1,region.eq.ivegs(iveg).and.good_mask,True)

  infile_sim2 = addfile(path2+"/ufs_land_monthly_mean.2013-2022."+tostring_with_format(imm,"%2.2d")+".nc","r")
  multiplier = 1.0 ; 3600.0 * infile_sim2->number_in_average
  st1_all_sim2 = multiplier * infile_sim2->temperature_soil(0,0,:)
   st1_all_sim2 = mask(st1_all_sim2,region.eq.ivegs(iveg).and.good_mask,True)
  st2_all_sim2 = multiplier * infile_sim2->temperature_soil(0,1,:)
   st2_all_sim2 = mask(st2_all_sim2,region.eq.ivegs(iveg).and.good_mask,True)
  st3_all_sim2 = multiplier * infile_sim2->temperature_soil(0,2,:)
   st3_all_sim2 = mask(st3_all_sim2,region.eq.ivegs(iveg).and.good_mask,True)
  st4_all_sim2 = multiplier * infile_sim2->temperature_soil(0,3,:)
   st4_all_sim2 = mask(st4_all_sim2,region.eq.ivegs(iveg).and.good_mask,True)

  st1_sim1(imm-1) = avg(st1_all_sim1)
  st2_sim1(imm-1) = avg(st2_all_sim1)
  st3_sim1(imm-1) = avg(st3_all_sim1)
  st4_sim1(imm-1) = avg(st4_all_sim1)

  st1_sim2(imm-1) = avg(st1_all_sim2)
  st2_sim2(imm-1) = avg(st2_all_sim2)
  st3_sim2(imm-1) = avg(st3_all_sim2)
  st4_sim2(imm-1) = avg(st4_all_sim2)

end do

  num_veg = num(region.eq.ivegs(iveg))
  res@tiMainString = tostring(region_names(ivegs(iveg)-1,:))+" ("+num_veg+")"
  plots(iveg) = gsn_csm_xy(wks,time,(/st1_sim1,st1_sim2,st2_sim1,st2_sim2,st3_sim1,st3_sim2,st4_sim1,st4_sim2/),res)

end do

resp = True

resp@gsnPanelMainString = "Soil Temperature [K]"
gsn_panel(wks,plots,(/3,3/),resp)

end
