
; This script compares monthly ET with a baseline

begin

wks  = gsn_open_wks("png", "et_monthly_region2_"+sim2+"_compare_to_"+sim1)

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

infile = addfile("/scratch2/NCEPDEV/land/data/evaluation/GLEAM/C96_conus/climo/GLEAM_C96_conus_climatology.nc","r")
tr_gleam_36a_all = infile->GLEAM_36a_Et  ; (month,locations)
tr_gleam_36b_all = infile->GLEAM_36b_Et
tr_gleam_38a_all = infile->GLEAM_38a_Et
tr_gleam_37b_all = infile->GLEAM_37b_Et
eb_gleam_36a_all = infile->GLEAM_36a_Eb
eb_gleam_36b_all = infile->GLEAM_36b_Eb
eb_gleam_38a_all = infile->GLEAM_38a_Eb
eb_gleam_37b_all = infile->GLEAM_37b_Eb
ec_gleam_36a_all = infile->GLEAM_36a_Ei
ec_gleam_36b_all = infile->GLEAM_36b_Ei
ec_gleam_38a_all = infile->GLEAM_38a_Ei
ec_gleam_37b_all = infile->GLEAM_37b_Ei

et_gleam_36a_all = tr_gleam_36a_all + eb_gleam_36a_all + ec_gleam_36a_all
et_gleam_36b_all = tr_gleam_36b_all + eb_gleam_36b_all + ec_gleam_36b_all
et_gleam_38a_all = tr_gleam_38a_all + eb_gleam_38a_all + ec_gleam_38a_all
et_gleam_37b_all = tr_gleam_37b_all + eb_gleam_37b_all + ec_gleam_37b_all

et_gleam_all = new((/12,1410,4/),float)
et_gleam_all(:,:,0) = et_gleam_36a_all
et_gleam_all(:,:,1) = et_gleam_36b_all
et_gleam_all(:,:,2) = et_gleam_38a_all
et_gleam_all(:,:,3) = et_gleam_37b_all

et_gleam_max_all = dim_max(et_gleam_all)
et_gleam_min_all = dim_min(et_gleam_all)

tr_all_sim1  = new((/num_locations/),float)
tr_all_sim2  = new((/num_locations/),float)
es_all_sim1  = new((/num_locations/),float)
es_all_sim2  = new((/num_locations/),float)
ec_all_sim1  = new((/num_locations/),float)
ec_all_sim2  = new((/num_locations/),float)
et_gleam_max = new((/num_locations/),float)
et_gleam_min = new((/num_locations/),float)

et_sim1 = new((/12/),float)
et_sim2 = new((/12/),float)
 t_sim1 = new((/12/),float)
 t_sim2 = new((/12/),float)
 e_sim1 = new((/12/),float)
 e_sim2 = new((/12/),float)
et_max  = new((/12/),float)
et_min  = new((/12/),float)

plots = new(9,graphic)

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
res@pmLegendParallelPosF   = .17   
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
res@xyExplicitLegendLabels = (/" "+sim1+" ET"," "+sim2+" ET"," "+sim1+" T"," "+sim2+" T", \
                               " "+sim1+" E" ," "+sim2+" E" ," GLEAM MAX ET"," GLEAM MIN ET"/)

ivegs = (/   13 ,   14 ,   15 ,   16 ,   17 ,  19 ,  20 ,  22,  23 /)

do iveg = 0,8

do imm = 1,12

  infile_sim1 = addfile(path1+"/ufs_land_monthly_mean.2013-2022."+tostring_with_format(imm,"%2.2d")+".nc","r")
  multiplier = 3600.0 * infile_sim1->number_in_average
  tr_all_sim1 = multiplier * infile_sim1->transpiration(0,:)
   tr_all_sim1 = mask(tr_all_sim1,region.eq.ivegs(iveg).and.good_mask,True)
  es_all_sim1 = multiplier * infile_sim1->evaporation_soil(0,:)
   es_all_sim1 = mask(es_all_sim1,region.eq.ivegs(iveg).and.good_mask,True)
  ec_all_sim1 = multiplier * infile_sim1->evaporation_canopy(0,:)
   ec_all_sim1 = mask(ec_all_sim1,region.eq.ivegs(iveg).and.good_mask,True)

  infile_sim2 = addfile(path2+"/ufs_land_monthly_mean.2013-2022."+tostring_with_format(imm,"%2.2d")+".nc","r")
  multiplier = 3600.0 * infile_sim2->number_in_average
  tr_all_sim2 = multiplier * infile_sim2->transpiration(0,:)
   tr_all_sim2 = mask(tr_all_sim2,region.eq.ivegs(iveg).and.good_mask,True)
  es_all_sim2 = multiplier * infile_sim2->evaporation_soil(0,:)
   es_all_sim2 = mask(es_all_sim2,region.eq.ivegs(iveg).and.good_mask,True)
  ec_all_sim2 = multiplier * infile_sim2->evaporation_canopy(0,:)
   ec_all_sim2 = mask(ec_all_sim2,region.eq.ivegs(iveg).and.good_mask,True)

   et_sim1(imm-1) = avg(tr_all_sim1 + es_all_sim1 + ec_all_sim1)
    t_sim1(imm-1) = avg(tr_all_sim1)
    e_sim1(imm-1) = avg(es_all_sim1 + ec_all_sim1)

   et_sim2(imm-1) = avg(tr_all_sim2 + es_all_sim2 + ec_all_sim2)
    t_sim2(imm-1) = avg(tr_all_sim2)
    e_sim2(imm-1) = avg(es_all_sim2 + ec_all_sim2)

   et_gleam_max  = mask(et_gleam_max_all(imm-1,:),region.eq.ivegs(iveg).and.good_mask,True)
   et_max(imm-1) = avg(et_gleam_max)

   et_gleam_min  = mask(et_gleam_min_all(imm-1,:),region.eq.ivegs(iveg).and.good_mask,True)
   et_min(imm-1) = avg(et_gleam_min)

end do

  num_veg = num(region.eq.ivegs(iveg))
  res@tiMainString = tostring(region_names(ivegs(iveg)-1,:))+" ("+num_veg+")"
  res@gsnXYFillColors = (/"transparent","transparent","transparent","transparent","transparent","transparent","grey80"/)
  plots(iveg) = gsn_csm_xy(wks,time,(/et_sim1,et_sim2,t_sim1,t_sim2,e_sim1,e_sim2,et_max,et_min/),res)

end do

resp = True

resp@gsnPanelMainString = "ET Components [mm]"
gsn_panel(wks,plots,(/3,3/),resp)

end
