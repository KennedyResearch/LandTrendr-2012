;this batch file

ppprrr = '046026'
path = 'F:\4626\'; F:\4626_2\4626\VCT\glovis

vct_convert_glovis = 1
prep_dem = 1
prep_nlcd = 1
run_vct_for_masks = 1
lt_convert_glovis = 1

;--------------------------------------------------------------------------------------------------------------
;***************************************
;VCT-DEM STUFF
;
;DEM what do you want to do?
; marked as "skip" above - USE 0
; use an original SRTM DEM example: SRTM_ffB01_p046r031.tif.gz or SRTM_ffB01_p046r031.tif - USE 1
; use an already VCT-prepared DEM? - USE 2
; have the program download a SRTM DEM? - USE 3

dem_type = 3 ;use: 1, 2, or 3 - see above for definition

;if the above was 1 or 2, supply the full path to the SRTM DEM
;  if you used 3, then type 'x' (include the quotes)
srtm_dem = ['x'] ;if the above variable is set to 1, give the full path, else comment out the line (;)

;optional variable
wgs_utm_pro_ref_img = ['x']  ;list of glovis image paths unpacked in the first step of VCT ("*\VCT\glovis\" folder) - order must match the path/row of the above DEMs

;optional variable
dem_out_img = ['x']  ;list the full path and name for the new VCT-ready DEMs - order must match that of the orig_srtm_dem variable (no extention)


;***************************************
;VCT-NLCD MAP STUFF

;NLCD what do you want to do?
; use an already VCT-prepared NLCD map? - USE 1
; have the program make one  - USE 2
nlcd_type = [2]

;optional variable
nlcdimg = ["L:\NLCD2001_landcover_v2_2-13-11\nlcd2001_landcover_v2_2-13-11.img"] ;full path to the national albers NLCD 2001 map

;optional variable
nlcdout = ['x']  ;list the full path and names for the output subset NLCD maps - must be in the same scene order as the orig_srtm_dem variable

;---------done with inputs--------
;--------------------------------------------------------------------------------------------------------------

run_params = $
  {ppprrr=ppprrr,$
   path=path,$
   vct_convert_glovis=vct_convert_glovis,$
   prep_dem=prep_dem,$
   dem_type=dem_type,$
   srtm_dem=srtm_dem,$
   wgs_utm_pro_ref_img,$
   dem_out_img=dem_out_img,$
   prep_nlcd=prep_nlcd,$
   nlcd_type=nlcd_type,$
   nlcdimg=nlcdimg,$
   nlcdout=nlcdout}
   run_vct_for_masks=run_vct_for_masks,$
   lt_convert_glovis=lt_convert_glovis}

landtrendr_auto, run_params





