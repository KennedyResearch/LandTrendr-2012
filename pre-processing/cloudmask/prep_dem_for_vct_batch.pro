;this batch file 

orig_srtm_dem = ["F:\4631\VCT\test_junk\prep\SRTM_ffB01_p046r031.tif"]  ;list of original SRTM DEM file paths to be processed 

orig_glovisimg = ["F:\4631\VCT\test_junk\outputs\046031_03119990822L71"]  ;list of glovis image paths unpacked in the first step of VCT ("*\VCT\glovis\" folder) - order must match the path/row of the above DEMs

dem_out_img = ["F:\4631\VCT\test_junk\prep\dem_p46r31"]  ;list the full path and name for the new VCT-ready DEMs - order must match that of the orig_srtm_dem variable (no extention) 

nlcdimg = ["L:\NLCD2001_landcover_v2_2-13-11\nlcd2001_landcover_v2_2-13-11.img"]  ;full path to the national albers NLCD 2001 map

nlcdout = ["F:\4631\VCT\test_junk\prep\lc_p46r31"]  ;list the full path and names for the output subset NLCD maps - must be in the same scene order as the orig_srtm_dem variable  


print, "preparing the dem for use in vct"
prep_dem_for_vct, orig_srtm_dem, orig_glovisimg, dem_out_img

print, "preparing the nlcd map for use in vct"
prep_nlcd_for_vct, dem_out_img, nlcdimg, nlcdout