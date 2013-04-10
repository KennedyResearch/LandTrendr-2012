retall
;this batchfile is used to manually create cloudmasks based either on spectral data from given image dates or
;from difference-from-cloud-free condition images

;---please state the landsat path/row ID and path to the data as described following each variable---
ppprrr = '045029' ; ex. '046026' -MUST BE PPP (path) and RRR (row), three digits for both path and row, use zero if needed ex: '0PP0RR'
path = 'C:\temp\cloudmask_move_test\045029\' ; ex. 'F:\046026\' -MAKE SURE THERE IS A "\" AT THE END, must point to a drive path, even if on a server
template_header = "C:\temp\cloudmask_move_test\045029\study_area\mrlc_template_headerfile.hdr" ;give the full path to a template projection header (.hdr) file

;select a cloud/shadow fixing method
;method 1: thresholds clouds\shadows based on image bands
;method 2: thresholds cloud\shadows based on image difference from cloud-free reference image composite
;info: method 2 requires a reference image composite and difference images to be created
cldmsk_fix_method = [1]

;if method 2 is selected, enter the image dates to include in the cloudmask reference image composite.
;for best results near-yearly time steps should be used and the images should be either cloud\haze-free
;or have really good cloudmasks
cldmsk_ref_dates = [1985196,1985228,1986199] ;example: [1985211,1986215,1992202]

;enter the year day ids of the cloudmasks that require manual fixing
fix_these_dates = [1985196, 1985228] ;example: [1985211] or [1985211,1986215] 



;#######################################################################################################################
params = {ppprrr:ppprrr,$
  path:path,$
  cldmsk_fix_method:cldmsk_fix_method,$
  cldmsk_ref_dates:cldmsk_ref_dates,$
  fix_these_dates:fix_these_dates,$
  template_header:template_header}
  
fix_cloudmask_processor, params
