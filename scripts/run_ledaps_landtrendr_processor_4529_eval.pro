retall
;this batch file controls all of the ledaps landtrendr preprocessing steps

;---please state the landsat path/row ID and path to the data as described following each variable---
ppprrr = '045029' ; ex. '046026' -MUST BE PPP (path) and RRR (row), three digits for both path and row, use zero if needed ex: '0PP0RR'
path = '/projectnb/trenders/scenes/045029/' ; ex. 'F:\046026\' -MAKE SURE THERE IS A "\" AT THE END, must point to a drive path, even if on a server  
useareafile = "/projectnb/trenders/scenes/gnn_snapped_cmon_usearea_files/045029_usearea.bsq" ; full path to a 0-1 raster that defines where Landtrendr will be run :: zeros = don't run, ones = run 
segparamstxt = "/projectnb/trenders/helperfiles/nbr_segmentation_parameters.txt" ;give full path to the segmentation parameter .txt file
template_hdr = "/projectnb/trenders/helperfiles/mrlc_template_headerfile.hdr"  ;give the full path to a template projection header (.hdr) file
label_parameters_txt = "/projectnb/trenders/helperfiles/eval_label_params.txt"
class_code_txt = "/projectnb/trenders/helperfiles/eval_class_codes.txt"


;PROCESSING SWITCHES
;1=do this, 0=don't do this, or as described
resume_segmentation  = 0  ;if segmentation crashed mid-process set this to 1 to begin on the chunk it let off, else leave at 0 
segmentation_eval    = 1  ;runs landtrendr in evaluation mode
segmentation         = 0  ;creates segmentation outputs
fit_to_vertices      = 0  ;use 1 to run bgw, 2 to run b5,b4,b3, 3 to run both
dist_rec_snapshots   = 0  ;create disturbance and recovery slice outputs
dark_seg_outputs     = 0  ;creates an output used to make a forest-nonforest mask
progressbaryesno     = 0  ;set to 1 to have a graphical progress bar during segmentation, 0 to use text 

;(OPTIONAL) ADJUST THE IMAGES TO RUN THROUGH SEGMENTATION  
image_list = [0] ;list image dates to leave in or take out of segmentation - leave as 0 if you want to use all images
  ;image date format: single: [1984226] multiple: [1984226,1985196,1992216]  
image_list_type = 1 ;1 = exclude above dates ::::::: 2 = run only on above dates


run_params = {ppprrr:ppprrr,$
  path:path,$
  segmentation_eval:segmentation_eval,$
  segparamstxt:segparamstxt,$
  segmentation:segmentation,$
  resume:resume_segmentation,$
  useareafile:useareafile,$
  fit_to_vertices:fit_to_vertices,$
  dist_rec_snapshots:dist_rec_snapshots,$
  dark_seg_outputs:dark_seg_outputs,$
  image_list:image_list,$
  image_list_type:image_list_type,$
  template_hdr:template_hdr, $
  progressbaryesno:progressbaryesno, $
  label_parameters_txt:label_parameters_txt, $
  class_code_txt:class_code_txt}
  
.run tbcd_v2
ledaps_landtrendr_processor, run_params
