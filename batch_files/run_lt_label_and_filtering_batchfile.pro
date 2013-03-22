
;#####################################################################################################
;
;landtrendr post-segmentation change labeling\map creation and spatial filter and patch aggregation 
;
;#####################################################################################################



;-------------inputs-------------
;full path to the diag.sav files in the outputs folder that you want to run labeling and filter on
diag_files=["T:\Groups\Spacers-Annex2\Scenes\044031\outputs\tcangle\LT_v2.00_tcangle_044031_paramset03_20120323_222349_diag.sav",$
  "T:\Groups\Spacers-Annex2\Scenes\044032\outputs\tcangle\LT_v2.00_tcangle_044032_paramset03_20120323_222708_diag.sav",$
  "T:\Groups\Spacers-Annex2\Scenes\045028\outputs\tcangle\LT_v2.00_tcangle_045028_paramset03_20120323_222545_diag.sav"]

;full path to label parameter .txt files.  these correspond to the above daig_files so one must exist
;for for each, they do not have to be the same, but can be     
label_parameters_txt=["\\forestry\share\Groups\Spacers-Annex\Projects\cmonster\lt_label_runs\mr_224\tcangle\mr_224_tcangle_label_params.txt",$
  "\\forestry\share\Groups\Spacers-Annex\Projects\cmonster\lt_label_runs\mr_224\tcangle\mr_224_tcangle_label_params.txt",$
  "\\forestry\share\Groups\Spacers-Annex\Projects\cmonster\lt_label_runs\mr_224\tcangle\mr_224_tcangle_label_params.txt"]

;full path to a single class code file that defines what map outputs will be created    
class_code_txt = "\\forestry\share\Groups\Spacers-Annex\Projects\cmonster\lt_label_runs\mr_224\mr_224_label_codes.txt"

;full path to a projection template file  
templatehdr = "\\forestry\share\Groups\Spacers-Annex2\Scenes\ppprrr\study_area\mrlc_template_headerfile.hdr"


;-------------run the program------------- 
run_lt_label_and_filtering, diag_files, label_parameters_txt, class_code_txt, templatehdr 