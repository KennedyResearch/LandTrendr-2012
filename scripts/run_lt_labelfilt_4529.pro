
;#####################################################################################################
;
;landtrendr post-segmentation change labeling\map creation and spatial filter and patch aggregation 
;
;#####################################################################################################



;-------------inputs-------------
;full path to the diag.sav files in the outputs folder that you want to run labeling and filter on
diag_files=["/projectnb/trenders/scenes/045029/outputs/nbr/LT_v2.00_nbr_045029_paramset01_20130212_192009_diag.sav"]

;full path to label parameter .txt files.  these correspond to the above daig_files so one must exist
;for for each, they do not have to be the same, but can be     

label_parameters_txt=["/projectnb/trenders/helperfiles/nbr_label_parameters.txt"]

;full path to a single class code file that defines what map outputs will be created    
class_code_txt = "/projectnb/trenders/helperfiles/nbr_label_codes.txt"


;full path to a projection template file  
templatehdr = "/projectnb/trenders/helperfiles/mrlc_template_headerfile.hdr"

;-------------run the program------------- 
run_lt_label_and_filtering, diag_files, label_parameters_txt, class_code_txt, templatehdr 
