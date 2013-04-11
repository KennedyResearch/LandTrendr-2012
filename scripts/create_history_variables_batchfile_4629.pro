retall

;this batchfile will create history variables from the segmentation and ftv outputs

;enter the paths to diag.sav files created during segmentation to indicate which
;segmentation runs you want to create history variables for 
diagfile = ["/projectnb/trenders/scenes/046029/outputs/nbr/LT_v2.00_nbr_046029_paramset01_20130125_164800_diag.sav"] ;full path to dia.sav files - can include multiple files, use a comma to separate them
template_headerfile = '/projectnb/trenders/helperfiles/mrlc_template_headerfile.hdr'


;#####################################################################################
create_history_variables, diagfile, template_headerfile
