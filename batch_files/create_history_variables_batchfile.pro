retall

;this batchfile will create history variables from the segmentation and ftv outputs

;enter the paths to diag.sav files created during segmentation to indicate which
;segmentation runs you want to create history variables for 
diagfile = ["C:\mock\this_diag_file.sav"] ;full path to dia.sav files - can include multiple files, use a comma to separate them



;#####################################################################################
create_history_variables, diagfile