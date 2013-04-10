;**************************************************************************** 
;Copyright © 2008-2011 Oregon State University                                
;All Rights Reserved.                                                         
;                                                                             
;                                                                             
;Permission to use, copy, modify, and distribute this software and its        
;documentation for educational, research and non-profit purposes, without     
;fee, and without a written agreement is hereby granted, provided that the    
;above copyright notice, this paragraph and the following three paragraphs    
;appear in all copies.                                                        
;                                                                             
;                                                                             
;Permission to incorporate this software into commercial products may be      
;obtained by contacting Oregon State University Office of Technology Transfer.
;                                                                             
;                                                                             
;This software program and documentation are copyrighted by Oregon State      
;University. The software program and documentation are supplied "as is",     
;without any accompanying services from Oregon State University. OSU does not 
;warrant that the operation of the program will be uninterrupted or           
;error-free. The end-user understands that the program was developed for      
;research purposes and is advised not to rely exclusively on the program for  
;any reason.                                                                  
;                                                                             
;                                                                             
;IN NO EVENT SHALL OREGON STATE UNIVERSITY BE LIABLE TO ANY PARTY FOR DIRECT, 
;INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST      
;PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN 
;IF OREGON STATE UNIVERSITYHAS BEEN ADVISED OF THE POSSIBILITY OF SUCH        
;DAMAGE. OREGON STATE UNIVERSITY SPECIFICALLY DISCLAIMS ANY WARRANTIES,       
;INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
;FITNESS FOR A PARTICULAR PURPOSE AND ANY STATUTORY WARRANTY OF               
;NON-INFRINGEMENT. THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS,    
;AND OREGON STATE UNIVERSITY HAS NO OBLIGATIONS TO PROVIDE MAINTENANCE,       
;SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.                            
;                                                                             
;**************************************************************************** 
;This batchfile reads a .csv file of plot ids, coords and
;   runs each one through landtrendr and plots results as .csv
;   files, which can be loaded into "lt_point_mode_pdf_summary"
;   in r to produce pdf print outs of plot spectral values and fits
;   per index per parameter set
;
;These steps must be completed prior to running:
;   In order to run landtrendr in point mode for testing various indices and parameter sets you first need to get...
;   all of the image dates normalized and have tasseled cap transformations completed on them
;
;Run the batchfile IDL>   @"path_to_this_batchfile"
;
;Files created:
;   *landtrendr_point_mode_runinfo.csv - run index key for all combinations of given parameters: pval, max_segments, recovery_threshold, desawtooth_val, distweightfactor, vertexcountovershoot, bestmodelproportion     
;   *landtrendr_point_mode_runinfo_p1.csv - run index key for all combinations of given parameters: pval, max_segments, recovery_threshold, desawtooth_val, distweightfactor, vertexcountovershoot, bestmodelproportion, dist_collapse_angle, recv_collapse_angle 
;   *landtrendr_point_mode_runinfo_p2.csv - run index key for all combinations of given parameters: pval, max_segments, recovery_threshold, desawtooth_val, distweightfactor, vertexcountovershoot, bestmodelproportion, dist_collapse_angle, recv_collapse_angle, pct_tree_loss1_threshold, pct_tree_loss20_threshold, pre_dist_cover_threshold, pct_tree_gain_threshold  
;   *landtrendr_point_mode_vertinfo.csv - extracted vertice years, vertice values, and segment deltas for runs in landtrendr_runinfo.csv     
;   *landtrendr_point_mode_vertinfo_p1.csv - extracted vertice years, vertice values, and segment deltas for runs in landtrendr_runinfo_p1.csv   
;   *landtrendr_point_mode_vertinfo_p2.csv - extracted vertice years, vertice values, and segment deltas for runs in landtrendr_runinfo_p2.csv   
;   *landtrendr_point_mode_src.csv - extracted source image year and value for the given plots 
;   *landtrendr_point_mode_pdf_summary_script.r - R script to create a pdf trajectory summary of the various parameter sets
;   *landtrendr_point_mode_fit_summary.pdf - a pdf file that displays the source and fitted data per point per parameter set 
;   
;
;   if R is istalled on your machine and the path to Rscript.exe is defined in your systems path variable...    
;   then this batchfile will call a program to automatically create the pdf summary, alternatively you'll...
;   need to run this batchfile and then navigate to the scenes *\outputs\lt_point_mode\ directory and open...
;   any ".r" files that were created, copy the contents out and paste them into an R session, which will run...
;   the function and create a pdf summary - do this for each ".r" file in the directory


retall
;-----------------------------------------------------------------------------------------------------
;---inputs---
;about variable: "only_create_pdf"... if you've run point mode already and just want to re-create a pdf (usually...
;to adjust the display settings) enter the full path to the "*landtrendr_point_mode_runinfo.csv" file, for the...
;parameter set you're interested in.  This file will be in the *\outputs\lt_point_mode\ directory...
;If you go this route the only other variables that are needed are "path" and everything in the PDF figure display properties section  

only_create_pdf = '' ;see the above description for this variable - default is: '' 


ppprrr =   '025028'   ;ex: '045029'
path =  'T:\Groups\Spacers-Annex\Scenes\2528\'  ;ex: 'F:\2528\'


index = 'wetness' ;select an index to run LT point mode on: 'Band1', 'Band2', 'Band3', 'Band4', 'Band5', 'Band7',
               ;'nbr', 'brightness', 'greenness', 'wetness', 'TCangle', 'NDVI' or create a new one in program:
               ;landtrendr_image_read.pro 

image_info_savefile = "\\forestry\share\Groups\Spacers-Annex\Scenes\2528\images\landtrendr_image_info_025028.sav" ;full path to the image info .sav file

mask_image = "\\forestry\share\Groups\Spacers-Annex\Scenes\2528\study_area\025028_vct_usearea.bsq" ;full path to the mask image for the scene

point_coord_file = "\\forestry\share\Groups\Spacers-Annex\Scenes\2528\outputs\lt_point_mode\2528_eval_point_sample.csv" ;full path to a point file point coordinate file - format the .csv file as the following  
;   integer, float , float
;   plotid , X     , Y
;   1      , 389737, 5422266
;   2      , 389934, 5422050
;   ...    , ...   , ...

p0_only         = 1    ;if you do not want to test percent cover parameters, or do not have a cover model use 1, else use 0
file_coords     = 0    ;if the given point file is in file coords use 1, if map coords use 0
include_p_and_f = 0    ;if you'd like to display the p of f statistic with the trajectory plots use 1, else use 0

;---PDF figure display properties---------------------------------------------------------------------------------------------------- 
width   = 14  ;ex: 14 ; a pdf with trajectories is created, what should the pdf width be (inches)
height  = 12  ;ex: 12 ; a pdf with trajectories is created, what should the pdf height be (inches)
columns = 3   ;ex: 4  ; a pdf with trajectories is created, how many columns of figures do you want per page - note that plot text can be lost with too many
rows    = 4   ;ex: 4  ; a pdf with trajectories is created, how many rows of figures do you want per page - note that plot text can be lost with too many

display_index_min = 0  ;set the minimum y value (index dependent) to be displayed in the output figures - if min and max are set to 0 a default range will be applied    
display_index_max = 0  ;set the maximum y value (index dependent) to be displayed in the output figures - if min and max are set to 0 a default range will be applied 
;-------------------------------------------------------------------------------------------------------------------------------------

;---fill in the default run parameters - best to leave as is---
restore, image_info_savefile
run_params = {image_info:image_info, $
			index:index, $   ;up to two bands, always in [brackets] [b1] or [b1,b2]
			background_val:0, $		;value to ignore in input image bands
			divisor:1, $			;-1 = calculate it for me
			minneeded:6, $	;minimum number of years needed in a pixel for it to calculate	trajectory
			subset:-1, $		;leave as -1 here,   landed separately in called program.
			mask_image:mask_image, $	;mask image has 1's everywhere TBCD should be run, 0's otherwise
			kernelsize:1, $	;the size of box over which to average before runn TBCD on a pixel -- defines one side of the box (i.e. 3 = 3 by 3 box)
			pval:0.05, $				;do not accept trajectories whose p_of_f is greater than this. 0.05 is typical.
			fix_doy_effect:1, $	;set to 1 if mean effect of doy should be taken out using 2nd order curve
			max_segments:6, $		;the maximum number of segments to search for, cannot be > 6. Set to -1 to have it calc'd on the fly
			recovery_threshold:0.5, $	;=1/min_number_years_for recovery segments cannot result in rate faster than 1/min number of years, whic
			skipfactor:1, $			;If >1, then not every pixel is run, but ever "skipfactor"th pixel is run in both x and y and the
										;remainder interpolated.  3 is a good choice
			desawtooth_val:0.9, $		;0.0-1.0 if set to <1.0, spikes that start and end at near same value are removed.  the value sets the
									;threshold on how different they can be to be removed -- 1.0 means no desawtoothing, .9 means that
									;spikes with differences > 0.90 in start and end point are fixed.
      distweightfactor:2, $
      vertexcountovershoot: 3, $
      bestmodelproportion: 0.75}
      

;---fill in the post process percent cover filter parameters - only one value allowed per parameter---
post_process_params = {collapse_dist_angle: [15], $   ;example syntax: [15,18,20]
						           collapse_rec_angle:  [30], $   ;example syntax: [15,18,20]
						           pct_tree_loss1: [10], $        ;example syntax: [15,18,20]
						           pct_tree_loss20: [5], $        ;example syntax: [15,18,20]
						           pre_dist_cover: [20], $        ;example syntax: [15,18,20]
						           pct_tree_gain: [5], $          ;example syntax: [15,18,20]
						           change_model: 'no_cover_model', $  ;optional, name of cover model function, if you don't have one, use 'no_cover_model'  
						           static_model: 'no_cover_model'} ;if you're concerned with % cover filtering then you must create a static change model, see "change_nbr_model_pilot_scenes.pro" for an example function, if you don't have one use 'no_cover_model'    

;---fill in the calibration parameters - as many values as you want to try per parameter, the number of elements does not have to be consistent
calibration_params = {pvals: [0.05,0.1], $                           ;example syntax: [15,18,20]
                      maxsegments: [4,5,6], $                        ;example syntax: [15,18,20]
                      recovery_thresholds: [0.25,1], $               ;example syntax: [15,18,20]
                      desawtooth_thresholds: [0.75, 1.0], $          ;example syntax: [15,18,20]
                      oververtex_thresholds: [3], $                  ;example syntax: [15,18,20]
                      distweight_thresholds: [2], $                  ;example syntax: [15,18,20]
                      bestmodel_thresholds: [0.75, 1.0], $           ;example syntax: [15,18,20]
                      dist_collapse_angles: [7, 15], $               ;example syntax: [15,18,20] - only used if you specified variable: 'p0_only' as 0 
                      recv_collapse_angles: [15, 30], $              ;example syntax: [15,18,20] - only used if you specified variable: 'p0_only' as 0  
                      pct_tree_loss1_thresholds: [10, 15], $         ;example syntax: [15,18,20] - only used if you specified variable: 'p0_only' as 0 
                      pct_tree_loss20_thresholds: [3, 5, 10], $      ;example syntax: [15,18,20] - only used if you specified variable: 'p0_only' as 0 
                      pre_dist_cover_thresholds: [10, 20, 40], $     ;example syntax: [15,18,20] - only used if you specified variable: 'p0_only' as 0 
                      pct_tree_gain_thresholds: [3, 5, 10]}          ;example syntax: [15,18,20] - only used if you specified variable: 'p0_only' as 0

;---done with inputs----------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------------------               
pdf_size_params = {width:width, height:height, columns:columns, rows:rows, display_index_min:display_index_min, $
  display_index_max:display_index_max}

base = landtrendrv04_multiple_points(point_coord_file, path, ppprrr, run_params,$
  post_process_params, calibration_params, image_info_savefile, p0_only=p0_only, file_coords=file_coords, only_create_pdf)

create_seg_point_mode_pdf_summary, path, pdf_size_params, base, include_p_and_f=include_p_and_f


