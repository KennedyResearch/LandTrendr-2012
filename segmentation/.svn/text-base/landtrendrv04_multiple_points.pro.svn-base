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

function landtrendrv04_multiple_points, point_coord_file, path, ppprrr, $
    run_params, post_process_params, calibration_params, image_info_savefile, p0_only=p0_only,$
    file_coords=file_coords, only_create_pdf
  
  if file_test(only_create_pdf) eq 1 then begin
    base = file_basename(only_create_pdf)
    pos = strpos(base, "landtrendr")-1
    newbase = strmid(base, 0, pos)
    return, newbase
  endif  
  
  ;goal:  plot landtrendr fits for all files in a given .csv file, where
  ;   the .csv has three columns:  plot, x, y coord
    
  ;create an output base name
  output_dir = path+"\outputs\lt_point_mode\"
  file_mkdir, output_dir
  
  ;first read the point coord file
  out = read_generic_csv(point_coord_file)
  n_suspicious = n_elements(out.suspicious)
  ;if n_suspicious ne 0 then begin
  ;print, 'WARNING:  Plot_multiple_plots found some suspicious plots in the csv file'
  ;print, 'Confirm that there are supposed to be '+string(n_elements(out.vals))
  ;print, 'plots.   If so, then probably ignorable.
  ;print, 'Press "Y" to keep going, or any other key to stop'
  ;a=''
  ;a = get_kbrd(1)
  ;if strupcase(a) ne "Y" then stop
  ;end
  
  labels = string(out.vals.plotid)
  xcoords = out.vals.x
  ycoords = out.vals.y
  
  n_points = n_elements(labels)
  
  time = timestamp()
  
  for i = 0, n_points-1 do begin
    point_coords = [[xcoords[i], ycoords[i]]]
    point_name = labels[i]
    this_run_params = run_params
    
    print, "fitting point: " + string(point_name)
    ok = landtrendrv04_point_run(this_run_params, post_process_params,$
      point_coords, point_name, calibration_params, file_coords=file_coords)
      
    ;write out the vertices
    lt_file = output_dir + ppprrr + "_" + run_params.index + "_" + time + '_landtrendr_point_mode_vertinfo.csv'
    p1_file = output_dir + ppprrr + "_" + run_params.index + "_"  + time + '_landtrendr_point_mode_vertinfo_p1.csv'
    p2_file = output_dir + ppprrr + "_" + run_params.index + "_"  + time + '_landtrendr_point_mode_vertinfo_p2.csv'
    
    lt_idx_file = output_dir + ppprrr + "_" + run_params.index + "_" + time +  '_landtrendr_point_mode_runinfo.csv'
    p1_idx_file = output_dir + ppprrr + "_" + run_params.index + "_" + time +  '_landtrendr_point_mode_runinfo_p1.csv'
    p2_idx_file = output_dir + ppprrr + "_" + run_params.index + "_" + time +  '_landtrendr_point_mode_runinfo_p2.csv'
    
    if i eq 0 then begin
      export_structure_to_file, ok.vertinfo, lt_file
      export_structure_to_file, ok.lt_params, lt_idx_file
      
      if not keyword_set(p0_only) then begin
        export_structure_to_file, ok.p1_vertinfo, p1_file
        export_structure_to_file, ok.p2_vertinfo, p2_file
        export_structure_to_file, ok.p1_params, p1_idx_file
        export_structure_to_file, ok.p2_params, p2_idx_file
      endif
    endif else begin
      export_structure_to_file, ok.vertinfo, lt_file, /APPEND, /NOHEADER
      if not keyword_set(p0_only) then begin
        export_structure_to_file, ok.p1_vertinfo, p1_file, /APPEND, /NOHEADER
        export_structure_to_file, ok.p2_vertinfo, p2_file, /APPEND, /NOHEADER
      endif
    endelse
  end
  
  ;return, {ok:1}
  
  restore, image_info_savefile ;to get the image_info to pass along
  ;out = read_generic_csv(point_coord_file)
  
  ;labels = string(out.vals.plotid)
  ;xcoords = out.vals.x
  ;ycoords = out.vals.y
  
  ;n_points = n_elements(labels)
  
  this_file = output_dir + ppprrr + "_" + run_params.index + "_" + time + '_landtrendr_point_mode_src.csv'
  format='(3(I,","))'
  openw, fun, this_file , /get_lun
  
  for i = 0, n_points-1 do begin
    print, "extracting source data for point:" + string(labels[i])
    ok = lt_stack_read(image_info, run_params.index, [xcoords[i], ycoords[i]], labels[i])
    pname = replicate(labels[i], n_elements(ok.x))
    vals = transpose([[pname], [ok.x], [ok.y]])
    printf, fun, vals, format=format
  endfor
  
  free_lun, fun
  
  newbase = ppprrr + "_" + run_params.index + "_" + time
  return, newbase
end
