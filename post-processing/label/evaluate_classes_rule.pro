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

;
; V1 works for two class rule with one placeholder
;
; need to fix when the matched segment is last segment and there is a placeholder after it.
; need to fix when the matched segment is first segment and there is a placeholder before it.
;

;
; V2 works for two class rule with one placeholder
;
; fixed when the matched segment is last segment and there is a placeholder after it.
;
;
; need to fix when the matched segment is first segment and there is a placeholder before it.
;

;
; V3 works for two class rule with one placeholder
;
; fixed when the matched segment is last segment and there is a placeholder after it.
;
;
; fixed when the matched segment is first segment and there is a placeholder before it.
;

;
; V4 works for two class rule with one placeholder
;
; fixed type pointer error

;
; V5 works for two class rule with one placeholder
;
; collapsing by angle is applied first before filtering.
; change filtering rule and add stable class.
; it is also intended to add a stable category.



function evaluate_classes_rule, interpd_class_codes, yr_traj, vv_traj, mag_traj, filter_params, merge_recovery, use_relative_mag=use_relative_mag
  ;mag_traj and dur_traj are redundent, as they will be recalculated based on
  ;yr_traj and vv_traj after filtering.

  if n_elements(use_relative_mag) eq 0 then use_relative_mag = 1

  ;convert mag_traj to long to avoid data overflow
  input_mag_traj = mag_traj
  
  ;determin scale factor
  dist_scale = 1
  picked = where(mag_traj ne 0, n)
  if n gt 0 then dist_scale = (vv_traj[picked[0]+1]-vv_traj[picked[0]])/(mag_traj[picked[0]])

  
  ;covert to cover model, and by default use relative magnitude
  ;TODO: SHOULD CHECK THE LOGIC OF HANDLING FILTERED SEGMENTS: MAGNITUDE may not be set as 0.
  cover_filtered = lt_filter_bycover(yr_traj, vv_traj, filter_params, use_relative_mag=use_relative_mag)
  
  if cover_filtered.ok ne 1 then begin
    print, "Error applying filtering rules."
    return, {ok:0}
  endif
  
  ;get filtered values for later use
  vv_traj = cover_filtered.original_vertvals
  yr_traj = cover_filtered.original_vertices
  mag_traj = long(cover_filtered.magcover) * (-1) ;dist_scale
  
  ;calcuate dur_traj
  dur_traj = shift(yr_traj, -1) - yr_traj
  dur_traj = dur_traj * (yr_traj gt 0)
  dur_traj = dur_traj[0:n_elements(dur_traj)-2]
  
  ;convert to cover, somewhat duplicate in landtrendrv04_filter_by_cover
  static_model = filter_params.static_model
  if static_model eq '' or static_model eq 'none' then begin
    print, "At least static model function is needed to convert to cover."
    return, {ok:-1}
  endif
  command = 'precov_traj = fix(' + static_model + '(vv_traj))'
  ok = execute(command)
  precov_traj = precov_traj * (dur_traj gt 0)
  

  
  
  types = *(interpd_class_codes.type)
  year_rule = *(interpd_class_codes.year_rule)
  years = *(interpd_class_codes.year)
  dur_rule = *(interpd_class_codes.duration_rule)
  durs = *(interpd_class_codes.duration)
  precov_rule = *(interpd_class_codes.precover_rule)
  precovs = *(interpd_class_codes.precover)
  
    
  ;evaluate whether the duration is appropriate
  ;the first rule is always the focus
  case 1 of
      (dur_rule[0] eq 'L'): begin
        vic = where(dur_traj gt durs[0], n)
        if n gt 0 then begin
          mag_traj[vic] = 0
          dur_traj[vic] = 0
        endif
      end
      (dur_rule[0] eq 'G'): begin
        vic = where(dur_traj le durs[0], n)
        if n gt 0 then begin 
          mag_traj[vic] = 0
          dur_traj[vic] = 0
        endif      
      end
      (dur_rule[0] eq 'X'): dummy = 1
      else: message, "this will never happen"
  endcase
  
  ;now go through the segments and see
  ;  which ones are the XX type (skipped) that will
  ;  not be written out to files
  
  skips = where(types eq 'XX', n_skips)
  n_types = n_elements(types)  ;this is the number of things needed to evaluate for this class.
  
  overall_match = bytarr(n_types)
  ;outvals = intarr(4*(n_types-n_skips))    ;for year, mag, and duration for each type that is not a spacer
  outvals = intarr(4*n_types)
  
  valid_year = where(yr_traj gt 0, n_segs)
  n_segs = n_segs-1 ;how many segments in the trajectories?
  
  type_counter = 0   ;keep track of which type rule we're evaluating
  type_pointer = 0   ;this will point to the output file layers -- same as type_counter, except this one won't count spacers
  
  ;first, need to determine all of the disturbance and recovery
  ;   segments, and note the largest disturbance and recovery
  
  dists = where(mag_traj gt 0, n_dists) ;1s for disturbance, 0s for stable and recovery
  if n_dists gt 0 then begin
    largest_dist = dists[(where(mag_traj[dists] eq max(mag_traj[dists]), many))[0]]
    recent_dist = dists[(where(yr_traj[dists] eq max(yr_traj[dists]), many))[0]]
    longest_dist = dists[(where(dur_traj[dists] eq max(dur_traj[dists]), many))[0]]
    
    ;second-greatest disturbance
    if n_dists ge 2 then begin
      mag_order = reverse(sort(mag_traj[dists]))
      secondlargest_dist = dists[mag_order[1]]
    endif else begin
      secondlargest_dist = [-1]
    endelse
  endif else begin
    largest_dist = [-1]
    recent_dist = [-1]
    longest_dist = [-1]
    secondlargest_dist = [-1]
  endelse
  ;largest_dist=dists[largest_dist[0]]
  
  
  recs  = where(mag_traj lt 0, n_recs) ;1s for disturbance, 0s for stable and recovery
  if n_recs gt 0 then begin
    largest_rec = recs[(where(mag_traj[recs] eq min(mag_traj[recs]), many))[0]]
    recent_rec = recs[(where(yr_traj[recs] eq max(yr_traj[recs]), many))[0]]
    longest_rec = recs[(where(dur_traj[recs] eq max(dur_traj[recs]), many))[0]]
    if n_recs ge 2 then begin
      mag_order = sort(mag_traj[recs])
      secondlargest_rec = recs[mag_order[1]]
    endif else begin
      secondlargest_rec = [-1]
    endelse
  ;largest_rec = recs[largest_rec[0]] ;this points to the index of the largest recovery
  endif else begin
    largest_rec = [-1]
    recent_rec = [-1]
    longest_rec = [-1]
    secondlargest_rec = [-1]
  endelse
  
  
  test = bytarr(4)  ;a flag to indicate that this segment matches the evaluation criterion: year, flag, duration, precover
  for i = 0, n_segs-1 do begin
    ;evaluate whether the year is appropriate
    if years[type_counter] ne -1 then begin
      case 1 of
        (year_rule[type_counter] eq 'EQ'):  test[0] = (yr_traj[i] eq years[type_counter])
        (year_rule[type_counter] eq 'LE'):  test[0] = (yr_traj[i] le years[type_counter])
        (year_rule[type_counter] eq 'GE'):  test[0] = (yr_traj[i] ge years[type_counter])
        else:   test[0] = 1    ;doesn't matter when the segment starts (type 4 in the year rule)
      endcase
    end else test[0] = 1        ;if user set the years to 0000, then it's a match regardless.
    
    ;evaluate whether the type is appropriate, and if so,
    ;   if the magnitude is greater than the threshold
    case 1 of
      (types[type_counter] eq 'FD'):  begin  ;first disturbance
        test[1] = (mag_traj[i] gt 0)    ;disturbance, as it is filtered already
        ;we need to handle the special case where the prior type was a
        ;  a spacer -- since we allow any arbitrary amount of space between
        ;   disturbance and recovery.  Thus, if disturbance is this target type,
        ;   but this segment isn't one but the type before was a spacer, then we can
        ;   still keep looking.
        if type_counter gt 1 and not(test[1])  then begin   ;this has to be the third type (type_counter = 2 or more)
          if types[type_counter-1] eq 'XX' then begin
            type_counter=type_counter-1 ;reset the type counter so that when it's incremented again we correctly evaluate for recovery in the next round
            test[1]=1       ;set this to 1 so we don't give up yet.
          endif
        endif
      end
      
      (types[type_counter] eq 'RD'):  begin  ;recent disturbance
        test[1] = (mag_traj[i] gt 0) * (i eq recent_dist)   ;disturbance
      ;we don't need to worry about skipped types, because this
      ;  largest disturbance criterion is unique, doesn't matter what
      ;  happened before and after.
      end
      
      (types[type_counter] eq 'GD'):  begin  ;largest disturbance
        test[1] = (mag_traj[i] gt 0) * (i eq largest_dist)   ;disturbance
      ;we don't need to worry about skipped types, because this
      ;  largest disturbance criterion is unique, doesn't matter what
      ;  happened before.
      end
      
      (types[type_counter] eq 'SD'):  begin  ;second largest disturbance
        test[1] = (mag_traj[i] gt 0) * (i eq secondlargest_dist)   ;disturbance
      ;we don't need to worry about skipped types, because this
      ;  largest disturbance criterion is unique, doesn't matter what
      ;  happened before.
      end
      
      (types[type_counter] eq 'LD'):  begin  ;longest disturbance
        test[1] = (mag_traj[i] gt 0) * (i eq longest_dist)   ;disturbance
      ;we don't need to worry about skipped types, because this
      ;  largest disturbance criterion is unique, doesn't matter what
      ;  happened before.
      end

      (types[type_counter] eq 'ST'):   begin ;stable
        test[1] = (mag_traj[i] eq 0)    ;stable
        ;we need to handle the special case where the prior type was a
        ;  a spacer -- since we allow any arbitrary amount of space between
        ;   disturbance and recovery.  Thus, if recovery is this target type,
        ;   but this segment isn't one but the type before was a spacer, then we can
        ;   still keep looking.
        if type_counter gt 1 and not(test[1])  then $   ;this has to be the third type (type_counter = 2 or more)
          if types[type_counter-1] eq 'XX' then begin
          type_counter=type_counter-1 ;reset the type counter so that when it's incremented again we correctly evaluate for recovery in the next round
          test[1]=1       ;set this to 1 so we don't give up yet.
        end
      end
      
      (types[type_counter] eq 'FR'):   begin ;first recovery
        test[1] = (mag_traj[i] lt 0)    ;recovery
        ;we need to handle the special case where the prior type was a
        ;  a spacer -- since we allow any arbitrary amount of space between
        ;   disturbance and recovery.  Thus, if recovery is this target type,
        ;   but this segment isn't one but the type before was a spacer, then we can
        ;   still keep looking.
        if type_counter gt 1 and not(test[1])  then $   ;this has to be the third type (type_counter = 2 or more)
          if types[type_counter-1] eq 'XX' then begin
          type_counter=type_counter-1 ;reset the type counter so that when it's incremented again we correctly evaluate for recovery in the next round
          test[1]=1       ;set this to 1 so we don't give up yet.
        end
      end
      
      (types[type_counter] eq 'RR'):  begin ;recent recovery
        test[1] = (mag_traj[i] lt 0) * (i eq recent_rec)  ;recovery
      end
      
      (types[type_counter] eq 'GR'):  begin ;largest recovery
        test[1] = (mag_traj[i] lt 0) * (i eq largest_rec)  ;recovery
      end
      
      (types[type_counter] eq 'SR'):  begin ;largest recovery
        test[1] = (mag_traj[i] lt 0) * (i eq secondlargest_rec)  ;recovery
      end
      
      (types[type_counter] eq 'LR'):  begin ;largest recovery
        test[1] = (mag_traj[i] lt 0) * (i eq longest_rec)  ;recovery
      end
      
      (types[type_counter] eq 'XX'):  test[1] = 1   ;anything will pass
      ;       begin     ;it has to not match the ones before or after it
      ;                                      if types[type_counter-1] eq 1 then prior_test = (mag_traj[i] le 0) $ ;if prior rule was disturbance, make sure this one is not
      ;                                                                   else prior_test = (mag_traj[i] ge 0)  ;if prior rule was recovery, make sure this isnot
      ;                                      if types[type_counter+1] eq 1 then after_test = (mag_traj[i] le 0) $ ;if prior rule was disturbance, make sure this one is not
      ;                                                                   else after_test = (mag_traj[i] ge 0)  ;if prior rule was recovery, make sure this isnot
      ;                                      test[1] = prior_test * after_test   ;only a spacer if it meets both rules
      ;                                   end
      else: return, {ok:0, message:"type not found"}
    endcase
    
    ;evaluate whether the duration is appropriate
    case 1 of
      (dur_rule[type_counter] eq 'L'): test[2] = (dur_traj[i] le durs[type_counter])
      (dur_rule[type_counter] eq 'G'): test[2] = (dur_traj[i] gt durs[type_counter])
      (dur_rule[type_counter] eq 'X'): test[2] = 1
    endcase
    
    ;        if dur_rule[type_counter] eq 'L' then test[2] = (dur_traj[i] le durs[type_counter]) else $ ;if zero, then look for duration le dur
    ;            test[2] = (dur_traj[i] gt durs[type_counter])
    if types[type_counter] eq 'XX' then test[2] = 1 ;if this is a spacer, we don't evaluate for duration, so set to 1
    
    
    
    case 1 of
      (precov_rule[type_counter] eq 'L'): test[3] = (precov_traj[i] le precovs[type_counter])
      (precov_rule[type_counter] eq 'G'): test[3] = (precov_traj[i] gt precovs[type_counter])
      (precov_rule[type_counter] eq 'X'): test[3] = 1
    endcase
    ;        if precov_rule[type_counter] eq 0 then test[3] = (precov_traj[i] le precovs[type_counter]) else $ ;if zero, then look for duration le dur
    ;            test[3] = (precov_traj[i] gt precovs[type_counter])
    if types[type_counter] eq 'XX' then test[3] = 1 ;if this is a spacer, we don't evaluate for duration, so set to 1
    
    
    ;evaluate whether the magnitude of this segment meets the criterion
    ;does this segment meet all of the criteria?
    isamatch = total(test) eq 4
    if isamatch then begin
      overall_match[type_counter] = 1
      
      ;this is a match of first segment with the second rule followed by a place holder.
      if i eq 0 and type_pointer eq 1 then outvals[0:3] = [0,0,0,0]
      
      outvals[(type_pointer*4)+0:(type_pointer*4)+3] = [yr_traj[i], mag_traj[i], dur_traj[i], vv_traj[i]]
      
      if i eq 0 and type_pointer eq 0 and types[type_pointer] eq 'XX' then i = i - 1
      
      type_pointer = type_pointer + 1
      ;if types[type_counter] ne 'XX' then begin ;if this is not a spacer, store values for writing out.
      ;outvals[(type_pointer*4)+0:(type_pointer*4)+3] = [yr_traj[i], mag_traj[i], dur_traj[i], vv_traj[i]]
      ;    type_pointer = type_pointer+1 ;only increment if this is not a spacer
      ;end
      type_counter = type_counter + 1 ;always increment, even if spacer, because we need to evaluate all
      
      if type_counter eq n_types then goto, getout  ;because we're zero-based
    end  else begin
      if type_counter ne 0 then begin    ;if this is not a match, and we had a match on the prior
        ;segment (indicated by type_counter not being 0), then reset things to see if we pick up the
        ;pattern from the beginning again.  To do this, we need to reset
        ;the type pointer  and type counter, and decrement the segment counter
        ;  so this one can be evaluated again
        type_counter = 0
        outvals[*]=0
        type_pointer = 0
        if i gt 0 and types[0] eq 'XX' then i=i-1
      end   ;end resetting in case this is not a match but prior was
    ;if this is not a match, and the type_counter is 0, then we can keep looking. no need to change anything
    end
    test[*]=0
  end   ;i   segments
  
  getout:
  ;we get here if we run out of types or segments.
  used_type = types ne 'XX'
  comp_match = used_type eq overall_match
  big_match = (total(comp_match) eq n_elements(types)) or (total(overall_match) eq n_types) ;we matched on every count
  return, {ok:1, match:big_match, class_num:big_match * interpd_class_codes.class_num, $
    outvals:outvals*big_match}
    
  return, out_struct
end



