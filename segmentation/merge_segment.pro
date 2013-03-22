
function merge_segment, flags
  cflag = flags[0]
  
  vert_idx = [0]
  for i=1,n_elements(flags)-1 do begin
    ; keep non-filtered 
    if flags[i] eq 0 then vert_idx = [vert_idx, i]
    
    ; if exclude same value 
    if flags[i] ne cflag and flags[i] ne 0 then begin
      vert_idx = [vert_idx, i]
      cflag = flags[i]
    endif
    
    ; always include the last one
    if i eq n_elements(flags)-1 and flags[i] ne 0 then vert_idx = [vert_idx, i]
  endfor

  return, vert_idx
end
