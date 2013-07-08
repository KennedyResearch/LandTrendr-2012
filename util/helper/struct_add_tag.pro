function struct_add_tag, orig_str, new_tag, new_data


; take an existing structure array, and add a new tag
;   to that structure, filled with the new data.

tn = tag_names(orig_str)
num_tags = n_elements(tn)
test = matchstr(tn, new_tag, /total_match)
if test[0] ne -1 then begin
   print, 'Error in add_tag'
   print, 'new_tag exists in orig_str'
   stop
   end


;check to make sure that the entire
;   new_data and the number of elements of
;   the structure are equivalent

  n_s = n_elements(orig_str)
  n_nd = n_elements(new_data)
  if n_s ne n_nd then begin
    print, 'Error in add_tag'
    print, 'new_data not of same dimension as'
    print, '   structure'
    print, 'new_data:' +string(n_nd)
    print, 'orig_str:' +string(n_s)
    stop
  end

;make a copy of a single element of the orig_str, to assign values
;   later
  single = orig_str[0]



;now make up a new structure
  str_string = 'new_st = {'

  for i = 0, (num_tags-1) do $
      str_string = strcompress(str_string+tn[i]+':(single).('+string(i)+'),', /remove_all)

  ;add the new one
  str_string = strcompress(str_string+new_tag+':new_data[0]', /remove_all)

  str_string = str_string+'}'

  ok = execute(str_string)
  if ok eq 0 then begin
     print, 'Error in struct_add_tag:'
     print, 'problem creating the structure'
     print, 'Here is the command attempted'
     print, str_string
     stop
  end

;make the appropriate size vector of the structure

  ret = replicate(new_st, n_s)
  for i = 0, (num_tags-1) do $
     ret.(i) = orig_str.(i)
  ret.(num_tags+1-1) = new_data

return, ret
end

