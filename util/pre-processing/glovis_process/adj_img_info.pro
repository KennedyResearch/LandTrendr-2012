pro adj_img_info, image_list, image_list_type, image_info_savefile

  restore, image_info_savefile
  
  image_list = strcompress(string(image_list), /rem)
  year = string(image_info.year)
  day = string(image_info.julday)
  yearday = strcompress(year+day, /rem)
  goods = 0
  for j=0, n_elements(yearday)-1 do begin
    check = where(image_list eq yearday[j], match)
    goods = [goods,match]
  endfor
  goods = goods[1:*]
  if image_list_type eq 1 then keep = where(goods eq 0)
  if image_list_type eq 2 then keep = where(goods eq 1)
  if image_list_type ne 1 and image_list_type ne 2 then message, "img_list_type must be set to 1 or 2"
  image_info = image_info[keep]
  save, image_info, filename = image_info_savefile

end