pro screenimg_gui_event, event

  widget_control, event.top, get_uvalue=infoptr
  info = *infoptr
  
  widget_control, event.id, get_uvalue=selected
  common yeargroup, yeargroup
  
  grp = yeargroup[0].grpindex
  case selected of
    'b1': if yeargroup[grp].doit[0] eq 0 then yeargroup[grp].doit[0] = 1 else yeargroup[grp].doit[0] = 0
    'b2': if yeargroup[grp].doit[1] eq 0 then yeargroup[grp].doit[1] = 1 else yeargroup[grp].doit[1] = 0
    'b3': if yeargroup[grp].doit[2] eq 0 then yeargroup[grp].doit[2] = 1 else yeargroup[grp].doit[2] = 0
    'b4': if yeargroup[grp].doit[3] eq 0 then yeargroup[grp].doit[3] = 1 else yeargroup[grp].doit[3] = 0
    'b5': if yeargroup[grp].doit[4] eq 0 then yeargroup[grp].doit[4] = 1 else yeargroup[grp].doit[4] = 0
    'b6': if yeargroup[grp].doit[5] eq 0 then yeargroup[grp].doit[5] = 1 else yeargroup[grp].doit[5] = 0
    'b7': if yeargroup[grp].doit[6] eq 0 then yeargroup[grp].doit[6] = 1 else yeargroup[grp].doit[6] = 0
    'b8': if yeargroup[grp].doit[7] eq 0 then yeargroup[grp].doit[7] = 1 else yeargroup[grp].doit[7] = 0
    'b9': if yeargroup[grp].doit[8] eq 0 then yeargroup[grp].doit[8] = 1 else yeargroup[grp].doit[8] = 0
    'b10': if yeargroup[grp].doit[9] eq 0 then yeargroup[grp].doit[9] = 1 else yeargroup[grp].doit[9] = 0
    'b11': if yeargroup[grp].doit[10] eq 0 then yeargroup[grp].doit[10] = 1 else yeargroup[grp].doit[10] = 0
    'b12': if yeargroup[grp].doit[11] eq 0 then yeargroup[grp].doit[11] = 1 else yeargroup[grp].doit[11] = 0
    'nextyear': begin
      dontdoit = where(yeargroup[grp].doit eq 0, n_dontdoit, ncomplement=complement)
      if complement eq 0 then begin
        check = dialog_message("No images have been selected.  Is this intended?", /question)
        if check eq "No" then break
      endif
      if n_dontdoit ge 1 then begin
        yearday = yeargroup[grp].yearday[dontdoit]
        goods = where(yearday ne "NA", n_goods)
        if n_goods ge 1 then begin
          goods = yearday[goods]
          goodsm = "   -"+goods
          tellit = transpose(["The following dates will be deleted.  Is this okay?","",goodsm])
          tellitok = dialog_message(tellit, /question)
          yearday = strcompress(goods, /rem)
          if tellitok eq 'Yes' then begin
            filestodelete = strarr(n_goods)
            for j=0, n_goods-1 do begin
              file = file_search(info.path+"glovis_targz\", strcompress("*"+yearday[j]+"*", /rem), count=n_files)
              if n_files eq 1 then filestodelete[j] = file else message, "glovis file cannot be found for this date: "+yearday 
            endfor
            deletenote = file_basename(filestodelete)+" because it was not selected for processing"
            outfile = info.path+"documentation\"+info.ppprrr+"_images_not_processed.txt"
            if file_exists(outfile) eq 1 then begin
              ;read in the file and append the new info
              openr, lun, outfile, /get_lun
              file_size = file_lines(outfile,/noexpand_path)
              file_list = strarr(file_size)
              readf, lun, file_list
              free_lun, lun
              fulltext = transpose([file_list,deletenote])
              n_lines = n_elements(fulltext)
              if n_lines gt 1 then begin
                unique = fulltext[1:*]
                unique = unique[uniq(unique, sort(unique))]
                fulltext = [fulltext[0],unique]
              endif
              openw, lun,outfile,/get_lun
              printf, lun,fulltext
              free_lun, lun
            endif else message, "images not selected for processing .txt list does not exist"
            delete_lt_files, info.path, info.ppprrr, yearday, /nowarning
          endif else break ; /nowarning
        endif
      endif
      
      ;display the new img grp
      if (n_elements(yeargroup)-1) lt grp+1 then begin
        widget_control, event.top, /destroy
        widget_control, info.leader, /destroy
        break
      endif
      grp = grp+1
      year = strtrim(strmid(yeargroup[grp].yearday[0],0,4),2)
      index =  where(strmatch(info.tifs, "*"+year+"*") eq 1, n_match)
      if n_match eq 1 then displayit = info.tifs[index] else message, ">>> could not find 321rgb image quicklooks"
      
      widget_control, info.display, get_value=display_v
      wset,display_v
      img = read_image(displayit)
      loadct,0 ;load a color table
      TVImage,reverse(img,3),true=1
      
      
      
      widget_control, info.b1, set_value=yeargroup[grp].yearday[0], set_button=0
      widget_control, info.b2, set_value=yeargroup[grp].yearday[1], set_button=0
      widget_control, info.b3, set_value=yeargroup[grp].yearday[2], set_button=0
      widget_control, info.b4, set_value=yeargroup[grp].yearday[3], set_button=0
      widget_control, info.b5, set_value=yeargroup[grp].yearday[4], set_button=0
      widget_control, info.b6, set_value=yeargroup[grp].yearday[5], set_button=0
      widget_control, info.b7, set_value=yeargroup[grp].yearday[6], set_button=0
      widget_control, info.b8, set_value=yeargroup[grp].yearday[7], set_button=0
      widget_control, info.b9, set_value=yeargroup[grp].yearday[8], set_button=0
      widget_control, info.b10, set_value=yeargroup[grp].yearday[9], set_button=0
      widget_control, info.b11, set_value=yeargroup[grp].yearday[10], set_button=0
      widget_control, info.b12, set_value=yeargroup[grp].yearday[11], set_button=0
      year = strtrim(strmid(yeargroup[grp].yearday[0],0,4),2)
      widget_control, info.nextyear, set_value="Finished with Year: "+year
      yeargroup.grpindex = grp
    end
  endcase
;print, yeargroup[grp].doit
  
  
end

pro screenimg_gui, path, ppprrr
  ;find the 321img list
  tiffilesav = file_search(path+"images\321rgb_quicklooks\", "*.sav", count=n_tiffilesav)
  if n_tiffilesav eq 1 then restore, tiffilesav
  
  common yeargroup, yeargroup
  
  leader=widget_base(map=0)
  widget_control, leader, /Realize
  base = widget_base(column=1, group_leader=leader, /modal, title='LandTrendr Screen Images')
  display = widget_draw(base, xsize=1260, ysize=945, x_scroll_size=1000, y_scroll_size=650)
  
  buttonbase = widget_base(base, column=3)
  buttonbase3 = widget_base(buttonbase, column=1)
  buttonbase1 = widget_base(buttonbase, column=1)
  buttonbase2 = widget_base(buttonbase, column=1)
  buttonrow1b = widget_base(buttonbase1, row=1, /nonexclusive)
  buttonrow2b = widget_base(buttonbase1, row=1, /nonexclusive)
  buttonrow3b = widget_base(buttonbase1, row=1, /nonexclusive)
  
  whattodo = widget_text(buttonbase3, ysize=5, xsize=70 ,/wrap, value="Please select images to process, those that are not selected" $
    +" will be deleted.  The position of the image date check boxes correspond to the position of the images" $
    +" in the viewer.  When you're finised selecting images for a given year, hit the 'Finished with Year:' button")
    
  xsize=65
  
  if yeargroup[0].yearday[0] ne 'NAa' then b1 = widget_button(buttonrow1b, xsize=xsize, value=yeargroup[0].yearday[0], frame=4, uvalue='b1')
  if yeargroup[0].yearday[1] ne 'NAa' then b2 = widget_button(buttonrow1b, xsize=xsize, value=yeargroup[0].yearday[1], uvalue='b2')
  if yeargroup[0].yearday[2] ne 'NAa' then b3 = widget_button(buttonrow1b, xsize=xsize, value=yeargroup[0].yearday[2], uvalue='b3')
  if yeargroup[0].yearday[3] ne 'NAa' then b4 = widget_button(buttonrow1b, xsize=xsize, value=yeargroup[0].yearday[3], uvalue='b4')
  
  if yeargroup[0].yearday[4] ne 'NAa' then b5 = widget_button(buttonrow2b, xsize=xsize, value=yeargroup[0].yearday[4], uvalue='b5')
  if yeargroup[0].yearday[5] ne 'NAa' then b6 = widget_button(buttonrow2b, xsize=xsize, value=yeargroup[0].yearday[5], uvalue='b6')
  if yeargroup[0].yearday[6] ne 'NAa' then b7 = widget_button(buttonrow2b, xsize=xsize, value=yeargroup[0].yearday[6], uvalue='b7')
  if yeargroup[0].yearday[7] ne 'NAa' then b8 = widget_button(buttonrow2b, xsize=xsize, value=yeargroup[0].yearday[7], uvalue='b8')
  
  if yeargroup[0].yearday[8] ne 'NAa' then b9 = widget_button(buttonrow3b, xsize=xsize, value=yeargroup[0].yearday[8], uvalue='b9')
  if yeargroup[0].yearday[9] ne 'NAa' then b10 = widget_button(buttonrow3b, xsize=xsize, value=yeargroup[0].yearday[9], uvalue='b10')
  if yeargroup[0].yearday[10] ne 'NAa' then b11 = widget_button(buttonrow3b, xsize=xsize, value=yeargroup[0].yearday[10], uvalue='b11')
  if yeargroup[0].yearday[11] ne 'NAa' then b12 = widget_button(buttonrow3b, xsize=xsize, value=yeargroup[0].yearday[11], uvalue='b12')
  
  year = strtrim(strmid(yeargroup[0].yearday[0],0,4),2)
  
  otherstuffb = widget_base(buttonbase2, row=1)
  nextyear = widget_button(otherstuffb, value="Finished with Year: "+year, uvalue='nextyear', xsize=200, ysize=80)
  
  widget_control, base, /realize
  
  tifs = file_search(path+"images\321rgb_quicklooks\", "*.tif", count=n_tifs)
  index =  where(strmatch(tifs, "*"+year+"*") eq 1, n_match)
  if n_match eq 1 then displayit = tifs[index] else message, ">>> could not find 321rgb image quicklooks"
  
  widget_control, display, get_value=display_v
  wset,display_v
  img = read_image(displayit)
  loadct,0 ;load a color table
  TVImage,reverse(img,3),true=1
  
  info = {b1:b1,b2:b2,b3:b3,b4:b4,b5:b5,b6:b6,b7:b7,b8:b8,b9:b9,b10:b10,b11:b11,b12:b12, $
    nextyear:nextyear, ppprrr:ppprrr, path:path, display:display, tifs:tifs, year:year, leader:leader}
  infoptr = ptr_new(info)
  widget_control, base, set_uvalue=infoptr
  
  xmanager, 'screenimg_gui', base, /no_block
  
end





