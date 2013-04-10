pro landtrendr_gui_event, event

  widget_control, event.top, get_uvalue=infoptr
  info = *infoptr
  
  widget_control, event.id, get_uvalue=selected
  common landtrendr, status
  case selected of
    ;runfile
    'runfile': begin
      filename = dialog_pickfile()
      if filename eq '' then status.path = 'No LandTrendr Run File Selected' else status.path = filename
      if status.path ne 'No LandTrendr Run File Selected' then widget_control, info.file, set_value=file_basename(filename) $
      else widget_control, info.file, set_value=status.path
    end
    ;ref img file
    'refimgbrowse': begin
      filename = dialog_pickfile()
      if filename eq '' then status.refimg = 'No Reference Image Selected' else status.refimg = filename
      if status.refimg ne 'No Reference Image Selected' then widget_control, info.refimgfile, set_value=file_basename(filename) $
      else widget_control, info.refimgfile, set_value=status.refimg
    end
    
    ;check boxes
    'grp1runall': if status.grp1runall eq 0 then status.grp1runall = 1 else status.grp1runall = 0
    'grp3runall': if status.grp3runall eq 0 then status.grp3runall = 1 else status.grp3runall = 0
    'grp4runall': if status.grp4runall eq 0 then status.grp4runall = 1 else status.grp4runall = 0
    'grp6runall': if status.grp6runall eq 0 then status.grp6runall = 1 else status.grp6runall = 0
    'progrp1boxa': if status.progrp1boxa eq 0 then status.progrp1boxa = 1 else status.progrp1boxa = 0
    'progrp1boxb': if status.progrp1boxb eq 0 then status.progrp1boxb = 1 else status.progrp1boxb = 0
    'progrp1boxc': if status.progrp1boxc eq 0 then status.progrp1boxc = 1 else status.progrp1boxc = 0
    'progrp1boxd': if status.progrp1boxd eq 0 then status.progrp1boxd = 1 else status.progrp1boxd = 0
    'progrp1boxe': if status.progrp1boxe eq 0 then status.progrp1boxe = 1 else status.progrp1boxe = 0
    'progrp1boxf': if status.progrp1boxf eq 0 then status.progrp1boxf = 1 else status.progrp1boxf = 0
    'progrp1boxg': if status.progrp1boxg eq 0 then status.progrp1boxg = 1 else status.progrp1boxg = 0
    'progrp2boxa': if status.progrp2boxa eq 0 then status.progrp2boxa = 1 else status.progrp2boxa = 0
    'progrp3boxa': if status.progrp3boxa eq 0 then status.progrp3boxa = 1 else status.progrp3boxa = 0
    'progrp3boxb': if status.progrp3boxb eq 0 then status.progrp3boxb = 1 else status.progrp3boxb = 0
    'progrp4boxa': if status.progrp4boxa eq 0 then status.progrp4boxa = 1 else status.progrp4boxa = 0
    'progrp4boxb': if status.progrp4boxb eq 0 then status.progrp4boxb = 1 else status.progrp4boxb = 0
    'progrp5boxa': if status.progrp5boxa eq 0 then status.progrp5boxa = 1 else status.progrp5boxa = 0
    'progrp6boxa': if status.progrp6boxa eq 0 then status.progrp6boxa = 1 else status.progrp6boxa = 0
    'progrp6boxb': if status.progrp6boxb eq 0 then status.progrp6boxb = 1 else status.progrp6boxb = 0
    'progrp6boxc': if status.progrp6boxc eq 0 then status.progrp6boxc = 1 else status.progrp6boxc = 0
    'progrp6boxd': if status.progrp6boxd eq 0 then status.progrp6boxd = 1 else status.progrp6boxd = 0
    'progrp7boxa': if status.progrp7boxa eq 0 then status.progrp7boxa = 1 else status.progrp7boxa = 0
    'progrp7boxb': if status.progrp7boxb eq 0 then status.progrp7boxb = 1 else status.progrp7boxb = 0
    'progrp7boxc': if status.progrp7boxc eq 0 then status.progrp7boxc = 1 else status.progrp7boxc = 0
    'progrp7boxd': if status.progrp7boxd eq 0 then status.progrp7boxd = 1 else status.progrp7boxd = 0
    'progrp7boxe': if status.progrp7boxe eq 0 then status.progrp7boxe = 1 else status.progrp7boxe = 0
    'progrp7boxf': if status.progrp7boxf eq 0 then status.progrp7boxf = 1 else status.progrp7boxf = 0
    'progrp7boxg': if status.progrp7boxg eq 0 then status.progrp7boxg = 1 else status.progrp7boxg = 0
    'progrp7boxh': if status.progrp7boxh eq 0 then status.progrp7boxh = 1 else status.progrp7boxh = 0
    ;    'fixthesetext':
    ;    'deletethesetext':
    'runthething': begin
      widget_control, info.file, get_value=selectedfile
      if selectedfile eq 'No LandTrendr Run File Selected' then ok = dialog_message('No LandTrendr Run File Was Selected.') else begin
        ;check if create ref was selected - if so check that a path was selected
        if status.progrp2boxa eq 1 then begin
          widget_control, info.refimgfile, get_value=refimgpath
          if refimgpath eq 'No Reference Image Selected' then begin
          ok = dialog_message('No Reference Image Was Selected.')
          break
          endif
        endif
        
        widget_control, info.fixthese, get_value=fixem
        status.fixthese=fixem
        widget_control, info.deletethese, get_value=deletem
        status.deletethese=deletem
        
        widget_control, info.refimgfile, get_value=refimgpath
        
        widget_control, event.top, /destroy
        package_stuff_from_gui, status
      endelse
    end
    ;help buttons
    'landtrendr1': lt_gui_help_menu, 'landtrendr1'
    'landtrendr2': lt_gui_help_menu, 'landtrendr2'
    'landtrendr3': lt_gui_help_menu, 'landtrendr3'
    'landtrendr4': lt_gui_help_menu, 'landtrendr4'
    'aboutgrp1': lt_gui_help_menu, 'aboutgrp1'
    'aboutgrp2': lt_gui_help_menu, 'aboutgrp2'
    'aboutgrp3': lt_gui_help_menu, 'aboutgrp3'
    'aboutgrp4': lt_gui_help_menu, 'aboutgrp4'
    'aboutgrp5': lt_gui_help_menu, 'aboutgrp5'
    'aboutgrp6': lt_gui_help_menu, 'aboutgrp6'
    'aboutgrp7': lt_gui_help_menu, 'aboutgrp7'
    'progrp1helpa': lt_gui_help_menu, 'progrp1helpa'
    'progrp1helpb': lt_gui_help_menu, 'progrp1helpb'
    'progrp1helpc': lt_gui_help_menu, 'progrp1helpc'
    'progrp1helpd': lt_gui_help_menu, 'progrp1helpd'
    'progrp1helpe': lt_gui_help_menu, 'progrp1helpe'
    'progrp1helpf': lt_gui_help_menu, 'progrp1helpf'
    'progrp1helpg': lt_gui_help_menu, 'progrp1helpg'
    'progrp2helpa': lt_gui_help_menu, 'progrp2helpa'
    'progrp3helpa': lt_gui_help_menu, 'progrp3helpa'
    'progrp3helpb': lt_gui_help_menu, 'progrp3helpb'
    'progrp4helpa': lt_gui_help_menu, 'progrp4helpa'
    'progrp4helpb': lt_gui_help_menu, 'progrp4helpb'
    'progrp5helpa': lt_gui_help_menu, 'progrp5helpa'
    'progrp6helpa': lt_gui_help_menu, 'progrp6helpa'
    'progrp6helpb': lt_gui_help_menu, 'progrp6helpb'
    'progrp6helpc': lt_gui_help_menu, 'progrp6helpc'
    'progrp6helpd': lt_gui_help_menu, 'progrp6helpd'
    'progrp7helpa': lt_gui_help_menu, 'progrp7helpa'
    'progrp7helpb': lt_gui_help_menu, 'progrp7helpb'
    'progrp7helpc': lt_gui_help_menu, 'progrp7helpc'
    'progrp7helpd': lt_gui_help_menu, 'progrp7helpd'
    'progrp7helpe': lt_gui_help_menu, 'progrp7helpe'
    'progrp7helpf': lt_gui_help_menu, 'progrp7helpf'
    'progrp7helpg': lt_gui_help_menu, 'progrp7helpg'
    'progrp7helph': lt_gui_help_menu, 'progrp7helph'
    'fixthesehelp': lt_gui_help_menu, 'fixthesehelp'
    'deletethesehelp': lt_gui_help_menu, 'deletethesehelp'
  endcase
;  print, "runfile: ",status.path
;  print, "grp1runall: ",status.grp1runall
;  print, "grp3runall: ",status.grp3runall
;  print, "grp4runall: ",status.grp4runall
;  print, "grp6runall: ",status.grp6runall
;  print, "progrp1boxa: ",status.progrp1boxa
;  print, "progrp1boxb: ",status.progrp1boxb
;  print, "progrp1boxc: ",status.progrp1boxc
;  print, "progrp1boxd: ",status.progrp1boxd
;  print, "progrp1boxe: ",status.progrp1boxe
;  print, "progrp1boxf: ",status.progrp1boxf
;  print, "progrp1boxg: ",status.progrp1boxg
;  print, "progrp2boxa: ",status.progrp2boxa
;  print, "progrp3boxa: ",status.progrp3boxa
;  print, "progrp3boxb: ",status.progrp3boxb
;  print, "progrp4boxa: ",status.progrp4boxa
;  print, "progrp4boxb: ",status.progrp4boxb
;  print, "progrp5boxa: ",status.progrp5boxa
;  print, "progrp6boxa: ",status.progrp6boxa
;  print, "progrp6boxb: ",status.progrp6boxb
;  print, "progrp6boxc: ",status.progrp6boxc
;  print, "progrp6boxd: ",status.progrp6boxd
;  print, "progrp7boxa: ",status.progrp7boxa
;  print, "progrp7boxb: ",status.progrp7boxb
;  print, "progrp7boxc: ",status.progrp7boxc
;  print, "progrp7boxd: ",status.progrp7boxd
;  print, "progrp7boxe: ",status.progrp7boxe
;  print, "progrp7boxf: ",status.progrp7boxf
;  print, "progrp7boxg: ",status.progrp7boxg
;  print, "progrp7boxh: ",status.progrp7boxh
;  print, "fixthese: ",status.fixthese
;  print, "deletethese: ",status.deletethese
end

pro landtrendr_gui, info
  common landtrendr, status
  status = {path:'No LandTrendr Run File Selected' ,$
    refimg:'No Reference Image Selected' ,$
    grp1runall:0 ,$
    grp3runall:0 ,$
    grp4runall:0 ,$
    grp6runall:0 ,$
    progrp1boxa:0 ,$
    progrp1boxb:0 ,$
    progrp1boxc:0 ,$
    progrp1boxd:0 ,$
    progrp1boxe:0 ,$
    progrp1boxf:0 ,$
    progrp1boxg:0 ,$
    progrp2boxa:0 ,$
    progrp3boxa:0 ,$
    progrp3boxb:0 ,$
    progrp4boxa:0 ,$
    progrp4boxb:0 ,$
    progrp5boxa:0 ,$
    progrp6boxa:0 ,$
    progrp6boxb:0 ,$
    progrp6boxc:0 ,$
    progrp6boxd:0 ,$
    progrp7boxa:0 ,$
    progrp7boxb:0 ,$
    progrp7boxc:0 ,$
    progrp7boxd:0 ,$
    progrp7boxe:0 ,$
    progrp7boxf:0 ,$
    progrp7boxg:0 ,$
    progrp7boxh:0 ,$
    fixthese:'Enter Dates to Fix',$
    deletethese:'Enter Dates to Delete' }
    
  frame = 583
  base = widget_base(column=1, mbar=mbar, title='LandTrendr Processor')  ;, /scroll, x_scroll_size=20, y_scroll_size=20
  topstuff = widget_base(base, column=1, xsize=frame, /frame, /align_center) ;, /frame, xpad=10
  fbase = widget_base(topstuff, column=3, /base_align_center) ;, /grid_layout
  
  landtrendr1 = widget_button(fbase, value='What is LandTrendr (LT)?', uvalue='landtrendr1')  ;
  landtrendr2 = widget_button(fbase, value='About LT Processing', uvalue='landtrendr2')  ;
  ;landtrendr3 = widget_button(fbase, value='About LT Segmentation', uvalue='landtrendr3')  ;, /align_right
  landtrendr4 = widget_button(fbase, value='About LT Landscape History Outputs', uvalue='landtrendr4')  ;, /align_right
  
  runfileb = widget_base(topstuff, column=1)
  runfile = widget_button(runfileb, value='Browse for LandTrendr Run File', uvalue='runfile', xsize=572, ysize=40)
  file = widget_text(topstuff, value='No LandTrendr Run File Selected', uvalue='file')
  
  buttsize = 40
  ;xsize = 300
  
  base1 = widget_base(base, column=2)
  left = widget_base(base1, column=1)
  right = widget_base(base1, column=1)
  
  ;process group 1
  aboutsize = 200
  checkxs = 200
  aligner = 225
  
  grp1 = widget_base(left, column=1, /frame)
  grp1label = widget_base(grp1, column=2)
  grp1labelbtn = widget_base(grp1label, column=1)
  grp1allbox = widget_base(grp1label, column=1, /nonexclusive)
  
  progrp1baselabel = widget_button(grp1labelbtn, xsize=aboutsize, value='About Procedure group 1', uvalue='aboutgrp1')
  grp1runall = widget_button(grp1allbox, value='Run all', xsize=buttsize, uvalue='grp1runall', /align_right)
  
  
  progrp1base = widget_base(grp1, column=2)
  
  progrp1checkbox = widget_base(progrp1base, column=1,/nonexclusive, xsize=aligner) ;
  progrp1help = widget_base(progrp1base, column=1)
  
  ;process group 1 checkbox stuff
  progrp1boxa = widget_button(progrp1checkbox, value='VCT Prepare GLOVIS Images', uvalue='progrp1boxa')
  progrp1boxb = widget_button(progrp1checkbox, value='Screen Images', uvalue='progrp1boxb')
  progrp1boxc = widget_button(progrp1checkbox, value='Prepare DEM for VCT', uvalue='progrp1boxc')
  progrp1boxd = widget_button(progrp1checkbox, value='Prepare Landcover for VCT', uvalue='progrp1boxd')
  progrp1boxe = widget_button(progrp1checkbox, value='Run VCT for Masks', uvalue='progrp1boxe')
  progrp1boxf = widget_button(progrp1checkbox, value='LT Prepare GLOVIS Images', uvalue='progrp1boxf')
  progrp1boxg = widget_button(progrp1checkbox, value='Convert VCT Masks to LT Masks', uvalue='progrp1boxg')
  
  ;process group 1 help stuff
  progrp1helpa = widget_button(progrp1help, value='Help', xsize=buttsize, uvalue='progrp1helpa')
  progrp1helpb = widget_button(progrp1help, value='Help', xsize=buttsize, uvalue='progrp1helpb')
  progrp1helpc = widget_button(progrp1help, value='Help', xsize=buttsize, uvalue='progrp1helpc')
  progrp1helpd = widget_button(progrp1help, value='Help', xsize=buttsize, uvalue='progrp1helpd')
  progrp1helpe = widget_button(progrp1help, value='Help', xsize=buttsize, uvalue='progrp1helpe')
  progrp1helpf = widget_button(progrp1help, value='Help', xsize=buttsize, uvalue='progrp1helpf')
  progrp1helpg = widget_button(progrp1help, value='Help', xsize=buttsize, uvalue='progrp1helpg')
  
  ;process group 2
  grp2 = widget_base(left, column=1, /frame)
  grp2label = widget_base(grp2, column=2)
  grp2labelbtn = widget_base(grp2label, column=1)
  grp2allbox = widget_base(grp2label, column=1, /nonexclusive)
  progrp2baselabel = widget_button(grp2labelbtn, xsize=aboutsize, value='About Procedure Group 2', uvalue='aboutgrp2')
  ;grp2about = widget_button(grp2allbox, value='Run all', xsize=buttsize, uvalue='grp2runall', /align_right)
  progrp2base = widget_base(grp2, column=2)
  progrp2base1 = widget_base(grp2, column=2)
  
  progrp2checkbox = widget_base(progrp2base, column=1,/nonexclusive, xsize=aligner)
  progrp2help = widget_base(progrp2base, column=1)
  
  ;process group 2 checkbox stuff
  progrp2boxa = widget_button(progrp2checkbox, value='Create a Reference Image', uvalue='progrp2boxa')
  ;process group 2 help stuff
  progrp2helpa = widget_button(progrp2help, value='Help', xsize=buttsize, uvalue='progrp2helpa')
  ;browse for reference image
  refimgbase = widget_base(progrp2base1, column=1)
  refimgbase1 = widget_base(progrp2base1, column=1)
  refimgfile = widget_text(refimgbase, value='No Reference Image Selected', uvalue='refimgfile',xsize=33) ;ysize=40 ,xsize=200
  refimgbrowse = widget_button(refimgbase1, value='Browse', xsize=51, uvalue='refimgbrowse')
  
  ;refimgtext = widget_button(refimgbase, value='Reference Image', uvalue='refimgtext', xsize=200)
  ;file = widget_text(topstuff, value='No LandTrendr Run File Selected', uvalue='file')
  
  ;process group 3
  grp3 = widget_base(left, column=1, /frame)
  grp3label = widget_base(grp3, column=2)
  grp3labelbtn = widget_base(grp3label, column=1)
  grp3allbox = widget_base(grp3label, column=1, /nonexclusive)
  progrp3baselabel = widget_button(grp3labelbtn, xsize=aboutsize, value='About Procedure Group 3', uvalue='aboutgrp3')
  grp3runall = widget_button(grp3allbox, value='Run all', xsize=buttsize, uvalue='grp3runall', /align_right)
  progrp3base = widget_base(grp3, column=2)
  
  ;progrp3base = widget_base(base1, column=2, /frame)
  progrp3checkbox = widget_base(progrp3base, column=1,/nonexclusive, xsize=aligner)
  progrp3help = widget_base(progrp3base, column=1)
  
  ;process group 3 checkbox stuff
  progrp3boxa = widget_button(progrp3checkbox, value='Fix Cloudmask(s)', uvalue='progrp3boxa')
  progrp3boxb = widget_button(progrp3checkbox, value='Radiometrically Normalize Images', uvalue='progrp3boxb')
  ;process group 3 help stuff
  progrp3helpa = widget_button(progrp3help, value='Help', xsize=buttsize, uvalue='progrp3helpa')
  progrp3helpb = widget_button(progrp3help, value='Help', xsize=buttsize, uvalue='progrp3helpb')
  
  ;process group 4
  grp4 = widget_base(left, column=1, /frame)
  grp4label = widget_base(grp4, column=2)
  grp4labelbtn = widget_base(grp4label, column=1)
  grp4allbox = widget_base(grp4label, column=1, /nonexclusive)
  progrp4baselabel = widget_button(grp4labelbtn, xsize=aboutsize, value='About Procedure Group 4', uvalue='aboutgrp4')
  grp4about = widget_button(grp4allbox, value='Run all', xsize=buttsize, uvalue='grp4runall', /align_right)
  progrp4base = widget_base(grp4, column=2)
  
  progrp4checkbox = widget_base(progrp4base, column=1,/nonexclusive, xsize=aligner)
  progrp4help = widget_base(progrp4base, column=1)
  
  ;process group 4 checkbox stuff
  progrp4boxa = widget_button(progrp4checkbox, value='Create Tasseled Cap Transformation', uvalue='progrp4boxa')
  progrp4boxb = widget_button(progrp4checkbox, value='Create ImageSync Image Lists', uvalue='progrp4boxb')
  ;process group 4 help stuff
  progrp4helpa = widget_button(progrp4help, value='Help', xsize=buttsize, uvalue='progrp4helpa')
  progrp4helpb = widget_button(progrp4help, value='Help', xsize=buttsize, uvalue='progrp4helpb')
  
  ;process group 5
  grp5 = widget_base(left, column=1, /frame)
  grp5label = widget_base(grp5, column=2)
  grp5labelbtn = widget_base(grp5label, column=1)
  grp5allbox = widget_base(grp5label, column=1, /nonexclusive)
  progrp5baselabel = widget_button(grp5labelbtn, xsize=aboutsize, value='About Procedure Group 5', uvalue='aboutgrp5')
  ;grp5about = widget_button(grp5allbox, value='Run all', xsize=buttsize, uvalue='grp5runall', /align_right)
  progrp5base = widget_base(grp5, column=2)
  
  progrp5checkbox = widget_base(progrp5base, column=1,/nonexclusive, xsize=aligner)
  progrp5help = widget_base(progrp5base, column=1)
  
  ;process group 5 checkbox stuff
  progrp5boxa = widget_button(progrp5checkbox, value='Evaluation Segmentation', uvalue='progrp5boxa')
  ;process group 5 help stuff
  progrp5helpa = widget_button(progrp5help, value='Help', xsize=buttsize, uvalue='progrp5helpa')
  
  ;process group 6
  grp6 = widget_base(right, column=1, /frame)
  grp6label = widget_base(grp6, column=2)
  grp6labelbtn = widget_base(grp6label, column=1)
  grp6allbox = widget_base(grp6label, column=1, /nonexclusive)
  progrp6baselabel = widget_button(grp6labelbtn, xsize=aboutsize, value='About Procedure Group 6', uvalue='aboutgrp6')
  grp6about = widget_button(grp6allbox, value='Run all', xsize=buttsize, uvalue='grp6runall', /align_right)
  progrp6base = widget_base(grp6, column=2)
  
  progrp6checkbox = widget_base(progrp6base, column=1,/nonexclusive, xsize=aligner)
  progrp6help = widget_base(progrp6base, column=1)
  
  ;process group 6 checkbox stuff
  progrp6boxa = widget_button(progrp6checkbox, value='Regular Segmentation', uvalue='progrp6boxa')
  progrp6boxb = widget_button(progrp6checkbox, value='Fit to Vertices', uvalue='progrp6boxb')
  progrp6boxc = widget_button(progrp6checkbox, value='Segmentation Classification', uvalue='progrp6boxc')
  progrp6boxd = widget_button(progrp6checkbox, value='Patch Aggregation and MMU Filtering', uvalue='progrp6boxd')
  
  ;process group 6 help stuff
  progrp6helpa = widget_button(progrp6help, value='Help', xsize=buttsize, uvalue='progrp6helpa')
  progrp6helpb = widget_button(progrp6help, value='Help', xsize=buttsize, uvalue='progrp6helpb')
  progrp6helpc = widget_button(progrp6help, value='Help', xsize=buttsize, uvalue='progrp6helpc')
  progrp6helpd = widget_button(progrp6help, value='Help', xsize=buttsize, uvalue='progrp6helpd')
  
  ;misc
  grp7 = widget_base(right, column=1, /frame)
  grp7label = widget_base(grp7, column=2)
  grp7labelbtn = widget_base(grp7label, column=1)
  grp7allbox = widget_base(grp7label, column=1, /nonexclusive)
  progrp7baselabel = widget_button(grp7labelbtn, xsize=aboutsize, value='About Miscellaneous', uvalue='aboutgrp7')
  ;grp7about = widget_button(grp7allbox, value='Run all', xsize=buttsize, uvalue='grp6runall', /align_right)
  progrp7base = widget_base(grp7, column=2)
  
  progrp7checkbox = widget_base(progrp7base, column=1,/nonexclusive, xsize=aligner)
  progrp7help = widget_base(progrp7base, column=1)
  
  ;misc checkbox stuff
  progrp7boxa = widget_button(progrp7checkbox, value='Make Image Info .sav file', uvalue='progrp7boxa')
  progrp7boxb = widget_button(progrp7checkbox, value='Make Yearly Composites', uvalue='progrp7boxb')
  progrp7boxc = widget_button(progrp7checkbox, value='Convert 1.0 to 1.5 .hdrs', uvalue='progrp7boxc')
  progrp7boxd = widget_button(progrp7checkbox, value='Repopulate Image Info .sav file', uvalue='progrp7boxd')
  progrp7boxe = widget_button(progrp7checkbox, value='Print Image Info', uvalue='progrp7boxe')
  progrp7boxf = widget_button(progrp7checkbox, value='Convert Flat .hds to Projected', uvalue='progrp7boxf')
  progrp7boxg = widget_button(progrp7checkbox, value='Delete Image Date(s) Completetly', uvalue='progrp7boxg')
  progrp7boxh = widget_button(progrp7checkbox, value='Add Years to Fitted and Source Outputs', uvalue='progrp7boxh')
  
  ;misc help stuff
  progrp7helpa = widget_button(progrp7help, value='Help', xsize=buttsize, uvalue='progrp7helpa')
  progrp7helpb = widget_button(progrp7help, value='Help', xsize=buttsize, uvalue='progrp7helpb')
  progrp7helpc = widget_button(progrp7help, value='Help', xsize=buttsize, uvalue='progrp7helpc')
  progrp7helpd = widget_button(progrp7help, value='Help', xsize=buttsize, uvalue='progrp7helpd')
  progrp7helpe = widget_button(progrp7help, value='Help', xsize=buttsize, uvalue='progrp7helpe')
  progrp7helpf = widget_button(progrp7help, value='Help', xsize=buttsize, uvalue='progrp7helpf')
  progrp7helpg = widget_button(progrp7help, value='Help', xsize=buttsize, uvalue='progrp7helpg')
  progrp7helph = widget_button(progrp7help, value='Help', xsize=buttsize, uvalue='progrp7helph')
  
  ;fix these
  ;base setup
  fixesbase = widget_base(right, row=2, /frame)
  fixesbase1 = widget_base(fixesbase, column=1)
  fixthesebase = widget_base(fixesbase1, column=2, xsize=xsize)
  fixthesebase1 = widget_base(fixthesebase, column=1, xsize=aligner-3)
  fixthesebase2 = widget_base(fixthesebase, column=1)
  
  fixesbase2 = widget_base(fixesbase, column=1)
  deletethesebase = widget_base(fixesbase2, column=2)
  deletethesebase1 = widget_base(deletethesebase, column=1, xsize=aligner-3)
  deletethesebase2 = widget_base(deletethesebase, column=1)
  
  fixthesetext = widget_text(fixthesebase1, /editable, value=status.fixthese, uvalue='fixthesetext')
  fixthesehelp = widget_button(fixthesebase2, value='Help', xsize=buttsize, uvalue='fixthesehelp')
  
  deletethesetext = widget_text(deletethesebase1, /editable, value=status.deletethese, uvalue='deletethesetext')
  deletethesehelp = widget_button(deletethesebase2, value='Help', xsize=buttsize, uvalue='deletethesehelp')
  
  
  ;start button
  startbase = widget_base(right, row=2, /frame)
  watch = widget_text(startbase, value='After hitting the run button, occassionally check the IDL console for progam '+ $
    'status and instructions.' , /wrap, xsize=45, ysize=3)
  runthething = widget_button(startbase, value='Run LandTrendr Procedure(s)', uvalue='runthething', xsize=280, ysize=76)
  
  
  widget_control, base, /realize
  
  info = {file:file, fixthese:fixthesetext, deletethese:deletethesetext, base:base, refimgfile:refimgfile}
  infoptr = ptr_new(info)
  widget_control, base, set_uvalue=infoptr
  
  xmanager, 'landtrendr_gui', base, /no_block
  
  
end

;    progrp1boxa
;    progrp1boxb
;    progrp1boxc
;    progrp1boxd
;    progrp1boxe
;    progrp1boxf
;    progrp1boxg
;    progrp2boxa
;    progrp3boxa
;    progrp3boxb
;    progrp4boxa
;    progrp4boxb
;    progrp5boxa
;    progrp6boxa
;    progrp6boxb
;    progrp6boxc
;    progrp6boxd
;    progrp7boxa
;    progrp7boxb
;    progrp7boxc
;    progrp7boxd
;    progrp7boxe
;    progrp7boxf
;    progrp7boxg
;    progrp7boxh