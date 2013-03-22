pro create_seg_point_mode_pdf_summary, path, pdf_size_params, basename, include_p_and_f=include_p_and_f

  ;find the src and vert files from point mode run
  src_file_all = file_search(path+"outputs\lt_point_mode\", basename+"*src.csv", count=n_src_file)
  vert_file_all = file_search(path+"outputs\lt_point_mode\", basename+"*vertinfo.csv", count=n_vert_file)
  
  ;make sure that the number of files for each match
  if n_src_file ne n_vert_file then message, "the number of point mode source files does not match the number of vert files - check this"
  
  ;sort them so that they are in the same order when calling them by index
  src_file_all = stringswap(src_file_all[uniq(src_file_all, sort(src_file_all))], "\", "/")
  vert_file_all = stringswap(vert_file_all[uniq(vert_file_all, sort(vert_file_all))], "\", "/")
  
  ;get the size of and arragement of the to-be-output pdf
  width = string(pdf_size_params.width)  ;'14'  ; a pdf with trejectories is created, what should the pdf width be (inches) ?
  height = string(pdf_size_params.height)  ;'12'  ; a pdf with trejectories is created, what should the pdf height be (inches) ?
  columns = string(pdf_size_params.columns)  ;'4'  ; a pdf with trejectories is created, how many columns of figures do you want per page - note that plot text can be lost with too many
  rows = string(pdf_size_params.rows)  ;'4'
  ymin = pdf_size_params.display_index_min
  ymax = pdf_size_params.display_index_max
  y_range = string(ymin)+","+string(ymax)
  
  
  for i=0, n_src_file-1 do begin
    print, ""
    print, "creating lt point mode trajectory pdf summary for: ", file_basename(src_file_all[i]) 
    
    ;subset the src and vert files and change the slash direction
    src_file = src_file_all[i]
    src_file = stringswap(src_file[uniq(src_file, sort(src_file))], "\", "/")
    vert_file = vert_file_all[i]
    vert_file = stringswap(vert_file[uniq(vert_file, sort(vert_file))], "\", "/")
    
    ;figure out the index
    split = strsplit(file_basename(src_file), "_", /extract)
    index = strcompress(split[1], /rem)
    
    ;figure out the output directory
    outdir = file_dirname(src_file)+"\"
    outdir = stringswap(outdir, "\", "/")
    
    ;create a name for the pdf summary file
    pos = strpos(file_basename(src_file), "_", /reverse_search)+1
    pdfout = '"'+strmid(file_basename(src_file),0,pos)+"fit_summary.pdf"+'"'
    
    ;set the y range based on the index
    if ymin eq 0 and ymax eq 0 then begin
      case index of
        'nbr': y_range = '0,900'
        'brightness': y_range = '0,300'
        'greenness': y_range = '0,300'
        'wetness': y_range = '-300,300'
        'TCangle': y_range = '-1000,1000'
        'NDVI': y_range = '-1000,1000'
        'Band1': y_range = '-2000,2000'
        'Band2': y_range = '-2000,2000'
        'Band3': y_range = '-2000,2000'
        'Band4': y_range = '-2000,2000'
        'Band5': y_range = '-2000,2000'
        'Band7': y_range = '-2000,2000'
      endcase
    endif
    
    ;create the function
    lines = 'plotLT = function(src_file, vert_file, index, outdir, ...) {'
    lines = [lines,'src_names = c("pointname", "year", "val")']
    lines = [lines,'src = read.table(src_file, header=F, sep=",")']
    lines = [lines,'names(src) = src_names']
    lines = [lines,'vert = read.table(vert_file, header=T, sep=",")']
    lines = [lines,'vert_x_names = paste("VERTYEAR", 1:8, sep="")']
    lines = [lines,'vert_y_names = paste("VERTVAL", 1:8, sep="")']
    lines = [lines,'plots = unique(src$pointname)']
    lines = [lines,'out_file = paste(outdir, '+pdfout+', sep="")']  ;     "lt_segments_", index, ".pdf"
    lines = [lines,'pdf(file=out_file, width='+width+', height='+height+')']
    lines = [lines,'par(mfrow=c('+columns+','+rows+'))']
    lines = [lines,'for (p in plots) {']
    lines = [lines,'this_src = src[src$pointname==p,]']
    lines = [lines,'src_x = this_src[,"year"]']
    lines = [lines,'src_y = this_src[,"val"]']
    lines = [lines,'all_fits = vert[vert$POINT_NAME==p,]']
    lines = [lines,'runs = unique(all_fits$RUNIDX)']
    lines = [lines,'for (run in runs) {']
    lines = [lines,'this_x = all_fits[all_fits$RUNIDX==run,vert_x_names]']
    lines = [lines,'this_y = all_fits[all_fits$RUNIDX==run,vert_y_names]']
    lines = [lines,'vic = this_x > 0']
    lines = [lines,'this_x = this_x[vic]']
    lines = [lines,'this_y = this_y[vic]']
    if keyword_set(include_p_and_f) eq 1 then begin
      ;lines = [lines,'this_f = all_fits[all_fits$RUNIDX==run,"F_STAT"]']
      lines = [lines,'this_p = all_fits[all_fits$RUNIDX==run,"P_OF_F"]']
      lines = [lines,'plot(src_x, src_y, xlab="Year", ylab=index, type="n", ..., main=paste(index, ", Plot: ", p, ", Run: ", run, ", P: ", this_p, sep=""))']
    endif else lines = [lines,'plot(src_x, src_y, xlab="Year", ylab=index, type="n", ..., main=paste(index, ", Plot: ", p, ", Run: ", run, sep=""))']
    lines = [lines,'points(src_x, src_y, col=4, cex=2 )']
    lines = [lines,'lines(this_x, this_y, col=rgb(1,0,0), lwd=3)']
    lines = [lines,'}']
    lines = [lines,'}']
    lines = [lines,'dev.off()']
    lines = [lines,'}']
    lines = [lines,'src_file = '+'"'+src_file+'"']
    lines = [lines,'vert_file = '+'"'+vert_file+'"']
    lines = [lines,'index = '+'"'+index+'"']
    lines = [lines,'outdir = '+'"'+outdir+'"']
    lines = [lines,'plotLT(src_file, vert_file, index, outdir, ylim=c('+y_range+'))']
    
    lines = transpose(lines)
    
    ;write out the script file
    r_script_file = file_dirname(src_file)+"\"+strmid(file_basename(src_file),0,pos)+"pdf_summary_script.r"
    openw, fun, r_script_file, /get_lun
    printf, fun, lines
    free_lun, fun
    
    cmd = "rscript "+'"'+r_script_file+'"'
    ;todo - test to see if rscript is working
    spawn, cmd, result, /hide
    if result[0] eq '' then message, "the pdf summary was not created, make sure you do not have another copy currently open and run again" else $
      print, "done creating lt point mode trajectory pdf summary for: ", file_basename(src_file_all[i])
  endfor
end


