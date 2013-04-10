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

pro envi_glovis_to_landtrendr1_junk, inpath, outpath=outpath, overwrite=overwrite, proj=proj, skipthermal=skipthermal, pixelSize=pixelSize, from_auto=from_auto

  if keyword_set(from_auto) eq 1 then begin
    ;unpack the structure
    path = from_auto.path
    proj = from_auto.proj
    file_cleanup = from_auto.file_cleanup
    decompress = from_auto.decompress
    ;decompress the .gz files
    if keyword_set(decompress) eq 1 then begin
      print, "decompressing .gz glovis files"
      print, "  ..."
      drive = strmid(path, 0, 2)
      length = strlen(path)
      pdrive = strmid(path, 2, length-2)
      file_mkdir, strcompress(drive+pdrive+"glovis_targz\tar", /rem)
      cmd = strcompress(pdrive + "glovis_targz\7za", /rem)+ " e " + $  ;"VCT\prep\7za"
        strcompress(pdrive+"glovis_targz\", /rem) + " " + strcompress("-o"+drive+pdrive+"glovis_targz\tar", /rem)
      cmd = drive+ " && " + cmd
      spawn, cmd
      
      ;decompress the .tar files
      print, "decompressing .tar glovis files"
      print, "  ..."
      targz_path = strcompress(path+"glovis_targz\tar\", /rem)
      imglist = file_search(targz_path, "*tar")
      for i=0, n_elements(imglist)-1 do begin
        file = imglist[i]
        len = strlen(file)
        outdir = strmid(file, 0, len-4)
        infile = strmid(file, 2, len-2)
        file_mkdir, outdir
        cmd = strcompress(pdrive + "glovis_targz\7za", /rem)+ " e " + $  ;"VCT\prep\7za"
          infile + " " + strcompress("-o"+outdir, /rem)
        cmd = drive+ " && " + cmd
        spawn, cmd
      endfor
    endif
    ;create auto inpaths and outpaths so the user does not have to move the files or rename folders
    inpath = strcompress(path+"glovis_targz\tar\", /rem)
    outpath = strcompress(path+'images\', /rem)
  endif
  
  
  
  ; ask for inpath and outpath
  if n_elements(inpath) eq 0 then inpath=dialog_pickfile(title='Select archive directory',/directory, /must_exist)
  if inpath eq "" then return
  if n_elements(outpath) eq 0 then outpath=dialog_pickfile(path=file_dirname(inpath), title='Select output directory',/directory, /must_exist)
  if outpath eq "" then return
  
  ; make sure path ends with '/'
  if keyword_set(from_auto) eq 0 then begin
    inpath = file_dirname(inpath,/mark_directory)+file_basename(inpath)+'/'
    outpath = file_dirname(outpath,/mark_directory)+file_basename(outpath)+'/'
  endif
  
  ; get archive directories
  dirnames = FILE_SEARCH(inpath+'L*', /fold_case, /test_directory,  /mark_directory)
  if dirnames[0] eq "" then begin
    ok = dialog_message('No GLOVIS archives found in this directory', /error, /center)
    return
  endif
  
  ; set up progressbar
  ; loop through archives
  n_archives = n_elements(dirnames)
  envi_report_init, ['Input dir = '+inpath,'Output dir : '+outpath], base=base, /interrupt, title='Import GLOVIS to LandTrendr'
  envi_report_inc, base, n_archives
  for i=0, n_archives-1 do begin
    envi_report_stat, base, i, n_archives, cancel=cancel
    if cancel then begin
      envi_report_init, base=base, /finish
      return
    endif
    ok = envi_glovis2landtrendr_doit(dirnames[i], outpath,overwrite=overwrite, $
      proj=proj, skipThermal=skipThermal,pixelSize=pixelSize, from_auto=from_auto)
    if ok.ok eq 1 then begin
      print, ok.message
      print, ok.warnings
      ;clean up all files except the glovis .tar.gz files
      if keyword_set(file_cleanup) eq 1 then file_delete, dirnames[i]
    endif else print, '>> '+ok.error
  endfor
  
  envi_report_init, base=base, /finish
  
END