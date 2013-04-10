pro create_polygons_from_patchid_imgs, path

  ;find the path to the arcmap install
  spawn, "echo %arcgishome%", installlocation
  
  dmt = '"'+stringswap(installlocation+"ArcToolbox\Toolboxes\Data Management Tools.tbx", "\","/")+'"'
  ct = '"'+stringswap(installlocation+"ArcToolbox\Toolboxes\Conversion Tools.tbx", "\","/")+'"'
  at = '"'+stringswap(installlocation+"ArcToolbox\Toolboxes\Analysis Tools.tbx", "\","/")+'"'
  
  ;find patchid images
  patchid_file = file_search(path+"\outputs\", "*patchid.bsq*", count=n_patchid_file)
  
  for i=0, n_patchid_file-1 do begin
    print, "converting raster patchid file: ", strcompress(string(i+1)+"/"+string(n_patchid_file), /rem), " to polygon"
    file = stringswap(patchid_file[i], "\", "/")
    
    ;make a directory to hold the shapefiles
    outdir = file_dirname(file)+"\patchid_shapefiles\"
    file_mkdir, outdir
    
    ;create an intermediate polygon output filename to be deleted
    temppolygon = stringswap(outdir+"temp_polygon1.shp", "\", "/")
    temppolygon1 = stringswap(outdir+"temp_polygon2.shp", "\", "/")
    
    ;create a final polygon output filename
    namebase = file_basename(patchid_file[i], ".bsq")
    ;outname = stringswap(namebase, "bsq", "shp")
    tempfullname = stringswap(outdir+"temp_polygon3.shp", "\", "/")
    kmzout = stringswap(tempfullname, "temp_polygon3.shp", "temp_polygon3.kmz") 
    ;fulloutname = stringswap(outdir+outname, "\", "/")
    
    ;put together the string lines for the .py script
    line1 = "import sys, string, os, arcgisscripting"
    line2 = "gp = arcgisscripting.create()"
    line3 = 'gp.AddToolbox('+dmt+')'
    line4 = 'gp.AddToolbox('+ct+')'
    line5 = 'gp.AddToolbox('+at+')'
    line6 = 'gp.CheckOutExtension("3D")'
    line7 = 'gp.RasterToPolygon_conversion('+'"'+file+'"'+', '+'"'+temppolygon+'"'+', "NO_SIMPLIFY", "Value")'
    line8 = 'gp.Select_analysis('+'"'+temppolygon+'"'+', '+'"'+temppolygon1+'"'+', "\"GRIDCODE\" > 0")'
    ;line8 = 'gp.DeleteFeatures_management('+'"'+temppolygon1+'"'+')'
    line9 = 'gp.Dissolve_management('+'"'+temppolygon1+'"'+', '+'"'+tempfullname+'"'+', "GRIDCODE", "", "MULTI_PART", "DISSOLVE_LINES")'
    line10 = 'gp.LayerToKML_conversion('+'"'+tempfullname+'"'+', '+'"'+kmzout+'"'+', "1", "false", "DEFAULT", "1024", "96")'
    
    script = transpose([line1,line2,line3,line4,line5,line6,line7,line8,line9,line10])
    
    ;write out the script file
    py_script_file = stringswap(outdir+file_basename(patchid_file[i]), ".bsq", "_polygonize.py")
    openw, fun, py_script_file, /get_lun
    printf, fun, script
    free_lun, fun
    
    ;run the script file
    cmd = "C:\python25\python "+'"'+py_script_file+'"'
    spawn, cmd, /hide
    
    ;rename the final polygon, the file name size has a limitation when running through the arcgisscripting geoprocessor so need to name after it is created
    temppolygon = file_search(outdir, "*temp_polygon3*")
    outnamebase = namebase
    finaloutnames = stringswap(temppolygon, "temp_polygon3", namebase)
    file_move, temppolygon, finaloutnames
    
    ;clean up temp files
    temp_files = file_search(outdir, "*temp_polygon*", count=n_temp_files)
    if n_temp_files ge 1 then file_delete, temp_files 
    file_delete, py_script_file
  endfor
end