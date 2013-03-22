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


function landtrendrv04_output_mapping, best_vertices, best_vertvals, point_name, point_coords, f_stat, p_of_f
  ;set up for writing out vertyears
  vertinfo = {point_name:"", point_xcoord:0.d, point_ycoord:0., $
    n_segments:0, vertyear1:0, vertyear2:0, vertyear3:0, vertyear4:0, $
    vertyear5:0, vertyear6:0, vertyear7:0, vertyear8:0,vertval1:0, $
    vertval2:0, vertval3:0, vertval4:0, vertval5:0, $
    vertval6:0, vertval7:0,vertval8:0, segmag1:0, $
    segmag2:0, segmag3:0, segmag4:0, segmag5:0, segmag6:0, segmag7:0, f_stat:0.1, p_of_f:0.1}
  
  vidx = where(best_vertices gt 0, max_verts)
  if max_verts eq 0 then return, vertinfo

  vertvals = best_vertvals[vidx]
  mags = vertvals[1:max_verts-1] - vertvals[0:max_verts-2]
 
  vertinfo.point_name = point_name
  vertinfo.point_xcoord = point_coords[0]
  vertinfo.point_ycoord = point_coords[1]
  vertinfo.n_segments = max_verts - 1

  vertinfo.vertyear1 = best_vertices[0]
  vertinfo.vertyear2 = best_vertices[1]
  vertinfo.vertval1 = vertvals[0]
  vertinfo.vertval2 = vertvals[1]
  vertinfo.segmag1 = mags[0]
  vertinfo.f_stat = f_stat
  vertinfo.p_of_f = p_of_f
  
  case 1 of
    (max_verts eq 3): begin
      vertinfo.vertyear3 = best_vertices[2]
      vertinfo.vertval3 = vertvals[2]
      vertinfo.segmag2 = mags[1]

    end

    (max_verts eq 4): begin
      vertinfo.vertyear3 = best_vertices[2]
      vertinfo.vertval3 = vertvals[2]
      vertinfo.segmag2 = mags[1]

      vertinfo.vertyear4 = best_vertices[3]
      vertinfo.vertval4 = vertvals[3]
      vertinfo.segmag3 = mags[2]

    end

    (max_verts eq 5): begin

      vertinfo.vertyear3 = best_vertices[2]
      vertinfo.vertval3 = vertvals[2]
      vertinfo.segmag2 = mags[1]

      vertinfo.vertyear4 = best_vertices[3]
      vertinfo.vertval4 = vertvals[3]
      vertinfo.segmag3 = mags[2]

      vertinfo.vertyear5 = best_vertices[4]
      vertinfo.vertval5 = vertvals[4]
      vertinfo.segmag4 = mags[3]
    end

    (max_verts eq 6): begin
      vertinfo.vertyear3 = best_vertices[2]
      vertinfo.vertval3 = vertvals[2]
      vertinfo.segmag2 = mags[1]

      vertinfo.vertyear4 = best_vertices[3]
      vertinfo.vertval4 = vertvals[3]
      vertinfo.segmag3 = mags[2]


      vertinfo.vertyear5 = best_vertices[4]
      vertinfo.vertyear6 = best_vertices[5]
      vertinfo.vertval5 = vertvals[4]
      vertinfo.vertval6 = vertvals[5]

      vertinfo.segmag4 = mags[3]
      vertinfo.segmag5 = mags[4]
    end

    (max_verts eq 7):  begin
      vertinfo.vertyear3 = best_vertices[2]
      vertinfo.vertval3 = vertvals[2]
      vertinfo.segmag2 = mags[1]

      vertinfo.vertyear4 = best_vertices[3]
      vertinfo.vertval4 = vertvals[3]
      vertinfo.segmag3 = mags[2]

      vertinfo.vertyear5 = best_vertices[4]
      vertinfo.vertyear6 = best_vertices[5]
      vertinfo.vertyear7 = best_vertices[6]

      vertinfo.vertval5 = vertvals[4]
      vertinfo.vertval6 = vertvals[5]
      vertinfo.vertval7 = vertvals[6]

      vertinfo.segmag4 = mags[3]
      vertinfo.segmag5 = mags[4]
      vertinfo.segmag6 = mags[5]
    end

    (max_verts eq 8):  begin
      vertinfo.vertyear3 = best_vertices[2]
      vertinfo.vertval3 = vertvals[2]
      vertinfo.segmag2 = mags[1]

      vertinfo.vertyear4 = best_vertices[3]
      vertinfo.vertval4 = vertvals[3]
      vertinfo.segmag3 = mags[2]

      vertinfo.vertyear5 = best_vertices[4]
      vertinfo.vertyear6 = best_vertices[5]
      vertinfo.vertyear7 = best_vertices[6]
      vertinfo.vertyear8 = best_vertices[7]

      vertinfo.vertval5 = vertvals[4]
      vertinfo.vertval6 = vertvals[5]
      vertinfo.vertval7 = vertvals[6]
      vertinfo.vertval8 = vertvals[7]

      vertinfo.segmag4 = mags[3]
      vertinfo.segmag5 = mags[4]
      vertinfo.segmag6 = mags[5]
      vertinfo.segmag7 = mags[6]
    end

    else:
  endcase
  return, vertinfo
end