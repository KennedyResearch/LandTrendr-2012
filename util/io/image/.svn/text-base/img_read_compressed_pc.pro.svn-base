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
; Copyright (c) 1998, Oregon State University.
; This software is currently in beta-test condition.  It may not be altered,
;	copied, or redistributed without express written permission of
;	Robert E. Kennedy, Oregon State University (kennedyr@fsl.orst.edu).
;	This software is supplied as is, with no express or implied
;	warranties.

function img_read_compressed_pc, un, block

;block is a blank variable that will hold the image.  Its size
;and type define what the block will be read as for this function.

blocktype = size(block, /type)


;get the starting point of this block
  point_lun, -un, starting_point

;read in the general block info.
  minimum_value = img_readlong_pc(un)	;changed back April 30, 2003
  										;not sure what the deal is here
  										; need to see whether
  										; the min value is different
  										; for floating point images

  ;minimum_value = img_readfloat_pc(un)	;change made march 18, 2003

  num_segments = img_readlong_pc(un)	;the number of run-length
     			;segments
  data_offset = img_readlong_pc(un)  ;offset into block where
     			;compressed values start
  bitsperpixel = img_readbyte(un) ;the number of bits per pixel

 		;if the data compression is only range compression (i.e.
;	it uses 4 bits per pixel instead of 8, but no
;	run length compression)
  m=0b	;a dumb ol' generic byte constant.
  if num_segments eq -1 then begin
 		num_segments = n_elements(block)
 		data_counts=lonarr(num_segments)+1l
 		goto, norunlength
 		end


;now read in the data counts


  data_counts = lonarr(num_segments)	;this will hold the number of
  				;reps for each pixel count


  for i = 0,num_segments-1 do begin
    readu, un, m
     byte_count =  ishft(m, -6)+1	;how many bytes are needed to
     high = high_mask(m, 2)		;get rid of the top 2 bits
     				;that are the count

    if byte_count gt 1 then begin	;if more than the initial byte was
    					;needed, then we need to add on the
    					;next bytes

        high=long(high)

        for l = 1, byte_count-1 do begin
	readu, un, m

	high = ishft(high, 8) + m	;add m to the value
	end
    end
    data_counts(i) = high
  end


;At this point, we know how many counts there are for each value.  Now need
;	to read in the values and assign to the variable.  There are
;	two cases -- there are fewer than 8 bits per pixel, or 8 or more

  norunlength:

  counter = 0l	;this points to the current subscript in the image
  		;variable, will be incremented



  if bitsperpixel eq 0 then begin	;if there's no data
  			block(*)=minimum_value		;changed from 0 to min value July 21, 2004

  			return, block
  			end

  ;print, "bitspp"
 ; print, bitsperpixel

  if bitsperpixel lt 8 then begin
    p = 8 / bitsperpixel



    for i = 0l, (num_segments-1)*bitsperpixel, bitsperpixel do begin
      bit_pos = ( (i/8.)-long(i/8.) )*8	;how many bits into the byte
      					;are we
      if bit_pos eq 0 then readu, un, m


      value = ishft(  high_mask(m, 8-(bit_pos+bitsperpixel)),  -bit_pos)

      block(counter:counter+data_counts(i/bitsperpixel)-1) = value
      counter = counter + data_counts(i/bitsperpixel)

    end
  end else begin	;bitsperpixel gt 8
    case 1 of
    (bitsperpixel eq 8):  begin
    			    m=0b
    			    for i = 0,num_segments-1 do begin
    			    readu, un, m

    			    block(counter:counter+data_counts(i)-1) = m
    			    counter=counter+data_counts(i)
    			    end
    			  end
    (bitsperpixel eq 16): begin
    			  if blocktype eq 2 or blocktype eq 12 then begin
    			    for i = 0, num_segments-1 do begin
				    m=img_readshort_pc(un)
				    block(counter:counter+data_counts(i)-1) = m
				    counter=counter+data_counts(i)

				    end
				    block = swap_endian(block)			;took out July 18, 2004
				  end else begin 	;if there is a different blocktype
				   ;build in capacity for unsigned 16-bit -- blocktype 12
				   if blocktype ne 3 then message, 'fix type'	;if not long type, then bail
				   temp = fix(block)


				   for i = 0, num_segments-1 do begin
				    m=img_readshort_pc(un)
				    temp(counter:counter+data_counts(i)-1) = m
				    counter=counter+data_counts(i)

				    end
				    temp = swap_endian(temp)


					block = block + temp



				  end


			  end

    (bitsperpixel eq 32): begin


    			    if blocktype eq 3 then begin
    			      for i = 0, num_segments-1 do begin
			        m=img_readlong_pc(un)


			        block(counter:counter+data_counts(i)-1) = swap_endian(m);added 3/3/08

			        counter=counter+data_counts(i)
			      end
			    end else begin

			      for i = 0,num_segments-1 do begin
					;print, 'using img_readfloat_pc'
			        m=img_readfloat_pc(un)

			        block(counter:counter+data_counts(i)-1) = swap_endian(m)
			        			;changed to swap endian on October 25, 2005

			        counter=counter+data_counts(i)


			      end
			    end



			   end

    else:	message, 'Compression failed due to unknown data type'
    endcase



  end	;reading into block

;need to add back in the minimum value
;	but need to make sure that it's the right type of array



p=size(block)
if p(0) lt 2 then message, "Compression routine failure -- block too small"
type = p(3)
case 1 of
  (type eq 1):	block=block+ byte(minimum_value)
  (type eq 2):	block=block+ fix(minimum_value)
  (type eq 3):	block=block+ long(minimum_value)
  (type eq 4):	block=block+ float(minimum_value)
  (type eq 5):	block=block+ double(minimum_value)
  (type eq 12):  block = block + uint(minimum_value)
  else:	message, 'Type not supported by compression routine"
endcase


if max(block) gt 194000000 then stop


return, block

end
