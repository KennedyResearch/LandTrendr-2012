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
; Copyright (c) 1998,2002 Oregon State University.
; This software is currently in beta-test condition.  It may not be altered,
;	copied, or redistributed without express written permission of
;	Robert E. Kennedy, Oregon State University (kennedyr@fsl.orst.edu).
;	This software is supplied as is, with no express or implied
;	warranties.

;Last update:  July 3, 2002

pro zot_img_pc, ufile, output_header, output_image, layers = layers, $
		subset=subset, block_av=block_av, hdronly=hdronly, $
		ignore = ignore, corner=corner, valid = valid


;Pixel types:  returned is the left number, the thing in quotes
;  is the code -- u =unsigned, s = signed, f = float, c = complex
;  		number = number of bits

;0="u1"
;1="u2"
;2="u4"
;3="u8"
;4="s8"
;5="u16"
;6="s16"
;7="u32"
;8= "s32"
;9="f32"
;10="f64"
;11="c64"
;12="c128"




;set validity to one, prove otherwise in program
valid = 1

;Is this a flat binary file?  If so, call zot_img_bsq.pro

  ;file = strcompress(ufile, /remove_all)
  file = ufile
  a= strlen(file)
  ext = strmid(file, a-4, 4)


;on_error, 2

;If subset is chosen,  (*,0) is upleftx,uplefty
;		       (*,1) is lorightx,lorighty

;if block_av is chosen and ignore is chosen, then we'll assign
;   that ignore value to any block that contains even one
;   instance of that value


;check keywords passed

  if n_elements(layers) eq 0 then layers = [-1]
  if n_elements(subset) eq 0 then subset = [-1]
  if n_elements(block_av) eq 0 then block_av = -1
  if n_elements(hdronly) eq 0 then hdronly = -1
  if n_elements(ignore) eq 0 then ignore = -1


;open file
  openr, un, file, /get_lun

;first time through, we know we need to look at byte 16 to get the
;	position of the Ehfa_file
  point_lun, un, 16
  headerPtr = img_readlong_pc(un)

;using that, we now need to look at the Ehfa_file for the
;	pointer to the root node of the object tree and the
;	dictionary entry

  point_lun, un, headerPtr
  img_read_Ehfa_File_pc, un, rootEntryPtr, dictionaryPtr

;now that we have the rootEntryPtr, we get the pointer to the
;	first child

  point_lun, un, rootEntryPtr
  img_read_Ehfa_Entry_pc, un, next, prev, parent, child, data, dataSize, $
  	name, type

;move to that child and get its info.
;get a list of all the nodes at this level, since some may be other
;	layers of imagery

  locations = lonarr(1)	;right now, we only there's one node
  types = strarr(1)
  names = strarr(1)
  counter = 0



  findallnodes:
  counter = counter + 1
  locations(counter-1) = child
  point_lun, un, child
  img_read_Ehfa_Entry_pc, un, next, prev, parent, child, data, dataSize, $
 	name, type
  types(counter-1) = type
  names(counter-1) = name

  if next ne 0 then begin
  		expand_rows, locations, 1, newdims
  		expand_rows, types, 1, newdims
  		expand_rows, names, 1, newdims
  		child = next
  		goto, findallnodes
  		end

  ;check through the list of nodes and see how many image layers we have

    numlayers = 0
    for i = 0, counter -1 do begin
       if types(i) eq 'Eimg_Layer' then begin
     		numlayers = numlayers+1

     	   ;now we save a pointer to this node position in the
     	   ;	positions/types/names variables

     		if numlayers eq 1 then image_pointer = bytarr(1) $
     		   else expand_cols, image_pointer, 1, newdims

     		image_pointer(numlayers-1)= i

       end
    end

  ;get the numbers of the bands in an integer, rather than string format

    w= names(image_pointer(*))
    bandnums=bytarr(numlayers)

    for i = 0, numlayers-1 do begin
      currentvictim = names(image_pointer(i))

      ;so far, we only recognize Layer_### and Band_###
        skipchars = 0
      case 1 of
      (strmid(currentvictim, 0, 4) eq 'Band'):	bandnums[i] = $
      		fix(strmid(currentvictim, 5, strlen(currentvictim)-skipchars))
      (strmid(currentvictim, 0, 5) eq 'Layer'):	bandnums[i] = $
      		fix(strmid(currentvictim, 6, strlen(currentvictim)-skipchars))

      else:  bandnums[i] = i+1			;changed march 27, 2006
      endcase

    end

      ;We need to pick only those layers that the user wants
     if layers(0) eq -1 then begin
     		pickedlayers=indgen(numlayers)	;we need to set up the
     					;pointer to all bands
     		players = numlayers
     		goto, skiplayersub	;if the user didn't specify
  						;we take 'em all
  		end


    ;adjust numlayers to the number the user wants
       players = n_elements(layers)

    ;check each band in the user list, check against our bandnum list
    ;	and assign pointers to the points



    e=0	;counter
    for i= 0, players-1 do begin
       w = where(bandnums eq layers(i), many)

       if many eq 0 then $
          print, 'Band '+string(layers(i))+' not found in image.' $
       else begin
       	   e=e+1
       	     		;set up layerpointer to point to the positions
    			;in 'locations' that hold the byte
    			;positions of nodes with image data

       	   if e eq 1 then pickedlayers = [0] else expand_cols, pickedlayers, 1, newdims
       	   pickedlayers(e-1)=w
       end

    end 	;i



skiplayersub:
;We'll assume that Map info is the same for all layers, to
;	make things easier.  This will be a problem if someone
;	saves an .img file from multiple layers that do not
;	line up in the upper left corner or have the same number of rows,cols


;April 99
;   With the above assumption, we only need to figure out all the positioning
;    stuff once, then loop through the layers.  So here it is

layer_count = 0


;point to the top node for this layer, based on the search
;	we did above, and read the Eimg_layer file

  point_lun, un, locations(image_pointer(pickedlayers(layer_count)))
  img_read_Ehfa_Entry_pc, un, next, prev, parent, child, data, dataSize, $
 	name, type


;The information located at the pointer "data" is in the format for
;	Eimg_layer, which gives information about the dimensions of
;	the image layer, whether its thematic or not, and what
;	format the pixels are in

  point_lun, un, data
  img_read_Eimg_layer_pc, un, width, height, layerType, pixelType, $
  	blockWidth, blockHeight
  blockDims = [ [blockWidth, blockHeight], [blockWidth, blockHeight] ]


;loop through the children of the Eimg_layer until you get to the
;	one with type labelled "Eprj_Mapinfo"

  startofimagelayer = child	;keep this for reference when we
  				;actually read the image later.

  next = child
    repeat begin

    point_lun, un, next
    img_read_Ehfa_Entry_pc, un, next, prev, parent, child, data, dataSize, $
 	name, type
    if next eq '0' then begin	;no map info available
        proName = 'none'
        upperLeftCenter = [1,height]
        lowerRightCenter = [width, 1]
        pixelSize=[1,1]
        units = 'pixels'
        goto, pastmapinfo
        end

    end until type eq "Eprj_MapInfo"

  point_lun, un, data
  img_read_Eprj_MapInfo_pc, un, proName, upperLeftCenter, lowerRightCenter, $
      pixelSize, units
pastmapinfo:
;print, 'Past mapinfo'


  ;figure out how many blocks there are on a side in the original image
  ;     if we hit a perfect multiple of block size, then we stick
  ;	with that number, otherwise, anything over that goes up
  ;	to the next integer value

    a=float(height)/blockHeight
    if a ne fix(a) then yblocks=long(a)+1 else yblocks =long(a)
    a=float(width)/blockWidth
    if a ne fix(a) then xblocks=long(a)+1 else xblocks = long(a)



;DETERMINE SUBSET INFORMATION, IF USER CALLED FOR IT

  ;if the user didn't send subset info, then the whole image is the
  ;the subset

 bounds =  [ [upperLeftCenter(0), upperLeftCenter(1)], $
 		[lowerRightCenter(0), lowerRightCenter(1)] ]
 if subset(0) eq -1 then subset = bounds else subset = double(subset)

qw= subset
; print, 'Original Subset'
; print, subset

 ;if the user sent the subset in outer-corner values, we need to
 ;		adjust to centers of pixels   (added 1/22/98, REK)

   if n_elements(corner) ne 0 then begin
   	subset(*,0) = edgit(subset(*,0), pixelSize, /tocenter, /map)  ;upl
   	subset(*,1) = edgit(subset(*,1), pixelSize, /tocenter, /map, /lowerright) ;lor

   end

 ;format = '(f14.4)'

;print, 'Adjust 1'
; print, format=format, subset
; print, 'Bounds'
; print, format=format, bounds


 subt = fix_edge(subset, bounds, /map)
 subt.coords = cutafter(subt.coords, 4)	;remove noise after fourth dec. place


 if subt.valid eq 0 then begin
 		print, 'zot_img_pc: Subset gives invalid coordinates. '
	    print, 'Subset:'
	    print, subset
	    print, 'Bounds of image:'
	    print, bounds
	    print, 'Bailing.'

 		goto, out
 		end

 subset= subt.coords
; print, 'Adjust 2'
; print, format=format, subset


;print, 'Check subset'

 ;get the file positions of the subset values the user
  ;	gave us.  If not an integer multiple of the
  ;	pixel size, we adjust the values on return
  ;     also, if we're averaging by block, we need to
  ;	adjust the values to the nearest whole block

    subset_file = lonarr(2,2)
    subset_block = lonarr(2,2)
    block_offset = lonarr(2,2)

   ;to ensure that our values are within the bounds  of the image,
   ;	we make a variable with all the values in one place
    file_constraints = [ [upperLeftCenter(0), upperLeftCenter(1)], $
 		[lowerRightCenter(0), lowerRightCenter(1)] ]

    ;       	0		1
    ;	0	upleftx		uplefty
    ;   1	lowerrtx	lowerrty

    for i = 0,1 do begin	;x, y
      for j = 0,1 do begin   	;upleft, lowerright

      bs= float(blockDims(i,0))  ;get the block size in this dimension
      adj = ( (i*2) -1)	;adjustment factor for x vs y (because
      			;map coords count from left for x but
      			;bottom for y.  In this line, adj is
      			;-1 for 'x' (when i=0) and +1 for 'y'

      v = (( [ (-1)*adj*subset[i,j] ] + [adj*upperLeftCenter[i]] ) / double(pixelSize[i]))[0]

    ;if we're doing block averaging

      if block_av eq 1 then begin
      	bs= float(blockDims(i,0))  ;get the block size in this dimension

       ;use v+1 because in fact we're looking for file coords of
       ;	63, 127, etc.,but need to divide by 64 to check.

         adj = j	;for the upper left, we need to check for 0, 64, etc.
         		;but for the lower right, we need to check for 63, 127, etc.

        if (v+adj)/bs ne fix((v+adj)/bs) then begin
      	    adj2=adj*bs
      	    temp = (v-adj2+adj)
      	    if i eq 0 then subset(i,j) = $
      	    	( ( ((ceil(temp/bs)*bs)*1)-adj )* $
      	    		pixelSize(i) )+upperLeftCenter(i) else $
      	    	subset(i,j) = upperLeftCenter(i)- $
      	    		( ( ((ceil(temp/bs)*bs)*1)-adj )* pixelSize(i) )
      	    ;print, 'Adjusting coordinates to integer multiples of block size.'
        end
    end else if long(v) ne cut_tiny(v, .001) then begin


      ;if we're doing just regular subset
        ;print, 'Adjusting coordinates to integer multiples of pixel size.'

        ;print, 'Old coordinate: '+string(subset(i,j))
        ;print, 'adjusting v'
        ;print, 'old = '+string(v)
      v = round(v)
        ;print, 'new = '+string(v)


        adj = (-1)*((i*2)-1)
        subset(i,j) = double(upperLeftCenter(i)+ (v * pixelSize(i)*adj) )



        ;now check to make sure that by moving we're not outside the edge of the image
        ;##### need to do


       ;print, 'New coordinate: '+string(subset(i,j))

      end

    ;for either case, we figure out where in the file this subset
    ;	corresponds to

      ;subset_file(i,j) = abs(fix(v/bs)*bs)-1

         ;(if i is 0, we're on  the x coord, if 1 then we're on the y)
      if i eq 0 then subset_file(i,j) = $
      		round((subset(i,j)-upperLeftCenter(i) ) / pixelSize(i)) else $
      		     subset_file(i,j) = $
      		round(( upperLeftCenter(i) - subset(i,j) ) / pixelSize(i))

      ;print, 'subset_file'
      ;print, subset_file

        ;if subset_file[0,1] ne 0 then stop

        ;print, subset_file


       ;figure out which blocks these coordinates are in

        blockdec = (subset_file(i,j)+1) / (float(blockDims(i,j)))

        subset_block(i,j) = ceil(blockdec)-1

      ;the block offset is the distance in pixels from the left edge
      ;	of the block.  NOTE that this is the number of values to
      ; add to the subscripts, i.e. an offset of 0 means that the
      ; edge of the image coincides with the edge of the block.

          block_offset(i,j) = ( (blockdec) -  $
        	 (ceil(blockdec)-1)) * (blockDims(i,j)-1)
      ;now we need to clip off the portions of image included in blocks but
      ;	that shouldn't be in the subset
          block_offset(*,1) = blockdims - block_offset(*,1)	;change to
    							;refer to right edge of block



       end
    end




    ;if we're only getting header information, we don't need to do any more

  filesize = [ subset_file(0,1)-subset_file(0,0)+1, $
    			subset_file(1,1)-subset_file(1,0)+1]


  ;check to make sure that we're not requesting sub-zero filesizes

  if filesize(0) lt 0 or filesize(1) lt 0 then begin
  		print, 'Coordinates yield 0 sized image'
  		goto, getout
  		end


  if hdronly ne -1 then goto, getout


    ;subset_dims is the dimensions of the subset in file pixels
    ;	or, in the case of block averaging, in number of blocks.
    ;NOTE that these are not subscripts, but the size of the dimensions
    ; of the sides
    ;NOTE that this right now assume that we're working with a system
    ;	like UTM, with northing and easting, so that upperleft x is
    ;  less than lower right x, but upperleft y is greater than lowerright y

      ;if block_av is -1, we're not doing block averaging and subset_dims is
      ;	in pixels.  Otherwise, we are doing block averaging and subset_dims is in
      ;	blocks.

        if block_av eq -1 then $
        subset_dims = ([ subset_file(0,1)-subset_file(0,0)+1, $
      		subset_file(1,1)-subset_file(1,0)+1 ]) else $
      	subset_dims = [ subset_block(0,1)-subset_block(0,0)+1, $
      		subset_block(1,1)-subset_block(1,0)+1 ]


    ;now we have the starting and ending blocks for the subset
    ;make a block pointer variable to point to these in the
    ;  block addresses variable.

      ;the number of blocks is determined by lortx-upleftx, uplefty-lowertx
      num_subs_blocks = [ subset_block(0,1)-subset_block(0,0)+1, $
      		subset_block(1,1)-subset_block(1,0)+1 ]

      blockpointers = lonarr(num_subs_blocks(0), num_subs_blocks(1))


      for x = 0, num_subs_blocks(0)-1 do begin
        for y = 0, num_subs_blocks(1)-1 do begin

        ;we establish the pointer relative to the original image
        ;   according to     pointer = x+ (y*xblocks)
        ;   Of course, the x value here is the offset from the
        ;   start of the subset, so the x to be used in the pointer
        ;   reference must be calculated first


        blockpointers(x,y) =  (subset_block(0,0)+x) + $
        	(  (subset_block(1,0)+y)*(xblocks) )
        end 	;y
      end 	;x


;SET UP BASIC IMAGE STUFF COMMON TO ALL LAYERS

  ;set up a temporary image variable
  ;	that will include the space around the edges of the image
  ;	cause by block size.  If we're doing block averaging, however
  ;	the dimensions of the resultant image will simply be the
  ;	number of blocks.


if block_av eq -1 then $
   	 tempdim = [num_subs_blocks(0)*blockWidth, $
   	 	num_subs_blocks(1)*blockHeight] else $
   	 tempdim = [num_subs_blocks(0), num_subs_blocks(1)]

   ;define the temporary image according to the file type
   ;	given in the header



    case 1 of
      (pixelType eq 3): tempim= bytarr(tempdim(0), tempdim(1))
      (pixelType eq 4 or $
       pixelType eq 6): tempim= intarr(tempdim(0), tempdim(1))
       (pixelType eq 5): tempim = uintarr(tempdim[0], tempdim[1])
       (pixelType eq 7):  tempim = lonarr(tempdim[0], tempdim[1])
      (pixelType eq 9): tempim = fltarr(tempdim(0), tempdim(1))
      else:  begin
      		print, "pixelType is " + string(pixelType)
      		print, "this is not supported"
      		a = get_kbrd(1)
      		stop
      		end
      endcase

   ;unless we're doing an averaging, in which case we'll have
   ;	decimals and will need to use floating point.
    if block_av ne -1 then tempim=fltarr(tempdim(0), tempdim(1))


 ;make a blank block
     case 1 of
      (pixelType eq 4 or pixelType eq 3): b= bytarr(blockWidth, blockHeight)
      (pixelType eq 5): b= uintarr(blockWidth, blockHeight)
      (pixelType eq 6): b= intarr(blockWidth, blockHeight)
      (pixelType eq 7): b= lonarr(blockWidth, blockHeight)
      (pixelType eq 9): b = fltarr(blockWidth, blockHeight)
      else:  message, 'Image type currently not supported'
     endcase
   ;determine number of pixels in a block
   pixelsperblock = float(n_elements(b))
   bytesw = 1


      if layer_count eq 0 then begin
  	case 1 of
         (pixelType eq 3): output_image= bytarr(subset_dims(0), subset_dims(1), players)
         (pixelType eq 4 or pixelType eq 6): output_image= intarr(subset_dims(0), subset_dims(1), players)
         (pixelType eq 5):  output_image = uintarr(subset_dims[0], subset_dims[1], players)
         (pixelType eq 7):  output_image = lonarr(subset_dims[0], subset_dims[1], players)

         (pixelType eq 9): output_image = fltarr(subset_dims(0), subset_dims(1), players)
         else:  message, 'Image type currently not supported'
        endcase
      end



;GET IMAGE DATA

  for layer_count = 0, players-1 do begin


   ;point to the top node for this layer, based on the search
   ;	we did above, and read the Eimg_layer file

     point_lun, un, locations(image_pointer(pickedlayers(layer_count)))
     img_read_Ehfa_Entry_pc, un, next, prev, parent, child, data, dataSize, $
 	name, type

     point_lun, un, data
     img_read_Eimg_layer_pc, un, width, height, layerType, pixelType, $
    	blockWidth, blockHeight

     next = child
    ;now loop until we find the object with the name 'RasterDMS'

      repeat begin
        point_lun, un, next
        img_read_Ehfa_Entry_pc, un, next, prev, parent, child, data, dataSize, $
   	   name, type
             ;print, name
      end until name eq 'RasterDMS'

      point_lun, un, data

  ;get the addresses.

    img_read_Edms_State_subs_pc, un, blockpointers, blocksize, valids, $
  		compresstype, blockaddresses




 ;read in the layers to the image, block by block
 ;	we do a separate loop for the case where we're block averaging
 ;	or not

 ;***READ IN IMAGE, NORMAL CASE



 if block_av eq -1 then begin
   if pixeltype ne 4 then begin
     for y = 0, num_subs_blocks(1) -1 do begin
      for x = 0, num_subs_blocks(0) - 1 do begin
      point_lun, un, blockaddresses(x,y)

      if compresstype(x,y) eq 0 then readu, un, b else $ 	;no compress.
      b= img_read_compressed_pc(un, b)
;erase
;tvscl, b
;xyouts, .6, .6, 'type is '+string(compresstype[x,y]), /norm
;stop


if max(long(b)) gt 194000000 then stop


     ;October 25, 2005.  Again trying to deal with the compress
     ;  type deal for floating point -- here's the odd behavior:
     ;  An image saved as float without compression can be read
     ;  directly with a readu, un, b, and it doesn't need to be
     ;  byte-swapped.  But within the "img_readfloat_pc.pro", where
     ;  I do the same direct read, but only of a compressed value,
     ;  I need to byteswap it. for now, I'm going to make the byte-swap
     ;  happen within the img_readfloat_pc


      tempim(x*blockWidth:(x*blockWidth)+blockWidth-1, $
     	      y*blockHeight:(y*blockHeight)+blockHeight-1) = b

     ; if max(b) gt 20000 then stop

     end
    end
   end else begin 	;if pixel type IS 4, which means we need to handle special
     for y = 0, num_subs_blocks(1) -1 do begin
      for x = 0, num_subs_blocks(0) - 1 do begin
      point_lun, un, blockaddresses(x,y)

      if compresstype(x,y) eq 0 then readu, un, b else $ 	;no compress.
          b= img_read_compressed_pc(un, fix(b), type = 4)

      b=conv_signed8bit_to_int(b)
      tempim(x*blockWidth:(x*blockWidth)+blockWidth-1, $
     	      y*blockHeight:(y*blockHeight)+blockHeight-1) = b

      ;clean up b, put back to bytarr because pixeltype is 4
      ; and b has switched to integer

      b = byte(b)


     end
    end
   end ;pixel type is four

;READ IN IMAGE, AVERAGING BLOCKS
end ;else begin	;if we're doing averaging
;    for y = 0, num_subs_blocks(1) -1 do begin
;     for x = 0, num_subs_blocks(0) - 1 do begin
;     point_lun, un, blockaddresses(x,y)
;     ;print, x,y
;     if compresstype(x,y) eq 0 then readu, un, b else $ 	;no compress.
;     b= img_read_compressed_pc(un, b)
;
;     ;swap bytes if 8.2 version
;       if bytesw eq 1 then b=swap_endian(b)
;
;     if ignore eq -1 then tempim(x, y) = total(b)/pixelsperblock else $
;       begin
;         pop = where(b eq ignore, many)
;         if many ne 0 then tempim(x,y) = ignore else $
;            tempim(x, y) = total(b)/pixelsperblock
;       end
;
;     end
;     ;print, float(y) / (num_subs_blocks(1)-1)
;    end
;   end
;
    if (block_offset(0,0) lt 0) or (block_offset(0,1) eq 0) or $
    	(block_offset(1,0) lt 0) or (block_offset(1,1) eq 0) then stop


    if block_av eq -1 then endim= tempim(block_offset(0,0): tempdim(0)-block_offset(0,1), $
    		block_offset(1,0): tempdim(1)-block_offset(1,1) )

  if numlayers eq 1 then output_image = endim else $
       output_image(*,*,layer_count) = endim


end	;going through layers of image



;this marker called if we're doing header only

getout:

;need to adjust the map information to take into account the aggregation stuf

  if block_av eq 1 then begin
     ;the center is now one-half of a block in from the edge, but
     ;need to first take out the .5 pixel of the upper left (or lower
     ;right) regular sized pixel

  	upperLeftCenter = subset(*,0)-((pixelSize/2)*[1,-1])+(pixelSize*(blockDims(*,0)/2.)*[1,-1])
  	lowerRightCenter = subset(*,1)+((pixelSize/2)*[1,-1])-(pixelSize*(blockDims(*,0)/2.)*[1,-1])

     ;the pixel size is now 64 times the original size

       pixelSize = pixelSize * blockDims(0,*)

  end else begin  ;not block averaging

     upperLeftCenter = subset[*,0]
     lowerRightCenter = subset[*,1]
  end

 ;if the user sent the subset in outer-corner values, we need to
 ;		adjust back to the original values of edge coords

   if n_elements(corner) ne 0 then begin
   	subset(*,0) = edgit(subset(*,0), pixelSize,  /map)  ;upl
   	subset(*,1) = edgit(subset(*,1), pixelSize,  /map, /lowerright) ;lor

   end
  output_header = {layernames:names(image_pointer(pickedlayers)), $
 	filesize:filesize, blockSize: blockDims(0,*), pixelType:pixelType, $
	proName:proName, upperLeftCenter:upperLeftCenter, $
        lowerRightCenter:lowerRightCenter, pixelSize:pixelSize, units:units, $
        n_layers:numlayers}

out:

free_lun, un
return


end


