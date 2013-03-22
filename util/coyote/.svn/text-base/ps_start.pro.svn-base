;+
; NAME:
;   PS_START/PS_END
;
; PURPOSE:
;
;    The purpose of PS_START and PS_END is to make it easy to set-up
;    for and close a PostScript file. The programs work in close conjunction
;    with PSCONFIG, another program from the Coyote Library.
;
;    If ImageMagick  (http://www.imagemagick.org/script/index.php) is installed 
;    on your computer, you can easily convert PostScript output to JPEG, PNG, and TIFF
;    image output. (See the keywords to PS_END.)
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;       Graphics, File Output, PostScript
;
; CALLING SEQUENCE:
;
;       PS_START
;       Various graphics commands here...
;       PS_END
;
; KEYWORD PARAMETERS FOR PS_START:
;
;       GUI:          The default behavior is to use PSCONFIG to configure the
;                     PostScript device silently. If you wish to allow the user
;                     to interatively configure the PostScript device, set this
;                     keyword.
;
;       SCALE_FACTOR: Set this to the PostScript scale factor. By default: 1.
;
;       Any keyword supported by PSCONFIG can be used to configure the PostScript device.
;       Common keywords would include FILENAME, XSIZE, YSIZE, XOFFSET, YOFFSET, etc. See
;       the PSCONFIG documentation for details.
;
; KEYWORD PARAMETERS FOR PS_END:
;
;      All keywords for PS_END require that ImageMagick is installed on your computer
;      and is configured correctly. Image conversion is done by spawning a "convert" 
;      command to ImageMagick.
;
;       JPEG:         Set this keyword to convert the PostScript output file to a JPEG image.
;       PNG:          Set this keyword to convert the PostScript output file to a PNG image.
;       TIFF:         Set this keyword to convert the PostScript output file to a TIFF image.
;       
;       The convert command looks like this. You can modify it in the code if you want it to
;       do something else for you.
;       
;          convert -density 300 ' + inputPostScriptFilename + ' -resize 25% ' + outputImageFilename
;
; COMMON BLOCKS:
;
;       _$FSC_PS_START_   Contains the PS_STRUCT structure for communication between
;                         PS_START and PS_END.
;
; SIDE EFFECTS:
;
;       When PS_START is called, the current graphics device is set to "PS" (the PostScript 
;       device). When PS_END is called the current graphics device is returned to the device
;       in effect when PS_START was called.
;
; RESTRICTIONS:
;
;       Requires numerous programs from the Coyote Library. To convert PostScript files
;       to PNG, JPEG, and TIFF files requires ImageMagick be installed on your
;       computer and configured correctly. You can download Coyote Library programs here:
;
;             http://www.dfanning.com/programs/coyoteprograms.zip
;
;       ImageMagick can be found here:
;
;              http://www.imagemagick.org/script/index.php
;
; EXAMPLE:
;
;       To create a line plot in a PostScript file named lineplot.ps and
;       also create a PNG file named lineplot.png for display in a browser,
;       type these commands.
;
;       PS_Start, FILENAME='lineplot.ps'
;       Plot, Findgen(11), COLOR=FSC_Color('navy'), /NODATA, XTITLE='Time', YTITLE='Signal'
;       OPlot, Findgen(11), COLOR=FSC_Color('indian red')
;       OPlot, Findgen(11), COLOR=FSC_Color('olive'), PSYM=2
;       PS_End, /PNG
;
; NOTES:
;
;       You can easily configure any modifications you like for your PostScript output
;       by setting fields in the plot and axis system variables (!P, !X, !Y, and !Z).
;       The modifications currently made by default in this program are these:
;
;          !P.Thick = 2
;          !X.Thick = 2
;          !Y.Thick = 2
;          !Z.Thick = 2
;          !P.Font = 1
;
; MODIFICATION HISTORY:
;
;       Written by: David W. Fanning, 20 May 2008.
;       Slight modification to allow filenames with spaces in them.
;-
;
;******************************************************************************************;
;  Copyright (c) 2008, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
PRO FSC_PS_SETUP__DEFINE

    struct = { FSC_PS_SETUP, $
               currentDevice: "", $
               setup: 0, $
               convert: "", $
               filename: "", $
               p: !P, $
               x: !X, $
               y: !Y, $
               z: !Z $
              }
            
END ;---------------------------------------------------------------



PRO PS_END, JPEG=jpeg, PNG=png, TIFF=tiff

   COMMON _$FSC_PS_START_, ps_struct
   
   ON_ERROR, 2 ; Return to caller.
   
   ; Close the PostScript file, if this is PostScript device.
   IF !D.Name EQ 'PS' THEN Device, /CLOSE_FILE
   
   ; Need to convert with ImageMagick?
   IF Keyword_Set(png) THEN ps_struct.convert = 'PNG'
   IF Keyword_Set(jpeg) THEN ps_struct.convert = 'JPEG'
   IF Keyword_Set(tiff) THEN ps_struct.convert = 'TIFF'
   IF ps_struct.convert NE "" THEN BEGIN

        basename = FSC_Base_Filename(ps_struct.filename, DIRECTORY=theDir, EXTENSION=theExtension)
        CASE 1 OF
            ps_struct.convert EQ 'PNG':  outfilename = Filepath(ROOT_DIR=theDir, basename + '.png')
            ps_struct.convert EQ 'JPEG': outfilename = Filepath(ROOT_DIR=theDir, basename + '.jpg')
            ps_struct.convert EQ 'TIFF': outfilename = Filepath(ROOT_DIR=theDir, basename + '.tif')
        ENDCASE
        IF N_Elements(outfilename) NE "" THEN BEGIN
            cmd = 'convert -density 300 ' + '"' +ps_struct.filename + '"' + ' -resize 25% ' + '"' + outfilename + '"'
            SPAWN, cmd
        ENDIF
        
   ENDIF
   
   ; Clean up.
   Set_Plot, ps_struct.currentDevice
   !P = ps_struct.p
   !X = ps_struct.x
   !Y = ps_struct.y
   !Z = ps_struct.z
   ps_struct.setup = 0
   ps_struct.currentDevice = ""
   ps_struct.filename = ""
   ps_struct.convert = ""

END ;---------------------------------------------------------------



PRO PS_START, SCALE_FACTOR=scale_factor, GUI=gui, _EXTRA=extra

   COMMON _$FSC_PS_START_, ps_struct
   
   ; Define the PS structure.
   IF N_Elements(ps_struct) EQ 0 THEN ps_struct = {FSC_PS_SETUP}
   
   ; If the setup flag is on, then we have to close the previous
   ; start command before we can continue.
   IF ps_struct.setup EQ 1 THEN PS_END
   
   ; Save current setup information in the PS_STRUCT structure.
   ps_struct.setup = 1
   ps_struct.currentDevice = !D.Name
   ps_struct.p = !P
   ps_struct.x = !X
   ps_struct.y = !Y
   ps_struct.z = !Z
   
   ; Change any parameters you feel like changing.
   !P.Thick = 2
   !X.Thick = 2
   !Y.Thick = 2
   !Z.Thick = 2
   
   ; Times true-type font as the default in PostScript.
   !P.Font = 1 
   Device, Set_Font='Times', /TT_FONT

   ; Configure the PostScript Device
   cancelled = 0
   sizes = PSWindow(_Extra=extra)
   keywords = PSConfig(_Extra=extra, INCHES=sizes.inches, XSIZE=sizes.xsize, YSIZE=sizes.ysize, $
      XOFFSET=sizes.xoffset, YOFFSET=sizes.yoffset, Cancel=cancelled, NOGUI=(1-Keyword_Set(gui)))
   IF cancelled THEN BEGIN
        PS_END
        RETURN
   ENDIF
   
   ; Let them know where the output will be.
   Print, 'PostScript output will be created here: ', keywords.filename
   
   Set_Plot, 'PS'
   Device, _EXTRA=keywords
   
   ; What about scale factor?
   IF N_Elements(scale_factor) NE 0 THEN $
        DEVICE, SCALE_FACTOR=scale_factor ELSE $
        DEVICE, SCALE_FACTOR=1
   
   ; Store filename.
   ps_struct.filename = keywords.filename
   
END