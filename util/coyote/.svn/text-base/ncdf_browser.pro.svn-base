;+
; NAME:
;       NCDF_BROWSER
;
; PURPOSE:
;
;       This program is designed to make it easier to browse
;       and read the data and metadata in netCDF files. The user
;       can browse files, and read the data and metadata into
;       main-level IDL variables. New netCDF files can be opened
;       at any time. The user interacts with the program via a
;       graphical user interface (GUI).
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:

;       File I/O
;
; CALLING SEQUENCE:
;
;       IDL> NCDF_Browser, filename
;
; Arguments:
;
;       filename: The name of a netCDF file to open and browse.
;
; KEYWORD PARAMETERS:
;       
;       EXTENSION: In general, netCDF files use *.nc, *.ncf, or *.ncdf file extensions to
;                  identify themselves as netCDF files. Some users have their own file extensions.
;                  You can use this keyword to identify the file extension you wish to use. If
;                  set here, it will be used as the file filter in place of the normal file 
;                  extensions in DIALOG_PICKFILE.
;
;                      obj = ('NCDF_DATA', file, EXTENSION='*.bin')
;
; NOTES:
;       
;       This program is only a (useful) front-end for a more flexible
;       object program of class NCDF_DATA. In this front end, the NCDF_DATA
;       object is created and then destroyed when the GUI is destroyed.
;       The NCDF_DATA object can be used to read netCDF data in a non-interactive
;       way, if you prefer not to use a GUI to interact with the data file.
;
; REQUIRES:
;
;        The following programs are required from the Coyote Library.
;
;              http://www.dfanning.com/netcdf_data__define.pro
;              http://www.dfanning.com/error_message.pro
;              http://www.dfanning.com/centertlb.pro
;              http://www.dfanning.com/undefine.pro
;              http://www.dfanning.com/textbox.pro
;              http://www.dfanning.com/fsc_base_filename.pro
;              http://www.dfanning.com/textlineformat.pro
;
; MODIFICATION HISTORY:
;       Written by:  David W. Fanning, 03 Feb 2008. Used ideas from many
;           people, including Chris Torrence, Ken Bowman, Liam Gumely, 
;           Andrew Slater, and Paul van Delst.
;       Added Extension keyword. DWF. 04 Feb 2008.
;-
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
PRO NCDF_BROWSER, filename, EXTENSION=extension

   IF N_Elements(extension) EQ 0 THEN extension = '*.nc;*.ncd;*.ncdf'

   ; Need a filename?
   IF N_Elements(filename) EQ 0 THEN BEGIN
      filename = Dialog_Pickfile(/READ, TITLE='Select a netCDF File to Open', $
         FILTER=extension)
      IF filename EQ "" THEN RETURN
    ENDIF
    
   ; Catch a nCDF failure to open file error.
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      IF theError EQ -1076 THEN BEGIN
         ok = Dialog_Message(/Error, 'File does not appear to be a netCDF file. Returning...')
         RETURN
      ENDIF
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Can we open this file as a nCDF file?
   fileID = NCDF_OPEN(filename) 
   NCDF_CLOSE, fileID
   
   ; Error handling. 
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Create an nCDF_DATA browse object.
   ncdfObj = Obj_New('NCDF_DATA', filename, /Destroy_From_Browser, EXTENSION=extension)
   IF Obj_Valid(ncdfObj) THEN ncdfObj -> Browse
   
END