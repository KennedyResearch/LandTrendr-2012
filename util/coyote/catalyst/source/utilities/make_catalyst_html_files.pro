;*****************************************************************************************************
;+
; NAME:
;       Make_Catalyst_HTML_Files
;
; PURPOSE:
;
;       This is used to produce Catalyst HTML files from the Catalyst source
;       code, using the IDL routine MK_HTML_HELP.
;
; AUTHORS:
;
;        FANNING SOFTWARE CONSULTING   BURRIDGE COMPUTING
;        1645 Sheely Drive             18 The Green South
;        Fort Collins                  Warborough, Oxon
;        CO 80526 USA                  OX10 7DN, ENGLAND
;        Phone: 970-221-0438           Phone: +44 (0)1865 858279
;        E-mail: davidf@dfanning.com   E-mail: davidb@burridgecomputing.co.uk
;
; CATEGORY:
;
;       Programming.
;
; CALLING SEQUENCE:
;
;       Make_Catalyst_HTML_Files
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W Fanning, 12 Oct 2008.
;-
;******************************************************************************************;
;  Copyright (c) 2008, jointly by Fanning Software Consulting, Inc.                        ;
;  and Burridge Computing. All rights reserved.                                            ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. or Burridge Computing       ;
;        nor the names of its contributors may be used to endorse or promote products      ;
;        derived from this software without specific prior written permission.             ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. AND BURRIDGE COMPUTING   ;
;  ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE     ;
;  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE          ;
;  DISCLAIMED. IN NO EVENT SHALL FANNING SOFTWARE CONSULTING, INC. OR BURRIDGE COMPUTING   ;
;  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL    ;
;  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;    ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
PRO Make_Catalyst_HTML_Files
    
    ; Locate the main Catalyst Directory.
    catalystDir = ProgramRootDir(/TWOUP)
    
    ; Can you find an HTML docs directory here?
    htmlDir = Filepath(ROOT_DIR=catalystDir, 'docs')
    IF File_Test(htmlDir, /DIRECTORY) EQ 0 THEN BEGIN
        htmlDir = Dialog_Pickfile(TITLE='Select Directory for HTML Output', /DIRECTORY)
        IF htmlDir EQ "" THEN RETURN
    ENDIF
    
    ; Search the library directory.
    searchDir = Filepath(ROOT_DIR=catalystDir, SUBDIR='library', "")
    searchFiles = File_Search(searchDir, '*.pro', COUNT=count)
    Mk_HTML_Help, searchFiles, Filepath(ROOT_DIR=htmlDir, 'library.html'), $
            TITLE='Catalyst Library Directory On-Line Help'
    
    ; Search the templates directory.
    searchDir = Filepath(ROOT_DIR=catalystDir, SUBDIR='templates', "")
    searchFiles = File_Search(searchDir, '*.pro', COUNT=count)
    Mk_HTML_Help, searchFiles, Filepath(ROOT_DIR=htmlDir, 'templates.html'), $
            TITLE='Catalyst Templates Directory On-Line Help'
   
    ; Search the source/applications directory.
    searchDir = Filepath(ROOT_DIR=catalystDir, SUBDIR=['source','applications'], "")
    searchFiles = File_Search(searchDir, '*.pro', COUNT=count)
    Mk_HTML_Help, searchFiles, Filepath(ROOT_DIR=htmlDir, 'source_applications.html'), $
            TITLE='Catalyst Source-Applications Directory On-Line Help'

    ; Search the source/coordinates directory.
    searchDir = Filepath(ROOT_DIR=catalystDir, SUBDIR=['source','coordinates'], "")
    searchFiles = File_Search(searchDir, '*.pro', COUNT=count)
    Mk_HTML_Help, searchFiles, Filepath(ROOT_DIR=htmlDir, 'source_coordinates.html'), $
            TITLE='Catalyst Source-Coordinates Directory On-Line Help'
    
    ; Search the source/core directory.
    searchDir = Filepath(ROOT_DIR=catalystDir, SUBDIR=['source','core'], "")
    searchFiles = File_Search(searchDir, '*.pro', COUNT=count)
    Mk_HTML_Help, searchFiles, Filepath(ROOT_DIR=htmlDir, 'source_core.html'), $
            TITLE='Catalyst Source-Core Directory On-Line Help'

    ; Search the source/data directory.
    searchDir = Filepath(ROOT_DIR=catalystDir, SUBDIR=['source','data'], "")
    searchFiles = File_Search(searchDir, '*.pro', COUNT=count)
    Mk_HTML_Help, searchFiles, Filepath(ROOT_DIR=htmlDir, 'source_data.html'), $
            TITLE='Catalyst Source-Data Directory On-Line Help'

    ; Search the source/graphics directory.
    searchDir = Filepath(ROOT_DIR=catalystDir, SUBDIR=['source','graphics'], "")
    searchFiles = File_Search(searchDir, '*.pro', COUNT=count)
    Mk_HTML_Help, searchFiles, Filepath(ROOT_DIR=htmlDir, 'source_graphics.html'), $
            TITLE='Catalyst Source-Graphics Directory On-Line Help'
 
    ; Search the source/interactions directory.
    searchDir = Filepath(ROOT_DIR=catalystDir, SUBDIR=['source','interactions'], "")
    searchFiles = File_Search(searchDir, '*.pro', COUNT=count)
    Mk_HTML_Help, searchFiles, Filepath(ROOT_DIR=htmlDir, 'source_interactions.html'), $
            TITLE='Catalyst Source-Interactions Directory On-Line Help'
 
    ; Search the source/ographics directory.
    searchDir = Filepath(ROOT_DIR=catalystDir, SUBDIR=['source','ographics'], "")
    searchFiles = File_Search(searchDir, '*.pro', COUNT=count)
    Mk_HTML_Help, searchFiles, Filepath(ROOT_DIR=htmlDir, 'source_ographics.html'), $
            TITLE='Catalyst Source-OGraphics Directory On-Line Help'
    
    ; Search the source/utilities directory.
    searchDir = Filepath(ROOT_DIR=catalystDir, SUBDIR=['source','utilities'], "")
    searchFiles = File_Search(searchDir, '*.pro', COUNT=count)
    Mk_HTML_Help, searchFiles, Filepath(ROOT_DIR=htmlDir, 'source_utilities.html'), $
            TITLE='Catalyst Source-Utilities Directory On-Line Help'
 
    ; Search the source/widgets directory.
    searchDir = Filepath(ROOT_DIR=catalystDir, SUBDIR=['source','widgets'], "")
    searchFiles = File_Search(searchDir, '*.pro', COUNT=count)
    Mk_HTML_Help, searchFiles, Filepath(ROOT_DIR=htmlDir, 'source_widgets.html'), $
            TITLE='Catalyst Source-Widgets Directory On-Line Help'
 
END