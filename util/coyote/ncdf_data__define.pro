;+
; NAME:
;       NCDF_DATA__DEFINE
;
; PURPOSE:
;
;       This program is designed to make it easier to browse
;       and read the data and metadata in netCDF files. The user
;       can browse files, and read the data and metadata into
;       main-level IDL variables. New netCDF files can be opened
;       at any time. The user interacts with the program via a
;       browser window (GUI) or directly through the methods of
;       the object. The program implements an IDL object.
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
;       IDL> nCDFObject = Obj_New('NCDF_DATA', filename)
;
; ARGUMENTS:
;
;       filename: The name of a netCDF file to open and browse.
;
; KEYWORD PARAMETERS:
;       
;       BROWSE:   If this keyword is set, the Browse Window is invoked as soon
;                 as the object is initiated.
;
;       DESTROY_FROM_BROWSER:  As with all objects, this object is persistent until
;                  it is destroyed. However, with this keyword set, the object will
;                  be destroyed when the user closes the Browse Window.
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
;       This program is designed to be flexible in how it is used, so it
;       can be used in both interactive and non-interactive (called directly)
;       ways. A less flexible way of interacting with the program is via the
;       NCDF_BROWSER program, which is a front-end to this object.
;
; REQUIRES:
;
;     The following programs are required from the Coyote Library.
;
;              http://www.dfanning.com/netcdf_data__define.pro
;              http://www.dfanning.com/error_message.pro
;              http://www.dfanning.com/centertlb.pro
;              http://www.dfanning.com/undefine.pro
;              http://www.dfanning.com/textbox.pro
;              http://www.dfanning.com/fsc_base_filename.pro
;              http://www.dfanning.com/textlineformat.pro
;
; METHODS:
;
;     The following methods can be used directly.
;
;     ncdfObject -> Browse                             ; Use GUI to browse file data and metadata.
;     ncdfObject -> OpenFile, filename                 ; Opens a new netCDF file.
;     globalAttr = ncdfObject -> ReadGlobalAttr()      ; Return a structure containing global attributes.
;     attribute = ncdfObject -> ReadAttribute(attrname); Return an attribute, identified by name.
;     dim = ncdfObject -> ReadDimension(dimName)        ; Return a dimension, identified by name.
;     variable = ncdfObject -> ReadVariable(varname)   ; Return a variable, identified by name.
;     varstruct = ncdfObject -> ReadVariableWithAttr(varname)   ; Return a variable, identified by 
;                                                               ; name, along with its attributes.
;     allData = ncdfObject -> ReadFile(filename)        ; Read all the data in the file, into structures.
;
; EXAMPLE:
;
;       IDL> filename = 'example.nc'
;       IDL> ncdfObj = Obj_New('NCDF_DATA', filename)
;       IDL> ncdfObj -> Browse
;       IDL> Obj_Destroy, ncdfObj
;
; MODIFICATION HISTORY:
;       Written by:  David W. Fanning, 03 Feb 2008. Used ideas from many
;           people, including Chris Torrence, Ken Bowman, Liam Gumely, 
;           Andrew Slater, and Paul van Delst.
;       Added EXTENSION keyword, resizeable TLB, and ability to download
;           individual global attibutes. DWF. 04 Feb 2008.
;       Added ReadDimension and ReadVariableWithAttr methods. DWF. 05 Feb 2008.
;       Ill-formed attribute names giving me fits. Now doing checks with IDL_VALIDNAME
;            before creating structures. 06 February 2008. DWF.
;       Same problem. Wide use of IDL_VALIDNAME everywhere it seems wise. 06 Feb 2008. DWF.
;       Added functionality to read a variable with its attributes from the browser interface,
;            and fixed a problem with reading CHAR values. 2 March 2008. DWF.
;       Fixed a problem with changing variable name when reading variable plus attributes. 6 March 2008. DWF.
;       Fixed a problem with not setting GLOBAL keyword when inquiring about global attribute. 6 March 2008. DWF.
;       Made sure file was parsed before attempting to read variables and attributes to avoid errors. 7 March 2008. DWF.
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

PRO NCDF_DATA::Browse, XOFFSET=xoffset, YOFFSET=yoffset
;+
; NAME:
;       NCDF_DATA::Browse
;
; PURPOSE:
;
;       This method is invoked to create a Browser Window the user can
;       interact with to explored the data and metadata in a netCDF file.
;
; CALLING SEQUENCE:
;
;       IDL> nCDFObject -> Browse
;
; ARGUMENTS:
;
;       None.
;
; KEYWORD PARAMETERS:
;       
;       XOFFSET:   Normally, the Browser Window is centered, however is this
;                  keyword and the YOFFSET keywords are used, the Browser Window
;                  can be located with the upper-left corner at these locations in 
;                  device coordinates. The X offset of the Browser Window.
;
;       YOFFSET:    The Y offset of the Browser Window.
;-

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      Widget_Control, self.tlb, /DESTROY
      RETURN
   ENDIF
   
   ; Check input parameters.
   IF N_Elements(xoffset) EQ 0 THEN xoffset = -1
   IF N_Elements(yoffset) EQ 0 THEN yoffset = -1

   ; Only one browser with this TLB on the display at a time.
   IF Widget_Info(self.tlb, /VALID_ID) THEN BEGIN
      Widget_Control, self.tlb, /SHOW
      RETURN
   ENDIF

   ; Make sure the file has been parsed.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile
   IF self.hasBeenParsed EQ 0 THEN RETURN
   
   ; Get some bitmaps for the widget_tree.
   bmfile = Filepath(SubDir=['resource','bitmaps'], 'volume.bmp')
   IF File_Test(bmfile) THEN BEGIN
      bm = Read_BMP(bmfile, r, g, b)
      i = Where(bm EQ 8, count)
      IF count GT 0 THEN bm[i] = 16B
      variableBM = [ [[r[bm]]], [[g[bm]]], [[b[bm]]] ]
   ENDIF

   bmfile = Filepath(SubDir=['resource','bitmaps'], 'axis.bmp')
   IF File_Test(bmfile) THEN BEGIN
      bm = Read_BMP(bmfile, r, g, b)
      i = Where(bm EQ 8, count)
      IF count GT 0 THEN bm[i] = 16B
      dimensionBM = [ [[r[bm]]], [[g[bm]]], [[b[bm]]] ]
   ENDIF
   
   bmfile = Filepath(SubDir=['resource','bitmaps'], 'ascii.bmp')
   IF File_Test(bmfile) THEN BEGIN
      bm = Read_BMP(bmfile, r, g, b)
      i = Where(bm EQ 80, count)
      IF count GT 0 THEN bm[i] = 95B
      attributeBM = [ [[r[bm]]], [[g[bm]]], [[b[bm]]] ]
   ENDIF
   
   bmfile = Filepath(SubDir=['resource','bitmaps'], 'sum.bmp')
   IF File_Test(bmfile) THEN BEGIN
      bm = Read_BMP(bmfile, r, g, b)
      i = Where(bm EQ 8, count)
      IF count GT 0 THEN bm[i] = 16B
      summaryBM = [ [[r[bm]]], [[g[bm]]], [[b[bm]]] ]
   ENDIF
   
   ; Set up the initial tree widget.
   self.tlb = Widget_Base(TITLE='netCDF Browser', COLUMN=1, UVALUE=self, /BASE_ALIGN_CENTER, $
      TLB_SIZE_EVENT=1)
   rowbase = Widget_Base(self.tlb, ROW=1, XPAD=0, YPAD=0)
   theTree = Widget_Tree(rowbase, SCR_XSIZE=200, SCR_YSIZE=300, UNAME='theTree')
   self.textDisplay = Widget_Text(rowbase, SCR_XSIZE=500, SCR_YSIZE=300, /SCROLL)

   ; Set up fundamental branch.
   aBranch = Widget_Tree(theTree, Value='netCDF File', /FOLDER, /EXPANDED, UNAME='FOLDER')
   aNode = Widget_Tree(aBranch, Value='Directory', UValue=self.directory, UNAME='DIRECTORY', BITMAP=summaryBM)
   aNode = Widget_Tree(aBranch, Value='File Name', UValue=self.filename, UNAME='FILENAME', BITMAP=summaryBM)
   summaryNode = Widget_Tree(aBranch, Value='Summary', UNAME='SUMMARY', BITMAP=summaryBM)
   Widget_Control, summaryNode, Set_Tree_Select=1
   
   ; Set up global attribute branch.
   IF Ptr_Valid(self.theAttributes) THEN BEGIN
      aBranch = Widget_Tree(theTree, Value='Global Attributes', /FOLDER, UNAME='FOLDER')
      theAttributes = *self.theAttributes
      FOR j=0,N_Elements(theAttributes)-1 DO BEGIN
         aNode = Widget_Tree(aBranch, Value=theAttributes[j].name, UValue=theAttributes[j].name, $
            UNAME='GLOBAL ATTRIBUTE', BITMAP=attributeBM)
      ENDFOR
   ENDIF

   ; Set up dimension branch.
   IF Ptr_Valid(self.theDimensions) THEN BEGIN
      aBranch = Widget_Tree(theTree, Value='Dimensions', /FOLDER, UNAME='FOLDER')
      theDimensions = *self.theDimensions
      FOR j=0,N_Elements(theDimensions)-1 DO BEGIN
         aNode = Widget_Tree(aBranch, Value=theDimensions[j].name, UValue=theDimensions[j].name, $
            UNAME='DIMENSION', BITMAP=dimensionBM)
      ENDFOR
   ENDIF

   ; Set up variable branch.
   IF Ptr_Valid(self.theVariables) THEN BEGIN
      aBranch = Widget_Tree(theTree, Value='Variables', /FOLDER, UNAME='FOLDER')

      theVariables = *self.theVariables
      FOR j=0,N_Elements(theVariables)-1 DO BEGIN

         IF Ptr_Valid(theVariables[j].var_attributes) THEN BEGIN
            varBranch = Widget_Tree(aBranch, Value=theVariables[j].name, UValue=theVariables[j].name, $
               UNAME='VARIABLE', /FOLDER, BITMAP=variableBM)
            theAttributes = *theVariables[j].var_attributes
            FOR k=0,N_Elements(theAttributes)-1 DO BEGIN
               IF theAttributes[k].name NE "" THEN BEGIN
                  aNode = Widget_Tree(varBranch, Value=theAttributes[k].name, $
                     UValue=[theVariables[j].name,theAttributes[k].name], $
                     BITMAP=attributeBM, UNAME='VARIABLE ATTRIBUTE')
               ENDIF
            ENDFOR

         ENDIF ELSE BEGIN
            aNode = Widget_Tree(aBranch, Value=theVariables[j].name, UValue=theVariables[j].name, $
               UNAME='VARIABLE', BITMAP=variableBM)
         ENDELSE
      ENDFOR

   ENDIF
   
   ; Application Buttons
   buttonBase = Widget_Base(self.tlb, /ROW, BASE_ALIGN_CENTER=1)
   button = Widget_Button(buttonBase, Value='Read Variable', UVALUE='READ_VAR_FROM_GUI')
   button = Widget_Button(buttonBase, Value='Read Variable with Attributes', UVALUE='READ_VARPLUS_FROM_GUI')
   button = Widget_Button(buttonBase, Value='Read Global Attribute', UVALUE='READ_ATTRIBUTE_FROM_GUI')
   button = Widget_Button(buttonBase, Value='Read Entire File', UVALUE='READ_FILE_FROM_GUI')
   button = Widget_Button(buttonBase, Value='Open New File', UVALUE='OPEN_NEW_FILE')
   button = Widget_Button(buttonBase, Value='Exit', UVALUE='QUIT_BROWSER')
   
   ; Get the geometries of the tree widget and the button base. These
   ; will set the minimun and maximum values for resizing.
   self.geoWindow = Widget_Info(self.tlb, /GEOMETRY)
   self.geoTree = Widget_Info(theTree, /GEOMETRY)
   self.geoButton = Widget_Info(buttonBase, /GEOMETRY)
   self.geoDisplay = Widget_Info(self.textDisplay, /GEOMETRY)
   self.minxsize = self.geoDisplay.scr_xsize
   self.minysize = self.geoDisplay.scr_ysize
   
   ; Position the application and realize it.
   IF (xoffset LT 0 AND yoffset LT 0) THEN CenterTLB, self.tlb ELSE CenterTLB, self.tlb, xoffset, yoffset, /DEVICE, /NOCENTER
   Widget_Control, self.tlb, /REALIZE
   self.theTree = theTree
   XManager, 'ncdf_data', self.tlb, /NO_BLOCK, EVENT_HANDLER='NCDF_DATA_WIDGET_EVENTS', $
      CLEANUP='NCDF_DATA_WIDGET_CLEANUP'
  
   ; Send an event to start up in the summary browse look.
   event ={WIDGET_TREE_SEL}
   event.top = self.tlb
   event.id = summaryNode
   event.handler = self.tlb
   event.clicks = 1
   event.type = 0
   Widget_Control, summaryNode, Send_Event=event
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::CleanParsedStructures
   ; An internal method used to clean up pointers in names structures
   ; to prevent memory leakage from the object.
   
   ; Clean up all pointers in the global attribute structures.
   IF Ptr_Valid(self.theAttributes) THEN BEGIN
      num = N_Elements(*self.theAttributes)
      FOR j=0,num-1 DO Ptr_Free, (*self.theAttributes)[j].value
   ENDIF

   ; Clean up all pointers in the variable structures.
   IF Ptr_Valid(self.theVariables) THEN BEGIN
      num = N_Elements(*self.theVariables)
      FOR j=0,num-1 DO BEGIN
         Ptr_Free, (*self.theVariables)[j].value
         Ptr_Free, (*self.theVariables)[j].datasize
         IF Ptr_Valid((*self.theVariables)[j].var_attributes) THEN BEGIN
            attrs = *(*self.theVariables)[j].var_attributes
            attnum = N_Elements( attrs )
            FOR k=0,attnum-1 DO Ptr_Free, attrs[k].value
            Ptr_Free, (*self.theVariables)[j].var_attributes
         ENDIF
      ENDFOR
   ENDIF

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::EventHandler, event
   ; An internal method used to process events and sent them to appropriate event handler methods.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Event branching is based initially on event structure names.
   thisEvent = Tag_Names(event, /STRUCTURE_NAME)
   CASE thisEvent OF
   
     'WIDGET_BASE': BEGIN
         Widget_Control, self.textDisplay, $
            SCR_XSIZE = (event.x -(self.geoTree.scr_xsize + 8)) > self.minXSize , $
            SCR_YSIZE = (event.y -(self.geoButton.scr_ysize + 8)) > self.minYSize
            
         END
   
     'WIDGET_TREE_SEL': self -> SelectionInTree, event
         
     'WIDGET_TREE_EXPAND': 
     
     'WIDGET_BUTTON': BEGIN
         Widget_Control, event.id, GET_UVALUE=buttonUValue
         CASE buttonUValue OF
         
            'READ_ATTRIBUTE_FROM_GUI': self -> ReadAttributeFromGUI, event
            'READ_ATTRIBUTE_AND_LEAVE': self -> ReadAttributeFromGUI_Events, event
            'READ_ATTRIBUTE_AND_STAY': self -> ReadAttributeFromGUI_Events, event
            'QUIT_READ_ATTRIBUTE_GUI': self -> ReadAttributeFromGUI_Events, event

            'READ_VAR_FROM_GUI': self -> ReadVariableFromGUI, event
            'READ_AND_LEAVE': self -> ReadVariableFromGUI_Events, event
            'READ_AND_STAY': self -> ReadVariableFromGUI_Events, event
            'QUIT_READ_VARIABLE_GUI': self -> ReadVariableFromGUI_Events, event

            'READ_VARPLUS_FROM_GUI': self -> ReadVarPlusFromGUI, event
            'READ_VARPLUS_AND_STAY': self -> ReadVarPlusFromGUI_Events, event
            'READ_VARPLUS_AND_LEAVE': self -> ReadVarPlusFromGUI_Events, event
            'QUIT_READ_VARPLUS_GUI': self -> ReadVarPlusFromGUI_Events, event
            
            'READ_FILE_FROM_GUI': self -> ReadFileFromGUI, event
            'OPEN_NEW_FILE': self -> OpenNewFile, event
            'QUIT_BROWSER': Widget_Control, event.top, /DESTROY
            'APPEND_FILENAME': 
            
            ELSE: Print, 'No case for ' + buttonUValue
         ENDCASE
         END
         
      'WIDGET_DROPLIST': BEGIN
         theName = Widget_Info(event.id, /UNAME)
         CASE theName OF
            'VARIABLES': self -> ReadVariableFromGUI_Events, event
            'VARIABLESPLUS': self -> ReadVarPlusFromGUI_Events, event     
            'ATTRIBUTES': self -> ReadAttributeFromGUI_Events, event
         ENDCASE
         END
         
      'WIDGET_TEXT_CH':
      
      ELSE: BEGIN
               ok = Dialog_Message('Unrecognized event in EventHandler method. See Console for details.')
               HELP, event, /Structure
            END
      
   ENDCASE 
END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::Destroy_From_Browser
   RETURN, self.destroy_from_browser
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::OpenNewFile, event

   ; Creates a dialog for the user to specify the name of a new netCDF file to open.
   ; Loads the file into the object.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Get the name of a new file. Look in the directory of the last file.
   filename = Dialog_Pickfile(FILTER=self.extension , PATH=self.directory, $
      /READ, TITLE='Select a netCDF File to Open')
   IF filename EQ "" THEN RETURN
   
   ; Open the new file.
   self -> OpenFile, filename
   
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::OpenFile, filename
;+
; NAME:
;       NCDF_DATA::OpenFile
;
; PURPOSE:
;
;       This method is used to open a new netCDF file and add it to the object.
;
; CALLING SEQUENCE:
;
;       IDL> nCDFObject -> OpenFile, filename
;
; ARGUMENTS:
;
;       filename:  The name of a netCDF file to open.
;
; KEYWORD PARAMETERS:
;       
;       None.
;-

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF

   IF N_Elements(filename) NE 0 THEN BEGIN
   
      ; Clean up from the old file that will here before.
      self -> CleanParsedStructures
      
      ; Set the filename and directory name.
      directory = File_Dirname(filename)
      IF directory EQ "" THEN CD, CURRENT=directory
      self.directory = directory
      self.filename = File_Basename(filename)
      
      ; Parse the new file.
      self.hasbeenParsed = 0
      self -> ParseFile
      
      ; If a browser is currently displayed, kill it and display
      ; a browser in the same location with new file information.
      IF Widget_Info(self.tlb, /Valid_ID) THEN BEGIN
         thisState = self.destroy_from_browser
         IF thisState THEN self.destroy_from_browser = 0
         Widget_Control, self.tlb, TLB_GET_OFFSET=offsets
         Widget_Control, self.tlb, /DESTROY
         self -> Browse, XOFFSET=offsets[0], YOFFSET=offsets[1]
         self.destroy_from_browser = thisState
      ENDIF
      
   ENDIF ELSE Message, 'Must pass name of file to open.'  
   
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ParseFile
   ; This internal method parses the new netCDF file initially, and creates 
   ; the IDL structures necessary for browsing the object.
   
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      self.hasBeenParsed = 0
      IF N_Elements(fileID) NE 0 THEN NCDF_Close, fileID
      RETURN
   ENDIF

   ; Check to see if file is available.
   IF self.filename EQ "" THEN BEGIN
      ok = Dialog_Message('Add a file to the NCDF_DATA object before proceeding.')
      RETURN
   ENDIF
   
   ; Be sure everything is cleaned up and ready to go.
   self -> CleanParsedStructures
   Ptr_Free, self.theAttributes
   Ptr_Free, self.theDimensions
   Ptr_Free, self.theVariables

   ; Open the file and find out how many dimensions, global attributes, and variables are there.
   fileID = NCDF_Open(Filepath(ROOT_DIR=self.directory, self.filename))
   info = NCDF_Inquire(fileID)

   ; First, get the global attributes.
   num_attr = info.ngatts
   IF num_attr GT 0 THEN BEGIN
      theAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, num_attr)
      FOR j=0,num_attr-1 DO BEGIN
          attribute_name = NCDF_AttName(fileID, j, /GLOBAL)
          NCDF_AttGet, fileID, attribute_name, theAttribute, /GLOBAL
          attinfo = NCDF_ATTINQ(fileID, attribute_name, /GLOBAL)
          att_type = StrUpCase(attinfo.dataType)
          
          ; Strings are stored as byte values in attributes, so convert them back.
          IF (Size(theAttribute, /TNAME) EQ 'BYTE') AND (att_type EQ 'CHAR') $
            THEN theAttribute = String(theAttribute)
          theAttributes[j].attrType = 'GLOBAL'
          theAttributes[j].dataType = att_type
          theAttributes[j].length = N_Elements(theAttribute)
          theAttributes[j].name = attribute_name
          theAttributes[j].value = Ptr_New(theAttribute)

      ENDFOR
      self.theAttributes = Ptr_New(theAttributes, /No_Copy)
   ENDIF

   ; Next, get the dimensions.
   num_dims = info.ndims
   IF num_dims GT 0 THEN BEGIN
      theDimensions = REPLICATE({NCDF_DATA_DIMENSION}, num_dims)
      FOR j=0,num_dims-1 DO BEGIN
          NCDF_DIMINQ, fileID, j, dimension_name, dimension_size
          theDimensions[j].name = dimension_name
          theDimensions[j].value = String(dimension_size)
      ENDFOR
      self.theDimensions = Ptr_New(theDimensions, /No_Copy)
   ENDIF

   ; Next, get the variables.
   num_vars = info.nvars
   IF num_vars GT 0 THEN BEGIN
      theVariables = REPLICATE({NCDF_DATA_VARIABLE}, num_vars)
      FOR j=0,num_vars-1 DO BEGIN

         ; Get information about the variable.
         varinfo = NCDF_VarInq(fileID, j)
         theVariables[j].datatype = varinfo.datatype
         theVariables[j].name = varinfo.name

          ; If this variable has attributes, get those, too.
          IF varinfo.natts GT 0 THEN BEGIN
               varAttributes = Replicate({NCDF_DATA_ATTRIBUTE}, varinfo.natts)
               FOR k=0,varinfo.natts-1 DO BEGIN
                   attribute_name = NCDF_AttName(fileID, j, k)
                   NCDF_AttGet, fileID, j, attribute_name, theAttribute
                   attinfo = NCDF_ATTINQ(fileID, j, attribute_name)
                   att_type = StrUpCase(attinfo.dataType)
          
                   ; Strings are stored as byte values in attributes, so convert them back.
                   IF (Size(theAttribute, /TNAME) EQ 'BYTE') AND (att_type EQ 'CHAR') $
                     THEN theAttribute = String(theAttribute)
                   varAttributes[k].attrType = StrUpCase(varinfo.name)
                   varAttributes[k].dataType = att_type
                   varAttributes[k].length = N_Elements(theAttribute)
                   varAttributes[k].name = attribute_name
                   varAttributes[k].value = Ptr_New(theAttribute)
               ENDFOR
               theVariables[j].var_attributes = Ptr_New(varAttributes)
          ENDIF

          ; Now, read the data so you can collect information about it.
          NCDF_VarGet, fileID, j, data
          theVariables[j].dataSize = Ptr_New(Size(data, /DIMENSIONS))
          minData = Min(data, MAX=maxData)
          theVariables[j].minValue = minData
          theVariables[j].maxValue = maxData
          Undefine, data
          
      ENDFOR
      self.theVariables = Ptr_New(theVariables, /No_Copy)
   ENDIF

   ; Successfully parsed file.
   self.hasBeenParsed = 1
   
   ; Close the file
   NCDF_Close, fileID

END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::ReadAttribute, theAttribute, SUCCESS=success
;+
; NAME:
;       NCDF_DATA::ReadAttribute
;
; PURPOSE:
;
;       This method is used to read and return a global attribute from a netCDF file.
;
; CALLING SEQUENCE:
;
;       IDL> value = nCDFObject -> ReadAttribute(theAttribute)
;
; RETURN VALUE:
;
;       value:      A variable containing the attribute.
;
; ARGUMENTS:
;
;       theAttribute: The name of the attribute you wish to read from the file.
;
; KEYWORD PARAMETERS:
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;-

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      success = 0
      RETURN, -1
   ENDIF
   
   ; Make sure the file has been parsed.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile
   
   ; Check again.
   success = 0
   IF self.hasBeenParsed EQ 0 THEN RETURN, -1
   
   ; Check for the name of the attribute.
   IF N_Elements(theAttribute) EQ 0 THEN Message, 'Must pass name of the attribute to read.'

   IF Ptr_Valid(self.theAttributes) EQ 0 THEN Message, 'No global attributes currently available for file.'
   index = Where(StrUpCase((*self.theAttributes).name) EQ StrUpCase(theAttribute), count)
   IF count GT 0 THEN BEGIN
      value = *(*self.theAttributes)[index].value
      success = 1
   ENDIF ELSE Message, 'Cannot locate global attribute ' + theAttribute + ' in file.'
   RETURN, value

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadAttributeFromGUI, event
   ; This internal method sets up a dialog for obtaining information from the user
   ; about which variables to read, etc.
   
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Get some position information.
   Widget_Control, event.top, TLB_GET_OFFSET=offsets

   ; We want a modal pop-up dialog widget.
   tlb = Widget_Base(GROUP_LEADER=event.top, XOFFSET=offsets[0]+50, YOFFSET=offsets[1]+50, $
      COLUMN=1, BASE_ALIGN_CENTER=1, /FLOATING, UVALUE=self, /MODAL)
   row = Widget_Base(tlb, ROW=2, /GRID_LAYOUT, FRAME=1)
   label = Widget_Label(row, Value='Attribute to Read: ')
   theList = ['All', (*self.theAttributes).name]
   self.attributeID = Widget_Droplist(row, Value=theList, UNAME='ATTRIBUTES', $
      UVALUE=['all_attributes', (*self.theAttributes).name], SCR_XSIZE=250)
   label = Widget_Label(row, Value='Attribute Name: ')
   self.attrnameID = Widget_Text(row, Value='all_attributes', /Editable, SCR_XSIZE=250)
   b = Widget_Base(tlb, ROW=1, XPAD=0, YPAD=0, /NONEXCLUSIVE)
   
   okToAppend = 1
   IF StrPos(FSC_BASE_FILENAME(self.filename), '.') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, '-') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, ' ') NE -1 THEN okToAppend = 0
   IF okToAppend THEN self.appendNameID = Widget_Button(b, Value='Append Filename to Attribute Name', UVALUE='APPEND_FILENAME')
   buttonrow = Widget_Base(tlb, ROW=1)
   button = Widget_Button(buttonrow, Value='Read Attribute and Leave', UVALUE='READ_ATTRIBUTE_AND_LEAVE')
   button = Widget_Button(buttonrow, Value='Read Attribute and Stay', UVALUE='READ_ATTRIBUTE_AND_STAY')
   button = Widget_Button(buttonrow, Value='Quit', UVALUE='QUIT_READ_ATTRIBUTE_GUI')
   
   ; If there is a tree selection, see if this corresponds to a variable in the list.
   ; If so, set this variable in the droplist widget.
   theSelection = Widget_Info(self.theTree, /TREE_SELECT)
   Widget_Control, theSelection, Get_Value=attrName
   index = Where(theList EQ attrName, count)
   IF count GT 0 THEN BEGIN
      Widget_Control, self.attributeID, SET_DROPLIST_SELECT=index
      Widget_Control, self.attrnameID, Set_Value=IDL_ValidName(theList[index], /CONVERT_ALL)
   ENDIF
   
   ; Get it going...
   Widget_Control, tlb, /REALIZE
   XMANAGER, 'read_attribute_and_leave', tlb, EVENT_HANDLER='NCDF_DATA_WIDGET_EVENTS', /NO_BLOCK
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadAttributeFromGUI_Events, event
   ; This internal method processes events from the user dialogs.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   thisEvent = Tag_Names(event, /STRUCTURE_NAME)
   CASE thisEvent OF
   
      'WIDGET_BUTTON': BEGIN
      
         Widget_Control, event.id, Get_UValue=buttonValue
         CASE buttonValue OF
         
            'READ_ATTRIBUTE_AND_STAY': BEGIN
            
               ; Get the variable name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
               Widget_Control, self.attrNameID, Get_Value=attrName
               thisAttrName = IDL_ValidName(attrName, /CONVERT_ALL)
               IF thisAttrName NE attrName THEN BEGIN
                  Widget_Control, self.attrNameID, Set_Value=thisAttrName       
                  attrName = thisAttrName
               ENDIF
               
               attrName = (addName) ? FSC_Base_FileName(self.filename) + '_' + attrName[0] : attrName[0]
               IF attrName EQ "" THEN Message, 'Must have a non-null attribute name to create an attribute.'
               
               ; Which attribute do you want to read?
               Widget_Control, self.attributeID, Get_Value=theList
               index = Widget_Info(self.attributeID, /DROPLIST_SELECT)
               theAttribute = theList[index]
               IF StrUpCase(theAttribute) EQ 'ALL' THEN BEGIN
                   theData = self -> ReadGlobalAttr(Success=success)
                   IF success EQ 0 THEN RETURN
               ENDIF ELSE BEGIN
                   theData = self -> ReadAttribute(theAttribute, Success=success)
                   IF success EQ 0 THEN RETURN
               ENDELSE
               
               ; Create the variable at the main IDL level. 
               (Scope_VarFetch(attrName, LEVEL=1, /ENTER)) = theData
               Print, 'An attribute named "' + attrName + '" has been created at the main IDL level.'
               
               ; Go to the next attribute on the list
               IF index EQ (N_Elements(theList)-1) THEN index = 0 ELSE index = index + 1
               Widget_Control, self.attributeID, SET_DROPLIST_SELECT=index
               Widget_Control, self.attrNameID, Set_Value=theList[index]
               END
               
            'READ_ATTRIBUTE_AND_LEAVE': BEGIN
               
               ; Get the attribute name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
               Widget_Control, self.attrNameID, Get_Value=attrName
               attrName = (addName) ? FSC_Base_FileName(self.filename) + '_' + attrName[0] : attrName[0]
               thisAttrName = IDL_ValidName(attrName, /CONVERT_ALL)
               IF thisAttrName NE attrName THEN BEGIN
                  Widget_Control, self.attrNameID, Set_Value=thisAttrName       
                  attrName = thisAttrName
               ENDIF
               IF attrName EQ "" THEN Message, 'Must have a non-null attribute name to create an attribute.'
               
               ; Which attribute do you want to read?
               Widget_Control, self.attributeID, Get_Value=theList
               index = Widget_Info(self.attributeID, /DROPLIST_SELECT)
               theAttribute = theList[index]
               IF StrUpCase(theAttribute) EQ 'ALL' THEN BEGIN
                   theData = self -> ReadGlobalAttr(Success=success)
                   IF success EQ 0 THEN RETURN
               ENDIF ELSE BEGIN
                   theData = self -> ReadAttribute(theAttribute, Success=success)
                   IF success EQ 0 THEN RETURN
               ENDELSE
               
               ; Create the attribute at the main IDL level.
               (Scope_VarFetch(attrName, LEVEL=1, /ENTER)) = theData
               Print, 'An attribute named "' + attrName + '" has been created at the main IDL level.'
               
               Widget_Control, event.top, /DESTROY
               END
               
            'QUIT_READ_ATTRIBUTE_GUI': Widget_Control, event.top, /DESTROY
            
         ENDCASE
      
         END
         
      'WIDGET_DROPLIST': BEGIN
         ; The name of the variable to write has to be changed when the droplist value changes.
         Widget_Control, event.id, Get_UVALUE=list
         Widget_Control, self.attrNameID, Set_Value=IDL_ValidName(list[event.index], /CONVERT_ALL)
         END
   
      'WIDGET_TEXT': ; Nothing to do here. We just want to read the value. Don't what it is.
      
   ENDCASE
END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::ReadDimension, dimensionName, SUCCESS=success
;+
; NAME:
;       NCDF_DATA::ReadDimension
;
; PURPOSE:
;
;       This method is used to read and return a dimension of a netCDF file.
;
; CALLING SEQUENCE:
;
;       IDL> dimension = nCDFObject -> ReadDimension(dimensionName)
;
; RETURN VALUE:
;
;       dimension: The value of the dimension.
;
; ARGUMENTS:
;
;       dimensionName:   The name of the dimension to read.
;
; KEYWORD PARAMETERS:
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;-
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      success = 0
      RETURN, -1
   ENDIF
      
   ; Check for the name of the file.
   theFile = Filepath(ROOT_DIR=self.directory, self.filename)

   ; Open the file.
   fileID = NCDF_Open(theFile)
   info = NCDF_Inquire(fileID)
   
   ; Add the dimensions.
   IF info.ndims GT 0 THEN BEGIN
      dimsStruct = Create_Struct('ndims', info.ndims)
      FOR j=0,info.ndims-1 DO BEGIN
         NCDF_DIMINQ, fileID, j, name, value
         name = IDL_ValidName(name, /CONVERT_ALL)
         dimsStruct = Create_Struct(dimsStruct, name, value)
       ENDFOR
    ENDIF
   
   ; Can you find a field in the structure with the dimension name?
   fields = Tag_Names(dimsStruct)
   i = Where(fields EQ StrUpCase(dimensionName), count)
   IF count EQ 0 THEN Message, 'Cannot find a dimension with name: ' + dimensionName + ' in file.'
   value = dimsStruct.(i)
  
   ; Close the file, set status flag, return the data.
   NCDF_CLOSE, fileID
   success = 1
   RETURN, value

END ;---------------------------------------------------------------------------------------------




FUNCTION NCDF_DATA::ReadFile, theFile, SUCCESS=success
;+
; NAME:
;       NCDF_DATA::ReadFile
;
; PURPOSE:
;
;       This method is used to read and return the contents of a netCDF file.
;
; CALLING SEQUENCE:
;
;       IDL> data = nCDFObject -> ReadFile(theFile)
;
; RETURN VALUE:
;
;       data:      A structure variable containing the filename, a structure of global attributes,
;                  a structure of dimensions, and one struture for each variable in the file.
;
; ARGUMENTS:
;
;       theFile:   The optional name of a netCDF file to read. If not supplied, the
;                  name of the file currently stored in the object will be read.
;
; KEYWORD PARAMETERS:
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;-

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      success = 0
      RETURN, -1
   ENDIF
      
   ; Check for the name of the file.
   IF N_Elements(theFile) EQ 0 THEN theFile = Filepath(ROOT_DIR=self.directory, self.filename)
   IF File_Test(theFile, /READ) EQ 0 THEN Message, 'Specified file does not exist or is not readable.'

   ; Open the file.
   fileID = NCDF_Open(theFile)
   info = NCDF_Inquire(fileID)
   
   ; Create the initial structure.
   struct = Create_Struct('_filename', self.filename)
   
   ; Add the global attributes.
   g_attributes = self -> ReadGlobalAttr(Success=success)
   IF success THEN struct = Create_Struct(struct, '_global_attr', Temporary(g_attributes))

   ; Add the dimensions.
   IF info.ndims GT 0 THEN BEGIN
      dimsStruct = Create_Struct('_ndims', info.ndims)
      FOR j=0,info.ndims-1 DO BEGIN
         NCDF_DIMINQ, fileID, j, name, value
         name = IDL_ValidName(name, /CONVERT_ALL)
         dimsStruct = Create_Struct(dimsStruct, name, value)
       ENDFOR
       struct = Create_Struct(struct, '_dimensions', dimsStruct)
   ENDIF
   
   ; Add the variables.
   IF info.nvars GT 0 THEN BEGIN
      FOR j=0,info.nvars-1 DO BEGIN
         varInfo = NCDF_VarInq(fileID, j)
         NCDF_VarGet, fileID, j, data
         IF Size(data, /N_DIMENSIONS) GT 0 THEN data = REFORM(Temporary(data))
         varStruct = Create_Struct('data', Temporary(data))
         
         ; Add the variable attributes to the structure.
         FOR k=0,varInfo.natts-1 DO BEGIN
            attrName = NCDF_AttName(fileID, j, k)
            NCDF_AttGet, fileID, j, attrName, value
            attinfo = NCDF_ATTINQ(fileID, attrName)
            att_type = StrUpCase(attinfo.dataType)
          
            ; Strings are stored as byte values in attributes, so convert them back.
            IF (Size(value, /TNAME) EQ 'BYTE') AND (att_type EQ 'CHAR') $
               THEN value = String(theAttribute)
            
             attrName = IDL_ValidName(attrName, /CONVERT_ALL)
            varStruct = Create_Struct(varStruct, attrName, value)
         ENDFOR
     struct = Create_Struct(struct, varInfo.name, Temporary(varStruct))
      ENDFOR
   ENDIF
   
   ; Close the file, set status flag, return the data.
   NCDF_CLOSE, fileID
   success = 1
   RETURN, struct

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadFileFromGUI, event
   ; This internal method obtains the name of the variable from the user
   ; and creates a variable of that name at the main IDL level.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF

   ; What would you like to name the variable.
   varName = TextBox(Title='Name of IDL Variable...', Label='Name of IDL Variable:', $
      Value='data', Cancel=cancelled)
   IF cancelled THEN RETURN
   varName = IDL_ValidName(varName, /CONVERT_ALL)
   IF varName EQ "" THEN Message, 'Variable names cannot be NULL.'
   
   ; Read the NCDF data file.
   Widget_Control, /HOURGLASS
   data = self -> ReadFile(Success=success)
   IF success EQ 0 THEN RETURN
   
   ; Create a main-level variable.
   (Scope_VarFetch(varName, LEVEL=1, /ENTER)) = data
    Print, 'A variable named "' + varName + '" has been created at the main IDL level.'
END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::ReadGlobalAttr, SUCCESS=success
;+
; NAME:
;       NCDF_DATA::ReadGlobalAttr
;
; PURPOSE:
;
;       This method is used to read and return the global attributes of a netCDF file.
;
; CALLING SEQUENCE:
;
;       IDL> struct = nCDFObject -> ReadGlobalAttr()
;
; RETURN VALUE:
;
;       struct:      A structure variable containing global attributes of the file.
;                    The attribute names are the fields of the structure.
;
; ARGUMENTS:
;
;       None. The global attributes of the file loaded into the object will be read and returned.
;
; KEYWORD PARAMETERS:
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;-

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      stop
      void = Error_Message()
      success = 0
      RETURN, -1
   ENDIF
   
   ; Make sure the file has been parsed.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile
   
   ; Check again.
   success = 0
   IF self.hasBeenParsed EQ 0 THEN RETURN, -1

   ; Open the file.
   fileID = NCDF_Open(Filepath(ROOT_DIR=self.directory, self.filename))
   info = NCDF_Inquire(fileID)

   ; Create a structure to hold the global attribute values.
   attrStruct = Create_Struct('filename', self.filename)
   FOR j=0,info.ngatts-1 DO BEGIN
      name = NCDF_AttName(fileID, j, /GLOBAL)
      NCDF_AttGet, fileID, name, value, /GLOBAL
      attinfo = NCDF_ATTINQ(fileID, name, /GLOBAL)
      att_type = StrUpCase(attinfo.dataType)
      IF Size(value, /TNAME) EQ 'BYTE' AND att_type EQ 'CHAR' THEN value = String(value)
      
      ; Names cannot have oddball characters in them. They have to
      ; conform to IDL's rules for creating variable names.
      name = IDL_ValidName(name, /CONVERT_ALL)
      attrStruct = Create_Struct(attrStruct, name, value)
   ENDFOR

   ; Close the file, set status flag, return the data.
   NCDF_CLOSE, fileID
   success = 1
   RETURN, attrStruct

END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::ReadVariable, theVariable, SUCCESS=success
;+
; NAME:
;       NCDF_DATA::ReadVariable
;
; PURPOSE:
;
;       This method is used to read and return a variable from a netCDF file.
;
; CALLING SEQUENCE:
;
;       IDL> data = nCDFObject -> ReadVariable(theVariable)
;
; RETURN VALUE:
;
;       data:      The nCDF variable.
;
; ARGUMENTS:
;
;       theVariable: The name of the variable you wish to read from the file.
;
; KEYWORD PARAMETERS:
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;-

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      success = 0
      RETURN, -1
   ENDIF
   
   ; Check for the name of the variable.
   IF N_Elements(theVariable) EQ 0 THEN Message, 'Must pass name of variable to read.'

   ; Open the file.
   fileID = NCDF_Open(Filepath(ROOT_DIR=self.directory, self.filename))
   
   ; Get the variable ID.
   varID = NCDF_VarID(fileID, theVariable)
   
   ; Read the data.
   NCDF_VarGet, fileID, varID, data
   
   ; Close the file, set status flag, return the data.
   NCDF_CLOSE, fileID
   success = 1
   RETURN, data

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadVariableFromGUI, event
   ; This internal method sets up a dialog for obtaining information from the user
   ; about which variables to read, etc.
   
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   ; Get some position information.
   Widget_Control, event.top, TLB_GET_OFFSET=offsets

   ; We want a modal pop-up dialog widget.
   tlb = Widget_Base(GROUP_LEADER=event.top, XOFFSET=offsets[0]+50, YOFFSET=offsets[1]+50, $
      COLUMN=1, BASE_ALIGN_CENTER=1, /FLOATING, UVALUE=self, /MODAL)
   row = Widget_Base(tlb, ROW=2, /GRID_LAYOUT, FRAME=1)
   label = Widget_Label(row, Value='Variable to Read: ')
   theList = [(*self.theVariables).name]
   self.variablelistID = Widget_Droplist(row, Value=[(*self.theVariables).name], $
      UVALUE=[(*self.theVariables).name], SCR_XSIZE=250, UNAME='VARIABLES')
   label = Widget_Label(row, Value='Variable Name: ')
   self.varnameID = Widget_Text(row, Value=IDL_ValidName((*self.theVariables)[0].name, /CONVERT_ALL), $
      /Editable, SCR_XSIZE=250)
   b = Widget_Base(tlb, ROW=1, XPAD=0, YPAD=0, /NONEXCLUSIVE)
   
   okToAppend = 1
   IF StrPos(FSC_BASE_FILENAME(self.filename), '.') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, '-') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, ' ') NE -1 THEN okToAppend = 0
   IF okToAppend THEN self.appendNameID = Widget_Button(b, Value='Append Filename to Variable Name', UVALUE='APPEND_FILENAME')
   buttonrow = Widget_Base(tlb, ROW=1)
   button = Widget_Button(buttonrow, Value='Read Variable and Leave', UVALUE='READ_AND_LEAVE')
   button = Widget_Button(buttonrow, Value='Read Variable and Stay', UVALUE='READ_AND_STAY')
   button = Widget_Button(buttonrow, Value='Quit', UVALUE='QUIT_READ_VARIABLE_GUI')
   
   ; If there is a tree selection, see if this corresponds to a variable in the list.
   ; If so, set this variable in the droplist widget.
   theSelection = Widget_Info(self.theTree, /TREE_SELECT)
   Widget_Control, theSelection, Get_Value=varName
   index = Where(theList EQ varName, count)
   IF count GT 0 THEN BEGIN
      Widget_Control, self.variablelistID, SET_DROPLIST_SELECT=index
      Widget_Control, self.varnameID, Set_Value=IDL_ValidName(theList[index], /CONVERT_ALL)
   ENDIF
   
   ; Get it going...
   Widget_Control, tlb, /REALIZE
   XMANAGER, 'read_and_leave', tlb, EVENT_HANDLER='NCDF_DATA_WIDGET_EVENTS', /NO_BLOCK
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadVariableFromGUI_Events, event
   ; This internal method processes events from the user dialogs.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   thisEvent = Tag_Names(event, /STRUCTURE_NAME)
   CASE thisEvent OF
   
      'WIDGET_BUTTON': BEGIN
      
         Widget_Control, event.id, Get_UValue=buttonValue
         CASE buttonValue OF
         
            'READ_AND_STAY': BEGIN
            
               ; Get the variable name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
               Widget_Control, self.varNameID, Get_Value=varName
               varName = (addName) ? FSC_Base_FileName(self.filename) + '_' + varName[0] : varName[0]
               thisVarName = IDL_ValidName(varName, /CONVERT_ALL)
               IF thisVarName NE varName THEN BEGIN
                  Widget_Control, self.varNameID, Set_Value=IDL_ValidName(thisVarName, /CONVERT_ALL)       
                  varName = thisVarName
               ENDIF
               IF varName EQ "" THEN Message, 'Must have a non-null variable name to create a variable.'
               
               ; Which variable do you want to read?
               Widget_Control, self.variableListID, Get_UValue=theList
               index = Widget_Info(self.variableListID, /DROPLIST_SELECT)
               theVariable = theList[index]
               Widget_Control, /HOURGLASS
               theData = self -> ReadVariable(theVariable, Success=success)
               IF success EQ 0 THEN RETURN
               
               ; Create the variable at the main IDL level.
               (Scope_VarFetch(varName, LEVEL=1, /ENTER)) = theData
               Print, 'A variable named "' + varName + '" has been created at the main IDL level.'
               
               ; Go to the next variable on the list
               IF index EQ (N_Elements(theList)-1) THEN index = 0 ELSE index = index + 1
               Widget_Control, self.variableListID, SET_DROPLIST_SELECT=index
               Widget_Control, self.varnameID, Set_Value=IDL_ValidName(theList[index], /CONVERT_ALL)
               END
               
            'READ_AND_LEAVE': BEGIN
               
               ; Get the variable name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
               Widget_Control, self.varNameID, Get_Value=varName
               varName = (addName) ? FSC_Base_FileName(self.filename) + '_' + varName[0] : varName[0]
               IF varName EQ "" THEN Message, 'Must have a non-null variable name to create a variable.'
               
               ; Which variable do you want to read?
               Widget_Control, self.variableListID, Get_UValue=theList
               index = Widget_Info(self.variableListID, /DROPLIST_SELECT)
               theVariable = theList[index]
               Widget_Control, /HOURGLASS
               theData = self -> ReadVariable(theVariable, Success=success)
               IF success EQ 0 THEN RETURN
              
               ; Create the variable at the main IDL level.
               (Scope_VarFetch(varName, LEVEL=1, /ENTER)) = theData
               Print, 'A variable named "' + varName + '" has been created at the main IDL level.'
               
               Widget_Control, event.top, /DESTROY
               END
               
            'QUIT_READ_VARIABLE_GUI': Widget_Control, event.top, /DESTROY
            
         ENDCASE
      
         END
         
      'WIDGET_DROPLIST': BEGIN
         ; The name of the variable to write has to be changed when the droplist value changes.
         Widget_Control, event.id, Get_UVALUE=list
         Widget_Control, self.varNameID, Set_Value=IDL_ValidName(list[event.index], /CONVERT_ALL)
         END
   
      'WIDGET_TEXT': ; Nothing to do here. We just want to read the value. Don't what it is.
      
   ENDCASE
END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::ReadVariableWithAttr, theVariable, SUCCESS=success
;+
; NAME:
;       NCDF_DATA::ReadVariableWithAttr
;
; PURPOSE:
;
;       This method is used to read and return a variable and its attributes from a netCDF file.
;
; CALLING SEQUENCE:
;
;       IDL> struct = nCDFObject -> ReadVariable(theVariable)
;
; RETURN VALUE:
;
;       struct:      A structure containing the variable (in the field "data") and its
;                    attributes.
;
; ARGUMENTS:
;
;       theVariable: The name of the variable you wish to read from the file.
;
; KEYWORD PARAMETERS:
;       
;       SUCCESS:    An output parameter, set to 1 if the file was read successfully,
;                   and to 0 otherwise.
;-

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      success = 0
      RETURN, -1
   ENDIF
   
   ; Make sure the file has been parsed.
   IF self.hasBeenParsed EQ 0 THEN self -> ParseFile
   
   ; Check again.
   success = 0
   IF self.hasBeenParsed EQ 0 THEN RETURN, -1

   ; Check for the name of the variable.
   IF N_Elements(theVariable) EQ 0 THEN Message, 'Must pass name of variable to read.'

   ; Open the file.
   fileID = NCDF_Open(Filepath(ROOT_DIR=self.directory, self.filename))
   
   ; Get the variable ID.
   varID = NCDF_VarID(fileID, theVariable)
   varInfo = NCDF_VarInq(fileID, varID)
   
   ; Read the variable.
   NCDF_VarGet, fileID, varID, data
   IF Size(data, /N_DIMENSIONS) GT 0 THEN data = REFORM(Temporary(data))
   varStruct = Create_Struct('data', Temporary(data))
         
   ; Add the variable attributes to the structure.
   FOR k=0,varInfo.natts-1 DO BEGIN
       attrName = NCDF_AttName(fileID, varID, k)
       NCDF_AttGet, fileID, varID, attrName, value
       IF Size(value, /TNAME) EQ 'BYTE' THEN value = String(value)
       attrName = IDL_ValidName(attrName, /CONVERT_ALL)
       varStruct = Create_Struct(varStruct, attrName, value)
   ENDFOR
   
   ; Close the file, set status flag, return the data.
   NCDF_CLOSE, fileID
   success = 1
   RETURN, varstruct



END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadVarPlusFromGUI, event
   ; This internal method sets up a dialog for obtaining information from the user
   ; about which variables to read, etc.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF

   ; Get some position information.
   Widget_Control, event.top, TLB_GET_OFFSET=offsets

   ; We want a modal pop-up dialog widget.
   tlb = Widget_Base(GROUP_LEADER=event.top, XOFFSET=offsets[0]+50, YOFFSET=offsets[1]+50, $
      COLUMN=1, BASE_ALIGN_CENTER=1, /FLOATING, UVALUE=self, /MODAL)
   row = Widget_Base(tlb, ROW=2, /GRID_LAYOUT, FRAME=1)
   label = Widget_Label(row, Value='Variable to Read: ')
   theList = [(*self.theVariables).name]
   self.varpluslistID = Widget_Droplist(row, Value=[(*self.theVariables).name], $
      UVALUE=[(*self.theVariables).name], SCR_XSIZE=250, UNAME='VARIABLESPLUS')
   label = Widget_Label(row, Value='Variable Name: ')
   thisVarname = IDL_ValidName((*self.theVariables)[0].name + '_struct', /CONVERT_ALL)
   self.varplusnameID = Widget_Text(row, Value=thisVarname, /Editable, SCR_XSIZE=250)
   b = Widget_Base(tlb, ROW=1, XPAD=0, YPAD=0, /NONEXCLUSIVE)
   
   okToAppend = 1
   IF StrPos(FSC_BASE_FILENAME(self.filename), '.') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, '-') NE -1 THEN okToAppend = 0
   IF StrPos(self.filename, ' ') NE -1 THEN okToAppend = 0
   IF okToAppend THEN self.appendNameID = Widget_Button(b, Value='Append Filename to Variable Name', UVALUE='APPEND_FILENAME')
   buttonrow = Widget_Base(tlb, ROW=1)
   button = Widget_Button(buttonrow, Value='Read Variable and Leave', UVALUE='READ_VARPLUS_AND_LEAVE')
   button = Widget_Button(buttonrow, Value='Read Variable and Stay', UVALUE='READ_VARPLUS_AND_STAY')
   button = Widget_Button(buttonrow, Value='Quit', UVALUE='QUIT_READ_VARPLUS_GUI')
   
   ; If there is a tree selection, see if this corresponds to a variable in the list.
   ; If so, set this variable in the droplist widget.
   theSelection = Widget_Info(self.theTree, /TREE_SELECT)
   Widget_Control, theSelection, Get_Value=varName
   index = Where(theList EQ varName, count)
   IF count GT 0 THEN BEGIN
      Widget_Control, self.varpluslistID, SET_DROPLIST_SELECT=index
      Widget_Control, self.varplusnameID, Set_Value=IDL_ValidName(theList[index], /CONVERT_ALL)
   ENDIF
   
   ; Get it going...
   Widget_Control, tlb, /REALIZE
   XMANAGER, 'read_and_leave', tlb, EVENT_HANDLER='NCDF_DATA_WIDGET_EVENTS', /NO_BLOCK
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::ReadVarPlusFromGUI_Events, event
   ; This internal method processes events from the user dialogs.

   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF
   
   thisEvent = Tag_Names(event, /STRUCTURE_NAME)
   CASE thisEvent OF
   
      'WIDGET_BUTTON': BEGIN
      
         Widget_Control, event.id, Get_UValue=buttonValue
         CASE buttonValue OF
         
            'READ_VARPLUS_AND_STAY': BEGIN
            
               ; Get the variable name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
               Widget_Control, self.varPlusNameID, Get_Value=varName
               varName = (addName) ? FSC_Base_FileName(self.filename) + '_' + varName[0] : varName[0]
               thisVarName = IDL_ValidName(varName, /CONVERT_ALL)
               IF thisVarName NE varName THEN BEGIN
                  Widget_Control, self.varPlusNameID, Set_Value=IDL_ValidName(thisVarName, /CONVERT_ALL)       
                  varName = thisVarName
               ENDIF
               IF varName EQ "" THEN Message, 'Must have a non-null variable name to create a variable.'
               
               ; Which variable do you want to read?
               Widget_Control, self.varpluslistID, Get_UValue=theList
               index = Widget_Info(self.varpluslistID, /DROPLIST_SELECT)
               theVariable = theList[index]
               theData = self -> ReadVariableWithAttr(theVariable, Success=success)
               IF success EQ 0 THEN RETURN
               
               ; Create the variable at the main IDL level.
               (Scope_VarFetch(varName, LEVEL=1, /ENTER)) = theData
               Print, 'A structure variable named "' + varName + '" has been created at the main IDL level.'
               
               ; Go to the next variable on the list
               IF index EQ (N_Elements(theList)-1) THEN index = 0 ELSE index = index + 1
               Widget_Control, self.varpluslistID, SET_DROPLIST_SELECT=index
               Widget_Control, self.varplusnameID, $
                  Set_Value=IDL_ValidName(theList[index] + '_struct', /CONVERT_ALL)
               END
               
            'READ_VARPLUS_AND_LEAVE': BEGIN
               
               ; Get the variable name. Do we need to append the filename to it?
               IF Widget_Info(self.appendNameID, /Valid_ID) THEN $
                  addName = Widget_Info(self.appendNameID, /BUTTON_SET) ELSE addName = 0
               Widget_Control, self.varplusNameID, Get_Value=varName
               varName = (addName) ? FSC_Base_FileName(self.filename) + '_' + varName[0] : varName[0]
               IF varName EQ "" THEN Message, 'Must have a non-null variable name to create a variable.'
               
               ; Which variable do you want to read?
               Widget_Control, self.varpluslistID, Get_UValue=theList
               index = Widget_Info(self.varpluslistID, /DROPLIST_SELECT)
               theVariable = theList[index]
               theData = self -> ReadVariableWithAttr(theVariable, Success=success)
               IF success EQ 0 THEN RETURN
               
               ; Create the variable at the main IDL level.
               (Scope_VarFetch(varName, LEVEL=1, /ENTER)) = theData
               Print, 'A structure variable named "' + varName + '" has been created at the main IDL level.'
               
               Widget_Control, event.top, /DESTROY
               END
               
            'QUIT_READ_VARPLUS_GUI': Widget_Control, event.top, /DESTROY
            
         ENDCASE
      
         END
         
      'WIDGET_DROPLIST': BEGIN
         ; The name of the variable to write has to be changed when the droplist value changes.
         Widget_Control, event.id, Get_UVALUE=list
         Widget_Control, self.varPlusNameID, Set_Value=IDL_ValidName(list[event.index], /CONVERT_ALL)
         END
   
      'WIDGET_TEXT': ; Nothing to do here. We just want to read the value. Don't what it is.
      
   ENDCASE
END ;---------------------------------------------------------------------------------------------




PRO NCDF_DATA::SelectionInTree, event
   ; This internal method processes events from the tree widget.
   
   ; Error handling
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN
   ENDIF

   ; Create variable for better formatting.
   tab = '   '
   
   ; What to do depends on the node name of the tree selection.
   nodeName = Widget_Info(event.id, /UNAME)
   CASE nodeName OF
   
      'FOLDER': Widget_Control, self.textDisplay, Set_Value=""
               
      'GLOBAL ATTRIBUTE': BEGIN
         si = 0
         Widget_Control, event.id, Get_Value=name
         gattr = *self.theAttributes
         i = Where(gattr.name EQ name, count)
         IF count GT 0 THEN BEGIN
             text = StrArr(3)
             thisAttr = (*self.theAttributes)[i]
             IF thisAttr.dataType EQ 'STRING' OR thisAttr.dataType EQ 'CHAR' THEN BEGIN
                  str = TextLineFormat(*thisAttr.value, LENGTH=80)
                  lines = N_Elements(str)
                  text = [text, StrArr(2 + lines)]
                   text[si] = StrUpCase(thisAttr.name) + ':'
                   text[si+1:si+1+(lines-1)] = tab + str
                   si = si + 2 + lines
             ENDIF ELSE BEGIN
                   var = *thisAttr.value
                   Help, var, OUTPUT=helptext
                   text = [text, StrArr(N_Elements(helptext) + 2)]
                   text[si] = StrUpCase(thisAttr.name) + ':'
                   text[si+1] = StrMid(helptext, 3)
                   si = si + N_Elements(helptext) + 2
             ENDELSE
             Widget_Control, self.textDisplay, Set_Value=text
         ENDIF ELSE Message, 'Cannot find global attribute ' + name
         END
                  
     'DIMENSION': BEGIN
          si = 0
          Widget_Control, event.id, Get_Value=name
          dims = *self.theDimensions
          i = Where(dims.name EQ name, count)
          IF count GT 0 THEN BEGIN
             text = StrArr(3)
             thisDim = (*self.theDimensions)[i]
             text[si] = StrUpCase(thisDim.name) + ':'
             text[si+1] = tab + StrTrim(thisDim.value,2)
          ENDIF
          Widget_Control, self.textDisplay, Set_Value=text
          END
                   
     'VARIABLE': BEGIN
           Widget_Control, event.id, Get_Value=name
           vars = *self.theVariables 
           i = Where(vars.name EQ name, count)
           thisVar = vars[i]
            text = StrArr(6)
            text[0] = tab + 'NAME: ' + name
            text[1] = tab + 'DATATYPE: ' + thisVar.datatype
            n = StrTrim(N_Elements(*thisVar.datasize),2)
            f = ' (' + n + '(I0, :, ", "))'
            d = String(*thisVar.datasize, FORMAT=f)
            text[2] = tab + 'N_DIMENSIONS:  ' + StrTrim(N_Elements(*thisVar.datasize),2)
            text[3] = tab + 'DIMENSIONS:  [' + d + ']'
            text[4] = tab + 'MIN VALUE:  ' + StrTrim(thisVar.minValue,2)
            text[5] = tab + 'MAX VALUE:  ' + StrTrim(thisVar.maxValue,2)
                  
            Widget_Control, self.textDisplay, Set_Value=text
               
            END
                  
      'FILENAME': BEGIN
             text = StrArr(3)
             text[0] = 'FILENAME:'
             text[1] = tab + self.filename
             Widget_Control, self.textDisplay, Set_Value=text
             END
                  
      'DIRECTORY': BEGIN
                  text = StrArr(3)
                  text[0] = 'DIRECTORY:'
                  text[1] = tab + self.directory
                  Widget_Control, self.textDisplay, Set_Value=text
                  END
                  
       'SUMMARY': BEGIN
         text = StrArr(3)
         text[0] = 'DIRECTORY:'
         text[1] = tab + self.directory
         si = 3
         
         text = [text, strArr(3)]
         text[3] = 'FILENAME:'
         text[4] = tab + self.filename
         si = si + 3
         
         IF Ptr_Valid(self.theAttributes) THEN BEGIN
            attr = *self.theAttributes
            FOR j=0,N_Elements(attr)-1 DO BEGIN
               thisAttr = attr[j]
               IF thisAttr.dataType EQ 'STRING' OR thisAttr.dataType EQ 'CHAR' THEN BEGIN
                  str = TextLineFormat(*thisAttr.value, LENGTH=80)
                  lines = N_Elements(str)
                  text = [text, StrArr(2 + lines)]
                  text[si] = StrUpCase(thisAttr.name) + ':'
                  text[si+1:si+1+(lines-1)] = tab + str
                  si = si + 2 + lines
               ENDIF ELSE BEGIN
                var = *thisAttr.value
                Help, var, OUTPUT=helptext
                text = [text, StrArr(N_Elements(helptext) + 2)]
                text[si] = StrUpCase(thisAttr.name) + ':'
                text[si+1] = StrMid(helptext, 3)
                si = si + N_Elements(helptext) + 2
            ENDELSE
            ENDFOR
         ENDIF
         
         Widget_Control, self.textDisplay, Set_Value=text
         END
         
      'VARIABLE ATTRIBUTE': BEGIN
         si = 0
         Widget_Control, event.id, Get_UValue=value
         varname = value[0]
         name = value[1]
         vars = *self.theVariables 
         i = Where(vars.name EQ varname, count)
         thisVar = vars[i]
         var_attributes = *thisVar.var_attributes
         i = Where(var_attributes.name EQ name, count)
         IF count GT 0 THEN BEGIN
            text = StrArr(3)
            thisAttr = var_attributes[i]
            IF thisAttr.dataType EQ 'STRING' OR thisAttr.dataType EQ 'CHAR' THEN BEGIN
                  str = TextLineFormat(*thisAttr.value, LENGTH=80)
                  lines = N_Elements(str)
                  text = [text, StrArr(2 + lines)]
                  text[si] = StrUpCase(thisAttr.name) + ':'
                  text[si+1:si+1+(lines-1)] = tab + str
                  si = si + 2 + lines
             ENDIF ELSE BEGIN
                var = *thisAttr.value
                Help, var, OUTPUT=helptext
                text = [text, StrArr(N_Elements(helptext) + 2)]
                text[si] = StrUpCase(thisAttr.name) + ':'
                text[si+1] = StrMid(helptext, 3)
                si = si + N_Elements(helptext) + 2
            ENDELSE
            Widget_Control, self.textDisplay, Set_Value=text
         ENDIF ELSE Message, 'Cannot find global attribute ' + name
         END
         
         ELSE: Message, 'Unexpected event from ' + nodename + '. Please investigate.'
         
   ENDCASE 

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA::CLEANUP

   ; This is the main cleanup routine for the object. Delete all created pointers.
   self -> CleanParsedStructures
   Ptr_Free, self.theAttributes
   Ptr_Free, self.theDimensions
   Ptr_Free, self.theVariables

END ;---------------------------------------------------------------------------------------------



FUNCTION NCDF_DATA::INIT, filename, $
   BROWSE=browse, $
   DESTROY_FROM_BROWSER=destroy_from_browser, $
   EXTENSION=extension

   ; Error handling. Return 0 if can't finish.
   CATCH, theError
   IF theError NE 0 THEN BEGIN
      CATCH, /CANCEL
      void = Error_Message()
      RETURN, 0
   ENDIF

   ; Check parameters.
   IF N_Elements(filename) NE 0 THEN BEGIN
      IF File_Test(filename, /READ) EQ 0 THEN Message, 'Specified file does not exist or is not readable.'
      basename = File_BaseName(filename)
      directory = File_DirName(filename)
      IF directory EQ '.\' OR directory EQ './' THEN CD, Current=directory
      self.filename = basename
      self.directory = directory
   ENDIF
   IF N_Elements(extension) EQ 0 THEN extension = '*.nc;*.ncd;*.ncdf'

   ; Set other object properties
   self.destroy_from_browser = Keyword_Set(destroy_from_browser)
   self.extension = extension
   
   ; Browse now?
   IF Keyword_Set(browse) THEN self -> Browse
   
   RETURN, 1

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA_ATTRIBUTE__DEFINE

   struct = { NCDF_DATA_ATTRIBUTE, $
              attrtype: "", $
              datatype: "", $
              length: 0L, $
              name: "", $
              value: Ptr_New() }
              
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA_DIMENSION__DEFINE

   struct = { NCDF_DATA_DIMENSION, $
              name: "", $
              value: "" }       ; Length or UNLIMITED.

END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA_VARIABLE__DEFINE

   struct = { NCDF_DATA_VARIABLE, $
              datasize: Ptr_New(), $
              datatype: "", $
              minValue: 0.0D, $
              maxValue: 0.0D, $
              name: "", $
              var_attributes: Ptr_New(), $
              value: Ptr_New() }

END;---------------------------------------------------------------------------------------------



PRO NCDF_DATA_WIDGET_CLEANUP, tlb

   Widget_Control, tlb, GET_UVALUE=self
   IF self -> Destroy_From_Browser() THEN Obj_Destroy, self
   
END ;---------------------------------------------------------------------------------------------



PRO NCDF_DATA_WIDGET_EVENTS, event

   Widget_Control, event.TOP, GET_UVALUE=self
   self -> EventHandler, event
   
END ;---------------------------------------------------------------------------------------------




PRO NCDF_DATA__DEFINE, class

   class = { NCDF_DATA,                $  ; The object class NCDF_DATA.
             appendNameID: 0L,         $  ; The button for appending filenames to variables.
             attributeID: 0L,          $  ; The widget containing the attribute list.
             attrNameID: 0L,           $  ; The widget containing the main-level attribute name.
             filename: "",             $  ; The filename of the netCDF file.
             destroy_from_browser: 0B, $  ; A flag to indicate the object is destroyed if brower is destroyed.
             directory: "",            $  ; The directory the file is located in.
             extension: '',            $  ; The file extension for FILTER keyword in DIALOG_PICKFILE.
             geoDisplay: {WIDGET_GEOMETRY}, $ ; Widget geometries for calculating resizeable windows.
             geoWindow: {WIDGET_GEOMETRY}, $
             geoButton: {WIDGET_GEOMETRY}, $
             geoTree: {WIDGET_GEOMETRY}, $
             hasBeenParsed: 0B,        $  ; A flag to indicate if the file has been parsed.
             minXSize: 0L,             $  ; Minimum X size of the Browser window.
             minYSize: 0L,             $  ; Minimum Y size of the Browser window.             
             textDisplay: 0L,          $  ; The widget where text information is displayed
             theAttributes: Ptr_New(), $  ; An array of global attribute structures.
             theDimensions: Ptr_New(), $  ; An array of dimension structures.
             theTree: 0L,              $  ; The tree widget ID.
             theVariables: Ptr_New(),  $  ; An array of variable structures.
             tlb: 0L,                  $  ; The TLB of the browser window.
             varpluslistID: 0L,        $  ; The list of variable plus attributes to be read.
             varplusnameID: 0L,        $  ; The widget containing the main-level variable name.
             variablelistID: 0L,       $  ; The list of variables available to be read.
             varnameID: 0L             $  ; The widget containing the main-level variable name.
           }

END ;---------------------------------------------------------------------------------------------
