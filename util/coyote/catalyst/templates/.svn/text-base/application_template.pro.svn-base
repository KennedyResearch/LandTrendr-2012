;*****************************************************************************************************
;+
; NAME:
;       APP_TEMPLATE
;
; PURPOSE:
;
;       The purpose of this routine is to provide a template for building
;       a Catalyst application.
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
;       Objects.
;
; SYNTAX:
;
;       App_Template
;
; MODIFICATION_HISTORY:
;
;       Written by: Donatella Nobody, April 17, 2003.
;-
;*****************************************************************************************************
;+
; NAME:
;        APP_TEMPLATE::CREATESTATUSBAR
;
; PURPOSE:
;
;        This method creates the status bar at the bottom of the application.
;
; SYNTAX:
;
;        theObject -> CreateStatusBar
;
; ARGUMENTS:
;
;       baseObject:   The parent of the statusbar. If undefined, use SELF.
;
; KEYWORDS:
;
;       _EXTRA:       Any keywords appropriate for the STATUSBAR object..
;
;-
;*****************************************************************************************************
PRO App_Template::CreateStatusBar, baseObject, _Extra=extrakeywords

   IF N_Elements(baseObject) EQ 0 THEN baseObject = self
   self._statusbar = OBJ_NEW('STATUSBAR', baseObject, Name='Statusbar', /Align_Left, _Extra=extrakeywords)

END



;*****************************************************************************************************
;+
; NAME:
;        APP_TEMPLATE::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the APP_TEMPLATE object. It will typically
;        be used to respond to events from widget objects created in application.
;
; SYNTAX:
;
;        This method is called automatically by the event handling mechanism.
;
; ARGUMENTS:
;
;       event: The event structure as described in the IDL help files, except
;              that the ID, TOP and HANDLER tags will be object references.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO App_Template::EventHandler, event

   @cat_pro_error_handler

   ; Branch on the object name.
   CASE StrUpCase(event.name) OF

      'DRAWWIDGET' : BEGIN
            theText = 'X: ' + StrTrim(Fix(event.x), 2) + '  Y: ' + StrTrim(Fix(event.y), 2)
            self._statusbar -> SetProperty, Text=theText
         END

      'EXIT': OBJ_DESTROY, self

      'TLB': BEGIN
         drawObject = self -> Get("DrawWidget", /Recursive_Search)
         drawObject -> SetProperty, XSize=event.x, YSize=event.y
         drawObject -> Draw
         self._statusbar -> Resize, drawObject
         END

       ELSE :

   ENDCASE

   IF Obj_Valid(self) THEN self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       APP_TEMPLATE::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain APP_TEMPLATE properties. Be sure
;       you ALWAYS call the superclass GETPROPERTY method if you have extra
;       keywords!
;
; SYNTAX:
;
;       theObject -> GetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     _REF_EXTRA:     Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO App_Template::GetProperty,  _Ref_Extra=extrakeywords

   @cat_pro_error_handler

   IF N_Elements(extrakeywords) NE 0 THEN self -> TOPLEVELBASE::GetProperty, _Extra=extrakeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       APP_TEMPLATE::GUI
;
; PURPOSE:
;
;       This method is where the graphical user interface elements of the program
;       are created.
;
; SYNTAX:
;
;       theObject -> GUI
;
; ARGUMENTS:
;
;     menuBarID:      The identifier of the menuBar widget, if any.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO App_Template::GUI, menubarID


   ; Create a Quit button in the menu bar.
   fileMenu = OBJ_NEW ('ButtonWidget', menuBarID ,  Value='File', /Menu)
   exitBttn = OBJ_NEW ('ButtonWidget', fileMenu,  Name='Exit', Value='Exit')

   ; Create a draw widget.
   baseID = OBJ_NEW('BASEWIDGET', self, Column=1, XPAD=0, YPAD=0, SPACE=0)
   drawID = OBJ_NEW('DRAWWIDGET', baseID, XSize=400, YSize=400, NAME='DrawWidget', Motion_Events=1)

   ; Create a statusbar object.
   self -> CreateStatusBar, self

   ; Open an image and display it in the draw widget. Use the coyote.jpg image
   ; if you can find it.
   filename = Filepath(Root_Dir=ProgramRootDir(/OneUp), SubDir='cat_resources', 'coyote.jpg')
   file = File_Search(filename, Count=count)
   IF count EQ 0 THEN BEGIN
      filename = Filepath(Subdir=['examples','data'], 'elev_t.jpg')
      file = File_Search(filename, Count=count)
   ENDIF
   IF count GT 0 THEN Read_JPEG, filename, image ELSE image = Dist(256)
   drawID -> Add, OBJ_NEW('CatImageData', image)

   ; Keep draw widgets from becoming active windows.
   Widget_Control, self->getID(), /Managed

   ; Display the entire application in the window.
   self -> Draw, /Center


END

;*****************************************************************************************************
;+
; NAME:
;       APP_TEMPLATE::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set APP_TEMPLATE properties. Be sure
;       you ALWAYS call the superclass SETPROPERTY method if you have extra
;       keywords!
;
; SYNTAX:
;
;       theObject -> SetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     _EXTRA:     Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO App_Template::SetProperty,  _Extra=extrakeywords

   @cat_pro_error_handler

   IF N_Elements(extrakeywords) NE 0 THEN self -> TOPLEVELBASE::SetProperty, _Extra=extrakeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       APP_TEMPLATE::KILL_NOTIFY
;
; PURPOSE:
;
;       This method is called automatically when the application is destroyed.
;       It's primary purpose is to clean up any objects that have been created
;       to run the Catalyst system. It might also destroy any Catalyst infrastructure
;       by calling CatDestroyDefaults.
;
; SYNTAX:
;
;     None.
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO App_Template::Kill_Notify

   CatDestroyDefaults

END


;*****************************************************************************************************
;+
; NAME:
;       APP_TEMPLATE::CLEANUP
;
; PURPOSE:
;
;       This is the APP_TEMPLATE object class destructor method.
;
; SYNTAX:
;
;       Called automatically when the object is destroyed.
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;      None.
;-
;*****************************************************************************************************
PRO App_Template::CLEANUP

   @cat_pro_error_handler

   self -> TOPLEVELBASE::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       APP_TEMPLATE::INIT
;
; PURPOSE:
;
;       This is the APP_TEMPLATE object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     __REF_EXTRA:     Any keywords appropriate for the superclass INIT method.
;                      In this case you MUST use _REF_EXTRA instead of _EXTRA because
;                      you need to return the MENUBAR object reference.
;-
;*****************************************************************************************************
FUNCTION App_Template::INIT, _Ref_Extra=extra

   @cat_func_error_handler

   ; The application is contained in a top-level base widget.
   ok = self->TOPLEVELBASE::INIT(_Extra=extra, XPAD=0, YPAD=0, SPACE=0)

   IF (ok) THEN self -> Report, /Completed $
   ELSE self -> Report, /Failed
   RETURN, ok

END


;*****************************************************************************************************
;
; NAME:
;       APP_TEMPLATE Class Definition
;
; PURPOSE:
;
;       This is the structure definition code for the APP_TEMPLATE object. The basic
;       application consists of a top-level base widget with a status bar.
;
;*****************************************************************************************************
PRO App_Template__Define, class

   class = {App_Template, INHERITS TopLevelBase, _statusbar:Obj_New()}

END


;*****************************************************************************************************
;
; NAME:
;       APP_TEMPLATE Test Program
;
; PURPOSE:
;
;       This is the structure definition code for the APP_TEMPLATE object.
;
;*****************************************************************************************************
PRO App_Template, tlb

   @cat_pro_error_handler

   ; Create the widgets that make up the application. Widgets that generate
   ; events should be named explicitly and should be children (or grandchildren)
   ; of the TLB.
   tlb = OBJ_NEW('App_Template', Column=1, MBar=menubarID, /Kill_Notify, $
      Name='TLB', SIZE_EVENTS=1)
   tlb -> GUI, menubarID


END
;*****************************************************************************************************
