;*****************************************************************************************************
;+
; NAME:
;       CLASSNAME__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to provide a starting template
;       for object creation.
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
;       theObject = Obj_New("CLASSNAME")
;
; SUPERCLASSES:
;
;       SUPERCLASS
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { CLASSNAME, $
;             INHERITS SUPERCLASS $
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: Donatella Nobody, April 17, 2003.
;-
;*****************************************************************************************************
;+
; NAME:
;       CLASSNAME::ADD
;
; PURPOSE:
;
;       This method is where you can screen what kinds of objects are
;       added to this object's hierarchy. The method is not always needed.
;       If you do create it, be CERTAIN to call the superclass ADD method
;       or your program will not work correctly.
;
; SYNTAX:
;
;       theObject -> Add, object
;
; ARGUMENTS:
;
;     object:     The object to be added to this one.
;
; KEYWORDS:
;
;     _EXTRA:     Any keywords appropriate for the superclass Add method.
;-
;*****************************************************************************************************
PRO ClassName::Add, object, _EXTRA=extraKeywords

   @cat_pro_error_handler

   self -> SUPERCLASS::Add, object, _EXTRA=extraKeywords

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CLASSNAME::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the CLASSNAME object. A
;       control panel is a graphical user interface for setting object
;       properties. If you create a control panel, the events are typically
;       sent to the EVENTHANDLER method.
;
; SYNTAX:
;
;       theObject -> ControlPanel, baseObject
;
; ARGUMENTS:
;
;       baseObject:    The object reference of a base widget for this control to
;                      be added to. If not supplied, the control panel will be in a
;                      self contained window (i.e., a TOPLEVELBASE object).
;
; KEYWORDS:
;
;       _EXTRA:       Any keywords appropriate for the CatControlPanel::INIT method.
;
;-
;*****************************************************************************************************
PRO ClassName::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel.

   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Object Control Panel', _EXTRA=extraKeywords)

   IF OBJ_VALID (cp) EQ 0 THEN RETURN

   ; Create the rest of the widgets.

   base = Obj_New('BASEWIDGET', cp, Column=1, Frame=1)

   ; Display the control panel if it created its own TLB.

   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CLASSNAME::DRAW
;
; PURPOSE:
;
;       This method may or may not be needed by your object, depending
;       upon whether a graphical representation of the object is required.
;       If you wish the DRAW method to automatically propogates down to any
;       objects contained in this object's container, call the superclass DRAW
;       method.
;
; SYNTAX:
;
;       theObject -> Draw
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       _Extra:    Any keywords appropriate for the superclass DRAW method..
;
;-
;*****************************************************************************************************
PRO ClassName::Draw, Extra=extrakeywords

   @cat_pro_error_handler

   ; Draw any objects contained within this object.
   self -> SUPERCLASS::Draw, Extra=extrakeywords

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;        CLASSNAME::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the CLASSNAME object. It will typically
;        be used to respond to events from widget objects created in the CONTROLPANEL
;        method.
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
PRO ClassName::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   CASE event.name OF
      ELSE: Print, 'Received an event from: ', event.name
   ENDCASE

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CLASSNAME::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain CLASSNAME properties. Be sure
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
PRO ClassName::GetProperty, _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SUPERCLASS::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CLASSNAME::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the CLASSNAME object's properties. Be sure
;       you ALWAYS call the superclass SETPROPERTY method if you have extra keywords!
;
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
PRO ClassName::SetProperty, _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SUPERCLASS::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CLASSNAME::CLEANUP
;
; PURPOSE:
;
;       This is the CLASSNAME object class destructor method.
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
PRO ClassName::CLEANUP

   @cat_pro_error_handler

   self -> SUPERCLASS::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CLASSNAME::INIT
;
; PURPOSE:
;
;       This is the CLASSNAME object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     parent:     The parent object referece. This object will be added to the parent's container.
;
; KEYWORDS:
;
;     _EXTRA:     Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION ClassName::INIT, parent, _EXTRA=extraKeywords

   ; Set up error handler and call superclass INIT method
   @cat_func_error_handler

   ok = self -> SUPERCLASS::INIT (parent, _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       CLASSNAME TEST PROGRAM
;
; PURPOSE:
;
;       This is the test program for the CLASSNAME object. It may not
;       be required in your object.
;
;*****************************************************************************************************
PRO ClassName_Test

END



;*****************************************************************************************************
;
; NAME:
;       CLASSNAME CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the CLASSNAME object.
;
;*****************************************************************************************************
PRO ClassName__DEFINE, class

   class = { CLASSNAME, $
             INHERITS SUPERCLASS $
           }

END


