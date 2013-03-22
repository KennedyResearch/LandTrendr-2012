;*****************************************************************************************************
;+
; NAME:
;       GENERALINTERACTION__DEFINE
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
;       theObject = Obj_New("GENERALINTERACTION")
;
; SUPERCLASSES:
;
;       INTERACTION
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { GENERALINTERACTION, $
;             INHERITS INTERACTION $
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: Donatella Nobody, February 6, 2004.
;-
;*****************************************************************************************************
;+
; NAME:
;       GENERALINTERACTION::DRAW
;
; PURPOSE:
;
;       This method draws the interaction in the display window.
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
PRO GeneralInteraction::Draw, _Extra=extrakeywords

   @cat_pro_error_handler

   self->INTERACTION::Draw, _Extra=extrakeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;        GENERALINTERACTION::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the GENERALINTERACTION object.
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
PRO GeneralInteraction::EventHandler, event

   ; Set up the error handler
   @cat_pro_error_handler

   ; Pick the possible context menu button events out of the more like draw widget events.
   CASE event.name OF

      'ACCEPT': BEGIN

         RETURN
         END

      'CANCEL': BEGIN

         RETURN
         END

      ELSE: ; From draw widget. Fall through.

   ENDCASE

   ; Draw widget events handled here.

   ; Report completion
   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       GENERALINTERACTION::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain GENERALINTERACTION properties. Be sure
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
PRO GeneralInteraction::GetProperty, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> INTERACTION::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       GENERALINTERACTION::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the GENERALINTERACTION object's properties. Be sure
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
;     _EXTRA:       Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO GeneralInteraction::SetProperty, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> INTERACTION::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       GENERALINTERACTION::CLEANUP
;
; PURPOSE:
;
;       This is the GENERALINTERACTION object class destructor method.
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
PRO GeneralInteraction::CLEANUP

   @cat_pro_error_handler

   self -> INTERACTION::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       GENERALINTERACTION::INIT
;
; PURPOSE:
;
;       This is the GENERALINTERACTION object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     drawObject:   The draw widget object that you will be taking over events from.
;
; KEYWORDS:
;
;     _EXTRA:       Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION GeneralInteraction::INIT, drawObject, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

   ok = self -> INTERACTION::INIT (drawObject, _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       GENERALINTERACTION CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the GENERALINTERACTION object.
;
;*****************************************************************************************************
PRO GeneralInteraction__DEFINE, class

   class = { GENERALINTERACTION, $
             INHERITS INTERACTION $
           }

END


;*****************************************************************************************************
;
; NAME:
;       GENERALINTERACTION TEST PROGRAM
;
; PURPOSE:
;
;       This is the test program for the GENERALINTERACTION object. It may not
;       be required in your object.
;
;*****************************************************************************************************
PRO GeneralInteraction_Test

END