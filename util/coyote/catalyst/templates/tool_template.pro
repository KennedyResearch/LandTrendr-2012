;*****************************************************************************************************
;+
; NAME:
;       CATTOOL_TEMPLATE
;
; PURPOSE:
;
;       The purpose of this routine is to implement a template for create Catalyst tools.
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
;       aTool = Obj_New("CATTOOL_TEMPLATE")
;
; SUPERCLASSES:
;
;       CATTOOL
;       CATATOM
;       CATCONTAINER
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { CATTOOL_TEMPLATE, $    ; The CATTOOL_TEMPLATE object class.
;             INHERITS CATTOOL, $    ; Inherits the CATTOOL object class.
;           }
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, September 10, 2003.
;-
;*****************************************************************************************************
;+
; NAME:
;       CATTOOL_TEMPLATE::APPLY
;
; PURPOSE:
;
;       This method applies or executes the "tool" on the target object
;;
; SYNTAX:
;
;       thisObject -> Apply
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       DRAW:     Set this keyword if you wish to call the draw method of the target object
;                 after the command has been applied.
;-
;*****************************************************************************************************
PRO CatTool_Template::Apply, Draw=draw

   @cat_pro_error_handler

   ; Make sure there is a target object.

   IF Obj_Valid(self._theTarget) EQ 0 THEN $
      Message, 'A target object is required for tool operation.'

   ; Cache the input image unless you have been told not to.

   IF self._noCache EQ 0 THEN self -> Cache, self._theTarget -> GetData(ORIGINAL=self._original)

   ; This is the section in which the tool is applied. Code here depends entirely
   ; on the tool you are building.

   ;;;;Application code here.

   ; Need a draw method?

   IF Keyword_Set(draw) THEN self._theTarget -> Draw

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATTOOL_TEMPLATE::GETPROPERTY
;
; PURPOSE:
;
;       This method enables the getting of the CATTOOL_TEMPLATE object class
;       properties.
;
; SYNTAX:
;
;       aGraphicsCmd -> GetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     _EXTRA:           Any keywords appropriate for the superclass SetProperty methods.
;-
;*****************************************************************************************************
PRO CatTool_Template::GetProperty, _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATTOOL::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       CATTOOL_TEMPLATE::REDO
;
; PURPOSE:
;
;       This method simply re-applies the APPLY method to the target object.
;;
; SYNTAX:
;
;       thisObject -> REDO
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       DRAW:     Set this keyword if you wish to call the DRAW method on the target object
;                 after the REDO operation.
;
;-
;*****************************************************************************************************
PRO CatTool_Template::Redo, Draw=draw

   @cat_pro_error_handler

   self -> Apply, Draw=draw

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATTOOL_TEMPLATE::SETPROPERTY
;
; PURPOSE:
;
;       This method enables the setting of the CATTOOL_TEMPLATE object class
;       properties.
;
; SYNTAX:
;
;       aGraphicsCmd -> SetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;      TARGETOBJECT:  The object that is the target of this command. Note that a target object
;                     is REQUIRED for the tool to work correctly, although you do not have to
;                     set the target in the this method. You can set it via the INIT method, too.
;                     There must be a valid target at the time the APPLY method is called,
;                     or an error is generated.
;
;     _EXTRA:         Any keywords appropriate for the superclass SetProperty methods.
;-
;*****************************************************************************************************
PRO CatTool_Template::SetProperty, $
   TARGETOBJECT=targetObject, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   IF N_Elements(targetObject) NE 0 THEN $
   BEGIN
     IF Obj_IsA_Valid(targetObject, 'CATIMAGE2D') THEN self._theTarget = targetObject ELSE $
        Message, 'The target object is invalid or not a sub-class of CATIMAGE2D.'
   ENDIF
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATTOOL::SetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATTOOL_TEMPLATE::UNDO
;
; PURPOSE:
;
;       This method attempts to undo the previous APPLY operation. Typically, this is
;       as simple as restoring the cached data to the target object, as shown here. Or,
;       it can be as complicated as you need it to be.
;;
; SYNTAX:
;
;       thisObject -> UNDO
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       DRAW:     Set this keyword if you wish to call the DRAW method on the target object
;                 after the UNDO operation.
;-
;*****************************************************************************************************
PRO CatTool_Template::Undo, DRAW=draw

   @cat_pro_error_handler

   ; Replace the cached data in the target object.

   IF Ptr_Valid(self._theCache) EQ 0 THEN RETURN
   self._theTarget -> SetData, *self._theCache, Original = self._original

   ; Need a draw?

   IF Keyword_Set(draw) THEN self._theTarget -> Draw

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       CATTOOL_TEMPLATE::CLEANUP
;
; PURPOSE:
;
;       This is the CATTOOL_TEMPLATE object class destructor method.
;
; SYNTAX:
;
;       Called automatically when the object is destroyed. Use it to clean up
;       and objects or pointers you have used in your tool.
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       _EXTRA:  Any keyword appropriate for the "TOOLATOM::Cleanup" method.
;-
;*****************************************************************************************************
PRO CatTool_Template::CLEANUP

   @cat_pro_error_handler

   self -> CATTOOL::CLEANUP

   self -> Report, /Completed
END


;*****************************************************************************************************
;+
; NAME:
;       CATTOOL_TEMPLATE::INIT
;
; PURPOSE:
;
;       This is the CATTOOL_TEMPLATE object class initialization method.
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;
;       TARGETOBJECT: The image object that is the target of this command. Note that a target object
;                     is REQUIRED for the command to work correctly, although you do not have to
;                     set the target in the INIT method. You can set it via the LOADCOMMAND and SETPROPERTY
;                     methods, too. There must be a valid target at the time the APPLY method is called,
;                     or an error is generated. The targetObject must be a valid CATIMAGE2D object.
;
;       _EXTRA:       Any keywords appropriate for superclass object's INIT methods.
;-
;*****************************************************************************************************
FUNCTION CatTool_Template::INIT, $
   TargetObject=targetObject, $
   _Extra=extraKeywords


   @cat_func_error_handler

   ; Is there a target object here?
   IF N_Elements(targetObject) NE 0 THEN self -> SetProperty, TARGETOBJECT=targetObject


   ; Call superclass INIT method
   ok = self -> CATTOOL::Init ( _Extra=extraKeywords)

   IF (ok) THEN self -> Report, /Completed $
   ELSE self -> Report, /Failed

   RETURN, ok

END


;*****************************************************************************************************
;+
; NAME:
;       CATTOOL_TEMPLATE OBJECT DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the CATTOOL_TEMPLATE object.
;-
;*****************************************************************************************************
PRO CatTool_Template__DEFINE, class

   class = { CATTOOL_TEMPLATE, $   ; The CATTOOL_TEMPLATE object class.
             INHERITS CATTOOL $    ; Inherits the CATTOOL object class.
           }

END
