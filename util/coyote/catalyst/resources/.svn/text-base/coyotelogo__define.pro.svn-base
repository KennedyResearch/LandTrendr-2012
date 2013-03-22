;*****************************************************************************************************
;+
; NAME:
;       COYOTELOGO__DEFINE
;
; PURPOSE:
;
;       The purpose of this routine is to create a Coyote logo image.
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
;       theLogoImage = Obj_New("COYOTELOGO")
;
; SUPERCLASSES:
;
;       CATTRUECOLORIMAGE
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { COYOTELOGO, $
;             INHERITS CATIMAGE $
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David Fanning, 27 Jan 2004.
;-
;*****************************************************************************************************
;+
; NAME:
;       COYOTELOGO::INIT
;
; PURPOSE:
;
;       This is the COYOTELOGO object class initialization method
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
;     _EXTRA:     Any keywords appropriate for the superclass INIT method.
;-
;*****************************************************************************************************
FUNCTION CoyoteLogo::INIT, _EXTRA=extraKeywords

   ; Set up error handler and call superclass init method
   @cat_func_error_handler

   filename = Filepath(Root_Dir=ProgramRootDir(), 'coyote.jpg')
   Read_JPEG, filename, image

   ok = self -> CATIMAGE::INIT (image, _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       COYOTELOGO CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the COYOTELOGO object.
;
;*****************************************************************************************************
PRO CoyoteLogo__DEFINE, class

   class = { COYOTELOGO, $
             INHERITS CATIMAGE $
           }

END
