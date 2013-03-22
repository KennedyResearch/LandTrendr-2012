; Copyright (c) 1997-2003, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.
;+
; NAME:
;   CAT_TRACKBALL
;
; PURPOSE:
;   This object translates widget events for draw widgets into
;       transformations that emulate a virtual trackball (for transforming
;       object graphics in three dimensions).
;
; CATEGORY:
;   Object Graphics.
;
; CALLING SEQUENCE:
;   To initially create:
;       oTrackball = OBJ_NEW('Cat_Trackball', Center, Radius)
;
;   To update the trackball state based on a widget event:
;       oCat_Trackball->Update, sEvent
;
;   To re-initialize the trackball state:
;       oCat_Trackball->Reset, Center, Radius
;
;   To destroy:
;       OBJ_DESTROY, oCat_Trackball
;
; INPUTS:
;   CAT_TRACKBALL::INIT:
;   Center: A two-dimensional vector, [x,y], representing the requested
;       center (measured in device units) of the trackball.
;       Radius: The requested radius (measured in device units) of the
;       trackball.
;
;   CAT_TRACKBALL::UPDATE:
;        sEvent: The widget event structure.  The event type indicates
;       how the trackball state should be updated.
;
;   CAT_TRACKBALL::RESET:
;   Center: A two-dimensional vector, [x,y], representing the requested
;       center (measured in device units) of the trackball.
;       Radius: The requested radius (measured in device units) of the
;       trackball.
;
; KEYWORD PARAMETERS:
;   CAT_TRACKBALL::INIT:
;   AXIS:       Set this keyword to indicate the axis about which
;           rotations are to be constrained if the CONSTRAIN
;           keyword is set to a nonzer value.  Valid values
;           include:
;               0 = X-Axis
;               1 = Y-Axis
;               2 = Z-Axis (default)
;   CONSTRAIN:  Set this keyword to a nonzero value to indicate that
;           the trackball transformations are to be constrained
;           about a given axis (as specified by the AXIS
;           keyword).  The default is zero (no constraints).
;   MOUSE:      Set this keyword to a bitmask to indicate which
;           mouse button to honor for trackball events.  The
;           least significant bit represents the leftmost
;           button, the next highest bit represents the middle
;           button, and the next highest bit represents the
;           right button.  The default is 1b, for the left
;           mouse button.
;
;   CAT_TRACKBALL::UPDATE:
;   MOUSE:      Set this keyword to a bitmask to indicate which
;           mouse button to honor for trackball events.  The
;           least significant bit represents the leftmost
;           button, the next highest bit represents the middle
;           button, and the next highest bit represents the
;           right button.  The default is 1b, for the left
;           mouse button.
;   TRANSFORM:  Set this keyword to a named variable that upon
;           return will contain a floating point 4x4 array
;           if a transformations matrix is calculated as
;           a result of the widget event.
;   TRANSLATE:  Set this keyword to indicate that the trackball
;           movement should be constrained to x and y translation
;           rather than rotation about an axis.
;
;   CAT_TRACKBALL::RESET:
;   AXIS:       Set this keyword to indicate the axis about which
;           rotations are to be constrained if the CONSTRAIN
;           keyword is set to a nonzer value.  Valid values
;           include:
;               0 = X-Axis
;               1 = Y-Axis
;               2 = Z-Axis (default)
;   CONSTRAIN:  Set this keyword to a nonzero value to indicate that
;           the trackball transformations are to be constrained
;           about a given axis (as specified by the AXIS
;           keyword).  The default is zero (no constraints).
;   MOUSE:      Set this keyword to a bitmask to indicate which
;           mouse button to honor for trackball events.  The
;           least significant bit represents the leftmost
;           button, the next highest bit represents the middle
;           button, and the next highest bit represents the
;           right button.  The default is 1b, for the left
;           mouse button.
;
; OUTPUTS:
;   CAT_TRACKBALL::UPDATE:
;   This function returns a 1 if a transformation matrix is calculated
;   as a result of the widget event, or 0 otherwise.
;
; EXAMPLE:
;   Create a trackball centered on a 512x512 pixel drawable area, and
;   a view containing the model to be manipulated:
;       xdim = 512
;       ydim = 512
;       wBase = WIDGET_BASE()
;       wDraw = WIDGET_DRAW(wBase, XSIZE=xdim, YSIZE=ydim, $
;                           GRAPHICS_LEVEL=2, /BUTTON_EVENTS, $
;                           /MOTION_EVENTS, /EXPOSE_EVENTS, RETAIN=0 )
;       WIDGET_CONTROL, wBase, /REALIZE
;       WIDGET_CONTROL, wDraw, GET_VALUE=oWindow
;
;       oCat_Trackball = OBJ_NEW('Cat_Trackball', [xdim/2.,ydim/2.], xdim/2.)
;       oView = OBJ_NEW('IDLgrView')
;       oModel = OBJ_NEW('IDLgrModel')
;       oView->Add, oModel
;
;       XMANAGER, 'TrackEx', wBase
;
;   In the widget event handler, handle trackball updates.
;   As the trackball transformation changes, update the transformation
;   for a model object (instance of IDLgrModel), and redraw the view:
;
;   PRO TrackEx_Event, sEvent
;       ...
;       bHaveXform = oCat_Trackball->Update( sEvent, TRANSFORM=TrackXform )
;       IF (bHaveXform) THEN BEGIN
;           oModel->GetProperty, TRANSFORM=ModelXform
;           oModel->SetProperty, TRANSFORM=ModelXform # TrackXform
;           oWindow->Draw, oView
;       ENDIF
;       ...
;   END
;
; MODIFICATION HISTORY:
;   Written by: DD, December 1996
;   Modified by David Fanning, Aug 9, 2003 to work with Catalyst System.
;      Required modification to UPDATE method to allow events from objects.
;
;-

;----------------------------------------------------------------------------
; CAT_TRACKBALL_CONSTRAIN
;
; Purpose:
;  Given a point and a constraint vector, map the point to its constrained
;  equivalent.
;
; Arguments:
;  pt - The unconstrained point.
;  vec - A three-element vector, [x,y,z], representing the unit vector about
;        which rotations are constrained.
;
FUNCTION CAT_TRACKBALL_CONSTRAIN, pt, vec, $
    START_TRANSFORM=startTransform

    COMPILE_OPT hidden

    if ARRAY_EQUAL(SIZE(startTransform, /DIM), [4,4]) then begin
        zeroVec = [0, 0, 0, 1] # startTransform
        vec = [vec, 1] # startTransform
        vec = vec[0:2] - zeroVec[0:2]
        norm = SQRT(TOTAL(vec^2))
        if (norm gt 0) then $
            vec = TEMPORARY(vec)/norm
;        print, ([0,0,0,1] # startTransform)[*]
    endif

    ; Project the point.
    proj = pt - TOTAL(vec * pt) * vec

    ; Normalizing factor.
    norm = SQRT(TOTAL(proj^2))

    IF (norm GT 0.0) THEN BEGIN
        IF (proj[2] LT 0.0) THEN $
            cpt = -1.0 / norm * proj $
        ELSE $
            cpt = 1.0 / norm * proj
    ENDIF ELSE IF vec[2] EQ 1.0 THEN $
        cpt = [1.0, 0.0, 0.0] $
    ELSE $
        cpt = [-vec[1], vec[0], 0.0] / SQRT(TOTAL(vec[0:1]^2))

    RETURN, cpt
END

;----------------------------------------------------------------------------
; CAT_TRACKBALL::UPDATE
;
; Purpose:
;  Given a widget event structure, updates the trackball state.
;
;  The return value is nonzero if a transformation matrix is calculated
;  as a result of the event, or zero otherwise.
;
; Arguments:
;  sEvent - The widget event structure.
;
; Keywords:
;  TRANSFORM - If a transformation matrix is calculated, upon return
;              transform will contain a 4x4 matrix.
;
FUNCTION CAT_TRACKBALL::UPDATE, sEvent, $
    START_TRANSFORM=startTransform, $
    TRANSFORM=transform, $
     MOUSE=mouse, TRANSLATE=translate

  ; Initialize return value.
  bHaveTransform=0

  IF (N_ELEMENTS(mouse) NE 0) THEN BEGIN
      if (mouse ne 1) and (mouse ne 2) and (mouse ne 4) then begin
          PRINT, 'Cat_Trackball: invalid value for MOUSE keyword.'
          RETURN, 0
      ENDIF ELSE $
          self.mouse = mouse
  ENDIF ; Don't set self.mouse if no argument, keep setting from INIT

  ; Ignore non-Draw-Widget events.
;  IF (TAG_NAMES(sEvent, /STRUCTURE_NAME) NE 'WIDGET_DRAW') THEN $
;    RETURN, bHaveTransform
  ; Determine event type.
  CASE sEvent.type OF
    0: BEGIN    ;Button press.
         ; Only handle event if appropriate mouse button.
         IF (sEvent.press EQ self.mouse) THEN BEGIN
           ; Calculate distance of mouse click from center of unit circle.
           xy = ([sEvent.x,sEvent.y] - self.center) / self.radius
           r = TOTAL(xy^2)
           IF (r GT 1.0) THEN $
             self.pt1 = [xy/SQRT(r) ,0.0] $
           ELSE $
             self.pt1 = [xy,SQRT(1.0-r)]

           ; Constrain if necessary.
           IF (self.constrain NE 0) THEN BEGIN
               vec = [0.,0.,0.]
               vec[self.axis] = 1.0
               self.pt1 = CAT_TRACKBALL_CONSTRAIN( self.pt1, vec, $
                START_TRANSFORM=startTransform)
           ENDIF
           self.pt0 = self.pt1
           self.btndown = 1b
         ENDIF
       END

    2: BEGIN    ;Button motion.
         IF (self.btndown EQ 1b) THEN BEGIN
           ; Calculate distance of mouse click from center of unit circle.
           xy = ([sEvent.x,sEvent.y] - self.center) / $
                self.radius
           r = TOTAL(xy^2)
           IF (r GT 1.0) THEN $
             pt1 = [xy/SQRT(r) ,0.0] $
           ELSE $
             pt1 = [xy,SQRT(1.0-r)]

           ; Constrain if necessary.
           IF (self.constrain NE 0) THEN BEGIN
               vec = [0d,0d,0d]
               vec[self.axis] = 1
               pt1 = CAT_TRACKBALL_CONSTRAIN( pt1, vec, $
                START_TRANSFORM=startTransform)
           ENDIF

           ; Update the transform only if the mouse button has actually
           ; moved from its previous location.
           pt0 = self.pt0
           IF ((pt0[0] NE pt1[0]) OR $
               (pt0[1] NE pt1[1]) OR $
               (pt0[2] NE pt1[2])) THEN BEGIN
             ; Compute transformation.
             q = [CROSSP(pt0,pt1), TOTAL(pt0*pt1)]

             x = q[0]
             y = q[1]
             z = q[2]
             w = q[3]
             if (keyword_set(translate)) then begin
               ; translation only
               transform = [[ 1,0,0, pt1[0]-pt0[0] ], $
                            [ 0,1,0, pt1[1]-pt0[1]], $
                            [ 0,0,1, 0], $
                            [ 0          , 0          , 0              , 1]]
             endif else begin
               transform = [[ w^2+x^2-y^2-z^2, 2*(x*y-w*z), 2*(x*z+w*y), 0], $
                            [ 2*(x*y+w*z), w^2-x^2+y^2-z^2, 2*(y*z-w*x), 0], $
                            [ 2*(x*z-w*y), 2*(y*z+w*x), w^2-x^2-y^2+z^2, 0], $
                            [ 0          , 0          , 0              , 1]]
             endelse

             bHaveTransform = 1b
             self.pt0 = pt1
           ENDIF

           self.pt1 = pt1
         ENDIF
       END

    1: BEGIN    ;Button Release.
         IF (self.btndown EQ 1b) THEN $
           self.btndown = 0b
       END

    ELSE: RETURN, bHaveTransform

   ENDCASE

  RETURN, bHaveTransform
END

;----------------------------------------------------------------------------
; CAT_TRACKBALL::INIT
;
; Purpose:
;   Initializes the trackball state.
;
; Arguments:
;  center - A two-dimensional vector, [x,y], representing the requested
;           center (measured in device units) of the trackball.
;  radius - The requested radius (measured in device units) of the trackball.
;
; Keywords:
;  AXIS -  Set this keyword to indicate the axis about which rotations
;          are to be constrained if the CONSTRAIN keyword is set to a
;          nonzero value).  Valid values include:
;               0 = X
;               1 = Y
;               2 = Z (default)
;  CONSTRAIN - Set this keyword to a nonzero value to indicate that the
;          trackball transformations are to be constrained about a given
;          axis (as specified by the AXIS keyword).  The default is zero
;          (no constraints).
;  MOUSE - Set this keyword to a bitmask to indicate which mouse button to
;          honor for trackball events.  The least significant bit represents
;          the leftmost button, the next highest bit represents the middle
;          button, and the next highest bit represents the right button.
;          The default is 1b, for the left moust button.
;
FUNCTION CAT_TRACKBALL::INIT, center, radius, AXIS=axis, CONSTRAIN=constrain, $
                          MOUSE=mouse
    IF (N_ELEMENTS(center) NE 2) THEN BEGIN
        PRINT, 'Cat_Trackball: center must be a two-dimensional array.'
        RETURN, 0
    ENDIF

    IF (N_ELEMENTS(radius) NE 1) THEN BEGIN
        PRINT, 'Cat_Trackball: invalid radius.'
        RETURN, 0
    ENDIF

    IF (N_ELEMENTS(axis) NE 0) THEN BEGIN
        IF (axis lt 0) OR (axis gt 2) THEN BEGIN
            PRINT, 'Cat_Trackball: invalid value for AXIS keyword.'
            RETURN, 0
        ENDIF ELSE $
            self.axis = axis
    ENDIF ELSE $
        self.axis = 2

    IF (N_ELEMENTS(constrain) NE 0) THEN $
        self.constrain = constrain $
    ELSE $
        self.constrain = 0

    IF (N_ELEMENTS(mouse) NE 0) THEN BEGIN
        if (mouse ne 1) and (mouse ne 2) and (mouse ne 4) then begin
            PRINT, 'Cat_Trackball: invalid value for MOUSE keyword.'
            RETURN, 0
        ENDIF ELSE $
            self.mouse = mouse
    ENDIF ELSE $
        self.mouse = 1 ; Default is left mouse button

    self.center = center
    self.radius = radius
    self.btndown = 0b

    RETURN, 1
END

;----------------------------------------------------------------------------
; CAT_TRACKBALL::CLEANUP
;
; Purpose:
;   Cleanup the trackball when it is destroyed.
;
; Arguments:
;  <None>
;
; Keywords:
;  <None>
;
PRO CAT_TRACKBALL::CLEANUP
    ; No work needs to be done.  We provide this method to avoid any
    ; problems resolving the Cleanup method call on Windows 3.11
    ; which has short filename restrictions.
END

;----------------------------------------------------------------------------
; CAT_TRACKBALL::RESET
;
; Purpose:
;   Resets the trackball state.
;
; Arguments:
;  center - A two-dimensional vector, [x,y], representing the requested
;           center (measured in device units) of the trackball.
;  radius - The requested radius (measured in device units) of the trackball.
;
; Keywords:
;  MOUSE - Set this keyword to a bitmask to indicate which mouse button to
;          honor for trackball events.  The least significant bit represents
;          the leftmost button, the next highest bit represents the middle
;          button, and the next highest bit represents the right button.
;          The default is 1b, for the left moust button.
;
PRO CAT_TRACKBALL::RESET, center, radius, AXIS=axis, CONSTRAIN=constrain, $
                      MOUSE=mouse
    IF (N_ELEMENTS(center) NE 2) THEN BEGIN
        PRINT, 'Cat_Trackball: center must be a two-dimensional array.'
        RETURN
    ENDIF

    IF (N_ELEMENTS(radius) NE 1) THEN BEGIN
        PRINT, 'Cat_Trackball: invalid radius.'
        RETURN
    ENDIF

    IF (N_ELEMENTS(axis) NE 0) THEN BEGIN
        IF (axis lt 0) OR (axis gt 2) THEN BEGIN
            PRINT, 'Cat_Trackball: invalid value for AXIS keyword.'
            RETURN
        ENDIF ELSE $
            self.axis = axis
    ENDIF ELSE $
        self.axis = 2

    IF (N_ELEMENTS(constrain) NE 0) THEN $
        self.constrain = constrain $
    ELSE $
        self.constrain = 0

    IF (N_ELEMENTS(mouse) NE 0) THEN BEGIN
        if (mouse ne 1) and (mouse ne 2) and (mouse ne 4) then begin
            PRINT, 'Cat_Trackball: invalid value for MOUSE keyword.'
            RETURN
        ENDIF ELSE $
            self.mouse = mouse
    ENDIF ELSE $
        self.mouse = 1 ; Default is left mouse button

    self.center = center
    self.radius = radius

    self.btndown = 0b

END

;----------------------------------------------------------------------------
; CAT_TRACKBALL__DEFINE
;
; Purpose:
;  Defines the object structure for a trackball object.
;
PRO cat_trackball__define, struct

  COMPILE_OPT hidden

  struct = {cat_trackball, $
            btndown: 0b,    $
            axis: 0, $
            constrain: 0b, $
            mouse: 0b, $
            center: LONARR(2), $
            radius: 0.0, $
            pt0: FLTARR(3), $
            pt1: FLTARR(3) $
           }
END
