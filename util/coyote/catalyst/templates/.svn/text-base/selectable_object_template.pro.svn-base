;*****************************************************************************************************
;+
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE
;
; PURPOSE:
;
;       The purpose of this routine is to provide a template for creating
;       a selectable object.
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
;       newObject = Obj_New("SELECTABLE_OBJECT_TEMPLATE", ...)
;       drawObject -> Add, newObject
;
; SUPERCLASSES:
;
;       SELECTABLEOBJECT
;       CATDATAATOM
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { SELECTABLE_OBJECT_TEMPLATE, $
;             insertedObject: Obj_New(), $; A reference to the new object created in the CreateNewObject method. (Ignored in CLEANUP.)
;             layerObject: Obj_New(), $   ; An optional CATLAYER object for holding the inserted selectable object.
;             linestyle: 0L, $            ; The line style of the selectable object.
;             thickness: 0.0, $           ; The thickness of the selectable object.
;             INHERITS SELECTABLEOBJECT $
;           }
;
; MESSAGES:
;
;   SELECTABLE_OBJECT_TEMPLATE_CHANGED:   This message is sent whenever SetProperty
;                                         method is called and the NOMESSAGE keyword
;                                         is NOT set.
;
; MODIFICATION_HISTORY:
;
;       Written by: Donatella Nobody, April 17, 2005.
;-
;*****************************************************************************************************
;+
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE::ADDTOEVENTSTRUCTURE
;
; PURPOSE:
;
;       This method receives an event structure, which it can add information to before being sent
;       to some other event handler. Normally, this method is called by an INTERACTION object of
;       some kind.
;
; SYNTAX:
;
;       theObject -> AddToEventStructure, event
;
; ARGUMENTS:
;
;       event:      The event structure that will be added to.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
FUNCTION Selectable_Object_Template::AddToEventStructure, event

   @cat_func_error_handler

   ; Add appropriate fields to the event structure.
   ;event = Create_Struct(event, ...)

   RETURN, event

END




;*****************************************************************************************************
;+
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE::CALCULATEBOUNDARYBOX
;
; PURPOSE:
;
;       This method calculates the extent of a boundary box about the selectable object
;       itself. The boundary box (self.box) is always stored in normalized coordinates.
;
; SYNTAX:
;
;       theObject -> CalculateBoundaryBox
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO Selectable_Object_Template::CalculateBoundaryBox

   @cat_pro_error_handler


   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE::CONTROLPANEL
;
; PURPOSE:
;
;       This method creates a control panel for the selectable object.
;
; SYNTAX:
;
;       boxObject -> ControlPanel, baseObject
;
; ARGUMENTS:
;
;       baseObject:    The object reference of a base widget for this control to
;                      be added to. If not supplied, the control panel will be in a
;                      self contained window.
;
; KEYWORDS:
;
;       _EXTRA:       Any keywords appropriate for the CATCONTROLPANEL::INIT method.
;-
;*****************************************************************************************************
PRO Selectable_Object_Template::ControlPanel, baseObject, _EXTRA=extraKeywords

   @cat_pro_error_handler

   ; Create a new control panel
   cp = OBJ_NEW ('CatControlPanel', self, PARENT=baseObject, COLUMN=1, $
      TITLE='Selectable_Object_Template Control Panel', _EXTRA=extraKeywords, /No_Cancel, /No_Apply, /No_OK)

   IF (NOT OBJ_VALID (cp)) THEN RETURN

   ; The YSIZE will change depending upon the number of items you include in the Control Panel.
   ; You include items in the Control Panel by regestering them at the botton of the INIT method.
   aproperties = Obj_New('PROPERTYSHEETWIDGET', cp, Value=self, Name='SELECTABLE_OBJECT_TEMPLATE PROPERTYSHEET', YSize=5)
   aproperties -> SetProperty, Event_Object=self

   ; Display the control panel if it created its own TLB.
   IF cp -> Created_Own_TLB(tlb) THEN tlb -> Draw, /Center

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE::COPYPARAMETERS
;
; PURPOSE:
;
;       This method returns the lower-left corner of a rectangle large enough to
;       erase the object in the DESTINATION keyword, and the number of columns and rows
;       of the rectangle in the EXTENT keyword. The values are in device coordinates. The
;       rectangle is often the same as the boundary box of the object. The purpose of
;       the rectangle is to allow an object to erase itself by copying this rectangle
;       from a pixmap window. This will be faster, in most cases, than making the object
;       invisible and redrawing the graphics window.
;
; SYNTAX:
;
;       theObject -> CopyParameters, drawid, DESTINATION=destination, EXTENT=extent
;
; ARGUMENTS:
;
;       drawID:         The identifier of a draw widget object whose extent will
;                       provide the size of the window for calculating device coordinates.
;                       This parameter is required. Typically, the draw widget object that
;                       contains the selectable object.
;
; KEYWORDS:
;
;       DESTINATION:    A two-element array containing the lower-left corner
;                       of the boundary box in device coordinates. An output keyword.
;
;       EXTENT:         A two-element array containing the number of columns and
;                       rows in the boundary box in device coordinates. An output keyword.
;
;-
;*****************************************************************************************************
PRO Selectable_Object_Template::CopyParameters, drawid, DESTINATION=destination, EXTENT=extent

   @cat_pro_error_handler

   IF N_Elements(drawid) EQ 0 THEN Message, 'The DRAWID argument is required.'

   ; Make sure the draw widget is the current graphics window.
   drawID -> SetWindow

   ; The following is typical code, given that the rectangle is the same as the boundary
   ; box of the object. Note that there is "padding" of 8 pixels about the actual boundary
   ; box for safety.

;   minx = Min(self.box[0,*], Max=maxx)
;   miny = Min(self.box[1,*], Max=maxy)
;   c = Convert_Coord([minx, maxx-minx], [miny, maxy-miny], /Normal, /To_Device)
;
;   destination = [c[0,0]-8,c[1,0]-8]
;   extent = [c[0,1]+16, c[1,1]+16]
;
;   ; Make sure you are not out of the window. (Primarily for X Windows devices.)
;   IF destination[0] LT 0 THEN destination[0] = 0.0
;   IF destination[0] GT ((!D.X_Size-1) - extent[0]) THEN destination[0] = (!D.X_Size) - extent[0] + 1
;   IF destination[1] LT 0 THEN destination[1] = 0.0
;   IF destination[1] GT ((!D.Y_Size-1) - extent[1]) THEN destination[1] = (!D.Y_Size) - extent[1] + 1

   self -> Report, /Completed


END



;*****************************************************************************************************
;+
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE::CREATENEWOBJECT
;
; PURPOSE:
;
;       This method creates a new object and adds it to both a draw widget and pixmap container.
;       If the object contains a layer object, the new object is instead added to the layer,
;       and the layer is added to the draw widget and pixmap containers. Creating a new object is
;       usually done at the end of an INSERT mode event, but this method can also be used as an
;       object COPY method.
;
; SYNTAX:
;
;       theObject -> CreateNewObject, drawID
;
; ARGUMENTS:
;
;       drawID:    The draw widget which will contain the newly created object. Required unless
;                  you are calling this method simply to make a copy of the current object.
;
;       pixmapID:  The pixmap which will contain the newly created object. Optional.
;
;
; KEYWORDS:
;
;       NEWOBJECT: An output keyword containing the new box object that gets created. If the
;                  method is called with just this keyword and no other arguments, the method
;                  functions as a object COPY method.
;
;-
;*****************************************************************************************************
PRO Selectable_Object_Template::CreateNewObject, drawID, pixmapID, NEWOBJECT=newObject

   @cat_pro_error_handler

;   ; Get all the properties you need from the current object.
;   self -> GetProperty, $
;      COLOR=color, $
;      LINESTYLE=linestyle, $
;      _EXTRA=extra
;
;   ; Transfer those properties to the new object.
;   newObject = Obj_New('SELECTABLE_OBJECT_TEMPLATE',  $
;      COLOR=color, $
;      LINESTYLE=linestyle, $
;      _EXTRA=extra
;   )

   ; Make sure you have at least one positional parameter to proceed.
   IF N_Params() LT 1 THEN RETURN

   ; If you have a layer object, add the newObject to it instead of directly
   ; to the draw widget and pixmap.
   IF Obj_Valid(self.layerObject) THEN BEGIN

      ; Add new object to the layer object.
      self.layerObject -> Add, newObject
      self.insertedObject = newObject

      ; If the annotation object is visible, draw the new object.
      self.layerObject -> GetProperty, Visible=visible
      IF visible THEN BEGIN

         ; Draw the new object.
         newObject -> Draw

         ; Refresh the pixmap, or it will think it is ready to draw.
         IF Obj_Valid(pixmapID) THEN pixmapID -> Refresh

      ENDIF

   ENDIF ELSE BEGIN

      ; Add the object to the draw widget and pixmap.
      IF Obj_Valid(drawID) THEN drawID -> Add, newObject
      IF Obj_Valid(pixmapID) THEN pixmapID -> Add, newObject
      self.insertedObject = newObject

      ; Draw the new object.
      newObject -> Draw

   ENDELSE

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE::DRAW
;
; PURPOSE:
;
;       This method draws the selectable object in the current graphics display window.
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
;       _Extra:    Any keywords appropriate for the superclass DRAW method.
;
;-
;*****************************************************************************************************
PRO Selectable_Object_Template::Draw, _Extra=extrakeywords

   @cat_pro_error_handler

   ; If the selectable object is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

  ; Apply the coordinate system.
   self -> ApplyCoords

   ; Calculate the boundary box.
   self -> CalculateBoundaryBox

   ; Draw the object here.
   ;Plot, ...

   self -> Report, /Completed


END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE::DRAWSELECTIONBOX
;
; PURPOSE:
;
;       This method draws a selection box around the selectable object.
;
; SYNTAX:
;
;       theObject -> DrawSelectionBox
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;       COLOR:    The name of a color to draw the selectable object in. By default, the color of
;                 the selectable object (self.color).
;
;-
;*****************************************************************************************************
PRO Selectable_Object_Template::DrawSelectionBox, Color=color

   @cat_pro_error_handler

   ; If the selectable object is invisible, then return immediately.
   IF self.visible EQ 0 THEN RETURN

   IF N_Elements(color) EQ 0 THEN color = self.color

   ; Draw the selection box.
;    x1 = self.box[0,0] & x2 = self.box[0,2]
;    y1 = self.box[1,0] & y2 = self.box[1,1]
;    PLOTS, [x1, x1, x2, x2, x1], [y1, y2, y2, y1, y1], /Normal, Color=FSC_Color(color)

   ; Draw the handles on the box.
;   PLOTS, x1, y1,   PSYM=6, Color=FSC_Color(color), Symsize=1.25
;   PLOTS, x1, y2,   PSYM=6, Color=FSC_Color(color), Symsize=1.25
;   PLOTS, x2, y1,   PSYM=6, Color=FSC_Color(color), Symsize=1.25
;   PLOTS, x2, y2,   PSYM=6, Color=FSC_Color(color), Symsize=1.25


   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE::EVENTHANDLER
;
; PURPOSE:
;
;       This method is an event handler for the Control Panel. If you have
;       user-definable properties, they will have to be accounted for here.
;       Otherwise, this event handler will correctly handle most GET and SET
;       PROPERTY keywords.
;
; SYNTAX:
;
;       Called automatically by the event handling system
;
; ARGUMENTS:
;
;       event:  The event structure.
;
; KEYWORDS:
;
;       None.
;-
;*****************************************************************************************************
PRO Selectable_Object_Template::EventHandler, event

   @cat_pro_error_handler

   ; Get the name of the widget generating the event. Branch on this.
   event.ID -> GetProperty, Name=eventName
   CASE eventName OF


      'SELECTABLE_OBJECT_TEMPLATE PROPERTYSHEET': BEGIN

         IF event.type EQ 0 THEN BEGIN
            CASE StrUpCase(event.identifier) OF

               'COLOR': BEGIN

                  event.component -> GetProperty, Color=color
                  event.id -> GetProperty, ID=group_leader
                  color = PickColorName(color, Group_Leader=group_leader)
                  event.component -> SetProperty, Color=color

                  ; Refresh the graphics hierarchy.
                  CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase

               ENDCASE

               ELSE: BEGIN

                  component = event.component
                  identifier = event.identifier
                  event.id -> GetProperty, Value=value, Component=component, Property_Value=identifier
                  event.component -> SetPropertyByIdentifier, identifier, value

                  ; Refresh the graphics hierarchy.
                  IF Obj_Valid(self) THEN CatRefreshDraw, self, Stop_At='DrawWidget', /NoErase ELSE RETURN


               ENDCASE

            ENDCASE

         ENDIF ; of BUTTON DOWN EVENT

         ENDCASE ; of SELECTABLE_OBJECT_TEMPLATE PROPERYSHEET events

        ; If you can't handle the event here. Pass it along to superclass EventHandler
        ELSE: self -> SelectableObject::EventHandler, event

   ENDCASE

   IF Obj_Valid(self) THEN self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain SELECTABLE_OBJECT_TEMPLATE properties. Be sure
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
PRO Selectable_Object_Template::GetProperty, $
   _REF_EXTRA=extraKeywords

   @cat_pro_error_handler

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SELECTABLEOBJECT::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE::INTERACTIONEVENTS
;
; PURPOSE:
;
;       This method accepts events from an interaction object of some type. The interaction
;       may pre-process events, or send them directly here. You are required to have the
;       following modes: SELECT, INSERT, and DRAW. Other modes are optional and are left
;       to the programmer to interpret. Finishing an INSERT or DRAW mode event requires that
;       the interaction mode by set to 'FINISHED_INSERT' or 'FINISHED_DRAW', respectively. This
;       is to alert the interaction that a mode has been completed.
;
;       SELECT:   All SELECT mode events are passed to the selectable object for initialization
;                 of the object, as needed. If no initialization is necessary, return immediately.
;
;       DRAW:     DRAW mode events are transitory. The object disappears as soon as the drawing of
;                 the object is complete and an ACCEPT event is sent to the responsible event handler.
;
;       INSERT:   INSERT mode events result in a new selectable object being created and inserted into
;                 the object hierarchy. These objects can be selected and manipulated by other "modes"
;                 of your choosing. Include the appropriate modes here.
;
; SYNTAX:
;
;       theObject -> INTERACTIONEVENTS
;
; ARGUMENTS:
;
;     event:          The widget event that is generated by the draw widget and handled by the Interaction
;                     object.
;
; KEYWORDS:
;
;     INTERACTION:    The object reference to a Interaction object that is receiving events.
;                     This is a *required* parameter, but is written as a keyword for programming clarity.
;
;-
;*****************************************************************************************************
PRO Selectable_Object_Template::InteractionEvents, event, Interaction=interaction

   @cat_pro_error_handler

   ; Get information from the INTERACTION object. In particular, you want to know the MODE, and
   ; the object references of the draw widget and pixmap.
   IF Obj_Valid(interaction) EQ 0 THEN Message, 'An "interaction" object is a required keyword parameter.'
   interaction -> GetProperty, MODE=mode, DRAWWIDGET=drawID, PIXMAP=pixmap

   ; What kind of event is this?
   eventTypes = ['DOWN', 'UP', 'MOTION', 'VIEWPORT', 'EXPOSED', 'CHARACTER', 'KEYPRESS']
   thisEvent = eventTypes[event.type]

   ; Action is based on the current mode of the interaction.
   CASE mode OF

      'SELECT': BEGIN

                       ; Only interested in LEFT mouse events PRESS events, here.
;                       IF event.press EQ 1 THEN BEGIN
;                       END ; of LEFT PRESS event

         END ; of SELECT mode

      'INSERT': BEGIN

         CASE thisEvent OF

           'DOWN': BEGIN
              END

           'UP': BEGIN

              ; Indicate you have finished the INSERT mode.
              interaction -> SetProperty, Mode='FINISHED_INSERT'

              END

           'MOTION': BEGIN
              END

           ELSE:

         ENDCASE ; of thisEvent in INSERT

         END ; of INSERT mode


      'DRAW': BEGIN

         CASE thisEvent OF

           'DOWN': BEGIN
              END

           'UP': BEGIN

              ; Indicate you have finished the mode.
              interaction -> SetProperty, Mode='FINISHED_DRAW'

              END

           'MOTION': BEGIN
              END

           ELSE:

         ENDCASE ; of thisEvent in DRAW

         END ; of DRAW mode

      ELSE: BEGIN
         ok = Dialog_Message('Unknown MODE [ ' + mode + ' ] in SELECTABLE_OBJECT_TEMPLATE::InteractionEvents method.')
         END

  ENDCASE

   self -> Report, /Completed


END



;*****************************************************************************************************
;+
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE::MOVE
;
; PURPOSE:
;
;       This method moves the selectable object in a graphics window.
;
; SYNTAX:
;
;       theObject -> Move, x, y
;
; ARGUMENTS:
;
;       X:          The number of pixels to move in the X direction.
;
;       Y:          The number of pixels to move in the Y direction.
;
; KEYWORDS:
;
;       NODRAW:     If this keyword is set, only the coordinates are updated. No drawing occurs.
;
;       PIXMAP:     Set this keyword to a pixmap that can be used to erase the previous
;                   contents of the window.
;-
;*****************************************************************************************************
PRO Selectable_Object_Template::Move, x, y, PIXMAP=pixmap, NODRAW=noDraw
   @cat_pro_error_handler

   ; Move code inserted here. Here is an example from the BOX selectable object.

;   ; Convert the device pixels into data coordinates.
;   self.internalCoords -> Draw
;   c = Convert_Coord(x, y, /Device, /To_Normal)
;   self.x1 = self.x1 + c[0,0]
;   self.y1 = self.y1 + c[1,0]
;   self.x2 = self.x2 + c[0,0]
;   self.y2 = self.y2 + c[1,0]
;
;   IF Obj_Valid(self._controlpanel) THEN $
;       self._controlpanel -> Refresh_Properties, Properties=['X1','Y1','X2','Y2']


   ; Do you need to draw?
   IF ~Keyword_Set(nodraw) THEN BEGIN
      IF Obj_Isa_Valid(pixmap, 'PIXMAPWIDGET') THEN BEGIN
         self -> CopyParameters, pixmap, Destination=d, Extent=e
         pixmap -> Copy, Destination=d, Extent=e, Origin=d
         self -> Draw
      ENDIF ELSE BEGIN
         CatRefreshDraw, self, Stop_At='DRAWWIDGET', /NoErase
      ENDELSE
   ENDIF

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE::SELECT
;
; PURPOSE:
;
;       This method returns the object reference if the requested point is inside
;       the bounding box of the object.
;
; SYNTAX:
;
;       selectedObject = theObject -> Select, x, y
;
; ARGUMENTS:
;
;       X:   The X location of a point in device or window coordinates.
;
;       Y:   The Y location of a point in device or window coordinates.
;
; KEYWORDS:
;
;       SUCCESS:   Set to 1 if a selection is made. To 0 otherwise.
;-
;*****************************************************************************************************
FUNCTION Selectable_Object_Template::Select, x, y, SUCCESS=success

   @cat_func_error_handler

   ; Set up return values.
   retval = Obj_New()
   success = 0

   ; No selection is possible if the object is invisible.
   IF self.visible EQ 0 THEN RETURN, retval

   ; If you belong to a group, you cannot be selected individually.
   IF Obj_Valid(self.mygroup) THEN RETURN, retval

   ; No selection is possible, if the object is currently unselectable.
   IF ~self.selectable THEN RETURN, retval

;   ; Convert the point from device to normalized coordinates.
;   c = Convert_Coord(x, y, /Device, /To_Normal)
;   xx = c[0,0]
;   yy = c[1,0]
;
;   ; Update the boundary box coordinates.
;   self -> CalculateBoundaryBox
;
;   ; Are you inside?
;   isInside = Inside(xx, yy, Reform(self.box[0,0:3]), Reform(self.box[1,0:3]))
;   IF isInside THEN BEGIN
;
;      retVal = self
;      success = 1
;
;   ENDIF

   self -> Report, /Completed
   RETURN, retval

END



;*****************************************************************************************************
;+
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE::SELECTPANEL
;
; PURPOSE:
;
;       Similar to a Control Panel, it gives context menu access to properties
;       of selectable objects. Objects will have a ContextMenu *only* if they
;       have subclassed the SELECTABLEOBJECT object. If you *have* subclassed
;       the SELECTABLEOBJECT object, then remove this method from your code.
;
; SYNTAX:
;
;       selectedObject = theObject -> SelectPanel, x, y, drawID
;
; ARGUMENTS:
;
;       X:       The X location of a point in device or window coordinates.
;
;       Y:       The Y location of a point in device or window coordinates.
;
;       DRAWID:  The identifer of the draw widget object in which the selection is taking place.
;
; KEYWORDS:
;
;       None:
;-
;*****************************************************************************************************
PRO Selectable_Object_Template::SelectPanel, x, y, drawID

   @cat_pro_error_handler

;   IF N_Params() NE 3 THEN Message, 'Incorrect number of positonal parameters.'
;
;   IF Obj_Valid(self.contextmenu) THEN Obj_Destroy, self.contextMenu
;   self.contextMenu = Obj_New('ContextMenuBase', drawID, Column=1, Event_Object=self)
;   button = Obj_New('ButtonWidget', self.contextMenu, Value='Bring to Front', Name='SEND_FRONT')
;   button = Obj_New('ButtonWidget', self.contextMenu, Value='Send to Back', Name='SEND_BACK')
;   button = Obj_New('ButtonWidget', self.contextMenu, Value='Move Forward', Name='MOVE_FORWARD')
;   button = Obj_New('ButtonWidget', self.contextMenu, Value='Move Backward', Name='MOVE_BACKWARD')
;   self -> GetProperty, Background=background
;
;   button = Obj_New('ButtonWidget', self.contextMenu, Value='Foreground Color...', Name='FOREGROUND_COLOR', /Separator)
;   IF background THEN $
;      button = Obj_New('ButtonWidget', self.contextMenu, Value='Background Off.', Name='BACKGROUND_INVISIBLE') ELSE $
;      button = Obj_New('ButtonWidget', self.contextMenu, Value='Background On', Name='BACKGROUND_VISIBLE')
;   backgroundID = Obj_New('ButtonWidget', self.contextMenu, Value='Background Color...', Name='BACKGROUND_COLOR')
;   IF ~background THEN backgroundID -> SetProperty, Sensitive=0
;   button = Obj_New('ButtonWidget', self.contextMenu, Value='Other Properties...', Name='OTHER_PROPERTIES', UValue=drawID)
;   button = Obj_New('ButtonWidget', self.contextMenu, Value='Delete', Name='DELETE_OBJECT', /Separator)
;
;   Widget_DisplayContextMenu, drawID -> GetID(), x+10, y-5, self.contextMenu->GetID()

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the SELECTABLE_OBJECT_TEMPLATE object's properties. Be sure
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
;     Usually the same keywords that can be set in the INIT method.
;
;     DRAW:       Set this keyword if you wish to call the DRAW method on SetProperty completion.
;
;     NOMESSAGE:  Set this keyword to suppress normal message sending.
;-
;*****************************************************************************************************
PRO Selectable_Object_Template::SetProperty, $
   DRAW=draw, $
   NOMESSAGE=nomessage, $
   _EXTRA=extraKeywords

   @cat_pro_error_handler

   sendMessage = 0

   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> SELECTABLEOBJECT::SetProperty, NOREFRESH=norefresh, _EXTRA=extraKeywords

   ; The object could have been deleted. If so, RETURN.
   IF ~Obj_Valid(self) THEN RETURN

   ; Send message?
   IF sendMessage AND ~Keyword_Set(nomessage) THEN self -> SendMessage, 'SELECTABLE_OBJECT_TEMPLATE_CHANGED'

   IF Keyword_Set(draw) THEN self -> Draw

   self -> Report, /Completed

END





;*****************************************************************************************************
;+
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE::CLEANUP
;
; PURPOSE:
;
;       This is the SELECTABLE_OBJECT_TEMPLATE object class destructor method.
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
PRO Selectable_Object_Template::CLEANUP

   @cat_pro_error_handler

   self -> SELECTABLEOBJECT::CLEANUP

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE::INIT
;
; PURPOSE:
;
;       This is the SELECTABLE_OBJECT_TEMPLATE object class initialization method
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
;     LAYER:        A CATLAYER object for holding other objects. Used here only when there is an UP
;                   event in INSERT mode. At that time a copy of this object is made and inserted
;                   the layer object and this is then inserted into the DrawWidget and/or Pixmap object.
;
;     THICKNESS:    Set this to the thickness of the selectable object. By default, 1.0.
;
;     _EXTRA:       Any keywords appropriate for the SELECTABLEOBJECT INIT method.
;-
;*****************************************************************************************************
FUNCTION Selectable_Object_Template::INIT, $
   LAYER=layer, $
   THICKNESS=thickness, $
   _EXTRA=extraKeywords

   ; Set up error handler and call superclass INIT method
   @cat_func_error_handler

   ; Create normalized internal coordinate system for this object.
   internalCoords = Obj_New('CatCoord', XRange=[0,1], YRange=[0,1], Position=[0,0,1,1])

   ok = self -> SELECTABLEOBJECT::INIT (INTERNALCOORDS=internalCoords, $
      DESCRIPTION='Selectable_Object_Template Properties', _EXTRA=extraKeywords)
   IF ~ok THEN RETURN, 0

   ; Check keywords.
   IF Obj_Isa_Valid(layer, "CATLAYER") NE 0 THEN BEGIN
      self.layerObject = layer
      self.layerObject -> AddParent, self
   ENDIF
   IF N_Elements(thickness) EQ 0 THEN thickness = 1.0

   ; Load object.
   self.thickness = thickness

   ; Register properties for the property sheet.
   self -> RegisterProperty, 'THICKNESS', 2, NAME="Thickness", VALID_RANGE=[1, 10]

   self -> Report, /Completed

   RETURN, 1

END

;*****************************************************************************************************
;
; NAME:
;       SELECTABLE_OBJECT_TEMPLATE CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the SELECTABLE_OBJECT_TEMPLATE object.
;
;*****************************************************************************************************
PRO Selectable_Object_Template__Define, class

   class = { SELECTABLE_OBJECT_TEMPLATE, $
             insertedObject: Obj_New(), $; A reference to the new object created in the CreateNewObject method. (Ignored in CLEANUP.)
             layerObject: Obj_New(), $   ; An optional CATLAYER object for holding the inserted selectable object.
             thickness: 0.0, $           ; The thickness of the line drawing the selectable object.
             INHERITS SELECTABLEOBJECT $
           }

END

