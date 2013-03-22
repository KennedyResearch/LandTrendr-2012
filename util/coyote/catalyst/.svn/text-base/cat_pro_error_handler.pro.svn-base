
   COMPILE_OPT idl2, HIDDEN

   ; Get the ErrorLevel
   IF (Obj_IsA_Valid (self, 'CatAtom')) THEN errorLevel = self._errorLevel $
   ELSE errorLevel = CatGetDefault ('ErrorLevel', Success=ok)

   IF (errorLevel EQ 0) THEN errorLevel = 2 ; default is to pop up a dialog

   ; Set up the Catch error handler (unless catch is inhibited)
   IF (errorLevel LT 3) THEN Catch, theError $
   ELSE theError = 0

   ; If an error has occurred ...
   IF theError NE 0 THEN $
   BEGIN

      ; Cancel the error handler and set up the error handling to "throw" error
      CATCH, /Cancel

      ; If this is a standard CATATOM object method, report it's failed
      IF (Obj_IsA_Valid (self, 'CatAtom')) THEN self -> Report, /Failed

      ; Get the call stack.
      HELP, Calls=callstack
      callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]
      HELP, /Last_Message, Output=msg
      ON_ERROR, 2

      ; If the error has been previously handled, don't handle it here
      positions = StrPos(msg, '[cat_handled]')
      foundit = Where(positions NE -1, count)
      IF count GT 0 THEN $
      BEGIN
         msg[0] = '[cat_handled]'
         IF Scope_Level() GT 2 $
            THEN MESSAGE, msg[0] $
            ELSE BEGIN
               Message, /RESET
               RETURN
            ENDELSE
      END
      ; Report the error
      IF (Obj_IsA_Valid (self, 'CatAtom')) THEN self -> Error $
      ELSE CASE errorLevel OF

            1 : BEGIN
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                Print, ''
                FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
             END

            2 : BEGIN
                dialog_msg = TextLineFormat(msg[0])
                junk = Dialog_Message(dialog_msg)
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                Print, ''
                FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
              END

            ELSE : BEGIN
                Print, ''
                Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
                Print, ''
                FOR j=0,N_Elements(msg)-1 DO Print, "     " + msg[j]
              END

          ENDCASE

      ; Throw the error, signalling that it's already been handled
      positions = StrPos(msg, '[cat_handled]')
      foundit = Where(positions NE -1, count)
      IF count GT 0 THEN MESSAGE, msg[0] + ' [cat_handled]' ELSE RETURN

   ENDIF

   ; If this is a standard CATATOM object method, report it's start
   IF (Obj_IsA_Valid (self, 'CatAtom')) THEN self -> Report, /Started
