;+
; NAME:
;       Terminator_Map
;
; PURPOSE:
;
;       This is a program for viewing a map of the Earth with a day/night terminator.
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
;
;       Graphics
;
; CALLING SEQUENCE:
;
;       Terminator_Map, center_lon, center_lat, Map_Projection='Aitoff'
;
; INPUT ARGUMENTS:
;
;       center_lon:      The longitude, in degrees, of the center of the map, (-180 to 180). Defaults to 0.0.
;
;       center_lat:      The latitude, in degrees, of the center of the map, (-90 to 90). Defaults to 0.0.
;                        Latitudes other than zero can cause program problems. See the NOTES section for details.
;
; INPUT KEYWORDS:
;
;       FLIPDAY:         The program suffers from an inability to always correctly distinguish the
;                        "day" side of the day/night terminator (see NOTES section). If the program
;                        gets it wrong, you can use this keyword to "flip" the day/night area. For
;                        example, here is a call that gets it wrong:
;
;                            Terminator_Map, 0, 90.0, MAP_PROJECTION=4, time="Thu Apr 13 08:49:34 2006"
;
;                        Unfortunately, you can only know if it is "wrong" by visual inspection. The sun
;                        is always placed correctly. I realize this limits the usefulness of the program,
;                        generally, but it is a limitation I am not motivated enough to fix. That is to
;                        say, the program meets my limited needs perfectly. :-)
;
;       KEEP_ASPECT:     If set, the aspect ratio of the map is more or less preserved.
;
;       MAP_PROJECTION:  Either the number or the name of one of the following map projection types. Defaults to "CYLINDRICAL".
;                        0 CYLINDRICAL
;                        1 GOODESHOMOLOSINE
;                        2 HAMMER
;                        3 LAMBERT
;                        4 MERCATOR
;                        5 MILLER_CYLINDRICAL
;                        6 MOLLWEIDE
;                        7 ROBINSON
;
;      TIME:             A date/time string of nearly any format. The month, however, should be represented
;                        as a three-letter string. The time is written as hh:mm:ss. Examples of date/time strings
;                        are these:
;
;                        'Wed Apr 12 22:39:52 2006'
;                        '12 APR 2006 04:16:00'
;                        'Apr 12, 2006 04:16:00'
;                        'Nov 12, 2006'
;
;                        If undefined, the current day and time (from SYSTIME) is used.
;
; OUTPUT KEYWORDS:
;
;       IMAGE:           If passed a named variable, will return a 24-bit image of the result in the display window.
;
; DEPENDENCIES:
;
;       Requires both the Coyote Library and the JHUAPL Library to be on the !PATH, since many programs
;       from both are used.
;
;           http://www.dfanning.com/programs/coyotefiles.zip
;           http://fermi.jhuapl.edu/s1r/idl/s1rlib/local_idl.html
;
; EXAMPLE:
;
;          Window, XSize=800, YSize=400
;          Terminator_Map
;
; NOTES:
;
;       The spherical geometry problems inherent with working the map projections are not
;       completely solved in this program. The main problem seems to be that if the
;       terminator polygon (represented in the program by the variables lon_terminus
;       and lat_terminus) overlaps the pole regions, then stardard methods for determining
;       if a point is inside or outside a polygon are not reliable. (*All* longitudes are
;       inside the polygon if the polygon overlaps a pole!) In the program, this manifests
;       itself as the sun side of the terminator appearing dark and the night side appearing
;       light. Hence, the FLIPDAY keyword.
;
;       To some extent, this problem can be eliminated by setting the CENTER_LAT parameter
;       to zero, at least with some map projections. I've thought about forcing it to be
;       zero and forcing you to use well-behaved map projections, but have decided to leave
;       the program the way it is so you can discover the limitations yourself.
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, April 12, 2006.
;-
;
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
PRO Terminator_Map, center_lon, center_lat, $
   FlipDay=flipday, $
   Image=image, $
   Map_Projection=map_index, $
   Keep_Aspect=keep_aspect, $
   Time=time

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      void = Error_Message()
      RETURN
   ENDIF

   mapProjections = ['Cylindrical', 'Goodeshomolosine', 'Hammer', $
      'Lambert', 'Mercator', 'Miller_Cylindrical', 'Mollweide', 'Robinson']

   ; Check arguments.
   IF N_Elements(center_lon) EQ 0 THEN center_lon = 0
   IF N_Elements(center_lat) EQ 0 THEN center_lat = 0
   IF N_Elements(time) EQ 0 THEN time = Systime()
   IF N_Elements(map_index) EQ 0 THEN map_index=0 ELSE BEGIN
      IF Size(map_index, /TNAME) NE 'STRING' THEN BEGIN
         map_index = 0 > map_index < 7
      ENDIF ELSE BEGIN
         index = Where(StrMid(StrUpCase(mapProjections), 0, $
     StrLen(map_index)) EQ StrUpCase(map_index), count)

         IF count EQ 0 THEN BEGIN
            Message, 'Cannot find map projection: ' + map_index +    '. Using CYLINDRICAL.', /Informational
            map_index = 0
         ENDIF ELSE map_index = index
      ENDELSE
   ENDELSE
   IF Keyword_Set(keep_aspect) THEN BEGIN
      CASE map_index OF
         3:    position = Aspect(1.0, Margin=0.05)     ; Lambert
         4:    position = Aspect(2.0/3.0, Margin=0.05) ; Mercator
         6:    position = Aspect(2.0/3.0, Margin=0.05) ; Miller_Cyindrical
         ELSE: position = Aspect(0.5, Margin=0.05)
      ENDCASE

   ENDIF ELSE position = [0.0, 0.0, 1.0, 1.0]

   ; Set up the map projection space.
   CASE map_index OF
      0: Map_Set, center_lat, center_lon, 0, Position=position, /NoErase, /CYLINDRICAL
      1: Map_Set, center_lat, center_lon, 0, Position=position, /NoErase, /GOODESHOMOLOSINE
      2: Map_Set, center_lat, center_lon, 0, Position=position, /NoErase, /HAMMER
      3: Map_Set, center_lat, center_lon, 0, Position=position, /NoErase, /LAMBERT
      4: Map_Set, center_lat, center_lon, 0, Position=position, /NoErase, /MERCATOR
      5: Map_Set, center_lat, center_lon, 0, Position=position, /NoErase, /ROBINSON
      6: Map_Set, center_lat, center_lon, 0, Position=position, /NoErase, /MILLER_CYLINDRICAL
      7: Map_Set, center_lat, center_lon, 0, Position=position, /NoErase, /MOLLWEIDE
   ENDCASE

   ; Obtain the AVHRR image, along with its color table vectors, to display.
   filename = Filepath(SubDirectory=['examples', 'data'], 'avhrr.png')
   image = Read_PNG(filename, r, g, b)

   ; Create a sun symbol to plot.
   phi = Findgen(32) * (!PI * 2 / 32.)
   phi = [ phi, phi(0)]
   UserSym, Cos(phi)*2, Sin(phi)*2, /Fill, Color=Fsc_Color('yellow')

   ; Find the latitude and longitude of the sub-solar point at the
   ; time the user specified. First, sort out the time values.
   time_copy = time
   offset = GMT_OFFSEC()                      ; Find correction from local time to Universal Time (UT).
   DT_TM_INC, time_copy, offset               ; Convert time to UT.
   DT_TM_BRK, time_copy, date_part, time_part ; Break into a date and a time part.
   DATE2YMD, date_part, y, m, d               ; Break data into year, month, and day.
   jd = YMD2JD(y, m, d)                       ; Convert date to julian day.
   ut = SECSTR(time_part)/3600.               ; Convert time string (in seconds) to sun ephemeris time in hours.

    ; Calculate solar RA and DEC and distance (in AU).
   SUN, y, m, d, ut, $
      APP_RA=ra, $           ; Apparent right accession.
      APP_DEC=dec, $         ; Apparent declination.
      DIST=sun_distance      ; Distance to sun in AU.

   ; Calculate local mean sidereal time. LMST returns value as fraction of a day,
   lm_sidereal_time = LMST(jd, ut/24.0, 0) * 24

   ; Calculate sub-solar point.
   sun_lat = dec
   sun_lon = 15.0 * (ra - lm_sidereal_time)

   ; Calculate the terminus.
   earthRadius = 6.371009D6 ; The mean Earth radius.
   scanAngle = ASIN(earthRadius / (sun_distance * 1.4956D11))
   arc_distance = !DPI - 1.57080D - scanAngle  ; 90 degrees - scanangle (but here in radians)

   lon_terminus = FltArr(36)
   lat_Terminus = FltArr(36)
   FOR j=0,35 DO BEGIN
      azimuth = j * 10.0
      results = LL_Arc_Distance([sun_lon, sun_lat], arc_distance, azimuth, /Degrees)
      lon_terminus[j] = results[0]
      lat_terminus[j] = results[1]
   ENDFOR

   ; Set up the Z graphics buffer
   thisDevice = !D.Name
   xsize = !D.X_Size
   ysize = !D.Y_Size
   Set_Plot, 'Z'
   Device, Set_Resolution=[xsize,ysize], Z_Buffer=0
   Erase, Color=0

   ; Prevent underflow warnings.
   except = !Except
   !Except = 0
   PolyFill, lon_terminus, lat_terminus, Color=128
   void = Check_Math()
   !Except = except
   snapshot = TVRD(position[0]*xsize, position[1]*ysize, $
      (position[2] - position[0])*xsize, (position[3] - position[1])*ysize)

   ; One would think that the subsolar position would "define" the day side
   ; of the day/night terminator. But it is not as simple as that. The code
   ; commented out below, for example, does NOT work because normal methods
   ; for determining if a point is inside or outside a polygon are unreliable
   ; for spherical polygons that overlap the poles.
   ;   insideit = Inside(sun_lon, sun_lat, lon_terminus, lat_terminus)
   ;   IF insideit THEN $
   ;      dark = Where(snapshot NE 128, count) ELSE $
   ;      dark = Where(snapshot EQ 128, count)
   IF Keyword_Set(flipday) THEN $
      dark = Where(snapshot NE 128, count) ELSE $
      dark = Where(snapshot EQ 128, count)

   ; Display the warped image.
   warp = Map_Image(image, x, y, xs, ys, Compress=1, Missing=254)
   TVImage, warp, x, y, XSIZE=xs, YSIZE=ys, /TV
   img = TVRD(position[0]*xsize, position[1]*ysize, $
      (position[2] - position[0])*xsize, (position[3] - position[1])*ysize)
   red = r[img]
   grn = g[img]
   blu = b[img]
   Color_Convert, red, grn, blu, h, l, s, /RGB_HLS
   rr = red
   gg = grn
   bb = blu
   Color_Convert, h, l*0.5, s, red, grn, blu, /HLS_RGB
   rr[dark] = red[dark]
   gg[dark] = grn[dark]
   bb[dark] = blu[dark]

   Set_Plot, thisDevice

   ; Have to re-establish map projection space after changing to another device,
   ; according to MAP_SET documentation.
   CASE map_index OF
      0: Map_Set, center_lat, center_lon, 0, Position=position, /NoErase, /CYLINDRICAL
      1: Map_Set, center_lat, center_lon, 0, Position=position, /NoErase, /GOODESHOMOLOSINE
      2: Map_Set, center_lat, center_lon, 0, Position=position, /NoErase, /HAMMER
      3: Map_Set, center_lat, center_lon, 0, Position=position, /NoErase, /LAMBERT
      4: Map_Set, center_lat, center_lon, 0, Position=position, /NoErase, /MERCATOR
      5: Map_Set, center_lat, center_lon, 0, Position=position, /NoErase, /ROBINSON
      6: Map_Set, center_lat, center_lon, 0, Position=position, /NoErase, /MILLER_CYLINDRICAL
      7: Map_Set, center_lat, center_lon, 0, Position=position, /NoErase, /MOLLWEIDE
   ENDCASE

   ; Display the image.
   TVImage, [ [[rr]], [[gg]], [[bb]] ], Position=position

   ; Plot the sun on the map.
   PLOTS, sun_lon, sun_lat, psym=8


   ; Add continental outlines and time zones.
   Map_Continents, Color=Fsc_Color('Medium Gray')
   Map_Continents, Color=Fsc_Color('Medium Gray'), /Countries
   Map_Grid, Color=Fsc_Color('Light Gray'), Lons=Scale_Vector(Indgen(25), -180, 180), $
      LonNames = Reverse(StrTrim(Indgen(25),2)), LonLabel=-5, /Label

   IF Arg_Present(image) THEN image = TVRead(True=3)
END