;+
; NAME:
;       HISTOPLOT
;
; PURPOSE:
;
;       This program is used to draw a histogram in IDL direct graphics.
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
;      HistoPlot, dataToHistogram
;
; ARGUMENTS:
;
;       dataToHistogram:  The data from which the histogram is created.
;
; INPUT KEYWORDS:
;
;       AXISCOLORNAME:    The name of the axis color. Default: "Navy". (All color names
;                         derived from FSC_COLOR.)
;
;       BACKCOLORNAME:    The name of the background color. Default: "White".
;
;       BINSIZE:          The binsize of the histogram. By default the data range, divided
;                         by NBINS. If the data is an integer, the binsize is rounded to the
;                         nearest integer. If the data is BYTE type, the binsize is 1. The
;                         binsize, if supplied, must have the same data type as the data.
;
;       DATACOLORNAME:    The name of the data color for drawing the histogram outlines.
;                         Default: "Indian Red".
;
;       FILE:             The name of a color name file to use with FSC_COLOR.
;
;       FILLPOLYGON:      Set this keyword to fill the histogram polygons. If this keyword
;                         is set, the following keyword can also be used.
;
;                         POLYCOLOR:    The name, or vector of names, of polygon colors.
;                                       If a vector, the names are cycled though, as needed.
;                                       Defaults to DATACOLORNAME.
;
;       L64:              If set, the return value of HISTOGRAM are 64-bit integers, rather than
;                         the default 32-bit integers.
;
;       LINE_FILL:        If set, the polygons are filled with lines instead of solid color. If
;                         this keyword is set, the following keywords can also be used.
;
;                         ORIENTATION:  The orientation of the lines in line-filled polygons in degrees.
;                         PATTERN:      Set to rectangular array of pixel giving fill pattern.
;                         POLYCOLOR:    The name, or vector of names, of line colors.
;                                       If a vector, the names are cycled though, as needed.
;                         SPACING:      The spacing, in centimeters, between parallel lines.
;
;       MAXINPUT:         The maximum value to use in calculating input histogram. Equivalent to MAX keyword
;                         in HISTOGRAM documentation.
;
;       MAX_VALUE:        The maximum Y data value to represent on graphics plot. Default: Max(histdataToPlot) * 1.05
;
;       MININPUT:         The minimum value to use in calculating input histogram. Equivalent to MIN keyword
;                         in HISTOGRAM documentation.
;
;       MIN_VALUE:        The minimum Y data value to represent on graphics plot. Default: 0.
;
;       NAN:              If set, ignore NAN values in calculating and plotting histogram.
;
;       NBINS:            The number of output bins in the histogram. Meaning is slightly different from
;                         meaning in the HISTOGRAM command. Used only to calculate BINSIZE when BINSIZE is
;                         not specified.
;
;       NOLINES:          Set this keyword if you prefer NOT to draw lines between individual histogram boxes.
;
;       OPLOT:            Set this keyword if you want to overplot data on already established axes.
;
;       OMAX:             The
;
;       The user may also enter any other keywords suitable for the PLOT and POLYFILL commands in IDL.
;
; OUTPUT KEYWORDS:
;
;       HISTDATA:         The output value of the internal HISTOGRAM command.
;
;       LOCATIONS:        Starting locations of each bin. (See HISTOGRAM documentation.)
;
;       OMAX:             The maximum output value used to construct the histogram. (See HISTOGRAM documentation.)
;
;       OMIN:             The minimum output value used to construct the histogram. (See HISTOGRAM documentation.)
;
;       REVERSE_INDICES:  List of reverse indices. (See HISTOGRAM documentation.)
;
; EXAMPLES:
;
;      IDL> Histoplot, Dist(256)
;      IDL> Histoplot, Fix(RandomU(seed, 200)*20), POLYCOLOR=['charcoal', 'steel blue'], /FILLPOLYGON
;      IDL> Histoplot, Fix(RandomU(seed, 200)*20), POLYCOLOR=['navy', 'forest green'], /LINE_FILL, ORIENTATION=[45,-45]
;
; REQUIRES:
;
;     Requires ERROR_MESSAGE and FSC_COLOR from the Coyote Library:
;
;        http://www.dfanning.com/programs/error_message.pro
;        http://www.dfanning.com/programs/fsc_color.pro
;
; MODIFICATION HISTORY:
;
;       Written by:  David W. Fanning, 14 November 2007.
;       Modified to work with !P.MULTI. 20 Nov 2007. DWF.
;       Slight problem with extra space at the right end of the plot resolved. 20 Nov 2007. DWF.
;       Added FILE and NOLINES keywords. 24 Nov 2007. DWF.
;       Added additional HISTOGRAM access via keywords. 24 Nov 2007. DWF.
;       Fixed a small problem with FILLPOLY keyword. 26 Nov 2007. DWF.
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
PRO HistoPlot , $                   ; The program name.
   dataToHistogram, $               ; The data to draw a histogram of.
   AXISCOLORNAME=axisColorName, $   ; The axis color.
   BACKCOLORNAME=backcolorName, $   ; The background color.
   DATACOLORNAME=datacolorName, $   ; The data color.
   _REF_EXTRA=extra, $              ; For passing extra keywords.
   FILE=file, $                     ; For specifying a color name file.
   MAX_VALUE=max_value, $           ; The maximum value to plot.
   MIN_VALUE=min_value, $           ; The minimum value to plot.
   NOLINES=nolines, $               ; No lines between histogram boxes.
   OPLOT=overplot, $                ; Set if you want overplotting.
   ;
   ; POLYFILL KEYWORDS
   ;
   FILLPOLYGON=fillpolygon, $       ; Set if you want filled polygons
   LINE_FILL=line_fill, $           ; Set if you want line-filled polygons.
   ORIENTATION=orientation, $       ; The orientation of the lines.
   PATTERN=pattern, $               ; The fill pattern.
   POLYCOLOR=polycolor, $           ; The name of the polygon draw/fill color.
   SPACING=spacing, $               ; The spacing of filled lines.
   ;
   ; HISTOGRAM OUTPUT KEYWORDS
   ;
   HISTDATA=histdata, $
   LOCATIONS=locations, $
   OMAX=omax, $
   OMIN=omin, $
   REVERSE_INDICES=ri, $
   ;
   ; HISTOGRAM INPUT KEYWORDS
   ;
   BINSIZE=binsize, $               ; The histogram bin size.
   L64=l64, $                       ; Input for HISTOGRAM.
   MAXINPUT=maxinput, $             ; The maximum value to HISTOGRAM.
   MININPUT=mininput, $             ; The minimum value to HISTOGRAM.
   NAN=nan, $                       ; Check for NAN.
   NBINS=nbins                      ; The number of bins to display.


   ; Catch any error in the HistoPlot program.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = Error_Message(!Error_State.Msg + '. Returning...')
      RETURN
   ENDIF

   ; Check for positional parameter.
   IF N_Elements(dataToHistogram) EQ 0 THEN Message, 'Must pass data to histogram.'

   IF N_Elements(nbins) EQ 0 THEN theseBins = 128.0 ELSE theseBins = DOUBLE(nbins)

   ; Check for histogram keywords.
   IF N_Elements(binsize) EQ 0 THEN BEGIN
      range = Max(dataToHistogram) - Min(dataToHistogram)
      CASE Size(DataToHistogram, /TNAME) OF
         'BYTE': binsize = 1
         'FLOAT': binsize = range / theseBins
         'DOUBLE': binsize = range / theseBins
         ELSE: binsize = Round(range / theseBins) > 1
      ENDCASE
   ENDIF ELSE BEGIN
       IF Size(binsize, /TYPE) NE Size(dataToHistogram, /TYPE) THEN $
         Message, 'The BINSIZE type is not the same as DATA type.'
   ENDELSE

   ; Check for keywords.
   IF N_Elements(dataColorName) EQ 0 THEN dataColorName = "Indian Red"
   IF N_Elements(axisColorName) EQ 0 THEN axisColorName = "Navy"
   IF N_Elements(backColorName) EQ 0 THEN backColorName = "White"
   IF N_Elements(polycolor) EQ 0 THEN polycolor = dataColorName
   line_fill = Keyword_Set(line_fill)
   IF line_fill THEN fillpolygon = 1
   IF Keyword_Set(fillpolygon) THEN BEGIN
      fillpolygon = Keyword_Set(fillpolygon)
      IF N_Elements(orientation) EQ 0 THEN orientation = 0
      IF N_Elements(spacing) EQ 0 THEN spacing = 0
   ENDIF
   IF N_Elements(min_value) EQ 0 THEN min_value = 0
   IF N_Elements(mininput) EQ 0 THEN mininput = Min(dataToHistogram)
   IF N_Elements(maxinput) EQ 0 THEN maxinput = Max(dataToHistogram)

   ; Load plot colors.
   TVLCT, r, g, b, /GET
   axisColor = FSC_Color(axisColorName, FILE=file)
   dataColor = FSC_Color(datacolorName, FILE=file)
   backColor = FSC_Color(backColorName, FILE=file)

  ; Calculate the histogram.
   histdata = Histogram(dataToHistogram, $
      BINSIZE=binsize, $
      L64=l64, $
      MAX=maxinput, $
      MIN=mininput, $
      NAN=nan, $
      LOCATIONS=locations, $
      OMAX=omax, $
      OMIN=omin, $
      REVERSE_INDICES=ri)

   ; Have to fudge the bins and histdata variables to get the
   ; histogram plot to make sense.
   npts = N_Elements(histdata)
   halfbinsize = binsize / 2.0
   bins = Findgen(N_Elements(histdata)+1) * binsize + mininput
   binsToPlot = [bins[0], bins[0:npts-1] + halfbinsize, bins[npts-1] + binsize]
   histdataToPlot = [histdata[0], histdata, histdata[npts-1]]
   ytitle = 'Histogram Density'
   ytickformat = '(I)'
   IF N_Elements(max_value) EQ 0 THEN max_value = Max(histdataToPlot) * 1.05
   yrange = [min_value, max_value]

   ; Set up data coordinate space by drawing plot without anything showing.
   IF ~Keyword_Set(overplot) THEN BEGIN

      ; Trouble doing this with !P.MULTI, so check and save variables.
      IF Total(!P.MULTI) NE 0 THEN BEGIN
         bangp = !P
         bangx = !X
         bangy = !Y
         bangmap = !MAP
      ENDIF
      Plot, binsToPlot, histdataToPlot, $            ; The fudged histogram and bin data.
            Background=backcolor, $                  ; The background color of the display.
            NoData=1, $                              ; Draw the axes only. No data.
            XStyle=5, $                              ; Exact axis scaling. No autoscaled axes.
            YRange=yrange, $                         ; The Y data range.
            YStyle=5, $                              ; Exact axis scaling. No autoscaled axes.
            _Extra=extra, $                          ; Pass any extra PLOT keywords.
            XTICKFORMAT='(A1)', YTICKFORMAT='(A1)'
      IF Total(!P.MULTI) NE 0 THEN BEGIN
         bangAfterp = !P
         bangAfterx = !X
         bangAftery = !Y
         bangAftermap = !MAP
      ENDIF
   ENDIF

   ; Do we need to have things be filled?
   IF Keyword_Set(fillpolygon) THEN BEGIN

       ncolors = N_Elements(polycolor)

      ; Are we line filling?
      IF line_fill THEN BEGIN

         norient = N_Elements(orientation)
         nspace = N_Elements(spacing)

         FOR j=0L,N_Elements(bins)-2 DO BEGIN
            x = [bins[j], bins[j], bins[j+1],  bins[j+1], bins[j]]
            y = min_value > [!Y.CRange[0], histDataToPlot[j+1], $
               histDataToPlot[j+1], !Y.CRange[0], !Y.CRange[0]] < max_value
            fillcolor = polycolor[j MOD ncolors]
            orient = orientation[j MOD norient]
            space = spacing[j MOD nspace]
            PolyFill, x, y, COLOR=FSC_Color(fillcolor, FILE=file), /LINE_FILL, ORIENTATION=orient, $
               PATTERN=pattern, SPACING=space, _Extra=extra
         ENDFOR

      ENDIF ELSE BEGIN ; Normal polygon color fill.

         FOR j=0L,N_Elements(bins)-2 DO BEGIN
            x = [bins[j], bins[j], bins[j+1],  bins[j+1], bins[j]]
            y = min_value > [!Y.CRange[0], histDataToPlot[j+1], histDataToPlot[j+1], $
                             !Y.CRange[0], !Y.CRange[0]] < max_value
            fillcolor = polycolor[j MOD ncolors]
            PolyFill, x, y, COLOR=FSC_Color(fillcolor, FILE=file), _Extra=extra
         ENDFOR

      ENDELSE
   ENDIF

   ; Plot the histogram of the display dataToHistogram. Do this after to repair
   ; damage to plot from POLYFILL.
   IF ~Keyword_Set(overplot) THEN BEGIN
      IF Total(!P.MULTI) NE 0 THEN BEGIN
         !P = bangp
         !X = bangx
         !Y = bangy
         !MAP = bangmap
      ENDIF
      Plot, binsToPlot, histdataToPlot, $            ; The fudged histogram and bin data.
            Color=axiscolor, $                       ; The color of the axes.
            NoData=1, $                              ; Draw the axes only. No data.
            XStyle=1, $                              ; Exact axis scaling. No autoscaled axes.
            XTitle='Data Value', $                   ; The title of the X axis.
            YMinor=1, $                              ; No minor tick mark on X axis.
            YRange=yrange, $                         ; The Y data range.
            YStyle=1, $                              ; Exact axis scaling. No autoscaled axes.
            YTickformat=ytickformat, $               ; The format of the Y axis annotations.
            YTitle=ytitle, $                         ; The title of the Y axis.
            /NOERASE, $                              ; Don't want to erase now!
            _Strict_Extra=extra                      ; Pass any extra PLOT keywords.
      IF Total(!P.MULTI) NE 0 THEN BEGIN
         !P = bangAfterp
         !X = bangAfterx
         !Y = bangAftery
         !MAP = bangAftermap
      ENDIF
   ENDIF

   ; Make histogram boxes by drawing lines in data color.
   IF Keyword_Set(nolines) THEN BEGIN
      last = N_Elements(bins)-1
      PlotS, [bins[0], bins[0]], min_value > [!Y.CRange[0], histDataToPlot[0]] < max_value, $
            Color=dataColor
      PlotS, [bins[last], bins[last]], min_value > [!Y.CRange[0], histDataToPlot[last]] < max_value, $
            Color=dataColor
   ENDIF ELSE BEGIN
      FOR j=0L, N_Elements(bins)-1 DO BEGIN
            PlotS, [bins[j], bins[j]], min_value > [!Y.CRange[0], histDataToPlot[j]] < max_value, $
               Color=dataColor
      ENDFOR
   ENDELSE

   ; Overplot the histogram data in the data color.
   OPlot, binsToPlot, histdataToPlot, PSym=10, Color=dataColor

   ; Clean up.
   TVLCT, r, g, b
END
