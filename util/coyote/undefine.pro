;+
; NAME:
;       UNDEFINE
;
; PURPOSE:
;       The purpose of this program is to delete or undefine
;       an IDL program variable from within an IDL program or
;       at the IDL command line. It is a more powerful DELVAR.
;
; AUTHOR:
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1642 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;       Utilities.
;
; CALLING SEQUENCE:
;       UNDEFINE, variable
;
; REQUIRED INPUTS:
;       variable: The variable to be deleted. Up to 10 variables may be specified as arguments.
;
; SIDE EFFECTS:
;       The variable no longer exists.
;
; EXAMPLE:
;       To delete the variable "info", type:
;
;        IDL> Undefine, info
;
; MODIFICATION HISTORY:
;       Written by David Fanning, 8 June 97, from an original program
;       given to me by Andrew Cool, DSTO, Adelaide, Australia.
;       Simplified program so you can pass it an undefined variable. :-) 17 May 2000. DWF
;       Simplified it even more by removing the unnecessary SIZE function. 28 June 2002. DWF.
;       Added capability to delete up to 10 variables at suggestion of Craig Markwardt. 10 Jan 2008. DWF.
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
PRO UNDEFINE, var0, var1, var2, var3, var4, var5, var6, var7, var8, var9
   var0 = 0 & dummy = Temporary(var0)
   var1 = 0 & dummy = Temporary(var1)
   var2 = 0 & dummy = Temporary(var2)
   var3 = 0 & dummy = Temporary(var3)
   var4 = 0 & dummy = Temporary(var4)
   var5 = 0 & dummy = Temporary(var5)
   var6 = 0 & dummy = Temporary(var6)
   var7 = 0 & dummy = Temporary(var7)
   var8 = 0 & dummy = Temporary(var8)
   var9 = 0 & dummy = Temporary(var9)
END