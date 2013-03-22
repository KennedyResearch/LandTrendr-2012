;**************************************************************************** 
;Copyright © 2008-2011 Oregon State University                                
;All Rights Reserved.                                                         
;                                                                             
;                                                                             
;Permission to use, copy, modify, and distribute this software and its        
;documentation for educational, research and non-profit purposes, without     
;fee, and without a written agreement is hereby granted, provided that the    
;above copyright notice, this paragraph and the following three paragraphs    
;appear in all copies.                                                        
;                                                                             
;                                                                             
;Permission to incorporate this software into commercial products may be      
;obtained by contacting Oregon State University Office of Technology Transfer.
;                                                                             
;                                                                             
;This software program and documentation are copyrighted by Oregon State      
;University. The software program and documentation are supplied "as is",     
;without any accompanying services from Oregon State University. OSU does not 
;warrant that the operation of the program will be uninterrupted or           
;error-free. The end-user understands that the program was developed for      
;research purposes and is advised not to rely exclusively on the program for  
;any reason.                                                                  
;                                                                             
;                                                                             
;IN NO EVENT SHALL OREGON STATE UNIVERSITY BE LIABLE TO ANY PARTY FOR DIRECT, 
;INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST      
;PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN 
;IF OREGON STATE UNIVERSITYHAS BEEN ADVISED OF THE POSSIBILITY OF SUCH        
;DAMAGE. OREGON STATE UNIVERSITY SPECIFICALLY DISCLAIMS ANY WARRANTIES,       
;INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
;FITNESS FOR A PARTICULAR PURPOSE AND ANY STATUTORY WARRANTY OF               
;NON-INFRINGEMENT. THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS,    
;AND OREGON STATE UNIVERSITY HAS NO OBLIGATIONS TO PROVIDE MAINTENANCE,       
;SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.                            
;                                                                             
;**************************************************************************** 

pro ortho_regress, X, Y, a, Xm, Ym, sigma_a, sigma_b
  ; performs orthogonal regression on column vectors X and Y
  ; regression line is Y = Ym + a(X-Xm) = (Ym-aXm) + aX = b + aX
  ; returns a = slope, Xm, Ym, sigma_a, sigma_b
  n = n_elements(X)
  Xm = mean(X)
  Ym = mean(Y)
  S = correlate([X,Y],/covariance,/double)
  Sxx = S[0,0]
  Syy = S[1,1]
  Sxy = S[1,0]
  eiv = eigenql(S,eigenvectors=eigenvectors,/double)
  ; slope
  a = -eigenvectors[0,1]/eigenvectors[1,1]
  ; intercept
  b = Ym-a*Xm
  ; std errors
  sigma2 = n*(Syy-2*a*Sxy+a*a*Sxx)/((n-2)*(1+a^2))
  tau = sigma2*a/((1+a^2)*Sxy)
  sigma_a = sqrt( sigma2*a*(1+a^2)*(1+tau)/(n*Sxy) )
  sigma_b = sqrt( sigma2*a*(1+a^2)*(Xm^2*(1+tau)+Sxy/a)/(n*Sxy) )
end