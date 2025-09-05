function ix  = mkindex(sx)
%__________________________________________________________________________
% function ix  = mkindex(sx)                                                        
% PURPOSE: Converts the vector SX of 0/1 elements into a vector      
%          of indices                                                
% INPUT:   SX   vector of ones and zeros                             
% OUTPUT:  IX   vector of indices for which SX[i] = 1                
%__________________________________________________________________________
   
  ix = find(sx);