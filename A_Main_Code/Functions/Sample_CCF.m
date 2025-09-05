function [CC,Cmax,Clag] = Sample_CCF(Cycles,D_,k,flag)
%__________________________________________________________________________
% [CC,Cmax] = Sample_CCF(Cycles,D_,k,flag)
%
% Calculates empirical cross correlations among series Cycles
% Graphs them together with theoretical ones as provided by D_
% INPUT
%   Cycles          Smoothed Cycles                 [T x n]
%   D_              Structure
%                   D_.CC  ...  Theoretical CCF      [m x n x n] 
%                   D_std  ...  Theoretical std dev  [n x 1]  
%                   
%                   If D_ is not available set D_.CC = D_s = nan;
%
%   k               Nr of cross corr to calculate
%   flag            logical: 1 = use theoretical ACF
%__________________________________________________________________________
   n  = size(Cycles,2);
   
 % Standard deviations
   disp(' ')
   disp(['' kron('_',ones(1,75))])
   disp('Standard deviations Cycles * 100');
   disp(['' kron('_',ones(1,75))])
   if flag 
      disp('From parameter estimates');
      disp(num2str((100*D_.std)','%10.4f')) 
   end
   disp('Sample estimates');
   disp(num2str((100*std(Cycles)')','%10.3f')) 

 % Calculate Sample CCF  
   CC     = nan(n,n,2*k+1);
   Cmax   = nan(n,n); 
      
   for i = 1:n
   for j = 1:n
       %c         = crosscorr(Cycles(:,i),Cycles(:,j),k);
       c         = xcorr_nv(Cycles(:,i),Cycles(:,j),k);
       CC(i,j,:) = c;
       Cmax(i,j) = max(abs(c));
       Clag(i,j) = find(Cmax(i,j)==abs(c));
   end
   end
      
% Graphs
  if flag
     m = min(k,size(D_.CC,1));
     figure('position', [0, 0, 600, 600]) 
     
     for i = 1:n
     for j = 1:n    
      x1          = squeeze(CC(i,j,:));
      x2          = nan(size(x1));
      if flag
         x2(k+1:k+m) = squeeze(D_.CC(1: 1:m,j,i));
         x2(  1:  m) = squeeze(D_.CC(m:-1:1,i,j));
      end

      subplot(n,n,(i-1)*n+j)
      plot(-k:k,[x1 x2])
      title(['CCF cycles'])
      if i == 1 && j == 1
         legend('Smpl','ACF','location','Best')
      end
    end
    end 
  end
   