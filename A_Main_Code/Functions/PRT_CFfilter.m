function [Cyc_t] = PRT_CFfilter(Date,Ym,lo,up,t_1,t_2)
%__________________________________________________________________________
% [Cyc_t] = [Cyc_t] = PRT_CFfilter(Date,Ym,lo,up)
% Produces pseudo real-time estimates of cycles and sample statistics
% i.e. estimates a(t|t+h) for the Christiano Fitzgerald filter
%
% INPUT  Ym            Data        T x k   
%        lo            lower bounds k x 1
%        up            upper bounds k x 1
%        t_1           Start period for calculating sample statistics
%        t_2           Final period for calculating sample statistics 
%
% OUTPUT Cyc_t         Sample statistics of PRT estimates
%        Cyc_t.Cxx     [T x n]  Estimates a(t|t+h) of cyclical components
%__________________________________________________________________________

  Cyc_t.C00 = nan(size(Ym));
  Cyc_t.C04 = nan(size(Ym));
  Cyc_t.C12 = nan(size(Ym));
  Cyc_t.C20 = nan(size(Ym));

% Full-sample estimate  
  for s =  1:size(Ym,2)
       Cyc_t.C_T(:,s) = CF_filter(Ym(:,s),lo(s),up(s));
  end

% PRT estimates   
  for s =  1:size(Ym,2)
  for t = 24:size(Ym,1)
       CCC   =  CF_filter(Ym(1:t,s),lo(s),up(s));
 
       Cyc_t.C00(t   ,s)   =  CCC(t);  
       Cyc_t.C04(t-4 ,s)   =  CCC(t-4);  
       Cyc_t.C12(t-12,s)   =  CCC(t-12);  
       Cyc_t.C20(t-20,s)   =  CCC(t-20);  
  end  
  end

% Calc std devs, std errors and show graph
  disp(' ')
  disp(['' kron('_',ones(1,75))])
  disp('Real-time estimates CF filter: sample moments cycles')
  disp(['Stat from ' num2str(Date(t_1,:)) ' to  ' num2str(Date(t_2,:))])
  disp(['' kron('_',ones(1,75))])
  figure
 
  for s = 1:size(Ym,2)
       C00  =  Cyc_t.C00(:,s);
       C04  =  Cyc_t.C04(:,s);
       C12  =  Cyc_t.C12(:,s);
       C20  =  Cyc_t.C20(:,s);
       
       s04  = RT_tests(C20',C04',C00',t_1,t_2,1);
       s12  = RT_tests(C20',C12',C00',t_1,t_2,1);
       s20  = RT_tests(C20',C20',C00',t_1,t_2,1);
        
       disp(' ')
       disp(['Series '  int2str(s)])
       disp('C0        C4        C12        C20')
       disp('Std dev cycles')
       disp(num2str([s04(3) s04(2) s12(2) s20(1)*100],'%10.4f'))
       disp('Std error rev')
       disp(num2str([0 s04(6) s12(6) s20(6)],'%10.4f'))
        
       subplot(size(Ym,2),1,s) 
       plot(1:size(Ym,1),[Cyc_t.C00(:,s) Cyc_t.C20(:,s)])
       if s==1; 
           legend('C0','C20');        
           title('PRT Estimates CF Filter')
       end
   end 
 