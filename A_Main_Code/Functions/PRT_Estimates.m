function [Cyc_t] = PRT_Estimates(Date,Ym,M_,S,p_,t_1,t_2)
%__________________________________________________________________________
% [Cyc_t] = [Cyc_t] = PRT_Estimates(Date,Ym,M_,S,p_)
% Produces pseudo real-time estimates of cycles and sample statistics
% i.e. estimates a(t|t+h) of the state vector based on parameters p_
%
%
% INPUT
%    t_1               Start period for calculating sample statistics
%    t_2               Final period for calculating sample statistics 
%
% OUTPUT               Sample statistics of PRT estimates
%    Cyc_t.Cxx         [T x k]  Estimates a(t|t+h) of cyclical components
%__________________________________________________________________________
   ix        = M_.Cycle;
   Cyc_t.C00 = nan(size(Ym));
   Cyc_t.C04 = nan(size(Ym));
   Cyc_t.C12 = nan(size(Ym));
   Cyc_t.C20 = nan(size(Ym));

% PRT estimates   
  for t = 24:size(Ym,1)
       R_0   =  AKF(Ym(1:t,:),M_.Model,p_); 
       R_0   =  ADS(R_0,M_.Model,p_);
       R_0   =  STS(R_0,M_.Model,p_);
        
       Cyc_t.C00(t    ,:)  =  R_0.Am(t   ,ix) * S.Z(:,ix)';  
       Cyc_t.C04(t-4  ,:)  =  R_0.Am(t-4 ,ix) * S.Z(:,ix)';  
       Cyc_t.C12(t-12,:)   =  R_0.Am(t-12,ix) * S.Z(:,ix)';  
       Cyc_t.C20(t-20,:)   =  R_0.Am(t-20,ix) * S.Z(:,ix)';  
  end  
  
% Calc std devs, std errros and show graph
  disp(' ')
  disp(['' kron('_',ones(1,75))])
  disp('Real-time estimates: sample moments cycles')
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
       disp('Std dev cycles (%)')
       disp(num2str([s04(3) s04(2) s12(2) s20(1)*100],'%10.3f'))
       disp('Std error revisions (%)')
       disp(num2str([0 s04(6) s12(6) s20(6)],'%10.3f'))
        
       Datf = Date(:,1); %3d modification to the original code, selecting only Date column-Farah
       %Datf = Date* [1;0.25]; %Original line-Farah
       subplot(size(Ym,2),1,s) 
       plot(Datf,[Cyc_t.C00(:,s) Cyc_t.C20(:,s)])
       if s==1; 
           legend('C0','C20');        
           title('PRT Estimates UOC model')
       end
   end 
 