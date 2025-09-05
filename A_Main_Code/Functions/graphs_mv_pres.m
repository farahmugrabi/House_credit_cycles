function [T,S,C] = graphs_mv_pres(Date,Ym,Z,R,R_s,M_,fpath,ctry)
%__________________________________________________________________________
% graphs(Date,Ym,Z,R,M_)
% Shows sets ofgraphs for the presentation of the unobserved components 
% and saves them in a specified folder.
% INPUT
%  Date
%  Ym
%  Z
%  R & R_s
%  M
%  Label
%  flag
%  county
%
% OUTPUT
%  T   
%  S   
%  C   
%__________________________________________________________________________
%  get(0,'ScreenSize');
%  set(0,'defaulttextinterpreter','arial');  
   set(0,'defaultaxesfontname','arial')
   set(0,'Units','normalized');
   set(0,'Units','centimeters');
  
  Cycles_CF      = CF_filter(Ym,32,120);
  Cycles_CF(:,1) = CF_filter(Ym(:,1),8,32);  

  % Datp  = Date(:,1) + (Date(:,2)-1) / 4;
  Datp  = Date(:,1) + (Date(:,1)-1) / 4;

  Label{1} = 'GDP';
  Label{2} = 'Credit';
  Label{3} = 'House Prices';

figure('position', [0, 0, 825, 400])
  for i = 1:size(Ym,2)
      
       j1 = M_.Trend;
       j2 = M_.Slope;
       j3 = M_.Cycle;
       
       T(:,i) = R_s.Am(:,j1) * (Z(i,j1))';    
       T(:,i) = R_s.Am(:,j1(i));
       S(:,i) = R_s.Am(:,j2(i));
       C(:,i) = R_s.Am(:,j3) * (Z(i,j3))';
       
       Cr = R.Am(:,M_.Cycle);
       
     % Data and Trend
       subplot(2,3,i)
       plot(Datp,Ym(:,i),'k')
       set(gca,'FontSize',8);                   
       title(Label{i}, 'FontSize',9)
       set(gca,'Ytick',[])
       xlim([Datp(1) Datp(end)]); 
      
     % CF
      subplot(2,3,3+i)   
      plot(Datp,Cycles_CF(:,i),'Color','k') 
      set(gca,'FontSize',8);                    
      hline = refline(0,0);
      set(hline,'Color','k')
      title('Christiano-Fitzgerald filter', 'FontSize',9)  
      xlim([Datp(1) Datp(end)]);
      m = abs(Cycles_CF(:,i));
      u = max(m(:))+ 0.005; 
      if u >= 0.1
         a = round(u*10)/10; 
      elseif u<0.1
         a = 0.1; 
      end    
      set(gca,'YTick',-a:a:a); 
      if u > a
      ylim([-u u]); 
      else  
      ylim([-a a]);
      end 
      
  end
  
  if ~isempty(fpath)
   NameFigure = [ctry,'_pres_ov'];
   saveTightFigure(gcf,NameFigure,fpath);  
  end

end

