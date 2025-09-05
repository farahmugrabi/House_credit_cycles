function [T,S,C] = graphs_mv_annex(Date,Ym,Z,R,R_s,M_,fpath,ctry)
%__________________________________________________________________________
% graphs(Date,Ym,Z,R,M_)
% Shows sets of graphs for the annex of the unobserved components and saves 
% them in a specified folder.
% INPUT
%  Date
%  Ym
%  Z
%  R & R_s
%  M
%  Label
%  flag
%  ctry
%
% OUTPUT
%  T   
%  S   
%  C   
%__________________________________________________________________________
% get(0,'ScreenSize');
% set(0,'defaulttextinterpreter','latex');  
  set(0,'defaultaxesfontname','arial')
  set(0,'Units','normalized');
  set(0,'Units','centimeters');
  
  Cycles_CF      = CF_filter(Ym,32,120);
  Cycles_CF(:,1) = CF_filter(Ym(:,1),8,32);  

  Datp  = Date(:,1) + (Date(:,2)-1) / 4;
  
  Label{1} = '$Y_{t}$: data and trend';
  Label{2} = '$C_{t}$: data and trend';
  Label{3} = '$P_{t}$: data and trend';

  figure('position', [0, 0, 825, 1000])
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
       subplot(5,3,i)
       plot(Datp,Ym(:,i),'Color',[0.5 , 0.5 , 0.5])
       hold on
       plot(Datp, T(:,i), 'Color', 'k')
       
       set(gca,'FontSize',8);          % set size of axis numbers
       title(Label{i}, 'FontSize',9,'interpreter', 'latex')
       hL = legend('data','trend', 'Location', 'north', ...
                   'Orientation', 'horizontal');
       set(hL,'FontSize', 6,'interpreter', 'latex')
       set(gca,'Ytick',[])
       xlim([Datp(1) Datp(end)]);
       
    % Data and Trend (diff) 
       subplot(5,3,3+i)
       plot(Datp(2:end),diff(Ym(:,i)),'Color',[0.5 , 0.5 , 0.5]) 
       hold on
       plot(Datp(2:end),diff(T(:,i)),'Color','b')
       hold on
       plot(Datp(2:end),S(2:end,i),'Color','r') 
       
       set(gca,'FontSize',8); % set size of axis numbers
       hline = refline(0,0);
       set(hline,'Color','k')
       hL = legend('$\Delta$ data','$\Delta$ level','slope',  ... 
                   'Location','north','Orientation','horizontal');
%        set(hL,'FontSize', 6,'interpreter', 'latex',  ... 
%               'PlotBoxAspectRatio',[1 0.0825 1]); Farah, .problem here
       title('Data and trend ($1^{st}$ differences)', 'interpreter', ... 
              'latex', 'FontSize',9)
          
       xlim([Datp(1) Datp(end)]);
       m = abs(diff(Ym(:,i)));
       u = max(m(:))+0.005;        %  max val of all series in this subplot
       if u >= 0.1
          a = round(u*10)/10;      %  if u is big, take rounded value as max
       elseif u< 0.1 && u>= 0.05
          a = 0.1;
       elseif u < 0.05 
          a = 0.05;                % if u falls within conventional bounds 
                                   % take [-0.05, 0.05] or [-0.1, 0.1]
       end
       set(gca,'YTick',-a:a:a);    % determines the unit of the y-axis.
       if u > a
       ylim([-u u]);               % determines the range of the y-axis
       else  
       ylim([-a a]);               % determines the range of the y-axis 
       end
       
     % Cycle
       subplot(5,3,2*3+i)
       plot(Datp,C(:,i),'Color','k')
       set(gca,'FontSize',8);          % set size of axis numbers
       hline = refline(0,0);
       set(hline,'Color','k')
       title('Smoothed cycle','interpreter', 'latex', 'FontSize',9)
       xlim([Datp(1) Datp(end)]);
       m = abs(C(:,i));
       u = max(m(:))+ 0.005;       %  max val of all series in this subplot
       if u >= 0.1
          a = round(u*10)/10;      %  if u is big, take rounded value as max
       elseif u<0.1
          a = 0.1;                 %  if u falls within conventional bounds 
                                   %  take [-0.1, 0.1] as the range
       end    
       set(gca,'YTick',-a:a:a); % determines the unit of the y-axis.
       if u > a
       ylim([-u u]);               % determines the range of the y-axis
       else  
       ylim([-a a]);               % determines the range of the y-axis
       end 
       
    % V
      subplot(5,3,3*3+i)   
      s = nanstd(R.Vm(:,i));
      plot(Datp,R.Vm(:,i)/s,'Color','k') 
      set(gca,'FontSize',8); % set size of axis numbers
      hline = refline(0,0);
      set(hline,'Color','k')
      title('Standardized prediction errors','interpreter', ... 
            'latex', 'FontSize',9) 
      xlim([Datp(1) Datp(end)]);
      m = abs(R.Vm(:,i)/s);
      u = max(m(:))+ 0.005; 
      if u >= 5
         a = round(u); 
      elseif u<5
         a = 5; 
      end    
      set(gca,'YTick',-a:a:a); 
      if u > a
      ylim([-u u]); 
      else  
      ylim([-a a]); 
      end 
      
    % CF
      subplot(5,3,4*3+i)   
      plot(Datp,Cycles_CF(:,i),'Color','k') 
      set(gca,'FontSize',8); % set size of axis numbers
      hline = refline(0,0);
      set(hline,'Color','k')
      title('Christiano-Fitzgerald filter','interpreter', ... 
            'latex', 'FontSize',9)  
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
   NameFigure = [ctry,'_annex_ov'];
   saveTightFigure(gcf,NameFigure,fpath);  
  end

end

