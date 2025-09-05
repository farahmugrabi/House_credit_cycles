function [T,S,C] = graphs_mv(Date,Ym,Z,R,R_s,M_,Names)
%__________________________________________________________________________
% graphs(Date,Ym,Z,R,M_)
% Shows graphs of the unobserved components
% INPUT
%  Date             Date vector [year quarter]             [T x 2]
%  Ym               Data y                                 [T x n]
%  Z                Matrix in obs equaton Y = Z alpha
%  R                Estimates of state vector alpha_t|t    [T x m]
%  R_s              Estimates of state vector alpha_t|T    [T x m]
%  M                Information on model structure
%
% OUTPUT
%  T                Trends                                 [T x n]
%  S                Slopes                                 [T x n]
%  C                Cycles                                 [T x n]
%__________________________________________________________________________
  
  Datp  = Date(:,1) + (Date(:,2)-1) / 4;
  color = {[1 0 0] [0 0.5 0] [0 0 1] [1 0 1]}; 
  j     = 1:5:5*size(Ym,2);
  
  figure('position', [0, 0, 1200, 600])
  
  for i = 1:size(Ym,2)
      
      j1 = M_.Trend;
      j2 = M_.Slope;
      j3 = M_.Cycle;
       
      T(:,i) = R_s.Am(:,j1) * (Z(i,j1))';    
      T(:,i) = R_s.Am(:,j1(i));
      S(:,i) = R_s.Am(:,j2(i));
      C(:,i) = R_s.Am(:,j3) * (Z(i,j3))';
       
    % Data and Trend
      subplot(size(Ym,2),5,j(i))
      plot(Datp,Ym(:,i),'Color','k')
      hold on
      plot(Datp, T(:,i), 'Color', color{i})
      title(Names{i})

    % Data and Trend (diff) 
      subplot(size(Ym,2),5,j(i)+1)
      plot(Datp(2:end),[diff([Ym(:,i) T(:,i)]) S(2:end,i)])
      title('Data, trend and slope (diff)')

    % Cycle
      subplot(size(Ym,2),5,j(i)+2)
      plot(Datp,[C(:,i)],'Color',color{i})
      refline(0,0)
      title('Cycle')
    
    % V
      s = nanstd(R.Vm(:,i));
      subplot(size(Ym,2),5,j(i)+4)
      plot(Datp,R.Vm(:,i)/s)
      title('Prediction errors')    
       
  end

% Plot comparison of cycles  
  for i = 1:size(Ym,2)-1
      subplot(size(Ym,2),5,j(i)+3) 
      plot(Datp,C(:,i),'Color',color{i})
      hold on
      plot(Datp,C(:,i+1),'Color',color{i+1})
      refline(0,0)
      title(['Cycle ' int2str(i) ' and ' int2str(i+1)])
  end
    
  subplot(size(Ym,2),5,size(Ym,2)*5-1) 
  for i =1:size(Ym,2)
      plot(Datp,C(:,i),'Color',color{i})
      hold on
  end
  refline(0,0)
  title('All Cycles')

end

