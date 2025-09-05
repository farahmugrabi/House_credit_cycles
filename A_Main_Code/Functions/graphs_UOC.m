function graphs_UOC(Date, Ym, Trend, Cycle, Irregular, Label)

%
% Shows graphs of the components of the UOC model
%

Datp  = Date(:,1) + (Date(:,2)-1) / 4;
figure('Position', [0, 0, 600, 600], 'Name', Label);
       
% Data and Trend
       subplot(2,2,1)
       plot(Datp, [Ym(:) Trend(:)])
       title('Data and trend')
       
       subplot(2,2,2)
       plot(Datp(2:end), diff([Ym(:) Trend(:)]));
       title('Data and Trend (1st diff)')
% Cycle
       subplot(2,2,3)
       plot(Datp, Cycle(:), 'r')
       refline(0,0);
       title('Cycle')
% Irregular component
       s = std(Irregular(:));
       subplot(2,2,4)
       plot(Datp,Irregular(:)/s)
       title('Irregular component')    

end

