function [S, G_,Co_,Ph_,omega] = SGF_AR1(Z,T,H,n_grid,fpath,ctry,M_)
%__________________________________________________________________________
% [G_ Co_ Ph_ omega] = GEC_SGF(Z,T,H,Scale,s_max,n_grid)
%  Calculates the spectral generating function (SGF) of stoch process x(t)   
%                   x(t)  = Z p(t)
%                   p(t)  = T p(t-1) + H e(t)         e(t) ~N(0,eye(n))  
%  This is used for the non-similar cycles model
%  The SGF G is calulcated for a grid over (0,pi/4) with n_grid points 
%
%  INPUTS
%   Z             (n x k)
%   T             (k x k)
%   H             (k x k)
%
%   n_grid         integer             Nr of points in grid over (-pi,pi)
%
% OUTPUTS
%    S                                 Summary statistics cycle average
%    G_           (n_grid x n x n)     Auto and cross spectra
%    C0_          (n_grid x n x n)     Coherence spectra (sqrt)
%    Ph_          (n_grid x n x n)     Phase spectra
%    omega        (n_grid x 1)         The grid
%
% 
%   see Hamilton, 1994, Time series analysis, p. 267ff
%   From 
%          p = (I-Tz)^(-1) H e 
%   the ACF for the process can be written as
%          G(z) = (I - Az)^(-1) HH' (I - A'z^-1)^(-1)
%   The SGF is given by G(exp(iw)) and z^-1 = exp(-iw)
%__________________________________________________________________________
 
% Definitions grid and SGF  
  [n,k]   = size(Z);
  omega   = linspace(pi/(2*n_grid),pi/2,n_grid);
  G_      = nan(n_grid,n,n);
  Co_     = nan(n_grid,n,n);
  Ph_     = nan(n_grid,n,n);
  PhQ     = nan(n_grid,n,n);
  
%__________________________________________________________________________
% Get SGF (see Hamilton, p. 267)
%__________________________________________________________________________
  for w = 1:n_grid
      im        =  sqrt(-1);
      F1        =  eye(k) - T  * exp(-im*omega(w));
      F2        =  eye(k) - T' * exp( im*omega(w)); 
      G         =  inv(F1) * (H*H') * inv(F2);
      
      G_(w,:,:) =  1/(2*pi) * Z * G * Z';      
  end

% Check whether diagonal of G is real-valued
  for w = 1:n_grid
      g = diag(squeeze(G_(w,:,:)));
      if  max(imag(g) ./ real(g)) > 1e-5
          warning('Non-real elements on auto spectra')
          disp([int2str(w) ': '  num2str(diag(g))])
      end
  end
  
% Standardise G_ 
  V = Z * InitCov(T,H*H') * Z';
  v = sqrt(diag(V));
  for  w = 1:n_grid
       G_(w,:,:)   = squeeze(G_(w,:,:)) ./ (v*v');
  end

% Calculate coherence (square root) and phase
  for  w = 1:n_grid
       G           =  squeeze(G_(w,:,:));
       g           =  sqrt(real(diag(G)));
       Co_(w,:,:)  =  abs(G) ./  (g*g');
       Ph_(w,:,:)  =  atan(imag(G) ./ real(G));
       PhQ(w,:,:)  =  Ph_(w,:,:)   ./ omega(w);
  end
    
%__________________________________________________________________________
% Statistics on coherence and phase  
% At peak of cycle
%__________________________________________________________________________
  for i = 1:n
      [x,m]       = max(squeeze(G_(:,i,i)));
       maxl       = omega(m);
       S.L_peak(i) = 2*pi/maxl;
      
       for j = 1:n
           S.C_peak(i,j) = Co_(m,i,j);
           S.P_peak(i,j) = Ph_(m,i,j);
           S.Q_peak(i,j) = Ph_(m,i,j)/maxl;
      end
  end

  
%__________________________________________________________________________
% Statistics on coherence and phase  
% Integrated over auto-spectra, i.e. average weighted by G_
% l = sum(om      .* G_(om) / sum(G_(om)))
% c = sum(Co_(om) .* G_(om) / sum(G_(om)))
%
% Short and long average over sub-samples
% 8  - 32 quarters
% 32 - 80 quarters
%__________________________________________________________________________

% Average cycle length
  for i = 1:n
      g_          = real(squeeze(G_(:,i,i)))';
      S.L_avg(i)  = sum(g_ .* omega) / sum(g_);
      S.L_avg(i)  = 2*pi / S.L_avg(i);
  end
   
% Average coherence and phase  
  for i = 1:n
  for j = 1:n
      g_     = sqrt(squeeze(G_(:,i,i)  .* G_(:,j,j)));
      g_     = real(g_);

      S.C_avg(i,j) = sum(g_ .* squeeze(Co_(:,i,j))) / sum(g_);
      S.P_avg(i,j) = sum(g_ .* squeeze(Ph_(:,i,j))) / sum(g_);
      S.Q_avg(i,j) = sum(g_ .* squeeze(PhQ(:,i,j))) / sum(g_);
  end
  end

% Short and long frequencies  
  ixs = find((omega <  (2*pi/8))  & (omega > (2*pi/24)));
  ixl = find((omega <  (2*pi/24)) & (omega > (2*pi/120)));
  
  for i = 1:n
      g_          = real(squeeze(G_(:,i,i)))';
      S.V_sht(i)  = sum(g_(ixs)) / sum(g_([ixs ixl]));
      S.V_lng(i)  = sum(g_(ixl)) / sum(g_([ixs ixl]));
  end  
  
  for i = 1:n
  for j = 1:n
      g_     = sqrt(squeeze(G_(:,i,i)  .* G_(:,j,j)));
      g_     = real(g_);
      
      S.C_srt(i,j) = sum(g_(ixs) .* squeeze(Co_(ixs,i,j))) / sum(g_(ixs));
      S.P_srt(i,j) = sum(g_(ixs) .* squeeze(Ph_(ixs,i,j))) / sum(g_(ixs));
      S.Q_srt(i,j) = sum(g_(ixs) .* squeeze(PhQ(ixs,i,j))) / sum(g_(ixs));
     
      S.C_lng(i,j) = sum(g_(ixl) .* squeeze(Co_(ixl,i,j))) / sum(g_(ixl));
      S.P_lng(i,j) = sum(g_(ixl) .* squeeze(Ph_(ixl,i,j))) / sum(g_(ixl));
      S.Q_lng(i,j) = sum(g_(ixl) .* squeeze(PhQ(ixl,i,j))) / sum(g_(ixl));
  end
  end
  
  disp(' '); 
  disp(['' kron('_',ones(1,75))])
  disp('Spectral statistics (integrated)')
  disp(['' kron('_',ones(1,75))])
  disp(' '); 

  disp('Cycle length (quarters)')
  disp(num2str(S.L_avg,'%10.4f'))
  disp(' '); 
  disp('Cycle length (years)')
  disp(num2str(S.L_avg./4,'%10.3f'))    
  disp(' '); 
  
  disp('Variance share long')
  disp(num2str(S.V_lng,'%10.3f'))
  disp(' '); 
  disp('Variance share short')
  disp(num2str(S.V_sht,'%10.3f'))    
  
  if n > 1
    disp(' ')
    disp('Coherence');      disp(num2str(S.C_avg,'%10.3f'))
    disp(' '); 
    disp('Phase');          disp(num2str(S.P_avg','%10.3f'))
    disp(' '); 
    disp('Phase (years)');  disp(num2str((S.Q_avg')./4,'%10.3f'))
    disp(' ')
    disp('Coh (long)') ;    disp(num2str(S.C_lng,'%10.3f'))
    disp(' ')
    disp('Coh (short)');    disp(num2str(S.C_srt,'%10.3f'))
  end 

  S.L_peak = S.L_peak';  S.C_peak = S.C_peak';
  S.P_peak = S.P_peak';  S.Q_peak = S.Q_peak';
  
  S.L_avg = S.L_avg';    S.C_avg = S.C_avg';
  S.P_avg = S.P_avg';    S.Q_avg = S.Q_avg'; 

%__________________________________________________________________________
% Graphs
%__________________________________________________________________________
  var{1} = '$Y_{t}$';
  var{2} = '$C_{t}$';
  var{3} = '$P_{t}$';
  set(0,'defaulttextinterpreter','latex');  
  
  figure('position', [0, 0, 800, 800])
  for i = 1:n
  for j = 1:n
      subplot(n,n,3*(i-1)+j)
      if  i == j   
          plot(omega,squeeze(real(G_(:,i,j))), 'Color', 'k')
          xlim([0 0.9]);
          ylim([0 10]);
          title(['Auto spectrum ',var{i}],'interpreter', 'latex')
      elseif i >  j   
          plot(omega,squeeze(Co_(:,i,j)), 'Color', 'k')
          xlim([0 0.9]);
          ylim([0 1]);
          title(['Coherence (',var{i},',',var{j},')'],'interpreter','latex')
      else
          plot(omega,squeeze(Ph_(:,i,j)), 'Color', 'k')
          xlim([0 0.9]);
          title(['Phase (',var{i},',',var{j},')'],'interpreter','latex')
      end
  end
  end
  
 if ~isempty(fpath)
     NameFigure = [ctry,'_annex_spec'];
     saveTightFigure(gcf,NameFigure,fpath);  
 end
  
 
   
  
  