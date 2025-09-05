function  Print_pars(p_,ps,px,lv,flag)
%__________________________________________________________________________
% This function prints the parameters such that they can be re-inserted 
% into the code 
% flag = 1: long output
%      = 0: short output (omit all parameters with nan and zeros) 
%
%
%__________________________________________________________________________
 n = length(p_);
 disp(' ')
 
 disp(['%' kron('_',ones(1,74))]);
 disp(['%  Likhood  = '  num2str(-lv,'%10.3f')])
 disp(['%' kron('_',ones(1,74))]);
 
 if flag == 0
    disp(['  M_.p0     =  zeros(1,' int2str(n) ');'])
    disp(['  M_.px     =  zeros(1,' int2str(n) ');'])
    disp(['  M_.ps     =   ones(1,' int2str(n) ');'])
 end
 
 
 for i = 1:n
  if i<10       e = ')  = '; 
  else          e =  ') = ';
  end
  if p_(i) >= 0 e1 = [e ' '];
  else          e1 = e; 
  end
  
  s1 = ['  M_.p0(' num2str(i) e1  num2str(p_(i),'%16.12f') '; '];
  s2 = ['  M_.ps(' num2str(i) e   int2str(ps(i)) ';  '];
  s3 = ['  M_.px(' num2str(i) e   num2str(px(i),1) ';  '];
     
  s1  = [s1 empty(34-length(s1))]; 
  s2  = [s2 empty(23-length(s2))]; 
  s3  = [s3 empty(22-length(s3))]; 

  if flag == 1 
         disp([s1 s2 s3])
  else
     if  ~isnan(p_(i)) & p_(i) ~= 0
         disp([s1 s2 s3]) 
     end
  end    

end
end

function      s = empty(n)
   s = kron(' ',ones(1,n));
end