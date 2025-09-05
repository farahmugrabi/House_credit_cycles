function C = cor_mat(V)
%__________________________________________________________________________
% C = cor_mat(V)
% Calculates correlation matric C from covariance matrix V
%__________________________________________________________________________

 s = sqrt(diag(V));
 S = s * s';
 C = V ./ S;

end

