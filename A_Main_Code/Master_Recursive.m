%Instructions: expand index 198 to the corresponding index. Where 198 is relative to quarter 2025 Q1.
%For example when data is available for 2025 Q2, replace 198 by 199.
%Go to GF3_S23_US_FIX_DYN_pars.m uncomment the section Data - Pseudo Real Time 
%Go to R script C.Properties/C.2.Properties.R, and replace 198 by 199 too
%The section #Plot: Pseudo real time (nd: new data points) is commented, so
%uncomment it in case the plots are required
function Master_Recursive()
global data_limit name_results;
for i=127:198
    data_limit =i;
    name_results=sprintf('n%d', i);
    save('data_limit', 'name_results');
    
    run('GF3_S23_US_FIX_DYN_pars.m');
end
end
