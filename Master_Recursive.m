function Master_Recursive()
global data_limit name_results;
for i=100:198
    data_limit =i;
    name_results=sprintf('n%d', i);
    save('data_limit', 'name_results');
    
    run('GF3_S23_US_FIX_DYN_pars.m');
end
end
