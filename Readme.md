# 📊 Reproducible Code for the Article  
**Housing and Credit Cycles in Ireland**  
Published in: *Research Technical Papers, Central Bank of Ireland*, Volume [x], No. [y], Year [yyyy]  

# 💻 📑 Code and Article Authors  
This repository and all accompanying code were produced by:
- **Dr. Farah Mugrabi** (Université catholique de Louvain)
- **Dr. Gerhard Rünstler** (European Central Bank)  

# 📬 Correspondence  
- 📧 [farah.mugrabi@uclouvain.be](mailto:farah.mugrabi@uclouvain.be)  
- 🌐 [www.farahmugrabi.com](http://www.farahmugrabi.com)  

# 🧠 About  
- This repository contains **fully reproducible codes** for the article above.  
- The code is heavily based on the routines developed by **Gerhard Rünstler** and published in: Rünstler, G. and Vlekke, M. (2018). *Business, housing, and credit cycles*. *Journal of Applied Econometrics*, 33(2), 212–226.  
- Additional contributions include: **Dmitry Kulikov**, who implemented the Dynare routines for Bayesian estimation based on the original code.  
- We use the routines provided by **Dynare**, a MATLAB-based platform for economic modeling, which implement the Gibbs sampler due to Carter–Kohn for the state simulation step.  

# ✏️ Citation  
If you use this code, please cite both the GitHub repository and the related article:  
- **GitHub repository** 
  -Mugrabi, F. & Rünstler, G. (202_). *Housing and Credit Cycles in Ireland*. GitHub. https://github.com/farahmugrabi/Housing_credit_cycles  
- **Articles** 
  - Rünstler, G. and Vlekke, M. (2018). *Business, housing, and credit cycles*. *Journal of Applied Econometrics*, 33(2), 212–226. https://www.jstor.org/stable/26609842
  - Mugrabi, F. & Rünstler, G. (202_). *Housing and Credit Cycles in Ireland*. *Research Technical Papers, Central Bank of Ireland*, [Volume __, No. __]. https://doi.org/[pending]  

# 📦 Requirements 
- For the estimation of cycles using the **benchmark model** (Rünstler & Vlekke, 2018), **MATLAB** is required.  
- For **data generation, alternative models, early-warning analysis, and real-time estimates**, **R** is required.  
- Code tested under **R version 4.5.1 (2025-06-13 ucrt)**.  
- Code tested under  **MATLAB R2024b**.  
- MATLAB tools: **Optimization Toolbox**
- Please install the required R packages: install.packages(c("dplyr","lubridate","DisaggregateTS","zoo","csodata","ggplot2","ecb","openxlsx","stringr","RJDemetra","vars","urca","mnormt","tseriesChaos","tsDyn","bvartools","tidyverse","readr","readxl","writexl","data.table","xlsx","tseries","mFilter","ggpubr","countrycode","hpfilter","pROC","qpcR","janitor","fbroc","ipred","lava","recipes","caret","gridExtra","mvtnorm","MASS","car","matrixcalc","viridis","ggridges","LaplacesDemon","tidyr","forecast"))

# 📂 Download & Structure  
Please download the following folders and scripts. The files and directories are the following:
- 📂 **A.Main_Code** (based on Rünstler, G. and Vlekke, M., 2018)
  - 📑 GF3_US_FIX_main.m (univariate master code)
  - 📑 GF3_S23_US_FIX_DYN_pars.m (multivariate master code)
  - 📑 Master_Recursive.m (pseudo real-time estimation master code)
  - 📑 data_limit.mat (auxiliary structure)
  - 📂 Outcome (save results, empty)
  - 📂 Functions (utils, 43 auxiliary functions)
- 📂 **B.Data** (data generation)
  - 📑 0.Data.R
  - 📂 Raw_data (data)
    - 📈 data_pre.xlsx (data) 
    - 📂 Ban_capital: Assets_pre_2002.xlsx,
    - 📂 Crisis_indicators: Babecky_et_al._(2012).xlsx, fred_exchange_rate_IE.xlsx, Laeven_and_Valencia_(2020).xlsx, Baron_Dieckelmann_Panics_and_bank_failures_database.xlsx,  
    - 📂 Loan_Rates: b.1.2.xlsx
    - 📂 OBrien_Velasco: OBrienvelasco_results.xlsx 
- 📂 **C.Properties** (Early warning and Real Time)
  - 📂 1.Crisis_events
    - 📈 0.Crisis_indicator.xlsx (dummy crisis based on multiple sources, need to update manually) 
  - 📑 C1.Bank_crisis.R (generation bank in distress dummies)
  - 📑 C2.Properties.R

# ⚙️ Instructions:
1. Download the full folder, do not change the location of the files or names.
2. C.Properties/Crisis_events/0.Crisis_indicator.xlsx needs to be updated manually, set dummy = 1 if any current systemic crisis occurs.
3. Follow @ to locate configurable options.
4. Run ▶️ the scripts in the following order:
  - B.Data/0.Data.R
  - A.Main_Code/GF3_S23_US_FIX_DYN_pars.m
  - C.Properties/C1.Bank_crisis.R
  - C.Properties/C2.Properties.R
5. Notes 📝:
- 0.Data script updates data from different open source APIs. Data prior to the coverage of these APIs are provided by the Central Bank of Ireland and stored in the B.Data folder.
- Benchmark results are based on GF3_S23_US_FIX_DYN_pars.m script, results are store in A.Main_Code/Outcome/Results_main.xlsx. The order of the variables is the following: GNI, National Credit, House prices. First three columns data, second three columns Trend, last three columns cycles. Last data points correspond to the same date of the last data point of the file data_model.xlsx in A.Main_Code
- One side results are based on GF3_S23_US_FIX_DYN_pars.m script, results are store in A.Main_Code/Outcome/Results_oneside20.xlsx, this is used for real time and early warning properties.
- There, Pseudo Real Time estimates section is commented. It is set to generate estimates recursively from quarter x to y, this can be set in Master_Recursive.m, results are store in A.Main_Code/Outcome/Results_n__.xlsx.
- O’Brien and Velasco estimates are obtained from the CBI, with the last available observation in 2024Q4. If no new data are available, request the series or comment out the relevant lines in C.2_Properties.R. As the early warning assessment relies on one-sided filters, the model remains usable until these estimates reach 2024Q4; beyond that, it requires updated data or exclusion.
- For the pseudo real time estimates charts, include this extra step: 
   - After running: A.Main_Code/GF3_S23_US_FIX_DYN_pars.m, run A.Main_Code/Master_Recursive.m
   - Uncomment C.Properties/C2.Properties.R, section #Plot: Pseudo real time (nd: new data points) and section #3-Pseudo Real time estimates.
- The published working paper used data until Q2 2025, however national credit was available only until Q1 2025. Therefore, in order to reproduce the same results, those datapoints should be considered. The file Data.R is set tu automatically update all data. 
