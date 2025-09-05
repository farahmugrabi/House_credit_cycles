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
**GitHub repository**  
> Mugrabi, F. & Rünstler, G. (202_). *Housing and Credit Cycles in Ireland*. GitHub. https://github.com/farahmugrabi/Housing_credit_cycles  
**Articles** 
> Rünstler, G. and Vlekke, M. (2018). *Business, housing, and credit cycles*. *Journal of Applied Econometrics*, 33(2), 212–226. https://www.jstor.org/stable/26609842
> Mugrabi, F. & Rünstler, G. (202_). *Housing and Credit Cycles in Ireland*. *Research Technical Papers, Central Bank of Ireland*, [Volume __, No. __]. https://doi.org/[pending]  

# 📦 Requirements 
- For the estimation of cycles using the **benchmark model** (Rünstler & Vlekke, 2018), **MATLAB** is required.  
- For **data generation, alternative models, early-warning analysis, and real-time estimates**, **R** is required.  
- Code tested under **R version 4.5.1 (2025-06-13 ucrt)**.  
- Code tested under  **MATLAB R2024b**.  
- MATLAB tools: **Optimization Toolbox**

# 📂 Download & Structure  
Please download the following folders and scripts, and place them all in the same location:
- 📂 **A.Main_Code** (based on Rünstler, G. and Vlekke, M., 2018)
  - 📑 GF3_US_FIX_main.m (univariate master code)
  - 📑 GF3_S23_US_FIX_DYN_pars.m (multivariate master code)
  - 📑 Master_Recursive.m (pseudo real-time estimation master code)
  - 📂 Outcome (save results)
  - 📈 data_model.xlsx (data) 
  - 📂 Functions (util master codes)
- 📂 **B.Data** (data generation)
  - 📂 Raw_data
  - 📑 0.Data.R

# 📦 Requirements  
- Please install the required R packages:  
install.packages(c("dplyr","lubridate","DisaggregateTS","zoo","csodata","ggplot2","ecb","openxlsx","stringr","RJDemetra"))

# ⚙️ Instructions:
1. Download the full folder, do not change the location of the files or names.
2. Follow @ to locate configurable options.
4. Run ▶️ the master script from beginning to end.
5. Notes 📝:
- Data script updates data from different open source APIs.Data prior to the coverage of these APIs are provided by the Central Bank of Ireland and stored in the Data_pre folder.
- Benchmark results are based on GF3_S23_US_FIX_DYN_pars.m script, results are store in A.Main_Code/Outcome/Results_main.xlsx.
- One side results are based on GF3_S23_US_FIX_DYN_pars.m script, results are store in A.Main_Code/Outcome/Results_oneside.xlsx, this is used for real time and early warning properties.
- There, Pseudo Real Time estimates section is commented. It is set to generate estimates recursively from quarter 100 to 197, this can be change in Master_Recursive.m, results are store in A.Main_Code/Outcome/Results_n__.xlsx.
