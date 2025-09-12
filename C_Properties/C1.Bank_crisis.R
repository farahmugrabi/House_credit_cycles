## By Farah Mugrabi
#Instructions:
#Follow @ to select options

library(tidyverse)
library(readr)
library(readxl)
library(writexl)
library(data.table)
library(zoo)
library(tseries)
library(mFilter)
library(ggpubr)
library(countrycode)
library(hpfilter)
library(matrixcalc)
library(MASS)
library(mvtnorm)
library(car)
library(ggplot2)
library(tidyr)
library(viridis)
library(ggridges)
library(countrycode)
library(LaplacesDemon)
library(grid)
library(openxlsx)
library(janitor)
library(forecast)

#Directories and colors
rm(list = ls())
path = dirname(rstudioapi::getSourceEditorContext()$path)

base_path <- normalizePath(file.path(path, ".."), winslash = "/")
setwd(base_path)
getwd()
setwd(path)
getwd()
dir.create(file.path(base_path, "D_Results", "Plots", "Bank_crisis"), recursive = TRUE, showWarnings = FALSE)
save_plots <- paste0(base_path, '/D_Results/Plots/Bank_crisis')

cbi_palette = c("#0B5471", "#7C477E", "#0083A0", "#5EC5C2", "#D2E288", "#007DC5", "#D12E7C", "#F57D20", "#FCAF17", "#DFCA94", "#000000", "#7e878e")

## Importing data-----------------------------------------------
#Aggregated assets from credit institutions--------------
k_series <- data.frame(Date = seq.Date(as.Date("2002-10-01"), as.Date("2024-10-01"), by = "quarter"))
k_series$full1<- rep(1, length(k_series[,1]))
k_series$empty1<- c(rep(NA, 45), rep(1,89-45))

assets<-openxlsx::read.xlsx("https://www.centralbank.ie/docs/default-source/statistics/data-and-analysis/credit-and-banking-statistics/bank-balance-sheets/bank-balance-sheets-data/ie_table_a-4_credit_institutions_-_aggregate_balance_sheet.xlsx?sfvrsn=2d49f1d_52", 
                  sheet ="Table A.4 - Assets", startRow = 12, colNames = F) %>% as.data.frame() %>% #in million
  mutate(Year=year(janitor::excel_numeric_to_date(X1))) %>% 
  mutate(Month=month(janitor::excel_numeric_to_date(X2))) %>% 
  mutate(assets= X3-X14-X15-X16-X17-X20)#remove the credit to non-resident 
dates<- paste0(assets$Year, '-', assets$Month, '-01')
assets$Date<- seq.Date(as.Date(first(dates)), length.out = length(dates), by="month")
assets<-assets %>% 
  dplyr::select(Date, assets) %>%
  filter(Date>"2002-12-01")

assets_pre<- openxlsx::read.xlsx(paste0(base_path,"/B_Data/Raw_data/Bank_capital/Assets_pre_2002.xlsx"),   #in million
                       sheet ="STATS_data", startRow = 1, colNames = T) %>% 
  as.data.frame() %>% 
  mutate(Date=janitor::excel_numeric_to_date(Date)) %>% 
  rename(assets= Total.Irish.Resident.Assets) %>% 
  dplyr::select(Date, assets) %>% 
  filter(Date<="2002-12-01") 

assets<- rbind(assets_pre, assets) %>% 
  dplyr::select(Date, assets) %>%
  mutate(Date=as.Date(as.yearqtr(Date))) %>% 
  group_by(Date) %>% 
  mutate(assets=mean(assets)) %>% 
  unique() %>% 
  as.data.frame()  %>% 
  mutate(assets_yoy=(assets/lag(assets,4)-1))

ggplot(assets, aes(x=Date))+geom_line(aes(y=assets))
ggplot(assets, aes(x=Date))+geom_line(aes(y=assets_yoy))

#Macro data------------------------------
macro_data = read_csv(paste0(base_path, "/B_Data/data_full.csv")) %>%
  mutate(Tota_cred_yoy=Tota_cred/lag(Tota_cred,4)-1,
         rpp_price_yoy= rpp_price_index/lag(rpp_price_index,4)-1, 
         GNI_cl_yoy=GNI_cl/lag(GNI_cl,4)-1) %>% 
  dplyr::select(Date,GNI_cl_yoy, Tota_cred_yoy, rpp_price_yoy,
                GNI_cl,rpp_price_index, Tota_cred)

#Merge all---------------
data<- merge.data.frame(k_series,assets, by="Date", all = T )
data<- merge.data.frame(data,macro_data, by="Date", all = T )


#Regression--------------
data<- data %>%
  mutate(GNI_cl_yoy_2= (GNI_cl^2)/(lag(GNI_cl,4)^2)-1) %>% 
  mutate(log_rpp_prices=log(rpp_price_index),
         log_rpp_prices_yoy= log_rpp_prices-lag(log_rpp_prices,4),
         log_rpp_prices_yoy_1= lag(log_rpp_prices_yoy,4)) %>% 
  mutate(assets_yoy_1=lag(assets_yoy,1),
         GNI_cl_yoy_1=lag(GNI_cl,1),
         GNI_cl_yoy_2_1=lag(GNI_cl_yoy_2,1))

data_reg<- data %>% na.omit()

ols<- lm(data = data_reg, formula = assets_yoy ~ assets_yoy_1 +log_rpp_prices_yoy_1)
summary_ols<- summary(ols)
rsq <- summary(ols)$r.squared
adj_rsq <- summary(ols)$adj.r.squared

#Predict with 0.95 confidence intervals
length_data<-length(data$assets_yoy)
predict<-  predict(ols, interval='confidence', data,  level = 0.95)#@Select confidence interval
length_pred<-length(predict[,1])
data$residuals_fit<- data$assets_yoy - c(rep(NA, length_data-length_pred),  predict[,1])
data$residuals_fit_lw<- data$assets_yoy - c(rep(NA, length_data-length_pred),  predict[,2])
data$residuals_fit_up<- data$assets_yoy + c(rep(NA, length_data-length_pred),  predict[,3])

#Extrapolation: (late sample period is realized, early sample period is extrapolated or fitted)
#yoy Growth: assets_extrapol is yoy growth extrapolated with the regression coef
data<- data %>%
  mutate(assets_yoy_1_c=coef(ols)[2]*assets_yoy_1,
         log_rpp_prices_yoy_1_c=coef(ols)[3]*log_rpp_prices_yoy_1,
         assets_extrapol=coef(ols)[1]+assets_yoy_1_c+log_rpp_prices_yoy_1_c) %>%
  dplyr::select(Date, assets, assets_yoy, GNI_cl_yoy_2,log_rpp_prices, assets_yoy_1, GNI_cl_yoy_1, GNI_cl_yoy_2_1,
                assets_yoy_1_c, assets_extrapol, log_rpp_prices_yoy_1, log_rpp_prices_yoy_1_c,log_rpp_prices_yoy,log_rpp_prices_yoy_1,
                residuals_fit_lw, residuals_fit_up,residuals_fit,GNI_cl_yoy)

date_missing<- which(is.na(data$assets_yoy)==FALSE)[1]
for (d in date_missing:2) {
  data$assets_yoy_1_c[d]<- data$assets_extrapol[d+1]*coef(ols)[2]
  data$assets_extrapol[d]<-coef(ols)[1]+data$assets_yoy_1_c[d]+data$log_rpp_prices_yoy_1_c[d]}

#Level: asset_extrapol_level is the extrapolated Level from extrapolated yoy change  - Extrapolated (late sample extrapolated and early sample period is the realized)
data$asset_extrapol_level<- data$assets
date_missing_level<- which(is.na(data$asset_extrapol_level)==FALSE)[1]-1
for (d in date_missing_level:1) {
  data$asset_extrapol_level[d]<- data$assets[d+4]*(1+data$assets_extrapol[d])}
date_missing_level<- which(is.na(data$asset_extrapol_level)==FALSE)[1]-1
for (d in date_missing_level:1) {
  data$asset_extrapol_level[d]<- data$asset_extrapol_level[d+4]*(1+data$assets_extrapol[d])}

data <- data %>%  as.data.frame()
ggplot(data, aes(x=Date))+geom_line(aes(y=asset_extrapol_level))+geom_line(aes(y=assets))
ggplot(data, aes(x=Date))+geom_line(aes(y=assets_extrapol, color="exrtapolation"))+geom_line(aes(y=assets_yoy, color="realized"))+
  scale_color_manual(values = c("exrtapolation" = cbi_palette[c(1)],"realized" = cbi_palette[c(4)]))

#Seasonally adjusted
data = data %>% 
  filter(Date>="1977-01-01") %>% 
  mutate(asset_extrapol_level_sa = ifelse(!is.na(asset_extrapol_level), log10(as.numeric(seasadj(stl(forecast::na.interp(ts(asset_extrapol_level, frequency = 4)), s.window = "periodic", robust = TRUE)))),NA_real_)) %>%
  mutate(asset_extrapol_level_sa_qoq= asset_extrapol_level_sa/lag(asset_extrapol_level_sa,1)-1) %>%
  mutate(asset_extrapol_level_sa_qoq= zoo::rollmean(asset_extrapol_level_sa_qoq, k = 4, fill = NA, align = "right", na.rm=T))

#Risk tolerance set up rule ------------
option<- c("asset_extrapol_level_sa_qoq", "assets_extrapol")
choose<- 1 #@choose 1=qoq, 2=  yoy
th1<- 0.000 #yoy drop beyond this number triggers thr_flag, + hitting this threshold for 4/2 consecutive quarters
th2<- -0.001 #yoy drop beyond this number triggers thr_flag, + hitting this threshold for 4/2 consecutive quarters
data<- data %>% 
  mutate(thr_flag1=if_else(get(option[choose])<= th1, 1, 0),
         thr_flag2=if_else(get(option[choose])<= th2, 1, 0),
         thr_flag3=if_else(get(option[choose])<= th1, 1, 0),
         thr_flag4=if_else(get(option[choose])<= th2, 1, 0),
         
         thr_flag1=replace_na(thr_flag1, 0),
         thr_flag2=replace_na(thr_flag2, 0),
         thr_flag3=replace_na(thr_flag3, 0),
         thr_flag4=replace_na(thr_flag4, 0)) %>% 
  
  mutate(consecutive_th1=thr_flag1*lag(thr_flag1,1),
         consecutive_th2=thr_flag2*lag(thr_flag2,1),
         consecutive_th3=thr_flag3*lag(thr_flag3,1)*lag(thr_flag3,2)*lag(thr_flag3,3),
         consecutive_th4=thr_flag4*lag(thr_flag4,1)*lag(thr_flag4,2)*lag(thr_flag4,3)) %>% 
  
  mutate( consecutive_th1=replace_na(consecutive_th1, 0),
          consecutive_th2=replace_na(consecutive_th2, 0),
          consecutive_th3=replace_na(consecutive_th3, 0),
          consecutive_th4=replace_na(consecutive_th4, 0)) %>% 
  
  mutate(dummy_1=ifelse(consecutive_th1==1, 1, 0),
         dummy_2=ifelse(consecutive_th2==1, 1, 0),
         dummy_3=ifelse(consecutive_th3==1, 1, 0),
         dummy_4=ifelse(consecutive_th4==1, 1, 0))

data<- data %>% 
  mutate(dummy_1_max= dummy_1*max(get(option[choose]), na.rm = T),
         dummy_1_min= dummy_1*min(get(option[choose]), na.rm = T),
         dummy_2_max= dummy_2*max(get(option[choose]), na.rm = T),
         dummy_2_min= dummy_2*min(get(option[choose]), na.rm = T),
         dummy_3_max= dummy_3*max(get(option[choose]), na.rm = T),
         dummy_3_min= dummy_3*min(get(option[choose]), na.rm = T),
         dummy_4_max= dummy_4*max(get(option[choose]), na.rm = T),
         dummy_4_min= dummy_4*min(get(option[choose]), na.rm = T))

reference_th<-c('dummy (0%, 2Q)','dummy (0.1%, 2Q)','dummy (0%, 4Q)', 'dummy (0.1%, 4Q)')#@change name accordingly

#Plots--------------
extrapolation_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=assets_yoy, color='Data'), size = 1.5)+
  geom_line(aes(y=assets_extrapol, color='extrapolation/fitted'), size = 1.5)+
  geom_ribbon(aes(ymin=residuals_fit_lw, ymax=residuals_fit_up, fill='CI_95'), alpha=0.6)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c("Data" = cbi_palette[c(1)],"extrapolation/fitted" = cbi_palette[c(2)]))+
  scale_fill_manual(values = c("CI_95" = 'grey'), guide='none')+
  geom_hline(aes(yintercept=0), color='grey', linetype="dashed")+
  ylab("yoy change")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))
ggsave(paste0(save_plots,"/banking_crisis_extrapolation_yoy.pdf"), extrapolation_plot, height = 20, width = 25)

yoy_growth_plot_gni<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=assets_yoy, color='Assets'), size = 1.5)+
  geom_line(aes(y=GNI_cl_yoy, color='GNI'), size = 1.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = cbi_palette[c(1,4)])+
  ylab("yoy change")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))
ggsave(paste0(save_plots,"/banking_crisis_GNI.pdf"), yoy_growth_plot_gni, height = 20, width = 25)

yoy_growth_plot_rpp<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=assets_yoy, color='Assets'), size = 1.5)+
  geom_line(aes(y=log_rpp_prices_yoy, color='RPP')) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = cbi_palette[c(1,4)])+
  ylab("yoy change")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))
ggsave(paste0(save_plots,"/banking_crisis_RPP.pdf"), yoy_growth_plot_rpp, height = 20, width = 25)

dummy_bcrisis_2q<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=get(option[choose]), color='Assets (qoq change)'), size=1.2)+
  geom_ribbon(aes(ymin=dummy_1_min, ymax=dummy_1_max, fill=reference_th[1]), alpha=0.5)+
  geom_ribbon(aes(ymin=dummy_2_min, ymax=dummy_2_max,  fill=reference_th[2]), alpha=0.5)+
  scale_color_manual(values = c('Assets (qoq change)'= cbi_palette[1], 'dummy (0%, 2Q)'=cbi_palette[10], 'dummy (0.1%, 2Q)'=cbi_palette[9]))+
  scale_fill_manual(values = c('dummy (0%, 2Q)'=cbi_palette[10], 'dummy (0.1%, 2Q)'=cbi_palette[9]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  geom_hline(aes(yintercept=0), color='grey', linetype="dashed")+
  ylab("qoq change")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))
ggsave(paste0(save_plots,"/banking_crisis_2Q.pdf"), dummy_bcrisis_2q, height = 20, width = 25)

dummy_bcrisis_4q<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=get(option[choose]), color='Assets (qoq change)'), size=1.2)+
  geom_ribbon(aes(ymin=dummy_3_min, ymax=dummy_3_max,  fill=reference_th[3]), alpha=0.5)+
  geom_ribbon(aes(ymin=dummy_4_min, ymax=dummy_4_max, fill=reference_th[4]), alpha=0.5)+
  scale_color_manual(values = c('Assets (qoq change)'= cbi_palette[1],
                                'dummy (0%, 4Q)'=cbi_palette[10], 'dummy (0.1%, 4Q)'=cbi_palette[9]))+
  scale_fill_manual(values =c('dummy (0%, 4Q)'=cbi_palette[10], 'dummy (0.1%, 4Q)'=cbi_palette[9]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  geom_hline(aes(yintercept=0), color='grey', linetype="dashed")+
  ylab("qoq change")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))
ggsave(paste0(save_plots,"/banking_crisis_4Q.pdf"), dummy_bcrisis_4q, height = 20, width = 25)

#Export crisis dummy-----------------
data_crisis<- data %>% 
  dplyr::select(Date, dummy_1,dummy_2,dummy_3,dummy_4)
nameworkbook<- paste0(path,"/1.Crisis_events/Bank_crisis_dummy.xlsx")
wb <- createWorkbook(nameworkbook)
addWorksheet(wb,'Bank_crisis_dummy')
writeData(wb, sheet = "Bank_crisis_dummy", data.frame(data_crisis))
saveWorkbook(wb,paste0(path,"/1.Crisis_events/Bank_crisis_dummy.xlsx"), overwrite = TRUE)

max_run_ones <- function(x) {
  r <- rle(as.integer(x == 1))
  if (any(r$values == 1)) max(r$lengths[r$values == 1]) else 0}
cols <- paste0("dummy_", 1:4)
max_runs <- sapply(data_crisis[ , cols, drop = FALSE], max_run_ones)
below2<- max_runs[c(1,2)] %>% max()
below4<- max_runs[c(3,4)] %>% max()

#Export Assets extrapolation-----------------
data_assets<- data %>% 
  dplyr::select(Date, asset_extrapol_level, assets_extrapol) #assets_extrapol= YOY

nameworkbook<- paste0(path,"/1.Crisis_events/Bank_assets_extrapolated.xlsx")
wb <- createWorkbook(nameworkbook)
addWorksheet(wb,'Bank_assets_extrapolated')
writeData(wb, sheet = "Bank_assets_extrapolated", data.frame(data_assets))
saveWorkbook(wb, paste0(path,"/1.Crisis_events/Bank_assets_extrapolated.xlsx"), overwrite = TRUE)

#Key takeaway for the paper:
print("🚀 Check also the fit:")
summary_ols
cat("📊 Model performance for extrapolation:\n",
    "✅ R-squared:", sprintf("%.3f", rsq), "\n",
    "✅ Adjusted R-squared:", sprintf("%.3f", adj_rsq), "\n")

cat(sprintf(
  "📄 Applying this rule with a 0.1%% threshold, we identify %d quarters in which the four-quarter moving average of qoq asset growth remained below the threshold for at least four consecutive quarters, compared with %d quarters when considering declines lasting at least two consecutive quarters.\n",
  below4, below2
))
dummy_bcrisis_4q