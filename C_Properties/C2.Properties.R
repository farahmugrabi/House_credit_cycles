#Instructions
#Follow @ to find the options
#Run the code twice: one with oneside_estimate= T and the other time with oneside_estimate=F
#Select oneside_estimate= True for the early warning and real time plots and False for the cycle plots. We base the early warning properties assessment on one sided filters
#Update C_Properties\1.Crisis_events\0.Crisis_indicator.xlsx. if any new systemic crisis (last obs 2024q4)

#Libraries
library(vars)
library(urca)
library(mnormt)
library(tseriesChaos)
library(tsDyn)
library(bvartools)
library(tidyverse)
library(readr)
library(readxl)
library(writexl)
library(data.table)
library(zoo)
library(xlsx)
library(tseries)
library(mFilter)
library(ggpubr)
library(countrycode)
library(hpfilter)
library(openxlsx)
library(pROC)
library(qpcR)
library(janitor)
library(fbroc)
library(ipred)
library(lava)
library(recipes)#https://www.digitalocean.com/community/tutorials/confusion-matrix-in-r
library(caret)
library(gridExtra)
library(urca)
library(tsDyn)
library(vars)
library(csodata)

#Directories
rm(list = ls())
cat("\014")
path<- dirname(rstudioapi::getSourceEditorContext()$path)

base_path <- normalizePath(file.path(path, ".."), winslash = "/")
setwd(base_path)
getwd()
dir.create(file.path(base_path, "D_Results", "Alternative_estimates"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(base_path, "D_Results", "Plots", "Properties"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(base_path, "D_Results", "Plots","Properties", "Real_Time"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(base_path, "D_Results", "Plots","Properties", "VECM"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(base_path, "D_Results", "Plots","Properties", "Early_warning"), recursive = TRUE, showWarnings = FALSE)
setwd(path)
getwd()

path_plots<-paste0(base_path, "/D_Results/Plots/Properties")

cbi_palette = c("#0B5471", "#7C477E", "#0083A0", "#5EC5C2", "#D2E288", "#007DC5", "#D12E7C", "#F57D20", "#FCAF17", "#DFCA94", "#000000", "#7e878e")

#A: Estimate alternative models-------------------------------------------------
#A.1-Univariate filters---------------------------------------------------------
#Data----------------
setwd(paste0(path, "/../"))
getwd()
bring_data <- paste0(getwd(), '/A_Main_Code/')
setwd(path)
data<-read.csv(paste0(bring_data, "data_model.csv"), header = F) %>% 
  mutate(V1=as.Date(as.yearqtr(V1)))%>% 
  rename(Date=V1,GNI=V2, NC=V3, RPP=V4)
data<-data %>% na.omit(.)

#Hodrick Prescott----------------------------
#One sided filter
l_hp <- length(as.data.frame(data)[,1])
hp_function<- function(s){
hp_cycle <- rep(0,l_hp)
hp_trend <- rep(0,l_hp)
hpfilter_40 <- hpfilter(as.data.frame(data)[1:(40),var[s]],type ='lambda' ,freq = 400000) #Burn in period
hp_cycle[1:40] <- hpfilter_40$cycle
hp_trend[1:40] <- hpfilter_40$trend
  for(i in 1:(l_hp-40)){
    hpfilter_40 <- hpfilter(as.data.frame(data)[1:(40+i),var[s]] ,type ='lambda', freq = 400000) #Expanded window
    hp_cycle[40+i] <- hpfilter_40$cycle[i+40]
    hp_trend[40+i] <- hpfilter_40$trend[i+40]
  }
var_decomp<-as.data.frame(cbind(data[,"Date"], data[, var[s]], hp_trend, hp_cycle)) %>% 
  mutate(V1=as.Date(V1))
colnames(var_decomp)<- c('Date',var[s], paste0("hp_trend_", var[s]), paste0("hp_cycle_", var[s]))
return(var_decomp)
}
var=c("GNI","NC", "RPP")
varpass<- list(1, 2, 3)
HP_Results_list_oneside<- lapply(varpass, hp_function)
HP_Results_oneside<- left_join(HP_Results_list_oneside[[1]], HP_Results_list_oneside[[2]])
HP_Results_oneside<- left_join(HP_Results_oneside, HP_Results_list_oneside[[3]])

#Two side filter
hp_function_ts <- function(s){
  hp <- mFilter::hpfilter(as.data.frame(data)[, var[s]], type = "lambda", freq = 400000)
  out <- cbind(data[, c("Date", var[s])], hp$trend, hp$cycle)
  colnames(out) <- c("Date", var[s], paste0("hp_trend_", var[s]), paste0("hp_cycle_", var[s]))
  out
}
HP_TS_Results_list <- lapply(varpass, hp_function_ts)
HP_TS_Results <- left_join(HP_TS_Results_list[[1]], HP_TS_Results_list[[2]])
HP_TS_Results <- left_join(HP_TS_Results, HP_TS_Results_list[[3]])

nameworkbook<- paste0(base_path,"/D_Results/Alternative_estimates/hodrickprescott_results.xlsx")
wb <- openxlsx::createWorkbook(nameworkbook)
openxlsx::addWorksheet(wb,'HP')
openxlsx::writeData(wb, sheet = 'HP', data.frame(HP_TS_Results))
openxlsx::saveWorkbook(wb, paste0(base_path,"/D_Results/Alternative_estimates/hodrickprescott_results.xlsx"), overwrite = TRUE)

# Real time properties: NC
HP_credit <- merge.data.frame(HP_TS_Results[, c("Date", "hp_cycle_NC")],
                              HP_Results_list_oneside[[2]][, c("Date", "hp_cycle_NC")],
                              by = "Date")
colnames(HP_credit) <- c("Date", "HP_NC_twosided", "HP_NC_onesided")
HP_credit$Diff <- HP_credit$HP_NC_twosided - HP_credit$HP_NC_onesided

HP_DIFF <- HP_credit[41:nrow(HP_credit), ] %>%
  mutate(Date = as.yearqtr(Date)) %>%
  pivot_longer(cols = c("HP_NC_twosided", "HP_NC_onesided"), values_to = "value", names_to = "variable") %>%
  ggplot() +
  geom_line(aes(x = Date, y = value, colour = variable)) +
  labs(x = "Quarter", y = "", title = "") +
  theme_classic() +
  scale_y_continuous("National Credit cycle") +
  scale_color_manual(values = cbi_palette[c(1, 8, 10)], name = "") +
  guides(fill = guide_legend(title = "")) +
  geom_hline(yintercept = 0, colour = cbi_palette[12], linetype = "dotted") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 40),
        axis.text = element_text(size = 40),
        axis.title = element_text(size = 40),
        legend.text = element_text(size = 40))

sd(HP_credit$Diff, na.rm = TRUE)
sd(HP_credit$Diff, na.rm = TRUE) / sd(HP_credit$HP_NC_onesided, na.rm = TRUE)
ggsave(paste0(path_plots, "/Real_time/HP_NC_onevstwo_sides.pdf"), HP_DIFF, height = 20, width = 25)

# Real time properties: RPP
HP_RPP <- merge.data.frame(HP_TS_Results[, c("Date", "hp_cycle_RPP")],
                           HP_Results_list_oneside[[3]][, c("Date", "hp_cycle_RPP")],
                           by = "Date")
colnames(HP_RPP) <- c("Date", "HP_RPP_twosided", "HP_RPP_onesided")
HP_RPP$Diff <- HP_RPP$HP_RPP_twosided - HP_RPP$HP_RPP_onesided

HP_DIFF_RPP <- HP_RPP[41:nrow(HP_RPP), ] %>%
  mutate(Date = as.yearqtr(Date)) %>%
  pivot_longer(cols = c("HP_RPP_twosided", "HP_RPP_onesided"), values_to = "value", names_to = "variable") %>%
  ggplot() +
  geom_line(aes(x = Date, y = value, colour = variable)) +
  labs(x = "Quarter", y = "", title = "") +
  theme_classic() +
  scale_y_continuous("House Prices cycle") +
  scale_color_manual(values = cbi_palette[c(1, 8, 10)], name = "") +
  guides(fill = guide_legend(title = "")) +
  geom_hline(yintercept = 0, colour = cbi_palette[12], linetype = "dotted") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 40),
        axis.text = element_text(size = 40),
        axis.title = element_text(size = 40),
        legend.text = element_text(size = 40))

sd(HP_RPP$Diff, na.rm = TRUE)
sd(HP_RPP$Diff, na.rm = TRUE) / sd(HP_RPP$HP_RPP_onesided, na.rm = TRUE)
ggsave(paste0(path_plots, "/Real_time/HP_RPP_onevstwo_sides.pdf"), HP_DIFF_RPP, height = 20, width = 25)

cor(HP_RPP$HP_RPP_twosided, HP_RPP$HP_RPP_onesided)
cor(HP_credit$HP_NC_twosided, HP_credit$HP_NC_onesided)

max(HP_RPP$Diff, na.rm = TRUE)
max(HP_credit$Diff, na.rm = TRUE)
sqrt(sum(HP_RPP$Diff^2, na.rm = TRUE))
sqrt(sum(HP_credit$Diff^2, na.rm = TRUE))

#Christiano-Fitzgerald----------------------------
minperiod<-c(8,32,32)
maxperiod<-c(32, 120, 120)

#One sided filter
l_cf <- length(as.data.frame(data)[,1])
cf_cycle <- rep(0,l_cf)
cf_trend <- rep(0,l_cf)
cf_data<- rep(0,l_cf)
CF_function<- function(s){
cf_filter_40<- cffilter(data[1:(40),var[s]],pl=minperiod[s],pu=maxperiod[s],root=TRUE,drift=TRUE, 
                        type=c("asymmetric"), #other options: "symmetric","fixed","baxter-king","trigonometric" #see page 9 ECB Occasional paper real anf financial cycles in EU countries: stylised facts and modelling implications, #see also specification in function CF_FILTER here \\Filescluster\shared\MPS\WORD\MPFS DEPT\Farah\Alternative_Gap\New_Model\Ruenstler_and_Vlekke_2018\D_Translating_to_R\Runstler_Vlekke_2018\Models
                        nfix=-1,
                        theta=1)
cf_data[1:40]<- data[1:(40),c(var[s])]
cf_cycle[1:40]<- data[1:(40),c(var[s])]-cf_filter_40$cycle[1:(40)]
cf_trend[1:40]<- cf_filter_40$trend[1:(40),]
for(i in 1:(l_cf-40)){
  cf_filter_40<- cffilter(data[1:(40+i),var[s]],pl=minperiod[s],pu=maxperiod[s],root=TRUE,drift=TRUE, 
                       type=c("asymmetric"), #other options: "symmetric","fixed","baxter-king","trigonometric" #see page 9 ECB Occasional paper real anf financial cycles in EU countries: stylised facts and modelling implications, #see also specification in function CF_FILTER here \\Filescluster\shared\MPS\WORD\MPFS DEPT\Farah\Alternative_Gap\New_Model\Ruenstler_and_Vlekke_2018\D_Translating_to_R\Runstler_Vlekke_2018\Models
                       nfix=-1,
                       theta=1)
  cf_cycle[40+i]<- cf_filter_40$cycle[(40+i)]
  cf_trend[40+i]<- cf_filter_40$trend[(40+i)]
}
cf_results<- cbind(cf_trend,cf_cycle)
colnames(cf_results)<- c( paste0("cf_trend_", var[s]), paste0("cf_cycle_", var[s]))
cf_results<- cbind(data[,c("Date",var[s])],cf_results)
return(cf_results)
}
var=c("GNI","NC", "RPP")
varpass<- list(1, 2, 3)
CF_Results_list_oneside<- lapply(varpass, CF_function)
CF_Results_oneside<- left_join(CF_Results_list_oneside[[1]], CF_Results_list_oneside[[2]])
CF_Results_oneside<- left_join(CF_Results_oneside, CF_Results_list_oneside[[3]])

#Two sided filter
cf_function<- function(s){
  cf_filter<- cffilter(data[,var[s]],pl=minperiod[s],pu=maxperiod[s],root=TRUE,drift=TRUE, 
           type=c("asymmetric"), #other options: "symmetric","fixed","baxter-king","trigonometric" #see page 9 ECB Occasional paper real anf financial cycles in EU countries: stylised facts and modelling implications
           nfix=-1,
           theta=1)
  cf_filter_results<- cbind(data[,c('Date',var[s])], data[,c(var[s])]-cf_filter$cycle, cf_filter$cycle)
  colnames(cf_filter_results)<- c('Date',var[s], paste0("cf_trend_", var[s]), paste0("cf_cycle_", var[s]))
  return(cf_filter_results)}

var=c("GNI","NC", "RPP")
varpass<- list(1, 2, 3)
CF_Results_list<- lapply(varpass, cf_function)
CF_Results<- left_join(CF_Results_list[[1]], CF_Results_list[[2]])
CF_Results<- left_join(CF_Results, CF_Results_list[[3]])

nameworkbook<- paste0(base_path,"/D_Results/Alternative_estimates/ChristianoFitzgeraldt_results.xlsx")
wb <- openxlsx::createWorkbook(nameworkbook)
openxlsx::addWorksheet(wb,'CF')
openxlsx::writeData(wb, sheet = 'CF', data.frame(CF_Results))
openxlsx::saveWorkbook(wb, paste0(base_path,"/D_Results/Alternative_estimates/ChristianoFitzgeraldt_results.xlsx"), overwrite = TRUE)

#Real time properties: Chirstiano Fitzegarld-------------------------------------
#Plot: One side (OS) vs two sided
CF_credit<- merge.data.frame(CF_Results[, c('Date', "cf_cycle_NC")], CF_Results_list_oneside[[2]][, c('Date', "cf_cycle_NC")], by='Date')
colnames(CF_credit)<- c('Date', 'CF_NC_twosided', 'CF_NC_onesided')
CF_credit$Diff<- CF_credit$CF_NC_twosided-CF_credit$CF_NC_onesided

CF_DIFF<- CF_credit[41:length(CF_credit[,1]),] %>%   
  mutate(Date= as.yearqtr(Date)) %>% 
  pivot_longer(cols = c('CF_NC_twosided', 'CF_NC_onesided'), values_to = "value", names_to = 'variable') %>% 
  ggplot()+ 
  geom_line(aes(x=Date, y=value, colour = variable)) +
  labs(x = "Quarter",y = "", title = "")+
  theme_classic() +
  scale_y_continuous("National Credit cycle")+
  scale_color_manual(values = cbi_palette[c(1, 8,10)], name='')+
  guides(fill=guide_legend(title=""))+
  geom_hline(yintercept = 0, colour = cbi_palette[12], linetype='dotted')+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 40),
        axis.text=element_text(size=40),
        axis.title=element_text(size=40),
        legend.text =element_text(size=40))
sd(CF_credit$Diff, na.rm = T)
sd(CF_credit$Diff, na.rm = T)/sd(CF_credit$CF_NC_onesided, na.rm = T)

ggsave(paste0(path_plots,  "/Real_time/CF_NC_onevstwo_sides.pdf"), CF_DIFF, height = 20, width = 25)

CF_RPP<- merge.data.frame(CF_Results[, c('Date', "cf_cycle_RPP")], CF_Results_list_oneside[[3]][, c('Date', "cf_cycle_RPP")], by='Date')
colnames(CF_RPP)<- c('Date', 'CF_RPP_twosided', 'CF_RPP_onesided')
CF_RPP$Diff<- CF_RPP$CF_RPP_twosided-CF_RPP$CF_RPP_onesided

CF_DIFF_RPP<- CF_RPP[41:length(CF_RPP[,1]),] %>%   
  mutate(Date= as.yearqtr(Date)) %>% 
  pivot_longer(cols = c('CF_RPP_twosided', 'CF_RPP_onesided'), values_to = "value", names_to = 'variable') %>% 
  ggplot()+ 
  geom_line(aes(x=Date, y=value, colour = variable)) +
  labs(x = "Quarter",y = "", title = "")+
  theme_classic() +
  scale_y_continuous("House Prices cycle")+
  scale_color_manual(values = cbi_palette[c(1, 8,10)], name='')+
  guides(fill=guide_legend(title=""))+
  geom_hline(yintercept = 0, colour = cbi_palette[12], linetype='dotted')+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 40),
        axis.text=element_text(size=40),
        axis.title=element_text(size=40),
        legend.text =element_text(size=40))

sd(CF_RPP$Diff, na.rm = T)
sd(CF_RPP$Diff, na.rm = T)/sd(CF_RPP$CF_RPP_onesided, na.rm = T)
ggsave(paste0(path_plots,  "/Real_time/CF_RPP_onevstwo_sides.pdf"), CF_DIFF_RPP, height = 20, width = 25)

cor(CF_RPP$CF_RPP_twosided, CF_RPP$CF_RPP_onesided)
cor(CF_credit$CF_NC_onesided, CF_credit$CF_NC_onesided)

max(CF_RPP$Diff)
max(CF_credit$Diff)
sqrt(sum(CF_RPP$Diff^2, na.rm = T))
sqrt(sum(CF_credit$Diff^2, na.rm = T))

#Plot: Pseudo real time (nd: new data points)
start_date<- as.Date("2021-10-01")
end_date<- last(data$Date)
dates<-as.Date(as.yearqtr(seq.Date(start_date,end_date, by = '1 quarter')))
minperiod<-c(8,32,32)
maxperiod<-c(32, 120, 120)
data_nd<-function(t){
  data<- data %>% filter(Date<=dates[t])
  cf_function<- function(s){
    cf_filter<- cffilter(data[,var[s]],pl=minperiod[s],pu=maxperiod[s],root=T,drift=T, 
                         type=c("asymmetric"), #other options: "symmetric","fixed","baxter-king","trigonometric" #see page 9 ECB Occasional paper real anf financial cycles in EU countries: stylised facts and modelling implications, #see also specification in function CF_FILTER here \\Filescluster\shared\MPS\WORD\MPFS DEPT\Farah\Alternative_Gap\New_Model\Ruenstler_and_Vlekke_2018\D_Translating_to_R\Runstler_Vlekke_2018\Models
                         nfix=-1,
                         theta=1)
    cf_filter_results<- cbind(data[,c('Date',var[s])], data[,c(var[s])]-cf_filter$cycle, cf_filter$cycle)
    colnames(cf_filter_results)<- c('Date',var[s], paste0("cf_trend_", var[s]), paste0("cf_cycle_", var[s]))
    return(cf_filter_results)}
  
  var=c("GNI","NC", "RPP")
  varpass<- list(1, 2, 3)
  CF_Results_list<- lapply(varpass, cf_function)
  CF_Results<- left_join(CF_Results_list[[1]], CF_Results_list[[2]])
  CF_Results<- left_join(CF_Results, CF_Results_list[[3]])
  return(CF_Results)
}

list_dates<- lapply(c(1:length(dates)), FUN = function(s)s)
CF_nd_results<- lapply(list_dates, FUN=data_nd)

name<-paste0("nd",c(1:length(dates)))
nameworkbook<-paste0(base_path,"/D_Results/Alternative_estimates/ChristianoFitzgeraldt_results_nd.xlsx")
wb <- openxlsx::createWorkbook(nameworkbook)
for (p in 1:length(dates)){
  openxlsx::addWorksheet(wb,name[p])
  openxlsx::writeData(wb, sheet = name[p], data.frame(CF_nd_results[[p]]))}
openxlsx::saveWorkbook(wb, paste0(base_path,"/D_Results/Alternative_estimates/ChristianoFitzgeraldt_results_nd.xlsx"), overwrite = TRUE)

#A.2-VECM-----------------------------------------------------------------------
#Loan rates data----------------------------------
names_loan_rates_old<-read.csv(paste0(base_path, "/B_Data/Raw_data/Loan_Rates//", "b.1.2.csv"), header = F)[1,] # I am not sure if in the future the CBI will change the order/names of the variables, I suggest we always compare the new dowloaded data with the names found at the moment we did this code
loan_rates<- read.csv("https://opendata.centralbank.ie/dataset/4ba390e9-c0de-4da3-a7e2-e47acdaa21ce/resource/120a26a9-acb6-473e-9b9a-469f9b0037eb/download/b.1.2.csv", header = F) 
names_loan_rates<- loan_rates[1,]
names_ok<- unique(as.character(names_loan_rates==names_loan_rates_old))
ifelse(names_ok=='TRUE', "Ok", "Download this manually")#The series we want to download are Loans to households, overdrafts, interest rate (%) =position 2, and Loans to non-financial corporations, overdrafts, interest rate (%) =position 9
HH_loan_rates<-loan_rates %>%
  slice(-1) %>%
  dplyr::select("V1", 'V5') %>%
  rename(Date=V1, HH_loan_rates=V5) %>%
  mutate(Date=as.Date(Date,format = "%d/%m/%Y")) %>% 
  mutate(HH_loan_rates=as.numeric(HH_loan_rates)) %>% 
  mutate(Date= ceiling_date(Date, unit = "month")) %>% 
  as.data.frame()

#Source: MFD: Variable_Mortgage_Rates.xlsx - Column A: Mortgage Rates - All Mortgage Lenders (see note 3) Midpoint i.e. Average of lowest and highest rates
loan_rates_old<- read_xlsx(paste0(base_path, "/B_Data/Raw_data/", "Data_pre.xlsx"), sheet = 'Monthly')[,c(1,2)] 
loan_rates_old<- loan_rates_old %>% 
  mutate(Date=as.Date(Date,format = "%d/%m/%Y")) %>% 
  mutate(HH_loan_rates=as.numeric(HH_loan_rates)) %>% 
  mutate(Date= ceiling_date(Date, unit = "month")) %>% 
  as.data.frame() %>% 
  mutate(HH_loan_rates= as.numeric(HH_loan_rates))

compare_old<- loan_rates_old %>% 
  filter(Date>="2003-02-01") 
compare_new<- HH_loan_rates %>% 
  filter(Date<="2005-08-01") 
level<- mean(compare_old[,2]-compare_new[,2])

loan_rates_old<- loan_rates_old %>% 
  mutate(HH_loan_rates=HH_loan_rates+level) %>% 
  filter(Date<"2003-02-01") %>% 
  filter(Date>="1975-10-01")

HH_loan_rates<- rbind(loan_rates_old , HH_loan_rates) %>% 
  mutate(Date=as.Date(as.yearqtr(Date)))%>% 
  group_by(Date) %>%
  mutate(HH_loan_rates=mean(HH_loan_rates, na.rm = T)) %>% 
  mutate(HH_loan_rates=log(HH_loan_rates))
str(HH_loan_rates)
ggplot(HH_loan_rates, aes(x=Date, y=HH_loan_rates))+geom_line()+geom_vline(xintercept = as.Date("2003-02-01"))

CPI_all<- cso_get_data('CPM01', pivot_format = "tall", use_dates = TRUE, use_factors = FALSE, cache = FALSE) %>% 
  filter(Statistic=='Consumer Price Index (Base Dec 2016=100)')%>%
  filter(Commodity.Group== 'All items') %>% 
  rename(Date=Month) %>% 
  mutate(Date = as.Date(Date)) %>%
  dplyr::select(Date,value) %>% 
  rename(CPI_all=value) %>% 
  as.data.frame()

CPI_all<- CPI_all %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(Date =as.Date(as.yearqtr(Date))) %>% 
  group_by(Date) %>%
  mutate(CPI_all=mean(CPI_all, na.rm = T)) %>% #in the original file the take the value of the last month of the quarter, here we take the average across the quarter 
  ungroup() %>% 
  dplyr::select(Date, CPI_all) %>% 
  distinct()%>% 
  as.data.frame() %>% 
  mutate(CPI= CPI_all/last(CPI_all))# re-based to 100 LAST quarter, so variables are expressed in contemporanous values

HH_loan_rates<- merge.data.frame(HH_loan_rates, CPI_all, by='Date')
HH_loan_rates<- HH_loan_rates %>% 
  mutate(HH_loan_rates_r= HH_loan_rates-log(CPI))
write.xlsx(HH_loan_rates, file = paste0(base_path,'/B_Data/HH_loan_rates.xlsx'))

pHH_loan_rates_r<- ggplot(HH_loan_rates, aes(x=Date, y=HH_loan_rates_r))+geom_line()+geom_vline(xintercept = as.Date("2003-02-01"))
pHH_loan_rates<- ggplot(HH_loan_rates, aes(x=Date, y=HH_loan_rates))+geom_line()+geom_vline(xintercept = as.Date("2003-02-01"))
ggsave(paste0(base_path,"/D_Results/Plots/Data/","pHH_loan_rates_r.pdf"), pHH_loan_rates_r, height = 20, width = 25)
ggsave(paste0(base_path,"/D_Results/Plots/Data/", "pHH_loan_rates.pdf"), pHH_loan_rates, height = 20, width = 25)

#Johansen Test for Cointegration >  https://www.r-bloggers.com/2021/12/vector-error-correction-model-vecm-using-r/ #the maximum eigenvalue test : H0 : There are r cointegrating vectors // H1 : There are r + 1 cointegrating vectors
data_vec<- merge.data.frame(data,HH_loan_rates[, c('Date', 'HH_loan_rates_r')], by='Date')  #Exactly as Galan and Mencia
colnames(data_vec)<- c("Date", "GNI", "NC", "RPP", "HH_loan_rates")
dummy<- rep(0, length(data_vec$Date))
dummy[which(data_vec$Date>="2008-01-01" & data_vec$Date<="2012-10-01" )]<- rep(1,length(which(data_vec$Date>="2008-01-01" & data_vec$Date<="2012-10-01" ))) #Laeven and Valencia (2020) - Systemic Banking Crisis 
dummy[which(data_vec$Date>="2008-01-01" & data_vec$Date<="2009-01-01" )]<- rep(1,length(which(data_vec$Date>="2008-01-01" & data_vec$Date<="2009-01-01" ))) #Laeven and Valencia (2020) - Systemic Banking Crisis 
data_vec<- data_vec %>% na.omit(.)
#Estimation VECM----------------------------------
for (md in c("GM","Cust")){#GM: Galan and Mencia 2018 specification, Cust: customized
if(md=='GM'){
  name='GM'
  coint_ca.jo <- ca.jo(data_vec[,-1], ecdet = "const", type  = "eigen", K = 3, spec = "transitory", season = 4)#Exactly as Galan and Mencia
  vecm<- VECM(data_vec[,c('GNI', 'NC', 'RPP', 'HH_loan_rates')], estim="ML", LRinclude='const', lag=4, r=2)
  beta_vec <- coint_ca.jo@V[,1] # Eigenvectors, normalised to first column: (These are the cointegration relations)
  # Cointegrating vector (manually retrieved or from summary(coint_ca.jo))> # GDP - β11 * National Credit - β12 * RPP = μ
  residuals<- data.frame(GNI= coint_ca.jo@Z0[,1],NC =coint_ca.jo@Z0[,2] , RPP =coint_ca.jo@Z0[,3],HH_loan_rates =coint_ca.jo@Z0[,4])
  namecols<- names(data_vec)[-1]
  lengthdata<- length(data_vec[,1])
  lengthcoint<- length(residuals[,1])
  data_vec2<- data_vec
  data_vec2$coint<- beta_vec[5]+beta_vec[1]*data_vec[,'GNI']+beta_vec[2]*data_vec[,'NC']+beta_vec[3]*data_vec[,'RPP']+beta_vec[4]*data_vec[,'HH_loan_rates']
    }
  else{
  name= 'cust'
  coint_ca.jo <- ca.jo(data_vec[,c("GNI", "NC", 'RPP')], ecdet = "const", type  = "eigen", K = 3, spec = "transitory", season = 4)#Our own, put dumvar = dummy if you want a dummy back
  vecm<- VECM(data_vec[,c('GNI', 'NC', 'RPP')], estim="ML", LRinclude='none', lag=3, r=1)
  summary(coint_ca.jo) #Check order cointegration:test statistic exceeds the 1, 5 and 10% critical values?, there is at least one cointegration. In the second r = 1 null hypothesis, since the test statistic  19.19 doesnt exceed the any critical values, the second hypothesis not rejected
  coint_ca.jo@teststat
  summary(vecm) # beta_vec<- t(coefB(vecm)) to extract eigen values form cointegration
  
  beta_vec <- coint_ca.jo@V[,1] # Eigenvectors, normalised to first column: (These are the cointegration relations)
  # vecm<- VECM(data_vec[,c('GNI', 'NC', 'RPP')], estim="ML", LRinclude='none', lag=3, r=1, beta=c(1,-1,-1.229265)) #RESTRICTION
  # beta_vec <- c(1,-1, -1.229265) # Restriction: (These are the cointegration relations)
  residuals<- data.frame(GNI= coint_ca.jo@Z0[,1],NC =coint_ca.jo@Z0[,2] , RPP =coint_ca.jo@Z0[,3])
  namecols<- names(data_vec)[-1]
  lengthdata<- length(data_vec[,1])
  lengthcoint<- length(residuals[,1])
  data_vec2<- data_vec
  data_vec2$coint<-beta_vec[4]+ beta_vec[1]*data_vec[,'GNI']+beta_vec[2]*data_vec[,'NC']+beta_vec[3]*data_vec[,'RPP']
    }

coint<- data_vec2 %>%
  mutate(Date= as.yearqtr(Date)) %>% 
  pivot_longer(cols='coint' ,names_to = "variable", values_to = "value") %>% 
  ggplot()+ 
  geom_line(aes(x=Date, y=value, colour = variable)) +
  theme_classic() +
  labs(x = "Quarter",y = "log", title = "")+
  scale_color_manual(values = cbi_palette[c(1)], name='')+
  scale_x_yearqtr(format = "%YQ%q", breaks=rev(seq(as.yearqtr(last(data_vec2$Date)),as.yearqtr(first(data_vec2$Date)), by=-12)))+
  guides(fill=guide_legend(title=""))+
  geom_hline(yintercept = 0, colour = cbi_palette[12], linetype='dotted')+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 40),
        axis.text=element_text(size=40),
        axis.title=element_text(size=40),
        legend.text =element_text(size=40))
ggsave(paste0(path_plots,"/VECM/coint_",name, '.pdf'), coint, height = 20, width = 25)

data_vec2<- data_vec
plot_main<- function(s){
    var<- namecols[s]
    if (md=='GM'){
    data_vec2$ect<- -(beta_vec[5]+beta_vec[1]*data_vec[,'GNI']+beta_vec[2]*data_vec[,'NC']+beta_vec[3]*data_vec[,'RPP']+beta_vec[4]*data_vec[,'HH_loan_rates']- beta_vec[s]*data_vec[,s+1])/beta_vec[s]
    }else{
      data_vec2$ect<- -(beta_vec[1]*data_vec[,'GNI']+beta_vec[2]*data_vec[,'NC']+beta_vec[3]*data_vec[,'RPP']- beta_vec[s]*data_vec[,s+1])/beta_vec[s]
    }
    
    data_vec2$gap<- data_vec[, s+1]-data_vec2$ect - c(rep(NA, lengthdata-lengthcoint), residuals[,s])
    data_vec2<- data_vec2 %>% 
      dplyr::select(c("Date",var, "gap", "ect"))
    colnames(data_vec2)<- c("Date","data", "gap", "ect")
  
  dt<-data_vec2 %>%
    mutate(Date= as.yearqtr(Date)) %>% 
    pivot_longer(cols=c(data, ect) ,names_to = "variable", values_to = "value") %>% 
    ggplot()+ 
    geom_line(aes(x=Date, y=value, colour = variable))+
    theme_classic() +
    labs(x = "Quarter",y = "log", title = s)+
    scale_color_manual(values = cbi_palette[c(1,4)], name='')+
    scale_x_yearqtr(format = "%YQ%q", breaks=rev(seq(as.yearqtr(last(data_vec2$Date)),as.yearqtr(first(data_vec2$Date)), by=-12)))+
    guides(fill=guide_legend(title=""))+
    theme(legend.position = "bottom",
          plot.title = element_text(size = 40),
          axis.text=element_text(size=40),
          axis.title=element_text(size=40),
          legend.text =element_text(size=40))
  
  c<- data_vec2 %>%
    mutate(Date= as.yearqtr(Date)) %>% 
    pivot_longer(cols='gap' ,names_to = "variable", values_to = "value") %>% 
    ggplot()+ 
    geom_line(aes(x=Date, y=value, colour = variable)) +
    theme_classic() +
    labs(x = "Quarter",y = "log", title = "")+
    scale_color_manual(values = cbi_palette[c(8)], name='')+
    scale_x_yearqtr(format = "%YQ%q", breaks=rev(seq(as.yearqtr(last(data_vec$Date)),as.yearqtr(first(data_vec$Date)), by=-12)))+
    guides(fill=guide_legend(title=""))+
    geom_hline(yintercept = 0, colour = cbi_palette[12], linetype='dotted')+
    theme(legend.position = "bottom",
          plot.title = element_text(size = 40),
          axis.text=element_text(size=40),
          axis.title=element_text(size=40),
          legend.text =element_text(size=40))
  
  plot_d_t_c<- grid.arrange(dt, c, nrow = 1)
  
  if(md=='GM'){
    name='GM'
  }else{name='Custom'}
 ggsave(paste0(path_plots,"/VECM/",var,'_', name, '.pdf'), plot_d_t_c, height = 20, width = 25)
return(data_vec2)
}
if (md=="GM"){
select_var_data<-list(GNI=1, NC=2,RPP= 3, Loan_rates=4)
vec_Gaps<-lapply(select_var_data,plot_main)
}else{
  select_var_data<-list(GNI=1, NC=2,RPP= 3)
  vec_Gaps<-lapply(select_var_data,plot_main)
}

if(md=='GM'){
VEC_GM_NC<- vec_Gaps$NC[, c('Date',"gap")]
names(VEC_GM_NC)<-c('Date', 'VEC_GM_NC')}
else{VEC_cust_NC<- vec_Gaps$NC[, c('Date',"gap")]
names(VEC_cust_NC)<-c('Date', 'VEC_cust_NC')
}
}
vec_NC<- merge.data.frame(VEC_GM_NC, VEC_cust_NC, by='Date')

#B-Early warning----------------------------------------------------------------
#B.1 Bring all estimations------------------------------------------------------
#Two-sided data frame
data_cycles1<-openxlsx::read.xlsx(paste0(base_path, "/D_Results/Alternative_estimates/ChristianoFitzgeraldt_results.xlsx")) %>%
  mutate(Date=excel_numeric_to_date(Date)) %>% 
  dplyr::select(c("Date","cf_cycle_GNI",	"cf_cycle_NC","cf_cycle_RPP"))

data_cycles2<-openxlsx::read.xlsx(paste0(base_path, "/D_Results/Alternative_estimates/hodrickprescott_results.xlsx")) %>%
  mutate(Date=excel_numeric_to_date(Date)) %>% 
  dplyr::select(c("Date","hp_cycle_GNI",	"hp_cycle_NC","hp_cycle_RPP"))

#Source: Central Bank of Ireland - Macro Financial Division - Gap updates.xlsx - Column B: Alt. credit gap (revised)
data_cycles3<-openxlsx::read.xlsx(paste0(base_path, "/B_Data/Raw_data/OBrien_Velasco/Obrienvelasco_results.xlsx"), sheet='OV') %>%
  dplyr::select(c("Date","Alt..credit.gap.(revised)")) %>% 
  rename(ov_cycle_NC="Alt..credit.gap.(revised)") %>% 
  mutate(Date=as.Date(as.yearqtr(Date)))

data_cycles4<-openxlsx::read.xlsx(paste0(base_path, "/A_Main_Code/Outcome/Results_main.xlsx"))[,c(7:9)]
colnames(data_cycles4)<-  c("benchmark_cycle_GNI",	"benchmark_cycle_NC","benchmark_cycle_RPP")

first_date_benchmark<- seq(data_cycles1$Date[1], by="quarter", length.out=13) %>% last(.) #@Check first date benchmark model, normally we have a burn in period of 13 quarters, see A_Main_Code> GF3_S23_US_FIX_DYN_pars, line 31: Ym= X(13:end,1:3);
data_cycles4$Date<- seq(first_date_benchmark, by="quarter", length.out=length(data_cycles4[,1]))

data_cycles<- merge.data.frame(data_cycles1,data_cycles2, by = 'Date')
data_cycles<- merge.data.frame(data_cycles,data_cycles3, by = 'Date')
data_cycles<- merge.data.frame(data_cycles,data_cycles4, by = 'Date')

colnames(data_cycles)
data_cycles<- data_cycles %>% 
  dplyr::select(c("Date",'cf_cycle_NC', 'hp_cycle_NC', 'benchmark_cycle_NC', "ov_cycle_NC",
                'cf_cycle_RPP', 'hp_cycle_RPP', 'benchmark_cycle_RPP', 
                'cf_cycle_GNI', 'hp_cycle_GNI', 'benchmark_cycle_GNI')) %>% 
  rename(CF_NC=cf_cycle_NC, HP_NC=hp_cycle_NC,Benchmark_NC=benchmark_cycle_NC, OV_NC=ov_cycle_NC) %>%
  rename(CF_RPP=cf_cycle_RPP, HP_RPP=hp_cycle_RPP,Benchmark_RPP=benchmark_cycle_RPP) %>% 
  rename(CF_GNI=cf_cycle_GNI, HP_GNI=hp_cycle_GNI,Benchmark_GNI=benchmark_cycle_GNI) %>%
  mutate(Benchmark_NC_prob=(Benchmark_NC-min(Benchmark_NC, na.rm = T))/max(Benchmark_NC-min(Benchmark_NC, na.rm = T), na.rm = T),
         HP_NC_prob=(HP_NC-min(HP_NC, na.rm = T))/max(HP_NC-min(HP_NC, na.rm = T), na.rm = T),
         CF_NC_prob=(CF_NC-min(CF_NC, na.rm = T))/max(CF_NC-min(CF_NC, na.rm = T), na.rm = T),
         OV_NC_prob=(OV_NC-min(OV_NC, na.rm = T))/max(OV_NC-min(OV_NC, na.rm = T), na.rm = T)) 

data_cycles<- data_cycles %>%   
  mutate(Benchmark_RPP_prob=(Benchmark_RPP-min(Benchmark_RPP, na.rm = T))/max(Benchmark_RPP-min(Benchmark_RPP, na.rm = T), na.rm = T),
  HP_RPP_prob=(HP_RPP-min(HP_RPP, na.rm = T))/max(HP_RPP-min(HP_RPP, na.rm = T), na.rm = T),
  CF_RPP_prob=(CF_RPP-min(CF_RPP, na.rm = T))/max(CF_RPP-min(CF_RPP, na.rm = T), na.rm = T)) %>% 
  mutate(Benchmark_GNI_prob=(Benchmark_GNI-min(Benchmark_GNI, na.rm = T))/max(Benchmark_GNI-min(Benchmark_GNI, na.rm = T), na.rm = T),
       HP_GNI_prob=(HP_GNI-min(HP_GNI, na.rm = T))/max(HP_GNI-min(HP_GNI, na.rm = T), na.rm = T),
       CF_GNI_prob=(CF_GNI-min(CF_GNI, na.rm = T))/max(CF_GNI-min(CF_GNI, na.rm = T), na.rm = T)) %>% 
  mutate(Benchmark_comp= (0.5*Benchmark_NC-mean(Benchmark_NC, na.rm=T))/sd(Benchmark_NC, na.rm=T)+(0.5*Benchmark_RPP-mean(Benchmark_RPP, na.rm=T))/sd(Benchmark_RPP, na.rm=T)) %>% 
  mutate(Benchmark_comp_prob=(Benchmark_comp-min(Benchmark_comp, na.rm = T))/max(Benchmark_comp-min(Benchmark_comp, na.rm = T), na.rm = T))

data_cycles<- merge.data.frame(data_cycles,vec_NC, by='Date') 
data_cycles<-data_cycles %>% 
  mutate(VEC_GM_NC_prob=(VEC_GM_NC-min(VEC_GM_NC, na.rm = T))/max(VEC_GM_NC-min(VEC_GM_NC, na.rm = T), na.rm = T),
         VEC_cust_NC_prob=(VEC_cust_NC-min(VEC_cust_NC, na.rm = T))/max(VEC_cust_NC-min(VEC_cust_NC, na.rm = T), na.rm = T))

#One-sided dataframe
oneside_cf<-CF_Results_list_oneside[[2]][,c("Date","cf_cycle_NC")]
data_cycles_os<- merge.data.frame(data_cycles,oneside_cf, by='Date') 
data_cycles_os<-data_cycles_os %>%
  dplyr::select(-CF_NC) %>% 
  rename(CF_NC=cf_cycle_NC) %>% 
  mutate(CF_NC_prob=(CF_NC-min(CF_NC, na.rm = T))/max(CF_NC-min(CF_NC, na.rm = T), na.rm = T))

oneside_cf<-CF_Results_list_oneside[[3]][,c("Date","cf_cycle_RPP")]
data_cycles_os<- merge.data.frame(data_cycles_os,oneside_cf, by='Date') 
data_cycles_os<-data_cycles_os %>%
  dplyr::select(-CF_RPP) %>% 
  rename(CF_RPP=cf_cycle_RPP) %>% 
  mutate(CF_RPP_prob=(CF_RPP-min(CF_RPP, na.rm = T))/max(CF_RPP-min(CF_RPP, na.rm = T), na.rm = T))

oneside_hp<-HP_Results_list_oneside[[2]][,c("Date","hp_cycle_NC")]
data_cycles_os<- merge.data.frame(data_cycles_os,oneside_hp, by='Date') 
data_cycles_os<-data_cycles_os %>%
  dplyr::select(-HP_NC) %>% 
  rename(HP_NC=hp_cycle_NC) %>% 
  mutate(HP_NC_prob=(HP_NC-min(HP_NC, na.rm = T))/max(HP_NC-min(HP_NC, na.rm = T), na.rm = T))

oneside_hp<-HP_Results_list_oneside[[3]][,c("Date","hp_cycle_RPP")]
data_cycles_os<- merge.data.frame(data_cycles_os,oneside_hp, by='Date') 
data_cycles_os<-data_cycles_os %>%
  dplyr::select(-HP_RPP) %>% 
  rename(HP_RPP=hp_cycle_RPP) %>% 
  mutate(HP_RPP_prob=(HP_RPP-min(HP_RPP, na.rm = T))/max(HP_NC-min(HP_RPP, na.rm = T), na.rm = T))

#Real time properties: Benchmark-------------------------------------------------
#One side benchmark
data_cycles_oneside<-openxlsx::read.xlsx(paste0(base_path, "/A_Main_Code/Outcome/Results_oneside.xlsx"), colNames = F)
first_date_oneside<- seq(first_date_benchmark, by="quarter", length.out=12) %>% last(.) #@Check first date one side, see A_Main_Code> GF3_S23_US_FIX_DYN_pars, Cycles_t.C20
data_cycles_oneside$Date<- seq(first_date_benchmark, by="quarter", length.out=length(data_cycles_oneside[,1]))
colnames(data_cycles_oneside)<-c("Benchmark_GNI", "Benchmark_RPP", "Benchmark_NC",'Date')

data_cycles_oneside<-data_cycles_oneside %>% 
  mutate(Benchmark_NC_prob=(Benchmark_NC-min(Benchmark_NC, na.rm = T))/max(Benchmark_NC-min(Benchmark_NC, na.rm = T), na.rm = T),
         Benchmark_RPP_prob=(Benchmark_RPP-min(Benchmark_RPP, na.rm = T))/max(Benchmark_RPP-min(Benchmark_RPP, na.rm = T), na.rm = T)) %>% 
  dplyr::select(Date,Benchmark_NC, Benchmark_NC_prob, Benchmark_RPP, Benchmark_RPP_prob) %>% 
  mutate(Benchmark_comp= (0.5*Benchmark_NC-mean(Benchmark_NC, na.rm=T))/sd(Benchmark_NC, na.rm=T)+(0.5*Benchmark_RPP-mean(Benchmark_RPP, na.rm=T))/sd(Benchmark_RPP, na.rm=T)) %>% 
  mutate(Benchmark_comp_prob= (Benchmark_comp-min(Benchmark_comp, na.rm = T))/max(Benchmark_comp-min(Benchmark_comp, na.rm = T), na.rm = T))

data_cycles_os<- data_cycles_os %>% 
  dplyr::select(-c(Benchmark_NC,Benchmark_NC, Benchmark_NC_prob, Benchmark_RPP, Benchmark_RPP_prob,
                   Benchmark_comp, Benchmark_comp_prob))
data_cycles_os<- merge.data.frame(data_cycles_os,data_cycles_oneside, by='Date')

#Two-sided vs One-sided benchmark-------------------------
data_cycles_full_benchmark<-data_cycles4 %>%  
  dplyr::select(c("Date", 'benchmark_cycle_NC', 'benchmark_cycle_RPP')) %>% 
  rename(Benchmark_NC=benchmark_cycle_NC) %>%
  rename(Benchmark_RPP=benchmark_cycle_RPP) %>% 
  mutate(Benchmark_NC_prob=(Benchmark_NC-min(Benchmark_NC, na.rm = T))/max(Benchmark_NC-min(Benchmark_NC, na.rm = T), na.rm = T)) %>% 
  mutate(Benchmark_RPP_prob=(Benchmark_RPP-min(Benchmark_RPP, na.rm = T))/max(Benchmark_RPP-min(Benchmark_RPP, na.rm = T), na.rm = T))

#National credit
diff_NC_benchmark<- merge.data.frame(data_cycles_full_benchmark[, c('Date', 'Benchmark_NC')],data_cycles_oneside[, c('Date', 'Benchmark_NC')], by='Date')
colnames(diff_NC_benchmark)<- c("Date",'Benchmark_NC_twosided', 'Benchmark_NC_onesided')
diff_NC_benchmark$Diff<- diff_NC_benchmark$Benchmark_NC_twosided-diff_NC_benchmark$Benchmark_NC_onesided

Benchmark_DIFF<- diff_NC_benchmark %>%   
  # mutate(Diff=Diff*3) %>% 
  mutate(Date= as.yearqtr(Date)) %>% 
  pivot_longer(cols = c('Benchmark_NC_twosided', 'Benchmark_NC_onesided'), values_to = "value", names_to = 'variable') %>% 
  ggplot()+ 
  geom_line(aes(x=Date, y=value, colour = variable)) +
  labs(x = "Quarter",y = "", title = "")+
  theme_classic() +
  scale_y_continuous("National Credit cycle")+
  scale_color_manual(values = cbi_palette[c(1, 8,10)], name='')+
  # scale_x_yearqtr(format = "%YQ%q", breaks=rev(seq(last(dt[,1],1), dt[1,1],by=-600)))+
  guides(fill=guide_legend(title=""))+
  geom_hline(yintercept = 0, colour = cbi_palette[12], linetype='dotted')+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 40),
        axis.text=element_text(size=40),
        axis.title=element_text(size=40),
        legend.text =element_text(size=40))
sd(diff_NC_benchmark$Diff, na.rm = T)
sd(diff_NC_benchmark$Diff, na.rm = T)/sd(diff_NC_benchmark$Benchmark_NC_onesided, na.rm = T)
cor(diff_NC_benchmark$Benchmark_NC_twosided[-c(1:15)], diff_NC_benchmark$Benchmark_NC_onesided[-c(1:15)])
max(diff_NC_benchmark$Diff, na.rm = T)
sqrt(sum(diff_NC_benchmark$Diff^2, na.rm = T))
ggsave(paste0(path_plots,  "/Real_time/Benchmark_NC_onevstwoside.pdf"), Benchmark_DIFF, height = 20, width = 25)

#House prices
diff_RPP_benchmark<- merge.data.frame(data_cycles_full_benchmark[, c('Date', 'Benchmark_RPP')],data_cycles_oneside[, c('Date', 'Benchmark_RPP')], by='Date')
colnames(diff_RPP_benchmark)<- c("Date",'Benchmark_RPP_twosided', 'Benchmark_RPP_onesided')
diff_RPP_benchmark$Diff<- diff_RPP_benchmark$Benchmark_RPP_twosided-diff_RPP_benchmark$Benchmark_RPP_onesided
cor(diff_RPP_benchmark$Benchmark_RPP_twosided[-c(1:15)], diff_RPP_benchmark$Benchmark_RPP_onesided[-c(1:15)])

Benchmark_DIFF<- diff_RPP_benchmark %>%   
  # mutate(Diff=Diff*3) %>% 
  mutate(Date= as.yearqtr(Date)) %>% 
  pivot_longer(cols = c('Benchmark_RPP_twosided', 'Benchmark_RPP_onesided'), values_to = "value", names_to = 'variable') %>% 
  ggplot()+ 
  geom_line(aes(x=Date, y=value, colour = variable)) +
  labs(x = "Quarter",y = "", title = "")+
  theme_classic() +
  scale_y_continuous("House Prices cycle")+
  scale_color_manual(values = cbi_palette[c(1, 8,10)], name='')+
  # scale_x_yearqtr(format = "%YQ%q", breaks=rev(seq(last(dt[,1],1), dt[1,1],by=-600)))+
  guides(fill=guide_legend(title=""))+
  geom_hline(yintercept = 0, colour = cbi_palette[12], linetype='dotted')+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 40),
        axis.text=element_text(size=40),
        axis.title=element_text(size=40),
        legend.text =element_text(size=40))
sd(diff_RPP_benchmark$Diff, na.rm = T)
sd(diff_RPP_benchmark$Diff, na.rm = T)/sd(diff_RPP_benchmark$Benchmark_RPP_onesided, na.rm = T)
max(diff_RPP_benchmark$Diff, na.rm = T)
sqrt(sum(diff_RPP_benchmark$Diff^2, na.rm = T))
ggsave(paste0(path_plots,  "/Real_time/Benchmark_RPP_onevstwoside.pdf"), Benchmark_DIFF, height = 20, width = 25)

#B.2 Crisis events--------------------------------------------------------------
#Currency crisis:-----------
data<-openxlsx::read.xlsx(paste0(base_path, "/B_Data/Raw_data/Crisis_indicators/fred_exchange_rate_IE.xlsx"), sheet="Quarterly") %>% 
  rename(Date=observation_date,FX=CCUSMA02IEQ618N) %>% 
  mutate(Date=convertToDate(Date)) %>% 
  filter(Date<='2002-01-01') %>% 
  filter(Date>='1978-01-01')

l_hp <- length(as.data.frame(data)[,1])
hp_cycle <- rep(0,l_hp)
hp_trend <- rep(0,l_hp)
hpfilter_40 <- hpfilter(as.data.frame(data)[1:(40),2],type ='lambda' ,freq = 1600) #Burn in period
hp_cycle[1:40] <- hpfilter_40$cycle
hp_trend[1:40] <- hpfilter_40$trend
for(i in 1:(l_hp-40)){
  hpfilter_40 <- hpfilter(as.data.frame(data)[1:(40+i),2] ,type ='lambda', freq = 1600) #Expanded window
  hp_cycle[40+i] <- hpfilter_40$cycle[i+40]
  hp_trend[40+i] <- hpfilter_40$trend[i+40]
}
var_decomp<-as.data.frame(cbind.data.frame(data[,"Date"], data[,2], hp_trend, hp_cycle))
colnames(var_decomp)<- c('Date','FX', paste0("hp_trend_", 'FX'), paste0("hp_cycle_", 'FX'))
var_decomp<- var_decomp %>% mutate(Date=as.yearqtr(Date,format = "%YQ%q")) 

fx_trend<- ggplot(var_decomp,aes(x=Date,y= hp_trend_FX, color='Trend (HP)'))+geom_line()+geom_line(aes(y=FX, color='FX (IE/US'))+
  theme_classic() +
  labs(x = "Quarter",y = "FX (IE/US)", title = '')+
  scale_color_manual(values = c("#0B5471",  "#5EC5C2" ), name="")+
  scale_x_yearqtr(format = "%YQ%q", breaks=rev(seq(last(var_decomp[,1]),first(var_decomp[,1]), by=-6)))+
  guides(fill=guide_legend(title=""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 40),
        axis.text=element_text(size=40),
        axis.title=element_text(size=40),
        legend.text =element_text(size=40))
ggsave(paste0(path_plots,"/Crisis_Events/fx",".pdf"), fx_trend, height = 20, width = 25)

nameworkbook<- paste0(path,"/1.Crisis_events/fx_filtered.xlsx")
wb <- openxlsx::createWorkbook(nameworkbook)
openxlsx::addWorksheet(wb,'FX')
openxlsx::writeData(wb, sheet = 'FX', data.frame(var_decomp))
openxlsx::saveWorkbook(wb, paste0(path,"/1.Crisis_events/fx_filtered.xlsx"), overwrite = TRUE)

#Systemic Crisis indicator----------------
pre_crisis<- 3 #@SELECT, years
data_crisis<- list()

#All sample dummy: it is the wider set, Babecky·et al. (2012)+ Laeven and Valencia (2020)+ 	FX	+Baron and Dieckelmann (2022)
data_crisis[[1]]<-openxlsx::read.xlsx(paste0(path, "/1.Crisis_events/0.Crisis_indicator.xlsx"), sheet="Crisis") %>% 
  mutate(Date=convertToDate(Date)) %>% 
  rename(Crisis=Crisis_Babecky_Baron) %>% 
  mutate(Crisis_3y_lag=lead(Crisis, pre_crisis*4), 
         Pre_crisis=if_else(Crisis==0,Crisis_3y_lag,0)) %>% 
  dplyr::select(Date,Crisis,Crisis_3y_lag,Pre_crisis, Reference, Label) %>% 
  replace(is.na(.), 0) %>% 
  mutate(Reference=ifelse(as.character(Reference)=="0", NA, Reference)) %>% 
  mutate(Reference=str_wrap(Reference, width=22))

plot_data<- data_crisis[[1]] %>% 
  mutate(bank_distress=if_else(Date %in% as.Date(c("1990-01-01", "1990-04-01", "1990-07-01",  "1990-10-01",
                                       	"2016-01-01", "2016-04-01", "2016-07-01", "2016-10-01")), 1,0)) %>% 
  mutate(systemic_crisis=ifelse(bank_distress!=1,Crisis, 0)) %>% 
  mutate(bank_distress=as.numeric(bank_distress), 
         systemic_crisis=as.numeric(systemic_crisis)) %>% 
  mutate(bank_distress=if_else(bank_distress==0, NA, 1), 
         systemic_crisis=if_else(systemic_crisis==0, NA, 1)) %>% 
  dplyr::select(Date,systemic_crisis,bank_distress,Reference, Label) %>% 
  pivot_longer(cols = !c(Date,Reference, Label), names_to = "type", values_to="Crisis")

systemic_crisisplot<- ggplot(plot_data, aes(x=Date,y=Crisis))+
  geom_ribbon(aes(ymin=0, ymax=Crisis, fill=type), alpha=0.5)+
  geom_text(aes(label=Reference, y=Label), vjust=1, hjust=0., size=10, angle=0, position ="identity")+
  guides(fill=guide_legend(title=""))+
  theme_classic() +
  labs(
    x = "Date",
    y = "Crisis Dummy",
    title = ""
  )+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 40),
        axis.text=element_text(size=40),
        axis.title=element_text(size=40),
        legend.text =element_text(size=40))+
  scale_fill_manual(values = c('#FCAF17','#0083A0'))
ggsave(paste0(path_plots,"/Crisis_events/systemic_crisisplot",".pdf"), systemic_crisisplot, height = 20, width = 25)

#Systemic crisis dummy: Like the first one but excluding Baron and Dieckelmann (2022), why because these authors add banks disruptions (Bank Eq. 30% decline) not associated to bank panic nor crisis. Separately, we are going to analyze this when we work with the substantial drops in assets, i.e. crisis dummy 3 onward (Banking Crisis indicator)
data_crisis[[2]]<-openxlsx::read.xlsx(paste0(path, "/1.Crisis_events/0.Crisis_indicator.xlsx"), sheet="Crisis") %>% 
  mutate(Date=convertToDate(Date)) %>% 
  rename(Crisis=Crisis_Babecky) %>% 
  mutate(Crisis_3y_lag=lead(Crisis, pre_crisis*4), 
         Pre_crisis=if_else(Crisis==0,Crisis_3y_lag,0)) %>% 
  dplyr::select(Date,Crisis,Crisis_3y_lag,Pre_crisis, Reference, Label) %>% 
  replace(is.na(.), 0) %>% 
  mutate(Reference=ifelse(as.character(Reference)=="0", NA, Reference)) %>% 
  mutate(Reference=str_wrap(Reference, width=22))

all_samples_crisis<- merge.data.frame(data_crisis[[1]], data_crisis[[2]], by='Date')
names(all_samples_crisis)<-c('Date',paste0(c('Babecky_Baron_'), names(data_crisis[[1]])[-1]),paste0(c('Babecky_'), names(data_crisis[[1]])[-1]))
all_samples_crisis<-all_samples_crisis %>%  
  dplyr::select(-c("Babecky_Baron_Label","Babecky_Reference","Babecky_Label"))
write.xlsx(as.data.frame(all_samples_crisis),paste0(path,"/1.Crisis_events/all_samples_crisis.xlsx"))

#Banking Crisis indicator----------------
bank_crisis_data<- openxlsx::read.xlsx(paste0(path, "/1.Crisis_events/bank_crisis_dummy.xlsx"), sheet="Bank_crisis_dummy")
bank_crisis_names<- names(bank_crisis_data)[-1]
for (n in c(1:length(bank_crisis_names))) {
  data_crisis[[2+n]]<-openxlsx::read.xlsx(paste0(path, "/1.Crisis_events/bank_crisis_dummy.xlsx"), sheet="Bank_crisis_dummy") %>% 
    rename(Crisis= bank_crisis_names[n]) %>% 
    mutate(Date=convertToDate(Date)) %>% 
    mutate(Crisis_3y_lag=lead(Crisis, pre_crisis*4), 
           Pre_crisis=if_else(Crisis==0,Crisis_3y_lag,0)) %>% 
    dplyr::select(Date,Crisis,Crisis_3y_lag,Pre_crisis) %>% 
    replace(is.na(.), 0)}

#B.3 Early warning Performance and optimal thresholds-----------------------------------------
setwd(path)
path_auc<- paste0(base_path, '/D_Results/Tables')
setwd(path_auc)
nameworkbook<- paste0("results_metrics.xlsx")
wb <- openxlsx::createWorkbook(nameworkbook)
setwd(path)

all_crisis_names<- c('Babecky_Baron', 'Babecky', paste0("Bank_crisis_",1:length(bank_crisis_names)))
indicator<-c("Benchmark_NC", "HP_NC", "CF_NC", "OV_NC",
             "Benchmark_RPP", "HP_RPP", "CF_RPP",
             "Benchmark_GNI", "HP_GNI", "CF_GNI",
             'VEC_cust_NC','VEC_GM_NC',
             "Benchmark_comp")#@set here all the names of the models/indicators
indicator<-indicator[c(1:8,11:13)]#@subsample: select the indicators for which you want to obtain the metrics
indicator_prob<-paste0(indicator,'_prob')

data_cycles_os<- data_cycles_os %>% 
  dplyr::select(Date, indicator_prob, indicator)
data_cycles<- data_cycles %>% 
  dplyr::select(Date, indicator_prob, indicator)

for (oneside_estimate in c(F,T)){
    
    if(oneside_estimate==T){
      data_ew<-data_cycles_os
    }else{
      data_ew<-data_cycles}
    
  roc_crisis<- function(c){#Babecky (c=2) is the benchmark, one could do all the assessment with the alternative Babecky_Baron crisis indicators
    
    data_roc<- merge(data_crisis[[c]], data_ew, by='Date')
    
    meanov=mean(data_roc$OV_NC, na.rm=T)
    meanovprob=mean(data_roc$OV_NC_prob, na.rm=T)
    data_roc<-data_roc %>% 
      mutate(OV_NC=if_else(is.na(OV_NC),meanov, OV_NC)) %>% 
      mutate(OV_NC_prob=if_else(is.na(OV_NC_prob),meanovprob, OV_NC_prob))
  
    roc_f <- function(m){
      
      y <- as.factor(data_roc[['Pre_crisis']])
      x <- data_roc[[ indicator_prob[m] ]]
      
      ok <- is.finite(x) & !is.na(y)
      y <- droplevels(y[ok]); x <- x[ok]
      if (nlevels(y) < 2) {
        return(list(sensitivity=NA, specificity=NA, auc=NA, thresholds=NA))
      }
      
      roc_obj <- tryCatch(
        roc(y, x, direction="<", quiet=TRUE,
            smooth=TRUE, smooth.method="density", bw=bw.nrd0(x)*0.99),
        error = function(e) {
          xj <- jitter(x, amount = sd(x, na.rm=TRUE)*1e-6)
          tryCatch(
            roc(y, xj, direction="<", quiet=TRUE,
                smooth=TRUE, smooth.method="density", bw=bw.nrd0(xj)*0.99),
            error = function(e2) roc(y, x, direction="<", quiet=TRUE, smooth=FALSE)
          )
        }
      )
      sensitivity <- data.frame(setNames(list(roc_obj$sensitivities), indicator[m]))
      specificity <- data.frame(setNames(list(roc_obj$specificities), indicator[m]))
      auc_val     <- as.numeric(auc(roc_obj))
      
      thresholds  <- if (!is.null(roc_obj$thresholds))
        data.frame(setNames(list(roc_obj$thresholds), indicator[m]))
      else NA
      
      list(sensitivity=sensitivity, specificity=specificity, auc=auc_val, thresholds=thresholds)
    }
    roc_results <- lapply(seq_along(indicator), roc_f)
    
    max_length<- length(data_roc[,1])
    roc_f_th<- function(m){
      roc<- roc(as.factor(data_roc[,'Pre_crisis']), data_roc[,indicator_prob[m]],direction="<",quiet=TRUE, smooth=F)
      sensitivity<- as.data.frame(c(roc$sensitivities, rep(NA,max_length-length(roc$sensitivities)+1)))
      specificity<- as.data.frame(c(roc$specificities, rep(NA,max_length-length(roc$sensitivities)+1)))
      names(sensitivity)<- paste0(indicator[m])
      names(specificity)<- paste0(indicator[m])
      auoc<-as.vector(roc$auc)
      thresholds<- as.data.frame(c(roc$thresholds, rep(NA,max_length-length(roc$sensitivities)+1)))
  
      list_results<- list(sensitivity,specificity, auoc,thresholds)
      return(list_results)}
    list_models<- lapply(c(1:length(indicator)), FUN = function(s)s)
    roc_results_th<- lapply(list_models, roc_f_th)
    
    #PLOT ROCs----
    sensitivity<- lapply(list_models, function(m){roc_results[[m]][[1]]})
    s_length<- length(sensitivity[[1]][,1])
    sensitivity<- do.call(cbind, sensitivity) %>% 
      as.data.frame() %>% 
      mutate(id = seq_along(1:(s_length))) %>%
      pivot_longer(cols=indicator, names_to = "model", values_to = "sensitivity") %>% 
      filter(model!="Benchmark_GNI")
    
    specificity<- lapply(list_models, function(m){roc_results[[m]][[2]]})
    s_length<- length(specificity[[1]][,1])
    specificity<- do.call(cbind, specificity) %>% 
      as.data.frame() %>% 
      mutate(id = seq_along(1:(s_length))) %>%
      pivot_longer(cols=indicator, names_to = "model", values_to = "specificity")%>% 
      filter(model!="Benchmark_GNI")
  
    final = left_join(sensitivity, specificity, by = c("model", 'id'))
    final = final %>% filter(model!="VEC_GM_NC")
    len<- length(unique(final$model))
    palette = c("#0B5471",  "#D2E288", "#5EC5C2",  "#D12E7C", "#F57D20", "#7e878e", "#FCAF17", "#DFCA94", "#000000", "#7C477E", "#0083A0", "#5EC5C2", "#007DC5")[1:len]
    
    roc_plot<- final %>%
      filter(model!=c("Benchmark_RPP")) %>%
      filter(model!=c("CF_RPP")) %>%
      filter(model!=c("HP_RPP")) %>%
      ggplot() +
      geom_line(aes(x = 1-specificity, y = sensitivity, colour = model), linewidth = 2.5) +
      geom_abline(colour = "#696969", linetype = 2) +
      theme_classic() +
      labs(
        x = "False Positive Rate",
        y = "True Positive Rate",
        title = ""
      )+
      theme(legend.position = "bottom",
            plot.title = element_text(size = 40),
            axis.text=element_text(size=40),
            axis.title=element_text(size=40),
            legend.text =element_text(size=40)) +
      scale_colour_manual("", values = palette) +
      scale_linetype(guide = 'none')
    if(c %in% c(1,2)){
    ggsave(paste0(path_plots,"/Early_warning/roc_NC_",all_crisis_names[c],".pdf"), roc_plot, height = 20, width = 25)
    }else{}
  
    roc_plot<- final %>%
      filter(model!=c("OV_NC")) %>%
      filter(model!=c("VEC_cust_NC")) %>%
      filter(model!=c("Benchmark_NC")) %>%
      filter(model!=c("HP_NC")) %>%
      filter(model!=c("CF_NC")) %>%
    
      ggplot() +
      geom_line(aes(x = 1-specificity, y = sensitivity, colour = model), linewidth = 2.5) +
      geom_abline(colour = "#696969", linetype = 2) +
      theme_classic() +
      labs(
        x = "False Positive Rate",
        y = "True Positive Rate",
        title = ""
      )+
      theme(legend.position = "bottom",
            plot.title = element_text(size = 40),
            axis.text=element_text(size=40),
            axis.title=element_text(size=40),
            legend.text =element_text(size=40)) +
      scale_colour_manual("", values = palette) +
      scale_linetype(guide = 'none')
    
    if(c %in% c(1,2)){
    ggsave(paste0(path_plots,"/Early_warning/roc_RPP_",all_crisis_names[c],".pdf"), roc_plot, height = 20, width = 25)
    }else{}
    
    #Thresholds (related to pre crisis period)---------------- 
    #TPR = TP / (TP + FN) = sensitivity
    #FPR = FP / (FP + TN) = 1-specificity
    #Specificity = TN / (FP + TN)
    
    #y -> TPR = Sensitivity = 1-FN Rate ie 
    #FNR = 1-Sensitivity
    #x -> FPR = 1-Specificity 
    #TN = Specificity
  
    threshold_f<- function(m){
      
      TPR<-roc_results_th[[m]][[1]]#sensitivities
      FPR<- 1-roc_results_th[[m]][[2]]#specificities
      FNR<- 1-roc_results_th[[m]][[1]]#sensitivities
      TNR<- roc_results_th[[m]][[2]]#specificities 
    
      sum_a<- TPR+TNR #Youden's J statistic: Obtain the minimum threshold that gives you the maximum TPR and TNR
      F1<- TPR / (TPR+(FPR+FNR)/2)#F1 the bigger the ratio of correctly predicted crises (TP) relative to erroneous  predictions (FP + FN) is, the bigger is F1, see Beutel et al. (2019) Does Machine Learning Help us Predict Banking Crises?
      
      accuracy<- as.data.frame(cbind(TPR, FNR, sum_a, F1))
      names(accuracy)<- c("TPR", "TN", "sum_a", 'F1')
    
      min_scaling<-min(data_ew[,indicator[m]], na.rm = T)
      max_scaling<-max(data_ew[,indicator[m]]-min_scaling, na.rm = T)
  
      threshold_position_sum_a<-which(accuracy$sum_a==max(accuracy$sum_a, na.rm = T))
      threshold_optimal_scaled_sum_a<- min(roc_results_th[[m]][[4]][,1][threshold_position_sum_a])
      threshold_optimal_sum_a<- (threshold_optimal_scaled_sum_a*max_scaling)+min_scaling
      
      threshold_position_F1<-which(accuracy$F1==max(accuracy$F1, na.rm = T))
      threshold_optimal_scaled_F1<- min(roc_results_th[[m]][[4]][,1][threshold_position_F1])
      threshold_optimal_F1<-  (threshold_optimal_scaled_F1*max_scaling)+min_scaling
      threshold_optimal<- c(threshold_optimal_sum_a, threshold_optimal_F1,threshold_optimal_scaled_F1)
      threshold_F1<- list(threshold_optimal, F1)
      return(threshold_F1)}
   
    #Extract F1------------
    list_indicator<- lapply(c(1:length(indicator)), FUN = function(s)s)
    thresholds_F1<-lapply(list_models, threshold_f)
    thresholds<-lapply(list_indicator, function(s)thresholds_F1[[s]][[1]])
    thresholds<- t(Reduce(function(x,y)cbind(x,y), thresholds)) %>% as.data.frame() 
    names(thresholds)<-c('T(TPR_TNR)', 'T(F1)', 'T(F1)scaled')
    
    F1<-lapply(list_indicator, function(s)thresholds_F1[[s]][[2]])
    F1<-  Reduce(cbind, F1)
    F1_max<- apply(F1, 2, function(s)max(s, na.rm = T))
      
    #Usefulness: see ECB working paper 2013, page 7-9: On policymakers’ loss functions and the evaluation  of early warning systems Peter Sarli
    p1<- sum(data_roc[,'Pre_crisis'])/length(as.factor(data_roc[,'Pre_crisis']))# probability of being in a pre-crisis
    p2<- 1-p1
    
    useful_f<- function(m){
      
      min_scaling<-min(data_ew[,indicator[m]], na.rm = T)
      max_scaling<-max(data_ew[,indicator[m]]-min_scaling, na.rm = T)
      
      T1<-1-roc_results_th[[m]][[1]]#type error 1, probability of not receiving a warning conditional on a crisis occurring 
      T2<-1-roc_results_th[[m]][[2]]#type error 2, probability of receiving a warning conditional on no crisis occurring 
      lambda<-rbind(roc_results_th[[m]][[4]]) # Thresholds =Lambda_scaled
      use_data<- cbind(T1, T2)
      use_data<- cbind(use_data, lambda)
      names(use_data)<- c('T1', 'T2', 'Lambda_scaled')
      u<-0.80 #@Select [0,1] policy maker preference between missing crises u and issuing false alarms 1-u. When u=0.5 > A cost-ignorant policymaker assumes the cost of missing a crisis and issuing a false alarm to be. While we show here that balanced preferences in the AD framework actually corresponded to a policymaker with u= 0.8 , an in-depth discussion of optimal preferences is, however, out of the scope of this paper, such as the political economy aspects of maximization of a policymaker's utility vs. social welfare
      
      loss_f<- u*use_data$T1*p1+(1-u)*use_data$T2*p2
      use_data<- cbind(use_data,loss_f)
      min_loss<- min(loss_f, na.rm = T)
      lambda_opt_position<- which(use_data$loss_f==min_loss)
      optimal_values<- use_data[lambda_opt_position, ]
      optimal_values<- optimal_values[which(optimal_values$Lambda_scaled==min(optimal_values$Lambda_scaled)),]#when more than 1 threshold gives the same loss function, we select the minimum
      optimal_values$abs_useful<- unique(min(c(u*p1, (1-u)*p2))-optimal_values$loss_f)
      optimal_values$rel_useful<- optimal_values$abs_useful/min(u*p1,p2*(1-u))
      optimal_values$Lambda<- (optimal_values$Lambda_scaled*max_scaling)+min_scaling #not scaled
     
      lambda_scaled_F1<- thresholds[m,3]#Optimal threshold
      lambda_F1_position<-which(use_data[, "Lambda_scaled"]==lambda_scaled_F1)
      loss_f_optimal_treshold_F1= u*use_data[lambda_F1_position,]$T1*p1+(1-u)*use_data[lambda_F1_position,]$T2*p2 #loss function for the optimal threshold found with F1 measure
      optimal_values_F1<- use_data[lambda_F1_position,]
      optimal_values_F1$abs_useful<- unique(min(c(u*p1, (1-u)*p2))-loss_f_optimal_treshold_F1)
      optimal_values_F1$Lambda<- (optimal_values_F1$Lambda_scaled*max_scaling)+min_scaling #not scaled
      optimal_values_F1$rel_useful<- optimal_values_F1$abs_useful/min(u*p1,p2*(1-u))
      
      results<- rbind(optimal_values, optimal_values_F1)
      
      results$model<- c(indicator[m], paste0(indicator[m], '_F1'))
      results<- results %>% dplyr::select(model, T1, T2, loss_f, abs_useful, rel_useful, Lambda, Lambda_scaled)
      
      return(results)}
    
    useful<-lapply(list_models, useful_f)
    useful<-Reduce(rbind, useful)
  
    #All results-------------------
    auc<- unlist(lapply(list_models, function(m){roc_results[[m]][[3]]})) 
    results<- list(indicator=indicator, auc=auc, thresholds=thresholds)
    
    results<- as.data.frame(as.tibble(results)) %>% 
      unnest(thresholds) %>% 
      as.data.frame() %>% 
      rename(Threshold_F1="T(F1)", Threshold_F1_scaled="T(F1)scaled") %>% 
      dplyr::select("indicator", "auc", "Threshold_F1", "Threshold_F1_scaled")%>% 
      mutate(F1_max=F1_max) 
    
    results2 <- useful %>% filter(model%in%indicator) 
    results<- merge.data.frame(results, results2, by.x = "indicator", by.y="model")
    
    results<- results %>% filter(indicator!="Benchmark_GNI")
    
    setwd(path_auc)
    if(oneside_estimate==F){
    openxlsx:: addWorksheet(wb,all_crisis_names[c])
    }else{}
    openxlsx::writeData(wb, sheet = all_crisis_names[c], data.frame(results))
    openxlsx::saveWorkbook(wb, "results_metrics.xlsx", overwrite = TRUE)
    setwd(path)
    
    return(results)
  }
  list_crisis<- lapply(c(1:6), function(s)s)
  lapply(list_crisis,roc_crisis)
  
  results<-list()
  for (c in 1:6) {
    results[[c]]<-openxlsx::read.xlsx(paste0(path_auc, "/results_metrics.xlsx"), sheet =all_crisis_names[c])
    results[[c]]$crisis<-rep(all_crisis_names[c],length(results[[c]]$indicator))}
  results<- Reduce(rbind, results)
  
  all_thresholds_crisis_f<- function(m){
    all_thresholds_crisis<- results[which(results$indicator==paste0(indicator[m])),]
    all_thresholds_crisis<- all_thresholds_crisis %>%  
      dplyr::select(Lambda_scaled,Lambda, crisis)
    Babecky<- all_thresholds_crisis %>% 
      filter(crisis=="Babecky") %>% 
      dplyr::select(Lambda_scaled)
    sd_all<-all_thresholds_crisis %>% 
      filter(crisis=="Babecky"| crisis== "Bank_crisis_1") %>% 
      dplyr::select(Lambda_scaled, Lambda) %>% 
      as.vector()
    sd_all_scaled<- sd(sd_all$Lambda_scaled, na.rm = T)
    sd_all<- sd(sd_all$Lambda, na.rm = T)
    
    Bank_crisis_12<-all_thresholds_crisis %>% 
      filter(crisis==c("Bank_crisis_1","Bank_crisis_2")) %>% 
      dplyr::select(Lambda_scaled, Lambda)
    sd_all2_scaled<- sd(Bank_crisis_12$Lambda_scaled)
    sd_all2<- sd(Bank_crisis_12$Lambda_scaled)
    
    sd_all_scaled<- sd_all2_scaled
    sd_all<- sd_all2
    
    up_b_th<- min(Babecky+ sd_all,1)
    low_b_th<- max(Babecky- sd_all,0)
    all_thresholds_crisis<- c(Babecky, low_b_th, up_b_th,sd_all_scaled,sd_all)
      return(all_thresholds_crisis)}
  
  list_indicator<- lapply(c(1:length(indicator)), FUN = function(s)s)
  all_thresholds_crisis<- lapply(list_indicator,all_thresholds_crisis_f)
  
  res_F<- function(m){
    results<- results %>% 
    rename(indicator_=indicator) %>% 
    filter(indicator_==indicator[m]) %>% 
    mutate(min_th_scaled=Lambda_scaled+all_thresholds_crisis[[m]][[4]]) %>% 
    mutate(max_th_scaled=Lambda_scaled-all_thresholds_crisis[[m]][[4]]) %>% 
    mutate(min_th=Lambda+all_thresholds_crisis[[m]][[5]]) %>% 
    mutate(max_th=Lambda-all_thresholds_crisis[[m]][[5]])
  return(results)}
  res_<- lapply(list_indicator, res_F)
  results<-  Reduce(rbind, res_) %>% 
    dplyr::select(crisis, indicator_, auc, F1_max, rel_useful,Lambda_scaled, min_th_scaled,	max_th_scaled, Lambda, min_th,	max_th, T1 ,  T2, loss_f, abs_useful)
  
  if (oneside_estimate==T){
  write.xlsx(results,paste0(path_auc, "/results_metrics.xlsx"))
  }else{}
  
  if(oneside_estimate==T){
  }else{
    results_twoside<- results}

  #Plot metrics-------------
plot_metric_data<- results %>% 
  filter(crisis=='Babecky') %>% 
  filter(indicator_!='Benchmark_GNI') %>% 
  dplyr::select(indicator_, auc, F1_max,rel_useful) %>% 
  rename(indicator=indicator_, F1=F1_max) %>% 
  pivot_longer(cols =! "indicator",names_to = 'metric') %>% 
  mutate(indicator = factor(indicator, levels = c(
    "Benchmark_comp",
    "Benchmark_RPP",
    "Benchmark_NC",
    "CF_NC",
    'CF_RPP',
    "HP_NC",
    "HP_RPP",
    "OV_NC",
    "VEC_GM_NC",
    "VEC_cust_NC"))) %>% 
  filter(indicator!='VEC_GM_NC')

plot_metric<- ggplot(data = plot_metric_data, aes(x = indicator, y = value, fill = factor(metric))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = 0.8) +
  guides(fill=guide_legend(title=""))+
  theme_classic() +
  labs(x = "",y = "metric", title = "")+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 40),
        axis.text=element_text(size=40),
        axis.title=element_text(size=40),
        text=element_text(size=40),
        legend.text =element_text(size=40))+
  scale_fill_manual(values = c("#0B5471", "#5EC5C2", "#D2E288"),name='')+
  geom_text(
    # data = subset(plot_metric_data, indicator == "Benchmark_NC"),
    aes(label = round(value, 2)),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 10
  )
ggsave(paste0(path_plots,"/Early_warning/metrics.pdf"), plot_metric, height = 20, width = 25)

order_metrics<-c("Benchmark_NC","Benchmark_RPP","Benchmark_comp","HP_NC","HP_RPP", "CF_NC","CF_RPP", "OV_NC","VEC_cust_NC") #@select ORDER
metricstab<- plot_metric_data %>% 
  pivot_wider(names_from =  metric, values_from = value) %>% 
  mutate(across(where(is.numeric),~round(.x,4))) %>% 
  # mutate(across(where(is.numeric),~trunc(.x,4))) %>% 
  arrange(match(as.character(indicator), order_metrics))

openxlsx::write.xlsx(metricstab, file = paste0(base_path,"/D_Results/Tables/metrics_summary.xlsx"))
}

#Plots Cycles: indicator, thresholds and pre crisis period #This plot is based on the two sided estimates, thresholds too (results_twoside)
data_roc<- merge(data_crisis[[2]], data_cycles, by='Date')#pre crisis periods
plot_f<- function(m){
  
  thresholds_plot<-results_twoside %>% 
    filter(crisis=="Babecky") %>% 
    filter(indicator_==indicator[m]) %>% 
    dplyr::select(Lambda_scaled, min_th_scaled,max_th_scaled)

  last_date<- last(data_roc[,1])
  start_date<- first(data_roc[,1])
  
  plot_crisi_cycles<-data_roc %>% 
    dplyr:: select(Date, Crisis, Pre_crisis,indicator_prob[m]) %>% #correct Pre_crisis
    mutate(Date= as.yearqtr(Date)) %>% 
    pivot_longer(cols=c(Crisis, Pre_crisis,indicator_prob[m]) ,names_to = "variable", values_to = "value") %>% 
    ggplot()+ 
    geom_line(aes(x=Date, y=value, colour = variable, linetype=variable), linewidth = 1.5)+
    theme_classic() +
    labs(x = "Quarter",y = "scaled cycle (0-1)", title = indicator[m])+
    scale_color_manual(values = cbi_palette[c(12,10,8)],breaks = c("Crisis", "Pre_crisis",indicator_prob[m]) ,name='', labels =c("Crisis", "Pre_crisis",paste0(indicator[m], "_cycle")))+
    scale_linetype_manual(values=c("solid", "dotdash", "solid"),breaks = c("Crisis", "Pre_crisis",indicator_prob[m]), name='', guide = "none")+
    # geom_hline(yintercept =thresholds$`T(F1)scaled`[m], colour = cbi_palette[12], linetype='dotted', linewidth = 1.5)+
    geom_hline(yintercept =thresholds_plot$Lambda_scaled, colour = '#0083A0', linetype='dotted', linewidth = 1.5)+
    geom_hline(yintercept =thresholds_plot$min_th_scaled, colour = '#FCAF17', linetype='dotted', linewidth = 1.5)+
    geom_hline(yintercept =thresholds_plot$max_th_scaled, colour = '#FCAF17', linetype='dotted', linewidth = 1.5)+
    labs(caption = "Middle dotted line: optimal threshold targeting pre crisis period, bounderies: +/-1 standard deviation among all the optimal thresholds targeting the pre crisis period and the pre bank in distress periods.")+
    scale_x_yearqtr(format = "%YQ%q", breaks=rev(seq(as.yearqtr(last_date),as.yearqtr(start_date), by=-12)))+
    guides(fill=guide_legend(title=""))+
    theme(legend.position = "bottom",
          plot.title = element_text(size = 40),
          axis.text=element_text(size=40),
          axis.title=element_text(size=40),
          text=element_text(size=40),
          legend.text =element_text(size=40))
  ggsave(paste0(path_plots,"/Early_warning/cycle_",all_crisis_names[2],indicator[m],".pdf"), plot_crisi_cycles, height = 20, width = 25)
  }
lapply(list_indicator,plot_f)

#3-Pseudo Real time estimates: New data points (nd)-----------------------------
#Data frames ----- 
limits_recursive_master<-100:198 #@Select according to the A.Main.Code/Master_Recursive.m
name_files<- c('GNI', 'NC', 'RPP')
files <- sprintf(paste0(base_path,"/A_Main_Code/Outcome/Results_n%d.xlsx"),limits_recursive_master )
cn <- sub("\\.xlsx$", "", files)

first_date<- read.csv(paste0(base_path,"/A_Main_Code/data_model.csv"), header = F)[1,1]
make_dates <- function(first_date, n){
  p <- strsplit(first_date, "q")[[1]]
  y <- as.integer(p[1]); q <- as.integer(p[2])
  out <- character(n)
  for(i in 1:n){
    out[i] <- sprintf("%dq%d", y, q)
    q <- q + 1
    if(q == 5){ q <- 1; y <- y + 1 }}
  out}
dates_recursive <- make_dates(first_date, 198)[limits_recursive_master]

to_date <- function(qstr){                     
  p <- strsplit(qstr, "q")[[1]]                 
  y <- as.integer(p[1]); q <- as.integer(p[2])  
  m <- c("01","04","07","10")[q]               
  as.Date(paste0(y,"-",m,"-01"))                
}                                               
dates_all_chr  <- make_dates(first_date, max(limits_recursive_master))       
dates_all_date <- as.Date(sapply(dates_all_chr, to_date))                      

read_col <- function(k){
  lst <- lapply(files, function(f) read_excel(f, col_types = "guess")[[k]])
  names(lst) <- cn
  m <- max(lengths(lst))
  df<- as.data.frame(lapply(lst, function(v){ length(v) <- m; v }), check.names = FALSE)
  df <- cbind(Date = dates_all_date[seq_len(nrow(df))], df)  
  colnames(df) <- c("Date", dates_recursive)                  
  return(df)
}
recursivedata<-list(read_col(7),read_col(8),read_col(9))

name_plot<- c('business cycle','credit cycle', 'house prices cycle')
ind<-c(which(indicator==c("Benchmark_GNI")),which(indicator==c("Benchmark_NC")),which(indicator==c("Benchmark_RPP")))
new_data_plot_f<- function(c){#c=1

  merged_df<- recursivedata[[c]] %>% 
    mutate(Date=as.Date(Date))
  
  dates_chr <- make_dates(first_date, limits_recursive_master %>% last(.))[first(limits_recursive_master):last(limits_recursive_master)]
  date_seq  <- as.Date(as.yearqtr(sub("q"," Q", dates_chr), format = "%Y Q%q"))
  date_names <- as.character(as.yearqtr(date_seq, format = "%Y Q%q"))
  
  colnames(merged_df) <- c("Date", date_names)
  
  breaks_ <- as.yearqtr(
    rev(seq(max(date_seq, na.rm = TRUE),
            min(merged_df$Date, na.rm = TRUE),
            by = -2500)), 
    format = '%Y Q%q'
  )
  
  # date_names<-as.character(as.yearqtr(seq.Date(as.Date("2000-07-01"), as.Date("2024-10-01"), by = '1 quarter'), format = '%Y Q%q'))
  # colnames(merged_df)<-c('Date',date_names)
  # breaks_<-as.yearqtr(rev(seq(as.Date("2024-10-01"),as.Date("1978-01-01"), by=-2500)), format = '%Y Q%q')

  if(indicator[ind][c]=="Benchmark_GNI"){
    min_scaling<-min(data_ew[,indicator[ind][c]], na.rm = T)
    max_scaling<-max(data_ew[,indicator[ind][c]]-min_scaling, na.rm = T)
    benchmark_threshold=0
    benchmark_threshold_lb=0
    benchmark_threshold_ub=0
  }else{
    min_scaling<-min(data_ew[,indicator[ind][c]], na.rm = T)
    max_scaling<-max(data_ew[,indicator[ind][c]]-min_scaling, na.rm = T)
    benchmark_threshold<-  (all_thresholds_crisis[[ind[c]]][[1]]*max_scaling)+min_scaling
    benchmark_threshold_lb<-(all_thresholds_crisis[[ind[c]]][[2]]*max_scaling)+min_scaling
    benchmark_threshold_ub<-(all_thresholds_crisis[[ind[c]]][[3]]*max_scaling)+min_scaling}

  max_V<- max(merged_df[, -1], na.rm = T)
  min_V<- min(merged_df[, -1], na.rm = T)
  pre_crisis<- data_crisis[[2]] %>% dplyr::select(c('Date', "Pre_crisis")) %>% 
    mutate(Pre_crisis=ifelse(Pre_crisis==1, max_V,min_V))
  gfc_crisis<- data_crisis[[2]] %>% dplyr::select(c('Date', "Crisis")) %>% 
    filter(Date>='2007-01-01') %>% 
    filter(Crisis==1) %>% 
    first() %>% 
    dplyr::select(Date) %>% 
    mutate(Date=as.yearqtr(as.Date(Date), format = '%Y Q%q'))
  
  merged_df<- merge.data.frame(pre_crisis, merged_df, by='Date', all = F)

  merged_df<-merged_df %>%
    mutate(Date= as.yearqtr(Date)) %>% 
    pivot_longer(cols=date_names ,names_to = "variable", values_to = "value")
  
  plot_<- merged_df %>% 
    ggplot()+ 
    geom_line(aes(x=Date, y=value, colour = variable))+
    geom_line(aes(x=Date, y=Pre_crisis, colour = "Pre_crisis"))+
    theme_classic() +
    labs(x = "Quarter",y = "log", title = "")+
    scale_color_manual(values = c('grey', colorRampPalette(c("#F57D20", "#000000"))(length(date_names))), name='',
                       breaks = c('Pre_crisis', "2024 Q3"), labels = c("Pre_crisis",name_plot[c]))+
    scale_x_yearqtr(format = "%YQ%q", breaks=breaks_)+
    guides(fill=guide_legend(title=""))+
    geom_hline(yintercept = benchmark_threshold, colour =  '#0083A0', linetype='dotted', linewidth = 1.5)+#AMEND THREHSOLD
    geom_hline(yintercept = benchmark_threshold_lb, colour ='#FCAF17', linetype='dotted', linewidth = 1.5)+
    geom_hline(yintercept = benchmark_threshold_ub, colour = '#FCAF17', linetype='dotted', linewidth = 1.5)+
    geom_vline(xintercept =as.yearqtr(gfc_crisis, format = '%Y Q%q') , colour = cbi_palette[7], linetype='dashed', linewidth = 1.1)+
    theme(legend.position = "bottom",
          plot.title = element_text(size = 40),
          axis.text=element_text(size=40),
          axis.title=element_text(size=40),
          text=element_text(size=40),
          legend.text =element_text(size=40))+
    annotate("rect", xmin = date_names[1], xmax = last(date_names),
             ymin=-Inf, ymax=Inf, fill='gray', alpha=0.8)

  ggsave(paste0(path_plots,"/Early_warning/pseudo_rt_",name_files[c], ".pdf"), plot_, height = 20, width = 25)
  }
list_vars<- lapply(c(1:3), FUN = function(s)s)
lapply(list_vars,new_data_plot_f)

#Plot composite indicator
data_roc<- merge(data_crisis[[2]], data_cycles, by='Date')#pre crisis periods
thresholds_plot<-results %>% 
  filter(crisis=="Babecky") %>% 
  filter(indicator_%in% c("Benchmark_RPP","Benchmark_NC")) %>% 
  dplyr::select(Lambda_scaled, min_th_scaled,max_th_scaled)
thresholds_plot_<-c(mean(thresholds_plot$Lambda_scaled), min(thresholds_plot$min_th_scaled), max(thresholds_plot$max_th_scaled))
thresholds_plot_<- as.data.frame(t(thresholds_plot_))
colnames(thresholds_plot_)<-colnames(thresholds_plot)
thresholds_plot<-thresholds_plot_

last_date<- last(data_roc[,1])
start_date<- first(data_roc[,1])

plot_crisis_cycles<-data_roc %>% 
  dplyr:: select(Date, Crisis, Pre_crisis,'Benchmark_comp') %>% #correct Pre_crisis
  mutate(Date= as.yearqtr(Date)) %>% 
  #mutate(Crisis= (Crisis*max(Benchmark_comp, na.rm=T))+min(Benchmark_comp, na.rm=T)) %>% 
  #mutate(Pre_crisis= (Pre_crisis*max(Benchmark_comp, na.rm=T))+min(Benchmark_comp, na.rm=T)) %>% 
  mutate(Crisis= ifelse(Crisis==1, max(Benchmark_comp, na.rm=T), min(Benchmark_comp, na.rm=T))) %>% 
  mutate(Pre_crisis= ifelse(Pre_crisis==1, max(Benchmark_comp, na.rm=T), min(Benchmark_comp, na.rm=T))) %>%  
  pivot_longer(cols=c(Crisis, Pre_crisis,'Benchmark_comp') ,names_to = "variable", values_to = "value") %>% 
  ggplot()+ 
  geom_line(aes(x=Date, y=value, colour = variable, linetype=variable), linewidth = 1.5)+
  theme_classic() +
  labs(x = "Quarter",y = "scaled cycle (0-1)", title = 'Benchmark_comp')+
  scale_color_manual(values = cbi_palette[c(12,10,8)],breaks = c("Crisis", "Pre_crisis",'Benchmark_comp') ,name='', labels =c("Crisis", "Pre_crisis",paste0('Benchmark_comp', "_cycle")))+
  scale_linetype_manual(values=c("solid", "dotdash", "solid"),breaks = c("Crisis", "Pre_crisis",'Benchmark_comp'), name='', guide = "none")+
  # geom_hline(yintercept =thresholds$`T(F1)scaled`[m], colour = cbi_palette[12], linetype='dotted', linewidth = 1.5)+
  geom_hline(yintercept =thresholds_plot$Lambda_scaled, colour = '#0083A0', linetype='dotted', linewidth = 1.5)+
  geom_hline(yintercept =thresholds_plot$min_th_scaled, colour = '#FCAF17', linetype='dotted', linewidth = 1.5)+
  geom_hline(yintercept =thresholds_plot$max_th_scaled, colour = '#FCAF17', linetype='dotted', linewidth = 1.5)+
  scale_x_yearqtr(format = "%YQ%q", breaks=rev(seq(as.yearqtr(last_date),as.yearqtr(start_date), by=-12)))+
  guides(fill=guide_legend(title=""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 40),
        axis.text=element_text(size=40),
        axis.title=element_text(size=40),
        text=element_text(size=40),
        legend.text =element_text(size=40))
ggsave(paste0(path_plots,"/Early_warning/",all_crisis_names[2],"_Benchmark_comp.pdf"), plot_crisis_cycles, height = 20, width = 25)

