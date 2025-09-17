## By Dr. Farah Mugrabi
#Instructions: Follow @ to select options

## Load Libraries
library(dplyr)
library(lubridate)
library(DisaggregateTS)
library(zoo)
library(csodata)
library(ggplot2)
library(ecb)
library(openxlsx)
library(stringr)
library(forecast)

#Paths and folders
rm(list = ls())
path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
getwd()

base_path <- normalizePath(file.path(path, ".."), winslash = "/")
setwd(base_path)
getwd()
dir.create(file.path(base_path, "D_Results", "Plots", "Data"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(base_path, "D_Results", "Tables"), recursive = TRUE, showWarnings = FALSE)
setwd(path)
getwd()

#GNI----------------
#Yearly GNI
GNI<- cso_get_data('NA001', pivot_format = "tall", use_dates = TRUE, use_factors = FALSE, cache = FALSE) %>% 
    filter(Item=='10. Modified gross national income at current market prices')%>%
    filter(Statistic== 'Modified Gross National Income at Current Market Prices') %>% 
    dplyr::select(Year,value) %>% 
    rename(GNI=value) %>% 
    mutate(Year= as.Date(paste0(Year, '-10-01', "%Y"))) #raw data in EU MILLION

#Quarterly GNI PRE 1998-01-01
#Source: Central Bank of Ireland - Macro Financial Division (MFD) - gdp_Haver.xlsx - Sheet: GNIstar_adjustment - Column D: gnistar
GNI_pre_1998Q1<- readxl::read_xlsx(paste0(path,"/Raw_Data/", "Data_pre.xlsx"),sheet='Quarterly', range = "A1:E10000")  %>% 
  mutate(Date = as.Date(as.yearqtr(Date, format = "%Y-Q%q"))) %>% 
  mutate(GNI_pre_1998Q1 = GNI_pre_1998Q1*1000) %>% 
  dplyr::select(Date,GNI_pre_1998Q1) %>% 
  filter(Date<="1997-10-01")

#Quarterly Modified Total Domestic Demand
MDD<-cso_get_data('NAQ05', pivot_format = "tall", use_dates = TRUE, use_factors = FALSE, cache = FALSE) %>%  
  filter(Sector=='Modified Total Domestic Demand')%>%
  filter(Statistic== '\tModified Total Domestic Demand and Components of Modified Gross Domestic Fixed Capital Formation at Current Market Prices') %>% 
  dplyr::select(Year,value) %>% 
  rename(MDD=value)%>% 
  mutate( lag0=lag(MDD,0),
          lag1=lag(MDD,1),
          lag2=lag(MDD,2),
          lag3=lag(MDD,3)) %>% 
  na.omit() %>% 
  mutate(MDD_4qra=lag0+lag1+lag2+lag3) %>% 
  dplyr::select(Year,MDD,MDD_4qra) 

#GNI quarterly linear interpolation
data<- merge.data.frame(MDD,GNI, all.x = T, by= "Year") %>% 
  mutate(ratio=GNI/MDD_4qra) %>% 
  mutate(quarter=ifelse(month(Year)==1, 1,ifelse(month(Year)==4,2,ifelse(month(Year)==7,3,4)))) %>% 
  mutate(ratio_q1= lag(ratio,0)) %>%
  mutate(ratio_q2= lag(ratio,1)) %>%
  mutate(ratio_q3= lag(ratio,2)) %>%
  mutate(ratio_q4= lag(ratio,3)) %>%
  mutate(ratio1= coalesce(ratio_q1, ratio_q2, ratio_q3, ratio_q4)) %>% 
  dplyr::select(!c(ratio_q1, ratio_q2, ratio_q3, ratio_q4)) %>% 
  mutate(ratio1=ifelse(is.na(ratio1),last(ratio, na_rm = T), ratio1)) %>% 
  mutate(GNIq=ifelse(!is.na(GNI), GNI, ratio1*MDD_4qra)) %>% #GNI quarterly linear interpolation
  dplyr::select(!quarter) %>% 
  rename(Date=Year)

#Unemployment
unemployment<-  cso_get_data("MUM01", pivot_format = "tall", use_factors = FALSE, use_dates = TRUE) %>% 
  filter(Statistic=="Seasonally Adjusted Monthly Unemployment Rate", Age.Group == "15 - 74 years",Sex == "Both sexes") %>% 
  mutate(Date = as.Date(as.yearqtr(Month, format = "%Y-%m-%d"))) %>% 
  dplyr::select(Date,value) %>% group_by(Date) %>% summarise(value = mean(value)) %>% 
  rename(unemployment=value)
data<- merge.data.frame(data, unemployment, by = 'Date')

#Chow-Li interpolation
GNI_cl<-disaggregate(
                Y=as.matrix(na.omit(data[,'GNI'])),
                X =as.matrix(data[,c("MDD", 'unemployment')]),
                aggMat = "sum",
                aggRatio = 4,
                method = "Chow-Lin",
                Denton = "additive-first-diff")

data$GNI_cl<- as.vector(GNI_cl$y_Est)
data<- data %>%  mutate( lag0=lag(GNI_cl,0),
                         lag1=lag(GNI_cl,1),
                         lag2=lag(GNI_cl,2),
                         lag3=lag(GNI_cl,3)) %>%
        mutate(GNI_cl=lag0+lag1+lag2+lag3) 
data$GNI_cl<- rowSums(data[c("lag0", "lag1", "lag2", "lag3")], na.rm = T)
data<- data %>% dplyr::select(!c(lag0, lag1, lag2, lag3))
data[1,'GNI_cl']<- data[1,'GNIq']
data[2,'GNI_cl']<- data[2,'GNIq']
data[3,'GNI_cl']<- data[3,'GNIq']

#Merge GNI pre 1998 and post estimations
data<- bind_rows(data,GNI_pre_1998Q1) %>% arrange(Date) %>% 
  mutate(GNI_cl=ifelse(!is.na(GNI_cl),GNI_cl,GNI_pre_1998Q1),
         GNIq=ifelse(!is.na(GNIq),GNIq,GNI_pre_1998Q1)) %>% 
  dplyr::select(!GNI_pre_1998Q1)

#CPI Ireland all items--------------
CPI_all<- cso_get_data('CPM01', pivot_format = "tall", use_dates = TRUE, use_factors = FALSE, cache = FALSE) %>% 
  filter(Statistic=='Consumer Price Index (Base Dec 2016=100)')%>%
  filter(Commodity.Group== 'All items') %>% 
  rename(Date=Month) %>% 
  mutate(Date = as.Date(Date)) %>%
  dplyr::select(Date,value) %>% 
  rename(CPI_all=value) %>% 
  as.data.frame()

CPI_all_q<- CPI_all %>% 
  mutate(Date = as.Date(Date)) %>% 
  mutate(Date =as.Date(as.yearqtr(Date))) %>% 
  group_by(Date) %>%
  mutate(CPI_all_q=mean(CPI_all, na.rm = T)) %>% #in the original file the take the value of the last month of the quarter, here we take the average across the quarter 
  ungroup() %>% 
  dplyr::select(Date, CPI_all_q) %>% 
  distinct()%>% 
  as.data.frame() %>% 
  mutate(CPI= CPI_all_q/last(CPI_all_q))# rebased to 100 LAST quarter, so variables are expressed in contemporaneous values

data<-merge.data.frame(data, CPI_all_q, by='Date')

#Compute Q-O-Q change
data<- data %>% 
  mutate(GNI_cl_qoq=(GNI_cl/lag(GNI_cl)-1)) %>% 
  mutate(GNIq_qoq=(GNIq/lag(GNIq)-1)) %>% 
  mutate(GNI_lcl= log10(GNI_cl/1000)) %>% 
  mutate(GNI_lq= log10(GNIq/1000)) %>% 
  mutate(GNI_rlcl= log10((GNI_cl/CPI)/1000)) %>% 
  mutate(GNI_rlq= log10((GNIq/CPI)/1000)) %>% 
  mutate(GNI_rcl= (GNI_cl*CPI)) %>% 
  mutate(GNI_rq= (GNIq*CPI))

#Plots
setwd(paste0(path, "/../"))
getwd()
save_plots <- paste0(getwd(), '/D_Results/Plots')
setwd(path)

GNI_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=GNI_cl/1000, color='Chow-Li'), size=1.5)+
  geom_line(aes(y=GNIq/1000, color='Linear'), size=1.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac","#d8b365"))+
  ylab("GNI (Euro billion)")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))

GNI_r_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=GNI_rlcl, color='Chow-Li'), size=1.5)+
  geom_line(aes(y=GNI_rlq, color='Linear'), size=1.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac","#d8b365"))+
  ylab("GNI (real Euro billion)")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))

GNI_yoy_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=GNI_cl_qoq, color='Chow-Li'), size=1.5)+
  geom_line(aes(y=GNIq_qoq, color='Linear'), size=1.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac","#d8b365"))+
  ylab("GNI q-o-q growth (%)")+
  guides(color = guide_legend(title = "Interpolation"))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))

chowlinvslinear<- (sd(data$GNIq_qoq,na.rm = T)-sd(data$GNI_cl_qoq,na.rm = T))*100 #dispersion of linear interpolation is x percentage points above the one obtained with Chow-Li

ggsave(paste0(save_plots,"/Data/GNI.pdf"), GNI_plot, height = 20, width = 25)
ggsave(paste0(save_plots,"/Data/GNI_yoy.pdf"), GNI_yoy_plot, height = 20, width = 25)
ggsave(paste0(save_plots,"/Data/GNI_r.pdf"), GNI_r_plot, height = 20, width = 25)

#House prices-----------------------------
#Source: Central Bank of Ireland - Macro Financial Division (MFD) - Residential property price growth.xlsx -Column B - Combined HPI (index = 100 / 2015Q2) - lhs - Based on old data from CSO - Identifier: HPM09
rpp_price_pre_2005<- readxl::read_xlsx(paste0(path,"/Raw_data/", "Data_pre.xlsx"),sheet='Quarterly',range = "A1:E10000")  %>% 
  mutate(Date = as.Date(as.yearqtr(Date, format = "%Y-Q%q"))) %>% 
  rename(rpp_price_index=House_Prices_pre_2005) %>% 
  dplyr::select(Date,rpp_price_index) %>% 
  filter(Date<="2004-10-01")

rpp_price_index <- csodata::cso_get_data("HPM09", pivot_format = "tall", use_dates = TRUE, use_factors = FALSE, cache = FALSE) %>% 
  filter(Type.of.Residential.Property=='National - all residential properties', Statistic=='Residential Property Price Index') %>%  
  mutate(Date = as.Date(as.yearqtr(Month, format = "%Y-%m-%d"))) %>% 
  dplyr::select(Date,value) %>% group_by(Date) %>% summarise(value = mean(value)) %>% 
  rename(rpp_price_index=value) 

rpp_price_index<-rbind(rpp_price_pre_2005,rpp_price_index)
data<- merge.data.frame(data, rpp_price_index, by = 'Date') 
data<-data %>%  
  mutate(rpp_price_index_g=(rpp_price_index/lag(rpp_price_index,4)-1)*100) %>% 
  mutate(rpp_price_index_l=log10(rpp_price_index))

#Credit (Counterpart: Domestic and International Banks)---------------
HH_credit_total<- read.csv("https://data-api.ecb.europa.eu/service/data/QSA/Q.N.IE.W0.S1M.S1.N.L.LE.F4.T._Z.XDC._T.S.V.N._T?format=csvdata") %>% 
  rename(Date=TIME_PERIOD, HH_credit_total=OBS_VALUE) %>% #in millions
  dplyr::select(Date, HH_credit_total) %>% #
  mutate(Date = as.Date(as.yearqtr(Date, format = "%Y-Q%q"))) %>% 
  mutate(HH_credit_total=HH_credit_total/1000)%>%  #convert to billions 
  filter(Date>="2003-01-01") %>% 
  as.data.frame()
data_credit<-HH_credit_total

NFC_credit_total<- read.csv("https://data-api.ecb.europa.eu/service/data/QSA/Q.N.IE.W0.S11.S1.N.L.LE.F4.T._Z.XDC._T.S.V.N._T?format=csvdata") %>% 
  rename(Date=TIME_PERIOD, NFC_credit_total=OBS_VALUE) %>% #in millions %>% 
  dplyr::select(Date, NFC_credit_total) %>% 
  mutate(Date = as.Date(as.yearqtr(Date, format = "%Y-Q%q"))) %>% 
  mutate(NFC_credit_total=NFC_credit_total/1000) %>%#convert to billions 
  filter(Date>="2002-01-01")

data_credit<- merge(data_credit,NFC_credit_total, by = "Date", all = T)

#National Credit (provided by domestic banks)-------------
# NFC_credit_national<- read.csv("https://opendata.centralbank.ie/dataset/f178f1ef-6be3-4377-8441-433e3531c11f/resource/ea2149b9-3429-4f9f-ab4f-99803f32b537/download/money-and-banking-statistics.csv")
NFC_credit_national<- readr::read_csv("https://opendata.centralbank.ie/dataset/f178f1ef-6be3-4377-8441-433e3531c11f/resource/ea2149b9-3429-4f9f-ab4f-99803f32b537/download/money-and-banking-statistics.csv")
NFC_credit_national$Data.type<- NFC_credit_national$`Data Type`
NFC_credit_national$Reporting.Date<- NFC_credit_national$`Reporting Date`

NFC_credit_national<- NFC_credit_national %>% 
  filter(Data.type=="Outstanding Amount", Item=='Credit to Non Financial Corporates, Irish Resident, Total ') %>% 
  dplyr::select(Reporting.Date,Value) %>% 
  rename(NFC_credit_national=Value, Month=Reporting.Date) %>% 
  mutate(Month = as.Date(Month)) %>% 
  mutate(Date =as.Date(as.yearqtr(Month))) %>% 
  group_by(Date) %>% 
  mutate(NFC_credit_national=sum(NFC_credit_national)/4000) %>% #in the original file the take the value of the last month of the quarter, here we take the average across the quarter 
  ungroup() %>% 
  dplyr::select(Date, NFC_credit_national) %>% 
  distinct()

#Pre_2002 (National Credit) 
#Source: Central Bank of Ireland - Macro Financial Division (MFD) - credit_stock_new.xlsx - sheet: CBI - long-run -Columns C & D - based on old public data https://www.centralbank.ie/statistics/data-and-analysis/credit-and-banking-statistics/bank-balance-sheets/bank-balance-sheets-data
Credit_pre2002Q4<- readxl::read_xlsx(paste0(path,"/Raw_Data/", "Data_pre.xlsx"),sheet='Quarterly', range = "A1:E10000")  %>% 
  mutate(Date = as.Date(as.yearqtr(Date, format = "%Y-Q%q"))) %>% 
  rename(NFC_credit_national=NFC_2002Q4) %>% 
  rename(HH_credit_total=HH_2002Q4) %>%
  dplyr::select(Date,NFC_credit_national,HH_credit_total) %>% 
  filter(Date<="2002-10-01")

NFC_credit_national<- rbind(Credit_pre2002Q4[,c('Date', "NFC_credit_national")],NFC_credit_national)
HH_credit_total<- rbind(Credit_pre2002Q4[,c('Date', "HH_credit_total")],HH_credit_total)

data_credit<-data_credit %>% dplyr::select(!HH_credit_total)
data_credit<- merge(data_credit,HH_credit_total, by = 'Date', all = T)
data_credit<- merge(data_credit,NFC_credit_national, by = 'Date', all = T)

data_credit<- data_credit %>% 
  mutate(growth_NFC=lag(NFC_credit_national)/NFC_credit_national)

date_missing<- which(is.na(data_credit$NFC_credit_total)==FALSE)[1]
for (d in date_missing:2) {
  data_credit$NFC_credit_total[d-1]<- data_credit$NFC_credit_total[d]*data_credit$growth_NFC[d-1]}

data<-merge.data.frame(data, data_credit, by='Date')

#Transformations---------------- 
data<- data %>%
  mutate(rpp_price_index_r=rpp_price_index*CPI) %>% 
  mutate(Nat_cred= (HH_credit_total+NFC_credit_national)) %>% 
  mutate(Nat_cred_r= (Nat_cred*CPI)) %>% 
  mutate(Nat_cred_l= log(HH_credit_total+NFC_credit_national)) %>% 
  mutate(Nat_cred_rl= log(Nat_cred_r)) %>% 
  mutate(Nat_cred_ratio= (Nat_cred)/GNIq) %>% 
  mutate(Nat_cred_ratio_l= log10(Nat_cred_ratio)) %>% 
  mutate(Nat_cred_ratio_cl= (Nat_cred)/GNI_cl) %>% 
  mutate(Tota_cred= (HH_credit_total+NFC_credit_total)) %>% 
  mutate(Tota_cred_l= log10(Tota_cred)) %>% 
  mutate(Tot_cred_ratio= (Tota_cred)/GNIq) %>% 
  mutate(Tot_cred_ratio_cl= (Tota_cred)/GNI_cl) %>% 
  mutate(Nat_cred_l_sa = ifelse(!is.na(Nat_cred),log10(as.numeric(seasadj(stl(forecast::na.interp(ts(Nat_cred, frequency = 4)),s.window = "periodic", robust = TRUE)))),NA_real_)) %>%
  mutate(Nat_cred_sa = ifelse(!is.na(Nat_cred),as.numeric(seasadj(stl(forecast::na.interp(ts(Nat_cred, frequency = 4)),s.window = "periodic", robust = TRUE))), NA_real_ )) %>% 
  mutate(GNI_sa = ifelse(!is.na(GNI_cl), as.numeric(seasadj(stl(forecast::na.interp(ts(GNI_cl/1000, frequency = 4)),s.window = "periodic", robust = TRUE))),NA_real_)) %>%
  mutate(GNI_l_sa = ifelse(!is.na(GNI_cl),log10(as.numeric(seasadj(stl(forecast::na.interp(ts(GNI_cl/1000, frequency = 4)),s.window = "periodic", robust = TRUE)))),NA_real_)) %>%
  mutate(rpp_price_index_sa = ifelse(!is.na(rpp_price_index), as.numeric(seasadj(stl(forecast::na.interp(ts(rpp_price_index, frequency = 4)),s.window = "periodic", robust = TRUE))), NA_real_)) %>% 
  mutate(rpp_price_index_l_sa = ifelse(!is.na(rpp_price_index), log10(as.numeric(seasadj(stl(forecast::na.interp(ts(rpp_price_index, frequency = 4)), s.window = "periodic", robust = TRUE)))),NA_real_)) %>% 
  mutate(Nat_cred_rl_sa = ifelse(!is.na(Nat_cred_r), log10(as.numeric(seasadj(stl(forecast::na.interp(ts(Nat_cred_r, frequency = 4)),s.window = "periodic", robust = TRUE)))), NA_real_)) %>% 
  mutate(GNI_rl_sa = ifelse(!is.na(GNI_rcl),log10(as.numeric(seasadj(stl(forecast::na.interp(ts(GNI_rcl/1000, frequency = 4)),s.window = "periodic", robust = TRUE)))),NA_real_)) %>% 
  mutate(rpp_price_index_rl_sa = ifelse(!is.na(rpp_price_index_r),log10(as.numeric(seasadj(stl(forecast::na.interp(ts(rpp_price_index_r, frequency = 4)),s.window = "periodic", robust = TRUE)))),NA_real_))

data_model<-data %>% dplyr::select(Date, GNI_l_sa, Nat_cred_l_sa, rpp_price_index_l_sa) %>% 
  mutate(Date=str_to_lower(str_remove(as.yearqtr(as.Date(Date)), " "))) 
colnames(data_model)<- NULL

data_model_r<-data %>% dplyr::select(Date, GNI_rl_sa, Nat_cred_rl_sa, rpp_price_index_rl_sa) %>% 
  mutate(Date=str_to_lower(str_remove(as.yearqtr(as.Date(Date)), " "))) 
colnames(data_model_r)<- NULL

#Graph on NFC
setwd(paste0(path, "/../"))
getwd()
save_plots <- paste0(getwd(), '/D_Results/Plots')
setwd(path)

nfc_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=NFC_credit_national, color='National'), size=1.5)+
  geom_line(aes(y=NFC_credit_total, color='Total'), size=1.5) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac","#d8b365"))+
  ylab("NFC credit EU Billion")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))
ggsave(paste0(save_plots,"/Data/nfc_credit.pdf"), nfc_plot, height = 20, width = 25)

#Graph on Variables used in the model
setwd(paste0(path, "/../"))
getwd()
save_plots <- paste0(getwd(), '/D_Results/Plots')
setwd(path)

cred_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=Nat_cred_rl_sa, color='National Credit'), size=1.5)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac"))+
  ylab("National Credit real logs")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))
ggsave(paste0(save_plots,"/Data/cred_plot.pdf"), cred_plot, height = 20, width = 25)

rpp_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=rpp_price_index_rl_sa, color='House Prices'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac"))+
  ylab("House Prices real log")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))
ggsave(paste0(save_plots,"/Data/rpp_plot.pdf"), rpp_plot, height = 20, width = 25)

gni_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=GNI_rl_sa, color='GNI'), size=1.5)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac"))+
  ylab("GNI real log")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))
ggsave(paste0(save_plots,"/Data/gni_plot.pdf"), gni_plot, height = 20, width = 25)

#Export data
# write.csv(data_model, paste0(path, "/data_model_nominal.csv"), row.names = FALSE)#@Uncomment if nominal data needed
write.csv(data, paste0(path, "/data_full.csv"), row.names = FALSE)
write.csv(data_model_r, paste0(base_path, "/A_Main_Code/data_model.csv"), row.names = FALSE)

#HH loan interest rates in order to perform VECM model
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
loan_rates_old<- readxl::read_xlsx(paste0(base_path, "/B_Data/Raw_data/", "Data_pre.xlsx"), sheet = 'Monthly')[,c(1,2)] 
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
ggplot(HH_loan_rates, aes(x=Date, y=HH_loan_rates))+geom_line(size=1.5)+geom_vline(xintercept = as.Date("2003-02-01"))

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
  mutate(CPI= CPI_all/last(CPI_all))# re-based to 100 LAST quarter, so variables are expressed in contemporaneous values

HH_loan_rates<- merge.data.frame(HH_loan_rates, CPI_all, by='Date')
HH_loan_rates<- HH_loan_rates %>% 
  mutate(HH_loan_rates_r= HH_loan_rates-log(CPI))
write.xlsx(HH_loan_rates, file = paste0(base_path,'/B_Data/HH_loan_rates.xlsx'))

pHH_loan_rates_r<- ggplot(HH_loan_rates, aes(x=Date, y=HH_loan_rates_r))+
  geom_line(size=1.5)+
  geom_vline(xintercept = as.Date("2003-02-01"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac"))+
  ylab("HH real loan rates")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))

pHH_loan_rates<- ggplot(HH_loan_rates, aes(x=Date, y=HH_loan_rates))+
  geom_line(size=1.5)+
  geom_vline(xintercept = as.Date("2003-02-01"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac"))+
  ylab("HH loan rates")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 50),
        axis.text=element_text(size=50),
        axis.title=element_text(size=50),
        legend.text =element_text(size=50))
ggsave(paste0(base_path,"/D_Results/Plots/Data/","pHH_loan_rates_r.pdf"), pHH_loan_rates_r, height = 20, width = 25)
ggsave(paste0(base_path,"/D_Results/Plots/Data/", "pHH_loan_rates.pdf"), pHH_loan_rates, height = 20, width = 25)

HHq <- HH_loan_rates %>%
  mutate(yq = paste0(year(Date), "Q", quarter(Date))) %>%
  group_by(yq) %>%
  summarize(
    Date = max(Date),                        
    HH_loan_rates_r = mean(HH_loan_rates_r, na.rm = TRUE)) %>%
  ungroup()
data<-merge.data.frame(data, HHq, by = 'Date')
write.xlsx(HHq, file=paste0(path,"/HH_loan_rates.xlsx"), rowNames = FALSE)


#Descriptive statistics-------------------------------------
df_selected <- data %>%
  dplyr::select(Date, GNI_l_sa, Nat_cred_l_sa, rpp_price_index_l_sa,HH_loan_rates_r, CPI_all_q)

df_selected <- df_selected %>%
  mutate(CPI_all_q = (log(CPI_all_q) - log(lag(CPI_all_q, 4)))*100)%>%
  filter(!is.infinite(CPI_all_q))

stats_table <- data.frame(
  Name = c("GNI", "National credit", "House prices", "Household Loan Rates",'CPI'),
  Description = c(
    "logarithms, seasonally adjusted, real terms",
    "logarithms, seasonally adjusted, real terms",
    "logarithms, seasonally adjusted, real terms",
    "real terms",
    "yoy log change"),
  Mean = c(mean(df_selected$GNI_l_sa, na.rm = TRUE),
           mean(df_selected$Nat_cred_l_sa, na.rm = TRUE),
           mean(df_selected$rpp_price_index_l_sa, na.rm = TRUE),
           mean(df_selected$HH_loan_rates_r, na.rm = TRUE),
           mean(df_selected$CPI_all_q, na.rm = TRUE)),
  SD = c(sd(df_selected$GNI_l_sa, na.rm = TRUE),
         sd(df_selected$Nat_cred_l_sa, na.rm = TRUE),
         sd(df_selected$rpp_price_index_l_sa, na.rm = TRUE),
         sd(df_selected$HH_loan_rates_r, na.rm = TRUE),
         sd(df_selected$CPI_all_q, na.rm = TRUE)),
  Max = c(max(df_selected$GNI_l_sa, na.rm = TRUE),
          max(df_selected$Nat_cred_l_sa, na.rm = TRUE),
          max(df_selected$rpp_price_index_l_sa, na.rm = TRUE),
          max(df_selected$HH_loan_rates_r, na.rm = TRUE),
          max(df_selected$CPI_all_q, na.rm = TRUE)),
  Min = c(min(df_selected$GNI_l_sa, na.rm = TRUE),
          min(df_selected$Nat_cred_l_sa, na.rm = TRUE),
          min(df_selected$rpp_price_index_l_sa, na.rm = TRUE),
          min(df_selected$HH_loan_rates_r, na.rm = TRUE),
          min(df_selected$CPI_all_q, na.rm = TRUE)),
  Period = paste0(format(min(df_selected$Date, na.rm = TRUE), "%Y-%m"),
                  " to ",
                  format(max(df_selected$Date, na.rm = TRUE), "%Y-%m")))

write.xlsx(stats_table, file=paste0(base_path,"/D_Results/Tables/descriptive_statistics.xlsx"), rowNames = FALSE)

#Data standard gap (BIS/ESRB 2014 recommendation)--------------
gdp = cso_get_data('NAQ03', pivot_format = "tall", use_dates = TRUE, use_factors = FALSE, cache = FALSE) %>% 
  filter(Statistic=='GDP at Current Market Prices (Seasonally Adjusted)')%>%
  rename(Date=Year) %>% 
  mutate(Date = as.Date(Date)) %>%
  dplyr::select(Date,value) %>% 
  rename(GDP=value) %>% 
  mutate(GDP = as.numeric(GDP)) %>%
  mutate(GDP_y= rollapply(data = GDP, width = 4, FUN =sum,align = "right", fill = NA,na.rm = TRUE)) %>% 
  mutate(GDP_y= GDP_y/1000) %>% 
  as.data.frame() %>% 
  dplyr::select(c("Date", 'GDP_y')) %>% 
  filter(Date>="1995-10-01") 

#Source: Central Bank of Ireland - Macro Financial Division (MFD) - gdp_Haver.xlsx - Sheet: GNIstar_adjustment - Column A: gdp_q
GNI_pre_1997<- readxl::read_xlsx(paste0(path,"/Raw_Data/", "Data_pre.xlsx"),sheet='Quarterly', range = "A1:F10000")  %>% 
  mutate(Date = as.Date(as.yearqtr(Date, format = "%Y-Q%q"))) %>% 
  mutate(GDP_y = GDP_pre_1997/400) %>% 
  dplyr::select(Date,GDP_y) %>% 
  filter(Date<="1995-07-01") %>% 
  as.data.frame()

gdp<- rbind(GNI_pre_1997[,c('Date', "GDP_y")],gdp)
data_standardg<- data %>% 
  dplyr::select("Date","NFC_credit_total", "HH_credit_total") %>% 
  merge.data.frame(.,gdp) %>% 
  mutate(Total_credit= NFC_credit_total+HH_credit_total) %>% 
  mutate(credit_to_gdp= Total_credit*100/GDP_y )

write.xlsx(data_standardg, file=paste0(path,"/data_standard_gap.xlsx"), rowNames = FALSE)


#Key takeaway for the paper:
cat("✅ Dispersion difference (pp), linear minus Chow–Lin interpolation:", sprintf("%.2f", chowlinvslinear), "\n",
    "Interpretation: > 0 ⇒ Chow–Lin has lower dispersion than linear; ",
    "< 0 ⇒ Chow–Lin has higher dispersion; = 0 ⇒ equal dispersion.\n", sep = "")
