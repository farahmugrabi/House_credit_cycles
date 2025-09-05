## By Dr. Farah Mugrabi
#Instructions: Follow @ to select options

#Please install the required R packages before running the code:  
#install.packages(c("dplyr","lubridate","DisaggregateTS","zoo","csodata","ggplot2","ecb","openxlsx","stringr","forecast"))

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
# library(RJDemetra) for X13 function/seasasonally adjusted, used in original paper, here replaced by seasadj from seasadj package

#Paths and folders
rm(list = ls())
path = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(path)
getwd()

base_path <- normalizePath(file.path(path, ".."), winslash = "/")
setwd(base_path)
getwd()
dir.create(file.path(base_path, "F_Results", "Plots", "Data"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(base_path, "F_Results", "Tables"), recursive = TRUE, showWarnings = FALSE)
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
GNI_pre_1998Q1<- readxl::read_xlsx(paste0(path,"/Raw_Data/", "Data_pre.xlsx"), range = "A1:E10000")  %>% 
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
  mutate(CPI= CPI_all_q/last(CPI_all_q))# rebased to 100 LAST quarter, so variables are expressed in contemporanous values

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
save_plots <- paste0(getwd(), '/F_Results/Plots')
setwd(path)

GNI_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=GNI_cl/1000, color='Chow-Li'))+
  geom_line(aes(y=GNIq/1000, color='Linear')) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac","#d8b365"))+
  ylab("GNI (Euro billion)")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 25),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text =element_text(size=25))

GNI_r_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=GNI_rlcl, color='Chow-Li'))+
  geom_line(aes(y=GNI_rlq, color='Linear')) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac","#d8b365"))+
  ylab("GNI (Euro billion)")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 25),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text =element_text(size=25))

GNI_yoy_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=GNI_cl_qoq, color='Chow-Li'))+
  geom_line(aes(y=GNIq_qoq, color='Linear')) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac","#d8b365"))+
  ylab("GNI q-o-q growth (%)")+
  guides(color = guide_legend(title = "Interpolation"))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 25),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text =element_text(size=25))

(sd(data$GNIq_qoq,na.rm = T)-sd(data$GNI_cl_qoq,na.rm = T))*100 #dispersion of linear interpolation is x percentage points above the one obtained with Chow-Li

ggsave(paste0(save_plots,"/Data/GNI.pdf"), GNI_plot, height = 20, width = 25)
ggsave(paste0(save_plots,"/Data/GNI_yoy.pdf"), GNI_yoy_plot, height = 20, width = 25)
ggsave(paste0(save_plots,"/Data/GNI_r.pdf"), GNI_r_plot, height = 20, width = 25)

#House prices-----------------------------
rre_price_pre_2005<- readxl::read_xlsx(paste0(path,"/Raw_data/", "Data_pre.xlsx"),range = "A1:E10000")  %>% 
  mutate(Date = as.Date(as.yearqtr(Date, format = "%Y-Q%q"))) %>% 
  rename(rre_price_index=House_Prices_pre_2005) %>% 
  dplyr::select(Date,rre_price_index) %>% 
  filter(Date<="2004-10-01")

rre_price_index <- csodata::cso_get_data("HPM09", pivot_format = "tall", use_dates = TRUE, use_factors = FALSE, cache = FALSE) %>% 
  filter(Type.of.Residential.Property=='National - all residential properties', Statistic=='Residential Property Price Index') %>%  
  mutate(Date = as.Date(as.yearqtr(Month, format = "%Y-%m-%d"))) %>% 
  dplyr::select(Date,value) %>% group_by(Date) %>% summarise(value = mean(value)) %>% 
  rename(rre_price_index=value) 

rre_price_index<-rbind(rre_price_pre_2005,rre_price_index)
data<- merge.data.frame(data, rre_price_index, by = 'Date') 
data<-data %>%  
  mutate(rre_price_index_g=(rre_price_index/lag(rre_price_index,4)-1)*100) %>% 
  mutate(rre_price_index_l=log10(rre_price_index))

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
NFC_credit_national<- read.csv("https://opendata.centralbank.ie/dataset/f178f1ef-6be3-4377-8441-433e3531c11f/resource/ea2149b9-3429-4f9f-ab4f-99803f32b537/download/money-and-banking-statistics.csv")

NFC_credit_national<- NFC_credit_national %>% 
  filter(Data.Type=="Outstanding Amount", Item=='Credit to Non Financial Corporates, Irish Resident, Total ') %>% 
  dplyr::select(Reporting.Date,Value) %>% 
  rename(NFC_credit_national=Value, Month=Reporting.Date) %>% 
  mutate(Month = as.Date(Month)) %>% 
  mutate(Date =as.Date(as.yearqtr(Month))) %>% 
  group_by(Date) %>% 
  mutate(NFC_credit_national=sum(NFC_credit_national)/4000) %>% #in the original file the take the value of the last month of the quarter, here we take the average across the quarter 
  ungroup() %>% 
  dplyr::select(Date, NFC_credit_national) %>% 
  distinct()

#Pre_2002 (National)
Credit_pre2002Q4<- readxl::read_xlsx(paste0(path,"/Raw_Data/", "Data_pre.xlsx"), range = "A1:E10000")  %>% 
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
  mutate(rre_price_index_r=rre_price_index*CPI) %>% 
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
  #mutate(Nat_cred_l_sa= ifelse(!is.na(Nat_cred), (log10(as.vector(x13(ts(Nat_cred, frequency = 4))$final[[1]][,"sa"]))),NA)) %>% 
  mutate(Nat_cred_sa = ifelse(!is.na(Nat_cred),as.numeric(seasadj(stl(forecast::na.interp(ts(Nat_cred, frequency = 4)),s.window = "periodic", robust = TRUE))), NA_real_ )) %>% 
  #mutate(Nat_cred_sa= ifelse(!is.na(Nat_cred), as.vector(x13(ts(Nat_cred, frequency = 4))$final[[1]][,"sa"]),NA)) %>% 
  mutate(GNI_sa = ifelse(!is.na(GNI_cl), as.numeric(seasadj(stl(forecast::na.interp(ts(GNI_cl/1000, frequency = 4)),s.window = "periodic", robust = TRUE))),NA_real_)) %>%
  #mutate(GNI_sa= ifelse(!is.na(GNI_cl), as.vector(x13(ts(GNI_cl/1000, frequency = 4))$final[[1]][,"sa"]), NA))%>% 
  mutate(GNI_l_sa = ifelse(!is.na(GNI_cl),log10(as.numeric(seasadj(stl(forecast::na.interp(ts(GNI_cl/1000, frequency = 4)),s.window = "periodic", robust = TRUE)))),NA_real_)) %>%
  #mutate(GNI_l_sa= ifelse(!is.na(GNI_cl), log10(as.vector(x13(ts(GNI_cl/1000, frequency = 4))$final[[1]][,"sa"])), NA))%>% 
  mutate(rre_price_index_sa = ifelse(!is.na(rre_price_index), as.numeric(seasadj(stl(forecast::na.interp(ts(rre_price_index, frequency = 4)),s.window = "periodic", robust = TRUE))), NA_real_)) %>% 
  #mutate(rre_price_index_sa= ifelse(!is.na(rre_price_index), as.vector(x13(ts(rre_price_index, frequency = 4))$final[[1]][,"sa"]), NA))%>% 
  mutate(rre_price_index_l_sa = ifelse(!is.na(rre_price_index), log10(as.numeric(seasadj(stl(forecast::na.interp(ts(rre_price_index, frequency = 4)), s.window = "periodic", robust = TRUE)))),NA_real_)) %>% 
  #mutate(rre_price_index_l_sa= ifelse(!is.na(rre_price_index), log10(as.vector(x13(ts(rre_price_index, frequency = 4))$final[[1]][,"sa"])),NA)) %>% 
  mutate(Nat_cred_rl_sa = ifelse(!is.na(Nat_cred_r), log10(as.numeric(seasadj(stl(forecast::na.interp(ts(Nat_cred_r, frequency = 4)),s.window = "periodic", robust = TRUE)))), NA_real_)) %>% 
  #mutate(Nat_cred_rl_sa= ifelse(!is.na(Nat_cred_r), (log10(as.vector(x13(ts(Nat_cred_r, frequency = 4))$final[[1]][,"sa"]))),NA)) %>% 
  mutate(GNI_rl_sa = ifelse(!is.na(GNI_rcl),log10(as.numeric(seasadj(stl(forecast::na.interp(ts(GNI_rcl/1000, frequency = 4)),s.window = "periodic", robust = TRUE)))),NA_real_)) %>% 
  #mutate(GNI_rl_sa= ifelse(!is.na(GNI_rcl/1000), log10(as.vector(x13(ts(GNI_rcl/1000, frequency = 4))$final[[1]][,"sa"])), NA))%>% 
  mutate(rre_price_index_rl_sa = ifelse(!is.na(rre_price_index_r),log10(as.numeric(seasadj(stl(forecast::na.interp(ts(rre_price_index_r, frequency = 4)),s.window = "periodic", robust = TRUE)))),NA_real_))
  #mutate(rre_price_index_rl_sa= ifelse(!is.na(rre_price_index_r), log10(as.vector(x13(ts(rre_price_index_r, frequency = 4))$final[[1]][,"sa"])),NA))
  
data_model<-data %>% dplyr::select(Date, GNI_l_sa, Nat_cred_l_sa, rre_price_index_l_sa) %>% 
  mutate(Date=str_to_lower(str_remove(as.yearqtr(as.Date(Date)), " "))) 
colnames(data_model)<- NULL

data_model_r<-data %>% dplyr::select(Date, GNI_rl_sa, Nat_cred_rl_sa, rre_price_index_rl_sa) %>% 
  mutate(Date=str_to_lower(str_remove(as.yearqtr(as.Date(Date)), " "))) 
colnames(data_model_r)<- NULL

#Graph on NFC
setwd(paste0(path, "/../"))
getwd()
save_plots <- paste0(getwd(), '/F_Results/Plots')
setwd(path)

nfc_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=NFC_credit_national, color='National'))+
  geom_line(aes(y=NFC_credit_total, color='Total')) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac","#d8b365"))+
  ylab("NFC credit EU Billion")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 25),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text =element_text(size=25))
ggsave(paste0(save_plots,"/Data/nfc_credit.pdf"), nfc_plot, height = 20, width = 25)

#Graph on VARIABLES to MODEL
setwd(paste0(path, "/../"))
getwd()
save_plots <- paste0(getwd(), '/F_Results/Plots')
setwd(path)

cred_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=Nat_cred_rl_sa, color='National Credit'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac"))+
  ylab("National Credit real logs")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 25),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text =element_text(size=25))
ggsave(paste0(save_plots,"/Data/cred_plot.pdf"), cred_plot, height = 20, width = 25)

hp_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=rre_price_index_rl_sa, color='House Prices'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac"))+
  ylab("House Prices real log")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 25),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text =element_text(size=25))
ggsave(paste0(save_plots,"/Data/hp_plot.pdf"), hp_plot, height = 20, width = 25)

gni_plot<- ggplot(data=data, aes(x=Date))+
  geom_line(aes(y=GNI_rl_sa, color='GNI'))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "grey"))+
  theme(legend.position="bottom",legend.title = element_text(size = 8)) +
  scale_color_manual(values = c( "#5ab4ac"))+
  ylab("GNI real log")+
  guides(color = guide_legend(title = ""))+
  theme(legend.position = "bottom",
        plot.title = element_text(size = 25),
        axis.text=element_text(size=25),
        axis.title=element_text(size=25),
        legend.text =element_text(size=25))
ggsave(paste0(save_plots,"/Data/gni_plot.pdf"), gni_plot, height = 20, width = 25)

#Export data
# write.csv(data_model, paste0(path, "/data_model_nominal.csv"), row.names = FALSE)
# write.csv(data, paste0(path, "/data_full.csv"), row.names = FALSE)
write.csv(data_model_r, paste0(path, "/data_model.csv"), row.names = FALSE)

#Descriptive statistics-------------------------------------
df_selected <- data %>%
  dplyr::select(Date, GNI_l_sa, Nat_cred_l_sa, rre_price_index_l_sa)
  
stats_table <- data.frame(
  Name = c("GNI", "National credit", "House prices"),
  Description = "seasonally adjusted real terms",
  Mean = c(mean(df_selected$GNI_l_sa, na.rm = TRUE),
           mean(df_selected$Nat_cred_l_sa, na.rm = TRUE),
           mean(df_selected$rre_price_index_l_sa, na.rm = TRUE)),
  SD = c(sd(df_selected$GNI_l_sa, na.rm = TRUE),
         sd(df_selected$Nat_cred_l_sa, na.rm = TRUE),
         sd(df_selected$rre_price_index_l_sa, na.rm = TRUE)),
  Max = c(max(df_selected$GNI_l_sa, na.rm = TRUE),
          max(df_selected$Nat_cred_l_sa, na.rm = TRUE),
          max(df_selected$rre_price_index_l_sa, na.rm = TRUE)),
  Min = c(min(df_selected$GNI_l_sa, na.rm = TRUE),
          min(df_selected$Nat_cred_l_sa, na.rm = TRUE),
          min(df_selected$rre_price_index_l_sa, na.rm = TRUE)),
  Period = paste0(format(min(df_selected$Date, na.rm = TRUE), "%Y-%m"),
                  " to ",
                  format(max(df_selected$Date, na.rm = TRUE), "%Y-%m")))

write.xlsx(stats_table, file=paste0(base_path,"/F_Results/Tables/descriptive_statistics.xlsx"), rowNames = FALSE)


