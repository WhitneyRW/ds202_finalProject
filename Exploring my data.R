#Exploring the data

library(tidyverse)
library(ggplot2)
library(lubridate)
library(maps)


#reading in excel sheets
unitC <- readxl::read_xls("unitConversions.xls")
district <- readxl::read_xls("district.xls")
energy <- readxl::read_xls("energy.xls")
facilityB<- readxl::read_xls("facilityBuild.xls")


#cleaning data
energy <- energy %>% select("BLDGNUM","FYR","FMONTH", "KWHRAMT", "KWDMDAMT","STEAMAMT", "GASAMT", "OILAMT", "COALAMT",
                            "KWHRCOST", "KWDMDCOST", "STEAMCOST", "GASCOST", "OILCOST", "COALCOST") %>% 
  mutate(across(FYR:COALCOST, as.numeric))
  
facilityB <- facilityB %>% select("BLDGNUM", "REGNNUM", "GROSSSQFT") %>% mutate(across(REGNNUM:GROSSSQFT, as.numeric))


#exploring
JoinDat <- energy %>% left_join(facilityB, by = "BLDGNUM") %>% mutate(REGNNUM= as.factor(REGNNUM))

qftRegion <- JoinDat %>% group_by(REGNNUM, FYR) %>% 
  summarize(totalQft = sum(GROSSSQFT)) %>% 
  filter(!is.na(totalQft)) %>% 
  arrange(-totalQft)

qftRegNoYr <- facilityB %>% group_by(REGNNUM) %>% 
  summarize(totalQft = sum(GROSSSQFT)) %>% 
  filter(!is.na(totalQft)) 

#What region uses the most energy over the years?

mEnergy <- JoinDat %>% group_by(REGNNUM,FYR) %>% 
  summarize(totalKWH = sum(KWHRAMT)) %>% 
  filter(!is.na(REGNNUM) & !(REGNNUM == 5))

  ggplot(mEnergy, aes(x=FYR, weight=totalKWH, fill=FYR)) + geom_bar() + facet_wrap(~REGNNUM) + 
    ylab("Kilowatt Hours") + xlab("Year")
  
#What region used gas most in total?
  mGasEnergy <- JoinDat %>% group_by(REGNNUM,FYR) %>% 
    summarize(totalGas = sum(GASAMT)) %>% 
    filter((!is.na(REGNNUM)) & !(REGNNUM == 5) & (!is.na(totalGas)))
  
  ggplot(mGasEnergy, aes(x=REGNNUM, weight=totalGas, fill=REGNNUM)) + geom_bar() + 
    ylab("GAS gallon total") + xlab("Region")
  
#What region used oil most?
  mOilEnergy <- JoinDat %>% group_by(REGNNUM,FYR) %>%      
    summarize(totalOil = sum(OILAMT)) %>% 
    filter(!is.na(REGNNUM) & !is.na(totalOil) & !(REGNNUM == 5))
  
    ggplot(mOilEnergy, aes(x=REGNNUM, weight=totalOil, fill = REGNNUM)) + geom_bar() +
        ylab("Oil gallon total") + xlab("Region")
  
  
  
#What region spent the most on energy?
  mCostEnergy <- JoinDat %>% group_by(REGNNUM,FYR) %>% 
    summarize(totalMoney = sum(KWHRCOST)) %>% 
    filter(!is.na(REGNNUM) & !(REGNNUM == 5))
  
  ggplot(mCostEnergy, aes(x=FYR, y=totalMoney)) + geom_line() + facet_wrap(~REGNNUM) + 
    ylab("Money Spent on Kilowatt Hours") + xlab("Year")
  
  
  
         
         