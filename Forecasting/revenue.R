###########################################################################################
######################  Revenue Forecasting  ##############################################
###########################################################################################
##  Monthly Forecasting Model   ##
####
##  Created on : 24th Jan 2019  ##
####
####
###########################################################################################


#Clearing Objects from the Environment
rm(list = ls())
###########################################################################################
######################            Loading Packages          ###############################

library(dplyr)
library(timeSeries)
library(forecast)
library(Metrics)
library(ggplot2)
#library(prophet)
library(zoo)
library(bindrcpp)
library(xts)
library(lubridate)
library(vioplot)

###########################################################################################
################################   Data Base Connections          #########################
library(RJDBC) #required library

#Loading Divers
jcc = JDBC("com.ibm.db2.jcc.DB2Driver",
           "/home/rstudio/R_Scripts/Assets/db2jars/db2jcc.jar") 
              #Calling jar file
  
# Providing DB Credentials
conn = dbConnect(jcc,
                 "jdbc:db2://49.205.69.22:50000/GPROD",
                 user="tuser",
                 password="tuser@123") 

###########################################################################################
########################     Query to Retrieve Data   #####################################



###########################################################################################
##########################   Fetch Data from DataBase  ####################################

Query_Output<-dbSendQuery(conn,"query")

df<- fetch(Query_Output,-1)
dbDisconnect(conn)

write.xlsx(df,"revenue.xlsx")

library(readxl)
df <- read_excel("revenue.xlsx")

###########################################################################################
#######################   Data Definition/Validation Check  ##############################

summary(df)   #Data Summary
class(df)     # "data.frame"
str(df)       #Structure .i.e; Formats of Each Column

#'data.frame':	2284 obs. of  5 variables:
#$ TIME_ID: chr  "23-10-2012" "26-10-2012" "29-10-2012" "30-10-2012" ...
# $ MONTH  : num  10 10 10 10 10 11 11 11 11 11 ...
# $ YEAR   : num  2012 2012 2012 2012 2012 ...
# $ MYEAR  : chr


#Converting into Date Format

df$MYEAR<-as.Date(df$MYEAR)

df2<-df[,4:5]

str(df2)

# 'data.frame':	2284 obs. of  2 variables:
# $ MYEAR  : Date, format: "2012-10-01" ...
# $ REVENUE: num  7870 20480 12300 32110 6320 ...


###########################################################################################
##############################   Data Manipulation   ######################################

df3<-tbl_df(df2)

df3 %>%
  group_by(MYEAR) %>%
  summarise(revenue= sum(REVENUE,na.rm = TRUE)) 

# Generating Start and End Values of Years and Months
startYear<- as.numeric(format(min(df3$MYEAR),'%Y'))
endYear<- as.numeric(format(max(df3$MYEAR),'%Y'))

startMonth<- as.numeric(format(min(df3$MYEAR),'%m'))
endMonth<- as.numeric(format(max(df3$MYEAR),'%m'))

#Converting to Time Series

df2ts<- ts(df3[,2], start = c(startYear,startMonth), end = c(endYear,endMonth), frequency = 12)
###########################################################################################
############################### Plotting to Visaualize the Data ###########################
autoplot(df2ts) +
  ggtitle("Yealy Revenue Collected") +
  ylab("Revenue amount")

#### Decompose ####

dd<- decompose(df2ts, "multiplicative")
plot(dd)

boxplot(df2ts~cycle(df2ts))
###########################################################################################
##Test
forecast6<-auto.arima(df2ts)
plot(forecast(forecast6,level = 0.95,h=9))
lines(fitted(forecast6),col="red",lty=2)
lines(OutsampleTs,col="green",lty=2)
###########################################################################################
##############################    Forecasting Models    ###################################


Y <-diff(df2ts)  # Adding Differnce to bring Stationarity 

################################## Snaive Model  #########################################

fit<-snaive(diff(Y))      ## Residual Error SD = 3991407.5258
print(summary(fit))
checkresiduals(fit)
################################## ETS Model  #########################################


fit_ets<-ets(diff(Y))     ## Residual Error SD = 3875903
print(summary(fit_ets))
checkresiduals(fit_ets)

################################## ARIMA Model  #########################################
                          ## Residual Error SD =2695923  , 2283637 ,193649167 ,12116105
fit_arima<-auto.arima(df2ts,d=1,D=1,stepwise = FALSE ,approximation = FALSE ,trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)

###########################################################################################
########################### ARIMA Forecast Prediction #####################################
fcast<-forecast(fit_arima,h=12)
autoplot(fcast,include = 30)
print(summary(fcast))
###########################################################################################
###########################################################################################
















###########################################################################################
##############################   ****THE END ****   #######################################
###########################################################################################
