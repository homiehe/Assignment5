library(tidyverse)
library(stringr)
library(zoo)
library(shiny)
library(reshape2)
library(ggplot2)
install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")
library(easyGgplot2)


url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"
url1S <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46070h"

get_url <- function(i){
  urls <- str_c(url1, i, url2, sep = "")
  urls
}

get_filenames <- function(i){
  filenames <-  str_c("mr", i, sep = "")
  filenames
}

get_MR <- function(a,b) {
  N <- length(a)
  for (i in 1:N){
    suppressMessages(
      assign(b[i], read_table(a[i],col_names = T))
    )
    
    file <- get(b[i])
    
    colnames(file)[1] <- "YYYY"
    file$YYYY <- as.numeric(file$YYYY) 
    
    if(i == 1){
      MR <- file
    }else{
      MR <- rbind(MR, file)
    }
  }
  MR  
}

clean_mr <- function(i){
  a <- get_MR(get_url(i),get_filenames(i))
  b <- a%>% select(YYYY,MM,DD,hh,ATMP,WTMP)
  b$ATMP <- as.numeric(b$ATMP)
  b$WTMP <- as.numeric(b$WTMP)
  b$ATMP[b$ATMP>50] <- NA
  b$WTMP[b$WTMP>50] <- NA
  b
}

#1987-2011

for (i in c(1987:1998)){
  if(i == 1987){
    data1987_1998 <- clean_mr(i)
  }else{
    data1987_1998 <- rbind(data1987_1998, clean_mr(i))
  }
}

data1987_1998$YYYY <- paste("19",data1987_1998$YYYY,sep="")

for (i in c(1999:2011)){
  if(i == 1999){
    data1999_2011 <- clean_mr(i)
  }else{
    data1999_2011 <- rbind(data1999_2011, clean_mr(i))
  }
}


data1987_2011 <- rbind(data1987_1998,data1999_2011)

#2012,2013,2014 using buoy 46070
Sget_url <- function(i){
  urls <- str_c(url1S, i, url2, sep = "")
  urls
}

Sclean_mr <- function(i){
  a <- get_MR(Sget_url(i),get_filenames(i))
  b <- a%>% select(YYYY,MM,DD,hh,ATMP,WTMP)
  b$ATMP <- as.numeric(b$ATMP)
  b$WTMP <- as.numeric(b$WTMP)
  b$ATMP[b$ATMP>50] <- NA
  b$WTMP[b$WTMP>50] <- NA
  b
}

data2012 <- Sclean_mr(2012)[-1,]
data2013 <- Sclean_mr(2013)[-1,]
data2014 <- Sclean_mr(2014)[-1,]
data2012_2014 <- rbind(data2012,data2013,data2014)

#2015-2017

for (i in c(2015:2017)){
  if(i == 2015){
    data2015_2017 <- clean_mr(i)
  }else{
    data2015_2017 <- rbind(data2015_2017, clean_mr(i))
  }
}

#total data

data_total <- rbind(data1987_2011,data2012_2014,data2015_2017)

#clean the data 
data_total <-data_total %>% filter(!is.na(ATMP))
data_total <-data_total %>% filter(!is.na(WTMP))
data_total$ATMP<-as.numeric(data_total$ATMP)
data_total$WTMP<-as.numeric(data_total$WTMP)

#get the time of each data
data_total$Time <-strptime(with(data_total, paste(YYYY, MM, DD, hh, sep="-")), format="%Y-%m-%d-%H")
data_total$Time <- as.Date(data_total$Time)

#get the daily data
daily<-subset(data_total,hh==12)

#get yearly average data
for (i in 1987:2017){
  if(i==1987){
    avg <- data_total %>% filter(YYYY==i) %>% group_by(YYYY) %>% summarise(avg_atmp=mean(ATMP),avg_wtmp=mean(WTMP))
  }
  else{
    avg <- rbind(avg, data_total %>% filter(YYYY==i) %>% group_by(YYYY) %>% summarise(avg_atmp=mean(ATMP),avg_wtmp=mean(WTMP)))
  }
}

plot_daily <-ggplot(daily, aes(x = Time)) + 
  geom_line(aes(y = WTMP), colour="green", size = 0.5) + 
  geom_line(aes(y = ATMP), colour = "blue", size = 0.5) +
  ylab(label="Celsius degrees") + 
  xlab("Time")+
  ggtitle("daily temp data")
plot_daily
#save the file
write.csv(data_total,"data.csv",row.names = FALSE)
write.csv(avg,"data_avg.csv",row.names = FALSE)







#2.Statistics
### Test that whether there is difference from 1988 to 2017

data_1987 <- data_total %>% filter(YYYY==1987) 
data_2017 <- data_total %>% filter(YYYY==2017) 

#obtaining same format data for daily tmp at 17:00
#2.1 test on ATMP
t.test(data_1987$ATMP,data_2017$ATMP)
#Because p<2.2e-16 < 0.05, we tend to reject the null hypothes 
#that atmp have the same mean in 1988 and 2017 

#2.2 test on WTMP
t.test(data_1987$WTMP,data_2017$WTMP)
#Because p<2.2e-16 < 0.05, we tend to reject the null hypothes 
#that Wtmp have the same mean in 1988 and 2017 

#Thus there are significant changes in the past 30 years
#But we also want to know which direction did the atmp and wtmp move towards

#using regression line to show the direction
reg_ATMP=ggplot(daily,aes(x=Time,y=ATMP))+geom_point()+geom_smooth(method="lm")+ggtitle("reg ATMP")
reg_WTMP=ggplot(daily,aes(x=Time,y=WTMP))+geom_point()+geom_smooth(method="lm")+ggtitle("reg WTMP")
ggplot2.multiplot(reg_ATMP,reg_WTMP)
#we can see a clear upward trend of the ATMP and WTMP reg line, which indicates that thee temperature is raising!

#3.Test whether sampling affected your evaluation of temperature change
###Use hourly data to make a graph
plot_hourly <-ggplot(data_total, aes(x = Time)) + 
  geom_line(aes(y = WTMP), colour="green", size = 0.5) + 
  geom_line(aes(y = ATMP), colour = "blue", size = 0.5) +
  ylab(label="Celsius degrees") + 
  xlab("Time")+
  ggtitle("hourly temp data")

ggplot2.multiplot(plot_daily,plot_hourly)
### We can see in the above pic, they are nearly the same, but the hourly graph takes more time to run, 
### Thus sampling doesn't affect our evaluation
