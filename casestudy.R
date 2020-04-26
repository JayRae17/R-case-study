rm(list=ls())


case.data<-read.csv(file.choose(),stringsAsFactors = TRUE,header=TRUE, sep = ",")
View(case.data)
summary(case.data)

case.data[is.na(case.data$deposit),"deposit"]<- mean(case.data$deposit,na.rm = T)
summary(case.data)

case.data<-case.data[!(case.data$education==""),]
summary(case.data)
#View(case.data)

case.data<-case.data[(case.data$loan=="yes" |case.data$loan=="no" ),]
case.data<-case.data[!(case.data$loan=="!!"),]
case.data<-case.data[!(case.data$loan=="%"),]
case.data<-case.data[!(case.data$loan==""),]
summary(case.data)
#View(case.data)

case.data<-case.data[!(case.data$contact=="??"),]
case.data<-case.data[!(case.data$contact=="?"),]
summary(case.data)
#View(case.data)
hist(case.data$deposit)


case.data$contact<-NULL
summary(case.data)

library(binaryLogic)
encode_binary <- function(x, order = unique(x), name = "v_") {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x2 <- as.binary(x)
  maxlen <- max(sapply(x2, length))
  x2 <- lapply(x2, function(y) {
    l <- length(y)
    if (l < maxlen) {
      y <- c(rep(0, (maxlen - l)), y)
    }
    y
  })
  d <- as.data.frame(t(as.data.frame(x2)))
  rownames(d) <- NULL
  colnames(d) <- paste0(name, 1:maxlen)
  d
}

case.data <- cbind(case.data, encode_binary(case.data$marital, name = "marital_"))
summary(case.data)
#View(case.data)



#duration 
case.data$duration<- case.data$duration/ 60
summary(case.data)
#View(case.data)


#discritize age 

age_bin<-cut (case.data$age,breaks=seq(min(case.data$age),max(case.data$age),(max(case.data$age)-min(case.data$age))/4))
summary(age_bin)

#View(age_bin)
levels(age_bin)
#rrr<-which(is.na(age_bin))
#print(rrr)


case.data["age_bin"]<-age_bin
summary(case.data)
#View(case.data)
case.data <-na.omit(case.data)
summary(case.data)


#normalizing balance

case.data["norm_deposit"]<-  (case.data$deposit-min(case.data$deposit))/ (max(case.data$deposit)-min(case.data$deposit)) * (10-1) +1
#case.data$norm_balance<- NULL
summary(case.data)
#View(case.data)



#5
dayz<- case.data$day
mz<-case.data$month
mz2<-toupper(substr(mz, start = 1, stop = 3))

#x <- c("January", "February", "March") 
#substr(x, start = 1, stop = 3)

dd<- sprintf("%s-%s-19",dayz,mz2)
print(dd)
case.data["date_diference"]<- Sys.Date()-as.Date(dd,format="%d-%b-%y")+1
case.data["Format_Date"]<-as.Date(dd,format="%d-%b-%y")
View(case.data)




#6
library(dplyr)
library(plotly)
time_series<-data.frame()
#col_num<-c("Date","Deposit")
#colnames(time_series)<-col_num
time_series$Date<-as.Date(dd,format="%d-%b-%y")
time_series["Deposit"]<-case.data$deposit
View(time_series)

def.period <- case.data %>% filter(case.data$Format_Date >= as.Date("2019-01-01") & case.data$Format_Date <= as.Date("2019-03-01"))
def.period2 <- case.data %>% filter(case.data$Format_Date >= as.Date("2019-03-01") & case.data$Format_Date <= as.Date("2019-06-01"))
min(def.period$Format_Date)
max(def.period$Format_Date)
plot(case.data$deposit ~ def.period$Format_Date ,type='l')

ggplot(data = def.period, aes(Format_Date, deposit))+
  geom_bar(stat="identity", fill="darkorchid4")+
  xlab("Date (Quaterly)") + ylab("Deposits") +
  ggtitle("TS graph showing months jan-mar") + theme_bw()

ggplot(data = def.period2, aes(Format_Date, deposit))+
  geom_bar(stat="identity", fill="darkorchid4")+
  xlab("Date (Quarterly)") + ylab("Deposits") +
  ggtitle("TS graph showing months mar-jun") + theme_bw()

#fig <- plot_ly(case.data, case.data$deposit = ~case.data$deposit, case.data$month = ~random_y, type = 'scatter', mode = 'lines')

View(def.period)

#questions 7a
ggplot(data = def.period, aes(job, mean(deposit)))+
  geom_bar(stat="identity", fill="darkorchid4")+
  xlab("job type") + ylab(" avg Deposits") +
  ggtitle("TS graph showing months jan-mar with avg deposits for each job type") + theme_linedraw()

ggplot(data = def.period2, aes(job, mean(deposit)))+
  geom_bar(stat="identity", fill="darkorchid4")+
  xlab("job type") + ylab(" avg Deposits") +
  ggtitle("TS graph showing months mar-jun with avg deposits for each job type") + theme_linedraw()


#7b
ggplot(data = def.period2, aes(job, education=="tertiary"))+
  geom_bar(stat="identity", fill="darkorchid4")+
  xlab("job type") + ylab(" avg Deposits") +
  ggtitle("TS graph showing months jan-mar with avg deposits for each job type") + theme_linedraw()





