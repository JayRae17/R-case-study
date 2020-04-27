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
case.data["last_deposit"]<- Sys.Date()-as.Date(dd,format="%d-%b-%y")+1
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

def.period <- case.data %>% filter(case.data$Format_Date >= as.Date("2019-01-01") & case.data$Format_Date <= as.Date("2019-03-31"))
def.period2 <- case.data %>% filter(case.data$Format_Date >= as.Date("2019-04-01") & case.data$Format_Date <= as.Date("2019-06-30"))
min(def.period$Format_Date)
max(def.period$Format_Date)
#plot(case.data$deposit ~ def.period$Format_Date ,type='l')

ggplot(data = def.period, aes(Format_Date, deposit))+
  geom_bar(stat="identity", fill="darkorchid4")+
  xlab("Date (Quaterly)") + ylab("Deposits") +
  ggtitle("TS graph showing months jan-mar") + theme_bw()

ggplot(data = def.period2, aes(Format_Date, deposit))+
  geom_bar(stat="identity", fill="darkorchid4")+
  xlab("Date (Quarterly)") + ylab("Deposits") +
  ggtitle("TS graph showing months apr-jun") + theme_bw()

#fig <- plot_ly(case.data, case.data$deposit = ~case.data$deposit, case.data$month = ~random_y, type = 'scatter', mode = 'lines')

View(def.period)

#questions 7a
case.data<-case.data[!(case.data$job==" "),]
case.data<-case.data[!(case.data$job==""),]
summary.()
ggplot(data = def.period, aes(job, mean(deposit)))+
  geom_bar(stat="identity", fill="darkorchid4")+
  xlab("job type") + ylab(" avg Deposits") +
  ggtitle("TS graph showing months jan-mar with avg deposits for each job type") + theme_linedraw()

ggplot(data = def.period2, aes(job, mean(deposit)))+
  geom_bar(stat="identity", fill="darkorchid4")+
  xlab("job type") + ylab(" avg Deposits") +
  ggtitle("TS graph showing months mar-jun with avg deposits for each job type") + theme_linedraw()




ggplot(data = case.data, aes(job, mean(deposit)))+
  geom_bar(stat="identity", fill="darkorchid4")+
  xlab("job type") + ylab(" avg Deposits") +
  ggtitle("TS graph showing entire dataset avg deposits for each job type") + theme_linedraw()


#7b

tertiary <- case.data %>% filter(case.data$education == "tertiary")
admin <- case.data %>% filter(case.data$job == "blue-collar")
admin_t<-admin %>% filter(admin$education == "tertiary")
admin_s<-admin %>% filter(admin$education == "secondary")
admin_p<-admin %>% filter(admin$education == "primary")
summary(admin)
#View(tertiary)
ggplot(data = admin, aes(education,admin$job))+
  geom_bar(stat="identity", fill="darkorchid4")+
  xlab("job type") + ylab(" number of persons ") +
  ggtitle("TS graph showing relationship blue collar and education level") + theme_linedraw()



get_job <- function(jobb,type){
  
  f1 <- case.data %>% filter(case.data$job == jobb)
  t<-f1 %>% filter(f1$education == "tertiary")
  s<-f1 %>% filter(f1$education == "secondary")
  p<-f1 %>% filter(f1$education == "primary")
  my_list <- list("primary" = length(p$education), "secondary" = length(s$education), "tertiary" = length(t$education))
  return(my_list[[type]]) 
}

# Pie Chart with Percentages
#admin  
build_graph<- function(title,jobb,type,type1,type2){
slices <- c(get_job(jobb,type), get_job(jobb,type1),get_job(jobb,type2))
lbls <- c(type, type1, type2)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main=title)

}


library(ggplot2)
dfnew <- subset(case.data, select=c("job", "education"))
                barplot(table(dfnew), main="Graph",         xlab="Education Level", col=c("darkblue","red","green","purple","white","black","yellow","orange","pink","blue","white","brown"),         legend = rownames(table(dfnew)), beside=TRUE)
table(dfnew)


admin_level<-build_graph("Admin education levels","admin.","primary","secondary","tertiary")

admin_level

blue_level<-build_graph("blue-collar education levels","blue-collar","primary","secondary","tertiary")

blue_level

entrepreneur_level<-build_graph("entrepreneur education levels","entrepreneur","primary","secondary","tertiary")

entrepreneur_level



#7c
#assumption made: persons that have no loan and no mortgage
not_morg <- case.data %>% filter(case.data$housing == "no" & case.data$loan=="no")
mortgage_precent <- (length(not_morg$housing)/length(case.data$housing))*100
ps="%"
sprintf("The percentage of persons who dont have mortgage and loan is %s%s percent",mortgage_precent,ps)


#7d

ggplot(data = case.data, aes(age_bin, mean(balance)))+
  geom_bar(stat="identity", fill="darkorchid4")+
  xlab("Age group") + ylab(" avg Balance") +
  ggtitle("TS graph showing entire dataset avg balance for each age group") + theme_linedraw()



#7e



