getwd()
setwd("/Users/azadeharam/Desktop")
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(corrplot)
#read the CreditCard.csv file and create a data frame ccdata
data[factor_vars] <- lapply(data[factor_vars], function(x) as.factor(x))
ccdata <- read.csv("CreditCard.csv")
#find the dimention of the matrix
dim(ccdata)
str(ccdata)
summary(ccdata)


ccdata$workstate <- ""
ccdata$genderH <- ""
ccdata$educationH <- ""
ccdata$maritalH <- ""
for (i in 1:nrow(ccdata)) {
  if ((ccdata[i,7] + ccdata[i,8] +ccdata[i,9]+ccdata[i,10] +ccdata[i,11]+ccdata[i,12]) <= 0){
    ccdata[i,26] <- "YES"  
  }
  else {
    ccdata[i,26] <- "NO"         
  }
}
for (i in 1:nrow(ccdata)) {
  if (ccdata[i,3] == 1) {
    ccdata[i,27] <- "Male"  
  }
  else {
    ccdata[i,27] <- "Female"         
  }
}
for (i in 1:nrow(ccdata)) {
  if (ccdata[i,4] == 1) {
    ccdata[i,28] <- "Graduate"
  } else if (ccdata [i,4] == 2) {
    ccdata[i,28] <- "University" 
  } else if (ccdata [i,4] == 3) {
    ccdata[i,28] <- "High School" 
  } else {
    ccdata[i,28] <- "Unknown" 
  }
}
for (i in 1:nrow(ccdata)) {
  if(ccdata[i,5] == 1) {
    ccdata[i,29] <- "Married"
  } else if (ccdata[i,5] == 2) {
    ccdata[i,29] <- "Single"
  } else {
    ccdata[i,29] <- "Other"
  }
}

ccdata$AGE.bucket<-cut(ccdata$AGE,c(10,20,30,40,50,60,70))
ccdata$workstate <-factor(ccdata$workstate)
ccdata$SEX <-factor(ccdata$SEX)
ccdata$EDUCATION <- factor(ccdata$EDUCATION)
ccdata$MARRIAGE <- factor(ccdata$MARRIAGE)
ccdata$AGEf <- factor(ccdata$AGE)
ccdata$default.payment.next.month<-factor(ccdata$default.payment.next.month)
ccdata$genderH <- factor(ccdata$genderH)
ccdata$educationH <- factor(ccdata$educationH)
ccdata$maritalH <- factor(ccdata$maritalH)
ccdata = subset(ccdata, select = -c(PAY_0,PAY_2,PAY_3,PAY_4,PAY_5,PAY_6))
dim(ccdata)
str(ccdata)
summary(ccdata)
#Balance Limits By Gender
d1 <- ggplot(ccdata, aes(factor(genderH), (LIMIT_BAL/1000), fill=educationH)) +geom_boxplot() +
xlab("Gender") + ylab("BLimit(x1000 NT$)") + scale_fill_brewer(palette = "Accent")
d1
# Balance limits by education and gender
d2 <- ggplot(ccdata, aes(factor(educationH), (LIMIT_BAL/1000), fill=genderH)) + geom_boxplot() +
xlab("Education") + ylab("BLimit(x1000 NT$)") + scale_fill_brewer(palette = "Paired")
d2

ggplot(ccdata, aes(factor(maritalH), (LIMIT_BAL/1000), fill=genderH)) + 
  geom_boxplot() +
  xlab("Marital Status") + 
  ylab("Balance Limit ( x 1000 NT$)") + 
  coord_cartesian(ylim = c(0,350)) +
  scale_fill_brewer(palette = "Paired")


#Result: By this graph, we can see it again that, there is no change at females side such as balance limits depending on their marital status, 
#however it changes a lot of things on males’ side starting with the expenditures which is the reason on increased balance limits. 

ggplot(aes(x = ccdata$LIMIT_BAL/1000), data = ccdata) +
  geom_histogram(aes(fill = ccdata$default.payment.next.month)) +
  xlab("Balance Limit x 1000") +
  ylab("Count") +
  scale_fill_discrete(name="Default Payment Next Month",
                      breaks=c(0, 1),
                      labels=c("No", "Yes")) +
  xlim(c(0,750)) +
  facet_wrap(~educationH)

#Result: Balance limits and count of defaulted clients are almost same for University and Graduate Level.
#Additionally, the ratio of defaulted clients at high school level seems almost the same as the university and graduate levels.

d4 <- ggplot(ccdata, aes(x=default.payment.next.month)) + 
  geom_bar(stat="count",color='blue',fill='light blue') +
  xlab("Default Payment Status") + ylab("Customer Count") + 
  facet_wrap(~educationH)

d4
d5 <- ggplot(ccdata, aes(x=default.payment.next.month),aes(y=stat_count(gender))) + 
  geom_bar(aes(fill=factor(ccdata$educationH))) +
  xlab("Default Payment Status")+ylab("Customer Count") + 
  facet_wrap(~genderH)+
  scale_fill_discrete(name="Education")

d5


#Result: There is no effect of education level on default event occurrence.

ggplot(data = subset(ccdata,!is.na(AGE.bucket)), aes(factor(educationH), (LIMIT_BAL/1000), fill=AGE.bucket)) + 
  geom_boxplot() +
  xlab("Education") + 
  ylab("Balance Limit ( x 1000 NT$)") + 
  coord_cartesian(ylim = c(0,500)) +
  scale_fill_brewer(palette = "Accent")
#Result: Reflection of the education levels on determination of balance limits for clients is increasing by later ages 
#,when we compare the averages of each age buckets with each other grouped by their education levels.


#Data Heading Conversation of Customer Bills

ccdata$SEP2005<-ccdata$BILL_AMT1
ccdata$AUG2005<-ccdata$BILL_AMT2
ccdata$JUL2005<-ccdata$BILL_AMT3
ccdata$JUN2005<-ccdata$BILL_AMT4
ccdata$MAY2005<-ccdata$BILL_AMT5
ccdata$APR2005<-ccdata$BILL_AMT6

#Expdenditure By Months
par(mfrow=c(3,2))

april <- ggplot(aes(x=AGE,y=APR2005/1000),data=ccdata) +
  xlab("Age") + 
  ylab("Amount of Bill in April") +
  coord_cartesian(xlim = c(21,60),ylim = c(0,700))+  
  geom_jitter(alpha=0.3, color="red") + geom_smooth(stat='summary', fun.y=mean)

april

may <- ggplot(aes(x=AGE,y=MAY2005/1000),data=ccdata) +
  xlab("Age") + 
  ylab("Amount of Bill in May") +
  coord_cartesian(xlim = c(21,60),ylim = c(0,700))+  
  geom_jitter(alpha=0.3, color="gray") + geom_smooth(stat='summary', fun.y=mean)
may

june <- ggplot(aes(x=AGE,y=JUN2005/1000),data=ccdata) +
  xlab("Age") + 
  ylab("Amount of Bill in June") +
  coord_cartesian(xlim = c(21,60),ylim = c(0,700))+  
  geom_jitter(alpha=0.3, color="yellow") + geom_smooth(stat='summary', fun.y=mean)
june

july <- ggplot(aes(x=AGE,y=JUL2005/1000),data=ccdata) +
  xlab("Age") + 
  ylab("Amount of Bill in July") +
  coord_cartesian(xlim = c(21,60),ylim = c(0,700))+  
  geom_jitter(alpha=0.3, color="red") + geom_smooth(stat='summary', fun.y=mean)
july

august <- ggplot(aes(x=AGE,y=AUG2005/1000),data=ccdata) +
  xlab("Age") + 
  ylab("Amount of Bill in August") +
  coord_cartesian(xlim = c(21,60),ylim = c(0,700))+  
  geom_jitter(alpha=0.3, color="gray") + geom_smooth(stat='summary', fun.y=mean)
august

september <- ggplot(aes(x=AGE,y=SEP2005/1000),data=ccdata) +
  xlab("Age") + 
  ylab("Amount of Bill in September") +
  coord_cartesian(xlim = c(21,60),ylim = c(0,700))+  
  geom_jitter(alpha=0.3, color="yellow") + geom_smooth(stat='summary', fun.y=mean)

september
grid.arrange(april,may,june,jul,august,september,ncol=3)


#Result: Distribution of expenditures on ages doesn’t show any differentiation
#between months and the average expenditure by age is almost same for each month.

#"Correlations Between Limit Balance, Bill Amounts & Payments"

ggplot(aes(x=AGE,y=LIMIT_BAL/1000),data=subset(ccdata,!is.na(AGE.bucket)))+
  xlab("Age") + 
  ylab("Balance Limit (x1000 NT$)") +
  coord_cartesian(xlim = c(21,60),ylim = c(0,700))+
  scale_color_brewer(palette = "Pastel1")+
  geom_jitter(alpha=0.5, position = position_jitter(h=0), aes(color=AGE.bucket)) +
  geom_smooth(stat='summary', fun.y=mean) +
  geom_smooth(stat='summary', fun.y=quantile, fun.args = list(probs = 0.1), color = 'black', linetype=2) +
  geom_smooth(stat='summary', fun.y=quantile, fun.args = list(probs = 0.5), color = 'red', linetype=2) +
  geom_smooth(stat='summary', fun.y=quantile, fun.args = list(probs = 0.9), color = 'black', linetype=2)



ggplot(aes(x=AGE,y=LIMIT_BAL/1000),data=subset(ccdata,!is.na(AGE.bucket)))+
  xlab("Age") + 
  ylab("Balance Limit (x1000 NT$)") +
  coord_cartesian(xlim = c(21,60),ylim = c(0,700))+
  scale_color_brewer(palette = "Pastel1")+
  geom_jitter(alpha=0.5, position = position_jitter(h=0), aes(color=AGE.bucket)) +
  geom_smooth(stat='summary', fun.y=mean) +
  geom_smooth(stat='summary', fun.y=quantile, fun.args = list(probs = 0.1), color = 'black', linetype=2) +
  geom_smooth(stat='summary', fun.y=quantile, fun.args = list(probs = 0.5), color = 'red', linetype=2) +
  geom_smooth(stat='summary', fun.y=quantile, fun.args = list(probs = 0.9), color = 'black', linetype=2) +
  geom_smooth(stat='summary', fun.y=quantile, fun.args = list(probs = 0.95), color = 'black', linetype=2) +
  facet_wrap(~default.payment.next.month)

#Result: When we reflect the correlations between limit balances, 
#bill amounts and payments amounts; 
#it presents us that there’s a low correlation between the limit balances and payments and bill amounts. 
#However it can be seen that bill amounts has high correlation between each other as expected since the bills a reflecting the cumulative amounts.

