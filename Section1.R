# German credit Scoring
# INTRODUCTION
# ============

# Le projet est découpé en 3 parties
# Exploratoty data analysis (EDA)
# Feature engineering
# Model 
# Prediction 
# Load Packages
library(ggplot2) # visualization
library(ggthemes) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
library(mice) # imputation
library(randomForest) # classification algorithm
library(gtools) # for discretisation
library(corrplot)
library(Hmisc)
library(devtools)
library(PerformanceAnalytics)
library(FactoMineR)

# Importing Data
ccdata <- read.csv("CreditCard.csv")
head(ccdata)
summary(ccdata)

# Change categorical variables into factors.
factor_vars <- c('SEX','EDUCATION','MARRIAGE','default.payment.next.month')
data[factor_vars] <- lapply(data[factor_vars], function(x) as.factor(x))


# We will looke at all our 25 variables, distributions, levels.
# Lets look at Education variable
plot(data$EDUCATION)
names(data)

# we remark that that levels (0 , 4 , 5 and 6 reprents a tini amount of the data)
x = data$EDUCATION[data$EDUCATION == c(0,4,5,6)]
data$EDUCATION[data$EDUCATION== 4]  <- 0 
data$EDUCATION[data$EDUCATION== 5]  <- 0 
data$EDUCATION[data$EDUCATION== 6]  <- 0 


#plot of education vs. default.payment.next.month
ggplot(data, aes(x = EDUCATION, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'Education') 
A = data

# From now we are using A instead of data
# Barplot of age vs. default.payment.next.month

barplot<- ggplot(A, aes(x=EDUCATION, fill = default.payment.next.month))+
  geom_bar(width = 1)+
  coord_polar()
qplot(AGE, data = A, geom = "density", fill = default.payment.next.month)
barplot

hist(A$AGE,col = "light blue",freq  = 2,main = " Histogram of Client's Age",breaks = 10,xlab = "Age of the client", ylab = "Frequency")
ggplot(A, aes(x = AGE, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'Age')


# LETS LOOK AT LIMIT_AMOUNT
# We plot a histogram for limit amount (include individuals and family credits)

plot(A$LIMIT_BAL)
mean(A$LIMIT_BAL)
boxplot(x = data[,2],col = "green")


# exploring limit_bal payment
hist(data$LIMIT_BAL,col="yellow",breaks = 20) # 10K to 1G , mean = 167484 


# RELATION BTWN DEFAULT AND EDUCATION
ggplot(data, aes(x = EDUCATION, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'Education') 

# RELATION BTWN DEFAULT AND gender

ggplot(data, aes(x = SEX, fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'Sex') 

# RELATION BTWN DEFAULT AND MARIAGE
ggplot(data, aes(x = MARRIAGE,  fill = default.payment.next.month)) +
  geom_bar() +
  labs(x = 'Mariage')

# RELATION BTWN LIMIT_BAL AND EDUCATION
ggplot(data, aes(x = EDUCATION, fill = LIMIT_BAL)) +
  geom_bar() +
  labs(x = 'Education') +
  labs(y = 'Limit Balance')

# relation btwn age and default
ggplot(data, aes(AGE, fill = default.payment.next.month)) + 
  geom_histogram(binwidth = 6) + 
  facet_grid(.~EDUCATION) 
# I include education since we know (a priori) it's a significant predictor
# we see clearly an important rate of default in the university category
df <- read.csv("CreditCard.csv", header=TRUE)


#Tidy the dataset and change PAY_0 to PAY_1.    
names(df)[names(df) == "PAY_0"] <- "PAY_1"
names(df)
# lets explore PAY_1

ggplot(data, aes(df$PAY_1, fill = default.payment.next.month)) + 
  geom_histogram(binwidth = 1) 

ggplot(data, aes(df$PAY_2, fill = default.payment.next.month)) + 
  geom_histogram(binwidth = 1) 

ggplot(data, aes(df$PAY_3, fill = default.payment.next.month)) + 
  geom_histogram(binwidth = 1) 

ggplot(data, aes(df$PAY_4, fill = default.payment.next.month)) + 
  geom_histogram(binwidth = 1) 

ggplot(data, aes(df$PAY_5, fill = default.payment.next.month)) + 
  geom_histogram(binwidth = 1)

ggplot(data, aes(df$PAY_6, fill = default.payment.next.month)) + 
  geom_histogram(binwidth = 1) 


# we see that if pay_0 is btwn 1 and 6, the probability of default is a lot highter


























