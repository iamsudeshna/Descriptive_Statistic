
library("openxlsx")
library("ggplot2")
library("dplyr")
library("combinat")  
setwd("C://Users//Sudeshna Kundu")
print(getwd())
data<-read.csv("SA1_Group_1.csv")

#==================================== Confidence Interval ========================================

print(mean(data$GOP_Year3))
print(sd(data$GOP_Year3))

# Since Standard deviation is greater than mean and,
# Also, from the Histogram we can say the data is highly right-skewed
# Removing the out-lier, Any GOP_Year3 which is blank or is not an outlier, we are removing the outliers by IQR. 
# Within this range of data Stand deviation is lesser than mean, and distribution is not skewed much.

Q3 = quantile(data$GOP_Year3, prob=0.75)
Q1 = quantile(data$GOP_Year3, prob=0.25)
IQR = Q3-Q1
LL = Q1-1.5*IQR
UL = Q3+1.5*IQR
data<-data[!is.na(data$GOP_Year3) & data$GOP_Year3!=0 & (data$GOP_Year3>=LL & data$GOP_Year3<=UL),]
hist(data$GOP_Year3, xlab = "GOP_Year3 ",
     col = "green", border = "black")
print(mean(data$GOP_Year3))
print(sd(data$GOP_Year3))
print(nrow(data))

error = qt(0.975,df=nrow(data)-1)*sd(data$GOP_Year3)/sqrt(nrow(data))

LL_CI_GOP = mean(data$GOP_Year3)-error
print(LL_CI_GOP)

UL_CI_GOP = mean(data$GOP_Year3)+error
print(UL_CI_GOP)


#==================================== Defining Performance Measures ==============================

# The two different performance measures are :
# 1. Asset Allocation Ratio = (GOP_Yr1 + GOP_Yr2 + GOP_Yr3)/NET_Yr3
# 2. Productivity = GOP_Yr3 / Total employees

data$AR <- (data$GOP_Year1 + data$GOP_Year2 + data$GOP_Year3)/data$NET_Year3
data$AR[data$NET_Year3 == 0] <- 0
data$AR[is.na(data$AR)] <- 0


data$Productivity <- data$GOP_Year3/data$EMP_TOTAL
data$Productivity[data$EMP_TOTAL == 0] <- 0
data$Productivity[is.na(data$Productivity)] <- 0


#=========================== 99% Confidence Interval for Measures ================================

LL_CI_AR = mean(data$AR)-qnorm(0.005, lower.tail=FALSE)*sd(data$AR)/sqrt(nrow(data))
print(LL_CI_AR)
UL_CI_AR = mean(data$AR)+qnorm(0.005, lower.tail=FALSE)*sd(data$AR)/sqrt(nrow(data))
print(UL_CI_AR)


LL_CI_Productivity = mean(data$Productivity)-qnorm(0.005, lower.tail=FALSE)*sd(data$Productivity)/sqrt(nrow(data))
print(LL_CI_Productivity)
UL_CI_Productivity = mean(data$Productivity)+qnorm(0.005, lower.tail=FALSE)*sd(data$Productivity)/sqrt(nrow(data))
print(UL_CI_Productivity)


#============================ SSSBE Unit Performance ==============================================

Probability_SSSBE = nrow(data[data$UNIT_TYPE == 1,])/nrow(data)
print(Probability_SSSBE)

Avg_AR = mean(data$AR)
print(Avg_AR)

Probability_GoodPerformance = nrow(data[data$AR>Avg_AR,])/nrow(data)
print(Probability_GoodPerformance)

Prob_SSSBE_Good = nrow(data[data$AR>Avg_AR & data$UNIT_TYPE == 1,]) / nrow(data)
print(Prob_SSSBE_Good)
if (Prob_SSSBE_Good > Probability_GoodPerformance)
{
  print("SSSBE units have a higher probability of being good in performance.")
}else
{
 print("The difference in performance between SSSBE and non-SSSBE units in Unclear") 
}


#============================== One-Sided Hypothesis Test ===========================================

null_hypothesis = "The population average of VOE_Year3 is 87,300."
alternate_hypothesis = "The population average of VOE_Year3 is less than 87,300."

t.test(data$VOE_Year3, mu=87300, alternative = 'less')

#p-value is 2.2e-16, less than 0.05, therefore we do reject the null hypothesis
print(alternate_hypothesis)
print(mean(data$VOE_Year3))

