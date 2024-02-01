#install.packages("openxlsx")
#install.packages("combinat")  
#install.packages("plotly")
#install.packages("tidyverse")
library("openxlsx")
library("ggplot2")
library("dplyr")
library("combinat")  
setwd("C://Users//Sudeshna Kundu//Desktop//SA-1//Assignment Individual")
print(getwd())
data<-read.xlsx(xlsxFile  = "Dataset_for_Assignment.xlsx")
data<-data[1:1000,]
print(data)


# Data Cleaning -> to clean up the data by replacing #NULL! with blank or zero.
data$AREA[is.na(data$AREA)] <-0
data$INCOME[is.na(data$INCOME)]<-0

#I.	Draw a histogram for the TOTAL score (you have to define appropriate class intervals)
    #and comment on the distribution of the TOTAL score.
print(hist(data$TOTAL, xlab = "Total Score", col = "orange",
     border = "black", xlim = c(0, 100),
     ylim = c(0, 200), breaks = 20)) 
#As conclusion we find out that the distribution of "Total score" is left skewed


#======================================================================================================================

#II. We take a simple random sample of 16 learners. 
  #What is the probability that the sample average of this sample (X-bar) is more than 60?

sample<-data[140:155,]   #This is a sample data for 16 learners
sample
print(hist(sample$TOTAL, xlab = "Total Score", col = "pink",
           border = "brown", xlim = c(0, 100),
           ylim = c(0, 10), breaks = 20))

#To answer the question II, we need to know the population mean and the population standard deviation
#of the total scores. Then we will use the central limit theorem to find the sampling
#distribution of the sample average and calculate the probability using a normal distribution.
sample_size <- nrow(sample) 
print(sample_size)
population_size <- nrow(data)
print(population_size)
Sample_size_percentage <- sample_size*100/population_size
print(Sample_size_percentage) 

#We can see that sample size, is less than 5% of population size, 1.6%
#Therefore we can conclude that sample's standard deviation is sample_std = population_std/sqrt(sample_size)

population_mean <- mean(data$TOTAL)
population_std <- sd(data$TOTAL)
sample_mean <- mean(sample$TOTAL)                        #for the given sample taken above
sample_std <- population_std/sqrt(sample_size)


print(paste0("Population Mean = ",population_mean))
print(paste0("Population Standard Deviation = ",population_std))
print(paste0("Sample Mean = ",sample_mean))
print(paste0("Sample Standard Deviation = ",sample_std))

#Using Central Limit Theorem to find the sampling distribution of sample average
meanArray <- array(0,dim=population_size-15)
for(i in 0:population_size-15){
  samples <- data[i:i+15,]
  meanArray[i] <- mean(samples$TOTAL)
}

probability_dist <- dnorm(meanArray,mean(meanArray),sd(meanArray))
plot(meanArray,probability_dist,xlab="Sample mean of Total scores", ylab="Probability Distribution") 
#follows normal Distribution 

#Calculating the probability that the sample average of this sample (X-bar) is more than 60
Prob_Xgreaterthan60 = 1-pnorm(60,mean(meanArray),sd(meanArray))
print(Prob_Xgreaterthan60)  #39.37%


#=====================================================================================================================


#III.	Create a new variable called “PERFORMANCE”. If the score is above the sample average, label the PERFORMANCE 
#as “HIGH”, otherwise, “LOW”. Using the new variable “PERFORMANCE” and GENDER,
#comment on the relative performance of Male and Female learners

data$PERFORMANCE <- ifelse(data$TOTAL >= sample_mean, "High", "Low")
data
data$Gender <- as.factor(data$Gender)

count <- table(data$Gender, data$PERFORMANCE)
barplot(count, main = "Performance Distribution vs Gender",
        xlab = "Performance", col = c("pink", "skyblue"),
        legend = rownames(count))
# There are much more number of females whose performance is higher than males
# For Low performance number males and females are almost same
# Also count of high performers irrespective of gender is higher than that of low performers


#====================================================================================================================





#IV.	Using the sample that you have just selected, calculate a 95% two-sided confidence interval for the following.

#A. Mean of TOTAL Score
#B. Mean of INCOME
#C. Proportion of “HIGH” performers among Male learners
#D. Proportion of “HIGH” performers among Female Learners.

#Solution for Part A
CI = 0.95
alpha = (1-CI)/2
z_score_alpha = qnorm(1-alpha)
print(z_score_alpha)

CI_Lower_limit = sample_mean - z_score_alpha*sample_std
print(CI_Lower_limit)
CI_Upper_limit = sample_mean + z_score_alpha*sample_std
print(CI_Upper_limit)


#Solution for Part B
CI = 0.95
alpha = (1-CI)/2
z_score_alpha = qnorm(1-alpha)
print(z_score_alpha)

sample_mean_income = mean(sample$INCOME)
print(sample_mean_income)
sample_std_income = sd(data$INCOME)/sqrt(sample_size)
print(sample_std_income)
CI_Lower_Lim = sample_mean_income - z_score_alpha*sample_std_income
print(CI_Lower_Lim)
CI_Upper_Lim = sample_mean_income + z_score_alpha*sample_std_income
print(CI_Upper_Lim)


#Solution for Part C&D

CI = 0.95
alpha = (1-CI)/2
z_score_alpha = qnorm(1-alpha)
print(z_score_alpha)

sample$PERFORMANCE <- ifelse(sample$TOTAL >= sample_mean, "High", "Low")

sample_Male_High_Population <- subset(sample, Gender == 'T' & PERFORMANCE == "High")
sample_Male_Population <- subset(sample, Gender == 'T')
sample_prop_male <- nrow(sample_Male_High_Population)/nrow(sample_Male_Population)

sample_Female_High_Population <- subset(sample, Gender == 'F' & PERFORMANCE == "High")
sample_Female_Population <- subset(sample, Gender == 'F')
sample_prop_female <- nrow(sample_Female_High_Population)/nrow(sample_Female_Population)

CI_prop_Male_UL <-  sample_prop_male + z_score_alpha *sqrt(sample_prop_male*(1-sample_prop_male) / sample_size)
CI_prop_Male_LL <-  sample_prop_male - z_score_alpha *sqrt(sample_prop_male*(1-sample_prop_male) / sample_size)
print(paste0("Lower Limit of 95% CI for male high performers' proportion = ",CI_prop_Male_LL))
print(paste0("Upper Limit of 95% CI for male high performers' proportion = ",CI_prop_Male_UL))


CI_prop_Female_UL <-  sample_prop_female + z_score_alpha *sqrt(sample_prop_female*(1-sample_prop_female) / sample_size)
CI_prop_Female_LL <-  sample_prop_female - z_score_alpha *sqrt(sample_prop_female*(1-sample_prop_female) / sample_size)
print(paste0("Lower Limit of 95% CI for female high performers' proportion = ",CI_prop_Female_LL))
print(paste0("Upper Limit of 95% CI for female high performers' proportion = ",CI_prop_Female_UL))


#================================================================================================================



# V.	If we want to estimate a 95% confidence interval for the population mean of “TOTAL” marks within ± 5 marks, 
# what is the sample size required? Please note that you do not know σ.  
# You may use your present sample of 1000 as a pilot sample.

Degrees_of_freedom <- 1000-1
t_value <- qt(p=alpha,df=Degrees_of_freedom,lower.tail = FALSE)
class(t_value)
sample_sz <- (t_value*population_mean/5)*(t_value*population_mean/5)
print(paste0("Sample Size required is ",ceiling(sample_sz)))

#=================================================================================================================




# VI.	If we want to estimate a 95% confidence interval for the proportion of “HIGH” performers
# within ± 0.05, what is the appropriate sample size? 

sample_High_Population <- subset(data, PERFORMANCE == "High")
Sample_high_Proportion <- nrow(sample_High_Population)/1000
print(Sample_high_Proportion)

sample_size_prop = z_score_alpha * z_score_alpha * Sample_high_Proportion*(1-Sample_high_Proportion)/(0.05*0.05)
print(paste0("Sample Size required is ",ceiling(sample_size_prop)))

#==================================================================================================================
