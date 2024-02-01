library(ggplot2)
library(dplyr)
library(scales)
setwd("C:/Users//Rshiny")
print(getwd())
hp<-read.csv("House_Price.csv")
print(hp)

hp[rowSums(is.na(hp)) != ncol(hp),] #removing blank rows
hp

head(hp)
hp$Fireplace <- as.factor(hp$Fireplace)
summary(hp)

#Exploratory Analysis
#=====================================================================================================================================

#Question 11. a)

#1.Now we want to know , if the Age of house is related to Price
plot(Price~Age, hp) 
x_ax <- hp$Age
y_ax <- hp$Price
fit1 <- lm(y_ax~x_ax, data=hp, raw=TRUE)
lines(x_ax, predict(fit1, data.frame(x=x_ax)), col='green')

# We can infer that ,by seeing the graph that house which are from 0 to 50 years are high in price
# Kind of inversely proportional, the newer the building more is the price

# Since old house which are more than 50 years of Age might have Fireplaces and no heating systems,
# so need to check if having a Fireplace affects the Age of House & Price
print(ggplot(data = hp, aes(x = Age, y = Price)) +
        geom_point(aes(col = Fireplace)))
#we can thus say that houses within 50 years of Age with fireplaces are costlier than houses with np fireplaces and aged between 0-50 years





#2.We want to know, if the Living Area is related to no.of bedrooms, bathrooms
#Also, we want to know if Price is related to Living Area , bedrooms, bathrooms

#Let there be a null-hypothesis H0 that there is no relation between Living Area with no.of bedrooms,bathrooms
df1 <- aov( Living.Area ~ Bedrooms + Bathrooms , data = hp)
df1
Pval_bedroom = summary(df1)[[1]][,"Pr(>F)"][1]
Pval_bathroom = summary(df1)[[1]][,"Pr(>F)"][2]
print(Pval_bedroom)
print(Pval_bathroom)
alpha <- 0.05
if(Pval_bedroom < alpha && Pval_bathroom < alpha) ##if P-value < alpha is true we reject the null hypothesis
  print("The Living Area is related to no.of bedrooms, bathrooms")
# So in conclusion, In general,
# if we Living area is greater no.of Bedrooms, Bathrooms is high


#Let there be a null-hypothesis H0 that there is no relation between Price with Living Area, no.of bedrooms,bathrooms
df2<- aov(Price ~ Living.Area + Bedrooms + Bathrooms , data = hp)
df2
Pval_Living_Area = summary(df2)[[1]][,"Pr(>F)"][1]
Pval_bedroom = summary(df2)[[1]][,"Pr(>F)"][2]
Pval_bathroom = summary(df2)[[1]][,"Pr(>F)"][3]
print(Pval_Living_Area)
print(Pval_bedroom)
print(Pval_bathroom)
alpha <- 0.05
if(Pval_bedroom < alpha && Pval_bathroom < alpha && Pval_Living_Area < alpha) ##if P-value < alpha is true we reject the null hypothesis
  print("The Price is related to Living Area, no.of bedrooms, bathrooms")
plot(Price~Living.Area,hp)
# So in conclusion,
# if the price is high no.of Bedrooms, Bathrooms, and living area is high




 
#3.Need to check if larger Lot size means large Living Area
plot(Living.Area~Lot.Size, data=hp)
y_1 <- hp$Living.Area
x_1 <- hp$Lot.Size
fit2 <-lm(y_1~x_1,raw=TRUE)
lines(x_1, predict(fit2, data.frame(x=x_1)), col='green')

#With Increase of Lot.Size, Living Area increases, 
#but if the lot size is greater than 2 acres , 
#there is not much increase of Living Area, 
#Could be there is a garden or lawn or a courtyard in those cases




#4.Now we need to see if Lot Size has any effect on Price
plot(Price~Lot.Size, data=hp)
y_2 <- hp$Price
x_2 <- hp$Lot.Size
fit3 <-lm(y_2~x_2,raw=TRUE)
lines(x_2, predict(fit3, data.frame(x=x_2)), col='green')

#With Increase of Lot.Size, Price increases, 
#but if the lot size is greater than 2 acres , 
#there is not much increase of Price, 
#Could be there is a garden in those cases





#5. Are these large Lot size houses old ?
plot(Age~Lot.Size, data=hp)
y_3 <- hp$Age
x_3 <- hp$Lot.Size
fit4 <-lm(y_3~poly(x_3,3),raw=TRUE)
lines(x_3, predict(fit4, data.frame(x=x_3)), col='green')
#Conclusion cannot be interpreted from the graph


#==============================================================Histogram========================================================================

#Question 11. b)

hist(hp$Living.Area, xlab='Living Area (Sq. ft.)',
     col = "blue", border = "black")
# It is right-skewed , it is not a symmetric distribution, in right skewed , we know that the mean > median > mode. 
# Mean has the highest peak in Right skewed distribution , therefor we can conclude that mean or average Living Area is approximately 2000 sq.ft.
# There are outliers from 4000-5000 sq.ft area
mean(hp$Living.Area)
#From histogram we cannot say anything about interquartile range

# Let's check in box-plot to get more details of Median
boxplot(hp$Living.Area, xlab='Living Area (Sq. ft.)',
        col = "yellow", border = "black")
#Since the median line is below the Mid section of the box we can say from the figure that its a right skewed distribution
#beyond 3500 sq.ft, there is presence of outliers.
median(hp$Living.Area) # we can see median is less than the mean , which is property of Right skewed distribution
#The interquartile range is between 1200 to 2200 sq.ft.