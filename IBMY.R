
##Read the data in##########################################################################

IBMdata = read.csv("C:/Users/Meow/Documents/Bootcamp/IBM continued analysis/IBMdata.csv")
IBMdata2 = read.csv("C:/Users/Meow/Documents/Bootcamp/IBM continued analysis/IBMdata2.csv")


##############################################################################################
library(dplyr)
library(tidyr)
library(corrplot)
library(gbm)
library(ggplot2)


View(IBMdata2)
View(IBMdata)
########################################################################################################################################


IBMdata2$Attrition = ifelse(IBMdata$AttritionN == '1', 0, 
                            ifelse(IBMdata$AttritionN == '2', 1, 2))


IBMdata$AttritionN = ifelse(IBMdata$AttritionN == '1', 0, 
                            ifelse(IBMdata$AttritionN == '2', 1, 2))



View(IBMdata3)

IBMdata3 = cbind(IBMdata$HourlyRate, IBMdata$AttritionN, IBMdata2)

IBMdata2$AttritionN = NULL
IBMdata2$Attrition = NULL


View(IBMdata2)

IBMmatrix = matrix(IBMdata2)


numericvar2<-  IBMdata2[,sapply(IBMmatrix, is.numeric )] %>% colnames()

ibmcor3<- cor(IBMdata2[,numericvar2])

corrplot::corrplot(ibmcor3, method = 'circle')


#########################################################################################################################################

ggplot(IBMdata2)

ggplot(IBMdata2, aes(x = IBMdata2$Age, y = IBMdata2$MonthlyIncome))+ geom_point()


#What factor is contributing most to retention
#greater the age the greater the salary
#is age the primary reason or does salary also have an effect. 



All <- glm(IBMdata3$`IBMdata$AttritionN` ~ IBMdata3$Age + IBMdata3$MonthlyIncome  + IBMdata3$OvertimeN + IBMdata3$NumCompaniesWorked + IBMdata3$GenderN + IBMdata3$JobSatisfaction + IBMdata3$NumCompaniesWorked + IBMdata3$JobInvolvement, data = IBMdata3, family = "binomial")

All2 <- glm(IBMdata3$OvertimeN + IBMdata3$NumCompaniesWorked + IBMdata3$GenderN + IBMdata3$JobSatisfaction + IBMdata3$NumCompaniesWorked + IBMdata3$JobInvolvement, data = IBMdata3, family = "binomial")


vif(All)

View(IBMdata3)






summary(All)



scatter.smooth(All$fit,
               residuals(All, type = "deviance"),
               lpars = list(col = "red"),
               xlab = "Fitted Probabilities",
               ylab = "Deviance Residual Values",
               main = "Residual Plot ")
abline(h = 0, lty = 2)






library(car)
influencePlot(All) #Can still inspect the influence plot.

summary(All) #Investigating the overall fit of the model



confint(All) #For logistic regression objects, the confint() function
#defaults to using the log likelihood to generate confidence
#intervals; this is similar to inverting the likelihood
#ratio test.

confint.default(All) # generate confidence intervals for logistic
#regression models based on the standard errors
#as we are accustomed to, we can use the
#confint.default() function.






































