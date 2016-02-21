#Linear regression Step by Step

#-------------------------------------------------------------------#
#library 
#-------------------------------------------------------------------#

library(car)
library(lmtest)
library(ggplot2)

#-------------------------------------------------------------------#
#input parameters 
#-------------------------------------------------------------------#

#path pointing to the input .csv file
c_path_in                    <- "D:/Dataset/sample.csv"  
#output folder path
c_path_out                   <- "D:/temp/"   
#continuous independent variables
c_var_in_independent         <- c('var_1','var_2','var_3','var_4') 
#continuous dependent variable
c_var_in_dependent           <- c('var_5')           

#-------------------------------------------------------------------#
#function 
#-------------------------------------------------------------------#

mape                         <- function(actual,predicted) mean(abs((actual - predicted)/actual))*100

rmse                         <- function (actual, predicted) sqrt(mean(actual-predicted)^2)

#-------------------------------------------------------------------#
#load input dataset
#-------------------------------------------------------------------#

data                         <- read.csv(c_path_in)

#-------------------------------------------------------------------#
#subset 
#-------------------------------------------------------------------#

data                         <- data[,c(c_var_in_independent,c_var_in_dependent)]
data                         <- na.omit(data)

#-------------------------------------------------------------------#
#model building
#-------------------------------------------------------------------#

formula                      <- as.formula(paste0(c_var_in_dependent, '~' , paste0(c_var_in_independent,collapse = '+'))) 

lmObj                        <- lm(formula = formula,
                                   data    = data)

#-------------------------------------------------------------------#
#model information
#-------------------------------------------------------------------#

parameterEstimate            <- coef(summary(lmObj))

varianceInflationFactor      <- car::vif(lmObj)
varianceInflationFactor      <- c(NA,varianceInflationFactor)
actual                       <- lmObj$model[,c_var_in_dependent]
predicted                    <- lmObj$fitted.values
residual                     <- round(residuals(lmObj),4)

mape                         <- mape(actual    = actual,
                                     predicted = predicted)

RSquared                     <- summary(lmObj)$r.squared
adjustedRSquared             <- summary(lmObj)$adj.r.squared
FStatistic                   <- summary(lmObj)$fstatistic["value"]
rootMeanSquaredError         <- rmse(actual, predicted)

#-------------------------------------------------------------------#
#Goldfeld Quandt test for Homoscedasticity
#-------------------------------------------------------------------#

goldfledQuandtTest          <- lmtest::gqtest(lmObj)[1]

#-------------------------------------------------------------------#
#Durbin watson test for autocorrelation (1.5 < stat < 2.5)
#-------------------------------------------------------------------#

durbinWatsonTest             <- lmtest::dwtest(lmObj)[1]

#-------------------------------------------------------------------#
#ouput 
#-------------------------------------------------------------------#

parameterEstimate            <- cbind.data.frame(parameterEstimate,varianceInflationFactor)

modelStatistic               <- cbind.data.frame(Statistic = c("R-Squared","Adj. R-Squared","Root Mean Squared Error","Mean Absolute Percentage Error","F-Statistic"),
                                                 Value =c(RSquared,adjustedRSquared,rootMeanSquaredError,mape,FStatistic))

write.csv(parameterEstimate,paste0(c_path_out,"/Estimates.csv"))

write.csv(modelStatistic,paste0(c_path_out,"/ModelSatistic.csv"))

#-------------------------------------------------------------------#
#Actual vs Predicted
#-------------------------------------------------------------------#

png(filename=paste0(c_path_out,"/ActualPredicted.png"),
    width     = 12,
    height    = 6,
    units     = "in",
    res =100
)

rs <- qplot(x    = actual,
            y    = predicted,
            xlab = "Actual",
            ylab = "Predicted") + theme(aspect.ratio = 1/2.5)
print({
  
  rs2 <- rs +  theme(axis.text.x=element_text(size=12, angle=40, vjust=.9, hjust=1.01))
})


dev.off()

#-------------------------------------------------------------------#
#Residual vs Predicted plot verifying the assumptions of Linear Model
#-------------------------------------------------------------------#

png(filename  = paste0(c_path_out,"/ResidualPredicted.png"),
    width     = 12,
    height    = 6,
    units     = "in",
    res       = 100
)

rs <- qplot(x    = predicted,
            y    = residual,
            xlab = "Predicted", 
            ylab = "Residual") + theme(aspect.ratio = 1/2.5)
print({
  
  rs2 <- rs +  theme(axis.text.x=element_text(size=12, angle=40, vjust=.9, hjust=1.01))
})


dev.off()