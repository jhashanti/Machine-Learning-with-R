#-------------------------------------------------------------------#
#input parameters 
#-------------------------------------------------------------------#

#c_path_out          <- "D:/temp"
#modelObj            <- modelObj

#-------------------------------------------------------------------#
#Confusion Matrix 
#-------------------------------------------------------------------#

confusionMatrix <- function(actual,fitted,cutOff){
  xtemp             <- sapply(fitted, FUN = function(x){ x > cutOff })
  tp                <- length(which(xtemp & actual))
  tn                <- length(which(!xtemp & actual))
  fp                <- length(which(xtemp & !actual))
  fn                <- length(which(!xtemp & !actual))
  sensitivity       <- (tp/(tp+fn))*100
  specificity       <- (tn/(fp+tn))*100
  ppv               <- (tp/(tp+fp))*100
  npv               <- (tn/(tn+fn))*100
  OneMinusS         <- (1-(tn/(fp+tn)))*100
  
  return(c(Prob                      = cutOff, 
           TruePositive              = tp,
           TrueNegative              = tn, 
           FalsePositive             = fp, 
           FalseNegative             = fn, 
           Sensitivity               = sensitivity, 
           Specificity               = specificity,
           PositivePredictedValue    = ppv,
           NegativePredictedValue    = npv,
           OneMinusSpecificity       = OneMinusS))
}

#-------------------------------------------------------------------#
#Classification Table
#-------------------------------------------------------------------#
prob                <- seq(from = 0.01,
                           to   = .99,
                           by   = 0.01)
matrix              <- as.data.frame(t(sapply(prob, FUN=function(x){confusionMatrix(actual = modelObj$y,
                                                                                    fitted = modelObj$fitted.values,
                                                                                    cutOff = x)})))

#-------------------------------------------------------------------#
#ROC curve
#-------------------------------------------------------------------#

png(filename  = paste0(c_path_out,"/ROC.png"),
    width     = 12,
    height    = 6,
    units     = "in",
    res       = 100
)

rs                  <- ggplot(matrix,
                              aes(x = OneMinusSpecificity,
                                  y = Sensitivity))+
                       geom_line() 
rs                  <- rs + scale_x_continuous(breaks = seq(from = 0,
                                                            to = 100,
                                                            by = 10)) + 
                            scale_y_continuous(breaks = seq(from = 0,
                                                            to = 100,
                                                            by = 10))
print({
  rs      
})


dev.off()

#-------------------------------------------------------------------#
#Sensitivity vs Specificity plot
#-------------------------------------------------------------------#

png(filename  = paste0(c_path_out,"/Sensitivity-Specificity.png"),
    width     = 12,
    height    = 6,
    units     = "in",
    res       = 100
)

rs                  <- ggplot(matrix,
                              aes(x     = Prob,
                                  y     = Sensitivity/Specificity,
                                  color = Sensitivity/Specificity))+
                       geom_line(aes(y     = Sensitivity,
                                     color ="Sensitivity"))+ 
                       geom_line(aes(y     = Specificity,
                                     color = "Specificity"))
rs                  <- rs + scale_x_continuous(breaks = seq(from = 0,
                                                            to   = 1,
                                                            by   = .1)) + 
                            scale_y_continuous(breaks = seq(from = 0,
                                                            to   = 100,
                                                            by   = 10))
print({
  rs 
})


dev.off()
