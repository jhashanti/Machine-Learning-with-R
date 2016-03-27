#Random Forest for classification

#-------------------------------------------------------------------#
#Library Required 
#-------------------------------------------------------------------#

library(randomForest) 

#-------------------------------------------------------------------#
#input parameters 
#-------------------------------------------------------------------#

#c_path_out           <- "D:/temp"
#c_path_in            <- "D:/temp/sample.csv"
#c_var_in_dependent   <- "var_1"
#c_var_in_independent <- c("var_2","var_3","var_4","var_5")
#n_tree               <- 101

#-------------------------------------------------------------------#
#Functions
#-------------------------------------------------------------------#

classificationError   <- function (actual, predicted)
{
  error <- length(which(actual != predicted)) / length(actual)
}

#-------------------------------------------------------------------#
#load dataset 
#-------------------------------------------------------------------#

data                   <- read.csv(c_path_out)

#-------------------------------------------------------------------#
#explore data
#-------------------------------------------------------------------#

#dim(data)
#summary(data)
#head(data,2)

#-------------------------------------------------------------------#
#subset
#-------------------------------------------------------------------#

data                   <- na.omit(data)

#-------------------------------------------------------------------#
#Split function for validation
#-------------------------------------------------------------------#

partition <- function(numRow,seed) {
  set.seed(seed)
  index                <- sample(x       = numRow,
                                 size    = trunc(numRow*(2/3)),
                                 replace = FALSE)
}

index                  <- partition(numRow = nrow(data),seed = 123)
train                  <- data[index,]
test                   <- data[-index,]

#-------------------------------------------------------------------#
#Model Building
#-------------------------------------------------------------------#

formula                <- paste0(c_var_in_dependent, "~", c_var_in_independent)
rf_fit                 <- randomForest(formula = as.formula(formula),
                                       data    = train,
                                       ntree   = n_tree) 

#-------------------------------------------------------------------#
#Validation
#-------------------------------------------------------------------#

test$predicted         <- predict(object  = rf_fit,
                                  newdata = test)

accuracy               <- classificationError(test[,c_var_in_dependent],test$predicted)
