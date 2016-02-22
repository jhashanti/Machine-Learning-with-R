#-------------------------------------------------------------------#
#library 
#-------------------------------------------------------------------#

library(car)
library(ResourceSelection)
library(ggplot2)

#-------------------------------------------------------------------#
#input parameters 
#-------------------------------------------------------------------#

c_path_in                         <- "D:/Dataset/sample.csv"
c_path_out                        <- "D:/temp/"
c_var_in_independent              <- c('var_1','var_2','var_3','var_4')
c_var_in_dependent                <- c('var_5')
x_val_event                       <- 'level_1'
#-------------------------------------------------------------------#
#function 
#-------------------------------------------------------------------#

#-------------------------------------------------------------------------------------------#
# Condordance Discordance
#-------------------------------------------------------------------------------------------#
#takes in a model
#returns a list of eleven named objects
  
associationStat <- function(model,b_somersD =1L, b_gamma = 1L, b_tau = 1L, b_cStat = 1L){
n_somersD_stat                    <- vector(mode = "numeric",
                                            length = 1)
n_gamma_stat                      <- vector(mode = "numeric",
                                            length = 1)
n_tau_stat                        <- vector(mode = "numeric",
                                            length = 1)
n_c_stat                          <- vector(mode = "numeric",
                                            length = 1)

    #-------------------------------------------------------------------------------------------#
    # SomersD Statistic
    #-------------------------------------------------------------------------------------------#
    #takes in a count of concordant pair,discordant pair,total pair
    #returns SomersD statistic
    
    somersD <- function(n_concordance, n_discordance, n_pairs){
      n_somersD_stat              <- (n_concordance - n_discordance)/n_pairs
      return(n_somersD_stat)
    }
    
    #-------------------------------------------------------------------------------------------#
    # Gamma Statistic
    #-------------------------------------------------------------------------------------------#
    #takes in a somers D statistic, count of tied pair,total pair
    #returns Gamma statistic
    
    gamma <- function(n_concordance, n_discordance){
      n_gamma_stat                <- (n_concordance - n_discordance) / (n_concordance + n_discordance)
      return(n_gamma_stat)
    }
    
    
    #-------------------------------------------------------------------------------------------#
    # Tau Statistic
    #-------------------------------------------------------------------------------------------#
    #takes in a count of concordant pair,discordant pair,no. of observation used
    #returns Tau statistic
    
    tau <- function(n_concordance, n_discordance, n_Observations_Used){
      n_tau_stat                  <- (n_concordance - n_discordance)/
                                      (0.5 * n_Observations_Used * (n_Observations_Used - 1))
      return(n_tau_stat)
    }
    
    #-------------------------------------------------------------------------------------------#
    # C Statistic
    #-------------------------------------------------------------------------------------------#
    #takes in a percent concordant pair,percent tied pair
    #returns C statistic
    
    cStat <- function(n_concordance, n_tied, n_pairs){
      #same as 0.5*(1+somersD)
      n_c_stat                    <- (n_concordance + (0.5 * n_tied)) / n_pairs
      return(n_c_stat)
    }
    
    x_temp                        <- as.logical(model$y)
    df_comparison                 <- expand.grid(list(predProbForActualEvent = model$fitted[x_temp], 
                                                      predProbForActualNonevent = model$fitted[!x_temp]))
    
    x_temp                        <- df_comparison[["predProbForActualEvent"]] > df_comparison[["predProbForActualNonevent"]]
    n_concordance                 <- sum(x_temp)
    x_temp                        <- df_comparison[["predProbForActualEvent"]][!x_temp] < df_comparison[["predProbForActualNonevent"]][!x_temp]
    n_discordance                 <- sum(x_temp)
    n_tied                        <- nrow(df_comparison) - n_concordance - n_discordance
    
    #  concData  
    n_pairs                       <- n_concordance + n_discordance + n_tied
    n_percentConcordant           <- n_concordance / n_pairs *100
    n_percentDiscordant           <- n_discordance / n_pairs *100
    n_percentTied                 <- n_tied / n_pairs * 100
    
    if(b_somersD){
      n_somersD_stat              <- somersD(n_concordance, n_discordance, 
                                             n_pairs)
    }
    
    if(b_gamma){
      n_gamma_stat                <- gamma(n_concordance ,n_discordance)
      
    }
    
    if(b_tau){
      n_tau_stat                  <- tau(n_concordance, n_discordance, length(model$fitted.values))
      
    }
    
    if(b_cStat){
      n_c_stat                    <- cStat(n_concordance, n_tied, n_pairs)
      
    }
    
    return(c(concordant_pair = n_concordance, discordant_pair = n_discordance, tied_pair = n_tied, 
             total_pair = n_pairs, percent_concordant_pair = n_percentConcordant, 
             percent_discordant_pair = n_percentDiscordant, percent_tied_pair = n_percentTied,
             somersD = n_somersD_stat, gamma = n_gamma_stat, 
             tau = n_tau_stat, c = n_c_stat))
  }

#-------------------------------------------------------------------#
#Group Rank
#-------------------------------------------------------------------#
  
group_rank <- function(data,order,groups,ties.method = 'average',na.last = TRUE){
  if(order == 'descending')
  {
      data                        <- -data
  }
  order_rank <- rank(x = data,na.last = na.last,ties.method = ties.method)
  if(is.na(na.last) || na.last == 'keep')
  {
      n                           <- sum(!is.na(data))
  }else
  { 
      n                           <- length(data)
  }
  group_rank                      <- floor((order_rank*groups)/(n+1))
    
  return(group_rank)
}
#-------------------------------------------------------------------#
#load input dataset
#-------------------------------------------------------------------#

data                              <- read.csv(c_path_in)

#-------------------------------------------------------------------#
#subset 
#-------------------------------------------------------------------#

data                              <- data[,c(c_var_in_independent,c_var_in_dependent)]
data                              <- na.omit(data)

data[,c_var_in_dependent]         <- data[,c_var_in_dependent] == x_val_event

formula                           <- paste0(c_var_in_dependent,"~",paste0(c(c_var_in_independent),collapse = "+"))
modelObj                          <- glm(formula = formula,
                                         data=data,
                                         family=binomial(link="logit"))

#-------------------------------------------------------------------#
#model information
#-------------------------------------------------------------------#

parameterEstimate                 <- coef(summary(modelObj))

varianceInflationFactor           <- car::vif(modelObj)
varianceInflationFactor           <- c(NA,varianceInflationFactor)

LIntercept                        <- modelObj$null.deviance
k                                 <- 2 
s                                 <- length(c(c_var_in_independent))
AIC                               <- 2*((k-1)+s) - 2* LIntercept[1]
SC                                <- ((k-1)+s)*log(length(modelObj$y))-2*LIntercept[1]
log_L                             <- -2*LIntercept

#Concordance and discordance calculation
concor_discor                     <- associationStat(modelObj)


#-------------------------------------------------------------------#
#Goodness of Fit test
#-------------------------------------------------------------------#

hosmerLemeshowTest                <- hoslem.test(modelObj$y, fitted(modelObj))

#-------------------------------------------------------------------#
#KS test
#-------------------------------------------------------------------#
ks_data                           <- cbind.data.frame(actual = modelObj$y,predicted = modelObj$fitted.values)

ks_data$rank                      <- group_rank(data = ks_data$predicted,
                                                order = 'descending',
                                                groups = 10,
                                                ties.method = 'average',
                                                na.last = 'keep')

total_event                       <- sum(ks_data$actual)
total_count                       <- length(ks_data$actual)
total_nonevent                    <- total_count - total_event

ks_grouped_data                   <- aggregate(x = ks_data$predicted,
                                          by = ks_data['rank'],
                                          FUN = mean)
colnames(ks_grouped_data)         <- c('rank','mean')

ks_grouped_data$count_event       <- aggregate(x = ks_data$actual,
                                               by = ks_data['rank'],
                                               FUN = function(x){
                                                      return (length(which(as.logical(x))))
                                                     })$x

ks_grouped_data$per_event         <- (ks_grouped_data$count_event/total_event)*100

ks_grouped_data$count_nonevent    <- aggregate(x = ks_data$actual,
                                            by = ks_data['rank'],
                                            FUN = function(x){
                                              return (length(which(!as.logical(x))))
                                            })$x

ks_grouped_data$per_nonevent      <- (ks_grouped_data$count_nonevent/total_nonevent)*100

ks_grouped_data$count_total       <- aggregate(x = ks_data$actual,
                                        by = ks_data['rank'],FUN = length)$x

ks_grouped_data$cum_per_event     <- cumsum(ks_grouped_data$per_event)

ks_grouped_data$cumm_per_nonevent <- cumsum(ks_grouped_data$per_nonevent)

ks_grouped_data$ks                <- abs(ks_grouped_data$cum_per_event - ks_grouped_data$cumm_per_nonevent)

ks_grouped_data$group             <- ks_grouped_data$rank + 1

ks_out                            <- ks_grouped_data[,c("group","count_event","count_nonevent","count_total","per_event","per_nonevent","cum_per_event","cumm_per_nonevent","ks")]

rm(ks_data)
rm(ks_grouped_data)

#-------------------------------------------------------------------#
#Lift Calculation
#-------------------------------------------------------------------#
individual_lift                   <- ks_grouped_data$per_event/10
percent_contacted                 <- ks_grouped_data$group * 10
lift                              <-  ks_grouped_data$cum_per_event/percent_contacted
lift                              <- data.frame(percent_contacted,lift,individual_lift)

#-------------------------------------------------------------------#
#Gains Calculation
#-------------------------------------------------------------------#

percent_contacted                 <- ks_grouped_data$group * 10
percent_positive_response         <- ks_grouped_data$cum_per_event
gains                             <- data.frame(percent_contacted,percent_positive_response)

#-------------------------------------------------------------------#
#ouput 
#-------------------------------------------------------------------#

parameterEstimate                 <- cbind.data.frame(parameterEstimate,varianceInflationFactor)
modelStatistic                    <- cbind.data.frame(Statistic = c('AIC','SC','Log Liklihood',names(concor_discor)),
                                                      Value = c(AIC,SC,log_L,concor_discor))

#-------------------------------------------------------------------#
#Gains Chart
#-------------------------------------------------------------------#

png(filename  = paste0(c_path_out,"/GainsChart.png"),
    width     = 12,
    height    = 6,
    units     = "in",
    res       = 100
)

rs <- qplot(x    = gains$percent_contacted,
            y    = gains$percent_positive_response,
            geom = "line",
            xlab = "Percent Contacted", 
            ylab = "Percent Positive Response") + theme(aspect.ratio = 1/2.5)
print({
  
  rs2 <- rs +  theme(axis.text.x=element_text(size=12, angle=40, vjust=.9, hjust=1.01))
})


dev.off()

#-------------------------------------------------------------------#
#Lift Chart
#-------------------------------------------------------------------#

png(filename  = paste0(c_path_out,"/LiftChart.png"),
    width     = 12,
    height    = 6,
    units     = "in",
    res       = 100
)

rs <- qplot(x    = lift$percent_contacted,
            y    = lift$lift,
            geom = "line",
            xlab = "Percent Contacted", 
            ylab = "Lift") + theme(aspect.ratio = 1/2.5)
print({
  
  rs2 <- rs +  theme(axis.text.x=element_text(size=12, angle=40, vjust=.9, hjust=1.01))
})


dev.off()
