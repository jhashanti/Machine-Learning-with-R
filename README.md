file:LinearRegression.R

Required packages: car, lmtest, ggplot2
Input parameters: 

                  c_path_in - path pointing to the input .csv file
                  c_path_out - output folder path
                  c_var_in_independent - one ore more independent variable(s)
                  c_var_in_dependent - one dependent variable
                  
Outputs: 
          
          parameterEstimates (Data Frame) - contains estimates of all the inpendent variables used for building the model &
                                            is exported as Estimates.csv to the location "c_path_out"
          modelStatistic (Data Frame)     - contains various model statistics & is exported as ModelStatistic.csv to the location                                                 "c_path_out"
          durbinWatsonTest (List)         - contains statistic used for testing autocorrelation
          goldfledQuantdtTest (List)      - contains statistic used for testing homoscedasticity
          ActualPredicted.png file is exported to the location "c_path_out"
          ResidualPredicted.png file is exported to the location "c_path_out"

file: LogisticRegression.R
Required packages: car, ResourceSelection, ggplot2
Input parameters: 

		c_path_in - path pointing to the input .csv file
		c_path_out - output folder path
		c_var_in_independent - one or more independent variables
		c_var_in_dependent - one binary dependent variable
		x_val_event - level of dependent variable
		
Outputs: 

	  parameterEstimate(Data Frame) - contains estimates of all the independent varaible used for building the model & is exported 					  as Estimate.csv file to the location "c_path_out"
	  modelStatistic(Data Frame)    - contains various model ststistics & is exported as ModelStatistic.csv to the location 						  "c_path_out"
	  hosmerLemeshowTest(List)      - contains statistic for testing goodness of fit
	  ks_out(Data Frame)            - for measuring the performance of the classification model
	  GainsChart.png is exported to the location "c_path_out"
	  LiftChart.png is exported to the location "c_path_out"
		 
file: Distance.R

Description: contains various method for calculating distance between two vectors
