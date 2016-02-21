file:LinearRegression.R
Required packages: car, lmtest, ggplot2
Input parameters: 
                  c_path_in - path pointing to the input .csv file
                  c_path_out - output folder path
                  c_var_in_independent - one ore more independent variable(s)
                  c_var_in_dependent - one dependent variable
Outputs: 
          parameterEstimates (Dataframe) - contains estimates of all the inpendent variables used for building the model &
                                           is exported as Estimates.csv to the location "c_path_out"
          modelStatistic (Dataframe)     - contains various model statistics & is exported as ModelStatistic.csv to the location "c_path_out"
          durbinWatsonTest (List)        - contains statistic used for testing autocorrelation
          goldfledQuantdtTest (List)     - contains statistic used for testing homoscedasticity
          ActualPredicted.png file is exported to the location "c_path_out"
          ResidualPredicted.png file is exported to the location "c_path_out"
