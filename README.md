###Linear Regression

* [Linear Regression](http://stattrek.com/regression/linear-regression.aspx)
* [Assumptions](http://www.statisticssolutions.com/assumptions-of-linear-regression/)
* [Variance Inflation Factor](https://onlinecourses.science.psu.edu/stat501/node/347)
* [T Test](http://stattrek.com/regression/slope-test.aspx?Tutorial=AP)
* [F Test](http://blog.minitab.com/blog/adventures-in-statistics/what-is-the-f-test-of-overall-significance-in-regression-analysis)
* [Adj. R Squared vs R Squared](http://stats.stackexchange.com/questions/52517/why-is-adjusted-r-squared-less-than-r-squared-if-adjusted-r-squared-predicts-the)
* [Mape](http://www.forecastpro.com/Trends/forecasting101August2011.html)
* [Interpreting Actual vs Predicted Plot](http://stats.stackexchange.com/questions/104622/what-does-an-actual-vs-fitted-graph-tell-us)
* [Interpreting Residual vs Predicted Plot](http://stats.stackexchange.com/questions/76226/interpreting-the-residuals-vs-fitted-values-plot-for-verifying-the-assumptions) 

File:LinearRegression.R

Required packages: car, lmtest, ggplot2

Input parameters: 

        c_path_in                       - path pointing to the input .csv file
        c_path_out                      - output folder path
        c_var_in_independent            - one ore more independent variable(s)
        c_var_in_dependent              - one dependent variable
                  
Outputs: 
          
        parameterEstimates (Data Frame) - Contains estimates of all the inpendent variables used for building the model &
                                          is exported as Estimates.csv to the location "c_path_out"
        modelStatistic (Data Frame)     - Contains various model statistics & is exported as ModelStatistic.csv to the location                                                 "c_path_out"
        durbinWatsonTest (List)         - Contains statistic used for testing autocorrelation
        goldfledQuantdtTest (List)      - Contains statistic used for testing homoscedasticity
        ActualPredicted.png file is exported to the location "c_path_out"
        ResidualPredicted.png file is exported to the location "c_path_out"
        
###Logistic Regression

* [Logistic Regression](http://vassarstats.net/logreg1.html)
* [Chi-Square Test](http://stattrek.com/chi-square-test/independence.aspx?Tutorial=AP)
* [Model Evaluation Metrics(Gains Chart, Lift Chart, K-S Test, Confusion Matrix, ROC)](http://www.analyticsvidhya.com/blog/2016/02/7-important-model-evaluation-error-metrics/)
* [Hosmer-Lemeshow Test](http://thestatsgeek.com/2014/02/16/the-hosmer-lemeshow-goodness-of-fit-test-for-logistic-regression/)

File: LogisticRegression.R

Required packages: car, ResourceSelection, ggplot2

Input parameters: 

	c_path_in                        - path pointing to the input .csv file
	c_path_out                       - output folder path
	c_var_in_independent             - one or more independent variables
	c_var_in_dependent               - one binary dependent variable
	x_val_event                      - level of dependent variable
		
Outputs: 

	parameterEstimate(Data Frame)    - Contains estimates of all the independent varaible used for building the model & is 							   exported as Estimate.csv file to the location "c_path_out"
	modelStatistic(Data Frame)       - Contains various model ststistics & is exported as ModelStatistic.csv to the location 						   "c_path_out"
	hosmerLemeshowTest(List)         - Contains statistic for testing goodness of fit
	ks_out(Data Frame)               - For measuring the performance of the classification model
	GainsChart.png is exported to the location "c_path_out"
	LiftChart.png is exported to the location "c_path_out"
	
File: LogisticModelAnalysis.R

Input parameters: 

	c_path_out                       - output folder path
	modelObj                         - Logistic Model Object
	
Description: 

Contains function for generating Confusion Matrix for all the cut points between 0.01 & 0.99.
ROC curve & Sensitivity - Specificity curve is plotted & exported as ROC.png & Sensitivity-Specificity.png respectively to the location "c_path_out"
		 
File: Distance.R

Description: 

Contains various method for calculating distance between two vectors
