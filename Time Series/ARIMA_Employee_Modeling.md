ARIMA Modeling to Predict Employee Requirement
================
Cincinnati Data
6/20/2017

Time Series Analysis for Employee Requirement
---------------------------------------------

This report presents methods for time series analysis in R. Specifically, we look at trends in employee requirement for a firm during FY16. Employees log hours each day. By taking time logged per day, extrapolating over an entire year, and finally dividing by available working hours, we can track employee requirement per day.

This analysis will allow firms to accurately gauge the amount of work, measured in full time employees, being logged each day. This analysis also allows firms to forecast future employee requirement so that resources can be allocated accordingly.

Plotting Employee Requirement
-----------------------------

The graph below visualizes employee requirement over 2016, and includes a trend line.

![](Visualization/viz-1-1.png)

Employee requirement appeared to follow an increasing pattern in 2016.

Anomaly Detection
-----------------

As 2016 came to a close, employee requirement increased significantly with single day entries for employee requirement far exceeding 1500 on several occasions. We next apply anomaly detection for evidence with respect to whether the peaks observed near the end of 2016 are anomalous, or rather, part of the general trend of increasing employee requirement.

![](ARIMA_Employee_Modeling_files/figure-markdown_github/viz-2-1.png)

Using a generous threshold of .1, only 2 points, or .55% of all observations, were considered anomalous. This provides some evidence that the increasing trend we observe in employee requirement, even the majority of the peaks, are indicative of a general increasing trend.

Auto Regressive Integrated Moving Average (ARIMA) for Time Series
-----------------------------------------------------------------

Instead of conducting time series analysis on the daily employee requirement values, which display considerable noise day to day, using the weekly or monthly moving averages provide a relatively smoothed set of observations in comparison. This is often preferable for generalizing trends. The graphs below visualize weekly and monthly ARIMA.

![](ARIMA_Employee_Modeling_files/figure-markdown_github/viz-3-1.png)

We next display weekly and monthly ARIMA seperately from the observed employee requirement observations.

![](ARIMA_Employee_Modeling_files/figure-markdown_github/viz-4-1.png)

The analysis in this report focuses primarily on monthly ARIMA, although weekly ARIMA and the observed values are also analyzed occasionally. As mentioned, monthly ARIMA provides smoothed estimates which allow for a clearer picture of the general increasing trend in employee requirement.

Trend Decomposition
-------------------

We next decompose monthly ARIMA into its underlying trends. These include the observed monthly ARIMA values, a generalized trend, a seasonal trend, and the remainder (noise) which is unexplanied by either the general or seasonal trends.

![](ARIMA_Employee_Modeling_files/figure-markdown_github/viz-5-1.png)

The generalized trend serves to further smooth monthly ARIMA. Additionally, some amount of variation in employee requirement remains unexplained by either the general increasing trend or seasonal variation.

Statistical Testing for Significant Increase in Employee Requirement, Stationarity, and Auto Correlation
--------------------------------------------------------------------------------------------------------

    ## 
    ##  Two Sample t-test
    ## 
    ## data:  half.1$`monthly requirement` and half.2$`monthly requirement`
    ## t = -31.371, df = 333, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -240.0494 -211.7211
    ## sample estimates:
    ## mean of x mean of y 
    ##  982.6229 1208.5081

There is evidence at the 1% level of significance that mean employee requirement was higher over the second half of 2016 than it was during the first half.

    ## 
    ##  Augmented Dickey-Fuller Test
    ## 
    ## data:  monthly.ARIMA
    ## Dickey-Fuller = -2.7194, Lag order = 12, p-value = 0.2732
    ## alternative hypothesis: stationary

We cannot reject non-stationarity. This is to say, we cannot reject that the series does not retain mean, variance, and auto-correlation over time. This is intuitive given the generally increasing pattern observed in the data. We did not expect stationarity in this series.

![](ARIMA_Employee_Modeling_files/figure-markdown_github/viz-6-1.png)

The data retain relatively high auto-correlation over time, which means that observations, even at a significant lag, are still correlated and potentially useful in determining the position for future values of employee requirement.

Forecasting
-----------

We next develop forecasting models to predict future values of employee requirement. We present maximum liklihood and drift models to forecast future employee requirement 30 days into 2017.

The first model visualized below is the max liklihood model.

![](ARIMA_Employee_Modeling_files/figure-markdown_github/viz-7-1.png)

    ## 
    ## Call:
    ## arima(x = deseasonal_cnt, order = c(1, 1, 4), method = "ML")
    ## 
    ## Coefficients:
    ##          ar1     ma1      ma2     ma3     ma4
    ##       0.9237  0.1133  -0.8364  0.0208  0.1433
    ## s.e.  0.0334  0.0628   0.0621  0.0585  0.0547
    ## 
    ## sigma^2 estimated as 12.46:  log likelihood = -896.36,  aic = 1804.73

The max liklihood model decreasingly increases over time, with functional form along the lines of k\*(1 / sqrt(x)).

The drift model is presented below.

![](ARIMA_Employee_Modeling_files/figure-markdown_github/viz-8-1.png)

    ## Series: deseasonal_cnt 
    ## ARIMA(0,1,1) with drift         
    ## 
    ## Coefficients:
    ##          ma1   drift
    ##       0.9382  1.1994
    ## s.e.  0.0171  0.3965
    ## 
    ## sigma^2 estimated as 14.11:  log likelihood=-915.92
    ## AIC=1837.85   AICc=1837.92   BIC=1849.28

The drift model follows a linear pattern of increase. Below, we visualize the drift model predictions vs. actuals using the final 25 days of the weekly ARIMA observations.

![](ARIMA_Employee_Modeling_files/figure-markdown_github/viz-9-1.png)

The drift model appears to appropriately model at least the last 25 weekly ARIMA observations.

Regression Analysis
-------------------

We next perform regression analysis, modeling employee requirement as a function of time, which we will eventaully use to produce forecasts for future employee requirement.

The first model presented below uses the daily logged employee requirement values to fit the regression.

![](ARIMA_Employee_Modeling_files/figure-markdown_github/viz-10-1.png)

The second regression below models employee requirement using monthly ARIMA.

![](ARIMA_Employee_Modeling_files/figure-markdown_github/viz-11-1.png)

We observe that the two linear regression models produce similar equations with respect to calculating employee requirement.

Regression 1: Employee Requirement = 884 + 1.23(Days), so f(100) = 1,007

Regression 2: Employee Requirement = 898 + 1.18(Days), so f(100) = 1,016

We can set the two equations equal to determine when they would produce identical predictions for employee requirement:

898 + 1.18(Days) = 884 + 1.23(Days)

14 = .05(Days)

280 = Days

So, the two regression equations will return identical predictions with respect to employee requirement approximately 280 days from the beginning of 2016.

We next include a quadratic term to more closely model the monthly ARIMA. The quadratic model preditions vs. monthly ARIMA is visualized below.

![](ARIMA_Employee_Modeling_files/figure-markdown_github/viz-12-1.png)

The quadratic model returns fitted values which are significantly closer to the monthly ARIMA values than do either of the linear models. However, the quadratic model predictions will begin to increase rapidly due to the quadratic term, and will eventually return estimates which are unrealistically high with respect to employee requirement.

Below we predict out employee requirement 400 days, 450 days, and 500 days from the start of 2016 for both linear models and the quadratic model.

    ##        1        2        3 
    ## 1374.837 1436.220 1497.603

    ##        1        2        3 
    ## 1369.066 1427.937 1486.809

    ##        1        2        3 
    ## 1518.136 1663.154 1824.931

As we see, the linear models return quite similar predictions while the quadratic model returns significantly higher estimates for employee requirement as we move through time.

Regularized Gradient Boosting (XGBoost) for Predicting Employee Requirement
---------------------------------------------------------------------------

We next select a non-parametric algorithm to model and predict employee requirement. We use the forecastxgb package and then build a custom regularized gradient boosted model to determine performance. The results of the forecastxgb model are visualized below.

![](ARIMA_Employee_Modeling_files/figure-markdown_github/viz-13-1.png)

While the gradient boosted model picks up on the seasonal fluctuations presented using weekly ARIMA, if we extrapolate out far enough the predictions converge on a single value. This may not be problematic since we would plan on updating our models as new data become available. However, if preditions far into the future are required, the forecastxgb model presents challenges.

We next train and test a custom built gradient boosted model. As you see, the model almost perfectly follows the monthly ARIMA values.

    ## [1]  train-rmse:63.495697 
    ## [2]  train-rmse:12.162457 
    ## [3]  train-rmse:5.922649 
    ## [4]  train-rmse:3.816701 
    ## [5]  train-rmse:2.588365

![](ARIMA_Employee_Modeling_files/figure-markdown_github/viz-14-1.png)

Unfortunately, however, the model is unable to pick up the increasing trend in employee requirement and returns converged predictions for all future dates. Using this model for future employee requirement predictions is not appropriate.

Step Function
-------------

We next build and visualize a custom step function over three domains of time. The model is visualized below.

![](ARIMA_Employee_Modeling_files/figure-markdown_github/viz-15-1.png)

The step function is more flexible in its predictions than are the linear models. Also, unlike the gradient boosted model, the step function can be used effectively for predicting future employee requirement. Since the general trend in employee requirement is increasing in nature, we review the predictions of the regression over the middle domain and the final domain of time. We also consider a combination of those two models and use each of those results to create predictions for future employee requirement later on.

Model Comparison
----------------

Below, we compare the model performance of each algorithm.

![](ARIMA_Employee_Modeling_files/figure-markdown_github/viz-16-1.png)

Predicting Future Employee Requirement
--------------------------------------

Lastly, we use each of the models we built during the course of this report to predict future employee requirement 100 days after the last observed monthly ARIMA value.

![](Visualization/viz-17-1.png)

The predictions range from a low of about 1400 to a high of about 1620. When data for the 100 day period of predictions becomes available, we will calculate monthly ARIMA values and compare each model's predictions to the actual observed monthly ARIMA values. We will then select the best performing algorithms to include in custom function models which will return accurate forecasts for employee requirement, giving firms an unparalleled level of business intelligence.

Response to Possible Concners
-----------------------------

One concern is that employee requirement can be modeled as a function of staffing count. When more staff come on board, employee requirement increases because the new staff are logging time, which we in turn use to calculate employee requirement This could lead to a situation in which employee requirement vs staffing count shortfall / surplus never significantly decreases.

-   This is unlikely the case. Workload is in flux, but theoreticall there is some amount of total workload that firms are responsible for completing. If firms were to hire exactly enough staff to complete their workload duing their available hours, hiring additional staff would not then increase workload, and additional hires would instead serve to redistribute and reduce the amount of work per employee.

The second concern is that we cannot be sure how accurate our predictions for future employee requirement will prove to be. We have strong evidence that we have observed an increase in employee requirement over time. However, it may be argued we are speculating that the trend will continue indefinitely.

-   We are prepared to actively test the validity of our predictive models. We will do this by comparing our predictions to actual observations. We have visualized our next 100 predictions and will test their respective performance when validation data become available. When we observe that employee requirement no longer increases, or increases at different rates than at present, we will simply adjust our models accordingly. The advantage to using machine learning techniques is that the algorithms learn patterns through time with additional data. This is exactly why we call them 'learning algorithms.'

A final concern may arise over the act of considering emlpoyee requirement to be a function of time. The processes and drivers that lead to employee requirement are impossibly complex, and at first glance it may seem we are excluding many of the variables that are relevant to this analysis.

-   In fact, drawing on the principles of dynamical systems, time acts as a catch all for every observed and unobserved variable that collectively drive employee requirement. Considering employee requirement to be a function of time drastically reduces the complexity of what would otherwise be a nearly infinitely complex model. Even though we cannot hope to identify each driver of employee requirement, the effects of each are nonetheless captured by the passage of time, and are therefore included in the model.

                                                     End of Report
