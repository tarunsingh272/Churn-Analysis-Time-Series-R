XGboost for Churn Analysis
================
Cincinnati Data
7/11/2017

Churn Analysis
--------------

This tutorial takes users through the use of regularized gradient boosting to predict customer churn in the telecom industry. Regularized gradient boosting, courtesy of the xgboost package, has been shown to be a highly effective classification and regression algorithm

First, let's load the relevatnt libraries and churn data provided as .csv files in the churn folder on the cincy-data GitHub page.

``` r
# libraries
library(xgboost)
library(qlcMatrix)
library(data.table)
library(ggplot2)
library(DiagrammeR)
library(leaps)
library(Ckmeans.1d.dp)

# read in csv files
churnTrain <- read.csv("file_path//churnTrain.csv", header = TRUE, strip.white = TRUE)
churnTest <- read.csv("file_path//churnTest.csv", header = TRUE, strip.white = TRUE)
```

Churn Review
------------

Now that the data sets are loaded, let's take a quick look at the churn breakdown in the training set.

``` r
# table of customer churn
table(churnTrain$churn)
```

    ## 
    ##   no  yes 
    ## 2850  483

``` r
# graph of customer churn
churn.graph <- ggplot(churnTrain, aes(x=churn, fill = ..count..)) + geom_bar()
churn.graph <- churn.graph + ylab("Number of Customers") + xlab("Customer Churn") + labs(title = "Breakdown of Customers by Churn")
churn.graph
```

![](XGboost_for_Churn_Analysis_files/figure-markdown_github/viz-graph1-1.png)

As we see, the majority of customers retain the telecom service. The challenge is to identify those customers who will drop the service.

Build XGboost Model
-------------------

We next build the regularized gradient boosted model on the training data.

``` r
# churn classification with xgboost

# drop state column
churnTrain$state <- NULL

# transform to sparse matrix
sparse_matrix <- sparse.model.matrix(churn ~ .-1, data = churnTrain)

# setting output vector
churnTrain$outputVector = 0
churnTrain$outputVector[churnTrain$churn == "yes"] = 1
outputVector <- churnTrain[, "outputVector"]

# building model
churn.bst <- xgboost(data = sparse_matrix, label = outputVector, max.depth = 10,
               eta = 1, nthread = 2, nround = 5, objective = "binary:logistic")
```

    ## [1]  train-error:0.034803 
    ## [2]  train-error:0.027603 
    ## [3]  train-error:0.022502 
    ## [4]  train-error:0.018602 
    ## [5]  train-error:0.012601

As expected, we observe a reduction in the training error for each of the five training rounds.

Test Accuracy of XGboost Model on Test Data
-------------------------------------------

Now that we have built our gradient boosted model, we test its performance by using it to predict churn in the test data set, and examining model predictions vs. actuals to determine model accuracy.

``` r
# apply trained xgboost model to test set

# save state colmn to bind after analysis
state <- data.frame(churnTest$state)

# drop state from test set
churnTest$state <- NULL

# saving test label
testLabel <- churnTest$churn

# transforming test to sparse
sparse_test_matrix <- sparse.model.matrix(churn~.-1, data=churnTest)

# grab label outcome for test vector
churnTest$outputVector = 0
churnTest$outputVector[churnTest$churn == "yes"] = 1
outputTestVector <- churnTest[, "outputVector"]

# making prediction on test data
pred <- predict(churn.bst, sparse_test_matrix)

# changing prediction to binary
prediction <- as.numeric(pred > 0.5)

# determine average model error
err <- mean(as.numeric(pred > 0.5) != outputTestVector)
print(sprintf("Model error is: %f", err))
```

    ## [1] "Model error is: 0.050990"

We see that the model error is about ~ 5%, which is exceptional accuracy.

Transform Data into Packaged Results
------------------------------------

Now that the model training and testing is complete, we want to transform the model results into a nicely packaged dataset which gives users a granular breakdown of the model's predictions.

To do this we add a variety of columns including prediction breakdown, which signifies the type of prediction, e.g. true positive, true negative.

``` r
# transforming data into packaged results

# adding in columns for final dataset export
model.probabilities <- data.frame(pred)
model.predictions <- data.frame(prediction)
model.predictions$prediction <- ifelse(model.predictions == 1, "yes", "no")
xgb.final <- cbind(churnTest, model.predictions, model.probabilities)
xgb.final$outputVector <- NULL
xgb.final$churn <- as.character(xgb.final$churn)
xgb.final$matching.prediction <- ifelse(xgb.final$churn == xgb.final$prediction, "match", 
                                        "no match")

# prediction breakdown
xgb.final$predict_breakdown <- ifelse(xgb.final$churn == "yes" & xgb.final$prediction == "yes", "True Positive", ifelse(xgb.final$churn == "yes" & xgb.final$prediction == "no", 
                                                                                                                        "False Negative", ifelse(xgb.final$churn == "no" & xgb.final$prediction == "no", "True Negative", "False Positive")))
# add back in state column
xgb.final <- cbind(state, xgb.final)


# rename columns
setnames(xgb.final, old = c("churnTest.state", "prediction", "pred", "matching.prediction", "predict_breakdown"), 
         new = c("State", "xgb model prediction", "xgb model probability of churn", "matching prediction", "prediction breakdown"))

# order columns
xgb.final <- xgb.final[,c(1,20,21,23,24,22,2:19)]
```

Analyzing Prediction Breakdown
------------------------------

We next present the granular prediction breakdown by reviewing model performance with respect to the prediction type (true positive, true negative, false positive, false negative).

``` r
# analyzing true positive & true negative predictive accuracy

# set total churn 
churn.total <- sum(xgb.final$churn=="yes")
churn.pred.correct <- sum(xgb.final$`prediction breakdown`=="True Positive")

# xgboost model correctly predicted churn
churn.accuracy.rate <- churn.pred.correct / churn.total
print(sprintf("the model accuracy with respect to accurately predicted churn is %f", churn.accuracy.rate))
```

    ## [1] "the model accuracy with respect to accurately predicted churn is 0.705357"

``` r
# set total retention
non.churn <- sum(xgb.final$churn=="no")
non.churn.pred <- sum(xgb.final$`prediction breakdown`=="True Negative")

# xgboost model correctly predicted 
retention.accuracy.rate <- non.churn.pred / non.churn
print(sprintf("the model accuracy with respect to accurately predicted retention is %f", retention.accuracy.rate))
```

    ## [1] "the model accuracy with respect to accurately predicted retention is 0.986833"

``` r
# graphing accuracy rates

# visualizing relative accuracy rates
accuracy.data <- data.frame(`Churn Category` = c("Retained", "Not Retained"), 
                            `Predictive Accuracy` = c(retention.accuracy.rate, churn.accuracy.rate))

accuracy.data$Churn.Category <- as.character(accuracy.data$Churn.Category)
accuracy.graph <- ggplot(accuracy.data, aes(x=Churn.Category, y=Predictive.Accuracy, fill = Churn.Category)) + geom_bar(stat = "identity")
accuracy.graph <- accuracy.graph + ylab("Predictive Accuracy") + xlab("Customer Class") + labs(title = "Predictive Accuracy with Resepct to Customer Class")
accuracy.graph
```

![](XGboost_for_Churn_Analysis_files/figure-markdown_github/viz-graph2-1.png)

As we expect, the model was more accurate with respect to predicting which customers would retain service than it was at predicting which customers would drop the service.

If our goal is to isolate customers who are expected to drop the service in the hopes of retaining them, we can adjust the churn prediction threshold from .5 to .4 or even lower so as to minimize false negatives. In this case, a higher false positive rate is preffered to high false negative rate.

Feature Importance
------------------

We next look at feauture importance, which is to say, analyze the variables used to train the model and their individual utility in obtaining accurate predictions.

``` r
# feature importance

# generating importance matrix
importance_matrix <-  xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = churn.bst)
head(importance_matrix)
```

    ##                          Feature       Gain      Cover  Frequency
    ## 1:             total_day_minutes 0.26772473 0.25351950 0.17192982
    ## 2:             total_eve_minutes 0.13807088 0.14148716 0.15789474
    ## 3: number_customer_service_calls 0.12049288 0.11761526 0.03508772
    ## 4:            total_intl_minutes 0.10316392 0.03603916 0.09824561
    ## 5:         international_planyes 0.08051292 0.10367143 0.04210526
    ## 6:              total_intl_calls 0.07940956 0.05256325 0.05614035

``` r
# generating plot that shows importance
xgb.ggplot.importance(importance_matrix = importance_matrix)
```

![](XGboost_for_Churn_Analysis_files/figure-markdown_github/viz-graph3-1.png)

In this case, we observe that total day minutes is clearly the most impactful input variable with respect to predicting customer churn.

Cross Validation Model Results
------------------------------

The initial train / test model split yielded highly accurate results. We next further benchmark model performance using five rounds of 10-fold cross validation.

``` r
# validate results with CV
churn.bst.CV <- xgb.cv(data = sparse_matrix, label = outputVector, max.depth = c(15),
                 eta = 1, nthread = 2, nround = 5, nfold = 10, objective = "binary:logistic",
                 prediction = TRUE)
```

    ## [1]  train-error:0.035570+0.000747   test-error:0.055501+0.009293 
    ## [2]  train-error:0.026869+0.001147   test-error:0.052802+0.011619 
    ## [3]  train-error:0.019569+0.000791   test-error:0.049799+0.011366 
    ## [4]  train-error:0.013268+0.001264   test-error:0.046201+0.009582 
    ## [5]  train-error:0.008067+0.001254   test-error:0.047401+0.010445

The results produce an optimized error rate of about ~ 5%, as did the initial train / test split.

Use to Model to Predict New Customers
-------------------------------------

Finally, we build a profile for two hypothetical customers and use our trained and validated model, which is now ready for production, to determine whether these two customers are expected to retain or drop the telecom service.

``` r
# predict new customer using best trained model
new.customer <- data.frame(account_length = c(100, 98), area_code = c("area_code_415", "area_code_408"), international_plan = c("yes", "no"), voice_mail_plan = c("yes", "no"), 
                           number_vmail_messages = c(20, 25), total_day_minutes=c(200, 195), total_day_calls=c(100, 95), total_day_charge=c(40, 45),
                           total_eve_minutes=c(200, 180), total_eve_calls=c(100, 90), total_eve_charge=c(20, 25), total_night_minutes=c(200, 190),
                           total_night_calls=c(100, 80), total_night_charge=c(10, 8), total_intl_minutes=c(15, 10), total_intl_calls=c(3, 2),
                           total_intl_charge=c(3, 1), number_customer_service_calls=c(2, 5))

# sparse matrix conversion
sparse_matrix_pred <- sparse.model.matrix(~.-1, data=new.customer)

# making prediction
probability <- predict(churn.bst, sparse_matrix_pred)

# changing prediction to binary
prediction <- as.numeric(probability > 0.5)

# creating data.frame for new predictions
final.results <- data.frame(new.customer, prediction, probability)
final.results$prediction <- ifelse(prediction==0, "no", "yes")
final.results$probability
```

    ## [1] 0.09372759 0.20611817

``` r
final.results$prediction
```

    ## [1] "no" "no"

The model returns probabilities of .09 and .2 respectively for the two hypothetical customers with respect to probability of dropping the service. Both customers are predicted "no" with respect to churn, meaning the model predicts they will be retained. As more data become available, the model can be retrained to boost predictive accuracy, all the while leading to exceptional business intelligence and improved customer retention.
