Churn Analysis with Neural Networks
================
Cincinnati Data
6/20/2017

Churn Analysis
--------------

This report analyzes neural network performance for classifying customer churn. Finding new customers is typically more difficult than retaining existing customers. By properly identifying existing customers who are expected to drop service, firms can formalize strategies to better retain them.

We will examine a neural network classifier to predict customer churn from one particular firm. We load the appropriate libraries and the data below.

``` r
library(neuralnet)
library(caTools)
library(mlbench)
library(caret)
library(e1071)

#---------------------------------------------------------------#

# load data

# read in csv files
churn.1 <- read.csv("//Users//paullovachykostoff//Desktop//Churn//R Data//churnTrain.csv", header = TRUE, strip.white = TRUE)
churn.2 <- read.csv("//Users//paullovachykostoff//Desktop//Churn//R Data//churnTest.csv", header = TRUE, strip.white = TRUE)
```

Scaling Variables
-----------------

The libraries and data are now loaded. We next combine the two data sets and scale the variables.

``` r
# combine data
churn <- rbind(churn.1, churn.2)

# remove train and test splits from global environment
rm(churn.1, churn.2)

# reorder columns 
churn <- churn[c(20,1,3,4,5,2,6:19)]

# create vectors of column max and min Values
maxs <- apply(churn[,6:20], 2, max)
mins <- apply(churn[,6:20], 2, min)

# use scale() and convert the resulting matrix to a data frame
scaled.data <- as.data.frame(scale(churn[,6:20], center = mins, scale = maxs - mins))
```

Train / Test Split
------------------

We choose a 70% / 30% train and test split for algorithm evaluation.

``` r
# convert churn column from Yes/No to 1/0
churn.col <- as.numeric(churn$churn)-1
data <- cbind(churn.col, scaled.data)

# set random seed
set.seed(101)

# create split - you can choose any column 
split <- sample.split(data$churn.col, SplitRatio = 0.70)

# cplit based off of split Boolean Vector
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)
```

Build Model
-----------

We next build the model to train and test on the data splits.

``` r
# grab names for variables
features <- names(scaled.data)

# concatenate strings
nnet <- paste(features,collapse = ' + ')
nnet <- paste('churn.col ~' ,nnet)

# convert to formula
nnet <- as.formula(nnet)

# review form - this is how R takes output and input variables with most models
nnet
```

    ## churn.col ~ account_length + number_vmail_messages + total_day_minutes + 
    ##     total_day_calls + total_day_charge + total_eve_minutes + 
    ##     total_eve_calls + total_eve_charge + total_night_minutes + 
    ##     total_night_calls + total_night_charge + total_intl_minutes + 
    ##     total_intl_calls + total_intl_charge + number_customer_service_calls

Train and Test Neural Network Model
-----------------------------------

We next train, test, and build a dataframe of model predictions vs actuals.

``` r
# run the neural network using training data ~ note hidden layer structure
nn <- neuralnet(nnet, train, hidden=c(5, 5, 5), linear.output=FALSE)

# compute predictions from test set
predicted.nn.values <- compute(nn, test[2:16])

# create data frame of node outputs and isolate individual probabilities
probs <- data.frame(predicted.nn.values)
probs$pred <- ifelse(probs$net.result > .5, 1, 0)

# grab prediction columns
probabilities <- subset(probs, select=c("net.result", "pred"))

# cbind predictions columns to test data
output.data <- cbind(test, probabilities)

# create match and prediction breakdown columns
output.data$match <- ifelse(output.data$churn.col == output.data$pred, "yes", "no")
output.data$`prediction breakdown` <- ifelse(output.data$churn.col == 1 & output.data$pred == 1, "true positive",
                                             ifelse(output.data$churn.col == 1 & output.data$pred == 0, "false negative",
                                                    ifelse(output.data$churn.col == 0 & output.data$pred == 1, "false positive",
                                                           "true negative")))
                                                                      
# reorganize columns
output.data <- output.data[c(1,18:20,17,2:16)]
```

Model Performance
-----------------

After the model has run, we want to gauge its performance. Below, the results are displayed in a confusion matrix.

``` r
# overall model accuracy and granular view - same as prediction breakdown column
model.accuracy <- sum(output.data$match == "yes") / nrow(output.data)
conf.mat <- confusionMatrix(test$churn.col, output.data$pred)
conf.mat
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 1259   29
    ##          1   78  134
    ##                                                 
    ##                Accuracy : 0.9286667             
    ##                  95% CI : (0.9144499, 0.9411755)
    ##     No Information Rate : 0.8913333             
    ##     P-Value [Acc > NIR] : 0.0000005740809       
    ##                                                 
    ##                   Kappa : 0.6746982             
    ##  Mcnemar's Test P-Value : 0.0000034784457       
    ##                                                 
    ##             Sensitivity : 0.9416604             
    ##             Specificity : 0.8220859             
    ##          Pos Pred Value : 0.9774845             
    ##          Neg Pred Value : 0.6320755             
    ##              Prevalence : 0.8913333             
    ##          Detection Rate : 0.8393333             
    ##    Detection Prevalence : 0.8586667             
    ##       Balanced Accuracy : 0.8818732             
    ##                                                 
    ##        'Positive' Class : 0                     
    ## 

The overall model accuracy is ~ .93. For a more granular breakdown, the model was accurate at a rate of .98 with repsect to customers who are expected to be retained, and a rate .63 with respect to customers who are expected to drop the service. Since identifying customers expected to leave is of primary importance, one method to capture more of the customers expected to drop the service is to drop the threshold of prediction from .5 to a lower value. This will serve to reduce model accuracy on the whole, but will allow us to capture more of the customers expected to drop service.

Visualze Neural Network
-----------------------

The last step is to visualize the neural network model used to predict churn.

``` r
# visualize neural network 
plot(nn, arrow.length = .165, dimension = 20, show.weights = TRUE, fontsize = 6)
```
![](Graphics/NN.Plot.png?raw=true)
