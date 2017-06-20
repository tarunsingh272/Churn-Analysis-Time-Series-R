#-----------------------------------------------------------------------#

library(xgboost)
library(qlcMatrix)
library(data.table)
library(ggplot2)
library(DiagrammeR)
library(leaps)
library(Ckmeans.1d.dp)

#-----------------------------------------------------------------------#

# load data

# read in csv files
churnTrain <- read.csv("//Users//paullovachykostoff//Desktop//Churn//R Data//churnTrain.csv", header = TRUE, strip.white = TRUE)
churnTest <- read.csv("//Users//paullovachykostoff//Desktop//Churn//R Data//churnTest.csv", header = TRUE, strip.white = TRUE)

#-----------------------------------------------------------------------#

# churn review

# table of customer churn
table(churnTrain$churn)

# graph of customer churn
churn.graph <- ggplot(churnTrain, aes(x=churn, fill = ..count..)) + geom_bar()
churn.graph <- churn.graph + ylab("Number of Customers") + xlab("Customer Churn") + labs(title = "Breakdown of Customers by Churn")
churn.graph

#-----------------------------------------------------------------------#

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

#-----------------------------------------------------------------------#

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

#-----------------------------------------------------------------------#

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

#-----------------------------------------------------------------------#

# analyzing true positive & true negative predictive accuracy

# set total churn 
churn.total <- sum(xgb.final$churn=="yes")
churn.pred.correct <- sum(xgb.final$`prediction breakdown`=="True Positive")

# xgboost model correctly predicted churn
churn.accuracy.rate <- churn.pred.correct / churn.total
print(sprintf("the model accuracy with respect to accurately predicted churn is %f", churn.accuracy.rate))

# set total retention
non.churn <- sum(xgb.final$churn=="no")
non.churn.pred <- sum(xgb.final$`prediction breakdown`=="True Negative")

# xgboost model correctly predicted 
retention.accuracy.rate <- non.churn.pred / non.churn
print(sprintf("the model accuracy with respect to accurately predicted retention is %f", retention.accuracy.rate))

#-----------------------------------------------------------------------#

# graphing accuracy rates

# visualizing relative accuracy rates
accuracy.data <- data.frame(`Churn Category` = c("Retained", "Not Retained"), 
                            `Predictive Accuracy` = c(retention.accuracy.rate, churn.accuracy.rate))

accuracy.data$Churn.Category <- as.character(accuracy.data$Churn.Category)
accuracy.graph <- ggplot(accuracy.data, aes(x=Churn.Category, y=Predictive.Accuracy, fill = Churn.Category)) + geom_bar(stat = "identity")
accuracy.graph <- accuracy.graph + ylab("Predictive Accuracy") + xlab("Customer Class") + labs(title = "Predictive Accuracy with Resepct to Customer Class")
accuracy.graph

#-----------------------------------------------------------------------#

# feature importance

# generating importance matrix
importance_matrix <-  xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = churn.bst)
head(importance_matrix)

# generating plot that shows importance
xgb.ggplot.importance(importance_matrix = importance_matrix)

#-----------------------------------------------------------------------#

# validate results with 10 fold CV

# validate results with CV
churn.bst.CV <- xgb.cv(data = sparse_matrix, label = outputVector, max.depth = c(15),
                 eta = 1, nthread = 2, nround = 5, nfold = 10, objective = "binary:logistic",
                 prediction = TRUE)

#-----------------------------------------------------------------------#

# run model on hypothetical customers

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
final.results$prediction

# end of script