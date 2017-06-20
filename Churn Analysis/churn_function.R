#--------------------------------------------------------------#

library(data.table)
library(dplyr)

#--------------------------------------------------------------#

# call churn data pre-processing function

source("//Users//paullovachykostoff//Desktop//Churn//R Scripts//pre_process.R")

#--------------------------------------------------------------#

# load data 

churnTrain <- read.csv("//Users//paullovachykostoff//Desktop//Churn//R Data//churnTrain.csv", header = TRUE, strip.white = TRUE)
churnTest <- read.csv("//Users//paullovachykostoff//Desktop//Churn//R Data//churnTest.csv", header = TRUE, strip.white = TRUE)

#-----------------------------------------------------------#

# input data into function

Pre.Process.Churn(churnTest, churnTrain)

#-----------------------------------------------------------#

# export data
# write.csv(State.View, "state view.csv", row.names = FALSE)
# write.csv(Customer.View, "customer view.csv", row.names = FALSE)
