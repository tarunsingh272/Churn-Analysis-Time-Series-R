#--------------------------------------------------------------#

library(data.table)
library(dplyr)

#--------------------------------------------------------------#

# call churn data pre-processing function

source("file_path//pre_process.R")

#--------------------------------------------------------------#

# load data 

churnTrain <- read.csv("file_path//churnTrain.csv", header = TRUE, strip.white = TRUE)
churnTest <- read.csv("file_path//churnTest.csv", header = TRUE, strip.white = TRUE)

#-----------------------------------------------------------#

# input data into function

Pre.Process.Churn(churnTest, churnTrain)

#-----------------------------------------------------------#

# export data
# write.csv(State.View, "state view.csv", row.names = FALSE)
# write.csv(Customer.View, "customer view.csv", row.names = FALSE)
