#-----------------------------------------------------------#

# Author:   Cincinnati Data
# Date:     June 19, 2017
# Name:     Pre_Process

#-----------------------------------------------------------#

# Functionality:  This function automates data pre-processing for Churn data
# Inputs:         Test and Train datasets  

#-----------------------------------------------------------#

# churn pre-processing function

Pre.Process.Churn <- function(test, train){
  
  # pre processing data
  
  # combine data
  Churn <- rbind(test, train)
  
  # convert factors to chacaters
  i <- sapply(Churn, is.factor)
  Churn[i] <- lapply(Churn[i], as.character)
  
  # create binary column to sum statistics by state
  Churn$binary.churn <- as.integer(ifelse(Churn$churn == "yes", 1, 0))
  Churn$binary.voice <- as.integer(ifelse(Churn$voice_mail_plan=="yes", 1,0))
  Churn$binary.international <- as.integer(ifelse(Churn$international_plan=="yes", 1,0))
  
  
  # aggregate observations by state
  churn.table <- data.table(Churn)
  churn.table <- churn.table[, .N, by = list(state)]
  setnames(churn.table, old = c("N"), new = c("Number of Customers"))
  
  # aggregate additional columns by state
  col.names <- data.frame(colnames(Churn))
  state.view <- aggregate(cbind(binary.international, binary.voice, binary.churn,account_length, 
                                number_vmail_messages, total_day_minutes, total_day_calls, 
                                total_day_charge, total_eve_minutes, total_eve_calls, 
                                total_eve_charge, total_night_minutes, total_night_calls, 
                                total_night_charge, total_intl_minutes, total_intl_calls, 
                                total_intl_charge, number_customer_service_calls)~state, 
                          data=Churn, sum, na.rm=TRUE)
  
  # merge for final churn data set
  churn <- merge(churn.table, state.view, by = c("state"), all.x = TRUE)
  
  # add in state specific columns
  churn$`percent churn` <- churn$binary.churn / churn$`Number of Customers`
  churn$`percent international plan` <- churn$binary.international / churn$`Number of Customers`
  churn$`percent voice mail plan` <- churn$binary.voice / churn$`Number of Customers`
  
  # add in averages per customer
  churn$`account length per customer` <- churn$account_length / churn$`Number of Customers`
  churn$`voice messages per customer` <- churn$number_vmail_messages / churn$`Number of Customers`
  churn$`service calls per customer` <- churn$number_customer_service_calls / churn$`Number of Customers`
  
  # add in day specific columns
  churn$`day calls per customer` <- churn$total_day_calls / churn$`Number of Customers`
  churn$`day charge per customer` <- churn$total_day_charge / churn$`Number of Customers`
  churn$`day minutes per customer` <- churn$total_day_minutes / churn$`Number of Customers`
  
  # evening specific columns
  churn$`evening min per customer` <- churn$total_eve_minutes / churn$`Number of Customers`
  churn$`evening calls per customer` <- churn$total_eve_calls / churn$`Number of Customers`
  churn$`evening charge per customer` <- churn$total_eve_charge / churn$`Number of Customers`
  
  # night specific columns
  churn$`night min per customer` <- churn$total_night_minutes / churn$`Number of Customers`
  churn$`night calls per customer` <- churn$total_night_calls / churn$`Number of Customers`
  churn$`night charge per customer` <- churn$total_night_charge / churn$`Number of Customers`
  
  # international specific columns
  churn$`int min per customer` <- churn$total_intl_minutes / churn$`Number of Customers`
  churn$`int calls per customer` <- churn$total_intl_calls / churn$`Number of Customers`
  churn$`int charge per customer` <- churn$total_intl_charge / churn$`Number of Customers`

  # rename data for export
  Churn$area_code <- NULL
  Customer.View <<- Churn
  State.View <<- churn
  
  # clear global environment
  rm(churnTrain, churnTest, pos = ".GlobalEnv")
  
  # notify end of function
  print("Data Pre-Processing Complete. The Customer.View and State.View datasets are ready for review.")
}


# end of function