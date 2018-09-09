library(stringr)
library(sqldf)
library(xgboost)
library(Metrics)
library(rBayesianOptimization)
library(ModelMetrics)

#Defining Functions
{
  
  xgboost_bays_opt <- function(maxdepth,nround,minchildweight,eta_){  
    
    library(xgboost)
    library(ModelMetrics)
    
    maxdepth <- round(maxdepth,0)
    nround <- round(nround,0)
    minchildweight <- round(minchildweight,0)
    
    gc()
    
    temporary_model <- xgboost(data = data.matrix(train)
                               ,nrounds = nround
                               ,max.depth = maxdepth
                               ,eta = eta_
                               ,min_child_weight = minchildweight
                               ,label = data.matrix(label_train)
                               ,verbose = FALSE
                               ,nthread = 1)
    
    predictions <- predict(temporary_model, newdata = data.matrix(test))
    
    #pos <- which(is.na(label_test))
    
    list(Score = 1/rmse(label_test,predictions),
         Pred = predictions)
    
  }
  
  
}

#Loading in Useful Postcode Ranges
{
Postcode_Ranges <- read.csv("C:/Users/lukeg/Documents/R - Projects/20180908-GovHack-V8/Useful-Postcode-Ranges-Simplified.csv", header = FALSE, stringsAsFactors = FALSE)
names(Postcode_Ranges) <- c("Postcode Ranges","Postcodes")

New_List <- list()
for (i in 1:nrow(Postcode_Ranges)){
  
New_List[length(New_List)+1] <- strsplit(Postcode_Ranges[i,2],",")

for (n in 1:length(New_List[[length(New_List)]])){
New_List[[length(New_List)]][n] <- str_replace( New_List[[length(New_List)]][n] ," ", "")
}

}


for ( v in 1:length(New_List) ){

for ( i in 1:length(New_List[[v]])  ){
  
  if ( grepl("-",New_List[[v]][i])  ){ 
    
  Temp <- as.character( seq( from = (as.numeric(unlist(strsplit( New_List[[v]][i] , "-" ))))[1]
         , to = (as.numeric(unlist(strsplit( New_List[[v]][i] , "-" ))))[2]) )
  
  for (p in 1:length(Temp)){
  New_List[[v]][length(New_List[[v]])+1] <- Temp[p]
  }
  
  }

}

New_List[[v]] <- New_List[[v]][!grepl("-",New_List[[v]])] 

}

#New_List <- as.numeric(New_List)

New_Matrix <- data.frame()

for (p in 1:length(New_List)){

for (i in 1:length(New_List[[p]])){
New_Matrix[i,1] <- Postcode_Ranges[p,1]
New_Matrix[i,2] <- New_List[[p]][i]
}

}

Useful_Postcode_Ranges <- New_Matrix

rm(New_Matrix, Postcode_Ranges, i, n, New_List, p, Temp, v)

names(Useful_Postcode_Ranges) <- c("Postcode Ranges","Postcodes")
}

#atoabsgovhack2018
{
  
  atoabsgovhack2018_ABS_Data <- read.csv("C:/Users/lukeg/Documents/R - Projects/20180908-GovHack-V8/atoabsgovhack2018_ABS_Data.csv", header = T, stringsAsFactors = FALSE)
  atoabsgovhack2018_ABS_SEIFA <- read.csv("C:/Users/lukeg/Documents/R - Projects/20180908-GovHack-V8/atoabsgovhack2018_ABS_SEIFA.csv", header = T, stringsAsFactors = FALSE)
  atoabsgovhack2018_ATO_Data <- read.csv("C:/Users/lukeg/Documents/R - Projects/20180908-GovHack-V8/atoabsgovhack2018_ATO_Data.csv", header = T, stringsAsFactors = FALSE)
  atoabsgovhack2018_Tax_Help_Center <- read.csv("C:/Users/lukeg/Documents/R - Projects/20180908-GovHack-V8/atoabsgovhack2018_Tax_Help_Center.csv", header = T, stringsAsFactors = FALSE)
  atoabsgovhack2017_Data <- read.csv("C:/Users/lukeg/Documents/R - Projects/20180908-GovHack-V8/atoabsgovhack2017_csv.csv", header = T, stringsAsFactors = FALSE)
    
  names(atoabsgovhack2018_ABS_Data) <- gsub('\\.', '_', names(atoabsgovhack2018_ABS_Data))
  names(atoabsgovhack2018_ABS_SEIFA) <- gsub('\\.', '_', names(atoabsgovhack2018_ABS_SEIFA))
  names(atoabsgovhack2018_ATO_Data) <- gsub('\\.', '_', names(atoabsgovhack2018_ATO_Data))
  names(atoabsgovhack2018_Tax_Help_Center) <- gsub('\\.', '_', names(atoabsgovhack2018_Tax_Help_Center))
  names(atoabsgovhack2017_Data) <- gsub('\\.', '_', names(atoabsgovhack2017_Data))
  
  #atoabsgovhack2018_ABS_Data <- rbind(atoabsgovhack2018_ABS_Data,atoabsgovhack2017_Data[,c(1:3,18:56)])
  #atoabsgovhack2018_ATO_Data <- rbind(atoabsgovhack2017_Data[,c(1:17)])
  
}

#Australian_Post_Codes_Lat_Lon
{
  Australian_Post_Codes_Lat_Lon <- read.csv("C:/Users/lukeg/Documents/R - Projects/20180908-GovHack-V8/Australian_Post_Codes_Lat_Lon.csv", header = T, stringsAsFactors = FALSE)
  names(Australian_Post_Codes_Lat_Lon) <- gsub('\\.', '_', names(Australian_Post_Codes_Lat_Lon))
  
  Australian_Post_Codes_Lat_Lon <- Australian_Post_Codes_Lat_Lon[,-which(names(Australian_Post_Codes_Lat_Lon) == "suburb")]

  Australian_Post_Codes_Lat_Lon <- Australian_Post_Codes_Lat_Lon[,c("postcode","lat","lon")]
  
  Australian_Post_Codes_Lat_Lon <- sqldf("SELECT
        postcode
        ,AVG(lat) AS lat
        ,AVG(lon) AS lon
        FROM Australian_Post_Codes_Lat_Lon
        GROUP BY postcode" )
  
}

#Data Joining / Transforming
{

for (z in 1:3){
    
#Join Data Together
{
  
  Unique_PostCodes <- cbind.data.frame(unique(atoabsgovhack2018_ABS_Data$Postcode),1:length(unique(atoabsgovhack2018_ABS_Data$Postcode)))
  names(Unique_PostCodes) <- c("Unique_PostCodes","Index")

  if (z == 1){
  Data <- sqldf(" SELECT *
                FROM Unique_PostCodes AS T1
                LEFT OUTER JOIN atoabsgovhack2018_Tax_Help_Center AS T2
                ON T1.Unique_PostCodes = T2.Post_Code
                LEFT OUTER JOIN atoabsgovhack2018_ABS_Data AS T3
                ON T1.Unique_PostCodes = T3.Postcode
                LEFT OUTER JOIN atoabsgovhack2018_ABS_SEIFA AS T4
                ON T1.Unique_PostCodes = T4.Postal_Area__POA__Code
                LEFT OUTER JOIN atoabsgovhack2018_ATO_Data AS T5
                ON T1.Unique_PostCodes = T5.Postcode
                LEFT OUTER JOIN Useful_Postcode_Ranges AS T6
                ON T1.Unique_PostCodes = T6.Postcodes
                LEFT OUTER JOIN Australian_Post_Codes_Lat_Lon AS T7
                ON T1.Unique_PostCodes = T7.postcode
                WHERE T3.Income_year = '2016' 
                AND Year = '2016'
                AND T5.Income_year = '2016'
                ")
  }
  
  if (z == 2){
    Data <- sqldf(" SELECT *
                FROM Unique_PostCodes AS T1
                LEFT OUTER JOIN atoabsgovhack2018_Tax_Help_Center AS T2
                ON T1.Unique_PostCodes = T2.Post_Code
                LEFT OUTER JOIN atoabsgovhack2018_ABS_Data AS T3
                ON T1.Unique_PostCodes = T3.Postcode
                LEFT OUTER JOIN atoabsgovhack2018_ABS_SEIFA AS T4
                ON T1.Unique_PostCodes = T4.Postal_Area__POA__Code
                LEFT OUTER JOIN atoabsgovhack2018_ATO_Data AS T5
                ON T1.Unique_PostCodes = T5.Postcode
                LEFT OUTER JOIN Useful_Postcode_Ranges AS T6
                ON T1.Unique_PostCodes = T6.Postcodes
                LEFT OUTER JOIN Australian_Post_Codes_Lat_Lon AS T7
                ON T1.Unique_PostCodes = T7.postcode
                WHERE T3.Income_year = '2011' 
                AND Year = '2011'
                AND T5.Income_year = '2011'
                ")
  }
  
  if (z == 3){
    Data <- sqldf(" SELECT *
                FROM Unique_PostCodes AS T1
                LEFT OUTER JOIN atoabsgovhack2018_Tax_Help_Center AS T2
                ON T1.Unique_PostCodes = T2.Post_Code
                LEFT OUTER JOIN atoabsgovhack2018_ABS_Data AS T3
                ON T1.Unique_PostCodes = T3.Postcode
                LEFT OUTER JOIN atoabsgovhack2018_ABS_SEIFA AS T4
                ON T1.Unique_PostCodes = T4.Postal_Area__POA__Code
                LEFT OUTER JOIN atoabsgovhack2018_ATO_Data AS T5
                ON T1.Unique_PostCodes = T5.Postcode
                LEFT OUTER JOIN Useful_Postcode_Ranges AS T6
                ON T1.Unique_PostCodes = T6.Postcodes
                LEFT OUTER JOIN Australian_Post_Codes_Lat_Lon AS T7
                ON T1.Unique_PostCodes = T7.postcode
                WHERE T3.Income_year = '2006' 
                AND Year = '2006'
                AND T5.Income_year = '2006'
                ")
  }
  
Data$Count[which(is.na(Data$Count))] <- 0
  
Columns_To_Remove <- c(which(names(Data) == "Postcode")
                       ,which(names(Data) == "Postal_Area__POA__Code")
                       ,which(names(Data) == "Postcodes")
                       ,which(names(Data) == "postcode")
                       ,which(names(Data) == "id") 
                       ,which(names(Data) == "Year")
                       ,which(names(Data) == "id.1")
                       ,which(names(Data) == "Income_year.1")
                       ,which(names(Data) == "Post_Code")
                       ,which(names(Data) == "Index")
                       )


Data <- Data[,-Columns_To_Remove]

}

#Column Conversions
{

  Data$Index_of_Economic_Resources <- as.numeric(Data$Index_of_Economic_Resources)
  Data$Index_of_Education_and_Occupation <- as.numeric(Data$Index_of_Education_and_Occupation)
  Data$Index_of_Relative_Socio_economic_Advantage_and_Disadvantage <- as.numeric(Data$Index_of_Relative_Socio_economic_Advantage_and_Disadvantage)
  Data$Index_of_Relative_Socio_economic_Disadvantage <- as.numeric(Data$Index_of_Relative_Socio_economic_Disadvantage)
  
  for (i in 1:nrow(Data)){
  Data[i,which(names(Data) == "Individuals1")] <- gsub(",","",Data[i,which(names(Data) == "Individuals1")])
  Data[i,which(names(Data) == "Taxable_income_or_loss1")] <- gsub(",","",Data[i,which(names(Data) == "Taxable_income_or_loss1")])
  Data[i,which(names(Data) == "Net_tax")] <- gsub(",","",Data[i,which(names(Data) == "Net_tax")])
  Data[i,which(names(Data) == "Gross_interest")] <- gsub(",","",Data[i,which(names(Data) == "Gross_interest")])
  Data[i,which(names(Data) == "Net_rent")] <- gsub(",","",Data[i,which(names(Data) == "Net_rent")])
  Data[i,which(names(Data) == "Net_capital_gain")] <- gsub(",","",Data[i,which(names(Data) == "Net_capital_gain")])
  Data[i,which(names(Data) == "Total_income_or_loss")] <- gsub(",","", Data[i,which(names(Data) == "Total_income_or_loss")])
  Data[i,which(names(Data) == "Total_deductions")] <- gsub(",","", Data[i,which(names(Data) == "Total_deductions")])
  Data[i,which(names(Data) == "Salary_or_wages")] <- gsub(",","", Data[i,which(names(Data) == "Salary_or_wages")])
  Data[i,which(names(Data) == "Medicare_levy")] <- gsub(",","", Data[i,which(names(Data) == "Medicare_levy")])
  Data[i,which(names(Data) == "Medicare_levy_surcharge")] <- gsub(",","", Data[i,which(names(Data) == "Medicare_levy_surcharge")])
  Data[i,which(names(Data) == "Total_work_related_expenses")] <- gsub(",","", Data[i,which(names(Data) == "Total_work_related_expenses")])
  Data[i,which(names(Data) == "Gifts_or_donations")] <- gsub(",","", Data[i,which(names(Data) == "Gifts_or_donations")])
  Data[i,which(names(Data) == "HELP_assessment_debt2")] <- gsub(",","", Data[i,which(names(Data) == "HELP_assessment_debt2")])
  }
  
  
  Data$Individuals1 <- as.numeric(Data$Individuals1)
  Data$Taxable_income_or_loss1 <- as.numeric(Data$Taxable_income_or_loss1)
  Data$Net_tax <- as.numeric(Data$Net_tax)
  Data$Gross_interest <- as.numeric(Data$Gross_interest)
  Data$Net_rent <- as.numeric(Data$Net_rent)
  Data$Net_capital_gain <- as.numeric(Data$Net_capital_gain)
  Data$Total_income_or_loss <- as.numeric(Data$Total_income_or_loss)
  Data$Total_deductions <- as.numeric(Data$Total_deductions)
  Data$Salary_or_wages <- as.numeric(Data$Salary_or_wages)
  Data$Medicare_levy <- as.numeric(Data$Medicare_levy)
  Data$Medicare_levy_surcharge <- as.numeric(Data$Medicare_levy_surcharge)
  Data$Total_work_related_expenses <- as.numeric(Data$Total_work_related_expenses)
  Data$Gifts_or_donations <- as.numeric(Data$Gifts_or_donations)
  Data$HELP_assessment_debt2 <- as.numeric(Data$HELP_assessment_debt2)
  
}
  
#Combining
  
  if (z == 1){Data_Storage <- NULL}
  Data_Storage <- rbind(Data_Storage,Data)
    
}
  
Data <- Data_Storage  

Data <- sqldf("SELECT * FROM Data AS T1
      LEFT OUTER JOIN Australian_Post_Codes_Lat_Lon AS T2
      ON T1.Unique_PostCodes = T2.postcode")



}

#xgboost
{

  column_to_predict <- 2
  columns_to_train <- c(1,3:62,64:65)
  
  #training_size = round(length(Data[,1])*0.8)
  #rows_to_train_on <- sample(1:nrow(Data), training_size)
  
  rows_to_train_on <- c(which(Data$Income_year == '2006'),which(Data$Income_year == '2011'))
  
  test = Data[-rows_to_train_on,columns_to_train]
  label_test = Data[-rows_to_train_on,column_to_predict]
  
  train = Data[rows_to_train_on,columns_to_train]
  label_train = Data[rows_to_train_on,column_to_predict]
  
  # optimised_hyperparameters <- BayesianOptimization(xgboost_bays_opt,
  #                                 bounds = list(
  #                                               maxdepth = c(1,30)
  #                                               ,nround = c(1,200)
  #                                               ,minchildweight = c(1,20)
  #                                               ,eta_ = c(0.01,1)
  #                                               ),
  #                                 init_points = 20, n_iter = 50,
  #                                 acq = "ucb", kappa = 2.576, eps = 0.0,
  #                                 verbose = TRUE,
  #                                 nug_thres = 10)
  # 
  # set.seed(1)
  # bst <- xgboost(data = as.matrix(train), label=label_train
  #                ,max.depth = round(optimised_hyperparameters$Best_Par[1],0)
  #                ,eta = 0.1
  #                ,nround = round(optimised_hyperparameters$Best_Par[2],0)
  #                ,min_child_weight = round(optimised_hyperparameters$Best_Par[3],0)
  #                ,subsample = optimised_hyperparameters$Best_Par[4]
  #                ,gamma = optimised_hyperparameters$Best_Par[5]
  #                ,nthread = 8
  #                ,objective = "reg:linear")
  
  #Best Parameters
  set.seed(1)
  bst <- xgboost(data = as.matrix(train), label=label_train
                 ,max.depth = 10
                 ,eta = 0.2130
                 ,nround = 180
                 ,min_child_weight = 10
                 ,nthread = 8
                 ,objective = "reg:linear")
  
  # set.seed(1)
  # bst <- xgboost(data = as.matrix(train), label=label_train, max.depth = 6, eta = 0.1, nround = 100, gamma = 1,
  #                colsample_bytree = 1, min_child_weight = 0, subsample = 1, max_delta_step = 0,
  #                nthread = 8, objective = "reg:linear")
  
  preds = predict(bst, as.matrix(test))
  
  rmse(label_test, preds)
  
  Results <- cbind.data.frame(label_test,round(preds,0))
  names(Results) <- c("Actual","Predicted")
  
  names <- dimnames(train)[[2]]
  importance_matrix <- xgb.importance(names, model = bst)
  xgb.plot.importance(importance_matrix)
  
}

#Saving Test Data
{

  rmse_error <- rmse(label_test, preds)
  rmse_error_column <- rep(rmse_error,nrow(Data[-rows_to_train_on,]))
  
  Percentage_Full_Correct <- length(which(label_test == round(preds,0)))/length(label_test)
  Percentage_Full_Correct_column <- rep(Percentage_Full_Correct,nrow(Data[-rows_to_train_on,]))
  
  write.csv(cbind.data.frame(Data[-rows_to_train_on,],Results,rmse_error_column,Percentage_Full_Correct_column),"C:/Users/lukeg/Documents/R - Projects/20180908-GovHack-V8/Results.csv")
  
  Data2 <- cbind.data.frame(importance_matrix[1:10,1],round(importance_matrix[1:10,2],2))
  write.csv(Data2,"C:/Users/lukeg/Documents/R - Projects/20180908-GovHack-V8/Importance_Matrix.csv")
  
}



#story for slide deck.
#how to tell story.
#y tomorrow morning to show this to site. to win judges.
#show we understand the people.
#sensitivity analysis. 

#problem didn't need or had. combine with other datasets.
#metric retailors is maximum disancte to get to outlet.

#correlation analysis. high incomes enquire more tax for example. 
#look into unmarried.

