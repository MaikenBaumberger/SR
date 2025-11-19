library(caret)
library(CAST)
library(terra)
library(raster)
library(sf)
library(st)
library(ModelMetrics)
library(dplyr)

directory <- "" #fill in the directory to the folder 

#####################################
#load and prepare spatio-temporal soil respiration data and predictors

load(file.path(directory,"sr_data.Rdata"))


#####################################
#create data set of soil core locations with additional predictors

sr_measured <- data.frame(sr_pred_filled$chamber_date_time,
                          sr_pred_filled$plotID,
                          sr_pred_filled$id,
                          sr_pred_filled$flow,
                          sr_pred_filled$land_use,
                          sr_pred_filled$topo_wetness,
                          sr_pred_filled$inclination,
                          sr_pred_filled$ndvi_area,
                          sr_pred_filled$ndvi_dif_area,
                          sr_pred_filled$ph_mineral,
                          sr_pred_filled$density,
                          sr_pred_filled$cn,
                          sr_pred_filled$WHCact,
                          sr_pred_filled$SOC_0_10,
                          sr_pred_filled$SM_0_10,
                          sr_pred_filled$ST_0_10,
                          sr_pred_filled$SOC_0_30,
                          sr_pred_filled$SM_0_30,
                          sr_pred_filled$ST_0_30,
                          sr_pred_filled$SOC_10_30,
                          sr_pred_filled$SM_10_30,
                          sr_pred_filled$ST_10_30)

names(sr_measured) <- c("chamber_date_time","plot_id","id","flow","land_use", "TWI", "slope", "ndvi", "ndvi_dif", "ph", "bulk_density","cn",
                        "WHC","SOC_0_10", "SM_0_10", "ST_0_10","SOC_0_30", "SM_0_30", "ST_0_30","SOC_10_30", "SM_10_30", "ST_10_30")

sr_measured <- sr_measured[complete.cases(sr_measured), ]


########################################
#create data set of all locations with spatial predictors

sr_spatial <- data.frame(sr_pred_filled$chamber_date_time,
                         sr_pred_filled$plotID,
                         sr_pred_filled$id,
                         sr_pred_filled$flow,
                         sr_pred_filled$land_use,
                         sr_pred_filled$topo_wetness,
                         sr_pred_filled$inclination,
                         sr_pred_filled$ndvi_area,
                         sr_pred_filled$ndvi_dif_area,
                         sr_pred_filled$SOC_0_10,
                         sr_pred_filled$SM_0_10,
                         sr_pred_filled$ST_0_10,
                         sr_pred_filled$SOC_0_30,
                         sr_pred_filled$SM_0_30,
                         sr_pred_filled$ST_0_30,
                         sr_pred_filled$SOC_10_30,
                         sr_pred_filled$SM_10_30,
                         sr_pred_filled$ST_10_30)

names(sr_spatial) <- c("chamber_date_time","plot_id","id","flow","land_use", "TWI", "slope", "ndvi", "ndvi_dif", 
                       "SOC_0_10", "SM_0_10", "ST_0_10","SOC_0_30", "SM_0_30", "ST_0_30","SOC_10_30", "SM_10_30", "ST_10_30")

sr_spatial<- sr_spatial[complete.cases(sr_spatial), ]


#########################################
#define possible hyperparameter

hyperparameter_spatial = expand.grid(mtry = c(2,3,4,5,6,7,8,9,10,11),
                                     min.node.size = c(5,10,15,20,25),
                                     splitrule = "variance")

hyperparameter_measured = expand.grid(mtry = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15),
                                      min.node.size = c(5,10,15,20,25),
                                      splitrule = "variance")

#########################################
#define predictors for the respective models

response <- "flow"


predictors_measured_10_30 <- c("land_use", "TWI", "slope", "ndvi", "ndvi_dif", "ph", "bulk_density","WHC", 
                               "cn","SOC_0_10", "SM_0_10", "ST_0_10","SOC_10_30", "SM_10_30", "ST_10_30")

predictors_spatial_10_30 <- c("land_use", "TWI", "slope", "ndvi", "ndvi_dif", "SOC_0_10", "SM_0_10", 
                              "ST_0_10","SOC_10_30", "SM_10_30", "ST_10_30")


#########################################
#backup data

sr_measured <- sr_measured[order(sr_measured$chamber_date_time), ]
sr_measured_original <- sr_measured
sr_spatial_original <- sr_spatial


#########################################
#create lists to save resuts

results <- list()       
predictions <- list()   


##########################################
#assigning repeat plots

repeat_plots <- c("P0103", "P0104", "P0105", "P0154", "P0155", "P0156", "P0203", "P0204", "P0205")
repeat_plots_data <- sr_measured[sr_measured$plot_id %in% repeat_plots, ]
non_repeat_plots_data <- sr_measured[!sr_measured$plot_id %in% repeat_plots, ]

repeat_plots_group1 <- repeat_plots_data %>%
  filter((plot_id == "P0103") |
           (plot_id == "P0154") |
           (plot_id == "P0203" ))

repeat_plots_group2 <- repeat_plots_data %>%
  filter((plot_id == "P0104") |
           (plot_id == "P0155") |
           (plot_id == "P0204" ))

repeat_plots_group3 <- repeat_plots_data %>%
  filter((plot_id == "P0105") |
           (plot_id == "P0156") |
           (plot_id == "P0205" ))


non_repeat_plots_group1 <- non_repeat_plots_data[seq(1, nrow(non_repeat_plots_data), by = 3), ]
non_repeat_plots_group2 <- non_repeat_plots_data[seq(2, nrow(non_repeat_plots_data), by = 3), ]
non_repeat_plots_group3 <- non_repeat_plots_data[seq(3, nrow(non_repeat_plots_data), by = 3), ]

table(repeat_plots_data$plot_id)

##################################
#data split 1/3

data_group1 <- rbind(non_repeat_plots_group1, repeat_plots_group1)
data_group2 <- rbind(non_repeat_plots_group2, repeat_plots_group2)
data_group3 <- rbind(non_repeat_plots_group3, repeat_plots_group3)

plot(data_group1$chamber_date_time,data_group1$flow)
points(data_group2$chamber_date_time,data_group2$flow,col="red")
points(data_group3$chamber_date_time,data_group3$flow,col="blue")

test_sets <- list(data_group1,data_group2,data_group3)

##################################
# function Nash-Sutcliffe Efficiency

nse <- function(observed, predicted) {
  numerator <- sum((observed - predicted)^2)
  denominator <- sum((observed - mean(observed))^2)
  return(1 - (numerator / denominator))
}


###########################################
#loop for calculating model performance and setting hyperparamters in a nested cross-validation with external test set

all_metrics_results <- list()
all_model_results <- list()
over_all_metrics_results <- list()

metrics_results <- data.frame(
  model = c("measured_10_30", "comp_10_30", "spatial_10_30"),
  RMSE = NA, Rsquared = NA, MAE = NA, NSE = NA
)

for (seed in 1:10) {  
  set.seed(seed) 
  
  print(seed)
  
  results <- list() 
  
  # loop over the three test sets
  for (i in 1:3) {
    # select test set
    test_set <- test_sets[[i]]
    
    # select training data
    train_measured <- sr_measured[!sr_measured$id %in% test_set$id, ]
    train_spatial <- sr_spatial[!sr_spatial$id %in% test_set$id, ]
    
    
    # cross-validation folds 
    folds_measured <- CAST::CreateSpacetimeFolds(train_measured, spacevar  = "plot_id", k = 4)
    folds_spatial <- CAST::CreateSpacetimeFolds(train_spatial, spacevar  = "plot_id", k = 8)
    
    #models
    models <- list(
      measured_10_30 = caret::train(
        x = train_measured[, predictors_measured_10_30],
        y = train_measured[, response],
        method = "ranger",
        tuneGrid = hyperparameter_measured,
        num.trees = 100,
        trControl = trainControl(
          method = "cv", number = 4,
          index = folds_measured$index,
          indexOut = folds_measured$indexOut,
          savePredictions = "final"),
        importance = "permutation"),
      
      comp_10_30 = caret::train(
        x = train_measured[, predictors_spatial_10_30],
        y = train_measured[, response],
        method = "ranger",
        tuneGrid = hyperparameter_spatial,
        num.trees = 100,
        trControl = trainControl(
          method = "cv", number = 4,
          index = folds_measured$index,
          indexOut = folds_measured$indexOut,
          savePredictions = "final"),
        importance = "permutation"),
      
      spatial_10_30 = caret::train(
        x = train_spatial[, predictors_spatial_10_30],
        y = train_spatial[, response],
        method = "ranger",
        tuneGrid = hyperparameter_spatial,
        num.trees = 100,
        trControl = trainControl(
          method = "cv", number = 8,
          index = folds_spatial$index,
          indexOut = folds_spatial$indexOut,
          savePredictions = "final"),
        importance = "permutation") )
    
    
    # model results
    results[[i]] <- lapply(models, function(model) {
      model$results[which.min(model$results$RMSE), c("RMSE", "Rsquared", "MAE","mtry","min.node.size")]
    })
    
    all_model_results[[seed]] <- results
    
    
    # prediction on test set
    for (model_name in names(models)) {
      test_set[[paste0("pred_", model_name)]] <- predict(models[[model_name]], test_set)
    }
    
    predictions[[i]] <- test_set
    
    
    predictions_df <- predictions[[i]] #do.call(rbind, predictions)
    
    # calculate metics
    for (j in 1:nrow(metrics_results)) {
      model_column <- paste0("pred_", metrics_results$model[j])
      
      metrics_results$RMSE[j] <- rmse(predictions_df$flow, predictions_df[[model_column]])
      metrics_results$Rsquared[j] <- R2(predictions_df$flow, predictions_df[[model_column]])
      metrics_results$MAE[j] <- mae(predictions_df$flow, predictions_df[[model_column]])
      metrics_results$NSE[j] <- nse(predictions_df$flow, predictions_df[[model_column]])
      metrics_results$test_set[j] <- i
      metrics_results$seed[j] <- seed
    }
    
    all_metrics_results[[i]] <- metrics_results
    
    over_all_metrics_results[[seed]] <- bind_rows(all_metrics_results)
    
  }
  #seed loop
  unpacked_results <- list()
  
  # iterate over seeds and test sets
  for (seed in seq_along(all_model_results)) {
    seed_results <- all_model_results[[seed]]
    
    for (test_set_index in seq_along(seed_results)) {
      testset_result <- seed_results[[test_set_index]]
      
      for (model_name in names(testset_result)) {
        model_result <- testset_result[[model_name]]
        model_result$model <- model_name
        model_result$seed <- seed
        model_result$test_set <- test_set_index
        
        unpacked_results[[length(unpacked_results) + 1]] <- model_result
      }
    }
  }

  test_set_results <- bind_rows(over_all_metrics_results)
  
  
  model_results_df <- do.call(rbind, unpacked_results)
  
  
}

#####################################
#model statistics

model_results_spatial <- model_results_df  %>%
  filter(model == "spatial_10_30") 
mean(model_results_spatial$Rsquared)
sd(model_results_spatial$Rsquared)
mean(model_results_spatial$RMSE)
sd(model_results_spatial$RMSE)
table(model_results_spatial$mtry)
table(model_results_spatial$min.node.size)

model_results_comp <- model_results_df  %>%
  filter(model == "comp_10_30") 
mean(model_results_comp$Rsquared)
sd(model_results_comp$Rsquared)
mean(model_results_comp$RMSE)
sd(model_results_comp$RMSE)
table(model_results_comp$mtry)
table(model_results_comp$min.node.size)

model_results_measured <- model_results_df  %>%
  filter(model == "measured_10_30") 
mean(model_results_measured$Rsquared)
sd(model_results_measured$Rsquared)
mean(model_results_measured$RMSE)
sd(model_results_measured$RMSE)
table(model_results_measured$mtry)
table(model_results_measured$min.node.size)

#####################################
#test statistics

test_metrics_summary_spatial <- test_set_results  %>%
  filter(model == "spatial_10_30") 
mean(test_metrics_summary_spatial$Rsquared)
sd(test_metrics_summary_spatial$Rsquared)
mean(test_metrics_summary_spatial$RMSE)
sd(test_metrics_summary_spatial$RMSE)
   
test_metrics_summary_comp <- test_set_results  %>%
  filter(model == "comp_10_30") 
mean(test_metrics_summary_comp$Rsquared)
sd(test_metrics_summary_comp$Rsquared)
mean(test_metrics_summary_comp$RMSE)
sd(test_metrics_summary_comp$RMSE)

test_metrics_summary_measured <- test_set_results  %>%
  filter(model == "measured_10_30") 
mean(test_metrics_summary_measured$Rsquared)
sd(test_metrics_summary_measured$Rsquared)
mean(test_metrics_summary_measured$RMSE)
sd(test_metrics_summary_measured$RMSE)


