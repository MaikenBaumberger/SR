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
                        "WHCact","SOC_0_10", "SM_0_10", "ST_0_10","SOC_0_30", "SM_0_30", "ST_0_30","SOC_10_30", "SM_10_30", "ST_10_30")

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
#define selected hyperparameter

hyperparameter_spatial = expand.grid(mtry = c(3),
                             min.node.size = c(5),
                             splitrule = "variance")

hyperparameter_measured = expand.grid(mtry = c(2),
                                     min.node.size = c(5),
                                     splitrule = "variance")


#########################################
#define predictors for the respective models
response <- "flow"


predictors_measured_10_30 <- c("land_use", "TWI", "slope", "ndvi", "ndvi_dif", "ph", "bulk_density","WHCact", 
                               "cn","SOC_0_10", "SM_0_10", "ST_0_10","SOC_10_30", "SM_10_30", "ST_10_30")

predictors_spatial_10_30 <- c("land_use", "TWI", "slope", "ndvi", "ndvi_dif", "SOC_0_10", "SM_0_10", "ST_0_10","SOC_10_30", "SM_10_30", "ST_10_30")


#########################################
#backup data

train_measured <- sr_measured#[!sr_measured$id %in% test_set$id, ]
train_spatial <- sr_spatial#[!sr_spatial$id %in% test_set$id, ]


#########################################
# cross-validation folds 

set.seed(6)

folds_measured <- CAST::CreateSpacetimeFolds(train_measured, spacevar  = "plot_id", k = 4)

folds_spatial <- CAST::CreateSpacetimeFolds(train_spatial, spacevar  = "plot_id", k = 8)


##########################################
#models

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
    savePredictions = "final" 
  ),
  importance = "permutation"
)


comp_10_30 = caret::train(
  x = train_measured[, predictors_spatial_10_30],
  y = train_measured[, response],
  method = "ranger",
  tuneGrid = hyperparameter_measured,
  num.trees = 100,
  trControl = trainControl(
    method = "cv", number = 4,
    index = folds_measured$index,
    indexOut = folds_measured$indexOut,
    savePredictions = "final"
  ),
  importance = "permutation"
)


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
    savePredictions = "final"
  ),
  importance = "permutation"
)


################################
#model results

measured_10_30
comp_10_30
spatial_10_30

global_validation(measured_10_30)
global_validation(spatial_10_30)

measured_10_30$results[which.min(measured_10_30$results$RMSE), c("RMSE", "Rsquared", "MAE")]
comp_10_30$results[which.min(comp_10_30$results$RMSE), c("RMSE", "Rsquared", "MAE")]
spatial_10_30$results[which.min(spatial_10_30$results$RMSE), c("RMSE", "Rsquared", "MAE")]

plot(spatial_10_30$pred$pred,spatial_10_30$pred$obs,abline(1,1),xlim=c(0,700),ylim=c(0,700))

plot(varImp(measured_10_30))
plot(varImp(spatial_10_30))

rf_spatial_10_30 <- spatial_10_30 

rf_measured_10_30 <- measured_10_30

save(rf_spatial_10_30,file="C:/Users/maike/Desktop/Carbon4D/3D_soil_respiration_clean/sr_model/verison_3/rf_spatial_10_30.Rdata")
save(rf_measured_10_30,file="C:/Users/maike/Desktop/Carbon4D/3D_soil_respiration_clean/sr_model/verison_3/rf_measured_10_30.Rdata")
