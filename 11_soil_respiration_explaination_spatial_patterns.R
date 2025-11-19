library(caret)
library(CAST)
library(terra)
library(raster)
library(sf)
library(st)
library(DALEX)
library(dplyr)
library(tmap)

directory <- "" #fill in the directory to the folder 


######################################
#load models

load(file.path(directory,"rf_spatial_10_30.Rdata"))

rfmodel <- rf_spatial_10_30


######################################
#calculate train DI for area of applicability
DI_sr = trainDI(model=rfmodel)


######################################
#load static spatial data (data is available on request)

static_raster= rast(file.path(directory,"static_raster.tif"))
names(static_raster) = c("soil_texture","soil_type","elevation","land_use","inclination","exposition",
                         "topo_wetness","northness","eastness")

elevation = static_raster$elevation
raster_base = elevation*0


######################################
#load NDVI data (data is available on request)
ndvi = terra::rast(file.path(directory,"NDVI_2021_2022_daily_round_3.tif"))


######################################
#location of soil temperature and soil moisture spatial data for mineral soil and organic layer (fill in direction)

base_path_sm <- ".../soil_moisture/pred_sm_"
base_path_st <- ".../soil_temperature/pred_st_"
base_path_st_org <- ".../spatial_prediction_st_sm_org/pred_st_"
base_path_sm_org <- ".../spatial_prediction_st_sm_org/pred_sm_"


###################################
#load SOC stock spatial data  (data is available on request)

soc <- terra::rast(file.path(directory,"pred_cstock_new.tif"))

######################################
#select date for spatial prediction and explanation

dates <- c("20220915")

for (date in dates){
  
  date_str <-  date
  
  soil_moisture <- terra::rast(paste0(base_path_sm, date_str, "1200.tif"))
  soil_temperature <- terra::rast(paste0(base_path_st, date_str, "1200.tif"))
  st_org <- terra::rast(paste0(base_path_st_org, date_str, "1200.tif"))
  sm_org <- terra::rast(paste0(base_path_sm_org, date_str, "1200.tif"))
  
 
  current_date <- as.Date(date_str, format="%Y%m%d")
  print(current_date)

  forest_mask <- static_raster$land_use == 1000
  
  st_org_forest <- st_org
  st_05_arable_meadow <- soil_temperature[[1]]
  st_org_forest[!forest_mask] <- NA
  st_05_arable_meadow[forest_mask] <- NA
  ST_0_10_spat <-  mean(st_org_forest,st_05_arable_meadow, na.rm=T)
  st_05_forest <- soil_temperature[[1]]
  st_05_forest[!forest_mask] <- NA
  st_15 <- soil_temperature[[2]]
  st_25 <- soil_temperature[[3]]
  ST_10_30_spat <-  mean(st_05_forest,st_15,st_25, na.rm=T)
  
  sm_org_forest <- sm_org
  sm_05_arable_meadow <- soil_moisture[[1]]
  sm_org_forest[!forest_mask] <- NA
  sm_05_arable_meadow[forest_mask] <- NA
  SM_0_10_spat <-  mean(sm_org_forest,sm_05_arable_meadow, na.rm=T)
  sm_05_forest <- soil_moisture[[1]]
  sm_05_forest[!forest_mask] <- NA
  sm_15 <- soil_moisture[[2]]
  sm_25 <- soil_moisture[[3]]
  SM_10_30_spat <-  mean(sm_05_forest,sm_15,sm_25, na.rm=T)


  soc_org_forest <- soc[[1]]
  soc_05_arable_meadow <- soc[[2]]
  soc_05_arable_meadow[forest_mask] <- NA
  
  plot(soc_05_arable_meadow)
  SOC_0_10_spat <-  sum(soc_org_forest,soc_05_arable_meadow, na.rm=T)
  plot(SOC_0_10_spat)
  
  soc_05_forest <- soc[[2]]
  soc_05_forest[!forest_mask] <- NA
  soc_15 <- soc[[3]]
  soc_25 <- soc[[4]]
  SOC_10_30_spat <-  sum(soc_05_forest,soc_15,soc_25, na.rm=T)
  plot(SOC_10_30_spat)


  spatially_land_use = static_raster$land_use
  
  spatially_inclination= static_raster$inclination
  
  spatially_topo_wetness = static_raster$topo_wetness
  
  
  doy = as.numeric(strftime(current_date, format = "%j"))
  
  hour=12


  spatially_ndvi = resample(ndvi[[doy]],raster_base)
  plot(spatially_ndvi)
  
  doy_14 <- doy-14
  
  spatially_ndvi_14 <- resample(ndvi[[doy_14]],raster_base)
  
  spatially_ndvi_dif <- spatially_ndvi - spatially_ndvi_14
  plot(spatially_ndvi_dif)


  predictors_res = c(ST_0_10_spat, ST_10_30_spat, SM_0_10_spat, SM_10_30_spat,SOC_0_10_spat, SOC_10_30_spat, 
                     spatially_ndvi, spatially_ndvi_dif,spatially_land_use, spatially_topo_wetness, spatially_inclination)
                     
  names(predictors_res)<- c( "ST_0_10","ST_10_30","SM_0_10","SM_10_30","SOC_0_10","SOC_10_30",
                             "ndvi", "ndvi_dif","land_use", "TWI", "slope")  
  
 

  levels(predictors_res$land_use) <- data.frame(ID=c(1000,1500,1600), 
                                               land_use=c("1000","1500","1600"))
  
  #prediction
  prediction_res <- predict(predictors_res,rfmodel,na.rm=TRUE)

  prediction_res_pr = project(prediction_res, "EPSG:32632")
  
  StudyArea <- st_read(file.path(directory,"StudyArea.gpkg"))
  
  prediction_res_pr= crop(prediction_res_pr,StudyArea$geom)
  
  plot(prediction_res_pr,colNA="grey60",axes=F)
  

  predictors_res_pr = project(predictors_res, "EPSG:32632",method="near")
  predictors_res_pr= crop(predictors_res_pr,StudyArea$geom)
  
  prediction_res_pr_round <- round(prediction_res_pr,0)

}


#######################################
#load models

load(file.path(directory,"rf_spatial_10_30.Rdata"))
load(file.path(directory,"rf_measured_10_30.Rdata"))

rfmodel <- rf_spatial_10_30
rfmodel_measured <- rf_measured_10_30


#####################################
#load and prepare spatio-temporal soil respiration data and predictors

load(file.path(directory,"sr_data.Rdata"))

sr_spatial <- data.frame(sr_pred_filled$flow,
                         sr_pred_filled$land_use,
                         sr_pred_filled$topo_wetness,
                         sr_pred_filled$inclination,
                         sr_pred_filled$ndvi_area,
                         sr_pred_filled$ndvi_dif_area,
                         sr_pred_filled$SOC_0_10,
                         sr_pred_filled$SM_0_10,
                         sr_pred_filled$ST_0_10,
                         sr_pred_filled$SOC_10_30,
                         sr_pred_filled$SM_10_30,
                         sr_pred_filled$ST_10_30)

names(sr_spatial) <- c("flow","land_use", "TWI", "slope", "ndvi", "ndvi_dif", 
                       "SOC_0_10", "SM_0_10", "ST_0_10","SOC_10_30", "SM_10_30", "ST_10_30")

sr_spatial<- sr_spatial[complete.cases(sr_spatial), ]

############################################

sr_measured <- data.frame(sr_pred_filled$flow,
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
                          sr_pred_filled$SOC_10_30,
                          sr_pred_filled$SM_10_30,
                          sr_pred_filled$ST_10_30)

names(sr_measured) <- c("flow","land_use", "TWI", "slope", "ndvi", "ndvi_dif", "ph", "bulk_density","cn",
                        "WHCact","SOC_0_10", "SM_0_10", "ST_0_10","SOC_10_30", "SM_10_30", "ST_10_30")

sr_measured <- sr_measured[complete.cases(sr_measured), ]


############################################
#use dalex for model interpretation

explainer_regr_rf <- DALEX::explain(rfmodel, label = "rf", 
                                    data = sr_spatial, y = sr_spatial$flow, 
                                    verbose = FALSE)

mp_regr_rf <- model_performance(explainer_regr_rf)

pdp_regr_rf  <- model_profile(explainer_regr_rf,type = "partial")

plot(pdp_regr_rf)

observation <- sr_spatial[2,]

plot(predict_parts(explainer_regr_rf, new_observation = observation))

plot(predict_profile(explainer_regr_rf, observation))


##########################################
#create model for dalex 

explainer_regr_rf_measured <- DALEX::explain(rfmodel_measured, label = "rf", 
                                    data = sr_measured, y = sr_measured$flow, 
                                    verbose = FALSE)


###########################################
#calculate shap feature importance spatial

plot(variable_importance(explainer_regr_rf_measured))

shap_values_measured <- predict_parts(
  explainer_regr_rf_measured,
  new_observation = sr_measured[1:104, ],
  type = "shap"
)

shap_values_measured
plot(shap_values_measured)

shap_importance_measured <- shap_values_measured %>%
  group_by(variable) %>%
  summarise(mean_abs_shap = mean(abs(contribution))) %>%
  arrange(desc(mean_abs_shap))

print(shap_importance_measured)


############################################
#calculate shap feature importance measured

plot(variable_importance(explainer_regr_rf_measured))

shap_values <- predict_parts(
  explainer_regr_rf,
  new_observation = sr_spatial[1:191, ],
  type = "shap"
)

shap_values
plot(shap_values)

shap_importance <- shap_values %>%
  group_by(variable) %>%
  summarise(mean_abs_shap = mean(abs(contribution))) %>%
  arrange(desc(mean_abs_shap))

print(shap_importance)
plot(shap_values)

#######################################
#spatial explaination with shap

x = predictors_res_pr

names(x)

exp = explainer_regr_rf
maxcell = 400000   #reduce number of pixels

plot(x[[2]])

if (terra::ncell(x) > 1.1 * maxcell) {
  x = terra::spatSample(x, maxcell, method = "regular", as.raster = TRUE, warn = FALSE, na.rm=T)
}

plot(x[[2]])

x_df = as.data.frame(x, na.rm = FALSE)

table(x_df$land_use)
x_df <- droplevels(x_df)

result = cbind(intercept = NA, x_df, pred = NA)

head(result)

result$land_use <- as.numeric(result$land_use)

i=111

for (i in seq_len(nrow(x_df))){
  if (complete.cases(x_df[i, ])){
    print(i)
    pp = predict_parts(exp, new_observation = x_df[i, ])
    pp_df = data.frame(contribution = pp$contribution, variable_name = pp$variable_name, label = pp$label)
    pp_df$variable_name = ifelse(pp_df$variable_name == "", "pred", pp_df$variable_name)
    pp_df = tidyr::pivot_wider(pp_df, names_from = variable_name, values_from = contribution)
    result[i, ] = pp_df[which.max(pp_df$pred), names(result)]
  } else {
    result[i, ] = NA
  }
}
result

r_result = terra::rast(x, nlyrs = ncol(result))
terra::values(r_result) = result
names(r_result) = names(result)


##################################
#write raster

writeRaster(r_result, filename=file.path(directory,"sr_explain_sep.tif",overwrite=TRUE))
writeRaster(predictors_res_pr, filename=file.path(directory,"sr_explain_pred_sep.tif",overwrite=TRUE))

