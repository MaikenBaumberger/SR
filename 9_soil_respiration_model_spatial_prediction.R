library(caret)
library(CAST)
library(terra)
library(raster)
library(sf)
library(st)


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
#select dates for spatial prediction


 dates <- c("20220115","20220215","20220315","20220415","20220515","20220615",
            "20220715","20220815","20220915","20221015","20221115","20221215")
#
#dates <- c("20220108","20220208","20220308","20220408","20220508","20220608",
#           "20220708","20220808","20220908","20221008","20221108","20221208")
# 
# dates <- c("20220121","20220221","20220321","20220421","20220521","20220621",
#            "20220721","20220821","20220921","20221021","20221121","20221221")
# 
# dates <- c("20220127","20220227","20220327","20220427","20220527","20220627",
#            "20220727","20220827","20220927","20221027","20221127","20221227")
#
# dates <- c("20220103","20220203","20220303","20220403","20220503","20220603",
#            "20220703","20220803","20220903","20221003","20221103","20221203")
#
# dates <- c("20220111","20220211","20220311","20220411","20220511","20220611",
#            "20220711","20220811","20220911","20221011","20221111","20221211")
#
# dates <- c("20220118","20220218","20220318","20220418","20220518","20220618",
#            "20220718","20220818","20220918","20221018","20221118","20221218")
#
# dates <- c("20220124","20220224","20220324","20220424","20220524","20220624",
#            "20220724","20220824","20220924","20221024","20221124","20221224")


################################
#prepare data for each date, include organic layer from forest sites in mineral soil, adjusting soil depths
#create spatial prediction
#calculate area of applicability

for (date in dates) {
  
  date_str <-  date
  
  soil_moisture <- terra::rast(paste0(base_path_sm, date_str, "1500.tif"))
  soil_temperature <- terra::rast(paste0(base_path_st, date_str, "1500.tif"))
  st_org <- terra::rast(paste0(base_path_st_org, date_str, "1500.tif"))
  sm_org <- terra::rast(paste0(base_path_sm_org, date_str, "1500.tif"))
  
  current_date <- as.Date(date_str, format="%Y%m%d")
  print(current_date)

  names(sm_org)
  names(st_org)
  plot(st_org)
  
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
  
  
  doy = as.numeric(strftime(current_date, format = "%j"))+365
  
  hour=12
  
  
  spatially_ndvi = resample(ndvi[[doy]],raster_base)
  plot(spatially_ndvi)
  
  doy_14 <- doy-14
  
  spatially_ndvi_14 <- resample(ndvi[[doy_14]],raster_base)
  
  spatially_ndvi_dif <- spatially_ndvi - spatially_ndvi_14
  plot(spatially_ndvi_dif)
  
  #define predictors
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
  
  output_path <- ".../prediction_res_pr_" #fill in direction
  output_file <- paste0(output_path, date_str, ".tif")
  terra::writeRaster(prediction_res_pr_round, output_file, overwrite = TRUE)
  
  #AOA
  AOA <- aoa(predictors_res_pr, rfmodel, trainDI = DI_sr, LPD = TRUE, verbose = FALSE)
  output_path_aoa <- "C:/Users/maike/Desktop/Carbon4D/3D_soil_respiration_clean/sr_prediction/prediction_und_AOA_3/AOA/aoa_res_pr_"
  output_file_aoa <- paste0(output_path_aoa, date_str, ".Rdata")
  output_file_aoa_rast <- paste0(output_path_aoa, date_str, ".tif")
  save(AOA, file=output_file_aoa)
  AOA_rast <- AOA$AOA
  terra::writeRaster(AOA_rast, output_file_aoa_rast, overwrite = TRUE)
  plot(prediction_res_pr, col=viridis::viridis(100))
  plot(AOA$AOA,col=c("red","transparent"),add=T,legend=F)

}


