
library(terra)
library(tmap)
library(ggplot2)


directory <- "" #fill in the directory to the folder 


#####################################


r_result_jan <- rast(file.path(directory,"sr_explain_jan.tif"))
predictors_res_jan <- rast(file.path(directory,"sr_explain_pred_jan.tif"))

r_result_may <- rast(file.path(directory,"sr_explain_may.tif"))
predictors_res_may <- rast(file.path(directory,"sr_explain_pred_may.tif"))

r_result_sep <- rast(file.path(directory,"sr_explain_sep.tif"))
predictors_res_sep <- rast(file.path(directory,"sr_explain_pred_sep.tif"))


####################################################
#visualization

ST_0_10_raster_jan = tm_shape(predictors_res_jan$ST_0_10)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(-1, 19),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ST_0_10_raster_jan

ST_0_10_raster_may = tm_shape(predictors_res_may$ST_0_10)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(-1, 19),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ST_0_10_raster_may

ST_0_10_raster_sep = tm_shape(predictors_res_sep$ST_0_10)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(-1, 19),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ST_0_10_raster_sep

ST_0_10_legend = tm_shape(predictors_res_pr$ST_0_10)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = seq(-1, 19, 5))+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.text.size = 1.5,
            legend.position = c("left", "center"), 
            frame = FALSE,
            legend.only= T)
ST_0_10_legend

ST_0_10_effect_jan = tm_shape(r_result_jan$ST_0_10)+
  tm_raster(style= "cont",title="ST_0_10_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ST_0_10_effect_jan

ST_0_10_effect_may = tm_shape(r_result_may$ST_0_10)+
  tm_raster(style= "cont",title="ST_0_10_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ST_0_10_effect_may

ST_0_10_effect_sep = tm_shape(r_result_sep$ST_0_10)+
  tm_raster(style= "cont",title="ST_0_10_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ST_0_10_effect_sep

ST_0_10_effect_legend = tm_shape(r_result_jan$ST_0_10)+
  tm_raster(style= "cont",title="",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25))+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.text.size = 1.5,
            legend.position = c("left", "center"), 
            frame = FALSE,
            legend.only= T)
ST_0_10_effect_legend

################################################


ST_10_30_raster_jan = tm_shape(predictors_res_jan$ST_10_30)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(-1, 19),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ST_10_30_raster_jan

ST_10_30_raster_may = tm_shape(predictors_res_may$ST_10_30)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(-1, 19),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ST_10_30_raster_may

ST_10_30_raster_sep = tm_shape(predictors_res_sep$ST_10_30)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(-1, 19),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ST_10_30_raster_sep

ST_10_30_legend = tm_shape(predictors_res_pr$ST_10_30)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = seq(-1, 19, 5))+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.text.size = 1.5,
            legend.position = c("left", "center"), 
            frame = FALSE,
            legend.only= T)
ST_10_30_legend

ST_10_30_effect_jan = tm_shape(r_result_jan$ST_10_30)+
  tm_raster(style= "cont",title="ST_10_30_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ST_10_30_effect_jan

ST_10_30_effect_may = tm_shape(r_result_may$ST_10_30)+
  tm_raster(style= "cont",title="ST_10_30_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ST_10_30_effect_may

ST_10_30_effect_sep = tm_shape(r_result_sep$ST_10_30)+
  tm_raster(style= "cont",title="ST_10_30_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ST_10_30_effect_sep

###############################################


SM_0_10_raster_jan = tm_shape(predictors_res_jan$SM_0_10)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(2, 30),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SM_0_10_raster_jan

SM_0_10_raster_may = tm_shape(predictors_res_may$SM_0_10)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(2, 30),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SM_0_10_raster_may

SM_0_10_raster_sep = tm_shape(predictors_res_sep$SM_0_10)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(2, 30),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SM_0_10_raster_sep

SM_0_10_legend = tm_shape(predictors_res_pr$SM_0_10)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = seq(2, 30, 7))+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.text.size = 1.5,
            legend.position = c("left", "center"), 
            frame = FALSE,
            legend.only= T)
SM_0_10_legend

SM_0_10_effect_jan = tm_shape(r_result_jan$SM_0_10)+
  tm_raster(style= "cont",title="SM_0_10_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SM_0_10_effect_jan

SM_0_10_effect_may = tm_shape(r_result_may$SM_0_10)+
  tm_raster(style= "cont",title="SM_0_10_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SM_0_10_effect_may

SM_0_10_effect_sep = tm_shape(r_result_sep$SM_0_10)+
  tm_raster(style= "cont",title="SM_0_10_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SM_0_10_effect_sep


################################################


SM_10_30_raster_jan = tm_shape(predictors_res_jan$SM_10_30)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(2, 30),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SM_10_30_raster_jan

SM_10_30_raster_may = tm_shape(predictors_res_may$SM_10_30)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(2, 30),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SM_10_30_raster_may

SM_10_30_raster_sep = tm_shape(predictors_res_sep$SM_10_30)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(2, 30),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SM_10_30_raster_sep

SM_10_30_legend = tm_shape(predictors_res_pr$SM_10_30)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = seq(2, 30, 7))+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.text.size = 1.5,
            legend.position = c("left", "center"), 
            frame = FALSE,
            legend.only= T)
SM_10_30_legend

SM_10_30_effect_jan = tm_shape(r_result_jan$SM_10_30)+
  tm_raster(style= "cont",title="SM_10_30_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SM_10_30_effect_jan

SM_10_30_effect_may = tm_shape(r_result_may$SM_10_30)+
  tm_raster(style= "cont",title="SM_10_30_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SM_10_30_effect_may

SM_10_30_effect_sep = tm_shape(r_result_sep$SM_10_30)+
  tm_raster(style= "cont",title="SM_10_30_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SM_10_30_effect_sep

###############################################


SOC_0_10_raster_jan = tm_shape(predictors_res_jan$SOC_0_10)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(20, 90),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SOC_0_10_raster_jan

SOC_0_10_raster_may = tm_shape(predictors_res_may$SOC_0_10)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(20, 90),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SOC_0_10_raster_may

SOC_0_10_raster_sep = tm_shape(predictors_res_sep$SOC_0_10)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(20, 90),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SOC_0_10_raster_sep

SOC_0_10_legend = tm_shape(predictors_res_pr$SOC_0_10)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = seq(20, 90, 20))+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.text.size = 1.5,
            legend.position = c("left", "center"), 
            frame = FALSE,
            legend.only= T)
SOC_0_10_legend

SOC_0_10_effect_jan = tm_shape(r_result_jan$SOC_0_10)+
  tm_raster(style= "cont",title="SOC_0_10_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SOC_0_10_effect_jan

SOC_0_10_effect_may = tm_shape(r_result_may$SOC_0_10)+
  tm_raster(style= "cont",title="SOC_0_10_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SOC_0_10_effect_may

SOC_0_10_effect_sep = tm_shape(r_result_sep$SOC_0_10)+
  tm_raster(style= "cont",title="SOC_0_10_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SOC_0_10_effect_sep


################################################


SOC_10_30_raster_jan = tm_shape(predictors_res_jan$SOC_10_30)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(20, 90),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SOC_10_30_raster_jan

SOC_10_30_raster_may = tm_shape(predictors_res_may$SOC_10_30)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(20, 90),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SOC_10_30_raster_may

SOC_10_30_raster_sep = tm_shape(predictors_res_sep$SOC_10_30)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(20, 90),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SOC_10_30_raster_sep

SOC_10_30_legend = tm_shape(predictors_res_pr$SOC_10_30)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = seq(20, 90, 20))+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.text.size = 1.5,
            legend.position = c("left", "center"), 
            frame = FALSE,
            legend.only= T)
SOC_10_30_legend

SOC_10_30_effect_jan = tm_shape(r_result_jan$SOC_10_30)+
  tm_raster(style= "cont",title="SOC_10_30_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SOC_10_30_effect_jan

SOC_10_30_effect_may = tm_shape(r_result_may$SOC_10_30)+
  tm_raster(style= "cont",title="SOC_10_30_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SOC_10_30_effect_may

SOC_10_30_effect_sep = tm_shape(r_result_sep$SOC_10_30)+
  tm_raster(style= "cont",title="SOC_10_30_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
SOC_10_30_effect_sep

###############################################


ndvi_raster_jan = tm_shape(predictors_res_jan$ndvi)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(-0.7, 0.9),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ndvi_raster_jan

ndvi_raster_may = tm_shape(predictors_res_may$ndvi)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(-0.7, 0.9),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ndvi_raster_may

ndvi_raster_sep = tm_shape(predictors_res_sep$ndvi)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(-0.7, 0.9),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ndvi_raster_sep

ndvi_legend = tm_shape(predictors_res_pr$ndvi)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = seq(-0.7, 0.9, 0.4))+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.text.size = 1.5,
            legend.position = c("left", "center"), 
            frame = FALSE,
            legend.only= T)
ndvi_legend

ndvi_effect_jan = tm_shape(r_result_jan$ndvi)+
  tm_raster(style= "cont",title="ndvi_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ndvi_effect_jan

ndvi_effect_may = tm_shape(r_result_may$ndvi)+
  tm_raster(style= "cont",title="ndvi_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ndvi_effect_may

ndvi_effect_sep = tm_shape(r_result_sep$ndvi)+
  tm_raster(style= "cont",title="ndvi_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ndvi_effect_sep

###############################################

ndvi_dif_raster_jan = tm_shape(predictors_res_jan$ndvi_dif)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(-0.6, 0.9),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ndvi_dif_raster_jan

ndvi_dif_raster_may = tm_shape(predictors_res_may$ndvi_dif)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(-0.6, 0.9),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ndvi_dif_raster_may

ndvi_dif_raster_sep = tm_shape(predictors_res_sep$ndvi_dif)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(-0.6, 0.9),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ndvi_dif_raster_sep

ndvi_dif_legend = tm_shape(predictors_res_pr$ndvi_dif)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = seq(-0.6, 0.9, 0.3))+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.text.size = 1.5,
            legend.position = c("left", "center"), 
            frame = FALSE,
            legend.only= T)
ndvi_dif_legend

ndvi_dif_effect_jan = tm_shape(r_result_jan$ndvi_dif)+
  tm_raster(style= "cont",title="ndvi_dif_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ndvi_dif_effect_jan

ndvi_dif_effect_may = tm_shape(r_result_may$ndvi_dif)+
  tm_raster(style= "cont",title="ndvi_dif_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ndvi_dif_effect_may

ndvi_dif_effect_sep = tm_shape(r_result_sep$ndvi_dif)+
  tm_raster(style= "cont",title="ndvi_dif_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
ndvi_dif_effect_sep

################################################



TWI_raster_jan = tm_shape(predictors_res_jan$TWI)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(5,30),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
TWI_raster_jan

TWI_raster_may = tm_shape(predictors_res_may$TWI)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(5,30),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
TWI_raster_may

TWI_raster_sep = tm_shape(predictors_res_sep$TWI)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(5,30),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
TWI_raster_sep

TWI_legend = tm_shape(predictors_res_pr$TWI)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = seq(5,30, 5))+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.text.size = 1.5,
            legend.position = c("left", "center"), 
            frame = FALSE,
            legend.only= T)
TWI_legend

TWI_effect_jan = tm_shape(r_result_jan$TWI)+
  tm_raster(style= "cont",title="TWI_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
TWI_effect_jan

TWI_effect_may = tm_shape(r_result_may$TWI)+
  tm_raster(style= "cont",title="TWI_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
TWI_effect_may

TWI_effect_sep = tm_shape(r_result_sep$TWI)+
  tm_raster(style= "cont",title="TWI_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
TWI_effect_sep
###############################################



slope_raster_jan = tm_shape(predictors_res_jan$slope)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(0,53),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
slope_raster_jan

slope_raster_may = tm_shape(predictors_res_may$slope)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(0,53),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
slope_raster_may

slope_raster_sep = tm_shape(predictors_res_sep$slope)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(0,53),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
slope_raster_sep

slope_legend = tm_shape(predictors_res_pr$slope)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = seq(0,53, 10))+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.text.size = 1.5,
            legend.position = c("left", "center"), 
            frame = FALSE,
            legend.only= T)
slope_legend

slope_effect_jan = tm_shape(r_result_jan$slope)+
  tm_raster(style= "cont",title="slope_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
slope_effect_jan

slope_effect_may = tm_shape(r_result_may$slope)+
  tm_raster(style= "cont",title="slope_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
slope_effect_may

slope_effect_sep = tm_shape(r_result_sep$slope)+
  tm_raster(style= "cont",title="slope_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
slope_effect_sep

##################################################


predictors_res_jan$land_use[predictors_res_jan$land_use>1600] <-NA
predictors_res_may$land_use[predictors_res_may$land_use>1600] <-NA
predictors_res_sep$land_use[predictors_res_sep$land_use>1600] <-NA


land_use_raster_jan = tm_shape(predictors_res_jan$land_use)+
  tm_raster(style= "cat",palette = c("darkgreen","gold","chartreuse3"),labels=c("f", "a", "g"),title="",legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
land_use_raster_jan

land_use_legend = tm_shape(predictors_res_pr$land_use)+
  tm_raster(style= "cat",palette = c("darkgreen","gold","chartreuse3"),labels=c("f", "a", "g"),title="")+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.text.size = 1.5,
            legend.position = c("left", "center"), 
            frame = FALSE,
            legend.only= T)
land_use_legend

land_use_effect_jan = tm_shape(r_result_jan$land_use)+
  tm_raster(style= "cont",title="land_use_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
land_use_effect_jan

land_use_effect_may = tm_shape(r_result_may$land_use)+
  tm_raster(style= "cont",title="land_use_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
land_use_effect_may

land_use_effect_sep = tm_shape(r_result_sep$land_use)+
  tm_raster(style= "cont",title="land_use_eff",n=10,
            palette = c("darkblue", "white", "darkred"),midpoint=0, legend.reverse = T ,breaks = seq(-50, 50, 25),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
land_use_effect_sep

###############################################


pred_legend = tm_shape(r_result_jan$pred)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = seq(10,370, 60))+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.4,
            legend.text.size = 1.5,
            legend.position = c("left", "center"), 
            frame = FALSE,
            legend.only= T)
pred_legend


pred_raster_jan = tm_shape(r_result_jan$pred)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(10,370),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
pred_raster_jan

pred_raster_may = tm_shape(r_result_may$pred)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(10,370),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
pred_raster_may

pred_raster_sep = tm_shape(r_result_sep$pred)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(10,370),legend.show = FALSE,colorNA = "grey60")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
pred_raster_sep

gap = tm_shape(r_result_sep$pred*NA)+
  tm_raster(style= "cont",title="",n=10,palette = "viridis",legend.reverse = TRUE,breaks = c(10,370),legend.show = FALSE,colorNA = "white")+
  tm_layout(inner.margins= c(0.03,0.03,0.03,0.03), 
            outer.margins = c(0,0,0,0), 
            frame = FALSE)
gap

library(pals)   
pal.bands(ocean.balance, ocean.delta, ocean.curl, main = "cmocean")  


################################################
#visualization

explain_dynamic <- tmap_arrange(ncol=9,nrow=8,heights  = c(1,0.3,1,1,1,1,1,1),widths  = c(0.5,1,1,1,0.3,1,1,1,0.6),
                            gap,gap,gap,gap,gap, pred_raster_jan, pred_raster_may, pred_raster_sep, pred_legend, 
                            gap,gap,gap,gap,gap,gap,gap,gap,gap,
                            ST_0_10_legend,ST_0_10_raster_jan,ST_0_10_raster_may,ST_0_10_raster_sep,gap,ST_0_10_effect_jan,ST_0_10_effect_may,ST_0_10_effect_sep,gap,
                            ST_10_30_legend,ST_10_30_raster_jan,ST_10_30_raster_may,ST_10_30_raster_sep,gap,ST_10_30_effect_jan,ST_10_30_effect_may,ST_10_30_effect_sep,gap,
                            SM_0_10_legend,SM_0_10_raster_jan,SM_0_10_raster_may,SM_0_10_raster_sep,gap,SM_0_10_effect_jan,SM_0_10_effect_may,SM_0_10_effect_sep,ST_0_10_effect_legend,
                            SM_10_30_legend,SM_10_30_raster_jan,SM_10_30_raster_may,SM_10_30_raster_sep,gap,SM_10_30_effect_jan,SM_10_30_effect_may,SM_10_30_effect_sep,gap,
                            ndvi_legend,ndvi_raster_jan,ndvi_raster_may,ndvi_raster_sep,gap,ndvi_effect_jan,ndvi_effect_may,ndvi_effect_sep,gap,
                            ndvi_dif_legend,ndvi_dif_raster_jan,ndvi_dif_raster_may,ndvi_dif_raster_sep,gap,ndvi_dif_effect_jan,ndvi_dif_effect_may,ndvi_dif_effect_sep,gap)



pdf(file.path(directory,"exp_dynamic.pdf",width= 20, height= 22))

explain_dynamic

dev.off()



explain_static <- tmap_arrange(ncol=9,nrow=7,heights  = c(1,0.3,1,1,1,1),widths  = c(0.9,0.5,1,0.5,1,1,1,0.6,0.9),
                               gap,gap,gap,gap, pred_raster_jan, pred_raster_may, pred_raster_sep, pred_legend,gap, 
                               gap,gap,gap,gap,gap,gap,gap,gap,gap,
                               gap,SOC_0_10_legend,SOC_0_10_raster_jan,gap,SOC_0_10_effect_jan,SOC_0_10_effect_may,SOC_0_10_effect_sep,gap,gap,
                               gap,SOC_10_30_legend,SOC_10_30_raster_jan,gap,SOC_10_30_effect_jan,SOC_10_30_effect_may,SOC_10_30_effect_sep,gap,gap,
                               gap,TWI_legend,TWI_raster_jan,gap,TWI_effect_jan,TWI_effect_may,TWI_effect_sep,ST_0_10_effect_legend,gap,
                               gap,slope_legend,slope_raster_jan,gap,slope_effect_jan,slope_effect_may,slope_effect_sep,gap,gap,
                               gap,land_use_legend,land_use_raster_jan,gap,land_use_effect_jan,land_use_effect_may,land_use_effect_sep,gap,gap)




pdf(file.path(directory,"exp_static.pdf",width= 20, height= 19))

explain_static

dev.off()
