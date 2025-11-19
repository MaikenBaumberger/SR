library(caret)
library(CAST)
library(terra)
library(raster)
library(sf)
library(st)
library(ModelMetrics)
library(pdp)


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


#######################################
#load models

load(file.path(directory,"rf_spatial_10_30.Rdata"))
load(file.path(directory,"rf_measured_10_30.Rdata"))

rfmodel_measured <- rf_measured_10_30
rfmodel_spatial <- rf_spatial_10_30


#######################################
#explore model results

plot(varImp(rfmodel_measured))
plot(varImp(rfmodel_spatial))

plot(rfmodel_spatial$pred$obs,rfmodel_spatial$pred$pred,abline(1,1),xlim=c(0,600),ylim=c(0,600))
plot(rfmodel_measured$pred$obs,rfmodel_measured$pred$pred,abline(1,1),xlim=c(0,600),ylim=c(0,600))


#######################################
#partial dependency

par_ST_0_10_sp <- partial(rfmodel_spatial, pred.var = c("ST_0_10"),rug=TRUE, data=sr_spatial)
par_ST_0_10_me <- partial(rfmodel_measured, pred.var = c("ST_0_10"),rug=TRUE, data=sr_measured)

plot_par_ST_0_10 <- autoplot(par_ST_0_10_sp, contour = TRUE, size=1.5,col="turquoise4")+
  geom_line(data=par_ST_0_10_me,aes(x=ST_0_10,y=yhat),  size=1.5,col="midnightblue")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("soil temperature 0-10 cm [°C]")+
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14))+
  ylim(c(50,220))+
  geom_point(data=sr_measured,aes(x=ST_0_10,y=50),shape = "|", size = 4,col="midnightblue")+
  geom_point(data=sr_spatial,aes(x=ST_0_10,y=62),shape = "|", size = 4,col="turquoise4")+
  geom_text(label="a", x=0.8, y=210,size=8)
plot_par_ST_0_10


par_SM_0_10_sp <- partial(rfmodel_spatial, pred.var = c("SM_0_10"),rug=TRUE, data=sr_spatial)
par_SM_0_10_me <- partial(rfmodel_measured, pred.var = c("SM_0_10"),rug=TRUE, data=sr_measured)

plot_par_SM_0_10 <- autoplot(par_SM_0_10_sp, contour = TRUE, size=1.5,col="turquoise4")+
  geom_line(data=par_SM_0_10_me,aes(x=SM_0_10,y=yhat),  size=1.5,col="midnightblue")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("soil moisture 0-10 cm [%]")+
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylim(c(50,220))+
  geom_point(data=sr_measured,aes(x=SM_0_10,y=50),shape = "|", size = 4,col="midnightblue")+
  geom_point(data=sr_spatial,aes(x=SM_0_10,y=62),shape = "|", size = 4,col="turquoise4")+
  geom_text(label="c", x=2.5, y=210,size=8)
plot_par_SM_0_10


par_SOC_0_10_sp <- partial(rfmodel_spatial, pred.var = c("SOC_0_10"),rug=TRUE, data=sr_spatial)
par_SOC_0_10_me <- partial(rfmodel_measured, pred.var = c("SOC_0_10"),rug=TRUE, data=sr_measured)

plot_par_SOC_0_10 <- autoplot(par_SOC_0_10_sp, contour = TRUE, size=1.5,col="turquoise4")+
  geom_line(data=par_SOC_0_10_me,aes(x=SOC_0_10,y=yhat),  size=1.5,col="midnightblue")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("SOC 0-10 cm [t/ha]")+
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14))+
  ylim(c(50,220))+
  geom_point(data=sr_measured,aes(x=SOC_0_10,y=50),shape = "|", size = 4,col="midnightblue")+
  geom_point(data=sr_spatial,aes(x=SOC_0_10,y=62),shape = "|", size = 4,col="turquoise4")+
  geom_text(label="f", x=17, y=210,size=8)

plot_par_SOC_0_10


par_ST_10_30_sp <- partial(rfmodel_spatial, pred.var = c("ST_10_30"),rug=TRUE, data=sr_spatial)
par_ST_10_30_me <- partial(rfmodel_measured, pred.var = c("ST_10_30"),rug=TRUE, data=sr_measured)

plot_par_ST_10_30 <- autoplot(par_ST_10_30_sp, contour = TRUE, size=1.5,col="turquoise4")+
  geom_line(data=par_ST_10_30_me,aes(x=ST_10_30,y=yhat),  size=1.5,col="midnightblue")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("soil temperature 10-30 cm [°C]")+
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylim(c(50,220))+
  geom_point(data=sr_measured,aes(x=ST_10_30,y=50),shape = "|", size = 4,col="midnightblue")+
  geom_point(data=sr_spatial,aes(x=ST_10_30,y=62),shape = "|", size = 4,col="turquoise4")+
  geom_text(label="b", x=1, y=210,size=8)
plot_par_ST_10_30


par_SM_10_30_sp <- partial(rfmodel_spatial, pred.var = c("SM_10_30"),rug=TRUE, data=sr_spatial)
par_SM_10_30_me <- partial(rfmodel_measured, pred.var = c("SM_10_30"),rug=TRUE, data=sr_measured)

plot_par_SM_10_30 <- autoplot(par_SM_10_30_sp, contour = TRUE, size=1.5,col="turquoise4")+
  geom_line(data=par_SM_10_30_me,aes(x=SM_10_30,y=yhat),  size=1.5,col="midnightblue")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("soil moisture 10-30 cm [%]")+
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylim(c(50,220))+
  geom_point(data=sr_measured,aes(x=SM_10_30,y=50),shape = "|", size = 4,col="midnightblue")+
  geom_point(data=sr_spatial,aes(x=SM_10_30,y=62),shape = "|", size = 4,col="turquoise4")+
  geom_text(label="d", x=2, y=210,size=8)
plot_par_SM_10_30


par_SOC_10_30_sp <- partial(rfmodel_spatial, pred.var = c("SOC_10_30"),rug=TRUE, data=sr_spatial)
par_SOC_10_30_me <- partial(rfmodel_measured, pred.var = c("SOC_10_30"),rug=TRUE, data=sr_measured)

plot_par_SOC_10_30 <- autoplot(par_SOC_10_30_sp, contour = TRUE, size=1.5,col="turquoise4")+
  geom_line(data=par_SOC_10_30_me,aes(x=SOC_10_30,y=yhat),  size=1.5,col="midnightblue")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("SOC 10-30 cm [t/ha]")+
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylim(c(50,220))+
  geom_point(data=sr_measured,aes(x=SOC_10_30,y=50),shape = "|", size = 4,col="midnightblue")+
  geom_point(data=sr_spatial,aes(x=SOC_10_30,y=62),shape = "|", size = 4,col="turquoise4")+
  geom_text(label="g", x=15, y=210,size=8)


par_landuse_sp <- partial(rfmodel_spatial, pred.var = c("land_use"),rug=TRUE, data=sr_spatial)
par_landuse_me <- partial(rfmodel_measured, pred.var = c("land_use"),rug=TRUE, data=sr_measured)

par_landuse_sp$land_use <- as.factor(par_landuse_sp$land_use)
par_landuse_me$land_use <- as.factor(par_landuse_me$land_use)

plot_par_landuse <- ggplot(data=par_landuse_sp, aes(x=land_use, y=yhat)) +
  geom_point(size=0)+
  geom_segment(aes(x=as.numeric(land_use) - 0.25, xend=as.numeric(land_use) + 0.25, y=yhat, yend=yhat), 
               size=1.5, color="turquoise4") +  
  geom_segment(data=par_landuse_me, aes(x=as.numeric(land_use) - 0.25, xend=as.numeric(land_use) + 0.25, y=yhat, yend=yhat), 
               size=1.5, color="midnightblue") + 
  scale_x_discrete(labels=c("1000" = "forest", "1500" = "arable", "1600" = "grass")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]")) +
  xlab("land use") +
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14))+
  ylim(c(50,220))+
  geom_text(label="k", x=0.6, y=210,size=8)


par_ndvi_sp <- partial(rfmodel_spatial, pred.var = c("ndvi"),rug=TRUE, data=sr_spatial)
par_ndvi_me <- partial(rfmodel_measured, pred.var = c("ndvi"),rug=TRUE, data=sr_measured)

plot_par_ndvi <- autoplot(par_ndvi_sp, contour = TRUE, size=1.5,col="turquoise4")+
  geom_line(data=par_ndvi_me,aes(x=ndvi,y=yhat),  size=1.5,col="midnightblue")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("NDVI")+
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylim(c(50,220))+
  geom_point(data=sr_measured,aes(x=ndvi,y=50),shape = "|", size = 4,col="midnightblue")+
  geom_point(data=sr_spatial,aes(x=ndvi,y=62),shape = "|", size = 4,col="turquoise4")+
  geom_text(label="l", x=0, y=210,size=8)


par_ndvi_dif_sp <- partial(rfmodel_spatial, pred.var = c("ndvi_dif"),rug=TRUE, data=sr_spatial)
par_ndvi_dif_me <- partial(rfmodel_measured, pred.var = c("ndvi_dif"),rug=TRUE, data=sr_measured)

plot_par_ndvi_dif <- autoplot(par_ndvi_dif_sp, contour = TRUE, size=1.5,col="turquoise4")+
  geom_line(data=par_ndvi_dif_me,aes(x=ndvi_dif,y=yhat),  size=1.5,col="midnightblue")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("NDVI change over two weeks")+
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylim(c(50,220))+
  geom_point(data=sr_measured,aes(x=ndvi_dif,y=50),shape = "|", size = 4,col="midnightblue")+
  geom_point(data=sr_spatial,aes(x=ndvi_dif,y=62),shape = "|", size = 4,col="turquoise4")+
  geom_text(label="m", x=-0.35, y=210,size=8)


par_twi_sp <- partial(rfmodel_spatial, pred.var = c("TWI"),rug=TRUE, data=sr_spatial)
par_twi_me <- partial(rfmodel_measured, pred.var = c("TWI"),rug=TRUE, data=sr_measured)

plot_par_twi <- autoplot(par_twi_sp, contour = TRUE, size=1.5,col="turquoise4")+
  geom_line(data=par_twi_me,aes(x=TWI,y=yhat),  size=1.5,col="midnightblue")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("TWI")+
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylim(c(50,220))+
  geom_point(data=sr_measured,aes(x=TWI,y=50),shape = "|", size = 4,col="midnightblue")+
  geom_point(data=sr_spatial,aes(x=TWI,y=62),shape = "|", size = 4,col="turquoise4")+
  geom_text(label="n", x=8, y=210,size=8)


par_slope_sp <- partial(rfmodel_spatial, pred.var = c("slope"),rug=TRUE, data=sr_spatial)
par_slope_me <- partial(rfmodel_measured, pred.var = c("slope"),rug=TRUE, data=sr_measured)

plot_par_slope <- autoplot(par_slope_sp, contour = TRUE, size=1.5,col="turquoise4")+
  geom_line(data=par_slope_me,aes(x=slope,y=yhat),  size=1.5,col="midnightblue")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("slope [%]")+
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylim(c(50,220))+
  geom_point(data=sr_measured,aes(x=slope,y=50),shape = "|", size = 4,col="midnightblue")+
  geom_point(data=sr_spatial,aes(x=slope,y=62),shape = "|", size = 4,col="turquoise4")+
  geom_text(label="o", x=1, y=210,size=8)


par_WHC <- partial(rfmodel_measured, pred.var = c("WHCact"),rug=TRUE, data=sr_measured)

plot_par_whc <- autoplot(par_WHC, contour = TRUE, size=1.5,col="midnightblue")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("% WHC 0-10 cm [%]")+
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  geom_point(data=sr_measured,aes(x=WHCact,y=50),shape = "|", size = 4,col="midnightblue")+
  ylim(c(50,220))+
  geom_text(label="e", x=13, y=210,size=8)


par_cn <- partial(rfmodel_measured, pred.var = c("cn"),rug=TRUE, data=sr_measured)

plot_par_cn <- autoplot(par_cn, contour = TRUE, size=1.5,col="midnightblue")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("C/N 0-10 cm")+
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  geom_point(data=sr_measured,aes(x=cn,y=50),shape = "|", size = 4,col="midnightblue")+
  ylim(c(50,220))+
  geom_text(label="h", x=11, y=210,size=8)


par_ph <- partial(rfmodel_measured, pred.var = c("ph"),rug=TRUE, data=sr_measured)

plot_par_ph <- autoplot(par_ph, contour = TRUE, size=1.5,col="midnightblue")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("pH  0-10 cm")+
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  geom_point(data=sr_measured,aes(x=ph,y=50),shape = "|", size = 4,col="midnightblue")+
  ylim(c(50,220))+
  geom_text(label="j", x=3.7, y=210,size=8)+
  geom_text(label="model including", x=6.2, y=165,size=5,col="midnightblue")+
  geom_text(label=" all predictors", x=6.2, y=150,size=5,col="midnightblue")+
  geom_text(label="model for spatial", x=6.2, y=200,size=5,col="turquoise4")+
  geom_text(label="predictions", x=6.2, y=185,size=5,col="turquoise4")
  

par_bd <- partial(rfmodel_measured, pred.var = c("bulk_density"),rug=TRUE, data=sr_measured)

plot_par_bd <- autoplot(par_bd, contour = TRUE, size=1.5,col="midnightblue")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("bulk density  0-10 cm [g/cm³]")+
  theme(axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  geom_point(data=sr_measured,aes(x=bulk_density,y=50),shape = "|", size = 4,col="midnightblue")+
  ylim(c(50,220))+
  geom_text(label="i", x=0.13, y=210,size=8)



ggpubr::ggarrange(plot_par_ST_0_10,plot_par_ST_10_30,plot_par_SM_0_10,plot_par_SM_10_30,plot_par_whc,
                  plot_par_SOC_0_10,plot_par_SOC_10_30,plot_par_cn,plot_par_bd,plot_par_ph,
                  plot_par_landuse,plot_par_ndvi,plot_par_ndvi_dif,
                  plot_par_twi, plot_par_slope,
                  ncol = 5, nrow = 3, widths = c(1.27, 1,1,1,1))

