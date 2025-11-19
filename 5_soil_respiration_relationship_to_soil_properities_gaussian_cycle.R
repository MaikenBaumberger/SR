directory <- "" #fill in the directory to the folder 


#########################################
#load and prepare spatio-temporal soil respiration data with predictors

load(file.path(directory,"sr_data.Rdata"))

sr_spatial <- data.frame(sr_pred_filled$chamber_date_time,
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
                          sr_pred_filled$WHC,
                          sr_pred_filled$WHCact,
                          sr_pred_filled$per_n,
                          sr_pred_filled$SOC_0_10,
                          sr_pred_filled$SM_0_10,
                          sr_pred_filled$ST_0_10,
                          sr_pred_filled$SOC_0_30,
                          sr_pred_filled$SM_0_30,
                          sr_pred_filled$ST_0_30,
                          sr_pred_filled$SOC_10_30,
                          sr_pred_filled$SM_10_30,
                          sr_pred_filled$ST_10_30)

names(sr_spatial) <- c("chamber_date_time","plot_id","id","flow","land_use", "TWI", "slope", "ndvi", "ndvi_dif", "ph", "bulk_density","cn",
                        "WHC","WHCact","N","SOC_0_10", "SM_0_10", "ST_0_10","SOC_0_30", "SM_0_30", "ST_0_30","SOC_10_30", "SM_10_30", "ST_10_30")



levels(sr_spatial$land_use)[levels(sr_spatial$land_use)==1000] <- "forest"
levels(sr_spatial$land_use)[levels(sr_spatial$land_use)==1500] <- "arable land"
levels(sr_spatial$land_use)[levels(sr_spatial$land_use)==1600] <- "grassland"

sr_measured <- sr_spatial[complete.cases(sr_spatial), ]

color_mapping <- c("forest" = "darkgreen","arable land"="#FFCC00","grassland"= "chartreuse3")  
label_mapping <- c("forest" = "forest", "arable land" = "arable land", "grassland" = "grassland")


#########################################
# create plots showing the relationship between soil respiration and its predictors seperated by land use

ST_0_10 <- ggplot(sr_spatial, aes(x = ST_0_10, y = flow, color=land_use)) +
  geom_point()+
  stat_ellipse(size=1)+
  #stat_ellipse(data=sr_spatial, aes(x = ST_0_10, y = flow), inherit.aes = FALSE,color="grey70",size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #plot.margin=margin(0,0,0,0),
        legend.position = "none",
        axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14))+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("soil temperature 0-10 cm [°C]")+
  scale_colour_manual(values = color_mapping, labels = label_mapping)+
  ylim(-150,700)+
  geom_text(label="a", x=-7.7, y=670,size=8, col="black")

ST_0_10

ST_10_30 <- ggplot(sr_spatial, aes(x = ST_10_30, y = flow, color=land_use)) +
  geom_point()+
  stat_ellipse(size=1)+
  #stat_ellipse(data=sr_spatial, aes(x = ST_10_30, y = flow), inherit.aes = FALSE,color="grey70",size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("soil temperature 10-30 cm [°C]")+
  scale_colour_manual(values = color_mapping, labels = label_mapping)+
  ylim(-150,700)+
  geom_text(label="b", x=-6, y=670,size=8, col="black")



SM_0_10 <- ggplot(sr_spatial, aes(x = SM_0_10, y = flow, color=land_use)) +
  geom_point()+
  stat_ellipse(size=1)+
  #stat_ellipse(data=sr_spatial, aes(x = SM_0_10, y = flow), inherit.aes = FALSE,color="grey70",size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("soil moisture 0-10 cm [%]")+
  scale_colour_manual(values = color_mapping, labels = label_mapping)+
  ylim(-150,700)+
  geom_text(label="c", x=-4, y=670,size=8, col="black")



SM_10_30 <- ggplot(sr_spatial, aes(x = SM_10_30, y = flow, color=land_use)) +
  geom_point()+
  stat_ellipse(size=1)+
  #stat_ellipse(data=sr_spatial, aes(x = SM_10_30, y = flow), inherit.aes = FALSE,color="grey70",size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("soil moisture 10-30 cm [%]")+
  scale_colour_manual(values = color_mapping, labels = label_mapping)+
  ylim(-150,700)+
  geom_text(label="d", x=2, y=670,size=8, col="black")



WHCact <- ggplot(sr_measured, aes(x = WHCact, y = flow, color=land_use)) +
  geom_point()+
  stat_ellipse(size=1)+
  #stat_ellipse(data=sr_measured, aes(x = WHCact, y = flow), inherit.aes = FALSE,color="grey70",size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("% WHC 0-10 cm [%]")+
  scale_colour_manual(values = color_mapping, labels = label_mapping)+
  ylim(-150,700)+
  geom_text(label="e", x=6, y=670,size=8, col="black")


WHC <- ggplot(sr_measured, aes(x = WHC, y = flow, color=land_use)) +
  geom_point()+
  stat_ellipse(size=1)+
  #stat_ellipse(data=sr_measured, aes(x = WHC, y = flow), inherit.aes = FALSE,color="grey70",size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("WHC")+
  scale_colour_manual(values = color_mapping, labels = label_mapping)+
  ylim(-100,600)


SOC_0_10 <- ggplot(sr_measured, aes(x = SOC_0_10, y = flow, color=land_use)) +
  geom_point()+
  stat_ellipse(size=1)+
  #stat_ellipse(data=sr_measured, aes(x = SOC_0_10, y = flow), inherit.aes = FALSE,color="grey70",size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        #plot.margin=margin(0,0,0,0),
        legend.position = "none",
        axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14))+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("SOC 0-10 cm [t/ha]")+
  scale_colour_manual(values = color_mapping, labels = label_mapping)+
  ylim(-150,700)+
  geom_text(label="f", x=5, y=670,size=8, col="black")
  #scale_x_continuous(expand = c(0.01,0.01))+
  #scale_y_continuous(expand = c(0.01,0.01))


SOC_10_30 <- ggplot(sr_measured, aes(x = SOC_10_30, y = flow, color=land_use)) +
  geom_point()+
  stat_ellipse(size=1)+
  #stat_ellipse(data=sr_measured, aes(x = SOC_10_30, y = flow), inherit.aes = FALSE,color="grey70",size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("SOC 10-30 cm [t/ha]")+
  scale_colour_manual(values = color_mapping, labels = label_mapping)+
  ylim(-150,700)+
  xlim(5,100)+
  geom_text(label="g", x=14, y=670,size=8, col="black")


CN <- ggplot(sr_measured, aes(x = cn, y = flow, color=land_use)) +
  geom_point()+
  stat_ellipse(size=1)+
  #stat_ellipse(data=sr_measured, aes(x = cn, y = flow), inherit.aes = FALSE,color="grey70",size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("C/N 0-10 cm")+
  scale_colour_manual(values = color_mapping, labels = label_mapping)+
  ylim(-150,700)+
  geom_text(label="h", x=11, y=670,size=8, col="black")

BD <- ggplot(sr_measured, aes(x = bulk_density, y = flow, color=land_use)) +
  geom_point()+
  stat_ellipse(size=1)+
  #stat_ellipse(data=sr_measured, aes(x = bulk_density, y = flow), inherit.aes = FALSE,color="grey70",size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("bulk density 0-10 cm [g/cm³]")+
  scale_colour_manual(values = color_mapping, labels = label_mapping)+
  ylim(-150,700)+
  xlim(-0.1,1.5)+
  geom_text(label="i", x=0, y=670,size=8, col="black")

PH <- ggplot(sr_measured, aes(x = ph, y = flow, color=land_use)) +
  geom_point()+
  stat_ellipse(size=1)+
  #stat_ellipse(data=sr_measured, aes(x = ph, y = flow), inherit.aes = FALSE,color="grey70",size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.title=element_blank(),
        legend.position = c(0.7,0.8),
        legend.text = element_text(size=14))+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("pH 0-10 cm")+
  scale_colour_manual(values = color_mapping, labels = label_mapping)+
  ylim(-150,700)+
  geom_text(label="j", x=3.7, y=670,size=8, col="black")


ndvi <- ggplot(sr_spatial, aes(x = ndvi, y = flow, color=land_use)) +
  geom_point()+
  stat_ellipse(size=1)+
  #stat_ellipse(data=sr_spatial, aes(x = ndvi, y = flow), inherit.aes = FALSE,color="grey70",size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("NDVI")+
  scale_colour_manual(values = color_mapping, labels = label_mapping)+
  ylim(-150,700)+
  xlim(-0.15,0.75)+
  geom_text(label="l", x=-0.1, y=670,size=8, col="black")

ndvi_dif <- ggplot(sr_spatial, aes(x = ndvi_dif, y = flow, color=land_use)) +
  geom_point()+
  stat_ellipse(size=1)+
  #stat_ellipse(data=sr_spatial, aes(x = ndvi_dif, y = flow), inherit.aes = FALSE,color="grey70",size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("NDVI change over two weeks")+
  scale_colour_manual(values = color_mapping, labels = label_mapping)+
  ylim(-150,700)+
  geom_text(label="m", x=-0.35, y=670,size=8, col="black")

twi <- ggplot(sr_spatial, aes(x = TWI, y = flow, color=land_use)) +
  geom_point()+
  stat_ellipse(size=1)+
  #stat_ellipse(data=sr_spatial, aes(x = TWI, y = flow), inherit.aes = FALSE,color="grey70",size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("TWI")+
  scale_colour_manual(values = color_mapping, labels = label_mapping)+
  ylim(-150,700)+
  geom_text(label="n", x=7.5, y=670,size=8, col="black")


slope <- ggplot(sr_spatial, aes(x = slope, y = flow, color=land_use)) +
  geom_point()+
  stat_ellipse(size=1)+
  #stat_ellipse(data=sr_spatial, aes(x = slope, y = flow), inherit.aes = FALSE,color="grey70",size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("slope [%]")+
  scale_colour_manual(values = color_mapping, labels = label_mapping)+
  ylim(-150,700)+
    geom_text(label="o", x=-1, y=670,size=8, col="black")


N <- ggplot(sr_measured, aes(x = N, y = flow, color=land_use)) +
  geom_point()+
  stat_ellipse(size=1)+
  #stat_ellipse(data=sr_measured, aes(x = N, y = flow), inherit.aes = FALSE,color="grey70",size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=14,color="black"),
        axis.title=element_text(size=14),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("N [%]")+
  scale_colour_manual(values = color_mapping, labels = label_mapping)+
  ylim(-100,600)


land_use <- ggplot(sr_spatial, aes(x = land_use, y = flow, color=land_use)) +
  geom_boxplot(size=1)+
  #stat_ellipse(size=1)+
  #stat_ellipse(data=sr_measured, aes(x = ph, y = flow), inherit.aes = FALSE,color="grey70",size=1)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text=element_text(size=14,color = "black"),
        axis.title=element_text(size=14))+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("land use")+
  scale_x_discrete(labels=c("forest","arable","grass"))+
  scale_colour_manual(values = color_mapping, labels = label_mapping)+
  ylim(-150,700)+
  geom_text(label="k", x=0.7, y=670,size=8, col="black")



ggpubr::ggarrange(ST_0_10,ST_10_30,SM_0_10,SM_10_30,WHCact,
                  SOC_0_10,SOC_10_30,CN,BD,PH,
                  land_use, ndvi, ndvi_dif,twi,slope,
                  ncol=5,nrow=3,widths = c(1.28,1,1,1,1))




