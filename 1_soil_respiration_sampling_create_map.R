library(sf)
library(terra)
library(ggplot2)
library(tmap)
library(dplyr)

directory <- "" #fill in the directory to the folder 

setwd(directory)

load(file.path(directory, "sr_data.Rdata"))

names(sr_pred_filled)


############################################
# categories of plots

repeat_plots <- c("P0103", "P0104", "P0105", "P0154", "P0155", "P0156", "P0203", "P0204", "P0205")

repeat_plots_data <- sr_pred_filled[sr_pred_filled$plotID %in% repeat_plots, ]

sr_data <- sr_pred_filled %>% filter(grepl("^PS", plotID))

auger_plots_data_2 <- sr_pred_filled %>% filter(grepl("^P0", plotID))
auger_plots_data <- auger_plots_data_2[!auger_plots_data_2$plotID %in% repeat_plots, ]

length(unique(auger_plots_data_2$plotID))

auger_plots <- data.frame(auger_plots_data$plotID,auger_plots_data$lat,auger_plots_data$lon)
names(auger_plots) <- c("plot_id","lat","lon")

sr_plots <- data.frame(sr_data$plotID,sr_data$lat,sr_data$lon,sr_data$flow)
names(sr_plots) <- c("plot_id","lat","lon","flow")

repeat_plots <- data.frame(repeat_plots_data$plotID,repeat_plots_data$lat,repeat_plots_data$lon)
names(repeat_plots) <- c("plot_id","lat","lon")

longterm_plot <- data.frame(plot_id = "P_long", lat = 50.14200, lon = 11.86660)


############################################
# convert to geodata

sr_plots$coordinates <- sf::st_as_sf(sr_plots,coords = c("lon","lat"), crs = 4326)
sr_plots_sf <- sf::st_as_sf(sr_plots,coords = c("lon","lat"), crs = 4326)

auger_plots$coordinates <- sf::st_as_sf(auger_plots,coords = c("lon","lat"), crs = 4326)
auger_plots_sf <- sf::st_as_sf(auger_plots,coords = c("lon","lat"), crs = 4326)

repeat_plots$coordinates <- sf::st_as_sf(repeat_plots,coords = c("lon","lat"), crs = 4326)
repeat_plots_sf <- sf::st_as_sf(repeat_plots,coords = c("lon","lat"), crs = 4326)

longterm_plot$coordinates <- sf::st_as_sf(longterm_plot,coords = c("lon","lat"), crs = 4326)
longterm_plots_sf <- sf::st_as_sf(longterm_plot,coords = c("lon","lat"), crs = 4326)


#########################################
# load satellite data 
 
sentinel <- terra::rast("sentinel_studyarea_20220725.tif")
sentinel_rgb <-  terra::rast("sentinel_studyarea_20220725_utm.tif")


#########################################
# create and save plot


map = tm_shape(sentinel_rgb)+
  tm_rgb(r = 1, g = 2, b = 3,  max.value =0.995,saturation = 1.2)+
  tm_shape(sr_plots_sf)+
  tm_dots(size=0.3,col="skyblue")+
  tm_shape(auger_plots_sf)+
  tm_dots(size=0.3,col="yellow")+
  tm_shape(repeat_plots_sf)+
  tm_dots(size=0.3,shape=15,col="yellow")+
  tm_shape(longterm_plots_sf)+
  tm_dots(size=0.3,shape=15,col="green")+
  tm_layout(legend.bg.color = "white",
            inner.margins= c(0.02,0.017,0.0,0.0), #c(0.03,0.017,0.0,0.0), 
            outer.margins = c(0,0,0,0), 
            legend.bg.alpha = 0.6,
            legend.title.size = 1.6,
            legend.text.size = 1.1,
            legend.position = c("left", "bottom"),
            legend.width = 0.57, 
            frame = FALSE)+
  tm_compass(position = c("right", "top"), size = 1,text.color="white",color.dark="white")+
  tm_scale_bar(position = c("center", "bottom"), width = 0.18,text.size=0.85,text.color="white")+
  tm_add_legend("symbol",title="sampling design", labels=c("SR + SP + SC","SR + SP","SR","temporal replicates"),
                col=c("yellow","skyblue","green","grey50"),shape=c(16,16,16,15))

map


pdf(file.path(directory, "sampling.pdf"),
    width= 6, 
    height= 6)

map

dev.off()

