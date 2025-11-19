
library("scales")
library("ggplot2")
library(dplyr)
library(purrr)

directory <- "" #fill in the directory to the folder 


#########################################
#load and prepare spatio-temporal soil respiration data and predictors

load(file.path(directory,"sr_data.Rdata"))

sr <- sr_pred_filled

sr_base <- data.frame(c(sr[1:8],sr[10:38]))

sr_base$date <- as.Date(sr_base$date)


sr_base$LULC[sr_base$LULC == "coniferous_forest"] <- "forest"
sr_base$LULC[sr_base$LULC == "arable_land"] <- "arable land"
sr_base$LULC[sr_base$LULC == "meadow"] <- "grassland"


#########################################
# function to calculate rolling meand and sd

calculate_rolling_stats <- function(data, time_window_days) {
  
  data <- data %>%
    arrange(date) %>%
    mutate(
      flow_mean = map_dbl(date, ~ {
        window_data <- data %>%
          filter(date >= (.x - time_window_days) & date <= .x + time_window_days)
        mean(window_data$flow, na.rm = TRUE)
      }),
      flow_sd = map_dbl(date, ~ {
        window_data <- data %>%
          filter(date >= (.x - time_window_days) & date <= .x + time_window_days)
        sd(window_data$flow, na.rm = TRUE)
      })
    )
  
  return(data)
}

Sys.setlocale("LC_TIME", "C")


###########################################
#group data by land use and calculate statistics

time_window_days <- 30 
sr_base <- sr_base %>%
  group_by(LULC) %>%
  group_modify(~ calculate_rolling_stats(.x, time_window_days)) %>%
  ungroup()


############################################
#plot data and mean temporal course of soil respiration per land use

ggplot(sr_base, aes(x = date, y = flow, color = factor(LULC))) +
  theme_bw(base_size = 16)+
  ylim(c(-0,700))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title=element_blank(),
        legend.position = c(0.08,0.84),
        axis.title.x=element_blank(),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 13, color = "black"),
        axis.text.y = element_text(size = 13, color = "black"))+
  scale_color_manual(values = c("#FFCC00","darkgreen", "chartreuse3")) +
  scale_fill_manual(values = c( "#FFCC00","darkgreen", "chartreuse3")) +
  geom_point(size = 1.5)+
  stat_smooth(span = 0.5,se=F)+
  ylab(expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"))+
  xlab("")+
  scale_x_date(labels = date_format("%b %Y"))+
  theme(plot.margin = margin(t = 0.1,  r = 1,  b = 0.1, l = 0.1, unit = "cm"))


########################################################################
