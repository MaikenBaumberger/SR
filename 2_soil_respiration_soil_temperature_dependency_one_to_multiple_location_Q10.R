directory <- "" #fill in the directory to the folder 


#########################################
# load and prepare continuous soil respiration measurementsfrom forest site (data is available on request)

co2_continous <- read.table(file.path(directory, "CO2_Daten.csv"), sep=";",header=T,dec=",")

co2_continous$date <- as.Date(co2_continous$Datum, format = "%d.%m.%Y")

plot(co2_continous$Tboden_10cm,co2_continous$FCO2_C_mg_m2_h)

co2_continous_k <- subset(co2_continous, Treatment=="K")
co2_continous_k <- subset(co2_continous, Plot=="P2")

table(co2_continous$Plot)

plot(co2_continous_k$Tboden_10cm,co2_continous_k$FCO2_C_mg_m2_h)


#########################################
#calculate mean for replicates

co2_continous_k_mean <- cbind(co2_continous_k[8:13])
head(co2_continous_k_mean)
library(dplyr)
data_summarized <- co2_continous_k_mean %>%
  group_by(date, Plot) %>%  
  summarise(
    mean_Ta = mean(Ta, na.rm = TRUE),  
    mean_FCO2_C_mg_m2_h = mean(FCO2_C_mg_m2_h, na.rm = TRUE),  
    mean_Tboden_10cm = mean(Tboden_10cm, na.rm = TRUE),  
  )

data_summarized[3:5] <- round(data_summarized[3:5] ,2)


#########################################
#load spatio-temporal soil respiration data and predictors
#load meteo data (data is available on request)

load(file.path(directory,"sr_data.Rdata"))

load(file.path(directory,"meteo_data.Rdata"))


#########################################
#add soil moisture data to soil respiration data

efflux_waldstein <- data.frame(data_summarized)

head(efflux_waldstein)

efflux_waldstein$datetime <- as.POSIXct(paste(efflux_waldstein$date, "12:00:00"), format = "%Y-%m-%d %H:%M:%S")

sm_waldstein <- data.frame(meteo_data$datetime,meteo_data$SoilMoiHourely)
names(sm_waldstein) <- c("datetime","soil_moisture")
sm_waldstein <- sm_waldstein[complete.cases(sm_waldstein), ]

efflux_waldstein_moi <- merge(efflux_waldstein,sm_waldstein,by="datetime")


#########################################
#select 60 spatio-temporal soil respiration measurements

set.seed(10)
sr_pred_filled_60 <- sr_pred_filled[sample(nrow(sr_pred_filled), 60),]


#########################################
#select 60 spatio-temporal soil respiration measurements from forest sites

sr_forest <- subset(sr_pred_filled, land_use==1000)

set.seed(10)
sr_forest_60 <- sr_forest[sample(nrow(sr_forest), 60),]


##########################################
#plot temperature dependency of soil respiration for on plot at a forest site

par(mfrow=c(1,3),mar=c(5,5,0.5,0.5),mgp = c(3, 0.75, 0))

efflux_waldstein_moi <- efflux_waldstein_moi[order(efflux_waldstein_moi$mean_Tboden_10cm), ]

temp=efflux_waldstein_moi$mean_Tboden_10cm
resp=efflux_waldstein_moi$mean_FCO2_C_mg_m2_h/100


tab<-matrix(c(temp, resp), length(temp))
mod <- nls(tab[,2] ~ (a*exp(b * tab[,1])), start = list(a = 0.2, b = 0))
coeff_b<-summary(mod)$coefficients[2,1]
Q10<-exp(10*coeff_b)
plot(tab[,1], tab[,2],col="black",pch=19,ylim=c(0,5),xlim=c(0,25),cex.axis=1.5,cex.lab=1.5,
     ylab=expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"),xlab="soil temperature [°C]",yaxt = "n")
lines(tab[,1], predict(mod, list(x = tab[,2])),lwd=2)
text(4,4.85,"Q10 = 3.5",cex=1.5)
text(4,4.45,"R² = 0.80",cex=1.5)
text(24,4.85,expression(bold("a")),cex=2)
axis(2, at = c(0,1,2,3,4,5), labels = c(0,100,200,300,400,500),cex.axis=1.5,las = 2)
Q10

predicted <- predict(mod)
observed <- tab[, 2]
mean_observed <- mean(observed)
SS_residual <- sum((observed - predicted)^2)
SS_total <- sum((observed - mean_observed)^2)
R2 <- 1 - (SS_residual / SS_total)
print(paste("R²:", R2))

##########################################
#plot temperature dependency of soil respiration for spatio-temporal distributed forest plots

sr_forest_60 <- sr_forest_60[order(sr_forest_60$ST_0_10), ]

temp=sr_forest_60$ST_0_10
resp=sr_forest_60$flow/100
#plot(temp,resp)

tab<-matrix(c(temp, resp), length(temp))
mod <- nls(tab[,2] ~ (a*exp(b * tab[,1])), start = list(a = 0.2, b = 0))
coeff_b<-summary(mod)$coefficients[2,1]
Q10<-exp(10*coeff_b)
plot(tab[,1], tab[,2],col="black",pch=19,ylim=c(0,5),xlim=c(0,25),cex.axis=1.5,cex.lab=1.5,
     ylab="",xlab="soil temperature [°C]",yaxt = "n")
lines(tab[,1], predict(mod, list(x = tab[,2])),lwd=2)
text(4,4.85,"Q10 = 2.5",cex=1.5)
text(4,4.45,"R² = 0.49",cex=1.5)
text(24,4.85,expression(bold("b")),cex=2)
axis(2, at = c(0,1,2,3,4,5), labels = c("","","","","",""),cex.axis=1.5)
Q10

predicted <- predict(mod)
observed <- tab[, 2]
mean_observed <- mean(observed)
SS_residual <- sum((observed - predicted)^2)
SS_total <- sum((observed - mean_observed)^2)
R2 <- 1 - (SS_residual / SS_total)
print(paste("R²:", R2))

##################################
#plot temperature dependency of soil respiration for spatio-temporal distributed plots at forest, grassland and arable land sites


sr_pred_filled_60 <- sr_pred_filled_60[order(sr_pred_filled_60$ST_0_10), ]

temp=sr_pred_filled_60$ST_0_10
resp=sr_pred_filled_60$flow/100
#plot(temp,resp)

tab<-matrix(c(temp, resp), length(temp))
mod <- nls(tab[,2] ~ (a*exp(b * tab[,1])), start = list(a = 0.2, b = 0))
coeff_b<-summary(mod)$coefficients[2,1]
Q10<-exp(10*coeff_b)
plot(tab[,1], tab[,2],col="black",pch=19,ylim=c(0,5),xlim=c(0,25),cex.axis=1.5,cex.lab=1.5,
     ylab="",xlab="soil temperature [°C]",yaxt = "n")
lines(tab[,1], predict(mod, list(x = tab[,2])),lwd=2)
text(4,4.85,"Q10 = 1.6",cex=1.5)
text(4,4.45,"R² = 0.22",cex=1.5)
text(24,4.85,expression(bold("c")),cex=2)
axis(2, at = c(0,1,2,3,4,5),  labels = c("","","","","",""), cex.axis=1.5)
Q10

predicted <- predict(mod)
observed <- tab[, 2]
mean_observed <- mean(observed)
SS_residual <- sum((observed - predicted)^2)
SS_total <- sum((observed - mean_observed)^2)
R2 <- 1 - (SS_residual / SS_total)
print(paste("R²:", R2))


