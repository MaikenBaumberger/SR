directory <- "" #fill in the directory to the folder 


#########################################
#load spatio-temporal soil respiration data with predictors

load(file.path(directory,"sr_data.Rdata"))

sr_spatial <- sr_pred_filled

sr_measured_grassland <- sr_spatial[sr_spatial$land_use==1600,]
sr_measured_forest <- sr_spatial[sr_spatial$land_use==1000,]
sr_measured_arable <- sr_spatial[sr_spatial$land_use==1500,]

par(mfrow=c(1,3),mar=c(5,5,0.5,0.5),mgp = c(3, 0.75, 0))


#########################################
#plot temperature dependency of soil respiration for spatio-temporal distributed plots at forest sites

sr_measured_forest <- sr_measured_forest[order(sr_measured_forest$ST_0_10), ]

temp=sr_measured_forest$ST_0_10
resp=sr_measured_forest$flow/100

tab<-matrix(c(temp, resp), length(temp))
mod <- nls(tab[,2] ~ (a*exp(b * tab[,1])), start = list(a = 0.2, b = 0))
coeff_b<-summary(mod)$coefficients[2,1]
Q10<-exp(10*coeff_b)
plot(tab[,1], tab[,2],col="darkgreen",pch=19,ylim=c(0,5),xlim=c(0,25),cex.axis=1.5,cex.lab=1.5,
     ylab=expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"),xlab="soil temperature [°C]",yaxt = "n")
lines(tab[,1], predict(mod, list(x = tab[,2])),lwd=2)
text(2.7,4.85,"forest",cex=1.5)
text(4,4.45,"Q10 = 2.6",cex=1.5)
text(4,4.05,"R² = 0.50",cex=1.5)
text(24,4.85,expression(bold("a")),cex=2)
axis(2, at = c(0,1,2,3,4,5), labels = c(0,100,200,300,400,500),cex.axis=1.5,las=2)
Q10

predicted <- predict(mod)
observed <- tab[, 2]
mean_observed <- mean(observed)
SS_residual <- sum((observed - predicted)^2)
SS_total <- sum((observed - mean_observed)^2)
R2 <- 1 - (SS_residual / SS_total)
print(paste("R²:", R2))
mod

##################################
#plot temperature dependency of soil respiration for spatio-temporal distributed plots at grassland sites

sr_measured_grassland <- sr_measured_grassland[order(sr_measured_grassland$ST_0_10), ]

temp=sr_measured_grassland$ST_0_10
resp=sr_measured_grassland$flow/100
#plot(temp,resp)

tab<-matrix(c(temp, resp), length(temp))
mod <- nls(tab[,2] ~ (a*exp(b * tab[,1])), start = list(a = 0.2, b = 0))
coeff_b<-summary(mod)$coefficients[2,1]
Q10<-exp(10*coeff_b)
plot(tab[,1], tab[,2],col="chartreuse3",pch=19,ylim=c(0,5),xlim=c(0,25),cex.axis=1.5,cex.lab=1.5,
     ylab="",xlab="soil temperature [°C]",yaxt = "n")
lines(tab[,1], predict(mod, list(x = tab[,2])),lwd=2)
text(3.8,4.85,"grassland",cex=1.5)
text(4,4.45,"Q10 = 2.2",cex=1.5)
text(3.9,4.05,"R² = 0.41",cex=1.5)
text(24,4.85,expression(bold("b")),cex=2)
axis(2, at = c(0,1,2,3,4,5),  labels = c("","","","","",""), cex.axis=1.5)
Q10

predicted <- predict(mod)
observed <- tab[, 2]
mean_observed <- mean(observed)
SS_residual <- sum((observed - predicted)^2)
SS_total <- sum((observed - mean_observed)^2)
R2 <- 1 - (SS_residual / SS_total)
print(paste("R²:", R2))


##################################
#plot temperature dependency of soil respiration for spatio-temporal distributed plots at arable land sites

sr_measured_arable <- sr_measured_arable[order(sr_measured_arable$ST_0_10), ]

temp=sr_measured_arable$ST_0_10
resp=sr_measured_arable$flow/100

tab<-matrix(c(temp, resp), length(temp))
mod <- nls(tab[,2] ~ (a*exp(b * tab[,1])), start = list(a = 0.2, b = 0))
coeff_b<-summary(mod)$coefficients[2,1]
Q10<-exp(10*coeff_b)
plot(tab[,1], tab[,2],col="#FFCC00",pch=19,ylim=c(0,5),xlim=c(0,25),cex.axis=1.5,cex.lab=1.5,
     ylab="",xlab="soil temperature [°C]",yaxt = "n")
lines(tab[,1], predict(mod, list(x = tab[,2])),lwd=2)
text(4.2,4.85,"arable land",cex=1.5)
text(4,4.45,"Q10 = 1.6",cex=1.5)
text(3.9,4.05,"R² = 0.14",cex=1.5)
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


