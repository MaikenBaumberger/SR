
library(sf)
library(terra)
library(viridis)

directory <- "" #fill in the directory to the folder 

######################################
#load soil respiration predictions and AOA (data is available on request)

rastlist <- list.files(path = directory, pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)

rastlist_AOA <- list.files(path = file.path(directory,"/AOA"), pattern='.tif$', 
                       all.files=TRUE, full.names=FALSE)

dates <- gsub("prediction_res_pr_|\\.tif", "", rastlist)

allrasters <- lapply(rastlist, rast)

stk1 <- terra::rast(rastlist)

names(stk1) <- dates

stk_AOA <- terra::rast(rastlist_AOA)

names(stk_AOA) <- dates

#######################################
#calculate spatio-temporal statistics

calc_stats <- function(r) {
  vals <- values(r, na.rm = TRUE)  
  c(mean = mean(vals), 
    sd = sd(vals), 
    min = min(vals), 
    max = max(vals), 
    range = max(vals) - min(vals))
}
stats_list <- lapply(1:nlyr(stk1), function(i) calc_stats(stk1[[i]]))
stats_df <- data.frame(Date = dates, do.call(rbind, stats_list))
stats_df$month <- substr(stats_df$Date, 5, 6)
stats_df_all <- stats_df
stats_df$Date <- NULL
monthly_means <- aggregate(. ~ month, data = stats_df, FUN = mean)
monthly_min <- aggregate(. ~ month, data = stats_df, FUN = min)
monthly_max <- aggregate(. ~ month, data = stats_df, FUN = max)

monthly_stats<- data.frame(c(monthly_means[1:3],monthly_min[4:4],monthly_max[5:6]))


###################################
#create raster of monthly mean

jan <- mean(stk1[[1:8]])
feb <- mean(stk1[[9:16]])
mar <- mean(stk1[[17:24]])
apr <- mean(stk1[[25:32]])
may <- mean(stk1[[33:40]])
jun <- mean(stk1[[41:48]])
jul <- mean(stk1[[49:56]])
aug <- mean(stk1[[57:64]])
sep <- mean(stk1[[65:72]])
oct <- mean(stk1[[73:80]])
nov <- mean(stk1[[81:88]])
dec <- mean(stk1[[89:96]])


######################################
#calculate number of pixels outside AOA

count_AOA_jan <- app(stk_AOA[[1:8]], function(x) sum(x == 0, na.rm = TRUE))
count_AOA_feb <- app(stk_AOA[[9:16]], function(x) sum(x == 0, na.rm = TRUE))
count_AOA_mar <- app(stk_AOA[[17:24]], function(x) sum(x == 0, na.rm = TRUE))
count_AOA_apr <- app(stk_AOA[[25:32]], function(x) sum(x == 0, na.rm = TRUE))
count_AOA_may <- app(stk_AOA[[33:40]], function(x) sum(x == 0, na.rm = TRUE))
count_AOA_jun <- app(stk_AOA[[41:48]], function(x) sum(x == 0, na.rm = TRUE))
count_AOA_jul <- app(stk_AOA[[49:56]], function(x) sum(x == 0, na.rm = TRUE))
count_AOA_aug <- app(stk_AOA[[57:64]], function(x) sum(x == 0, na.rm = TRUE))
count_AOA_sep <- app(stk_AOA[[65:72]], function(x) sum(x == 0, na.rm = TRUE))
count_AOA_oct <- app(stk_AOA[[73:80]], function(x) sum(x == 0, na.rm = TRUE))
count_AOA_nov <- app(stk_AOA[[81:88]], function(x) sum(x == 0, na.rm = TRUE))
count_AOA_dec <- app(stk_AOA[[89:96]], function(x) sum(x == 0, na.rm = TRUE))


par(mfrow=c(3,4))
plot(count_AOA_jan)
plot(count_AOA_feb)
plot(count_AOA_mar)
plot(count_AOA_apr)
plot(count_AOA_may)
plot(count_AOA_jun)
plot(count_AOA_jul)
plot(count_AOA_aug)
plot(count_AOA_sep)
plot(count_AOA_oct)
plot(count_AOA_nov)
plot(count_AOA_dec)


##################################
#visualization

cols=viridisLite::viridis(40,direction = 1)#-1
b <- seq(from=0,to=400,by=10)


layout_matrix <- matrix(
  c(1, 2, 3, 4,5, 
    6, 7, 8, 9,5, 
    6, 7, 8, 9,10, 
    11, 12, 13, 14,10), 
  ncol = 5, 
  byrow = TRUE
)

heights <- c(1, 0.5, 0.5,1)
widths <- c(1,1,1,1,0.5)
layout(layout_matrix,heights=heights,widths=widths)
par(mar = c(0, 0, 0, 0))  


plot(jan, col = cols, breaks = b, axes=F,legend=F,colNA="grey75",main=expression("January"),cex.main=2.5)
plot(count_AOA_jan>4,col=c("transparent","red"),add=TRUE,legend=F)
plot(feb, col = cols, breaks = b, axes=F,legend=F,colNA="grey75",main=expression("February"),cex.main=2.5)
plot(count_AOA_feb>4,col=c("transparent","red"),add=TRUE,legend=F)
plot(mar, col = cols, breaks = b, axes=F,legend=F,colNA="grey75",main=expression("March"),cex.main=2.5)
plot(count_AOA_mar>4,col=c("transparent","red"),add=TRUE,legend=F)
plot(apr, col = cols, breaks = b, axes=F,legend=F,colNA="grey75",main=expression("April"),cex.main=2.5)
plot(count_AOA_apr>4,col=c("transparent","red"),add=TRUE,legend=F)

min_value <- 0
max_value <- 400

colors <- viridis(100) 
values <- seq(min_value, max_value, length.out = 100)

par(mar = c(0, 0, 13, 0))
plot.new()
plot.window(xlim = c(0, 1), ylim = c(min_value, max_value))


rect(
  xleft = 0.1, xright = 0.25,
  ybottom = head(values, -1), 
  ytop = tail(values, -1), 
  col = colors, border = NA
)


axis(
  side = 4,                   
  at = seq(min_value, max_value, by = 50), 
  las = 1,                    
  line = -9.5,                
  tick = FALSE,
  cex.axis = 1.8
)

mtext(
  expression(CO[2]~"efflux"~"[mg C "*m^-2*h^-1*"]"), 
  side = 4,     
  line = -3,    
  las = 0,      
  cex = 1.3    
)


par(mar = c(0, 0, 0, 0))  
plot(may, col = cols, breaks = b, axes=F,legend=F,colNA="grey75",main=expression("May"),cex.main=2.5)
plot(count_AOA_may>4,col=c("transparent","red"),add=TRUE,legend=F)
plot(jun, col = cols, breaks = b, axes=F,legend=F,colNA="grey75",main=expression("June"),cex.main=2.5)
plot(count_AOA_jun>4,col=c("transparent","red"),add=TRUE,legend=F)
plot(jul, col = cols, breaks = b, axes=F,legend=F,colNA="grey75",main=expression("July"),cex.main=2.5)
plot(count_AOA_jul>4,col=c("transparent","red"),add=TRUE,legend=F)
plot(aug, col = cols, breaks = b, axes=F,legend=F,colNA="grey75",main=expression("August"),cex.main=2.5)
plot(count_AOA_aug>4,col=c("transparent","red"),add=TRUE,legend=F)


plot(1, type = "n", xlim = c(0, 3), ylim = c(0, 3), axes = FALSE, xlab = "", ylab = "")
square_size <- 0.4  
y_bottom <- 0.8
y_top <- y_bottom + square_size -0.25
rect(0.6, y_bottom, 0.6 + square_size, y_top, col = "grey75", border = "black")
rect(1.6, y_bottom, 1.6 + square_size, y_top, col = "red", border = "black")
text(0.8, y_top + 0.8, expression("outside area of interest"), srt = 90, cex = 2, col = "black", font = 2)
text(1.8, y_top + 0.93, expression("outside area of applicability"), srt = 90, cex = 2, col = "black", font = 2)


plot(sep, col = cols, breaks = b, axes=F,legend=F,colNA="grey75",main=expression("September"),cex.main=2.5)
plot(count_AOA_sep>4,col=c("transparent","red"),add=TRUE,legend=F)
plot(oct, col = cols, breaks = b, axes=F,legend=F,colNA="grey75",main=expression("October"),cex.main=2.5)
plot(count_AOA_oct>4,col=c("transparent","red"),add=TRUE,legend=F)
plot(nov, col = cols, breaks = b, axes=F,legend=F,colNA="grey75",main=expression("November"),cex.main=2.5)
plot(count_AOA_nov>4,col=c("transparent","red"),add=TRUE,legend=F)
plot(dec, col = cols, breaks = b, axes=F,legend=F,colNA="grey75",main=expression("December"),cex.main=2.5)
plot(count_AOA_dec>4,col=c("transparent","red"),add=TRUE,legend=F)

#pdf 15x10
