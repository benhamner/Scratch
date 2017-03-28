library(ggplot2)
library(grid)
library(scales)
library(multiplot)

num_months <- 48

df <- data.frame(Months   = 1:num_months,
                 Actual   = rep(NA, num_months),
                 Forecast = rep(NA, num_months))

df$Actual[1:10] = c(5000, 25000, 15000, 8000, 10000, 5200, 7500, 5600, 3500, 3000)
df$Forecast[11:num_months] = 9000*exp(-0.1*(11:num_months))

df <- melt(df, id="Months", measure=c("Actual", "Forecast"))
df <- rename(df, c(value="Production", variable="Type"))

base_plot <- ggplot(data=df, aes(x=Months, y=Production)) + 
  geom_bar(aes(alpha=Type), stat="identity") +
  scale_alpha_manual(values=c(1.0, 0.5), guide=FALSE)
  theme_grey(base_size=20) +
#  scale_y_log10(labels=comma, breaks=c(1000, 10000)) + 
  ylim(0, 30000)
base_plot

first_three <- base_plot + 
  geom_bar(data=df[df$Type=="Actual" & df$Months<=3,], stat="identity", fill="red") + 
  ggtitle("First Three Months Production") +  theme(plot.title=element_text(colour="red"))
first_three

first_six <- base_plot + 
  geom_bar(data=df[df$Type=="Actual" & df$Months<=6,], stat="identity", fill="blue") + 
  ggtitle("First Six Months Production") + theme(plot.title=element_text(colour="blue"))
first_six

best_six_data <- copy(df)
best_six_data[best_six_data$Production<5500 & !is.na(best_six_data$Production),]$Production <- NA

best_six  <- base_plot +
  geom_bar(data=best_six_data, stat="identity", fill="green") + 
  ggtitle("Best Six Months Production") + theme(plot.title=element_text(colour="green"))
best_six

eur  <- base_plot +
  geom_bar(stat="identity", fill="orange") + 
  ggtitle("Expected Ultimate Recovery") +  theme(plot.title=element_text(colour="orange"))
eur

multiplot(first_three, best_six, first_six, eur,cols=2)





df <- data.frame(Months   = 1:num_months,
                 Actual   = rep(NA, num_months),
                 Forecast = rep(NA, num_months))

df$Actual[1:10] = c(5000, 25000, 14000, 8000, 10000, 5200, 7500, 5600, 3500, 3000)
df$Actual[11:20] = c(4000, 27000, 11000, 8000, 10000, 5200, 7500, 5600, 3500, 3000)
df$Actual[21:30] = c(8000, 26000, 17000, 8000, 10000, 5200, 7500, 5600, 3500, 3000)
df$Actual[31:40] = c(7000, 33000, 13000, 8000, 10000, 5200, 7500, 5600, 3500, 3000)
df$Actual[41:48] = c(4000, 21000, 12000, 8000, 10000, 5200, 7500, 5600, 3500, 3000)

df <- melt(df, id="Months", measure=c("Actual", "Forecast"))
df <- rename(df, c(value="Production", variable="Type"))

base_plot <- ggplot(data=df, aes(x=Months, y=Production)) + 
  geom_bar(aes(alpha=Type), stat="identity") +
  scale_alpha_manual(values=c(1.0, 0.5))
theme_grey(base_size=20) +
  #  scale_y_log10(labels=comma, breaks=c(1000, 10000)) + 
  ylim(0, 30000)
base_plot

first_three <- base_plot + 
  geom_bar(data=df[df$Type=="Actual" & df$Months<=3,], stat="identity", fill="blue") + 
  ggtitle("First Three Months Production")
first_three

first_six <- base_plot + 
  geom_bar(data=df[df$Type=="Actual" & df$Months<=6,], stat="identity", fill="blue") + 
  ggtitle("First Six Months Production")
first_six

best_six_data <- copy(df)
best_six_data[best_six_data$Production<15000 & !is.na(best_six_data$Production),]$Production <- NA

best_six  <- base_plot +
  geom_bar(data=best_six_data, stat="identity", fill="blue") + 
  ggtitle("Best Six Months Production") + theme(plot.title=element_text(colour="blue"))
best_six

eur  <- base_plot +
  geom_bar(stat="identity", fill="blue") + 
  ggtitle("Best Six Months Production")
eur

library("MASS")
data(geyser, "MASS")

df = geyser[geyser$duration<2.5 & geyser$duration>1,]
df$RockPropertyAEstimate <- jitter(df$duration, factor=10)
df$RockPropertyBEstimate <- df$waiting + df$duration*50-40
m <- ggplot(df, aes(x = RockPropertyAEstimate, y = RockPropertyBEstimate)) +
  geom_point() +
  xlim(1.5, 2.5) +
  ylim(110, 170) + 
  xlab("Rock Property A Estimate") +
  ylab("Rock Property B Estimate") + 
  ggtitle("Latitude: 31.46\nLongitude: -102.19\nDepth: 8212ft")
m + geom_density2d()

EnergyLibrary("Charting")

importanceDF <- data.frame(FeatureDisplayName=c("Depth", "Latitude", "Longitude", "Proppant Amount", "Proppant Size", "Fluid Amount", "Fluid Type", "Cluster Spacing", "Lateral Length"), Importance=c(0.24, 0.17, 0.16, 0.09, 0.06, 0.05, 0.04, 0.01, 0.00))
importanceDF$Importance <- pmax(0, importanceDF$Importance)

ggplot(importanceDF) +
  geom_bar(
    mapping=aes(x=reorder(FeatureDisplayName,Importance, FUN=median), y=Importance),
    fill = "cornflowerblue",
    alpha = 0.8,
    stat = "identity"
  ) +
  theme(
    axis.title.y = element_blank(),
    axis.text.x = element_text(size=14),
    plot.title = element_text(size=15)
  ) +
  ggtitle("Permutation Variable Importance") +
  ylab("Relative Importance") + 
  coord_flip()




multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}