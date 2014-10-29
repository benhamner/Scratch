library(ggplot2)
library(grid)

df <- data.frame(Year                = c(2010, 2011, 2012, 2013,   2014),
                 ClassificationError = c(0.28,0.257,0.153,0.11197, 0.06656))

ggplot(data=df, aes(x=Year, y=ClassificationError)) + 
    geom_line(size=2, color="#157FAB") +
    geom_point(size=4) +
    ylim(0.0,0.3) + 
    theme_light(base_size=24) +
    theme(plot.margin = unit(c(1,1,1,1), "cm")) +
    ylab("Classification Error") +
    xlab("Year") +
    ggtitle("ImageNet Object Classification Performance Over Time") + 
    theme(plot.title=element_text(vjust=2)) + 
    theme(axis.title.x=element_text(vjust=-1)) +
    theme(axis.title.y=element_text(vjust=1)) +
    geom_line(data=data.frame(Year=c(2010,2014), ClassificationError=c(0,0)), size=1, linetype=2) +
    geom_text(data=data.frame(Year=c(2012), ClassificationError=c(0.02)), label="Perfect", size=6)

ggsave("ImageNetObjectClassification.png", width=8, height=6, units="in")
    