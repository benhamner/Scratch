library(ggplot2)
library(scales)

df <- data.frame(Object         = factor(c("Sharks   ","Sharks   ","Cars","Cars"), levels=c("Sharks   ", "Cars")),
                 Action         = factor(c("Scared Of","Killed By","Scared Of","Killed By"), levels=c("Scared Of", "Killed By")),
                 NumberOfPeople = c(313e6, 2, 2, 33500))

ggplot(data=df, aes(x=Action, y=NumberOfPeople, fill=Object)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    theme_grey(base_size=20) +
    scale_y_log10(labels=comma) +
    ylab("Number of People") +
    xlab("") +
    theme(legend.position="bottom",
          legend.direction="horizontal",
          plot.margin = unit(c(1,1,1,1), "cm")) + 
    scale_fill_manual("", values = c("#004358","#FD7400","#00A388","#FF6138","#047C8C","#F1706D")) +
    ggtitle("Fear of Sharks and Cars in the US")