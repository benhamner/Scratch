library(ggplot2)
library(grid)
library(scales)

df <- data.frame(Object         = factor(c("Sharks   ","Sharks   ","Cars","Cars"), levels=c("Sharks   ", "Cars")),
                 Action         = factor(c("Scared Of","Killed By","Scared Of","Killed By"), levels=c("Scared Of", "Killed By")),
                 NumberOfPeople = c(313e6, 2, 2, 33500))

ggplot(data=df, aes(x=Action, y=NumberOfPeople, fill=Object)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    theme_grey(base_size=20) +
    scale_y_log10(labels=comma, breaks=c(1,1000,1e6,3e8)) +
    ylab("Number of People") +
    xlab("") +
    theme(legend.position="bottom",
          legend.direction="horizontal",
          plot.title = element_text(vjust=3),
          axis.text.x = element_text(color="#000000"),
          plot.margin = unit(c(1,1,1,1), "cm")) + 
    scale_fill_manual("", values = c("#004358","#FD7400","#00A388","#FF6138","#047C8C","#F1706D")) +
    ggtitle("Fear of Sharks and Cars in the US")

    ggsave("SharkCar.png", width=8, height=6, units="in")
    