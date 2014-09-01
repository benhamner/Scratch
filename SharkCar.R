library(ggplot2)
library(scales)

df <- data.frame(Object         = factor(c("Sharks","Sharks","Cars","Cars"), levels=c("Sharks", "Cars")),
                 Action         = factor(c("Scared Of","Killed By","Scared Of","Killed By"), levels=c("Scared Of", "Killed By")),
                 NumberOfPeople = c(313e6, 2, 1, 33500))

ggplot(data=df, aes(x=Action, y=NumberOfPeople, fill=Object)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    theme_grey(base_size=18) +
    scale_y_log10(labels=comma) +
    ylab("Number of People") +
    xlab("") +
    theme(legend.position="bottom", legend.direction="horizontal") +
    scale_fill_discrete("")
