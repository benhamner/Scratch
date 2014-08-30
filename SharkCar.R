df <- data.frame(Object         = factor(c("Sharks","Sharks","Cars","Cars"), levels=c("Sharks", "Cars")),
                 Action         = factor(c("Scared Of","Killed By","Scared Of","Killed By"), levels=c("Scared Of", "Killed By")),
                 NumberOfPeople = c(6e9, 1, 1, 1e6))

ggplot(data=df, aes(x=Action, y=NumberOfPeople, fill=Object)) + 
    geom_bar(stat="identity", position=position_dodge()) +
    scale_y_log10()