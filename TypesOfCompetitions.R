library(ggplot2)
library(grid)
library(scales)

df <- data.frame(CompetitionType = c("Binary\nClassification", "Multiclass\nClassification ", "Optimization ", "Other", "Ranking", "Regression"),
                 Count           = c(40, 24, 3, 8, 10, 38))
                 
ggplot(data=df, aes(x=CompetitionType, y=Count)) + 
  theme_light(base_size=24) +
    geom_bar(stat="identity", fill="#157FAB") +
    theme(plot.margin = unit(c(1,1,1,1), "cm")) +
    ylab("Number of Competitions") +
    xlab("") +
    ggtitle("ImageNet Object Classification Performance Over Time") + 
    theme(plot.title=element_text(vjust=2)) + 
    theme(axis.title.y=element_text(vjust=1)) +
    ggtitle("Types of Public Competitions")

    ggsave("CompetitionTypes.png", width=12, height=8, units="in")
