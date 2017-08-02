setwd("C:/Users/S.Sagara/Documents/Data Science/coursera/R specialization/Reproducible Data/week4")

library(data.table)
library(dplyr)
library(ggplot2)

df <- fread("StormData.csv")

#Questions
#Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
df$dth_inj <- df$FATALITIES + df$INJURIES

summary1 <- df %>% 
        group_by(EVTYPE) %>% 
        summarize(total_dth_inj = sum(dth_inj))

summary1 <- summary1[order(-summary1$total_dth_inj),]
summary2 <- summary1[1:30,]

g <- ggplot(summary2, aes(EVTYPE, total_dth_inj))
g + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Across the United States, which types of events have the greatest economic consequences?