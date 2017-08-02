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
g + geom_bar(stat = "identity") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x = "Event Type", y = "Total Fatalities and Injuries")

#Across the United States, which types of events have the greatest economic consequences?
df$prop_u <- ifelse(df$PROPDMGEXP == "K", df$PROPDMG * 1000, ifelse(df$PROPDMGEXP == "M", df$PROPDMG * 1000000,df$PROPDMG))
df$crop_u <- ifelse(df$CROPDMGEXP == "K", df$CROPDMG * 1000, ifelse(df$CROPDMGEXP == "M", df$CROPDMG * 1000000,df$CROPDMG))
df$tot_dmg <- df$prop_u + df$crop_u

summaryd <- df %>% 
        group_by(EVTYPE) %>% 
        summarize(total_damage = sum(tot_dmg))
summaryd <- summaryd[order(-summaryd$total_damage),]
summaryd2 <- summaryd[1:30,]
g <- ggplot(summaryd2, aes(EVTYPE, total_damage))
g + geom_bar(stat = "identity") + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(x = "Event Type", y = "Total Damages")

##use below if knitr does not work
#knit2html(spin("StormData.Rmd",knit = FALSE), force_v1 = TRUE)
