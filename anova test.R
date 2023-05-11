library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(car)
library(rstatix)
library(datarium)

# import data
setwd("C:/Users/clyho/OneDrive - Aalborg Universitet/Desktop/Shid/Databehandling")
dataFrame <- read.csv("Datasheet_last_rows.csv", header = TRUE, stringsAsFactors = TRUE)

# Analysis of glance time pr condition (normal, homogeneity)
glanceTimeData <- subset(dataFrame, select = c("Participants", "Condition", "TotalGlanceTime"))
summary(glanceTimeData)
get_summary_stats(glanceTimeData)

glanceTimeData %>%
    group_by(Condition) %>%
    identify_outliers(TotalGlanceTime)

glanceTimeModel <- lm(TotalGlanceTime ~ Condition, data = glanceTimeData)
ggqqplot(residuals(glanceTimeModel))
shapiro_test(residuals(glanceTimeModel))

bartlett.test(TotalGlanceTime ~ Condition, data = glanceTimeData)

glanceTimeTest <- glanceTimeData %>% anova_test(TotalGlanceTime ~ Condition)
glanceTimeTest

# Analysis of glance count pr condition(normal, homogeneity)
glanceCountData <- subset(dataFrame, select = c("Participants", "Condition", "TotalGlances"))
summary(glanceCountData)
get_summary_stats(glanceCountData)

glanceCountData %>%
    group_by(Condition) %>%
    identify_outliers(TotalGlances)

glanceCountModel <- lm(TotalGlances ~ Condition, data = glanceCountData)
ggqqplot(residuals(glanceCountModel))
shapiro_test(residuals(glanceCountModel))

bartlett.test(TotalGlances ~ Condition, data = glanceCountData)

glanceCountTest <- glanceCountData %>% anova_test(TotalGlances ~ Condition)
glanceCountTest

glanceCountPost <- glanceCountData %>% tukey_hsd(TotalGlances ~ Condition)
glanceCountPost

glanceCountPost <- glanceCountPost %>% add_xy_position(x = "Condition")
ggboxplot(glanceCountData, x = "Condition", y = "TotalGlances") +
    stat_pvalue_manual(glanceCountPost, hide.ns = TRUE) +
    labs(
        subtitle = get_test_label(glanceCountTest, detailed = TRUE),
        caption = get_pwc_label(glanceCountPost)
    )

# Analysis of crashes pr condition (not normal, homogeneity)
crashData <- subset(dataFrame, select = c("Participants", "Condition", "TotalCrash"))
summary(crashData)
get_summary_stats(crashData)

crashData %>%
    group_by(Condition) %>%
    identify_outliers(TotalCrash)

crashDataModel <- lm(TotalCrash ~ Condition, data = crashData)
ggqqplot(residuals(crashDataModel))
shapiro_test(residuals(crashDataModel))

crashData %>% levene_test(TotalCrash ~ Condition)

crashDataTest <- crashData %>% kruskal_test(TotalCrash ~ Condition)
crashDataTest

# Analysis of lane breaks pr condition (not normal, homogeneity)
laneData <- subset(dataFrame, select = c("Participants", "Condition", "LaneBreaks"))
summary(laneData)
get_summary_stats(laneData)

laneData %>%
    group_by(Condition) %>%
    identify_outliers(LaneBreaks)

laneDataModel <- lm(LaneBreaks ~ Condition, data = laneData)
ggqqplot(residuals(laneDataModel))
shapiro_test(residuals(laneDataModel))

laneData %>% levene_test(LaneBreaks ~ Condition)

laneDataTest <- laneData %>% kruskal_test(LaneBreaks ~ Condition)
laneDataTest

# Analysis of SUS pr condition (not normal, homogeneity)
susData <- subset(dataFrame, select = c("Participants", "Condition", "SUS"))
summary(susData)
get_summary_stats(susData)

susData %>%
    group_by(Condition) %>%
    identify_outliers(SUS)

susDataModel <- lm(SUS ~ Condition, data = susData)
ggqqplot(residuals(susDataModel))
shapiro_test(residuals(susDataModel))

susData %>% levene_test(SUS ~ Condition)

susDataTest <- susData %>% kruskal_test(SUS ~ Condition)
susDataTest

# Analysis of SART pr condition (normal, homogeneity )
sartData <- subset(dataFrame, select = c("Participants", "Condition", "SART"))
sartData <- sartData[!(sartData$Participants == "P12" | sartData$Participants == "P13" | sartData$Participants == "P29" | sartData$Participants == "P30"), ]
summary(sartData)
get_summary_stats(sartData)

sartData %>%
    group_by(Condition) %>%
    identify_outliers(SART)

sartDataModel <- lm(SART ~ Condition, data = sartData)
ggqqplot(residuals(sartDataModel))
shapiro_test(residuals(sartDataModel))

bartlett.test(SART ~ Condition, data = sartData)

sartDataTest <- sartData %>% anova_test(SART ~ Condition)
sartDataTest
