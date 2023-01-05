library(tidyverse)
library(emmeans)
library(janitor)

goby <- read_csv("behavior_data.csv") 
glimpse(goby)
summary(goby) # reads in with several blank rows; remove them (done on line 15 below)

# names(goby)
# str(goby)
# dim(goby)

## Choose to convert temp to be a factor rather than numeric (could choose otherwise)

goby <- goby %>% mutate(temp = factor(temp)) %>% remove_empty("rows")

## Can split data to separate LOE and avoidance experiments, and 
## fit two separate one-way ANOVA models
## (Code follows later that does not split data and instead fits two-way ANOVA model)

## LOE data, boxplot, summary statistics, and one-way ANOVA model with post-hoc

LOE <- goby %>% filter(metric=="LOE")

ggplot(data=LOE, aes(x=temp, group=temp, y=CO2_onecurve)) + geom_boxplot()

LOE %>% group_by(temp) %>% summarize(mean_CO2 = mean(CO2_onecurve), 
                                     sd_CO2 = sd(CO2_onecurve), 
                                     n = n())

mod_LOE <- aov(CO2_onecurve ~ temp, data = LOE)
summary(mod_LOE)
TukeyHSD(mod_LOE) # Temp 5 is significantly lower than temp 15 and 25

## Avoidance data, boxplot, summary statistics, and one-way ANOVA model with post-hoc

# Should note that one missing value in avoidance data on CO2 variable
# Document this missing value/exclusion at temp=5

avoid <- goby %>% filter(metric=="avoidance") %>% na.omit()

ggplot(data=avoid, aes(x=temp, group=temp, y=CO2_onecurve)) + geom_boxplot()

avoid %>% group_by(temp) %>% summarize(mean_CO2 = mean(CO2_onecurve), 
                                       sd_CO2 = sd(CO2_onecurve), 
                                       n = n())

mod_avoid <- aov(CO2_onecurve ~ temp, data = avoid)
summary(mod_avoid)
TukeyHSD(mod_avoid) # temp 5 is significantly lower than temp 25


## Here we keep LOE and avoidance data together and fit a two-factor ANOVA model.
## Include interaction term of temp by metric

mod_two = aov(CO2_onecurve ~ temp * metric, data = goby)
summary(mod_two)
emmeans(mod_two, pairwise ~ temp | metric) # post-hoc comparisons
plot(mod_two) # diagnostic plots

## Can also look into other metric included in data set; peripheral to central research question

# Scatterplot relating pH and CO2
ggplot(aes(x = pH, y = CO2_onecurve), data = goby) + geom_point()

# Fish length and weight data also available through USGS data, but in separate .csv file 
# (morphometrics.csv). 
