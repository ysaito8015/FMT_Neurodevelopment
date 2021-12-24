rm(list = ls())
neurodevelopmentData <- read.table("../data/neurodevelopmentData.dat", header = TRUE)
print(head(neurodevelopmentData))

# check repeated measure times
print(with(neurodevelopmentData, table(id,PND)))
#     PND
# id   1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21
#   0  1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1
#   1  1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1
#   2  1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1
#   3  1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1
#   4  1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1
#   5  1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1
#   6  1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1
#   7  1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1
#   8  1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1
#   9  1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1
#   10 1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1


# facterize
neurodevelopmentData$PND <- as.factor(neurodevelopmentData$PND)
neurodevelopmentData$Forelimb_grasp <- as.factor(neurodevelopmentData$Forelimb_grasp)
neurodevelopmentData$Forelimb_placing <- as.factor(neurodevelopmentData$Forelimb_placing)
neurodevelopmentData$Auditory_startle <- as.factor(neurodevelopmentData$Auditory_startle)
neurodevelopmentData$Tactile_startle <- as.factor(neurodevelopmentData$Tactile_startle)
neurodevelopmentData$Air_righting <- as.factor(neurodevelopmentData$Air_righting)
neurodevelopmentData$Ears_unfolding <- as.factor(neurodevelopmentData$Ears_unfolding)
neurodevelopmentData$Ears_twitch <- as.factor(neurodevelopmentData$Ears_twitch)
neurodevelopmentData$Gait_b <- as.factor(neurodevelopmentData$Gait_b)
neurodevelopmentData$Walk_b <- as.factor(neurodevelopmentData$Walk_b)
neurodevelopmentData$Eyes_opened_left <- as.factor(neurodevelopmentData$Eyes_opened_left)
neurodevelopmentData$Eyes_opened_right <- as.factor(neurodevelopmentData$Eyes_opened_right)
neurodevelopmentData$INcisor_eruption <- as.factor(neurodevelopmentData$Incisor_eruption)


# box plots
png(file = "../plots/boxplot-Gait.png")
par(las = 1,
    family = "sans",
    lwd = 2,
    cex = 1)
boxplot(Gait ~ PND,
        neurodevelopmentData,
        ylab = "Gait (sec)")
dev.off()

png(file = "../plots/boxplot-Walk.png")
par(las = 1,
    family = "sans",
    lwd = 2,
    cex = 1)
boxplot(Walk ~ PND,
        neurodevelopmentData,
        ylab = "Walk (sec)")
dev.off()

png(file = "../plots/boxplot-Surface.png")
par(las = 1,
    family = "sans",
    lwd = 2,
    cex = 1)
boxplot(Surface_T_.s. ~ PND,
        neurodevelopmentData,
        ylab = "Surface T (sec)")
dev.off()

png(file = "../plots/boxplot-NG_turn.png")
par(las = 1,
    family = "sans",
    lwd = 2,
    cex = 1)
boxplot(NG_turn ~ PND,
        neurodevelopmentData,
        ylab = "NG turn (sec)")
dev.off()

png(file = "../plots/boxplot-NG_top.png")
par(las = 1,
    family = "sans",
    lwd = 2,
    cex = 1)
boxplot(NG_top ~ PND,
        neurodevelopmentData,
        ylab = "NG top (sec)")
dev.off()

png(file = "../plots/boxplot-Wire.png")
par(las = 1,
    family = "sans",
    lwd = 2,
    cex = 1)
boxplot(Wire ~ PND,
        neurodevelopmentData,
        ylab = "Wire (sec)")
dev.off()

png(file = "../plots/boxplot-Weight.png")
par(las = 1,
    family = "sans",
    lwd = 2,
    cex = 1)
boxplot(Weight_.g.~ PND,
        neurodevelopmentData,
        ylab = "Weight (grams)")
dev.off()

# ANOVA
library(car)
neurodevelopment.lm.m <- lm(Gait ~ PND, neurodevelopmentData)
print(Anova(neurodevelopment.lm.m))
# Anova Table (Type II tests)
# 
# Response: Gait
#           Sum Sq  Df F value    Pr(>F)    
# PND       123415  20  33.849 < 2.2e-16 ***
# Residuals  38283 210                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

cat("\n")
neurodevelopment.lm.m <- lm(Walk ~ PND, neurodevelopmentData)
print(Anova(neurodevelopment.lm.m))
# Anova Table (Type II tests)
# 
# Response: Walk
#           Sum Sq  Df F value    Pr(>F)    
# PND       111196  20  30.617 < 2.2e-16 ***
# Residuals  38134 210                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

cat("\n")
neurodevelopment.lm.m <- lm(Surface_T_.s. ~ PND, neurodevelopmentData)
print(Anova(neurodevelopment.lm.m))
# Anova Table (Type II tests)
# 
# Response: Surface_T_.s.
#           Sum Sq  Df F value    Pr(>F)    
# PND        76016  20  52.069 < 2.2e-16 ***
# Residuals  15329 210                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# Plot individual data
par(las = 1,
    family="sans",
    lwd = 2,
    cex = 1)

library(dplyr)
library(tidyr)

plotData =
    neurodevelopmentData %>%
    select(id, Gait, PND) %>%
    spread(key = id, value=Gait) %>%
    select(-PND) %>%
    as.matrix(.)

print(plotData)

png(file = "../plots/plot-Gait.png")
matplot(plotData,
        type = "b",
        lty = 1,
        xlab = "PND",
        ylab = "Gait")
dev.off()

plotData =
    neurodevelopmentData %>%
    select(id, Weight_.g., PND) %>%
    spread(key = id, value=Weight_.g.) %>%
    select(-PND) %>%
    as.matrix(.)

print(plotData)

png(file = "../plots/plot-Weight.png")
matplot(plotData,
        type = "b",
        lty = 1,
        xlab = "PND",
        ylab = "Weight")
dev.off()


# Repeated measures ANOVA
library(lme4)

neuro.lmer.m <- lmer(Weight_.g. ~ PND + (1|id), neurodevelopmentData)
print(Anova(neuro.lmer.m))

#Analysis of Deviance Table (Type II Wald chisquare tests)
#
#Response: Weight_.g.
#    Chisq Df Pr(>Chisq)    
#PND  4800 20  < 2.2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
