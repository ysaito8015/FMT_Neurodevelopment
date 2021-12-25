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


# real-valued data
numericData <- c("Gait", "Walk", "Surface_T_s",
                 "NG_turn", "NG_top", "Wire",
                 "Weight_g")


# facterize
factorialData <- c("PND", "Forelimb_grasp", "Forelimb_placing", "Auditory_startle",
                   "Tactile_startle", "Air_righting", "Ears_unfolding", "Ears_twitch",
                   "Gait_b", "Walk_b", "Eyes_opened_left", "Eyes_opened_right",
                   "Incisor_eruption")

# for (i in 1:length(factorialData)) {
#     colName <- factorialData[i]
#     assign(colName,
#            as.factor(eval(parse(text = colName))),
#            env = .GlobalEnv)
# }

# this for loop executes as same things as below

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


# box plots for numericData
for (i in 1:length(numericData)) {
    colNamePng <- paste("../plots/boxplot-", numericData[i], ".png", sep = "")
    png(file = colNamePng)
    par(las = 1,
        family = "sans",
        lwd = 2,
        cex = 1)
    boxplot(eval(parse(text = numericData[i])) ~ PND,
            #neurodevelopmentData,
            ylab = numericData[i])
    dev.off()
}



# one-way ANOVA, ignore the individual difference
library(car)

for (i in 1:length(numericData)) {
    cat("-------")
    cat("\n")
    expName <- parse(text = numericData[i])
    neurodevelopment.lm.m <- lm(eval(expName) ~ PND, neurodevelopmentData)
    print(Anova(neurodevelopment.lm.m))
    cat("\n")
    cat("-------")
}


# -------
# Anova Table (Type II tests)
# 
# Response: Gait
#           Sum Sq  Df F value    Pr(>F)    
# PND       123415  20  33.849 < 2.2e-16 ***
# Residuals  38283 210                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# --------------
# Anova Table (Type II tests)
# 
# Response: Walk
#           Sum Sq  Df F value    Pr(>F)    
# PND       111196  20  30.617 < 2.2e-16 ***
# Residuals  38134 210                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# --------------
# Anova Table (Type II tests)
# 
# Response: Surface_T
#           Sum Sq  Df F value    Pr(>F)    
# PND        76016  20  52.069 < 2.2e-16 ***
# Residuals  15329 210                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# --------------
# Anova Table (Type II tests)
# 
# Response: NG_turn
#           Sum Sq  Df F value    Pr(>F)    
# PND        74855  20  8.6897 < 2.2e-16 ***
# Residuals  90450 210                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# --------------
# Anova Table (Type II tests)
# 
# Response: NG_top
#           Sum Sq  Df F value    Pr(>F)    
# PND        13560  20  2.9076 6.928e-05 ***
# Residuals  48968 210                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# --------------
# Anova Table (Type II tests)
# 
# Response: Wire
#           Sum Sq  Df F value    Pr(>F)    
# PND       115292  20  52.724 < 2.2e-16 ***
# Residuals  22961 210                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# --------------
# Anova Table (Type II tests)
# 
# Response: Weight_g
#           Sum Sq  Df F value    Pr(>F)    
# PND       978.41  20  59.787 < 2.2e-16 ***
# Residuals 171.83 210                      
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# 
# Plot individual data
par(las = 1,
    family="sans",
    lwd = 2,
    cex = 1)

library(dplyr)
library(tidyr)

for (i in 1:length(numericData)) {
    colName <- numericData[i]
    expName <- parse(text = paste("neurodevelopmentData$", colName, sep = ""))
    saveName <- paste("../plots/repeated-measures-plot-", colName, ".png", sep = "")
    df = data.frame(neurodevelopmentData$id, neurodevelopmentData$PND, eval(expName))
    names(df)[1] <- "id"
    names(df)[3] <- colName
    plotData =
        df %>%
        pivot_wider(names_from = "id",
                    values_from = colName)
    plotData <- plotData[c(2:11)]

    png(file = saveName)
    matplot(plotData,
            type = "b",
            lty = 1,
            xlab = "PND",
            ylab = colName)
    dev.off()
}

# Repeated measures ANOVA
library(lme4)
library(emmeans)

for (i in 1:length(numericData)) {
    cat("-------")
    cat("\n")
    expName <- parse(text = numericData[i])
    neuro.lmer.m <- lmer(eval(expName) ~ PND + (1|id), neurodevelopmentData)
    print(Anova(neuro.lmer.m))
    cat("\n")
    print(emmeans(neuro.lmer.m, specs = "PND"))
    cat("-------")
}


# --------------
# boundary (singular) fit: see ?isSingular
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Gait
#      Chisq Df Pr(>Chisq)    
# PND 676.99 20  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#  PND emmean   SE  df lower.CL upper.CL
#  1    60.00 4.07 210  51.9748     68.0
#  2    60.00 4.07 210  51.9748     68.0
#  3    58.20 4.07 210  50.1766     66.2
#  4    59.19 4.07 210  51.1684     67.2
#  5    56.43 4.07 210  48.4021     64.5
#  6    57.46 4.07 210  49.4330     65.5
#  7    52.53 4.07 210  44.5039     60.6
#  8    55.92 4.07 210  47.8912     63.9
#  9    60.00 4.07 210  51.9748     68.0
#  10   24.82 4.07 210  16.7975     32.8
#  11    5.95 4.07 210  -2.0716     14.0
#  12    8.05 4.07 210   0.0202     16.1
#  13    6.51 4.07 210  -1.5198     14.5
#  14    5.82 4.07 210  -2.2070     13.8
#  15    2.70 4.07 210  -5.3252     10.7
#  16    2.47 4.07 210  -5.5543     10.5
#  17   13.82 4.07 210   5.7939     21.8
#  18   19.46 4.07 210  11.4375     27.5
#  19   14.07 4.07 210   6.0448     22.1
#  20   27.17 4.07 210  19.1448     35.2
#  21   29.88 4.07 210  21.8539     37.9
# 
# Degrees-of-freedom method: kenward-roger 
# Results are given on the eval (not the response) scale. 
# Confidence level used: 0.95 
# --------------
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Walk
#      Chisq Df Pr(>Chisq)    
# PND 613.57 20  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#  PND emmean   SE  df lower.CL upper.CL
#  1    60.00 4.06 210   51.990     68.0
#  2    60.00 4.06 210   51.990     68.0
#  3    58.41 4.06 210   50.405     66.4
#  4    60.00 4.06 210   51.990     68.0
#  5    60.00 4.06 210   51.990     68.0
#  6    60.00 4.06 210   51.990     68.0
#  7    55.61 4.06 210   47.599     63.6
#  8    57.28 4.06 210   49.269     65.3
#  9    60.00 4.06 210   51.990     68.0
#  10   41.06 4.06 210   33.053     49.1
#  11   11.45 4.06 210    3.444     19.5
#  12   17.90 4.06 210    9.891     25.9
#  13    9.91 4.06 210    1.905     17.9
#  14    8.33 4.06 210    0.318     16.3
#  15    5.35 4.06 210   -2.657     13.4
#  16    5.34 4.06 210   -2.670     13.3
#  17   15.06 4.06 210    7.050     23.1
#  18   21.60 4.06 210   13.594     29.6
#  19   17.26 4.06 210    9.248     25.3
#  20   39.08 4.06 210   31.075     47.1
#  21   30.97 4.06 210   22.961     39.0
# 
# Degrees-of-freedom method: kenward-roger 
# Results are given on the eval (not the response) scale. 
# Confidence level used: 0.95 
# --------------
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Surface_T
#      Chisq Df Pr(>Chisq)    
# PND 1060.6 20  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#  PND emmean   SE  df lower.CL upper.CL
#  1   60.000 2.58 209    54.92    65.08
#  2   55.909 2.58 209    50.83    60.99
#  3   32.698 2.58 209    27.62    37.78
#  4   29.528 2.58 209    24.45    34.61
#  5    3.035 2.58 209    -2.04     8.11
#  6    2.506 2.58 209    -2.57     7.58
#  7    1.341 2.58 209    -3.74     6.42
#  8    1.178 2.58 209    -3.90     6.26
#  9    1.605 2.58 209    -3.47     6.68
#  10   0.883 2.58 209    -4.20     5.96
#  11   0.675 2.58 209    -4.40     5.75
#  12   0.642 2.58 209    -4.44     5.72
#  13   0.489 2.58 209    -4.59     5.57
#  14   0.402 2.58 209    -4.68     5.48
#  15   0.325 2.58 209    -4.75     5.40
#  16   0.321 2.58 209    -4.76     5.40
#  17   0.535 2.58 209    -4.54     5.61
#  18   0.561 2.58 209    -4.52     5.64
#  19   0.296 2.58 209    -4.78     5.37
#  20   0.214 2.58 209    -4.86     5.29
#  21   0.230 2.58 209    -4.85     5.31
# 
# Degrees-of-freedom method: kenward-roger 
# Results are given on the eval (not the response) scale. 
# Confidence level used: 0.95 
# --------------
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: NG_turn
#      Chisq Df Pr(>Chisq)    
# PND 194.21 20  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#  PND emmean   SE  df lower.CL upper.CL
#  1    60.00 6.26 172    47.65     72.4
#  2    60.00 6.26 172    47.65     72.4
#  3    60.00 6.26 172    47.65     72.4
#  4    60.00 6.26 172    47.65     72.4
#  5    42.36 6.26 172    30.01     54.7
#  6    17.04 6.26 172     4.69     29.4
#  7    36.55 6.26 172    24.20     48.9
#  8    25.08 6.26 172    12.73     37.4
#  9    19.77 6.26 172     7.42     32.1
#  10   14.75 6.26 172     2.40     27.1
#  11    9.76 6.26 172    -2.59     22.1
#  12    4.86 6.26 172    -7.49     17.2
#  13    4.49 6.26 172    -7.86     16.8
#  14    4.09 6.26 172    -8.26     16.4
#  15   25.66 6.26 172    13.31     38.0
#  16   29.77 6.26 172    17.42     42.1
#  17   25.25 6.26 172    12.90     37.6
#  18   29.80 6.26 172    17.45     42.2
#  19   31.58 6.26 172    19.23     43.9
#  20   35.47 6.26 172    23.12     47.8
#  21   36.56 6.26 172    24.21     48.9
# 
# Degrees-of-freedom method: kenward-roger 
# Results are given on the eval (not the response) scale. 
# Confidence level used: 0.95 
# --------------
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: NG_top
#      Chisq Df Pr(>Chisq)    
# PND 62.348 20  3.066e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#  PND emmean  SE  df lower.CL upper.CL
#  1     60.0 4.6 193     50.9     69.1
#  2     60.0 4.6 193     50.9     69.1
#  3     60.0 4.6 193     50.9     69.1
#  4     60.0 4.6 193     50.9     69.1
#  5     60.0 4.6 193     50.9     69.1
#  6     60.0 4.6 193     50.9     69.1
#  7     60.0 4.6 193     50.9     69.1
#  8     60.0 4.6 193     50.9     69.1
#  9     55.7 4.6 193     46.6     64.8
#  10    49.2 4.6 193     40.1     58.3
#  11    46.2 4.6 193     37.1     55.2
#  12    52.5 4.6 193     43.4     61.6
#  13    36.0 4.6 193     26.9     45.1
#  14    37.4 4.6 193     28.3     46.5
#  15    43.8 4.6 193     34.8     52.9
#  16    45.5 4.6 193     36.5     54.6
#  17    46.6 4.6 193     37.5     55.7
#  18    60.0 4.6 193     50.9     69.1
#  19    52.6 4.6 193     43.6     61.7
#  20    55.4 4.6 193     46.3     64.5
#  21    55.2 4.6 193     46.1     64.3
# 
# Degrees-of-freedom method: kenward-roger 
# Results are given on the eval (not the response) scale. 
# Confidence level used: 0.95 
# --------------
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Wire
#      Chisq Df Pr(>Chisq)    
# PND 1176.9 20  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#  PND emmean   SE  df lower.CL upper.CL
#  1    60.00 3.15 173   53.777    66.22
#  2    60.00 3.15 173   53.777    66.22
#  3    60.00 3.15 173   53.777    66.22
#  4    60.00 3.15 173   53.777    66.22
#  5    60.00 3.15 173   53.777    66.22
#  6    60.00 3.15 173   53.777    66.22
#  7     4.52 3.15 173   -1.707    10.74
#  8     2.56 3.15 173   -3.664     8.78
#  9     6.74 3.15 173    0.515    12.96
#  10    6.49 3.15 173    0.267    12.71
#  11    6.64 3.15 173    0.414    12.86
#  12    6.57 3.15 173    0.348    12.79
#  13    7.34 3.15 173    1.115    13.56
#  14   10.63 3.15 173    4.403    16.85
#  15    9.62 3.15 173    3.402    15.85
#  16   17.59 3.15 173   11.363    23.81
#  17   20.43 3.15 173   14.203    26.65
#  18   28.20 3.15 173   21.977    34.42
#  19   28.13 3.15 173   21.903    34.35
#  20   29.39 3.15 173   23.164    35.61
#  21   36.70 3.15 173   30.482    42.93
# 
# Degrees-of-freedom method: kenward-roger 
# Results are given on the eval (not the response) scale. 
# Confidence level used: 0.95 
# --------------
# Analysis of Deviance Table (Type II Wald chisquare tests)
# 
# Response: Weight_G
#     Chisq Df Pr(>Chisq)    
# PND  4800 20  < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#  PND emmean    SE   df lower.CL upper.CL
#  1    0.252 0.273 17.1   -0.323    0.827
#  2    0.276 0.273 17.1   -0.299    0.852
#  3    2.478 0.273 17.1    1.903    3.053
#  4    2.985 0.273 17.1    2.410    3.561
#  5    3.424 0.273 17.1    2.849    3.999
#  6    3.904 0.273 17.1    3.329    4.479
#  7    4.417 0.273 17.1    3.842    4.993
#  8    4.860 0.273 17.1    4.285    5.435
#  9    5.336 0.273 17.1    4.761    5.911
#  10   5.799 0.273 17.1    5.224    6.374
#  11   6.037 0.273 17.1    5.462    6.613
#  12   6.335 0.273 17.1    5.760    6.910
#  13   6.653 0.273 17.1    6.078    7.228
#  14   6.781 0.273 17.1    6.206    7.357
#  15   6.774 0.273 17.1    6.199    7.350
#  16   6.604 0.273 17.1    6.029    7.179
#  17   6.594 0.273 17.1    6.019    7.169
#  18   6.576 0.273 17.1    6.001    7.151
#  19   6.621 0.273 17.1    6.046    7.196
#  20   6.727 0.273 17.1    6.151    7.302
#  21   6.961 0.273 17.1    6.386    7.536
# 
# Degrees-of-freedom method: kenward-roger 
# Results are given on the eval (not the response) scale. 
# Confidence level used: 0.95 
