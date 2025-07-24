
# Final Project Eq Ag 2025
# Joss, Brooks, Rowan

# LOAD IN DATA
library(arrow)
library(magrittr)
library(dplyr)
library(tidyverse)
library(ggplot)
library(ggplot2)
install.packages("ggeffects")
library(ggeffects)
# Import data (calenviroscreen, hispanic (B03002) immigration data (R...292) and insurance coverage (R...324))

# Create datasets and filtering out what we want
insurance <- R13894324_SL140
migration <- R13894292_SL140
demos <- ACSDT5Y2021_B03002_Data
CalScreen <- data
CPP <- CropAndPasturePct


# create our total number of uninsured women 7-55 for each tract
insurance$UninsuredWomen <- insurance$ACS21_5yr_B27001036 + insurance$ACS21_5yr_B27001039 + insurance$ACS21_5yr_B27001042 + insurance$ACS21_5yr_B27001045 + insurance$ACS21_5yr_B27001048

# Filtering the enviroscreen so its just what we want
CalFilter <- CalScreen[, c("Census Tract", "Total Population", "California County", "ZIP", "Pesticides", "Asthma", "Low Birth Weight", "Education")]
CalFilter$Geo_Id <- CalFilter$`Census Tract`

# Fixing formatting of geoid
demos$Geo_Id <- substr(demos$GEO_ID, 11, 21)

# Filtering the demo so its just total number of hispanic or latino people by tract
demos$LatinoPop <- demos$B03002_012E
LatinoPop <- demos[, c("Geo_Id", "LatinoPop")]

# Repeat steps for immigration the big thing is just cleaning the variable names
migration$Geo_Id <- substr(migration$Geo__geoid_, 2, 11)
migration$MigrantPop <- migration$ACS21_5yr_B05002026
ImmigrantPop <- migration[, c("Geo_Id", "MigrantPop")]

# Same for women
insurance$Geo_Id <- substr(insurance$Geo__geoid_, 2, 11)
JW <- insurance[, c("Geo_Id", "UninsuredWomen")]

# And for crop pasture
CPP$Geo_Id <- substr(CPP$GEOID, 2, 11)
CropPasturePct <- CPP[, c("Geo_Id", "CropPasturePct", "LandSqKm")]

# Mergin tables by geo id
m0 <- merge(CalFilter, CropPasturePct, by = "Geo_Id")
m1 <- merge(m0, LatinoPop, by = "Geo_Id")
m2 <- merge(m1, ImmigrantPop, by = "Geo_Id")
m3 <- merge(m2, JW, by = "Geo_Id")

# Making a percent clip
m3$UninsuredPct <- 100*(m3$UninsuredWomen / m3$`Total Population`)

# For whatever reason latino pop was a boolean value so I fixed that
m3$LatinoPop <- as.numeric(m3$LatinoPop)
m3$LatinoPct <- 100*(m3$LatinoPop / m3$`Total Population`)
m3$MigrantPct <- 100*(m3$MigrantPop / m3$`Total Population`)
m3$PopulationDensity <- as.numeric(m3$`Total Population`/m3$LandSqKm)

# Just cleaning up by filtering out base population data
RegTable <- m3[, -c(11, 12, 13)]

# Checking correlation in migrant and hispanic % tables
cor.test(RegTable$LatinoPct, RegTable$MigrantPct)

# Visualizing the correlation
ML <- ggplot(RegTable, aes(x = LatinoPct, y = MigrantPct)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatterplot of LatinoPct vs MigrantPct") +
  theme_minimal()

# What about population density vs crop pasture percent
cor.test(RegTable$LatinoPct, RegTable$MigrantPct)
RegTable$CropPasturePct <- as.numeric(RegTable$CropPasturePct)
cor.test((RegTable$PopulationDensity), (RegTable$CropPasturePct))

ML <- ggplot(RegTable, aes(x = PopulationDensity, y = CropPasturePct)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatterplot of LatinoPct vs MigrantPct") +
  theme_minimal()
# --------------------------------------------------
# LINEAR REGRESSION
# --------------------------------------------------
summary(RegTable[,c("Low Birth Weight", "LatinoPct", "UninsuredPct", "CropPasturePct", "Pesticides")])
# --------------------------------------------------
# ANNA ATTEMPT
# --------------------------------------------------
subset_table_data <- RegTable[,c("Low Birth Weight", "LatinoPct", "UninsuredPct", "CropPasturePct", "Pesticides")]
summary(subset_table_data)
# -------------------------------------------------- 
plot(x = RegTable$Pesticides, y = RegTable$`Low Birth Weight`)
# --------------------------------------------------
# LOG-TRANSFORMED DATA
# --------------------------------------------------
RegTable$logpop <- log10(RegTable$PopulationDensity)
RegTable$asinh <- asinh(RegTable$PopulationDensity)
# --------------------------------------------------
# ANOVA (Pesticides, LBW, Latino)
# --------------------------------------------------
RegTable$`Low Birth Weight` <- as.numeric(RegTable$`Low Birth Weight`)
aov_result <- RegTable %>%
  filter(!is.na(`Low Birth Weight`)) %>%
  aov(`Low Birth Weight` ~ Pesticides + Education + CropPasturePct + UninsuredPct + LatinoPct + asinh, data = .)
# --------------------------------------------------
summary(aov_result)
# --------------------------------------------------
# RESULTS
# summary(aov_result)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# Pesticides        1     11   10.79   4.642   0.0312 *  
# Education       615   3311    5.38   2.317  < 2e-16 ***
# CropPasturePct    1     79   78.98  {33.993} 5.82e-09 ***
# UninsuredPct      1      0    0.20   0.088   0.7666    
# LatinoPct         1     14   13.65   5.877   0.0154 *  
# asinh (popdense)  1     91   90.90  39.124 4.25e-10 ***
# Residuals      6057  14073    2.32                     

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# ---------------------------------------------------
# INTERPRETATION: being near a pasture matters but pesticides doesn't?
# ---------------------------------------------------
# New Variable: pesticide x pasture
# ---------------------------------------------------
RegTable <- RegTable %>%
  mutate (PP = Pesticides * CropPasturePct)
# ---------------------------------------------------
# NEW ANOVA
# ---------------------------------------------------
AR <- RegTable %>%
  filter(!is.na(`Low Birth Weight`)) %>%
  aov(`Low Birth Weight` ~  Education + Pesticides + CropPasturePct + PP + UninsuredPct + LatinoPct + asinh, data = .)
# ---------------------------------------------------
# SUMMARY
# ---------------------------------------------------
summary(AR)
# ---------------------------------------------------
# RESULTS
# ---------------------------------------------------
# summary(AR)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# Education       615   3288    5.35   2.301  < 2e-16 ***
# Pesticides        1     34   33.70  14.505 0.000141 ***
# CropPasturePct    1     79   78.98  33.994 5.81e-09 ***
# PP                1      4    3.94   1.697 0.192759    
# UninsuredPct      1      0    0.24   0.102 0.749519    
# LatinoPct         1     13   13.21   5.685 0.017142 *  
#  asinh             1     90   90.17  38.813 4.98e-10 ***
# Residuals      6056  14070    2.32                     
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# ---------------------------------------------------
# INTERPRETATION: pesticides are more impactful now??
# ---------------------------------------------------
# NANCY DREWING ON THE PP - FAIL
# ---------------------------------------------------
interaction.plot(x.factor = RegTable$Pesticides, trace.factor = RegTable$CropPasturePct, response = RegTable$`Low Birth Weight`, fun = mean, type = "b", col = c("red", "blue"), pch = c(1, 19), xlab = "Pesticides", ylab = "Mean of Response", legend = TRUE)
# ---------------------------------------------------
# FILTER OUT MISSING BIRTH WEIGHT / RUN LINEAR REGRESSION MODEL (OLS)
ols <- RegTable %>%
  filter(!is.na(`Low Birth Weight`)) %>%
  lm(`Low Birth Weight` ~  Education + Pesticides + CropPasturePct + PP + UninsuredPct + LatinoPct + asinh, data = .)
summary(ols)
# ----------------------------------------------------
# GROUP TRACTS BY POPULATION DENSITY
# ----------------------------------------------------
RegTable$PopGroup <- ifelse(RegTable$PopulationDensity > median(RegTable$PopulationDensity, na.rm = TRUE), "Above Median", "Below Median")
# ----------------------------------------------------
# BIN PESTICIDE LEVELS INTO QUARTILES
#-----------------------------------------------------
RegTables$PesticideBin <- cut(RegTable$Pesticides, breaks = quantile(RegTable$Pesticides, probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE, labels = c("Low", "Medium_High", "High"))
# SLIGHT JITTER TO PESTICIDE VALUES (ONLY FOR BINNING/PLOTTING)
# ----------------------------------------------------
RegTable$Pesticide_jitter <- jitter(RegTable$Pesticides, amount = 0.01)
# ----------------------------------------------------
# TRY BINNING AGAIN WITH JITTERED VALUES
# ----------------------------------------------------
RegTable$PesticideBin <- cut(RegTable$Pesticide_jitter, breaks = quantile(RegTable$Pesticide_jitter, probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE, labels = c("Low", "Medium-Low", "Medium-High", "High"))
# ----------------------------------------------------
# CREATE VIOLIN PLOT
# ----------------------------------------------------
ggplot(RegTable, aes(x = PesticideBin, y = 'Low Birth Weight', fill = PesticideBin)) + geom_violin(trim = FALSE, alpha = 0.7, color = "gray30") + stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") + facet_wrap(~ PopGroup) + labs(title = "Low Birth Weight by Pesticide Exposure and Population Density", x = "Pesticide Exposure (Quartiles)", y = "Low Birth Weight (%)") + theme_minimal() + theme(legend.position = "none", strip.text = element_text(face = "bold"))
# ----------------------------------------------------
# INTERPRET VIOLIN
# ----------------------------------------------------
# X-axis: 4 bins of pesticide exposure: "Low" → "High"
# Y-axis: % of low birth weight
# Facets: Split into "Above Median" and "Below Median" population density
# Violin shape: Distribution of LBW across tracts in each bin
# Black dot: Mean low birth weight for that bin
# no clear answer*
# ----------------------------------------------------
# CLEAN UP PLOT
# ----------------------------------------------------
ggplot(RegTable, aes(x = PesticideBin, y = `Low Birth Weight`, fill = PesticideBin)) + geom_violin(trim = FALSE, alpha = 0.8, color = "gray30") + geom_jitter(width = 0.1, size = 0.5, alpha = 0.2, color = "black") + stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "white", color = "black") + facet_wrap(~ PopGroup) + labs(title = "Low Birth Weight by Pesticide Exposure (Grouped by Population Density)", subtitle = "Black dots = Mean; Each violin = distribution across census tracts", x = "Pesticide Exposure (Quartiles)", y = "Low Birth Weight (%)") + scale_fill_brewer(palette = "Set2") + theme_minimal(base_size = 14) + theme(legend.position = "none", strip.text = element_text(face = "bold", size = 14), axis.text.x = element_text(angle = 20, hjust = 1))
# ----------------------------------------------------
# CLEAN AGAIN
# ----------------------------------------------------
cleaned_data <- RegTable %>%
  filter(!is.na(`Low Birth Weight`),
    !is.na(Pesticides),
    !is.na(CropPasturePct),
    !is.na(Education),
    !is.na(UninsuredPct),
    !is.na(LatinoPct),
    !is.na(PopGroup))
# -----------------------------------------------------
# RUN SEPARATE MODELS FOR EACH POPULATION GROUP
# -----------------------------------------------------
#         HIGH-DENSITY GROUP
# -----------------------------------------------------
lm_above <- lm(`Low Birth Weight` ~ Pesticides * CropPasturePct + Education + LatinoPct + UninsuredPct, data = cleaned_data[cleaned_data$PopGroup == "Above Median", ])
# -----------------------------------------------------
#         LOW-DENSITY GROUP
# -----------------------------------------------------
lm_below <- lm(`Low Birth Weight` ~ Pesticides * CropPasturePct + Education + LatinoPct + UninsuredPct, data = cleaned_data[cleaned_data$PopGroup == "Below Median", ])
# -----------------------------------------------------
#   VIEW AND INTERPRET RESULTS
# -----------------------------------------------------
summary(lm_above)
summary(lm_below)
# -----------------------------------------------------
# WRITE OUT LINEAR REGRESSION EQUATIONS
# -----------------------------------------------------
#         ABOVE MEDIAN POP
# -----------------------------------------------------
# (Intercept)                       2.44733  
# Pesticides                       -0.00055  
# CropPasturePct                   -0.01312  
# Pesticides:CropPasturePct        0.000074  
# Education                        -0.02279  
# LatinoPct                         0.02120  
# UninsuredPct                      0.03003 
# REGRESSION EQUATION: Low Birth Weight=2.45−0.00055⋅Pesticides−0.0131⋅CropPasturePct+0.000074⋅(Pesticides×CropPasturePct) −0.0228⋅Education+0.0212⋅LatinoPct+0.0300⋅UninsuredPct
# -----------------------------------------------------
#                             OBSERVATIONS
# -----------------------------------------------------
#     PESTICIDES: small negative coefficient (-.00055), holding everything else constant, increasing pesticide use is slighty associated with lower lower birth weight but it's not significant (p = 0.142).
#     CROPPASTUREPCT: negative (-0.0131), significant at 0.01 level - more cropland is associated with lower birth weight rates in higher-density tracts
#     PESTICIDES X CROPPASTUREPCT: small positive (0.000074), signigicant (p = 0.0157) meaning the effects on low birth weight gets more positive as cropland % increases

#     CONTROLS:
#       Education: significant and negative, more education -> lower rates of lower birth weight
#       LatinoPct & UninsuredPct: positive and significant, higher % Latino or unisured -> higher rate of lower birth rate
# ------------------------------------------------------
# BELOW MEDIAN SUMMARY
# ------------------------------------------------------
summary(lm_below)
# ------------------------------------------------------
# BELOW MEDIAN POP
# ------------------------------------------------------
# (Intercept)                     = 3.18181  
# Pesticides                     = -0.0003556  
# CropPasturePct                 = -0.0211287  
# Pesticides:CropPasturePct      =  0.0000734  
# Education                      = -0.0235887  
# LatinoPct                      =  0.0136097  
# UninsuredPct                   =  0.0373833  
# REGRESSION EQUATION: Low Birth Weight=3.18−0.00036⋅Pesticides−0.0211⋅CropPasturePct+0.000073⋅(Pesticides×CropPasturePct)−0.0236⋅Education+0.0136⋅LatinoPct+0.0374⋅UninsuredPct
# -------------------------------------------------------
# ggplot2 WITH REGRESSION LINE AND CONFIDENCE BAND
# -------------------------------------------------------
predicated <- ggpredict(lm_above, terms = c("Pesticides [all]"))
# -------------------------------------------------------
# PLOT PREDICTIONS
# -------------------------------------------------------
ggplot(predicted, aes(x = x, y = predicted)) + geom_line