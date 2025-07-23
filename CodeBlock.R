
# Final Project Eq Ag 2025
# Joss, Brooks, Rowan

# LOAD IN DATA
library(arrow)
library(dplyr)
library(tidyverse)

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
# ----------------------------------------------------

