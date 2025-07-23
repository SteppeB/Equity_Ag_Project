
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

# Mergin tables by geo id
m1 <- merge(CalFilter, LatinoPop, by = "Geo_Id")
m2 <- merge(m1, ImmigrantPop, by = "Geo_Id")
m3 <- merge(m2, JW, by = "Geo_Id")

# Making a percent clip
m3$UninsuredPct <- 100*(m3$UninsuredWomen / m3$`Total Population`)

# For whatever reason latino pop was a boolean value so I fixed that
m3$LatinoPop <- as.numeric(m3$LatinoPop)
m3$LatinoPct <- 100*(m3$LatinoPop / m3$`Total Population`)
m3$MigrantPct <- 100*(m3$MigrantPop / m3$`Total Population`)

# Just cleaning up by filtering out base population data
RegTable <- m3[, -c(10, 11, 12)]

# Regression
Regression <- lm(data = RegTable)
