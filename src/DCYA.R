library(ggplot2)
library(weights)
library(questionr)
library(lme4)
library(sandwich)
library(car)
library(lmtest)
library(multiwayvcov)
library(finalfit)
library(dplyr)
library(gridExtra)
library(cowplot)
library(radiant.data)

# Load each year's survey results into their own dataframe
dcya_2012_raw = read.csv("DCYA_HS_2012.csv", header = TRUE)
dcya_2015_raw = read.csv("DCYA_HS_2015.csv", header = TRUE)
dcya_2018_raw = read.csv("DCYA_HS_2018.csv", header = TRUE)

# report unweighted sample size for raw data
# 2012
length(dcya_2012_raw$RespondentID)
# 2015
length(dcya_2015_raw$RespondentID)
# 2018
length(dcya_2018_raw$RespondentID)

# report weighted sample size for raw data
# 2012
sum(dcya_2012_raw$Weight)
# 2015
sum(dcya_2015_raw$Weight)
# 2018
sum(dcya_2018_raw$Weighting2)


# Retain questions that are important and converge on common column names.
col_names = c("SchoolID", "Weighting", "Age", "Grade", "Race", "Sex", "LGBQ", "Transgender", "FeltAnxious", "CantStopWorry", "Depression", "Height", "Weight", "DentistCheckup", "WearSeatBelt", "GangMember", "Inhalant", "ReportCardGrades")

# splitting by year
dcya_2012 = data.frame(
    dcya_2012_raw$Building,
    dcya_2012_raw$Weight,
    dcya_2012_raw$Age,
    dcya_2012_raw$Grade,
    dcya_2012_raw$NewRace,
    dcya_2012_raw$Gender,
    dcya_2012_raw$GLBT,
    dcya_2012_raw$Transgender,
    dcya_2012_raw$q0039_0001,
    dcya_2012_raw$q0039_0002,
    dcya_2012_raw$q0043,
    suppressWarnings(
        (as.numeric(as.character(dcya_2012_raw$q0025_0001_0001)) + 3) * 12 +
        (as.numeric(as.character(dcya_2012_raw$q0025_0001_0002)) - 1)),
    suppressWarnings(as.numeric(as.character(dcya_2012_raw$q0026_0001))),
    dcya_2012_raw$q0035,
    dcya_2012_raw$q0037,
    dcya_2012_raw$q0105,
    dcya_2012_raw$q0082_0004,
    dcya_2012_raw$q0095)
names(dcya_2012) = col_names

dcya_2015 = data.frame(
    dcya_2015_raw$Building,
    dcya_2015_raw$Weight,
    dcya_2015_raw$Age,
    dcya_2015_raw$Grade,
    dcya_2015_raw$Race,
    dcya_2015_raw$Sex,
    dcya_2015_raw$GLBQ,
    dcya_2015_raw$Transgender,
    dcya_2015_raw$q0046_0001,
    dcya_2015_raw$q0046_0002,
    dcya_2015_raw$q0049,
    suppressWarnings(
        (as.numeric(as.character(dcya_2015_raw$q0029_0001_0001)) + 3) * 12 +
        (as.numeric(as.character(dcya_2015_raw$q0029_0001_0002)) - 1)),
    suppressWarnings(as.numeric(as.character(dcya_2015_raw$q0030_0001))),
    dcya_2015_raw$q0042,
    dcya_2015_raw$q0044,
    dcya_2015_raw$q0103,
    dcya_2015_raw$q0085_0004,
    dcya_2015_raw$q0091)
names(dcya_2015) = col_names

dcya_2018 = data.frame(
    dcya_2018_raw$Schoolcode,
    dcya_2018_raw$Weighting2,
    dcya_2018_raw$Age,
    dcya_2018_raw$Grade,
    dcya_2018_raw$Race,
    dcya_2018_raw$Sex,
    dcya_2018_raw$SexualOrientation,
    dcya_2018_raw$Transgender,
    dcya_2018_raw$FeltAnxious,
    dcya_2018_raw$CantStopWorry,
    dcya_2018_raw$Depression,
    suppressWarnings(
        as.numeric(as.character((dcya_2018_raw$HeightFeet + 3) * 12 + dcya_2018_raw$HeightInch))),
    suppressWarnings(as.numeric(as.character(dcya_2018_raw$Weight))),
    dcya_2018_raw$DentistCheckup,
    dcya_2018_raw$WeatSeatBelt,
    dcya_2018_raw$InaGang,
    dcya_2018_raw$Inhale,
    dcya_2018_raw$GradesInSchool)
names(dcya_2018) = col_names

# data cleaning
# Recode values to reflect numeric age. Assume '14 and younger' is 14 and '18 and older' is 18
# dcya_2012 <- subset(dcya_2012, Age >= 3 & Age <= 7)
dcya_2012["Age"] <- dcya_2012["Age"] + 11
dcya_2015["Age"] <- dcya_2015["Age"] + 13
dcya_2018["Age"] <- dcya_2018["Age"] + 13

# Filter out respondents that selected a grade < 9 or 'other'
dcya_2012 <- subset(dcya_2012, Grade >= 3 & Grade <= 6 | is.na(Grade))
dcya_2012["Grade"] <- dcya_2012["Grade"] + 6
dcya_2015 <- subset(dcya_2015, Grade <= 4 | is.na(Grade))
dcya_2015["Grade"] <- dcya_2015["Grade"] + 8
dcya_2018 <- subset(dcya_2018, Grade <= 4 | is.na(Grade))
dcya_2018["Grade"] <- dcya_2018["Grade"] + 8

# Filter out respondents that did not know what transgender is (or not sure)
dcya_2015 <- subset(dcya_2015,Transgender != 3 | is.na(Transgender))
dcya_2018 <- subset(dcya_2018, Transgender != 3 | is.na(Transgender))

# Normalize on 2012 categories for sexual orientation
dcya_2015 <- subset(dcya_2015, LGBQ != 5 | is.na(LGBQ))
dcya_2018 <- subset(dcya_2018, LGBQ != 4 & LGBQ != 5 & LGBQ != 7 | is.na(LGBQ))
dcya_2018_lgbq <- dcya_2018$LGBQ
dcya_2018_lgbq[dcya_2018$LGBQ == 6] <- 4
dcya_2018["LGBQ"] <- dcya_2018_lgbq


# Normalize on 2012 values for race, dropping 'Other' and 'Pacific Islander'
dcya_2012 = subset(dcya_2012, Race >= 1 & Race <= 8 | is.na(Race))

dcya_2015 = subset(dcya_2015, Race != 9 | is.na(Race))
dcya_2015_race <- dcya_2015$Race
dcya_2015_race[dcya_2015$Race == 1] <- 5
dcya_2015_race[dcya_2015$Race == 5] <- 7
dcya_2015_race[dcya_2015$Race == 7] <- 1
dcya_2015["Race"] = dcya_2015_race

dcya_2018 = subset(dcya_2018, Race != 7 | is.na(Race))
dcya_2018_race <- dcya_2018$Race
dcya_2018_race[dcya_2018$Race == 1] <- 6
dcya_2018_race[dcya_2018$Race == 2] <- 5
dcya_2018_race[dcya_2018$Race == 3] <- 2
dcya_2018_race[dcya_2018$Race == 4] <- 3
dcya_2018_race[dcya_2018$Race == 5] <- 4
dcya_2018_race[dcya_2018$Race == 6] <- 7
dcya_2018_race[dcya_2018$Race == 8] <- 1
dcya_2018_race[dcya_2018$Race == 9] <- 8
dcya_2018["Race"] = dcya_2018_race

# Recode anxiety questions to be on consistent scale 0-3
dcya_2012["FeltAnxious"] <- abs(dcya_2012$FeltAnxious - 4)
dcya_2015["FeltAnxious"] <- abs(dcya_2015$FeltAnxious - 4)
dcya_2018["FeltAnxious"] <- abs(dcya_2018$FeltAnxious - 4)
dcya_2012["CantStopWorry"] <- abs(dcya_2012$CantStopWorry - 4)
dcya_2015["CantStopWorry"] <- abs(dcya_2015$CantStopWorry - 4)
dcya_2018["CantStopWorry"] <- abs(dcya_2018$CantStopWorry - 4)

# Add a year column to each dataframe
dcya_2012["Year"] <- 2012
dcya_2015["Year"] <- 2015
dcya_2018["Year"] <- 2018

# Add a recoded year column to each dataframe 
# dcya_2012["YearRecode"] <- 1
# dcya_2015["YearRecode"] <- 4
# dcya_2018["YearRecode"] <- 7

# Concatenate into a single data frame
dcya <- rbind(dcya_2012, dcya_2015, dcya_2018)

# check data
dim(dcya)
summary(dcya)

# Screener, assign tags for each item
# Identify top/bottom 2.5% of height
# quantile(dcya$Height, 0.975, na.rm = TRUE)
# quantile(dcya$Height, 0.025, na.rm = TRUE)
# dcya$tag1 <- dcya$Height
# dcya$tag1 <- 0
# dcya$tag1[dcya$Height < 60 | dcya$Height > 76] <- 1

# Identify top/bottom 1% of height
quantile(dcya$Height, 0.99, na.rm = TRUE)
quantile(dcya$Height, 0.01, na.rm = TRUE)
dcya$tag1 <- dcya$Height
dcya$tag1 <- 0
dcya$tag1[dcya$Height < 59 | dcya$Height > 93] <- 1

# Identify top/bottom 2.5% of weight
# quantile(dcya$Weight, 0.975, na.rm = TRUE)
# quantile(dcya$Weight, 0.025, na.rm = TRUE)
# dcya$tag2 <- dcya$Weight
# dcya$tag2 <- 0
# dcya$tag2[dcya$Weight < 100 | dcya$Weight > 235] <- 1

# Identify top/bottom 1% of weight
quantile(dcya$Weight, 0.99, na.rm = TRUE)
quantile(dcya$Weight, 0.01, na.rm = TRUE)
dcya$tag2 <- dcya$Weight
dcya$tag2 <- 0
dcya$tag2[dcya$Weight < 90 | dcya$Weight > 300] <- 1

dcya$tag3 <- dcya$DentistCheckup
dcya$tag3 <- 0
dcya$tag3[dcya$DentistCheckup == 4] <- 1

dcya$tag4 <- dcya$WearSeatBelt
dcya$tag4 <- 0
dcya$tag4[dcya$WearSeatBelt == 1] <- 1

dcya$tag5 <- dcya$GangMember
dcya$tag5 <- 0
dcya$tag5[dcya$GangMember >= 3] <- 1

dcya$tag6 <- dcya$Inhalant
dcya$tag6 <- 0
dcya$tag6[dcya$Inhalant >= 2] <- 1

# dcya$tag7 <- dcya$ReportCardGrades 
# dcya$tag7 <- 0
# dcya$tag7[dcya$ReportCardGrades >= 7] <- 1

# sum items for Tagtotal
dcya$tagtotal <- dcya$tag1 + dcya$tag2 + dcya$tag3 + dcya$tag4 + dcya$tag5 + dcya$tag6 
table(dcya$tagtotal)
prop.table(table(dcya$tagtotal))

# filters cases with >= 3 extreme responses on the screener (retains 0, 1, 2), .75% of dataset (less than 1% of the data)
dcya <- filter(dcya, dcya$tagtotal<3)



## Filter out respondents that replied with extreme height and weight
# dcya <- subset(
#	dcya,
#	!(Weight < 70
#        | Weight > 500
#        | Height < 48
#        | Height > 96
#        | (Height < 59 & Weight > 300)
#        | (Height > 93 & Weight < 90)))

# check data
dim(dcya)
summary(dcya)


# examine SchoolID variable
class(dcya$SchoolID)
table(dcya$SchoolID, dcya$Year)


# filter out SchoolID 26 due to low n (risk for identification) and over 1 time pt. only
dcya <- subset(dcya, SchoolID < 26)

# check data
dim(dcya)
summary(dcya)

# Add a column that is the # of times a school is in the dataset
dcya_weighting_factor <- dcya$SchoolID
dcya_weighting_factor[!is.element(dcya$SchoolID, c(1,17,18,23,26))] <- 3
dcya_weighting_factor[is.element(dcya$SchoolID, c(1,17,18,23))] <- 2
dcya_weighting_factor[dcya$SchoolID == 26] <- 1
dcya["WeightingFactor"] = dcya_weighting_factor

# check data
table(dcya$SchoolID, dcya$WeightingFactor)

# Add a column that is the scaled weight based on # of times school is in dataset
dcya["ScaledWeighting"] = dcya$Weighting/dcya$WeightingFactor
range(dcya$ScaledWeighting)

# set reference groups for regression analyses
dcya_sex <- dcya$Sex
dcya_sex[dcya$Sex == 2] <- 1
dcya_sex[dcya$Sex == 1] <- 2
dcya["Sex"] <- dcya_sex
# Sex: 1 = Male, 2 = Female

dcya_depression <- dcya$Depression
dcya_depression[dcya$Depression ==2] <- 1
dcya_depression[dcya$Depression ==1] <-2
dcya["Depression"] <- dcya_depression 
# Depression, 1 = Not depressed, 2 = Depressed

dcya_transgender <- dcya$Transgender
dcya_transgender[dcya$Transgender == 2] <- 1 
dcya_transgender[dcya$Transgender == 1] <- 2 
dcya["Transgender"] <- dcya_transgender
# Transgender: 1 = No, 2 = Yes

# # Remove original rownames
# rownames(dcya) <- NULL

# Re-code variables
dcya$Grade[dcya$Grade == 9] <- "09"
dcya$Grade[dcya$Grade == 10] <- "10"
dcya$Grade[dcya$Grade == 11] <- "11"
dcya$Grade[dcya$Grade == 12] <- "12"
dcya$Race[dcya$Race == 1] <- "1_White"
dcya$Race[dcya$Race == 2] <- "2_Black"
dcya$Race[dcya$Race == 3] <- "3_Latino"
dcya$Race[dcya$Race == 4] <- "4_Hmong"
dcya$Race[dcya$Race == 5] <- "5_Asian"
dcya$Race[dcya$Race == 6] <- "6_Native"
dcya$Race[dcya$Race == 7] <- "7_Middle Eastern"
dcya$Race[dcya$Race == 8] <- "8_Multi-racial"
dcya$Sex[dcya$Sex == 1] <- "1_Male"
dcya$Sex[dcya$Sex == 2] <- "2_Female"
dcya$Depression[dcya$Depression == 1] <- "1_NotDepressed"
dcya$Depression[dcya$Depression == 2] <- "2_Depressed"
dcya$LGBQ[dcya$LGBQ == 1] <- "1_Straight"
dcya$LGBQ[dcya$LGBQ == 2] <- "2_Gay"
dcya$LGBQ[dcya$LGBQ == 3] <- "3_Bisexual"
dcya$LGBQ[dcya$LGBQ == 4] <- "4_Questioning"
dcya$Transgender[dcya$Transgender == 1] <- "1_Cisgender"
dcya$Transgender[dcya$Transgender == 2] <- "2_Transgender"


# Factorize categorical variables: "SchoolID", "Age", "Grade", "Race", "Sex", "LGBQ", "Transgender"
dcya$SchoolID <- factor(dcya$SchoolID)
#dcya$Age <- factor(dcya$Age)
dcya$Grade <- factor(dcya$Grade)
dcya$Race <- factor(dcya$Race)
dcya$Sex <- factor(dcya$Sex)
dcya$Depression <- factor(dcya$Depression)
dcya$LGBQ <- factor(dcya$LGBQ)
dcya$Transgender <- factor(dcya$Transgender)


# check data
dim(dcya)
summary(dcya)

##################################################################
# Examining and handling missing data
# Make a new column that is NA for missing on GAD2
dcya$RespondedGAD2 <- is.na(dcya$FeltAnxious) | is.na(dcya$CantStopWorry) 
dcya$RespondedGAD2[dcya$RespondedGAD2] <- NA

# Make a new column that is NA for missing in dataframe
dcya$MissingAny <- is.na(dcya$FeltAnxious) | is.na(dcya$CantStopWorry) | is.na(dcya$Grade) | is.na(dcya$Race) | is.na(dcya$Sex) | is.na(dcya$LGBQ) | is.na(dcya$Transgender) | is.na(dcya$Depression)
dcya$MissingAny[dcya$MissingAny] <- NA


# examine missing data in covariates and outcome variable
explanatory = c("Grade", "Race", 
	"Sex", "LGBQ", "Transgender", "Depression", "MissingAny")
dependent = "RespondedGAD2"

dcya %>%
	ff_glimpse(dependent, explanatory)
	
dcya %>% 
	missing_plot()
	
dcya %>%
	missing_compare(dependent, explanatory)

###########################################################################
# Handling missing data
# Remove missing cases via listwise deletion
# conduct a complete case analysis, as a result
dcya <- dcya[!is.na(dcya$Grade) & !is.na(dcya$Race) & !is.na(dcya$Sex) & !is.na(dcya$LGBQ) & !is.na(dcya$Transgender) & !is.na(dcya$FeltAnxious) & !is.na(dcya$CantStopWorry) & !is.na(dcya$Depression),]
# check data
dim(dcya)
summary(dcya)

# Outcome Variable
# Add a column that is the combined anxiety score (GAD2)
dcya["GAD2"] = dcya$FeltAnxious + dcya$CantStopWorry

# A binary indication of anxiety based on GAD2 threshold
dcya_anxious <- dcya$GAD2
dcya_anxious[dcya$GAD2 <= 2] <- 0
dcya_anxious[dcya$GAD2 > 2] <- 1
dcya["Anxious"] = dcya_anxious

# check data
dim(dcya)
summary(dcya)
	


# Save each year as a separate dataframe for sanity checking
dcya_2012 <- subset(dcya, Year == 2012)
dcya_2015 <- subset(dcya, Year == 2015)
dcya_2018 <- subset(dcya, Year == 2018)


########################################################

# descriptive stats on merged data
# Sex: 
wpct(dcya$Sex, weight = dcya$ScaledWeighting)
# Race: 
wpct(dcya$Race, weight = dcya$ScaledWeighting)
# LGBQ: 
wpct(dcya$LGBQ, weight = dcya$ScaledWeighting)
# Transgender: 
wpct(dcya$Transgender, weight = dcya$ScaledWeighting)

weighted.mean(dcya$Age, dcya$ScaledWeighting, na.rm = TRUE)
weighted.sd(dcya$Age, dcya$ScaledWeighting, na.rm = TRUE)


# descriptive stats by year
# Grade
n.grade = as.data.frame(table(dcya$Grade, dcya$Year))
names(n.grade) = c("Grade", "Year", "n")
print(as.data.frame(n.grade))
pct_grade_wt = as.data.frame(prop.table(wtd.table(dcya$Grade, dcya$Year, dcya$Weighting), 2))
names(pct_grade_wt) = c("Grade", "Year", "Pct")
pct_grade_wt$Pct <- pct_grade_wt$Pct * 100
print(as.data.frame(pct_grade_wt))

# Sex
n.sex = as.data.frame(table(dcya$Sex, dcya$Year))
names(n.sex) = c("Sex", "Year", "n")
print(head(as.data.frame(n.sex)))
pct_sex_wt = as.data.frame(prop.table(wtd.table(dcya$Sex, dcya$Year, dcya$Weighting), 2))
names(pct_sex_wt) = c("Sex", "Year", "Pct")
pct_sex_wt$Pct <- pct_sex_wt$Pct * 100
print(head(as.data.frame(pct_sex_wt)))

# Race
n.race = as.data.frame(table(dcya$Race, dcya$Year))
names(n.race) = c("Race", "Year", "n")
print(as.data.frame(n.race))
pct_race_wt = as.data.frame(prop.table(wtd.table(dcya$Race, dcya$Year, dcya$Weighting), 2))
names(pct_race_wt) = c("Race", "Year", "Pct")
pct_race_wt$Pct <- pct_race_wt$Pct * 100
print(as.data.frame(pct_race_wt))

# LGBQ
n.lgbq = as.data.frame(table(dcya$LGBQ, dcya$Year))
names(n.lgbq) = c("LGBQ", "Year", "n")
print(as.data.frame(n.lgbq))
pct_lgbq_wt = as.data.frame(prop.table(wtd.table(dcya$LGBQ, dcya$Year, dcya$Weighting), 2))
names(pct_lgbq_wt) = c("LGBQ", "Year", "Pct")
pct_lgbq_wt$Pct <- pct_lgbq_wt$Pct * 100
print(as.data.frame(pct_lgbq_wt))

# Transgender
n.transgender = as.data.frame(table(dcya$Transgender, dcya$Year))
names(n.transgender) = c("Transgender", "Year", "n")
print(as.data.frame(n.transgender))
pct_transgender_wt = as.data.frame(prop.table(wtd.table(dcya$Transgender, dcya$Year, dcya$Weighting), 2))
names(pct_transgender_wt) = c("Transgender", "Year", "Pct")
pct_transgender_wt$Pct <- pct_transgender_wt$Pct * 100
print(as.data.frame(pct_transgender_wt))

# Anxiety by year in whole sample
pct_anxious_wt = as.data.frame(prop.table(wtd.table(dcya$Anxious, dcya$Year, dcya$Weighting), 2))
names(pct_anxious_wt) = c("Anxious", "Year", 'Pct')
pct_anxious_wt <- subset(pct_anxious_wt, Anxious == 1)
pct_anxious_wt$Pct <- pct_anxious_wt$Pct * 100
print(head(as.data.frame(pct_anxious_wt)))

# recode survey year for regression analyses
dcya$Year[dcya$Year == 2012] <- 1
dcya$Year[dcya$Year == 2015] <- 4
dcya$Year[dcya$Year == 2018] <- 7

########################################################
# model building

# fit fixed-effect logistic regression to examine trend in anxiety prevalence between 2012-2018, unweighted binomial, unadjusted model
glm1 <- glm(Anxious~Year, family = "binomial", data=dcya)
summary(glm1)

# fit fixed-effect logistic regression to examine trend in anxiety prevalence between 2012-2018, weighted binomial, unadjusted model
glm2 <- glm(Anxious~Year, weights = ScaledWeighting, family = "binomial", data=dcya)
summary(glm2)

# fit fixed-effect logistic regression model, weighted quasibinomial, unadjusted model
glm3 <- glm(Anxious~Year, weights = ScaledWeighting, family = "quasibinomial", data=dcya)
summary(glm3)

# manually calc default standard errors
# first examine variance-covariance matrix
vcov(glm3)
se_default <- sqrt(diag(vcov(glm3))) 
se_default

# calculate cluster-robust standard errors
# need to account for clustering in schools, use vcovCL
# clustered sandwich estimators are used to adjust inference when errors are correlated within (but not between) clusters
# we assume independence across schools
# vcovCL(x, cluster = variable indicating clustering of observations, sandwich = TRUE)
voc_cl <- vcovCL(glm3, cluster = ~SchoolID, sandwich = TRUE)
voc_cl # this gives you variance covariance matrix accouting for clustering
se_cl <-sqrt(diag(voc_cl)) 
se_cl # extract cluster robust standard errors

# coefficient tables
coeftest(glm3, voc_cl)
# compare to coefficient table with default SEs
coeftest(glm3)

# calculate OR and 95% CI manually
est <- exp(glm3$coef)
lower <- est-1.96*se_cl
upper <- est+1.96*se_cl
cbind(est, lower, upper)

# manually calculate p-values 
z_stat <- coef(glm3)/se_cl # calculate the z-statistic (coefficient/corresponding standard error)
z_stat
p_values <- pchisq(z_stat^2, 1, lower.tail = FALSE) # compare squared z-statistic to a chi-suared distribution on 1 degree of freedom
p_values


########################################################
## Alternate way, check to confirm results above replicate
# without cluster SE
summary(glm3)

# cluster standard errors by type
cluster_se <- cluster.vcov(glm3, ~SchoolID)
coeftest(glm3, cluster_se)


# Odds Ratio and 95% CI
# sum.coef <- summary(m1)$coef
# est <- exp(sum.coef[,1])
# lower.ci <-exp(sum.coef[,1]-1.96*sum.coef[,2])
# upper.ci <-exp(sum.coef[,1]+1.96*sum.coef[,2])
# cbind(est,lower.ci, upper.ci)


########################################################

## Adjusted Models
# fixed effect logistic regression to examine trend in anxiety prevalence between 2012-2018, unweighted binomial, adjusted model
m1 <- glm(Anxious ~ Year + Grade + Race + Sex + LGBQ + Transgender + Depression, data = dcya, family = binomial)
summary(m1)

# fixed effect logistic regression to examine trend in anxiety prevalence between 2012-2018, weighted binomial, adjusted model
m2 <- glm(Anxious ~ Year + Grade + Race + Sex + LGBQ + Transgender + Depression, data = dcya, family = binomial, weights = ScaledWeighting)
summary(m2)

# fixed effect logistic regression to examine trend in anxiety prevalence between 2012-2018, weighted quasibinomial, adjusted model
m3 <- glm(Anxious ~ Year + Grade + Race + Sex + LGBQ + Transgender + Depression, data = dcya, family = quasibinomial, weights = ScaledWeighting)
summary(m3)

### cluster robust SEs for ajusted model (m3)
# cluster standard errors by type
cluster_se_adj_model <- cluster.vcov(m3, ~SchoolID)
cluster_se_adj_model_extracted <- sqrt(diag(cluster_se_adj_model))
cluster_se_adj_model_extracted
cf1 <- coeftest(m3, cluster_se_adj_model)

# Report Adjusted Odds Ratio & 95% confidence interval for logistic regression
est <- exp(m3$coef)
lower.ci <- est-1.96*cluster_se_adj_model_extracted
upper.ci <- est+1.96*cluster_se_adj_model_extracted
cbind(est,lower.ci, upper.ci)


########################################################
# Stratified Regression Models
# Grade
## Grade 9
dcya_grade9 = dcya %>%
	filter(Grade == "09")
# unadjusted model
grade9_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_grade9)
summary(grade9_glm1)
cluster_se_grade9 <- cluster.vcov(grade9_glm1, ~SchoolID)
cluster_se_grade9_extracted <- sqrt(diag(cluster_se_grade9))
coeftest(grade9_glm1, cluster_se_grade9)

# Odds Ratio and 95% CI
est <- exp(grade9_glm1$coef)
lower.ci <- est-1.96*cluster_se_grade9_extracted
upper.ci <- est+1.96*cluster_se_grade9_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
grade9_glm2 <- glm(Anxious ~ Year + Sex + Race + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_grade9)
summary(grade9_glm2)
cluster_se_grade9_a <- cluster.vcov(grade9_glm2, ~SchoolID)
cluster_se_grade9_a_extracted <- sqrt(diag(cluster_se_grade9_a))
coeftest(grade9_glm2, cluster_se_grade9_a)

# Odds Ratio and 95% CI
est <- exp(grade9_glm2$coef)
lower.ci <-est-1.96*cluster_se_grade9_a_extracted 
upper.ci <-est+1.96*cluster_se_grade9_a_extracted
cbind(est,lower.ci, upper.ci)

## Grade 10
dcya_grade10 = dcya %>%
	filter(Grade == "10")
# unadjusted model
grade10_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_grade10)
summary(grade10_glm1)
cluster_se_grade10 <- cluster.vcov(grade10_glm1, ~SchoolID)
cluster_se_grade10_extracted <- sqrt(diag(cluster_se_grade10))
coeftest(grade10_glm1, cluster_se_grade10)

# Odds Ratio and 95% CI
est <- exp(grade10_glm1$coef)
lower.ci <-est-1.96*cluster_se_grade10_extracted 
upper.ci <-est+1.96*cluster_se_grade10_extracted 
cbind(est,lower.ci, upper.ci)

# adjusted model
grade10_glm2 <- glm(Anxious ~ Year + Sex + Race + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_grade10)
summary(grade10_glm2)
cluster_se_grade10_a <- cluster.vcov(grade10_glm2, ~SchoolID)
cluster_se_grade10_a_extracted <- sqrt(diag(cluster_se_grade10_a))
coeftest(grade10_glm2, cluster_se_grade10_a)

# Odds Ratio and 95% CI
est <- exp(grade10_glm2$coef)
lower.ci <-est-1.96*cluster_se_grade10_a_extracted 
upper.ci <-est+1.96*cluster_se_grade10_a_extracted
cbind(est,lower.ci, upper.ci)

## Grade 11
dcya_grade11 = dcya %>%
	filter(Grade == "11")
# unadjusted model
grade11_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_grade11)
summary(grade11_glm1)
cluster_se_grade11 <- cluster.vcov(grade11_glm1, ~SchoolID)
cluster_se_grade11_extracted <- sqrt(diag(cluster_se_grade11))
coeftest(grade11_glm1, cluster_se_grade11)

# Odds Ratio and 95% CI
est <- exp(grade11_glm1$coef)
lower.ci <-est-1.96*cluster_se_grade11_extracted
upper.ci <-est+1.96*cluster_se_grade11_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
grade11_glm2 <- glm(Anxious ~ Year + Sex + Race + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_grade11)
summary(grade11_glm2)
cluster_se_grade11_a <- cluster.vcov(grade11_glm2, ~SchoolID)
cluster_se_grade11_a_extracted <- sqrt(diag(cluster_se_grade11_a))
coeftest(grade11_glm2, cluster_se_grade11_a)

# Odds Ratio and 95% CI
est <- exp(grade11_glm2$coef)
lower.ci <-est-1.96*cluster_se_grade11_a_extracted
upper.ci <-est+1.96*cluster_se_grade11_a_extracted
cbind(est,lower.ci, upper.ci)

## Grade 12
dcya_grade12 = dcya %>%
	filter(Grade == "12")
# unadjusted model
grade12_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_grade12)
summary(grade12_glm1)
cluster_se_grade12 <- cluster.vcov(grade12_glm1, ~SchoolID)
cluster_se_grade12_extracted <- sqrt(diag(cluster_se_grade12))
coeftest(grade12_glm1, cluster_se_grade12)

# Odds Ratio and 95% CI
est <- exp(grade12_glm1$coef)
lower.ci <- est-1.96*cluster_se_grade12_extracted
upper.ci <- est+1.96*cluster_se_grade12_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
grade12_glm2 <- glm(Anxious ~ Year + Sex + Race + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_grade12)
summary(grade12_glm2)
cluster_se_grade12_a <- cluster.vcov(grade12_glm2, ~SchoolID)
cluster_se_grade12_a_extracted <- sqrt(diag(cluster_se_grade12_a))
coeftest(grade12_glm2, cluster_se_grade12_a)

# Odds Ratio and 95% CI
est <- exp(grade12_glm2$coef)
lower.ci <- est-1.96*cluster_se_grade12_a_extracted
upper.ci <- est+1.96*cluster_se_grade12_a_extracted
cbind(est,lower.ci, upper.ci)

# Sex
## Female
dcya_female = dcya %>%
	filter(Sex == "2_Female")
# unadjusted model
female_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_female)
summary(female_glm1)
cluster_se_female <- cluster.vcov(female_glm1, ~SchoolID)
cluster_se_female_extracted <- sqrt(diag(cluster_se_female))
coeftest(female_glm1, cluster_se_female)

# Odds Ratio and 95% CI
est <- exp(female_glm1$coef)
lower.ci <- est-1.96*cluster_se_female_extracted
upper.ci <- est+1.96*cluster_se_female_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
female_glm2 <- glm(Anxious ~ Year + Grade + Race + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_female)
summary(female_glm2)
cluster_se_female_a <- cluster.vcov(female_glm2, ~SchoolID)
cluster_se_female_a_extracted <- sqrt(diag(cluster_se_female_a))
coeftest(female_glm2, cluster_se_female_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(female_glm2$coef)
lower.ci <- est-1.96*cluster_se_female_a_extracted
upper.ci <- est+1.96*cluster_se_female_a_extracted
cbind(est,lower.ci, upper.ci)

## Male
dcya_male = dcya %>%
	filter(Sex == "1_Male")
# unadjusted model
male_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_male)
summary(male_glm1)
cluster_se_male <- cluster.vcov(male_glm1, ~SchoolID)
cluster_se_male_extracted <- sqrt(diag(cluster_se_male))
coeftest(male_glm1, cluster_se_male)

# Odds Ratio and 95% CI
est <- exp(male_glm1$coef)
lower.ci <- est-1.96*cluster_se_male_extracted
upper.ci <- est+1.96*cluster_se_male_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
male_glm2 <- glm(Anxious ~ Year + Grade + Race + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_male)
summary(male_glm2)
cluster_se_male_a <- cluster.vcov(male_glm2, ~SchoolID)
cluster_se_male_a_extracted <- sqrt(diag(cluster_se_male_a))
coeftest(male_glm2, cluster_se_male_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(male_glm2$coef)
lower.ci <- est-1.96*cluster_se_male_a_extracted
upper.ci <- est+1.96*cluster_se_male_a_extracted
cbind(est,lower.ci, upper.ci)


# Race/Ethnicity
## White
dcya_white = dcya %>%
	filter(Race == "1_White")
# unadjusted model
white_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_white)
summary(white_glm1)
cluster_se_white <- cluster.vcov(white_glm1, ~SchoolID)
cluster_se_white_extracted <- sqrt(diag(cluster_se_white))
coeftest(white_glm1, cluster_se_white)

# Odds Ratio and 95% CI
est <- exp(white_glm1$coef)
lower.ci <- est-1.96*cluster_se_white_extracted
upper.ci <- est+1.96*cluster_se_white_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
white_glm2 <- glm(Anxious ~ Year + Grade + Sex + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_white)
summary(white_glm2)
cluster_se_white_a <- cluster.vcov(white_glm2, ~SchoolID)
cluster_se_white_a_extracted <- sqrt(diag(cluster_se_white_a))
coeftest(white_glm2, cluster_se_white_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(white_glm2$coef)
lower.ci <- est-1.96*cluster_se_white_a_extracted
upper.ci <- est+1.96*cluster_se_white_a_extracted
cbind(est,lower.ci, upper.ci)


## Black
dcya_black = dcya %>%
	filter(Race == "2_Black")
# unadjusted model
black_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_black)
summary(black_glm1)
cluster_se_black <- cluster.vcov(black_glm1, ~SchoolID)
cluster_se_black_extracted <- sqrt(diag(cluster_se_black))
coeftest(black_glm1, cluster_se_black)

# Odds Ratio and 95% CI
est <- exp(black_glm1$coef)
lower.ci <- est-1.96*cluster_se_black_extracted
upper.ci <- est+1.96*cluster_se_black_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
black_glm2 <- glm(Anxious ~ Year + Grade + Sex + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_black)
summary(black_glm2)
cluster_se_black_a <- cluster.vcov(black_glm2, ~SchoolID)
cluster_se_black_a_extracted <- sqrt(diag(cluster_se_black_a))
coeftest(black_glm2, cluster_se_black_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(black_glm2$coef)
lower.ci <- est-1.96*cluster_se_black_a_extracted
upper.ci <- est+1.96*cluster_se_black_a_extracted
cbind(est,lower.ci, upper.ci)


## Latino
dcya_latino = dcya %>%
	filter(Race == "3_Latino")
# unadjusted model
latino_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_latino)
summary(latino_glm1)
cluster_se_latino <- cluster.vcov(latino_glm1, ~SchoolID)
cluster_se_latino_extracted <- sqrt(diag(cluster_se_latino))
coeftest(latino_glm1, cluster_se_latino)

# Odds Ratio and 95% CI
est <- exp(latino_glm1$coef)
lower.ci <- est-1.96*cluster_se_latino_extracted
upper.ci <- est+1.96*cluster_se_latino_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
latino_glm2 <- glm(Anxious ~ Year + Grade + Sex + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_latino)
summary(latino_glm2)
cluster_se_latino_a <- cluster.vcov(latino_glm2, ~SchoolID)
cluster_se_latino_a_extracted <- sqrt(diag(cluster_se_latino_a))
coeftest(latino_glm2, cluster_se_latino_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(latino_glm2$coef)
lower.ci <- est-1.96*cluster_se_latino_a_extracted
upper.ci <- est+1.96*cluster_se_latino_a_extracted
cbind(est,lower.ci, upper.ci)


## Hmong
dcya_hmong = dcya %>%
	filter(Race == "4_Hmong")
# unadjusted model
hmong_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_hmong)
summary(hmong_glm1)
cluster_se_hmong <- cluster.vcov(hmong_glm1, ~SchoolID)
cluster_se_hmong_extracted <- sqrt(diag(cluster_se_hmong))
coeftest(hmong_glm1, cluster_se_hmong)

# Odds Ratio and 95% CI
est <- exp(hmong_glm1$coef)
lower.ci <- est-1.96*cluster_se_hmong_extracted
upper.ci <- est+1.96*cluster_se_hmong_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
hmong_glm2 <- glm(Anxious ~ Year + Grade + Sex + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_hmong)
summary(hmong_glm2)
cluster_se_hmong_a <- cluster.vcov(hmong_glm2, ~SchoolID)
cluster_se_hmong_a_extracted <- sqrt(diag(cluster_se_hmong_a))
coeftest(hmong_glm2, cluster_se_hmong_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(hmong_glm2$coef)
lower.ci <- est-1.96*cluster_se_hmong_a_extracted
upper.ci <- est+1.96*cluster_se_hmong_a_extracted
cbind(est,lower.ci, upper.ci)


## Asian
dcya_asian = dcya %>%
	filter(Race == "5_Asian")
# unadjusted model
asian_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_asian)
summary(asian_glm1)
cluster_se_asian <- cluster.vcov(asian_glm1, ~SchoolID)
cluster_se_asian_extracted <- sqrt(diag(cluster_se_asian))
coeftest(asian_glm1, cluster_se_asian)

# Odds Ratio and 95% CI
est <- exp(asian_glm1$coef)
lower.ci <- est-1.96*cluster_se_asian_extracted
upper.ci <- est+1.96*cluster_se_asian_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
asian_glm2 <- glm(Anxious ~ Year + Grade + Sex + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_asian)
summary(asian_glm2)
cluster_se_asian_a <- cluster.vcov(asian_glm2, ~SchoolID)
cluster_se_asian_a_extracted <- sqrt(diag(cluster_se_asian_a))
coeftest(asian_glm2, cluster_se_asian_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(asian_glm2$coef)
lower.ci <- est-1.96*cluster_se_asian_a_extracted
upper.ci <- est+1.96*cluster_se_asian_a_extracted
cbind(est,lower.ci, upper.ci)


## Native
dcya_native = dcya %>%
	filter(Race == "6_Native")
# unadjusted model
native_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_native)
summary(native_glm1)
cluster_se_native <- cluster.vcov(native_glm1, ~SchoolID)
cluster_se_native_extracted <- sqrt(diag(cluster_se_native))
coeftest(native_glm1, cluster_se_native)

# Odds Ratio and 95% CI
est <- exp(native_glm1$coef)
lower.ci <- est-1.96*cluster_se_native_extracted
upper.ci <- est+1.96*cluster_se_native_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
native_glm2 <- glm(Anxious ~ Year + Grade + Sex + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_native)
summary(native_glm2)
cluster_se_native_a <- cluster.vcov(native_glm2, ~SchoolID)
cluster_se_native_a_extracted <- sqrt(diag(cluster_se_native_a))
coeftest(native_glm2, cluster_se_native_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(native_glm2$coef)
lower.ci <- est-1.96*cluster_se_native_a_extracted 
upper.ci <- est+1.96*cluster_se_native_a_extracted 
cbind(est,lower.ci, upper.ci)


## Middle Eastern
dcya_me = dcya %>%
	filter(Race == "7_Middle Eastern")
# unadjusted model
me_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_me)
summary(me_glm1)
cluster_se_me <- cluster.vcov(me_glm1, ~SchoolID)
cluster_se_me_extracted <- sqrt(diag(cluster_se_me))
coeftest(me_glm1, cluster_se_me)

# Odds Ratio and 95% CI
est <- exp(me_glm1$coef)
lower.ci <- est-1.96*cluster_se_me_extracted
upper.ci <- est+1.96*cluster_se_me_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
me_glm2 <- glm(Anxious ~ Year + Grade + Sex + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_me)
summary(me_glm2)
cluster_se_me_a <- cluster.vcov(me_glm2, ~SchoolID)
cluster_se_me_a_extracted <- sqrt(diag(cluster_se_me_a))
coeftest(me_glm2, cluster_se_me_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(me_glm2$coef)
lower.ci <- est-1.96*cluster_se_me_a_extracted
upper.ci <- est+1.96*cluster_se_me_a_extracted
cbind(est,lower.ci, upper.ci)


## Multi-racial
dcya_mr = dcya %>%
	filter(Race == "8_Multi-racial")
# unadjusted model
mr_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_mr)
summary(mr_glm1)
cluster_se_mr <- cluster.vcov(mr_glm1, ~SchoolID)
cluster_se_mr_extracted <- sqrt(diag(cluster_se_mr))
coeftest(mr_glm1, cluster_se_mr)

# Odds Ratio and 95% CI
est <- exp(mr_glm1$coef)
lower.ci <- est-1.96*cluster_se_mr_extracted
upper.ci <- est+1.96*cluster_se_mr_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
mr_glm2 <- glm(Anxious ~ Year + Grade + Sex + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_mr)
summary(mr_glm2)
cluster_se_mr_a <- cluster.vcov(mr_glm2, ~SchoolID)
cluster_se_mr_a_extracted <- sqrt(diag(cluster_se_mr_a))
coeftest(mr_glm2, cluster_se_mr_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(mr_glm2$coef)
lower.ci <- est-1.96*cluster_se_mr_a_extracted
upper.ci <- est+1.96*cluster_se_mr_a_extracted
cbind(est,lower.ci, upper.ci)


# Sexual Orientation
## Straight
dcya_straight = dcya %>%
	filter(LGBQ == "1_Straight")
# unadjusted model
straight_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_straight)
summary(straight_glm1)
cluster_se_straight <- cluster.vcov(straight_glm1, ~SchoolID)
cluster_se_straight_extracted <- sqrt(diag(cluster_se_straight))
coeftest(straight_glm1, cluster_se_straight)

# Odds Ratio and 95% CI
est <- exp(straight_glm1$coef)
lower.ci <- est-1.96*cluster_se_straight_extracted
upper.ci <- est+1.96*cluster_se_straight_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
straight_glm2 <- glm(Anxious ~ Year + Grade + Sex + Race + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_straight)
summary(straight_glm2)
cluster_se_straight_a <- cluster.vcov(straight_glm2, ~SchoolID)
cluster_se_straight_a_extracted <- sqrt(diag(cluster_se_straight_a))
coeftest(straight_glm2, cluster_se_straight_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(straight_glm2$coef)
lower.ci <- est-1.96*cluster_se_straight_a_extracted
upper.ci <- est+1.96*cluster_se_straight_a_extracted
cbind(est,lower.ci, upper.ci)

## Gay or Lesbian
dcya_gay = dcya %>%
	filter(LGBQ == "2_Gay")
# unadjusted model
gay_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_gay)
summary(gay_glm1)
cluster_se_gay <- cluster.vcov(gay_glm1, ~SchoolID)
cluster_se_gay_extracted <- sqrt(diag(cluster_se_gay))
coeftest(gay_glm1, cluster_se_gay)

# Odds Ratio and 95% CI
est <- exp(gay_glm1$coef)
lower.ci <- est-1.96*cluster_se_gay_extracted
upper.ci <- est+1.96*cluster_se_gay_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
gay_glm2 <- glm(Anxious ~ Year + Grade + Sex + Race + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_gay)
summary(gay_glm2)
cluster_se_gay_a <- cluster.vcov(gay_glm2, ~SchoolID)
cluster_se_gay_a_extracted <- sqrt(diag(cluster_se_gay_a))
coeftest(gay_glm2, cluster_se_gay_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(gay_glm2$coef)
lower.ci <- est-1.96*cluster_se_gay_a_extracted
upper.ci <- est+1.96*cluster_se_gay_a_extracted
cbind(est,lower.ci, upper.ci)


## Bisexual
dcya_bisexual = dcya %>%
	filter(LGBQ == "3_Bisexual")
# unadjusted model
bisexual_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_bisexual)
summary(bisexual_glm1)
cluster_se_bisexual <- cluster.vcov(bisexual_glm1, ~SchoolID)
cluster_se_bisexual_extracted <- sqrt(diag(cluster_se_bisexual))
coeftest(bisexual_glm1, cluster_se_bisexual)

# Odds Ratio and 95% CI
est <- exp(bisexual_glm1$coef)
lower.ci <- est-1.96*cluster_se_bisexual_extracted
upper.ci <- est+1.96*cluster_se_bisexual_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
bisexual_glm2 <- glm(Anxious ~ Year + Grade + Sex + Race + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_bisexual)
summary(bisexual_glm2)
cluster_se_bisexual_a <- cluster.vcov(bisexual_glm2, ~SchoolID)
cluster_se_bisexual_a_extracted <- sqrt(diag(cluster_se_bisexual_a))
coeftest(bisexual_glm2, cluster_se_bisexual_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(bisexual_glm2$coef)
lower.ci <- est-1.96*cluster_se_bisexual_a_extracted
upper.ci <- est+1.96*cluster_se_bisexual_a_extracted
cbind(est,lower.ci, upper.ci)


## Questioning
dcya_questioning = dcya %>%
	filter(LGBQ == "4_Questioning")
# unadjusted model
questioning_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_questioning)
summary(questioning_glm1)
cluster_se_questioning <- cluster.vcov(questioning_glm1, ~SchoolID)
cluster_se_questioning_extracted <- sqrt(diag(cluster_se_questioning))
coeftest(questioning_glm1, cluster_se_questioning)

# Odds Ratio and 95% CI
est <- exp(questioning_glm1$coef)
lower.ci <- est-1.96*cluster_se_questioning_extracted
upper.ci <- est+1.96*cluster_se_questioning_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
questioning_glm2 <- glm(Anxious ~ Year + Grade + Sex + Race + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_questioning)
summary(questioning_glm2)
cluster_se_questioning_a <- cluster.vcov(questioning_glm2, ~SchoolID)
cluster_se_questioning_a_extracted <- sqrt(diag(cluster_se_questioning_a))
coeftest(questioning_glm2, cluster_se_questioning_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(questioning_glm2$coef)
lower.ci <- est-1.96*cluster_se_questioning_a_extracted
upper.ci <- est+1.96*cluster_se_questioning_a_extracted
cbind(est,lower.ci, upper.ci)



# Gender Identity
## Cisgender
dcya_cisgender = dcya %>%
	filter(Transgender == "1_Cisgender")
# unadjusted model
cisgender_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_cisgender)
summary(cisgender_glm1)
cluster_se_cisgender <- cluster.vcov(cisgender_glm1, ~SchoolID)
cluster_se_cisgender_extracted <- sqrt(diag(cluster_se_cisgender))
coeftest(cisgender_glm1, cluster_se_cisgender)

# Odds Ratio and 95% CI
est <- exp(cisgender_glm1$coef)
lower.ci <- est-1.96*cluster_se_cisgender_extracted
upper.ci <- est+1.96*cluster_se_cisgender_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
cisgender_glm2 <- glm(Anxious ~ Year + Grade + Sex + Race + LGBQ + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_cisgender)
summary(cisgender_glm2)
cluster_se_cisgender_a <- cluster.vcov(cisgender_glm2, ~SchoolID)
cluster_se_cisgender_a_extracted <- sqrt(diag(cluster_se_cisgender_a))
coeftest(cisgender_glm2, cluster_se_cisgender_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(cisgender_glm2$coef)
lower.ci <- est-1.96*cluster_se_cisgender_a_extracted
upper.ci <- est+1.96*cluster_se_cisgender_a_extracted
cbind(est,lower.ci, upper.ci)

## Transgender
dcya_transgender = dcya %>%
	filter(Transgender == "2_Transgender")
# unadjusted model
transgender_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_transgender)
summary(transgender_glm1)
cluster_se_transgender <- cluster.vcov(transgender_glm1, ~SchoolID)
cluster_se_transgender_extracted <- sqrt(diag(cluster_se_transgender))
coeftest(transgender_glm1, cluster_se_transgender)

# Odds Ratio and 95% CI
est <- exp(transgender_glm1$coef)
lower.ci <- est-1.96*cluster_se_transgender_extracted
upper.ci <- est+1.96*cluster_se_transgender_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
transgender_glm2 <- glm(Anxious ~ Year + Grade + Sex + Race + LGBQ + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_transgender)
summary(transgender_glm2)
cluster_se_transgender_a <- cluster.vcov(transgender_glm2, ~SchoolID)
cluster_se_transgender_a_extracted <- sqrt(diag(cluster_se_transgender_a))
coeftest(transgender_glm2, cluster_se_transgender_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(transgender_glm2$coef)
lower.ci <- est-1.96*cluster_se_transgender_a_extracted
upper.ci <- est+1.96*cluster_se_transgender_a_extracted
cbind(est,lower.ci, upper.ci)



########################################################
# Differential time trends: Logistic regressions with two-way interaction of year x each demographic variable
# run exact same adjusted model, and add the interaction term
# don't interpret main effects, just write about interaction term 

glm_grade <- glm(Anxious ~ Race + Sex + LGBQ + Transgender + Depression + Year*Grade, weights = ScaledWeighting, family = "quasibinomial", data=dcya)
summary(glm_grade)
# cluster standard errors by type
cluster_se_dt1 <- cluster.vcov(glm_grade, ~SchoolID)
coeftest(glm_grade, cluster_se_dt1)


glm_sex <- glm(Anxious ~ Grade + Race + LGBQ + Transgender + Depression + Year*Sex, weights = ScaledWeighting, family = "quasibinomial", data=dcya)
summary(glm_sex)
# cluster standard errors by type
cluster_se_dt2 <- cluster.vcov(glm_sex, ~SchoolID)
coeftest(glm_sex, cluster_se_dt2)

glm_race <- glm(Anxious ~ Grade + Sex + LGBQ + Transgender + Depression + Year*Race, weights = ScaledWeighting, family = "quasibinomial", data=dcya)
summary(glm_race)
# cluster standard errors by type
cluster_se_dt3 <- cluster.vcov(glm_race, ~SchoolID)
coeftest(glm_race, cluster_se_dt3)

glm_lgbq <- glm(Anxious ~ Grade + Race + Sex + Transgender + Depression + Year*LGBQ, weights = ScaledWeighting, family = "quasibinomial", data=dcya)
summary(glm_lgbq)
# cluster standard errors by type
cluster_se_dt4 <- cluster.vcov(glm_lgbq, ~SchoolID)
coeftest(glm_lgbq, cluster_se_dt4)

glm_transgender <- glm(Anxious ~ Year + Grade + Race + Sex + LGBQ + Depression + Year*Transgender, weights = ScaledWeighting, family = "quasibinomial", data=dcya)
summary(glm_transgender)
# cluster standard errors by type
cluster_se_dt5 <- cluster.vcov(glm_transgender, ~SchoolID)
coeftest(glm_transgender, cluster_se_dt5)


########################################################

# Computes the percentage of anxious students, stratified by the given column
anxious_by_col <- function(col_name) {
    # make separate dataframes for each year
    year_2012 = subset(as.data.frame(prop.table(wtd.table(dcya_2012$Anxious, dcya_2012[, col_name], dcya_2012$Weighting), 2)), Var1 == 1)
    year_2015 = subset(as.data.frame(prop.table(wtd.table(dcya_2015$Anxious, dcya_2015[, col_name], dcya_2015$Weighting), 2)), Var1 == 1)
    year_2018 = subset(as.data.frame(prop.table(wtd.table(dcya_2018$Anxious, dcya_2018[, col_name], dcya_2018$Weighting), 2)), Var1 == 1)

    # repurpose a column and store the year
    year_2012$Var1 <- 2012
    year_2015$Var1 <- 2015
    year_2018$Var1 <- 2018

    # combine into a single dataframe
    all_years = rbind(year_2012, year_2015, year_2018)
    names(all_years) <- c("Year", col_name, "PctAnxious")
    rownames(all_years) <- NULL
    all_years$PctAnxious <- all_years$PctAnxious * 100
    all_years
}

# Computes the percentage of anxious students, stratified by the given column
# by_age <- anxious_by_col("Age")
# print(by_age)

by_grade = anxious_by_col("Grade")
print(by_grade)

by_sex = anxious_by_col("Sex")
print(by_sex)

by_race = anxious_by_col("Race")
print(by_race)

by_LGBQ = anxious_by_col("LGBQ")
print(by_LGBQ)

by_transgender = anxious_by_col("Transgender")
print(by_transgender)


# Computes the confidence interval of anxious students, stratified by the year
anxious_ci_by_year <- function(interval = 0.95) {
	ci_by_year = as.data.frame.matrix(t(wtd.table(dcya$Anxious, dcya$Year, dcya$Weighting))[, c(2,1)])
	ci_by_year <- data.frame(Year = row.names(ci_by_year), ci_by_year, row.names = NULL)
	names(ci_by_year) <- c("Year", "Anxious", "NotAnxious")
	conf_intervals = mapply(
	      function(x, y) {
	            result = prop.test(x, x+y, conf.level = interval, correct = FALSE);
	            result$conf.int
	        },
	        ci_by_year$Anxious,
	        ci_by_year$NotAnxious)
	conf_intervals = as.data.frame(t(conf_intervals))
	colnames(conf_intervals) <- c("CI_lower", "CI_upper")
	ci_by_year <- cbind(ci_by_year, conf_intervals)
}

# Computes the confidence interval of anxious students, stratified by the given column
anxious_ci_by_col <- function(col_name, interval = 0.95) {
    year_2012 = t(wtd.table(dcya_2012$Anxious, dcya_2012[, col_name], dcya_2012$Weighting))
    year_2012 <- as.data.frame.matrix(year_2012[, c(2,1)])
    year_2012 <- data.frame(Year = 2012, Col = row.names(year_2012), year_2012, row.names = NULL)

    year_2015 = t(wtd.table(dcya_2015$Anxious, dcya_2015[, col_name], dcya_2015$Weighting))
    year_2015 <- as.data.frame.matrix(year_2015[, c(2,1)])
    year_2015 <- data.frame(Year = 2015, Col = row.names(year_2015), year_2015, row.names = NULL)

    year_2018 = t(wtd.table(dcya_2018$Anxious, dcya_2018[, col_name], dcya_2018$Weighting))
    year_2018 <- as.data.frame.matrix(year_2018[, c(2,1)])
    year_2018 <- data.frame(Year = 2018, Col = row.names(year_2018), year_2018, row.names = NULL)

    all_years = rbind(year_2012, year_2015, year_2018)
    names(all_years) <- c("Year", col_name, "Anxious", "NotAnxious")
    
    pct_anxious = all_years["Anxious"] / (all_years["Anxious"] + all_years["NotAnxious"]) * 100
    names(pct_anxious) <- c("PctAnxious")
    all_years <- cbind(all_years, pct_anxious)

    conf_intervals = mapply(
        function(x, y) {
            result = prop.test(x, x+y, conf.level = interval, correct = FALSE);
            result$conf.int
        },
        all_years$Anxious,
        all_years$NotAnxious)
    conf_intervals = as.data.frame(t(conf_intervals))
    colnames(conf_intervals) <- c("CI_lower", "CI_upper")
    conf_intervals["CI_upper"] <- conf_intervals["CI_upper"] * 100
    conf_intervals["CI_lower"] <- conf_intervals["CI_lower"] * 100

    all_years <- cbind(all_years, conf_intervals)
}

print(anxious_ci_by_year())
# print(anxious_ci_by_col("Age"))
print(anxious_ci_by_col("Grade"))
print(anxious_ci_by_col("Sex"))
print(anxious_ci_by_col("Race"))
print(anxious_ci_by_col("LGBQ"))
print(anxious_ci_by_col("Transgender"))



########################################################
# recode survey year for plots
dcya$Year[dcya$Year == 1] <- 2012
dcya$Year[dcya$Year == 4] <- 2015
dcya$Year[dcya$Year == 7] <- 2018


# merge pct and CI_lower & CI_upper for plots
plot_year <- merge(pct_anxious_wt, (anxious_ci_by_year()), by.x="Year", by.y="Year")
plot_year$CI_lower <- plot_year$CI_lower*100
plot_year$CI_upper <- plot_year$CI_upper*100
# convert year into continuous value between 2012 and 2018
plot_year$Year <- (as.integer(plot_year$Year) - 1) * 3 + 2012
plot_year

paper_theme <- theme_minimal() + theme(
	plot.margin = margin(t=5, b=5),
	plot.title = element_text(size = 8, face = "bold", hjust = 0.5, margin = margin(b=2)),
	panel.grid.minor.x = element_blank(),
	axis.text.x = element_text(size = 8, margin = margin(t=3)),
	axis.text.y = element_text(size = 8, margin = margin(r=3)),
	axis.title.x = element_text(size = 8, margin = margin(t=5)),
	axis.title.y = element_text(size = 8, margin = margin(t=5)),
	legend.position = "right",
	legend.background = element_rect(color = NA),
	legend.text = element_text(size = 8),
	legend.title = element_blank(),
	legend.key.width = unit(15, "pt"),
	legend.key.height = unit(10, "pt"))

# Data Visualization: plots figures
pdf("original_figures/Figure_1.pdf", colormodel = "cmyk")
p1 <- ggplot(data=plot_year, aes(x = Year, y = Pct)) +
    ggtitle("(a)") +
    geom_point(color = "blue") +
    geom_line(color = "blue") +
    #geom_ribbon(aes(ymin = CI_lower, ymax= CI_upper), fill = "blue", alpha = 0.3) +
    scale_x_continuous(name = "Aggregated", breaks = seq(2012, 2018, by = 3)) +
    scale_y_continuous(name = element_blank(), limits=c(0, 60)) +
    coord_fixed(ratio=0.0325) +
    paper_theme
p1
dev.off()

plot_grade = anxious_ci_by_col("Grade")

pdf("original_figures/Figure_2.pdf", colormodel = "cmyk")
p2 <- ggplot(data=plot_grade, aes(x = Year, y = PctAnxious, group = Grade, shape = Grade, color = Grade)) +
    ggtitle("(b)") +
    geom_point() +
    geom_line(aes(colour=Grade)) +
    #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = Grade), alpha = 0.3, colour = NA) +
    scale_x_continuous(name = "Grade", breaks = seq(2012, 2018, by = 3)) +
    scale_y_continuous(name = element_blank(), limits=c(0, 60)) +
    scale_colour_discrete(name = "Grade") +
    coord_fixed(ratio=0.07) +
    paper_theme
p2
dev.off()

plot_sex = anxious_ci_by_col("Sex")

pdf("original_figures/Figure_3.pdf", colormodel = "cmyk")
p3 <- ggplot(data=plot_sex, aes(x = Year, y = PctAnxious, group = Sex, shape = Sex, color = Sex)) +
    ggtitle("(c)") +
	geom_point() +
    geom_line(aes(colour=Sex)) +
    #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = Sex), alpha = 0.3, colour = NA) +
    scale_x_continuous(name = "Sex", breaks = seq(2012, 2018, by = 3)) +
    scale_y_continuous(name = element_blank(), limits=c(0, 60)) +
    scale_color_discrete(name = "Sex", labels = c("Male", "Female")) +
    scale_fill_discrete(name = "Sex", labels = c("Male", "Female")) +
    scale_shape_discrete(name = "Sex", labels = c("Male", "Female")) +
    coord_fixed(ratio=0.07) +
    paper_theme
p3
dev.off()

plot_race = anxious_ci_by_col("Race")

pdf("original_figures/Figure_4.pdf", colormodel = "cmyk")
p4 <- ggplot(data=plot_race, aes(x = Year, y = PctAnxious, group = Race, shape = Race, color = Race)) +
    ggtitle("(d)") +
    geom_point() +
    geom_line(aes(colour=Race)) +
    #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = Race), alpha = 0.3, colour = NA) +
    scale_x_continuous(name = "Race/Ethnicity", breaks = seq(2012, 2018, by = 3)) +
    scale_y_continuous(name = element_blank(), limits=c(0, 60)) +
    scale_color_discrete(name = "Race/Ethnicity", labels = c("White", "Black", "Latino", "Hmong", "Asian", "Native", "Middle Eastern", "Multi-racial")) +
    scale_shape_manual(name = "Race/Ethnicity", values = c(16,17,15,3,8,10,9,12), labels = c("White", "Black", "Latino", "Hmong", "Asian", "Native", "Middle Eastern", "Multi-racial")) +
    coord_fixed(ratio=0.07) +
    paper_theme
p4
dev.off() 


plot_LGBQ = anxious_ci_by_col("LGBQ")

pdf("original_figures/Figure_5.pdf", colormodel = "cmyk")
p5 <- ggplot(data=plot_LGBQ, aes(x = Year, y = PctAnxious, group = LGBQ, shape = LGBQ, color = LGBQ)) +
    ggtitle("(e)") +
    geom_point() +
    geom_line(aes(colour=LGBQ)) +
    #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = LGBQ), alpha = 0.3, colour = NA) +
    scale_x_continuous(name = "Sexual Orientation", breaks = seq(2012, 2018, by = 3)) +
    scale_y_continuous(name = element_blank(), limits=c(0, 80)) +
    scale_color_discrete(name = "Sexual Orientation", labels = c("Heterosexual", "Gay or Lesbian", "Bisexual", "Questioning")) +
    scale_shape_discrete(name = "Sexual Orientation", labels = c("Heterosexual", "Gay or Lesbian", "Bisexual", "Questioning")) +
    coord_fixed(ratio=0.07) +
    paper_theme
p5
dev.off()

plot_transgender = anxious_ci_by_col("Transgender")

pdf("original_figures/Figure_6.pdf", colormodel = "cmyk")
p6 <- ggplot(data=plot_transgender, aes(x = Year, y = PctAnxious, group = Transgender, shape = Transgender, color = Transgender)) +
    ggtitle("(f)") +
    geom_point() +
    geom_line(aes(colour=Transgender)) +
    #geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = Transgender), alpha = 0.3, colour = NA) +
    scale_x_continuous(name = "Gender Identity", breaks = seq(2012, 2018, by = 3)) +
    scale_y_continuous(name = element_blank(), limits=c(0, 80)) +
    scale_color_discrete(name = "Gender Identity", labels = c("Cisgender", "Transgender")) +
    scale_fill_discrete(name = "Gender Identity", labels = c("Cisgender", "Transgender")) +
    scale_shape_discrete(name = "Gender Identity", labels = c("Cisgender", "Transgender")) +
    coord_fixed(ratio=0.07) +
    paper_theme
p6
dev.off()


### combine plots #1-6 into one plot 
pdf("original_figures/Figure_combined.pdf", colormodel = "cmyk")
plots = plot_grid(
    plotlist = list(p1, p2, p3, p4, p5, p6),
    align = "hv",
    axis = "lr",
    nrow = 3,
    rel_heights = c(1/3, 1/3, 1/3))
ylabel = ggdraw() + draw_label("Percentage Meeting Anxiety Screening Criteria",
                               x=0, y=0.5, vjust=1.5, angle=90, size=10)
plot_grid(plotlist = list(ylabel, plots), rel_widths = c(1/25, 24/25))
dev.off()





########################################################
# Supplemental Analyses, removing grade 9

# filter out students in grade 9 in each survey year
dcya2 <- subset(dcya, Grade == "10" | Grade == "11" | Grade == "12")
dim(dcya2)
table(dcya2$Grade)

pct_anxious_wt = as.data.frame(prop.table(wtd.table(dcya2$Anxious, dcya2$Year, dcya2$Weighting), 2))
names(pct_anxious_wt) = c("Anxious", "Year", 'Pct')
pct_anxious_wt <- subset(pct_anxious_wt, Anxious == 1)
pct_anxious_wt$Pct <- pct_anxious_wt$Pct * 100
print(head(as.data.frame(pct_anxious_wt)))

# unadjusted model
# fit fixed-effect logistic regression to examine trend in anxiety prevalence between 2012-2018, unweighted binomial, unadjusted model
glm1 <- glm(Anxious~Year, family = "binomial", data=dcya2)
summary(glm1)

# fit fixed-effect logistic regression to examine trend in anxiety prevalence between 2012-2018, weighted binomial, unadjusted model
glm2 <- glm(Anxious~Year, weights = ScaledWeighting, family = "binomial", data=dcya2)
summary(glm2)

# fit fixed-effect logistic regression model, weighted quasibinomial, unadjusted model
glm3 <- glm(Anxious~Year, weights = ScaledWeighting, family = "quasibinomial", data=dcya2)
summary(glm3)

# manually calc default standard errors
# first examine variance-covariance matrix
vcov(glm3)
se_default <- sqrt(diag(vcov(glm3))) 
se_default

# calculate cluster-robust standard errors
# need to account for clustering in schools, use vcovCL
# clustered sandwich estimators are used to adjust inference when errors are correlated within (but not between) clusters
# we assume independence across schools
# vcovCL(x, cluster = variable indicating clustering of observations, sandwich = TRUE)
voc_cl <- vcovCL(glm3, cluster = ~SchoolID, sandwich = TRUE)
voc_cl # this gives you variance covariance matrix accouting for clustering
se_cl <-sqrt(diag(voc_cl)) 
se_cl # extract cluster robust standard errors

# coefficient tables
coeftest(glm3, voc_cl)
# compare to coefficient table with default SEs
coeftest(glm3)

# calculate OR and 95% CI manually
est <- exp(glm3$coef)
lower <- est-1.96*se_cl
upper <- est+1.96*se_cl
cbind(est, lower, upper)

# Adjusted Models
# fixed effect logistic regression to examine trend in anxiety prevalence between 2012-2018, unweighted binomial, adjusted model
m1_a <- glm(Anxious ~ Year + Grade + Race + Sex + LGBQ + Transgender + Depression, data = dcya2, family = binomial)
summary(m1_a)

# fixed effect logistic regression to examine trend in anxiety prevalence between 2012-2018, weighted binomial, adjusted model
m2_a <- glm(Anxious ~ Year + Grade + Race + Sex + LGBQ + Transgender + Depression, data = dcya2, family = binomial, weights = ScaledWeighting)
summary(m2_a)

# fixed effect logistic regression to examine trend in anxiety prevalence between 2012-2018, weighted quasibinomial, adjusted model
m3_a <- glm(Anxious ~ Year + Grade + Race + Sex + LGBQ + Transgender + Depression, data = dcya2, family = quasibinomial, weights = ScaledWeighting)
summary(m3_a)

### cluster robust SEs for ajusted model (m3_a)
# cluster standard errors by type
cluster_se_adj_model <- cluster.vcov(m3_a, ~SchoolID)
cluster_se_adj_model_extracted <- sqrt(diag(cluster_se_adj_model))
cluster_se_adj_model_extracted
coeftest(m3_a, cluster_se_adj_model)

# Report Adjusted Odds Ratio & 95% confidence interval for logistic regression
est <- exp(m3_a$coef)
lower.ci <- est-1.96*cluster_se_adj_model_extracted
upper.ci <- est+1.96*cluster_se_adj_model_extracted
cbind(est,lower.ci, upper.ci)

########################################################
# Stratified Regression Models
# Grade 
## Grades 10 - 12 should not be impacted by dropping grade 9 from analyses

# Sex
## Female
dcya_female = dcya2 %>%
	filter(Sex == "2_Female")
# unadjusted model
female_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_female)
summary(female_glm1)
cluster_se_female <- cluster.vcov(female_glm1, ~SchoolID)
cluster_se_female_extracted <- sqrt(diag(cluster_se_female))
coeftest(female_glm1, cluster_se_female)

# Odds Ratio and 95% CI
est <- exp(female_glm1$coef)
lower.ci <- est-1.96*cluster_se_female_extracted
upper.ci <- est+1.96*cluster_se_female_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
female_glm2 <- glm(Anxious ~ Year + Grade + Race + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_female)
summary(female_glm2)
cluster_se_female_a <- cluster.vcov(female_glm2, ~SchoolID)
cluster_se_female_a_extracted <- sqrt(diag(cluster_se_female_a))
coeftest(female_glm2, cluster_se_female_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(female_glm2$coef)
lower.ci <- est-1.96*cluster_se_female_a_extracted
upper.ci <- est+1.96*cluster_se_female_a_extracted
cbind(est,lower.ci, upper.ci)

## Male
dcya_male = dcya2 %>%
	filter(Sex == "1_Male")
# unadjusted model
male_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_male)
summary(male_glm1)
cluster_se_male <- cluster.vcov(male_glm1, ~SchoolID)
cluster_se_male_extracted <- sqrt(diag(cluster_se_male))
coeftest(male_glm1, cluster_se_male)

# Odds Ratio and 95% CI
est <- exp(male_glm1$coef)
lower.ci <- est-1.96*cluster_se_male_extracted
upper.ci <- est+1.96*cluster_se_male_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
male_glm2 <- glm(Anxious ~ Year + Grade + Race + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_male)
summary(male_glm2)
cluster_se_male_a <- cluster.vcov(male_glm2, ~SchoolID)
cluster_se_male_a_extracted <- sqrt(diag(cluster_se_male_a))
coeftest(male_glm2, cluster_se_male_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(male_glm2$coef)
lower.ci <- est-1.96*cluster_se_male_a_extracted
upper.ci <- est+1.96*cluster_se_male_a_extracted
cbind(est,lower.ci, upper.ci)


# Race/Ethnicity
## White
dcya_white = dcya2 %>%
	filter(Race == "1_White")
# unadjusted model
white_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_white)
summary(white_glm1)
cluster_se_white <- cluster.vcov(white_glm1, ~SchoolID)
cluster_se_white_extracted <- sqrt(diag(cluster_se_white))
coeftest(white_glm1, cluster_se_white)

# Odds Ratio and 95% CI
est <- exp(white_glm1$coef)
lower.ci <- est-1.96*cluster_se_white_extracted
upper.ci <- est+1.96*cluster_se_white_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
white_glm2 <- glm(Anxious ~ Year + Grade + Sex + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_white)
summary(white_glm2)
cluster_se_white_a <- cluster.vcov(white_glm2, ~SchoolID)
cluster_se_white_a_extracted <- sqrt(diag(cluster_se_white_a))
coeftest(white_glm2, cluster_se_white_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(white_glm2$coef)
lower.ci <- est-1.96*cluster_se_white_a_extracted
upper.ci <- est+1.96*cluster_se_white_a_extracted
cbind(est,lower.ci, upper.ci)


## Black
dcya_black = dcya2 %>%
	filter(Race == "2_Black")
# unadjusted model
black_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_black)
summary(black_glm1)
cluster_se_black <- cluster.vcov(black_glm1, ~SchoolID)
cluster_se_black_extracted <- sqrt(diag(cluster_se_black))
coeftest(black_glm1, cluster_se_black)

# Odds Ratio and 95% CI
est <- exp(black_glm1$coef)
lower.ci <- est-1.96*cluster_se_black_extracted
upper.ci <- est+1.96*cluster_se_black_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
black_glm2 <- glm(Anxious ~ Year + Grade + Sex + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_black)
summary(black_glm2)
cluster_se_black_a <- cluster.vcov(black_glm2, ~SchoolID)
cluster_se_black_a_extracted <- sqrt(diag(cluster_se_black_a))
coeftest(black_glm2, cluster_se_black_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(black_glm2$coef)
lower.ci <- est-1.96*cluster_se_black_a_extracted
upper.ci <- est+1.96*cluster_se_black_a_extracted
cbind(est,lower.ci, upper.ci)


## Latino
dcya_latino = dcya2 %>%
	filter(Race == "3_Latino")
# unadjusted model
latino_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_latino)
summary(latino_glm1)
cluster_se_latino <- cluster.vcov(latino_glm1, ~SchoolID)
cluster_se_latino_extracted <- sqrt(diag(cluster_se_latino))
coeftest(latino_glm1, cluster_se_latino)

# Odds Ratio and 95% CI
est <- exp(latino_glm1$coef)
lower.ci <- est-1.96*cluster_se_latino_extracted
upper.ci <- est+1.96*cluster_se_latino_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
latino_glm2 <- glm(Anxious ~ Year + Grade + Sex + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_latino)
summary(latino_glm2)
cluster_se_latino_a <- cluster.vcov(latino_glm2, ~SchoolID)
cluster_se_latino_a_extracted <- sqrt(diag(cluster_se_latino_a))
coeftest(latino_glm2, cluster_se_latino_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(latino_glm2$coef)
lower.ci <- est-1.96*cluster_se_latino_a_extracted
upper.ci <- est+1.96*cluster_se_latino_a_extracted
cbind(est,lower.ci, upper.ci)


## Hmong
dcya_hmong = dcya2 %>%
	filter(Race == "4_Hmong")
# unadjusted model
hmong_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_hmong)
summary(hmong_glm1)
cluster_se_hmong <- cluster.vcov(hmong_glm1, ~SchoolID)
cluster_se_hmong_extracted <- sqrt(diag(cluster_se_hmong))
coeftest(hmong_glm1, cluster_se_hmong)

# Odds Ratio and 95% CI
est <- exp(hmong_glm1$coef)
lower.ci <- est-1.96*cluster_se_hmong_extracted
upper.ci <- est+1.96*cluster_se_hmong_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
hmong_glm2 <- glm(Anxious ~ Year + Grade + Sex + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_hmong)
summary(hmong_glm2)
cluster_se_hmong_a <- cluster.vcov(hmong_glm2, ~SchoolID)
cluster_se_hmong_a_extracted <- sqrt(diag(cluster_se_hmong_a))
coeftest(hmong_glm2, cluster_se_hmong_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(hmong_glm2$coef)
lower.ci <- est-1.96*cluster_se_hmong_a_extracted
upper.ci <- est+1.96*cluster_se_hmong_a_extracted
cbind(est,lower.ci, upper.ci)


## Asian
dcya_asian = dcya2 %>%
	filter(Race == "5_Asian")
# unadjusted model
asian_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_asian)
summary(asian_glm1)
cluster_se_asian <- cluster.vcov(asian_glm1, ~SchoolID)
cluster_se_asian_extracted <- sqrt(diag(cluster_se_asian))
coeftest(asian_glm1, cluster_se_asian)

# Odds Ratio and 95% CI
est <- exp(asian_glm1$coef)
lower.ci <- est-1.96*cluster_se_asian_extracted
upper.ci <- est+1.96*cluster_se_asian_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
asian_glm2 <- glm(Anxious ~ Year + Grade + Sex + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_asian)
summary(asian_glm2)
cluster_se_asian_a <- cluster.vcov(asian_glm2, ~SchoolID)
cluster_se_asian_a_extracted <- sqrt(diag(cluster_se_asian_a))
coeftest(asian_glm2, cluster_se_asian_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(asian_glm2$coef)
lower.ci <- est-1.96*cluster_se_asian_a_extracted
upper.ci <- est+1.96*cluster_se_asian_a_extracted
cbind(est,lower.ci, upper.ci)


## Native
dcya_native = dcya2 %>%
	filter(Race == "6_Native")
# unadjusted model
native_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_native)
summary(native_glm1)
cluster_se_native <- cluster.vcov(native_glm1, ~SchoolID)
cluster_se_native_extracted <- sqrt(diag(cluster_se_native))
coeftest(native_glm1, cluster_se_native)

# Odds Ratio and 95% CI
est <- exp(native_glm1$coef)
lower.ci <- est-1.96*cluster_se_native_extracted
upper.ci <- est+1.96*cluster_se_native_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
native_glm2 <- glm(Anxious ~ Year + Grade + Sex + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_native)
summary(native_glm2)
cluster_se_native_a <- cluster.vcov(native_glm2, ~SchoolID)
cluster_se_native_a_extracted <- sqrt(diag(cluster_se_native_a))
coeftest(native_glm2, cluster_se_native_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(native_glm2$coef)
lower.ci <- est-1.96*cluster_se_native_a_extracted 
upper.ci <- est+1.96*cluster_se_native_a_extracted 
cbind(est,lower.ci, upper.ci)


## Middle Eastern
dcya_me = dcya2 %>%
	filter(Race == "7_Middle Eastern")
# unadjusted model
me_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_me)
summary(me_glm1)
cluster_se_me <- cluster.vcov(me_glm1, ~SchoolID)
cluster_se_me_extracted <- sqrt(diag(cluster_se_me))
coeftest(me_glm1, cluster_se_me)

# Odds Ratio and 95% CI
est <- exp(me_glm1$coef)
lower.ci <- est-1.96*cluster_se_me_extracted
upper.ci <- est+1.96*cluster_se_me_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
me_glm2 <- glm(Anxious ~ Year + Grade + Sex + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_me)
summary(me_glm2)
cluster_se_me_a <- cluster.vcov(me_glm2, ~SchoolID)
cluster_se_me_a_extracted <- sqrt(diag(cluster_se_me_a))
coeftest(me_glm2, cluster_se_me_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(me_glm2$coef)
lower.ci <- est-1.96*cluster_se_me_a_extracted
upper.ci <- est+1.96*cluster_se_me_a_extracted
cbind(est,lower.ci, upper.ci)


## Multi-racial
dcya_mr = dcya2 %>%
	filter(Race == "8_Multi-racial")
# unadjusted model
mr_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_mr)
summary(mr_glm1)
cluster_se_mr <- cluster.vcov(mr_glm1, ~SchoolID)
cluster_se_mr_extracted <- sqrt(diag(cluster_se_mr))
coeftest(mr_glm1, cluster_se_mr)

# Odds Ratio and 95% CI
est <- exp(mr_glm1$coef)
lower.ci <- est-1.96*cluster_se_mr_extracted
upper.ci <- est+1.96*cluster_se_mr_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
mr_glm2 <- glm(Anxious ~ Year + Grade + Sex + LGBQ + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_mr)
summary(mr_glm2)
cluster_se_mr_a <- cluster.vcov(mr_glm2, ~SchoolID)
cluster_se_mr_a_extracted <- sqrt(diag(cluster_se_mr_a))
coeftest(mr_glm2, cluster_se_mr_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(mr_glm2$coef)
lower.ci <- est-1.96*cluster_se_mr_a_extracted
upper.ci <- est+1.96*cluster_se_mr_a_extracted
cbind(est,lower.ci, upper.ci)


# Sexual Orientation
## Straight
dcya_straight = dcya2 %>%
	filter(LGBQ == "1_Straight")
# unadjusted model
straight_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_straight)
summary(straight_glm1)
cluster_se_straight <- cluster.vcov(straight_glm1, ~SchoolID)
cluster_se_straight_extracted <- sqrt(diag(cluster_se_straight))
coeftest(straight_glm1, cluster_se_straight)

# Odds Ratio and 95% CI
est <- exp(straight_glm1$coef)
lower.ci <- est-1.96*cluster_se_straight_extracted
upper.ci <- est+1.96*cluster_se_straight_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
straight_glm2 <- glm(Anxious ~ Year + Grade + Sex + Race + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_straight)
summary(straight_glm2)
cluster_se_straight_a <- cluster.vcov(straight_glm2, ~SchoolID)
cluster_se_straight_a_extracted <- sqrt(diag(cluster_se_straight_a))
coeftest(straight_glm2, cluster_se_straight_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(straight_glm2$coef)
lower.ci <- est-1.96*cluster_se_straight_a_extracted
upper.ci <- est+1.96*cluster_se_straight_a_extracted
cbind(est,lower.ci, upper.ci)

## Gay or Lesbian
dcya_gay = dcya2 %>%
	filter(LGBQ == "2_Gay")
# unadjusted model
gay_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_gay)
summary(gay_glm1)
cluster_se_gay <- cluster.vcov(gay_glm1, ~SchoolID)
cluster_se_gay_extracted <- sqrt(diag(cluster_se_gay))
coeftest(gay_glm1, cluster_se_gay)

# Odds Ratio and 95% CI
est <- exp(gay_glm1$coef)
lower.ci <- est-1.96*cluster_se_gay_extracted
upper.ci <- est+1.96*cluster_se_gay_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
gay_glm2 <- glm(Anxious ~ Year + Grade + Sex + Race + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_gay)
summary(gay_glm2)
cluster_se_gay_a <- cluster.vcov(gay_glm2, ~SchoolID)
cluster_se_gay_a_extracted <- sqrt(diag(cluster_se_gay_a))
coeftest(gay_glm2, cluster_se_gay_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(gay_glm2$coef)
lower.ci <- est-1.96*cluster_se_gay_a_extracted
upper.ci <- est+1.96*cluster_se_gay_a_extracted
cbind(est,lower.ci, upper.ci)


## Bisexual
dcya_bisexual = dcya2 %>%
	filter(LGBQ == "3_Bisexual")
# unadjusted model
bisexual_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_bisexual)
summary(bisexual_glm1)
cluster_se_bisexual <- cluster.vcov(bisexual_glm1, ~SchoolID)
cluster_se_bisexual_extracted <- sqrt(diag(cluster_se_bisexual))
coeftest(bisexual_glm1, cluster_se_bisexual)

# Odds Ratio and 95% CI
est <- exp(bisexual_glm1$coef)
lower.ci <- est-1.96*cluster_se_bisexual_extracted
upper.ci <- est+1.96*cluster_se_bisexual_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
bisexual_glm2 <- glm(Anxious ~ Year + Grade + Sex + Race + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_bisexual)
summary(bisexual_glm2)
cluster_se_bisexual_a <- cluster.vcov(bisexual_glm2, ~SchoolID)
cluster_se_bisexual_a_extracted <- sqrt(diag(cluster_se_bisexual_a))
coeftest(bisexual_glm2, cluster_se_bisexual_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(bisexual_glm2$coef)
lower.ci <- est-1.96*cluster_se_bisexual_a_extracted
upper.ci <- est+1.96*cluster_se_bisexual_a_extracted
cbind(est,lower.ci, upper.ci)


## Questioning
dcya_questioning = dcya2 %>%
	filter(LGBQ == "4_Questioning")
# unadjusted model
questioning_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_questioning)
summary(questioning_glm1)
cluster_se_questioning <- cluster.vcov(questioning_glm1, ~SchoolID)
cluster_se_questioning_extracted <- sqrt(diag(cluster_se_questioning))
coeftest(questioning_glm1, cluster_se_questioning)

# Odds Ratio and 95% CI
est <- exp(questioning_glm1$coef)
lower.ci <- est-1.96*cluster_se_questioning_extracted
upper.ci <- est+1.96*cluster_se_questioning_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
questioning_glm2 <- glm(Anxious ~ Year + Grade + Sex + Race + Transgender + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_questioning)
summary(questioning_glm2)
cluster_se_questioning_a <- cluster.vcov(questioning_glm2, ~SchoolID)
cluster_se_questioning_a_extracted <- sqrt(diag(cluster_se_questioning_a))
coeftest(questioning_glm2, cluster_se_questioning_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(questioning_glm2$coef)
lower.ci <- est-1.96*cluster_se_questioning_a_extracted
upper.ci <- est+1.96*cluster_se_questioning_a_extracted
cbind(est,lower.ci, upper.ci)



# Gender Identity
## Cisgender
dcya_cisgender = dcya2 %>%
	filter(Transgender == "1_Cisgender")
# unadjusted model
cisgender_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_cisgender)
summary(cisgender_glm1)
cluster_se_cisgender <- cluster.vcov(cisgender_glm1, ~SchoolID)
cluster_se_cisgender_extracted <- sqrt(diag(cluster_se_cisgender))
coeftest(cisgender_glm1, cluster_se_cisgender)

# Odds Ratio and 95% CI
est <- exp(cisgender_glm1$coef)
lower.ci <- est-1.96*cluster_se_cisgender_extracted
upper.ci <- est+1.96*cluster_se_cisgender_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
cisgender_glm2 <- glm(Anxious ~ Year + Grade + Sex + Race + LGBQ + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_cisgender)
summary(cisgender_glm2)
cluster_se_cisgender_a <- cluster.vcov(cisgender_glm2, ~SchoolID)
cluster_se_cisgender_a_extracted <- sqrt(diag(cluster_se_cisgender_a))
coeftest(cisgender_glm2, cluster_se_cisgender_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(cisgender_glm2$coef)
lower.ci <- est-1.96*cluster_se_cisgender_a_extracted
upper.ci <- est+1.96*cluster_se_cisgender_a_extracted
cbind(est,lower.ci, upper.ci)

## Transgender
dcya_transgender = dcya2 %>%
	filter(Transgender == "2_Transgender")
# unadjusted model
transgender_glm1 <- glm(Anxious ~ Year, weights = ScaledWeighting, family = "quasibinomial", data = dcya_transgender)
summary(transgender_glm1)
cluster_se_transgender <- cluster.vcov(transgender_glm1, ~SchoolID)
cluster_se_transgender_extracted <- sqrt(diag(cluster_se_transgender))
coeftest(transgender_glm1, cluster_se_transgender)

# Odds Ratio and 95% CI
est <- exp(transgender_glm1$coef)
lower.ci <- est-1.96*cluster_se_transgender_extracted
upper.ci <- est+1.96*cluster_se_transgender_extracted
cbind(est,lower.ci, upper.ci)

# adjusted model
transgender_glm2 <- glm(Anxious ~ Year + Grade + Sex + Race + LGBQ + Depression, weights = ScaledWeighting, family = "quasibinomial", data = dcya_transgender)
summary(transgender_glm2)
cluster_se_transgender_a <- cluster.vcov(transgender_glm2, ~SchoolID)
cluster_se_transgender_a_extracted <- sqrt(diag(cluster_se_transgender_a))
coeftest(transgender_glm2, cluster_se_transgender_a)

# Adjusted Odds Ratio and 95% CI
est <- exp(transgender_glm2$coef)
lower.ci <- est-1.96*cluster_se_transgender_a_extracted
upper.ci <- est+1.96*cluster_se_transgender_a_extracted
cbind(est,lower.ci, upper.ci)



########################################################
# Differential time trends: Logistic regressions with two-way interaction of year x each demographic variable
# run exact same adjusted model, and add the interaction term
# don't interpret main effects, just write about interaction term 

glm_grade <- glm(Anxious ~ Race + Sex + LGBQ + Transgender + Depression + Year*Grade, weights = ScaledWeighting, family = "quasibinomial", data=dcya2)
summary(glm_grade)
# cluster standard errors by type
cluster_se_dt1 <- cluster.vcov(glm_grade, ~SchoolID)
coeftest(glm_grade, cluster_se_dt1)


glm_sex <- glm(Anxious ~ Grade + Race + LGBQ + Transgender + Depression + Year*Sex, weights = ScaledWeighting, family = "quasibinomial", data=dcya2)
summary(glm_sex)
# cluster standard errors by type
cluster_se_dt2 <- cluster.vcov(glm_sex, ~SchoolID)
coeftest(glm_sex, cluster_se_dt2)

glm_race <- glm(Anxious ~ Grade + Sex + LGBQ + Transgender + Depression + Year*Race, weights = ScaledWeighting, family = "quasibinomial", data=dcya2)
summary(glm_race)
# cluster standard errors by type
cluster_se_dt3 <- cluster.vcov(glm_race, ~SchoolID)
coeftest(glm_race, cluster_se_dt3)

glm_lgbq <- glm(Anxious ~ Grade + Race + Sex + Transgender + Depression + Year*LGBQ, weights = ScaledWeighting, family = "quasibinomial", data=dcya2)
summary(glm_lgbq)
# cluster standard errors by type
cluster_se_dt4 <- cluster.vcov(glm_lgbq, ~SchoolID)
coeftest(glm_lgbq, cluster_se_dt4)

glm_transgender <- glm(Anxious ~ Year + Grade + Race + Sex + LGBQ + Depression + Year*Transgender, weights = ScaledWeighting, family = "quasibinomial", data=dcya2)
summary(glm_transgender)
# cluster standard errors by type
cluster_se_dt5 <- cluster.vcov(glm_transgender, ~SchoolID)
coeftest(glm_transgender, cluster_se_dt5)


########################################################

# Save each year as a separate dataframe for sanity checking
dcya_2012 <- subset(dcya2, Year == 2012)
dcya_2015 <- subset(dcya2, Year == 2015)
dcya_2018 <- subset(dcya2, Year == 2018)


# Computes the percentage of anxious students, stratified by the given column
anxious_by_col <- function(col_name) {
    # make separate dataframes for each year
    year_2012 = subset(as.data.frame(prop.table(wtd.table(dcya_2012$Anxious, dcya_2012[, col_name], dcya_2012$Weighting), 2)), Var1 == 1)
    year_2015 = subset(as.data.frame(prop.table(wtd.table(dcya_2015$Anxious, dcya_2015[, col_name], dcya_2015$Weighting), 2)), Var1 == 1)
    year_2018 = subset(as.data.frame(prop.table(wtd.table(dcya_2018$Anxious, dcya_2018[, col_name], dcya_2018$Weighting), 2)), Var1 == 1)

    # repurpose a column and store the year
    year_2012$Var1 <- 2012
    year_2015$Var1 <- 2015
    year_2018$Var1 <- 2018

    # combine into a single dataframe
    all_years = rbind(year_2012, year_2015, year_2018)
    names(all_years) <- c("Year", col_name, "PctAnxious")
    rownames(all_years) <- NULL
    all_years$PctAnxious <- all_years$PctAnxious * 100
    all_years
}

# Computes the percentage of anxious students, stratified by the given column
# by_age <- anxious_by_col("Age")
# print(by_age)

by_grade = anxious_by_col("Grade")
print(by_grade)

by_sex = anxious_by_col("Sex")
print(by_sex)

by_race = anxious_by_col("Race")
print(by_race)

by_LGBQ = anxious_by_col("LGBQ")
print(by_LGBQ)

by_transgender = anxious_by_col("Transgender")
print(by_transgender)

# Computes the confidence interval of anxious students, stratified by the year
anxious_ci_by_year <- function(interval = 0.95) {
	ci_by_year = as.data.frame.matrix(t(wtd.table(dcya2$Anxious, dcya2$Year, dcya2$Weighting))[, c(2,1)])
	ci_by_year <- data.frame(Year = row.names(ci_by_year), ci_by_year, row.names = NULL)
	names(ci_by_year) <- c("Year", "Anxious", "NotAnxious")
	conf_intervals = mapply(
	      function(x, y) {
	            result = prop.test(x, x+y, conf.level = interval, correct = FALSE);
	            result$conf.int
	        },
	        ci_by_year$Anxious,
	        ci_by_year$NotAnxious)
	conf_intervals = as.data.frame(t(conf_intervals))
	colnames(conf_intervals) <- c("CI_lower", "CI_upper")
	ci_by_year <- cbind(ci_by_year, conf_intervals)
}

# Computes the confidence interval of anxious students, stratified by the given column
anxious_ci_by_col <- function(col_name, interval = 0.95) {
    year_2012 = t(wtd.table(dcya_2012$Anxious, dcya_2012[, col_name], dcya_2012$Weighting))
    year_2012 <- as.data.frame.matrix(year_2012[, c(2,1)])
    year_2012 <- data.frame(Year = 2012, Col = row.names(year_2012), year_2012, row.names = NULL)

    year_2015 = t(wtd.table(dcya_2015$Anxious, dcya_2015[, col_name], dcya_2015$Weighting))
    year_2015 <- as.data.frame.matrix(year_2015[, c(2,1)])
    year_2015 <- data.frame(Year = 2015, Col = row.names(year_2015), year_2015, row.names = NULL)

    year_2018 = t(wtd.table(dcya_2018$Anxious, dcya_2018[, col_name], dcya_2018$Weighting))
    year_2018 <- as.data.frame.matrix(year_2018[, c(2,1)])
    year_2018 <- data.frame(Year = 2018, Col = row.names(year_2018), year_2018, row.names = NULL)

    all_years = rbind(year_2012, year_2015, year_2018)
    names(all_years) <- c("Year", col_name, "Anxious", "NotAnxious")

    conf_intervals = mapply(
        function(x, y) {
            result = prop.test(x, x+y, conf.level = interval, correct = FALSE);
            result$conf.int
        },
        all_years$Anxious,
        all_years$NotAnxious)
    conf_intervals = as.data.frame(t(conf_intervals))
    colnames(conf_intervals) <- c("CI_lower", "CI_upper")

    all_years <- cbind(all_years, conf_intervals)
}

print(anxious_ci_by_year())
# print(anxious_ci_by_col("Age"))
print(anxious_ci_by_col("Grade"))
print(anxious_ci_by_col("Sex"))
print(anxious_ci_by_col("Race"))
print(anxious_ci_by_col("LGBQ"))
print(anxious_ci_by_col("Transgender"))


##########################################################################################################################
# 5/7/21 Revisions
# compare survey year as linear term trend vs. as categorical predictor
# recode survey year for regression analyses
dcya$Year[dcya$Year == 2012] <- 1
dcya$Year[dcya$Year == 2015] <- 4
dcya$Year[dcya$Year == 2018] <- 7

# original, as linear term trend 
glm2 <- glm(Anxious~Year, weights = ScaledWeighting, family = "binomial", data=dcya)
summary(glm2)

glm3 <- glm(Anxious~Year, weights = ScaledWeighting, family = "quasibinomial", data=dcya)
summary(glm3)


dcya$Year_cat <- as.factor(dcya$Year) # levels: 1 (2012 = referent), 4, 7
glm2_cat <- glm(Anxious~Year_cat, weights = ScaledWeighting, family = "binomial", data=dcya)
summary(glm2_cat) 

### unadjusted model comparison, glm2 = year as continuous, glm2_cat = year as categorical
BIC(glm2, glm2_cat)


### adjusted model comparison
m2_adj <- glm(Anxious ~ Year + Grade + Race + Sex + LGBQ + Transgender + Depression, data = dcya, family = binomial, weights = ScaledWeighting)
summary(m2_adj)

m2_adj_cat <- glm(Anxious ~ Year_cat + Grade + Race + Sex + LGBQ + Transgender + Depression, data = dcya, family = binomial, weights = ScaledWeighting)
summary(m2_adj_cat)

# adjusted model
BIC(m2_adj, m2_adj_cat)




