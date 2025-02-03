library(MatchIt)
library(markdown)
library(oddsratio)
library(tidyverse)
library(dplyr)
library(forestplot)
library(ggplot2)
library(coefplot)
library(data.table)

df <- fread(file = "path_to_your_datafile")
df[df == ""] <- NA

colnames(df)
setnames(df, "T1D_fp", "T1D")
setnames(df, "T2D_fp", "T2D")

#set correct structure
df$cAMP_LoF <- as.factor(df$cAMP_LoF)
df$G40S <- as.factor(df$G40S)
df$frameshift <- as.factor(df$frameshift)
df$variant_type <- as.factor(df$variant_type)
df$T2D <- as.factor(df$T2D)
df$sex <- as.factor(df$sex)
df$age_2023 <- as.numeric(df$age_2023)
df$demens_diseases <- as.factor(df$demens_diseases)
df$neuro_diseases <- as.factor(df$neuro_diseases)
df$alzheimer <- as.factor(df$alzheimer)

#the G40S column consists of hetero (1) and homozygous (2) carriers. Pool these participants. 
# mutate the df so 1+2 are just 1
df <- df %>% 
  mutate(G40S = as.character(G40S)) %>%  # Convert factor to character for manipulation
  mutate(G40S = ifelse(G40S %in% c("1", "2"), "1", G40S)) %>%  # Pool 1 and 2 into 1
  mutate(G40S = factor(G40S))  # Convert back to factor
table(df$G40S)

#### Data check ####
count(df, cAMP_LoF)
count(df, frameshift)
count(df, G40S)
#Same amount of controls in all three

#Are the variant types mutually exclusive? 
#Find common cases
#change structure to character for manipulation
df$cAMP_LoF <- as.character(df$cAMP_LoF)
df$G40S <- as.character(df$G40S)
df$frameshift <- as.character(df$frameshift)
# Check for common cases labeled as 1 between cAMP_LoF and G40S
common_cases <- intersect(which(df$cAMP_LoF == 1), which(df$G40S == "1"))
# Print the common cases, if any
if (length(common_cases) > 0) {
  cat("Common cases (1) between cAMP_LoF and G40S:", common_cases, "\n")
} else {
  cat("No common cases (1) between cAMP_LoF and G40S.\n")
}
#3 individuals have both G40S and cAMP_LoF
#Change back to factor
df$cAMP_LoF <- as.factor(df$cAMP_LoF)
df$G40S <- as.factor(df$G40S)
df$frameshift <- as.factor(df$frameshift)
#### end ####


#### cAMP LoF ####
table(df$cAMP_LoF)
# Remove rows with missing or non-finite values in age etc.
df_cAMP_demens <- df[complete.cases(df$age_2023, df$sex, df$cAMP_LoF, df$demens_diseases), ] 
table(df_cAMP_demens$cAMP_LoF)
#make 2-by-2 table
count(df_cAMP_demens, cAMP_LoF)
count(df_cAMP_demens, demens_diseases, cAMP_LoF)
7623/395452*100
3/113*100
# Calculate prevalence of demens_diseases by cAMP_LoF category
prevalence_cAMP <- df_cAMP_demens %>%
  group_by(cAMP_LoF) %>%
  summarise(prevalence_demens = mean(demens_diseases == 1) * 100)

# Print the results
print(prevalence_cAMP)

#Descriptives for sex, age and T2D
#Finding the %, mean and SD for the baseline characteristics
baseline_cAMP <- cbind(
  T2D = c(
    sum(df_cAMP_demens$T2D == 1 & df_cAMP_demens$cAMP_LoF == 0, na.rm = TRUE) / sum(df_cAMP_demens$cAMP_LoF == 0, na.rm = TRUE) * 100,
    sum(df_cAMP_demens$T2D == 1 & df_cAMP_demens$cAMP_LoF == 1, na.rm = TRUE) / sum(df_cAMP_demens$cAMP_LoF == 1, na.rm = TRUE) * 100
  ),
  sex = c(
    sum(df_cAMP_demens$sex == "Female" & df_cAMP_demens$cAMP_LoF == 0, na.rm = TRUE) / sum(df_cAMP_demens$cAMP_LoF == 0, na.rm = TRUE) * 100,
    sum(df_cAMP_demens$sex == "Female" & df_cAMP_demens$cAMP_LoF == 1, na.rm = TRUE) / sum(df_cAMP_demens$cAMP_LoF == 1, na.rm = TRUE) * 100
  ),
  age_median = c(
    median(df_cAMP_demens$age_2023[df_cAMP_demens$cAMP_LoF == 0], na.rm = TRUE),
    median(df_cAMP_demens$age_2023[df_cAMP_demens$cAMP_LoF == 1], na.rm = TRUE)
  ),
  age_Q1 = c(
    quantile(df_cAMP_demens$age_2023[df_cAMP_demens$cAMP_LoF == 0], probs = 0.25, na.rm = TRUE),
    quantile(df_cAMP_demens$age_2023[df_cAMP_demens$cAMP_LoF == 1], probs = 0.25, na.rm = TRUE)
  ),
  age_Q3 = c(
    quantile(df_cAMP_demens$age_2023[df_cAMP_demens$cAMP_LoF == 0], probs = 0.75, na.rm = TRUE),
    quantile(df_cAMP_demens$age_2023[df_cAMP_demens$cAMP_LoF == 1], probs = 0.75, na.rm = TRUE)
  )
)

baseline_cAMP

#glm
glm_camp <- glm(demens_diseases ~ cAMP_LoF, data = df_cAMP_demens, family = "binomial")
summary(glm_camp)
confint(glm_camp)
print(nobs(glm_camp))
#odds ratio
odds_ratio_table_cAMP <- exp(coef(glm_camp)) #yields the odds ratios (ORs) for each predictor variable in the model
print(odds_ratio_table_cAMP) #OR of prevelant dementia
exp(confint.default(glm_camp)) # 95% CI

#multiple glm
multiple_glm_camp <- glm(demens_diseases ~ cAMP_LoF + age_2023 + sex, data = df_cAMP_demens, family = "binomial")
summary(multiple_glm_camp)
confint(multiple_glm_camp)
#odds ratio
odds_ratio_table_cAMP <- exp(coef(multiple_glm_camp)) #yields the odds ratios (ORs) for each predictor variable in the model
print(odds_ratio_table_cAMP) #OR of prevelant dementia
exp(confint.default(multiple_glm_camp)) # 95% CI
print(nrow(df_cAMP_demens))

table_cAMP <- table(df_cAMP_demens$cAMP_LoF, df_cAMP_demens$demens_diseases)
# Perform Chi-square test for independence
chisq_test <- chisq.test(table_cAMP)
# Print the results
print(chisq_test)
#### end####



#### G40S pooled ####
# Remove rows with missing or non-finite values for age, sex, demens_diseases and G40S
df_G40S_demens <- df[complete.cases(df$age_2023,df$sex, df$demens_diseases, df$G40S), ]

#make 2-by-2 table
count(df_G40S_demens, G40S)
count(df_G40S_demens, demens_diseases, G40S)
7623/395452*100
119/6924*100
# Calculate prevalence of demens_diseases by G40S category
prevalence_G40S <- df_G40S_demens %>%
  group_by(G40S) %>%
  summarise(prevalence_demens = mean(demens_diseases == 1) * 100)
# Print the results
print(prevalence_G40S)

#Finding the mean and SD for the patient characteristics
baseline_G40S <- cbind(
  T2D = c(
    sum(df_G40S_demens$T2D == 1 & df_G40S_demens$G40S == 0, na.rm = TRUE) / sum(df_G40S_demens$G40S == 0, na.rm = TRUE) * 100,
    sum(df_G40S_demens$T2D == 1 & df_G40S_demens$G40S == 1, na.rm = TRUE) / sum(df_G40S_demens$G40S == 1, na.rm = TRUE) * 100
  ),
  sex = c(
    sum(df_G40S_demens$sex == "Female" & df_G40S_demens$G40S == 0, na.rm = TRUE) / sum(df_G40S_demens$G40S == 0, na.rm = TRUE) * 100,
    sum(df_G40S_demens$sex == "Female" & df_G40S_demens$G40S == 1, na.rm = TRUE) / sum(df_G40S_demens$G40S == 1, na.rm = TRUE) * 100
  ),
  age_median = c(
    median(df_G40S_demens$age_2023[df_G40S_demens$G40S == 0], na.rm = TRUE),
    median(df_G40S_demens$age_2023[df_G40S_demens$G40S == 1], na.rm = TRUE)
  ),
  age_Q1 = c(
    quantile(df_G40S_demens$age_2023[df_G40S_demens$G40S == 0], probs = 0.25, na.rm = TRUE),
    quantile(df_G40S_demens$age_2023[df_G40S_demens$G40S == 1], probs = 0.25, na.rm = TRUE)
  ),
  age_Q3 = c(
    quantile(df_G40S_demens$age_2023[df_G40S_demens$G40S == 0], probs = 0.75, na.rm = TRUE),
    quantile(df_G40S_demens$age_2023[df_G40S_demens$G40S == 1], probs = 0.75, na.rm = TRUE)
  )
)
baseline_G40S

# glm
glm_G40S <- glm(demens_diseases ~ G40S, data = df_G40S_demens, family = "binomial")
summary(glm_G40S)
confint(glm_G40S)
#odds ratio
odds_ratio_table_G40S <- exp(coef(glm_G40S)) #yields the odds ratios (ORs) for each predictor variable in the model
print(odds_ratio_table_G40S) #OR of prevelant dementia
exp(confint.default(glm_G40S)) # 95% CI
print(nrow(df_G40S_demens))

#multiple glm  
multiple_glm_G40S <- glm(demens_diseases ~ G40S + age_2023 + sex, data = df_G40S_demens, family = "binomial")
summary(multiple_glm_G40S)
confint(multiple_glm_G40S)
#odds
odds_ratio_table_G40S_multi <- exp(coef(multiple_glm_G40S)) #yields the odds ratios (ORs) for each predictor variable in the model
print(odds_ratio_table_G40S_multi) #OR of prevelant dementia
exp(confint.default(multiple_glm_G40S)) # 95% CI

print(nobs(multiple_glm_G40S))
#### end ####


#### frameshift ####
table(df$frameshift)
# Remove rows with missing or non-finite values for age, sex, frameshift and demens_diseases
df_frameshift <- df[complete.cases(df$age_2023, df$sex, df$frameshift, df$demens_diseases), ]
#make 2-by-2 table
count(df_frameshift, frameshift)
count(df_frameshift, demens_diseases, frameshift)
7623/395452*100
3/249*100
# Calculate prevalence of demens_diseases by G40S category
prevalence_frameshift <- df_frameshift %>%
  group_by(frameshift) %>%
  summarise(prevalence_demens = mean(demens_diseases == 1) * 100)
# Print the results
print(prevalence_frameshift)

#Finding the %, mean and SD for the baseline characteristics
baseline_frameshift <- cbind(
  T2D = c(
    sum(df_frameshift$T2D == 1 & df_frameshift$frameshift == 0, na.rm = TRUE) / sum(df_frameshift$frameshift == 0, na.rm = TRUE) * 100,
    sum(df_frameshift$T2D == 1 & df_frameshift$frameshift == 1, na.rm = TRUE) / sum(df_frameshift$frameshift == 1, na.rm = TRUE) * 100
  ),
  sex = c(
    sum(df_frameshift$sex == "Female" & df_frameshift$frameshift == 0, na.rm = TRUE) / sum(df_frameshift$frameshift == 0, na.rm = TRUE) * 100,
    sum(df_frameshift$sex == "Female" & df_frameshift$frameshift == 1, na.rm = TRUE) / sum(df_frameshift$frameshift == 1, na.rm = TRUE) * 100
  ),
  age_median = c(
    median(df_frameshift$age_2023[df_frameshift$frameshift == 0], na.rm = TRUE),
    median(df_frameshift$age_2023[df_frameshift$frameshift == 1], na.rm = TRUE)
  ),
  age_Q1 = c(
    quantile(df_frameshift$age_2023[df_frameshift$frameshift == 0], probs = 0.25, na.rm = TRUE),
    quantile(df_frameshift$age_2023[df_frameshift$frameshift == 1], probs = 0.25, na.rm = TRUE)
  ),
  age_Q3 = c(
    quantile(df_frameshift$age_2023[df_frameshift$frameshift == 0], probs = 0.75, na.rm = TRUE),
    quantile(df_frameshift$age_2023[df_frameshift$frameshift == 1], probs = 0.75, na.rm = TRUE)
  )
)

baseline_frameshift

# glm
glm_frameshift <- glm(demens_diseases ~ frameshift, data = df_frameshift, family = "binomial")
summary(glm_frameshift)
confint(glm_frameshift)
print(nrow(df_frameshift))
#odds ratio
odds_ratio_table_frameshift <- exp(coef(glm_frameshift)) #yields the odds ratios (ORs) for each predictor variable in the model
print(odds_ratio_table_frameshift) #OR of prevelant dementia
exp(confint.default(glm_frameshift)) # 95% CI

#multiple glm
multiple_glm_frameshift <- glm(demens_diseases ~ frameshift + age_2023 + sex, data = df_frameshift, family = "binomial")
summary(multiple_glm_frameshift)
confint(multiple_glm_frameshift)
#odds ratio
odds_ratio_table_frameshift <- exp(coef(multiple_glm_frameshift)) #yields the odds ratios (ORs) for each predictor variable in the model
print(odds_ratio_table_frameshift) #OR of prevelant dementia
exp(confint.default(multiple_glm_frameshift)) # 95% CI
####end####




#variants in alzheimers disease 

#### cAMP Alzheimer ####
# Remove rows with missing or non-finite values in age, sex, alzheimer and cAMP_LoF
df_alzheimer_nomissing_cAMP <- df[complete.cases(df$age_2023, df$sex, df$alzheimer, df$cAMP_LoF), ]
count(df_alzheimer_nomissing_cAMP, alzheimer, cAMP_LoF)
#multiple glm 
multiple_glm_cAMP_alzheimer <- glm(alzheimer ~ cAMP_LoF + age_2023 + sex, data = df_alzheimer_nomissing_cAMP, family = "binomial")
summary(multiple_glm_cAMP_alzheimer)
#odds ratio
odds_ratio_table_cAMP_alzheimer <- exp(coef(multiple_glm_cAMP_alzheimer)) #yields the odds ratios (ORs) for each predictor variable in the model
print(odds_ratio_table_cAMP_alzheimer) #OR of prevelant dementia
exp(confint.default(multiple_glm_cAMP_alzheimer)) # 95% CI
table(df_alzheimer_nomissing_cAMP$alzheimer,df_alzheimer_nomissing_cAMP$cAMP_LoF)
#### end ####


#### G40S pooled Alzheimer####
# Remove rows with missing or non-finite values in age, sex, alzheimer and G40S
df_alzheimer_nomissing_G40S <- df[complete.cases(df$age_2023, df$sex, df$alzheimer, df$G40S), ]
# mutate the df so 1+2 in G40S are just 1
df_G40S_G40S_alzheimer <- df_alzheimer_nomissing_G40S %>%
  mutate(G40S = factor(ifelse(G40S %in% c(1, 2), 1, as.character(G40S))))
count(df_G40S_G40S_alzheimer, alzheimer, G40S)
# multiple glm
glm_G40S_alzheimer <- glm(alzheimer ~ G40S + age_2023 + sex, data = df_G40S_G40S_alzheimer, family = "binomial")
summary(glm_G40S_alzheimer)
#odds ratio
odds_ratio_table_G40S_alzheimer <- exp(coef(glm_G40S_alzheimer)) #yields the odds ratios (ORs) for each predictor variable in the model
print(odds_ratio_table_G40S_alzheimer) #OR of prevelant dementia
exp(confint.default(glm_G40S_alzheimer)) # 95% CI
#### end ####

#### frameshift Alzheimer ####
# Remove rows with missing or non-finite values in age, sex, alzheimer and frameshift
df_alzheimer_nomissing_frameshift <- df[complete.cases(df$age_2023, df$sex, df$alzheimer, df$frameshift), ]
count(df_alzheimer_nomissing_frameshift, alzheimer, frameshift)
#multiple glm 
multiple_glm_frameshift_alzheimer <- glm(alzheimer ~ frameshift + age_2023 + sex, data = df_alzheimer_nomissing_frameshift, family = "binomial")
summary(multiple_glm_frameshift_alzheimer)
#odds ratio
odds_ratio_table_frameshift_alzheimer <- exp(coef(multiple_glm_frameshift_alzheimer)) #yields the odds ratios (ORs) for each predictor variable in the model
print(odds_ratio_table_frameshift_alzheimer) #OR of prevelant dementia
exp(confint.default(multiple_glm_frameshift_alzheimer)) # 95% CI
table(df_alzheimer_nomissing_frameshift$alzheimer,df_alzheimer_nomissing_frameshift$frameshift)
#### end ####








