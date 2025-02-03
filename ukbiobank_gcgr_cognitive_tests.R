library(MatchIt)
library(markdown)
library(oddsratio)
library(tidyverse)
library(dplyr)
library(forestplot)
library(ggplot2)
library(coefplot)
library(data.table)
library(moments)
library(MASS)
library(car)

df <- fread(file = "path_to_your_datafile")
df[df == ""] <- NA

colnames(df)
names(df)[names(df) == "Mean time to correctly identify matches | Instance 0"] <- "test1"
names(df)[names(df) == "Mean time to correctly identify matches | Instance 1"] <- "test2"
names(df)[names(df) == "Mean time to correctly identify matches | Instance 2"] <- "test3"
names(df)[names(df) == "Mean time to correctly identify matches | Instance 3"] <- "test4"
names(df)[names(df) == "Fluid intelligence score | Instance 1"] <- "test5"
names(df)[names(df) == "Fluid intelligence score | Instance 0"] <- "test6"
names(df)[names(df) == "Fluid intelligence score | Instance 2"] <- "test7"
names(df)[names(df) == "Fluid intelligence score | Instance 3"] <- "test8"
names(df)[names(df) == "Number of symbol digit matches made correctly"] <- "test9"
names(df)[names(df) == "Specific cognitive ability (AS) | Instance 2"] <- "test10"
names(df)[names(df) == "Specific cognitive ability (AS) | Instance 3"] <- "test11"
names(df)[names(df) == "Number of word pairs correctly associated | Instance 2"] <- "test12"
names(df)[names(df) == "Number of word pairs correctly associated | Instance 3"] <- "test13"
names(df)[names(df) == "Prospective memory result | Instance 0"] <- "test14" 
names(df)[names(df) == "Prospective memory result | Instance 1"] <- "test15" 
names(df)[names(df) == "Prospective memory result | Instance 2"] <- "test16" 
names(df)[names(df) == "Prospective memory result | Instance 3"] <- "test17" 
names(df)[names(df) == "Time to complete round | Instance 0 | Array 0"] <- "test18"
names(df)[names(df) == "Time to complete round | Instance 0 | Array 1"] <- "test19"
names(df)[names(df) == "Time to complete round | Instance 0 | Array 2"] <- "test20"
names(df)[names(df) == "Time to complete round | Instance 0 | Array 3"] <- "test21"
names(df)[names(df) == "Time to complete round | Instance 1 | Array 1"] <- "test22"
names(df)[names(df) == "Time to complete round | Instance 1 | Array 2"] <- "test23"
names(df)[names(df) == "Time to complete round | Instance 1 | Array 3"] <- "test24"
names(df)[names(df) == "Time to complete round | Instance 2 | Array 1"] <- "test25"
names(df)[names(df) == "Time to complete round | Instance 2 | Array 2"] <- "test26"
names(df)[names(df) == "Time to complete round | Instance 2 | Array 3"] <- "test27"
names(df)[names(df) == "Time to complete round | Instance 3 | Array 1"] <- "test28"
names(df)[names(df) == "Time to complete round | Instance 3 | Array 2"] <- "test29"
names(df)[names(df) == "Time to complete round | Instance 3 | Array 3"] <- "test30"

#set correct structure 
df$cAMP_LoF <- as.factor(df$cAMP_LoF)
df$G40S <- as.factor(df$G40S)
df$frameshift <- as.factor(df$frameshift)
df$BMI <- as.numeric(df$BMI)
df$T2D_bs <- as.factor(df$T2D_bs)
df$sex <- as.character(df$sex)
df$age_enrolment <- as.numeric(df$age_enrolment)
df$test1 <- as.numeric(df$test1)
df$test2 <- as.numeric(df$test2)
df$test3 <- as.numeric(df$test3)
df$test4 <- as.numeric(df$test4)
df$test5 <- as.numeric(df$test5)
df$test6 <- as.numeric(df$test6)
df$test7 <- as.numeric(df$test7)
df$test8 <- as.numeric(df$test8)
df$test9 <- as.numeric(df$test9)
df$test10 <- as.numeric(df$test10)
df$test11 <- as.numeric(df$test11)
df$test12 <- as.numeric(df$test12)
df$test13 <- as.numeric(df$test13)
df$test18 <- as.numeric(df$test18)
df$test19 <- as.numeric(df$test19)
df$test20 <- as.numeric(df$test20)
df$test21 <- as.numeric(df$test21)
df$test22 <- as.numeric(df$test22)
df$test23 <- as.numeric(df$test23)
df$test24 <- as.numeric(df$test24)
df$test25 <- as.numeric(df$test25)
df$test26 <- as.numeric(df$test26)
df$test27 <- as.numeric(df$test27)
df$test28 <- as.numeric(df$test28)
df$test29 <- as.numeric(df$test29)
df$test30 <- as.numeric(df$test30)

sum(!is.na(df$test1)) #406437
sum(!is.na(df$test2)) #17801
sum(!is.na(df$test3)) #51386
sum(!is.na(df$test4)) #4379
sum(!is.na(df$test5)) #17696
sum(!is.na(df$test6)) #130371
sum(!is.na(df$test7)) #50866
sum(!is.na(df$test8)) #4365
sum(!is.na(df$test9)) #99717
sum(!is.na(df$test10)) #28705
sum(!is.na(df$test11)) #4273
sum(!is.na(df$test12)) #40958
sum(!is.na(df$test13)) #4459
sum(!is.na(df$test18)) #97076
sum(!is.na(df$test19)) #400519
sum(!is.na(df$test20)) #399657
sum(!is.na(df$test21)) #0
sum(!is.na(df$test22)) #17715
sum(!is.na(df$test23)) #17695
sum(!is.na(df$test24)) #0
sum(!is.na(df$test25)) #50977
sum(!is.na(df$test26)) #50950
sum(!is.na(df$test27)) #20181
sum(!is.na(df$test28)) #4331
sum(!is.na(df$test29)) #4366
sum(!is.na(df$test30)) #2014

#Approximately 400,000 participants have completed test 1 (reaction time test), 19 (pairs matching_array1) and 20 (pairs matching_array2)
#Test 19 and test 20 are the same type of test (pairs matching).Test 19 includes 3 pairs of cards and test 20 includes 6 pairs
#We will only use test 1 and test 20 in our further analyses as most participants have completed these tests (test 19 has previously been deemed too easy)
#the pairs matching test is called visual memory test in the article

#Start by looking at differences between individuals with or without a GCGR cAMP_LoF variant for test 1 and test 20 
####test1 and test20_cAMP####
count(df, cAMP_LoF)
# Remove rows with missing or non-finite values for age_enrolment, sex, test1, test20 and cAMP_LoF
df_camp_nomissing <- df[complete.cases(df$age_enrolment, df$sex, df$test1, df$test20, df$cAMP_LoF), ]
print(nrow(df_camp_nomissing))
table(df_camp_nomissing$cAMP_LoF)

#Match individuals with or without a GCGR cAMP_LoF variant in a 1:5 ratio on age and sex
matchit_model_camp <- matchit(cAMP_LoF ~ age_enrolment  + sex, data = df_camp_nomissing, method = "nearest", distance = "logit", ratio=5) #matching
matched_data_camp <- match.data(matchit_model_camp) # Access matched data
summary(matchit_model_camp)

baseline_matched_camp <- cbind(
  T2D_bs = c(
    sum(matched_data_camp$T2D_bs == 1 & matched_data_camp$cAMP_LoF == 0)/sum(matched_data_camp$cAMP_LoF == 0)*100,
    sum(matched_data_camp$T2D_bs == 1 & matched_data_camp$cAMP_LoF == 1)/sum(matched_data_camp$cAMP_LoF == 1)*100),
  sex = c(
    sum(matched_data_camp$sex == "Female" & matched_data_camp$cAMP_LoF == 0)/sum(matched_data_camp$cAMP_LoF == 0)*100,
    sum(matched_data_camp$sex == "Female" & matched_data_camp$cAMP_LoF == 1)/sum(matched_data_camp$cAMP_LoF == 1)*100),
  age_median = c(
    median(matched_data_camp$age_enrolment[matched_data_camp$cAMP_LoF == 0]),
    median(matched_data_camp$age_enrolment[matched_data_camp$cAMP_LoF == 1])
  ),
  age_Q1 = c(
    quantile(matched_data_camp$age_enrolment[matched_data_camp$cAMP_LoF == 0], probs = 0.25),
    quantile(matched_data_camp$age_enrolment[matched_data_camp$cAMP_LoF == 1], probs = 0.25)
  ),
  age_Q3 = c(
    quantile(matched_data_camp$age_enrolment[matched_data_camp$cAMP_LoF == 0], probs = 0.75),
    quantile(matched_data_camp$age_enrolment[matched_data_camp$cAMP_LoF == 1], probs = 0.75)
  ),
  BMI_median = c(
    median(matched_data_camp$BMI[matched_data_camp$cAMP_LoF == 0], na.rm = TRUE),
    median(matched_data_camp$BMI[matched_data_camp$cAMP_LoF == 1], na.rm = TRUE)
  ),
  BMI_Q1 = c(
    quantile(matched_data_camp$BMI[matched_data_camp$cAMP_LoF == 0], probs = 0.25, na.rm = TRUE),
    quantile(matched_data_camp$BMI[matched_data_camp$cAMP_LoF == 1], probs = 0.25, na.rm = TRUE)
  ),
  BMI_Q3 = c(
    quantile(matched_data_camp$BMI[matched_data_camp$cAMP_LoF == 0], probs = 0.75, na.rm = TRUE),
    quantile(matched_data_camp$BMI[matched_data_camp$cAMP_LoF == 1], probs = 0.75, na.rm = TRUE)
  )
)

baseline_matched_camp

#T2D
contingency_table <- table(matched_data_camp$cAMP_LoF, matched_data_camp$T2D_bs)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

#test 1; cAMP LOF variant

#check assumptions
hist(matched_data_camp$test1, main = "Histogram of test1", col = "lightblue", border = "black")
# Create a QQ plot for test1
qqnorm(matched_data_camp$test1)
qqline(matched_data_camp$test1, col = 2)
#these data are skewed and we will therefore do a non-parametric t-test:
wilcox_test_test1_cAMP_LoF <- wilcox.test(test1 ~ cAMP_LoF, data = matched_data_camp)
print(wilcox_test_test1_cAMP_LoF)
# Find median for each group
median_group_0 <- median(matched_data_camp$test1[matched_data_camp$cAMP_LoF == 0], na.rm = TRUE)
median_group_1 <- median(matched_data_camp$test1[matched_data_camp$cAMP_LoF == 1], na.rm = TRUE)
# Displaying results
cat("Median for Group 0:", median_group_0, "\n")
cat("Median for Group 1:", median_group_1, "\n")
# Find Q1 and Q3 for each group
q1_group_0 <- quantile(matched_data_camp$test1[matched_data_camp$cAMP_LoF == 0], probs = 0.25, na.rm = TRUE)
q3_group_0 <- quantile(matched_data_camp$test1[matched_data_camp$cAMP_LoF == 0], probs = 0.75, na.rm = TRUE)
q1_group_1 <- quantile(matched_data_camp$test1[matched_data_camp$cAMP_LoF == 1], probs = 0.25, na.rm = TRUE)
q3_group_1 <- quantile(matched_data_camp$test1[matched_data_camp$cAMP_LoF == 1], probs = 0.75, na.rm = TRUE)
# Displaying results
cat("Q1 for Group 0:", q1_group_0, "\n")
cat("Q3 for Group 0:", q3_group_0, "\n")
cat("Q1 for Group 1:", q1_group_1, "\n")
cat("Q3 for Group 1:", q3_group_1, "\n")


# Test 20 (Pairs matching second array); cAMP LOF variant

#check assumptions
hist(matched_data_camp$test20, main = "Histogram of test20", col = "lightblue", border = "black")
# Create a QQ plot for test1
qqnorm(matched_data_camp$test20)
qqline(matched_data_camp$test20, col = 2)
#these data are skewed and we will therefore do a non-parametric t-test:
wilcox_test_test20_cAMP_LoF <- wilcox.test(test20 ~ cAMP_LoF, data = matched_data_camp)
print(wilcox_test_test20_cAMP_LoF)
# Find median for each group
median_group_0 <- median(matched_data_camp$test20[matched_data_camp$cAMP_LoF == 0], na.rm = TRUE)
median_group_1 <- median(matched_data_camp$test20[matched_data_camp$cAMP_LoF == 1], na.rm = TRUE)
# Displaying results
cat("Median for Group 0:", median_group_0, "\n")
cat("Median for Group 1:", median_group_1, "\n")
# Find Q1 and Q3 for each group
q1_group_0 <- quantile(matched_data_camp$test20[matched_data_camp$cAMP_LoF == 0], probs = 0.25, na.rm = TRUE)
q3_group_0 <- quantile(matched_data_camp$test20[matched_data_camp$cAMP_LoF == 0], probs = 0.75, na.rm = TRUE)
q1_group_1 <- quantile(matched_data_camp$test20[matched_data_camp$cAMP_LoF == 1], probs = 0.25, na.rm = TRUE)
q3_group_1 <- quantile(matched_data_camp$test20[matched_data_camp$cAMP_LoF == 1], probs = 0.75, na.rm = TRUE)
# Displaying results
cat("Q1 for Group 0:", q1_group_0, "\n")
cat("Q3 for Group 0:", q3_group_0, "\n")
cat("Q1 for Group 1:", q1_group_1, "\n")
cat("Q3 for Group 1:", q3_group_1, "\n")
####end####



####test1 and test20 frameshift####
# Remove rows with missing or non-finite values for age_enrolment, sex, test1, test20 and frameshift.
df_nomissing_fs <- df[complete.cases(df$age_enrolment, df$sex, df$test20, df$test1, df$frameshift), ]

matchit_model_fs <- matchit(frameshift ~ age_enrolment + sex, data = df_nomissing_fs, method = "nearest", distance = "logit", ratio=5) #matching
matched_data_fs <- match.data(matchit_model_fs) # Access matched data
summary(matchit_model_fs)

baseline_matched_fs <- cbind(
  T2D_bs = c(
    sum(matched_data_fs$T2D_bs == 1 & matched_data_fs$frameshift == 0)/sum(matched_data_fs$frameshift == 0)*100,
    sum(matched_data_fs$T2D_bs == 1 & matched_data_fs$frameshift == 1)/sum(matched_data_fs$frameshift == 1)*100),
  sex = c(
    sum(matched_data_fs$sex == "Female" & matched_data_fs$frameshift == 0)/sum(matched_data_fs$frameshift == 0)*100,
    sum(matched_data_fs$sex == "Female" & matched_data_fs$frameshift == 1)/sum(matched_data_fs$frameshift == 1)*100),
  age_median = c(
    median(matched_data_fs$age_enrolment[matched_data_fs$frameshift == 0]),
    median(matched_data_fs$age_enrolment[matched_data_fs$frameshift == 1])
  ),
  age_Q1 = c(
    quantile(matched_data_fs$age_enrolment[matched_data_fs$frameshift == 0], probs = 0.25),
    quantile(matched_data_fs$age_enrolment[matched_data_fs$frameshift == 1], probs = 0.25)
  ),
  age_Q3 = c(
    quantile(matched_data_fs$age_enrolment[matched_data_fs$frameshift == 0], probs = 0.75),
    quantile(matched_data_fs$age_enrolment[matched_data_fs$frameshift == 1], probs = 0.75)
  ),
  BMI_median = c(
    median(matched_data_fs$BMI[matched_data_fs$frameshift == 0], na.rm = TRUE),
    median(matched_data_fs$BMI[matched_data_fs$frameshift == 1], na.rm = TRUE)
  ),
  BMI_Q1 = c(
    quantile(matched_data_fs$BMI[matched_data_fs$frameshift == 0], probs = 0.25, na.rm = TRUE),
    quantile(matched_data_fs$BMI[matched_data_fs$frameshift == 1], probs = 0.25, na.rm = TRUE)
  ),
  BMI_Q3 = c(
    quantile(matched_data_fs$BMI[matched_data_fs$frameshift == 0], probs = 0.75, na.rm = TRUE),
    quantile(matched_data_fs$BMI[matched_data_fs$frameshift == 1], probs = 0.75, na.rm = TRUE)
  )
)

baseline_matched_fs

# Test 1; frameshift variant

#check assumptions
hist(matched_data_fs$test1, main = "Histogram of test1", col = "lightblue", border = "black")
# Create a QQ plot for test1
qqnorm(matched_data_fs$test1)
qqline(matched_data_fs$test1, col = 2)
#these data are skewed and we will therefore do a non-parametric t-test:
wilcox_test_test1_fs <- wilcox.test(test1 ~ frameshift, data = matched_data_fs)
print(wilcox_test_test1_fs)
# Find median for each group
median_group_0 <- median(matched_data_fs$test1[matched_data_fs$frameshift == 0], na.rm = TRUE)
median_group_1 <- median(matched_data_fs$test1[matched_data_fs$frameshift == 1], na.rm = TRUE)
# Displaying results
cat("Median for Group 0:", median_group_0, "\n")
cat("Median for Group 1:", median_group_1, "\n")
# Find Q1 and Q3 for each group
q1_group_0 <- quantile(matched_data_fs$test1[matched_data_fs$frameshift == 0], probs = 0.25, na.rm = TRUE)
q3_group_0 <- quantile(matched_data_fs$test1[matched_data_fs$frameshift == 0], probs = 0.75, na.rm = TRUE)
q1_group_1 <- quantile(matched_data_fs$test1[matched_data_fs$frameshift == 1], probs = 0.25, na.rm = TRUE)
q3_group_1 <- quantile(matched_data_fs$test1[matched_data_fs$frameshift == 1], probs = 0.75, na.rm = TRUE)
# Displaying results
cat("Q1 for Group 0:", q1_group_0, "\n")
cat("Q3 for Group 0:", q3_group_0, "\n")
cat("Q1 for Group 1:", q1_group_1, "\n")
cat("Q3 for Group 1:", q3_group_1, "\n")


# Test 20; frameshift variant

#check normality 
hist(matched_data_fs$test20, main = "Histogram of test20", col = "lightblue", border = "black")
qqnorm(matched_data_fs$test20)
qqline(matched_data_fs$test20, col = 2)

#these data are skewed 
#perform non-parametric wilcox test:
wilcox_test_test20_fs <- wilcox.test(test20 ~ frameshift, data = matched_data_fs)
print(wilcox_test_test20_fs)
# Find median for each group
median_group_0 <- median(matched_data_fs$test20[matched_data_fs$frameshift == 0], na.rm = TRUE)
median_group_1 <- median(matched_data_fs$test20[matched_data_fs$frameshift == 1], na.rm = TRUE)
# Displaying results
cat("Median for Group 0:", median_group_0, "\n")
cat("Median for Group 1:", median_group_1, "\n")
# Find Q1 and Q3 for each group
q1_group_0 <- quantile(matched_data_fs$test20[matched_data_fs$frameshift == 0], probs = 0.25, na.rm = TRUE)
q3_group_0 <- quantile(matched_data_fs$test20[matched_data_fs$frameshift == 0], probs = 0.75, na.rm = TRUE)
q1_group_1 <- quantile(matched_data_fs$test20[matched_data_fs$frameshift == 1], probs = 0.25, na.rm = TRUE)
q3_group_1 <- quantile(matched_data_fs$test20[matched_data_fs$frameshift == 1], probs = 0.75, na.rm = TRUE)
# Displaying results
cat("Q1 for Group 0:", q1_group_0, "\n")
cat("Q3 for Group 0:", q3_group_0, "\n")
cat("Q1 for Group 1:", q1_group_1, "\n")
cat("Q3 for Group 1:", q3_group_1, "\n")
####end####



####test1 and test20 G40Spooled####
# Remove rows with missing or non-finite values for age_enrolment, sex, test1, test20 and G40S
df_nomissing_G40S <- df[complete.cases(df$age_enrolment, df$sex, df$test1, df$test20, df$G40S), ]

#the G40S column consists of hetero (1) and homozygous (2) carriers. Pool these participants. 
# mutate the df so 1+2 are just 1
df_G40S <- df_nomissing_G40S %>%
  mutate(G40S = factor(ifelse(G40S %in% c(1, 2), 1, as.character(G40S))))
count(df_G40S)

matchit_model_G40S <- matchit(G40S ~ age_enrolment + sex, data = df_G40S, method = "nearest", distance = "logit", ratio=5) #matching
matched_data_G40S <- match.data(matchit_model_G40S) # Access matched data
summary(matchit_model_G40S)

#Finding the mean and SD for the baseline characteristics
baseline_matched_G40S <- cbind(
  T2D_bs = c(
    sum(matched_data_G40S$T2D_bs == 1 & matched_data_G40S$G40S == 0) / sum(matched_data_G40S$G40S == 0) * 100,
    sum(matched_data_G40S$T2D_bs == 1 & matched_data_G40S$G40S == 1) / sum(matched_data_G40S$G40S == 1) * 100
  ),
  sex = c(
    sum(matched_data_G40S$sex == "Female" & matched_data_G40S$G40S == 0) / sum(matched_data_G40S$G40S == 0) * 100,
    sum(matched_data_G40S$sex == "Female" & matched_data_G40S$G40S == 1) / sum(matched_data_G40S$G40S == 1) * 100
  ),
  age_median = c(
    median(matched_data_G40S$age_enrolment[matched_data_G40S$G40S == 0]),
    median(matched_data_G40S$age_enrolment[matched_data_G40S$G40S == 1])
  ),
  age_Q1 = c(
    quantile(matched_data_G40S$age_enrolment[matched_data_G40S$G40S == 0], probs = 0.25),
    quantile(matched_data_G40S$age_enrolment[matched_data_G40S$G40S == 1], probs = 0.25)
  ),
  age_Q3 = c(
    quantile(matched_data_G40S$age_enrolment[matched_data_G40S$G40S == 0], probs = 0.75),
    quantile(matched_data_G40S$age_enrolment[matched_data_G40S$G40S == 1], probs = 0.75)
  ),
  BMI_median = c(
    median(matched_data_G40S$BMI[matched_data_G40S$G40S == 0], na.rm = TRUE),
    median(matched_data_G40S$BMI[matched_data_G40S$G40S == 1], na.rm = TRUE)
  ),
  BMI_Q1 = c(
    quantile(matched_data_G40S$BMI[matched_data_G40S$G40S == 0], probs = 0.25, na.rm = TRUE),
    quantile(matched_data_G40S$BMI[matched_data_G40S$G40S == 1], probs = 0.25, na.rm = TRUE)
  ),
  BMI_Q3 = c(
    quantile(matched_data_G40S$BMI[matched_data_G40S$G40S == 0], probs = 0.75, na.rm = TRUE),
    quantile(matched_data_G40S$BMI[matched_data_G40S$G40S == 1], probs = 0.75, na.rm = TRUE)
  )
)

baseline_matched_G40S


#T2D
contingency_table <- table(matched_data_G40S$G40S, matched_data_G40S$T2D_bs)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

# Test 1; G40S variant

#check assumptions
hist(matched_data_G40S$test1, main = "Histogram of test1", col = "lightblue", border = "black")
# Create a QQ plot for test1
qqnorm(matched_data_G40S$test1)
qqline(matched_data_G40S$test1, col = 2)

#these data are skewed and I will therefore do a non-parametric ttest:
wilcox_test_test1_G40S <- wilcox.test(test1 ~ G40S, data = matched_data_G40S)
print(wilcox_test_test1_G40S)
# Find median for each group
median_group_0 <- median(matched_data_G40S$test1[matched_data_G40S$G40S == 0], na.rm = TRUE)
median_group_1 <- median(matched_data_G40S$test1[matched_data_G40S$G40S == 1], na.rm = TRUE)
# Displaying results
cat("Median for Group 0:", median_group_0, "\n")
cat("Median for Group 1:", median_group_1, "\n")
# Find Q1 and Q3 for each group
q1_group_0 <- quantile(matched_data_G40S$test1[matched_data_G40S$G40S == 0], probs = 0.25, na.rm = TRUE)
q3_group_0 <- quantile(matched_data_G40S$test1[matched_data_G40S$G40S == 0], probs = 0.75, na.rm = TRUE)
q1_group_1 <- quantile(matched_data_G40S$test1[matched_data_G40S$G40S == 1], probs = 0.25, na.rm = TRUE)
q3_group_1 <- quantile(matched_data_G40S$test1[matched_data_G40S$G40S == 1], probs = 0.75, na.rm = TRUE)
# Displaying results
cat("Q1 for Group 0:", q1_group_0, "\n")
cat("Q3 for Group 0:", q3_group_0, "\n")
cat("Q1 for Group 1:", q1_group_1, "\n")
cat("Q3 for Group 1:", q3_group_1, "\n")

# Test 20; G40S variant

#check assumptions
hist(matched_data_G40S$test20, main = "Histogram of test20", col = "lightblue", border = "black")
# Create a QQ plot for test20
qqnorm(matched_data_G40S$test20)
qqline(matched_data_G40S$test20, col = 2)

#these data are skewed and we will therefore do a non-parametric t-test:
wilcox_test_test20_G40S <- wilcox.test(test20 ~ G40S, data = matched_data_G40S)
print(wilcox_test_test20_G40S)
# Find median for each group
median_group_0 <- median(matched_data_G40S$test20[matched_data_G40S$G40S == 0], na.rm = TRUE)
median_group_1 <- median(matched_data_G40S$test20[matched_data_G40S$G40S == 1], na.rm = TRUE)
# Displaying results
cat("Median for Group 0:", median_group_0, "\n")
cat("Median for Group 1:", median_group_1, "\n")
# Find Q1 and Q3 for each group
q1_group_0 <- quantile(matched_data_G40S$test20[matched_data_G40S$G40S == 0], probs = 0.25, na.rm = TRUE)
q3_group_0 <- quantile(matched_data_G40S$test20[matched_data_G40S$G40S == 0], probs = 0.75, na.rm = TRUE)
q1_group_1 <- quantile(matched_data_G40S$test20[matched_data_G40S$G40S == 1], probs = 0.25, na.rm = TRUE)
q3_group_1 <- quantile(matched_data_G40S$test20[matched_data_G40S$G40S == 1], probs = 0.75, na.rm = TRUE)
# Displaying results
cat("Q1 for Group 0:", q1_group_0, "\n")
cat("Q3 for Group 0:", q3_group_0, "\n")
cat("Q1 for Group 1:", q1_group_1, "\n")
cat("Q3 for Group 1:", q3_group_1, "\n")
####end####





























