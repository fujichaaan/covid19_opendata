#######################################################################################
## 8. Estimation of the effects of each cue to actions on avoiding social gathering  ##
#######################################################################################

################ 8.1 China ################ 

## 1. china_conformity
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + conformity, data = data_china, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ch_gather_conformity <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_ch_gather_conformity <- round(exp(y[19, 1]), 2)
UpperCL_ch_gather_conformity <- round(exp(y[19, 2]), 2)
pvalue_ch_gather_conformity <- x$coefficients[19, "Pr(>|z|)"]
z1 <- c(OR_ch_gather_conformity, paste(LowerCL_ch_gather_conformity, UpperCL_ch_gather_conformity, sep = "-"), pvalue_ch_gather_conformity)

## 2. china_recom_family
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_family, data = data_china, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ch_gather_recom_family <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_ch_gather_recom_family <- round(exp(y[19, 1]), 2)
UpperCL_ch_gather_recom_family <- round(exp(y[19, 2]), 2)
pvalue_ch_gather_recom_family <- x$coefficients[19, "Pr(>|z|)"]
z2 <- c(OR_ch_gather_recom_family, paste(LowerCL_ch_gather_recom_family, UpperCL_ch_gather_recom_family, sep = "-"), pvalue_ch_gather_recom_family)

## 3. china_recom_doctors
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_doctors, data = data_china, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ch_gather_recom_doctors <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_ch_gather_recom_doctors <- round(exp(y[19, 1]), 2)
UpperCL_ch_gather_recom_doctors <- round(exp(y[19, 2]), 2)
pvalue_ch_gather_recom_doctors <- x$coefficients[19, "Pr(>|z|)"]
z3 <- c(OR_ch_gather_recom_doctors, paste(LowerCL_ch_gather_recom_doctors, UpperCL_ch_gather_recom_doctors, sep = "-"), pvalue_ch_gather_recom_doctors)

## 4. china_recom_polit
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_polit, data = data_china, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ch_gather_recom_polit <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_ch_gather_recom_polit <- round(exp(y[19, 1]), 2)
UpperCL_ch_gather_recom_polit <- round(exp(y[19, 2]), 2)
pvalue_ch_gather_recom_polit <- x$coefficients[19, "Pr(>|z|)"]
z4 <- c(OR_ch_gather_recom_polit, paste(LowerCL_ch_gather_recom_polit, UpperCL_ch_gather_recom_polit, sep = "-"), pvalue_ch_gather_recom_polit)

china_mask <- rbind(z1, z2, z3, z4)

################ 8.2 Italy ################ 

## 1. italy_conformity
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + conformity, data = data_italy, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_it_gather_conformity <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_it_gather_conformity <- round(exp(y[19, 1]), 2)
UpperCL_it_gather_conformity <- round(exp(y[19, 2]), 2)
pvalue_it_gather_conformity <- x$coefficients[19, "Pr(>|z|)"]
z1 <- c(OR_it_gather_conformity, paste(LowerCL_it_gather_conformity, UpperCL_it_gather_conformity, sep = "-"), pvalue_it_gather_conformity)

## 2. italy_recom_family
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_family, data = data_italy, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_it_gather_recom_family <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_it_gather_recom_family <- round(exp(y[19, 1]), 2)
UpperCL_it_gather_recom_family <- round(exp(y[19, 2]), 2)
pvalue_it_gather_recom_family <- x$coefficients[19, "Pr(>|z|)"]
z2 <- c(OR_it_gather_recom_family, paste(LowerCL_it_gather_recom_family, UpperCL_it_gather_recom_family, sep = "-"), pvalue_it_gather_recom_family)

## 3. italy_recom_doctors
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_doctors, data = data_italy, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_it_gather_recom_doctors <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_it_gather_recom_doctors <- round(exp(y[19, 1]), 2)
UpperCL_it_gather_recom_doctors <- round(exp(y[19, 2]), 2)
pvalue_it_gather_recom_doctors <- x$coefficients[19, "Pr(>|z|)"]
z3 <- c(OR_it_gather_recom_doctors, paste(LowerCL_it_gather_recom_doctors, UpperCL_it_gather_recom_doctors, sep = "-"), pvalue_it_gather_recom_doctors)

## 4. italy_recom_polit
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_polit, data = data_italy, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_it_gather_recom_polit <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_it_gather_recom_polit <- round(exp(y[19, 1]), 2)
UpperCL_it_gather_recom_polit <- round(exp(y[19, 2]), 2)
pvalue_it_gather_recom_polit <- x$coefficients[19, "Pr(>|z|)"]
z4 <- c(OR_it_gather_recom_polit, paste(LowerCL_it_gather_recom_polit, UpperCL_it_gather_recom_polit, sep = "-"), pvalue_it_gather_recom_polit)

italy_mask <- rbind(z1, z2, z3, z4)

################ 8.3 Japan ################ 

## 1. japan_conformity
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + conformity, data = data_japan, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_jp_gather_conformity <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_jp_gather_conformity <- round(exp(y[19, 1]), 2)
UpperCL_jp_gather_conformity <- round(exp(y[19, 2]), 2)
pvalue_jp_gather_conformity <- x$coefficients[19, "Pr(>|z|)"]
z1 <- c(OR_jp_gather_conformity, paste(LowerCL_jp_gather_conformity, UpperCL_jp_gather_conformity, sep = "-"), pvalue_jp_gather_conformity)

## 2. japan_recom_family
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_family, data = data_japan, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_jp_gather_recom_family <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_jp_gather_recom_family <- round(exp(y[19, 1]), 2)
UpperCL_jp_gather_recom_family <- round(exp(y[19, 2]), 2)
pvalue_jp_gather_recom_family <- x$coefficients[19, "Pr(>|z|)"]
z2 <- c(OR_jp_gather_recom_family, paste(LowerCL_jp_gather_recom_family, UpperCL_jp_gather_recom_family, sep = "-"), pvalue_jp_gather_recom_family)

## 3. japan_recom_doctors
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_doctors, data = data_japan, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_jp_gather_recom_doctors <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_jp_gather_recom_doctors <- round(exp(y[19, 1]), 2)
UpperCL_jp_gather_recom_doctors <- round(exp(y[19, 2]), 2)
pvalue_jp_gather_recom_doctors <- x$coefficients[19, "Pr(>|z|)"]
z3 <- c(OR_jp_gather_recom_doctors, paste(LowerCL_jp_gather_recom_doctors, UpperCL_jp_gather_recom_doctors, sep = "-"), pvalue_jp_gather_recom_doctors)

## 4. japan_recom_polit
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_polit, data = data_japan, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_jp_gather_recom_polit <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_jp_gather_recom_polit <- round(exp(y[19, 1]), 2)
UpperCL_jp_gather_recom_polit <- round(exp(y[19, 2]), 2)
pvalue_jp_gather_recom_polit <- x$coefficients[19, "Pr(>|z|)"]
z4 <- c(OR_jp_gather_recom_polit, paste(LowerCL_jp_gather_recom_polit, UpperCL_jp_gather_recom_polit, sep = "-"), pvalue_jp_gather_recom_polit)

japan_mask <- rbind(z1, z2, z3, z4)

################ 8.4 Korea ################ 

## 1. korea_conformity
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + conformity, data = data_korea, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ko_gather_conformity <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_ko_gather_conformity <- round(exp(y[19, 1]), 2)
UpperCL_ko_gather_conformity <- round(exp(y[19, 2]), 2)
pvalue_ko_gather_conformity <- x$coefficients[19, "Pr(>|z|)"]
z1 <- c(OR_ko_gather_conformity, paste(LowerCL_ko_gather_conformity, UpperCL_ko_gather_conformity, sep = "-"), pvalue_ko_gather_conformity)

## 2. korea_recom_family
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_family, data = data_korea, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ko_gather_recom_family <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_ko_gather_recom_family <- round(exp(y[19, 1]), 2)
UpperCL_ko_gather_recom_family <- round(exp(y[19, 2]), 2)
pvalue_ko_gather_recom_family <- x$coefficients[19, "Pr(>|z|)"]
z2 <- c(OR_ko_gather_recom_family, paste(LowerCL_ko_gather_recom_family, UpperCL_ko_gather_recom_family, sep = "-"), pvalue_ko_gather_recom_family)

## 3. korea_recom_doctors
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_doctors, data = data_korea, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ko_gather_recom_doctors <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_ko_gather_recom_doctors <- round(exp(y[19, 1]), 2)
UpperCL_ko_gather_recom_doctors <- round(exp(y[19, 2]), 2)
pvalue_ko_gather_recom_doctors <- x$coefficients[19, "Pr(>|z|)"]
z3 <- c(OR_ko_gather_recom_doctors, paste(LowerCL_ko_gather_recom_doctors, UpperCL_ko_gather_recom_doctors, sep = "-"), pvalue_ko_gather_recom_doctors)

## 4. korea_recom_polit
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_polit, data = data_korea, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ko_gather_recom_polit <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_ko_gather_recom_polit <- round(exp(y[19, 1]), 2)
UpperCL_ko_gather_recom_polit <- round(exp(y[19, 2]), 2)
pvalue_ko_gather_recom_polit <- x$coefficients[19, "Pr(>|z|)"]
z4 <- c(OR_ko_gather_recom_polit, paste(LowerCL_ko_gather_recom_polit, UpperCL_ko_gather_recom_polit, sep = "-"), pvalue_ko_gather_recom_polit)

korea_mask <- rbind(z1, z2, z3, z4)

################ 8.5 UK ################ 

## 1. uk_conformity
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + conformity, data = data_uk, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk_gather_conformity <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_uk_gather_conformity <- round(exp(y[19, 1]), 2)
UpperCL_uk_gather_conformity <- round(exp(y[19, 2]), 2)
pvalue_uk_gather_conformity <- x$coefficients[19, "Pr(>|z|)"]
z1 <- c(OR_uk_gather_conformity, paste(LowerCL_uk_gather_conformity, UpperCL_uk_gather_conformity, sep = "-"), pvalue_uk_gather_conformity)

## 2. uk_recom_family
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_family, data = data_uk, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk_gather_recom_family <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_uk_gather_recom_family <- round(exp(y[19, 1]), 2)
UpperCL_uk_gather_recom_family <- round(exp(y[19, 2]), 2)
pvalue_uk_gather_recom_family <- x$coefficients[19, "Pr(>|z|)"]
z2 <- c(OR_uk_gather_recom_family, paste(LowerCL_uk_gather_recom_family, UpperCL_uk_gather_recom_family, sep = "-"), pvalue_uk_gather_recom_family)

## 3. uk_recom_doctors
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_doctors, data = data_uk, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk_gather_recom_doctors <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_uk_gather_recom_doctors <- round(exp(y[19, 1]), 2)
UpperCL_uk_gather_recom_doctors <- round(exp(y[19, 2]), 2)
pvalue_uk_gather_recom_doctors <- x$coefficients[19, "Pr(>|z|)"]
z3 <- c(OR_uk_gather_recom_doctors, paste(LowerCL_uk_gather_recom_doctors, UpperCL_uk_gather_recom_doctors, sep = "-"), pvalue_uk_gather_recom_doctors)

## 4. uk_recom_polit
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_polit, data = data_uk, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk_gather_recom_polit <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_uk_gather_recom_polit <- round(exp(y[19, 1]), 2)
UpperCL_uk_gather_recom_polit <- round(exp(y[19, 2]), 2)
pvalue_uk_gather_recom_polit <- x$coefficients[19, "Pr(>|z|)"]
z4 <- c(OR_uk_gather_recom_polit, paste(LowerCL_uk_gather_recom_polit, UpperCL_uk_gather_recom_polit, sep = "-"), pvalue_uk_gather_recom_polit)

uk_mask <- rbind(z1, z2, z3, z4)

################ 8.6 US ################ 

## 1. us_conformity
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + conformity, data = data_us, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us_gather_conformity <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_us_gather_conformity <- round(exp(y[19, 1]), 2)
UpperCL_us_gather_conformity <- round(exp(y[19, 2]), 2)
pvalue_us_gather_conformity <- x$coefficients[19, "Pr(>|z|)"]
z1 <- c(OR_us_gather_conformity, paste(LowerCL_us_gather_conformity, UpperCL_us_gather_conformity, sep = "-"), pvalue_us_gather_conformity)

## 2. us_recom_family
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_family, data = data_us, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us_gather_recom_family <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_us_gather_recom_family <- round(exp(y[19, 1]), 2)
UpperCL_us_gather_recom_family <- round(exp(y[19, 2]), 2)
pvalue_us_gather_recom_family <- x$coefficients[19, "Pr(>|z|)"]
z2 <- c(OR_us_gather_recom_family, paste(LowerCL_us_gather_recom_family, UpperCL_us_gather_recom_family, sep = "-"), pvalue_us_gather_recom_family)

## 3. us_recom_doctors
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_doctors, data = data_us, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us_gather_recom_doctors <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_us_gather_recom_doctors <- round(exp(y[19, 1]), 2)
UpperCL_us_gather_recom_doctors <- round(exp(y[19, 2]), 2)
pvalue_us_gather_recom_doctors <- x$coefficients[19, "Pr(>|z|)"]
z3 <- c(OR_us_gather_recom_doctors, paste(LowerCL_us_gather_recom_doctors, UpperCL_us_gather_recom_doctors, sep = "-"), pvalue_us_gather_recom_doctors)

## 4. us_recom_polit
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + recom_polit, data = data_us, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us_gather_recom_polit <- round(exp(x$coefficients[19, 1]), 2)
LowerCL_us_gather_recom_polit <- round(exp(y[19, 1]), 2)
UpperCL_us_gather_recom_polit <- round(exp(y[19, 2]), 2)
pvalue_us_gather_recom_polit <- x$coefficients[19, "Pr(>|z|)"]
z4 <- c(OR_us_gather_recom_polit, paste(LowerCL_us_gather_recom_polit, UpperCL_us_gather_recom_polit, sep = "-"), pvalue_us_gather_recom_polit)

us_mask <- rbind(z1, z2, z3, z4)

gather_behav_change <- cbind(china_mask, italy_mask, japan_mask, korea_mask, uk_mask, us_mask)
write.csv(gather_behav_change, "gather_behav_change.csv")