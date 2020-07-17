####################################################################################################################
## This is a code for statistical analysis used in the paper of "Public perceptions, individual characteristics,  ##
## and health preventive behaviors for the COVID-19 among adults in six countries: a cross-sectional study"       ##
## Ryosuke Fujii, Kensuke Suzuki, Junichiro Niimi.                                                                ##
####################################################################################################################

#The original data is publicly available on https://osf.io/aubkc/.
#Representative evidence from China, Japan, Korea, Italy, the UK and the US on exposure, behavioral response, economic impacts and beliefs relating to the Covid-19 pandemic.

## 1. Preparation for analysis ###########################################
library(tableone)

#Importing dataset
data <- read.csv("public_dataset.csv", header = T, sep =","); nrow(data)

#Excluding "Prefer not to answer" for questions regarding age_group, income, and gender
data <- data[which(data$age_group != "Prefer not to answer" & data$gender != "Prefer not to answer" & data$income_group != "Prefer not to answer"),]; nrow(data)
data$gender <- factor(data$gender)

#Regarding age and income group, change levels in factor variables (<=65 --> 0, more than 66 --> 1)
data$age_group <- factor(data$age_group, levels = c("Between 18 and 25", "Between 26 and 35", "Between 36 and 45", "Between 46 and 55", "Between 56 and 65", "Between 66 and 75", "Above 75"))
data$age65 <- factor(as.numeric(data$age_group == "Between 66 and 75" | data$age_group == "Above 75"))

#Changing the levels of income_group
data$income_group <- factor(data$income_group, levels = c("First quintile ", "Second quintile", "Third quintile", "Fourth quintile", "Fifth quintile"))

#Refine the variable for living arrangement (Live alone --> 1, others --> 0)
data$living_arrangement <- factor(as.numeric(data$current_living_arrangement == "Live alone"))

#Refine the variable for current living area (Urban --> 1, Countryside & Semi-Urban --> 0)
data$living_area <- factor(as.numeric(data$current_living_area == "Urban"))

#Refine the variable of wearing a mask at the survey and before pandemic
data$mask_now <- as.numeric(data$Q293_2 == "Sometimes" | data$Q293_2 == "Very often" | data$Q293_2 == "Always")
data$mask_before <- as.numeric(data$Q291_2 == "Sometimes" | data$Q291_2 == "Very often" | data$Q291_2 == "Always")

#Refine the variable of washing hands or using hand sanitizer at the survey and before pandemic
data$wash_now <- as.numeric(data$Q293_1 == "Sometimes" | data$Q293_1 == "Very often" | data$Q293_1 == "Always")
data$wash_before <- as.numeric(data$Q291_1 == "Sometimes" | data$Q291_1 == "Very often" | data$Q291_1 == "Always")

#Refine the variable of social gathering more than 20 persons at the survey and before pandemic
data$gather_now <- as.numeric(data$Q293_8 == "Never" | data$Q293_8 == "Rarely")
data$gather_before <- as.numeric(data$Q291_8 == "Never" | data$Q291_8 == "Rarely")

#Refine the variable of public transportation use at current stage
data$pubtrans <- factor(as.numeric(data$Q293_14 == "Sometimes" | data$Q293_14 == "Very often" | data$Q293_14 == "Always"))

#Regarding effectiveness of policy (wearing a mask), changing levels
data$belief_policy_effectiveness_mask <- factor(data$belief_policy_effectiveness_7, levels = c("Not effective at all", "Slightly effective", "Moderately effective", "Very effective", "Extremely effective"))

#Regarding effectiveness of policy (hand washing), changing levels
data$belief_effectiveness_washinghands <- factor(data$belief_washinghands, levels = c("Not effective at all", "Slightly effective", "Moderately effective", "Very effective", "Extremely effective"))

#Regarding effectiveness of policy (social gathering), changing levels
data$belief_policy_effectiveness_gather <- factor(data$belief_policy_effectiveness_5, levels = c("Not effective at all", "Slightly effective", "Moderately effective", "Very effective", "Extremely effective"))

## 2. Basic characteristics of all participants ###########################################

#Characteristics in all subjects according to countries
listvar <- c("age65", "gender", "living_area", "living_arrangement", "income_group", "neg_nonfin_anxiety", "pubtrans", "belief_you_got_infected", "belief_inf_serious_hosp", "belief_policy_effectiveness_mask", "belief_effectiveness_washinghands", "belief_policy_effectiveness_gather", "Q293_2", "Q293_1", "Q293_8")
listcat <- c("age65", "gender", "living_area", "living_arrangement", "income_group", "neg_nonfin_anxiety", "pubtrans", "belief_policy_effectiveness_mask",  "belief_effectiveness_washinghands", "belief_policy_effectiveness_gather", "Q293_2", "Q293_1", "Q293_8")
table1 <- CreateTableOne(vars = listvar, factorVars = listcat, strata = "country", data = data)
table1 <- print(table1, cramVars = c("income_group", "belief_policy_effectiveness_mask",  "belief_effectiveness_washinghands", "belief_policy_effectiveness_gather"))

#Output these tables as CSV files.
write.csv(table1, "Table1.csv")

#1. reason_behchange_conformity --> conformity
data$conformity <- factor(as.numeric(data$reason_behchange_conformity == "Selected"))
#2. reason_behchange_reccom_family --> recom_family
data$recom_family <- factor(as.numeric(data$reason_behchange_reccom_family == "Selected"))
#3. reason_behchange_reccom_doctors --> recom_doctors
data$recom_doctors <- factor(as.numeric(data$reason_behchange_reccom_doctors == "Selected"))
#4. reason_behchange_reccom_polit --> recom_polit
data$recom_polit <- factor(as.numeric(data$reason_behchange_reccom_polit == "Selected"))

#Characteristics for the reasons in all subjects according to countries
listvar <- c("conformity", "recom_family", "recom_ doctors", "recom_polit")
table2 <- CreateTableOne(vars = listvar, factorVars = listcat, strata = "country", data = data)
table2 <- print(table2); write.csv(table2, "Table2.csv")

#Divide data into each country
data_china <- subset(data, country == "china")
data_italy <- subset(data, country == "italy")
data_japan <- subset(data, country == "japan")
data_korea <- subset(data, country == "korea")
data_uk <- subset(data, country == "uk")
data_us <- subset(data, country == "us")

## 3. Countries-stratified analysis for wearing masks (current) ################################
#1. China
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_mask + mask_before, data = data_china, family = binomial); summary(result)

#Calculating odds ratios
ModelName <- result
alpha <- 0.05
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_china <- round(exp(x$coefficients[,1]), 2)
LowerCL_china <- round(exp(y[,1]), 2)
UpperCL_china <- round(exp(y[,2]), 2)
pvalue_china <- summary(result)$coefficients[,"Pr(>|z|)"]

#2. Italy
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_mask + mask_before, data = data_italy, family = binomial); summary(result)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_italy <- round(exp(x$coefficients[,1]), 2)
LowerCL_italy <- round(exp(y[,1]), 2)
UpperCL_italy <- round(exp(y[,2]), 2)
pvalue_italy <- summary(result)$coefficients[,"Pr(>|z|)"]

#3. Japan
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_mask + mask_before, data = data_japan, family = binomial); summary(result)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_japan <- round(exp(x$coefficients[,1]), 2)
LowerCL_japan <- round(exp(y[,1]), 2)
UpperCL_japan <- round(exp(y[,2]), 2)
pvalue_japan <- summary(result)$coefficients[,"Pr(>|z|)"]

#4. Korea
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_mask + mask_before, data = data_korea, family = binomial); summary(result)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_korea <- round(exp(x$coefficients[,1]), 2)
LowerCL_korea <- round(exp(y[,1]), 2)
UpperCL_korea <- round(exp(y[,2]), 2)
pvalue_korea <- summary(result)$coefficients[,"Pr(>|z|)"]

#5. UK
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_mask + mask_before, data = data_uk, family = binomial); summary(result)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk <- round(exp(x$coefficients[,1]), 2)
LowerCL_uk <- round(exp(y[,1]), 2)
UpperCL_uk <- round(exp(y[,2]), 2)
pvalue_uk <- summary(result)$coefficients[,"Pr(>|z|)"]

#6. US
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_mask + mask_before, data = data_us, family = binomial); summary(result)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us <- round(exp(x$coefficients[,1]), 2)
LowerCL_us <- round(exp(y[,1]), 2)
UpperCL_us <- round(exp(y[,2]), 2)
pvalue_us <- summary(result)$coefficients[,"Pr(>|z|)"]

#Writing results
Mask <- cbind(OR_china, LowerCL_china, UpperCL_china, pvalue_china, 
              OR_italy, LowerCL_italy, UpperCL_italy, pvalue_italy, 
              OR_japan, LowerCL_japan, UpperCL_japan, pvalue_japan,
              OR_korea, LowerCL_korea, UpperCL_korea, pvalue_korea,
              OR_uk, LowerCL_uk, UpperCL_uk, pvalue_uk,
              OR_us, LowerCL_us, UpperCL_us, pvalue_us)
write.csv(Mask, "result_mask_now.csv")

## 4. Countries-stratified analysis for washing hands (current) #################################
#1. China
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_effectiveness_washinghands + wash_before, data = data_china, family = binomial); summary(result)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_china <- round(exp(x$coefficients[,1]), 2)
LowerCL_china <- round(exp(y[,1]), 2)
UpperCL_china <- round(exp(y[,2]), 2)
pvalue_china <- summary(result)$coefficients[,"Pr(>|z|)"]

#2. Italy
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_effectiveness_washinghands + wash_before, data = data_italy, family = binomial); summary(result)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_italy <- round(exp(x$coefficients[,1]), 2)
LowerCL_italy <- round(exp(y[,1]), 2)
UpperCL_italy <- round(exp(y[,2]), 2)
pvalue_italy <- summary(result)$coefficients[,"Pr(>|z|)"]

#3. Japan
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_effectiveness_washinghands + wash_before, data = data_japan, family = binomial); summary(result)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_japan <- round(exp(x$coefficients[,1]), 2)
LowerCL_japan <- round(exp(y[,1]), 2)
UpperCL_japan <- round(exp(y[,2]), 2)
pvalue_japan <- summary(result)$coefficients[,"Pr(>|z|)"]

#4. Korea
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_effectiveness_washinghands + wash_before, data = data_korea, family = binomial); summary(result)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_korea <- round(exp(x$coefficients[,1]), 2)
LowerCL_korea <- round(exp(y[,1]), 2)
UpperCL_korea <- round(exp(y[,2]), 2)
pvalue_korea <- summary(result)$coefficients[,"Pr(>|z|)"]

#5. UK
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_effectiveness_washinghands + wash_before, data = data_uk, family = binomial); summary(result)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk <- round(exp(x$coefficients[,1]), 2)
LowerCL_uk <- round(exp(y[,1]), 2)
UpperCL_uk <- round(exp(y[,2]), 2)
pvalue_uk <- summary(result)$coefficients[,"Pr(>|z|)"]

#6. US
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_effectiveness_washinghands + wash_before, data = data_us, family = binomial); summary(result)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us <- round(exp(x$coefficients[,1]), 2)
LowerCL_us <- round(exp(y[,1]), 2)
UpperCL_us <- round(exp(y[,2]), 2)
pvalue_us <- summary(result)$coefficients[,"Pr(>|z|)"]

#Writing results
Wash <- cbind(OR_china, LowerCL_china, UpperCL_china, pvalue_china, 
              OR_italy, LowerCL_italy, UpperCL_italy, pvalue_italy, 
              OR_japan, LowerCL_japan, UpperCL_japan, pvalue_japan,
              OR_korea, LowerCL_korea, UpperCL_korea, pvalue_korea,
              OR_uk, LowerCL_uk, UpperCL_uk, pvalue_uk,
              OR_us, LowerCL_us, UpperCL_us, pvalue_us)
write.csv(Wash, "result_wash_now.csv")

## 5. Countries-stratified analysis for avoidance of social gathering (current) #################
#1. China
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before, data = data_china, family = binomial); summary(result)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_china <- round(exp(x$coefficients[,1]), 2)
LowerCL_china <- round(exp(y[,1]), 2)
UpperCL_china <- round(exp(y[,2]), 2)
pvalue_china <- summary(result)$coefficients[,"Pr(>|z|)"]

#2. Italy
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before, data = data_italy, family = binomial); summary(result)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_italy <- round(exp(x$coefficients[,1]), 2)
LowerCL_italy <- round(exp(y[,1]), 2)
UpperCL_italy <- round(exp(y[,2]), 2)
pvalue_italy <- summary(result)$coefficients[,"Pr(>|z|)"]

#3. Japan
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before, data = data_japan, family = binomial); summary(result)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_japan <- round(exp(x$coefficients[,1]), 2)
LowerCL_japan <- round(exp(y[,1]), 2)
UpperCL_japan <- round(exp(y[,2]), 2)
pvalue_japan <- summary(result)$coefficients[,"Pr(>|z|)"]

#4. Korea
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before, data = data_korea, family = binomial); summary(result)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_korea <- round(exp(x$coefficients[,1]), 2)
LowerCL_korea <- round(exp(y[,1]), 2)
UpperCL_korea <- round(exp(y[,2]), 2)
pvalue_korea <- summary(result)$coefficients[,"Pr(>|z|)"]

#5. UK
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before, data = data_uk, family = binomial); summary(result)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk <- round(exp(x$coefficients[,1]), 2)
LowerCL_uk <- round(exp(y[,1]), 2)
UpperCL_uk <- round(exp(y[,2]), 2)
pvalue_uk <- summary(result)$coefficients[,"Pr(>|z|)"]

#6. US
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before, data = data_us, family = binomial); summary(result)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us <- round(exp(x$coefficients[,1]), 2)
LowerCL_us <- round(exp(y[,1]), 2)
UpperCL_us <- round(exp(y[,2]), 2)
pvalue_us <- summary(result)$coefficients[,"Pr(>|z|)"]

#Writing results
Gather <- cbind(OR_china, LowerCL_china, UpperCL_china, pvalue_china, 
                OR_italy, LowerCL_italy, UpperCL_italy, pvalue_italy, 
                OR_japan, LowerCL_japan, UpperCL_japan, pvalue_japan,
                OR_korea, LowerCL_korea, UpperCL_korea, pvalue_korea,
                OR_uk, LowerCL_uk, UpperCL_uk, pvalue_uk,
                OR_us, LowerCL_us, UpperCL_us, pvalue_us)
write.csv(Gather, "result_gather_now.csv")

## 6. Estimation of the effects of each cue to actions on wearing a mask #########################

##################################
##  6.1       China             ##
##################################

## 1. china_conformity
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + conformity, data = data_china, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ch_mask_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ch_mask_conformity <- round(exp(y[20, 1]), 2)
UpperCL_ch_mask_conformity <- round(exp(y[20, 2]), 2)
pvalue_ch_mask_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_ch_mask_conformity, paste(LowerCL_ch_mask_conformity, UpperCL_ch_mask_conformity, sep = "-"), pvalue_ch_mask_conformity)

## 2. china_recom_family
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_family, data = data_china, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ch_mask_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ch_mask_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_ch_mask_recom_family <- round(exp(y[20, 2]), 2)
pvalue_ch_mask_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_ch_mask_recom_family, paste(LowerCL_ch_mask_recom_family, UpperCL_ch_mask_recom_family, sep = "-"), pvalue_ch_mask_recom_family)

## 3. china_recom_doctors
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_doctors, data = data_china, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ch_mask_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ch_mask_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_ch_mask_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_ch_mask_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_ch_mask_recom_doctors, paste(LowerCL_ch_mask_recom_doctors, UpperCL_ch_mask_recom_doctors, sep = "-"), pvalue_ch_mask_recom_doctors)

## 4. china_recom_polit
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_polit, data = data_china, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ch_mask_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ch_mask_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_ch_mask_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_ch_mask_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_ch_mask_recom_polit, paste(LowerCL_ch_mask_recom_polit, UpperCL_ch_mask_recom_polit, sep = "-"), pvalue_ch_mask_recom_polit)

china_mask <- rbind(z1, z2, z3, z4)

##################################
##  6.2       italy             ##
##################################

## 1. italy_conformity
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + conformity, data = data_italy, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_it_mask_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_it_mask_conformity <- round(exp(y[20, 1]), 2)
UpperCL_it_mask_conformity <- round(exp(y[20, 2]), 2)
pvalue_it_mask_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_it_mask_conformity, paste(LowerCL_it_mask_conformity, UpperCL_it_mask_conformity, sep = "-"), pvalue_it_mask_conformity)

## 2. italy_recom_family
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_family, data = data_italy, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_it_mask_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_it_mask_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_it_mask_recom_family <- round(exp(y[20, 2]), 2)
pvalue_it_mask_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_it_mask_recom_family, paste(LowerCL_it_mask_recom_family, UpperCL_it_mask_recom_family, sep = "-"), pvalue_it_mask_recom_family)

## 3. italy_recom_doctors
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_doctors, data = data_italy, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_it_mask_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_it_mask_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_it_mask_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_it_mask_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_it_mask_recom_doctors, paste(LowerCL_it_mask_recom_doctors, UpperCL_it_mask_recom_doctors, sep = "-"), pvalue_it_mask_recom_doctors)

## 4. italy_recom_polit
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_polit, data = data_italy, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_it_mask_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_it_mask_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_it_mask_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_it_mask_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_it_mask_recom_polit, paste(LowerCL_it_mask_recom_polit, UpperCL_it_mask_recom_polit, sep = "-"), pvalue_it_mask_recom_polit)

italy_mask <- rbind(z1, z2, z3, z4)

##################################
##  6.3       japan             ##
##################################

## 1. japan_conformity
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + conformity, data = data_japan, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_jp_mask_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_jp_mask_conformity <- round(exp(y[20, 1]), 2)
UpperCL_jp_mask_conformity <- round(exp(y[20, 2]), 2)
pvalue_jp_mask_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_jp_mask_conformity, paste(LowerCL_jp_mask_conformity, UpperCL_jp_mask_conformity, sep = "-"), pvalue_jp_mask_conformity)

## 2. japan_recom_family
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_family, data = data_japan, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_jp_mask_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_jp_mask_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_jp_mask_recom_family <- round(exp(y[20, 2]), 2)
pvalue_jp_mask_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_jp_mask_recom_family, paste(LowerCL_jp_mask_recom_family, UpperCL_jp_mask_recom_family, sep = "-"), pvalue_jp_mask_recom_family)

## 3. japan_recom_doctors
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_doctors, data = data_japan, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_jp_mask_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_jp_mask_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_jp_mask_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_jp_mask_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_jp_mask_recom_doctors, paste(LowerCL_jp_mask_recom_doctors, UpperCL_jp_mask_recom_doctors, sep = "-"), pvalue_jp_mask_recom_doctors)

## 4. japan_recom_polit
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_polit, data = data_japan, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_jp_mask_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_jp_mask_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_jp_mask_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_jp_mask_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_jp_mask_recom_polit, paste(LowerCL_jp_mask_recom_polit, UpperCL_jp_mask_recom_polit, sep = "-"), pvalue_jp_mask_recom_polit)

japan_mask <- rbind(z1, z2, z3, z4)

##################################
##  6.4       korea             ##
##################################

## 1. korea_conformity
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + conformity, data = data_korea, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ko_mask_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ko_mask_conformity <- round(exp(y[20, 1]), 2)
UpperCL_ko_mask_conformity <- round(exp(y[20, 2]), 2)
pvalue_ko_mask_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_ko_mask_conformity, paste(LowerCL_ko_mask_conformity, UpperCL_ko_mask_conformity, sep = "-"), pvalue_ko_mask_conformity)

## 2. korea_recom_family
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_family, data = data_korea, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ko_mask_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ko_mask_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_ko_mask_recom_family <- round(exp(y[20, 2]), 2)
pvalue_ko_mask_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_ko_mask_recom_family, paste(LowerCL_ko_mask_recom_family, UpperCL_ko_mask_recom_family, sep = "-"), pvalue_ko_mask_recom_family)

## 3. korea_recom_doctors
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_doctors, data = data_korea, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ko_mask_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ko_mask_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_ko_mask_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_ko_mask_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_ko_mask_recom_doctors, paste(LowerCL_ko_mask_recom_doctors, UpperCL_ko_mask_recom_doctors, sep = "-"), pvalue_ko_mask_recom_doctors)

## 4. korea_recom_polit
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_polit, data = data_korea, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ko_mask_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ko_mask_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_ko_mask_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_ko_mask_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_ko_mask_recom_polit, paste(LowerCL_ko_mask_recom_polit, UpperCL_ko_mask_recom_polit, sep = "-"), pvalue_ko_mask_recom_polit)

korea_mask <- rbind(z1, z2, z3, z4)

##################################
##  6.5       uk             ##
##################################

## 1. uk_conformity
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + conformity, data = data_uk, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk_mask_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_uk_mask_conformity <- round(exp(y[20, 1]), 2)
UpperCL_uk_mask_conformity <- round(exp(y[20, 2]), 2)
pvalue_uk_mask_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_uk_mask_conformity, paste(LowerCL_uk_mask_conformity, UpperCL_uk_mask_conformity, sep = "-"), pvalue_uk_mask_conformity)

## 2. uk_recom_family
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_family, data = data_uk, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk_mask_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_uk_mask_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_uk_mask_recom_family <- round(exp(y[20, 2]), 2)
pvalue_uk_mask_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_uk_mask_recom_family, paste(LowerCL_uk_mask_recom_family, UpperCL_uk_mask_recom_family, sep = "-"), pvalue_uk_mask_recom_family)

## 3. uk_recom_doctors
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_doctors, data = data_uk, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk_mask_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_uk_mask_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_uk_mask_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_uk_mask_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_uk_mask_recom_doctors, paste(LowerCL_uk_mask_recom_doctors, UpperCL_uk_mask_recom_doctors, sep = "-"), pvalue_uk_mask_recom_doctors)

## 4. uk_recom_polit
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_polit, data = data_uk, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk_mask_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_uk_mask_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_uk_mask_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_uk_mask_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_uk_mask_recom_polit, paste(LowerCL_uk_mask_recom_polit, UpperCL_uk_mask_recom_polit, sep = "-"), pvalue_uk_mask_recom_polit)

uk_mask <- rbind(z1, z2, z3, z4)

##################################
##  6.6       us                ##
##################################

## 1. us_conformity
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + conformity, data = data_us, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us_mask_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_us_mask_conformity <- round(exp(y[20, 1]), 2)
UpperCL_us_mask_conformity <- round(exp(y[20, 2]), 2)
pvalue_us_mask_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_us_mask_conformity, paste(LowerCL_us_mask_conformity, UpperCL_us_mask_conformity, sep = "-"), pvalue_us_mask_conformity)

## 2. us_recom_family
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_family, data = data_us, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us_mask_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_us_mask_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_us_mask_recom_family <- round(exp(y[20, 2]), 2)
pvalue_us_mask_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_us_mask_recom_family, paste(LowerCL_us_mask_recom_family, UpperCL_us_mask_recom_family, sep = "-"), pvalue_us_mask_recom_family)

## 3. us_recom_doctors
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_doctors, data = data_us, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us_mask_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_us_mask_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_us_mask_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_us_mask_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_us_mask_recom_doctors, paste(LowerCL_us_mask_recom_doctors, UpperCL_us_mask_recom_doctors, sep = "-"), pvalue_us_mask_recom_doctors)

## 4. us_recom_polit
result <- glm(mask_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + mask_before + recom_polit, data = data_us, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us_mask_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_us_mask_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_us_mask_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_us_mask_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_us_mask_recom_polit, paste(LowerCL_us_mask_recom_polit, UpperCL_us_mask_recom_polit, sep = "-"), pvalue_us_mask_recom_polit)

us_mask <- rbind(z1, z2, z3, z4)

mask_behav_change <- cbind(china_mask, italy_mask, japan_mask, korea_mask, uk_mask, us_mask)
write.csv(mask_behav_change, "mask_behav_change.csv")

## 7. Estimation of the effects of each cue to actions on washing hands/ using hand sanitizor ####

##################################
##  7.1       China             ##
##################################

## 1. china_conformity
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + conformity, data = data_china, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ch_wash_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ch_wash_conformity <- round(exp(y[20, 1]), 2)
UpperCL_ch_wash_conformity <- round(exp(y[20, 2]), 2)
pvalue_ch_wash_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_ch_wash_conformity, paste(LowerCL_ch_wash_conformity, UpperCL_ch_wash_conformity, sep = "-"), pvalue_ch_wash_conformity)

## 2. china_recom_family
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_family, data = data_china, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ch_wash_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ch_wash_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_ch_wash_recom_family <- round(exp(y[20, 2]), 2)
pvalue_ch_wash_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_ch_wash_recom_family, paste(LowerCL_ch_wash_recom_family, UpperCL_ch_wash_recom_family, sep = "-"), pvalue_ch_wash_recom_family)

## 3. china_recom_doctors
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_doctors, data = data_china, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ch_wash_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ch_wash_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_ch_wash_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_ch_wash_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_ch_wash_recom_doctors, paste(LowerCL_ch_wash_recom_doctors, UpperCL_ch_wash_recom_doctors, sep = "-"), pvalue_ch_wash_recom_doctors)

## 4. china_recom_polit
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_polit, data = data_china, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ch_wash_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ch_wash_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_ch_wash_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_ch_wash_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_ch_wash_recom_polit, paste(LowerCL_ch_wash_recom_polit, UpperCL_ch_wash_recom_polit, sep = "-"), pvalue_ch_wash_recom_polit)

china_mask <- rbind(z1, z2, z3, z4)

##################################
##  7.2       italy             ##
##################################

## 1. italy_conformity
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + conformity, data = data_italy, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_it_wash_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_it_wash_conformity <- round(exp(y[20, 1]), 2)
UpperCL_it_wash_conformity <- round(exp(y[20, 2]), 2)
pvalue_it_wash_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_it_wash_conformity, paste(LowerCL_it_wash_conformity, UpperCL_it_wash_conformity, sep = "-"), pvalue_it_wash_conformity)

## 2. italy_recom_family
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_family, data = data_italy, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_it_wash_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_it_wash_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_it_wash_recom_family <- round(exp(y[20, 2]), 2)
pvalue_it_wash_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_it_wash_recom_family, paste(LowerCL_it_wash_recom_family, UpperCL_it_wash_recom_family, sep = "-"), pvalue_it_wash_recom_family)

## 3. italy_recom_doctors
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_doctors, data = data_italy, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_it_wash_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_it_wash_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_it_wash_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_it_wash_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_it_wash_recom_doctors, paste(LowerCL_it_wash_recom_doctors, UpperCL_it_wash_recom_doctors, sep = "-"), pvalue_it_wash_recom_doctors)

## 4. italy_recom_polit
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_polit, data = data_italy, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_it_wash_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_it_wash_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_it_wash_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_it_wash_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_it_wash_recom_polit, paste(LowerCL_it_wash_recom_polit, UpperCL_it_wash_recom_polit, sep = "-"), pvalue_it_wash_recom_polit)

italy_mask <- rbind(z1, z2, z3, z4)

##################################
##  7.3       japan             ##
##################################

## 1. japan_conformity
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + conformity, data = data_japan, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_jp_wash_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_jp_wash_conformity <- round(exp(y[20, 1]), 2)
UpperCL_jp_wash_conformity <- round(exp(y[20, 2]), 2)
pvalue_jp_wash_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_jp_wash_conformity, paste(LowerCL_jp_wash_conformity, UpperCL_jp_wash_conformity, sep = "-"), pvalue_jp_wash_conformity)

## 2. japan_recom_family
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_family, data = data_japan, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_jp_wash_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_jp_wash_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_jp_wash_recom_family <- round(exp(y[20, 2]), 2)
pvalue_jp_wash_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_jp_wash_recom_family, paste(LowerCL_jp_wash_recom_family, UpperCL_jp_wash_recom_family, sep = "-"), pvalue_jp_wash_recom_family)

## 3. japan_recom_doctors
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_doctors, data = data_japan, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_jp_wash_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_jp_wash_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_jp_wash_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_jp_wash_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_jp_wash_recom_doctors, paste(LowerCL_jp_wash_recom_doctors, UpperCL_jp_wash_recom_doctors, sep = "-"), pvalue_jp_wash_recom_doctors)

## 7. japan_recom_polit
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_polit, data = data_japan, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_jp_wash_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_jp_wash_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_jp_wash_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_jp_wash_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_jp_wash_recom_polit, paste(LowerCL_jp_wash_recom_polit, UpperCL_jp_wash_recom_polit, sep = "-"), pvalue_jp_wash_recom_polit)

japan_mask <- rbind(z1, z2, z3, z4)

##################################
##  7.4       korea             ##
##################################

## 1. korea_conformity
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + conformity, data = data_korea, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ko_wash_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ko_wash_conformity <- round(exp(y[20, 1]), 2)
UpperCL_ko_wash_conformity <- round(exp(y[20, 2]), 2)
pvalue_ko_wash_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_ko_wash_conformity, paste(LowerCL_ko_wash_conformity, UpperCL_ko_wash_conformity, sep = "-"), pvalue_ko_wash_conformity)

## 2. korea_recom_family
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_family, data = data_korea, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ko_wash_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ko_wash_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_ko_wash_recom_family <- round(exp(y[20, 2]), 2)
pvalue_ko_wash_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_ko_wash_recom_family, paste(LowerCL_ko_wash_recom_family, UpperCL_ko_wash_recom_family, sep = "-"), pvalue_ko_wash_recom_family)

## 3. korea_recom_doctors
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_doctors, data = data_korea, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ko_wash_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ko_wash_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_ko_wash_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_ko_wash_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_ko_wash_recom_doctors, paste(LowerCL_ko_wash_recom_doctors, UpperCL_ko_wash_recom_doctors, sep = "-"), pvalue_ko_wash_recom_doctors)

## 4. korea_recom_polit
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_polit, data = data_korea, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ko_wash_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ko_wash_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_ko_wash_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_ko_wash_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_ko_wash_recom_polit, paste(LowerCL_ko_wash_recom_polit, UpperCL_ko_wash_recom_polit, sep = "-"), pvalue_ko_wash_recom_polit)

korea_mask <- rbind(z1, z2, z3, z4)

##################################
##  7.5       uk             ##
##################################

## 1. uk_conformity
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + conformity, data = data_uk, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk_wash_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_uk_wash_conformity <- round(exp(y[20, 1]), 2)
UpperCL_uk_wash_conformity <- round(exp(y[20, 2]), 2)
pvalue_uk_wash_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_uk_wash_conformity, paste(LowerCL_uk_wash_conformity, UpperCL_uk_wash_conformity, sep = "-"), pvalue_uk_wash_conformity)

## 2. uk_recom_family
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_family, data = data_uk, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk_wash_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_uk_wash_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_uk_wash_recom_family <- round(exp(y[20, 2]), 2)
pvalue_uk_wash_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_uk_wash_recom_family, paste(LowerCL_uk_wash_recom_family, UpperCL_uk_wash_recom_family, sep = "-"), pvalue_uk_wash_recom_family)

## 3. uk_recom_doctors
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_doctors, data = data_uk, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk_wash_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_uk_wash_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_uk_wash_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_uk_wash_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_uk_wash_recom_doctors, paste(LowerCL_uk_wash_recom_doctors, UpperCL_uk_wash_recom_doctors, sep = "-"), pvalue_uk_wash_recom_doctors)

## 4. uk_recom_polit
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_polit, data = data_uk, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk_wash_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_uk_wash_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_uk_wash_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_uk_wash_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_uk_wash_recom_polit, paste(LowerCL_uk_wash_recom_polit, UpperCL_uk_wash_recom_polit, sep = "-"), pvalue_uk_wash_recom_polit)

uk_mask <- rbind(z1, z2, z3, z4)

##################################
##  7.6       us                ##
##################################

## 1. us_conformity
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + conformity, data = data_us, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us_wash_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_us_wash_conformity <- round(exp(y[20, 1]), 2)
UpperCL_us_wash_conformity <- round(exp(y[20, 2]), 2)
pvalue_us_wash_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_us_wash_conformity, paste(LowerCL_us_wash_conformity, UpperCL_us_wash_conformity, sep = "-"), pvalue_us_wash_conformity)

## 2. us_recom_family
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_family, data = data_us, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us_wash_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_us_wash_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_us_wash_recom_family <- round(exp(y[20, 2]), 2)
pvalue_us_wash_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_us_wash_recom_family, paste(LowerCL_us_wash_recom_family, UpperCL_us_wash_recom_family, sep = "-"), pvalue_us_wash_recom_family)

## 3. us_recom_doctors
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_doctors, data = data_us, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us_wash_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_us_wash_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_us_wash_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_us_wash_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_us_wash_recom_doctors, paste(LowerCL_us_wash_recom_doctors, UpperCL_us_wash_recom_doctors, sep = "-"), pvalue_us_wash_recom_doctors)

## 4. us_recom_polit
result <- glm(wash_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + wash_before + recom_polit, data = data_us, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us_wash_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_us_wash_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_us_wash_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_us_wash_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_us_wash_recom_polit, paste(LowerCL_us_wash_recom_polit, UpperCL_us_wash_recom_polit, sep = "-"), pvalue_us_wash_recom_polit)

us_mask <- rbind(z1, z2, z3, z4)

wash_behav_change <- cbind(china_mask, italy_mask, japan_mask, korea_mask, uk_mask, us_mask)
write.csv(wash_behav_change, "wash_behav_change.csv")

## 8. Estimation of the effects of each cue to actions on avoiding social gathering ##############

##################################
##  8.1       China             ##
##################################

## 1. china_conformity
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + conformity, data = data_china, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ch_gather_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ch_gather_conformity <- round(exp(y[20, 1]), 2)
UpperCL_ch_gather_conformity <- round(exp(y[20, 2]), 2)
pvalue_ch_gather_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_ch_gather_conformity, paste(LowerCL_ch_gather_conformity, UpperCL_ch_gather_conformity, sep = "-"), pvalue_ch_gather_conformity)

## 2. china_recom_family
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_family, data = data_china, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ch_gather_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ch_gather_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_ch_gather_recom_family <- round(exp(y[20, 2]), 2)
pvalue_ch_gather_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_ch_gather_recom_family, paste(LowerCL_ch_gather_recom_family, UpperCL_ch_gather_recom_family, sep = "-"), pvalue_ch_gather_recom_family)

## 3. china_recom_doctors
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_doctors, data = data_china, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ch_gather_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ch_gather_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_ch_gather_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_ch_gather_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_ch_gather_recom_doctors, paste(LowerCL_ch_gather_recom_doctors, UpperCL_ch_gather_recom_doctors, sep = "-"), pvalue_ch_gather_recom_doctors)

## 4. china_recom_polit
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_polit, data = data_china, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ch_gather_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ch_gather_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_ch_gather_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_ch_gather_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_ch_gather_recom_polit, paste(LowerCL_ch_gather_recom_polit, UpperCL_ch_gather_recom_polit, sep = "-"), pvalue_ch_gather_recom_polit)

china_mask <- rbind(z1, z2, z3, z4)

##################################
##  8.2       italy             ##
##################################

## 1. italy_conformity
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + conformity, data = data_italy, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_it_gather_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_it_gather_conformity <- round(exp(y[20, 1]), 2)
UpperCL_it_gather_conformity <- round(exp(y[20, 2]), 2)
pvalue_it_gather_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_it_gather_conformity, paste(LowerCL_it_gather_conformity, UpperCL_it_gather_conformity, sep = "-"), pvalue_it_gather_conformity)

## 2. italy_recom_family
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_family, data = data_italy, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_it_gather_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_it_gather_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_it_gather_recom_family <- round(exp(y[20, 2]), 2)
pvalue_it_gather_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_it_gather_recom_family, paste(LowerCL_it_gather_recom_family, UpperCL_it_gather_recom_family, sep = "-"), pvalue_it_gather_recom_family)

## 3. italy_recom_doctors
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_doctors, data = data_italy, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_it_gather_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_it_gather_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_it_gather_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_it_gather_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_it_gather_recom_doctors, paste(LowerCL_it_gather_recom_doctors, UpperCL_it_gather_recom_doctors, sep = "-"), pvalue_it_gather_recom_doctors)

## 4. italy_recom_polit
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_polit, data = data_italy, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_it_gather_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_it_gather_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_it_gather_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_it_gather_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_it_gather_recom_polit, paste(LowerCL_it_gather_recom_polit, UpperCL_it_gather_recom_polit, sep = "-"), pvalue_it_gather_recom_polit)

italy_mask <- rbind(z1, z2, z3, z4)

##################################
##  8.3       japan             ##
##################################

## 1. japan_conformity
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + conformity, data = data_japan, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_jp_gather_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_jp_gather_conformity <- round(exp(y[20, 1]), 2)
UpperCL_jp_gather_conformity <- round(exp(y[20, 2]), 2)
pvalue_jp_gather_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_jp_gather_conformity, paste(LowerCL_jp_gather_conformity, UpperCL_jp_gather_conformity, sep = "-"), pvalue_jp_gather_conformity)

## 2. japan_recom_family
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_family, data = data_japan, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_jp_gather_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_jp_gather_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_jp_gather_recom_family <- round(exp(y[20, 2]), 2)
pvalue_jp_gather_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_jp_gather_recom_family, paste(LowerCL_jp_gather_recom_family, UpperCL_jp_gather_recom_family, sep = "-"), pvalue_jp_gather_recom_family)

## 3. japan_recom_doctors
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_doctors, data = data_japan, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_jp_gather_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_jp_gather_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_jp_gather_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_jp_gather_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_jp_gather_recom_doctors, paste(LowerCL_jp_gather_recom_doctors, UpperCL_jp_gather_recom_doctors, sep = "-"), pvalue_jp_gather_recom_doctors)

## 4. japan_recom_polit
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_polit, data = data_japan, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_jp_gather_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_jp_gather_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_jp_gather_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_jp_gather_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_jp_gather_recom_polit, paste(LowerCL_jp_gather_recom_polit, UpperCL_jp_gather_recom_polit, sep = "-"), pvalue_jp_gather_recom_polit)

japan_mask <- rbind(z1, z2, z3, z4)

##################################
##  8.4       korea             ##
##################################

## 1. korea_conformity
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + conformity, data = data_korea, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ko_gather_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ko_gather_conformity <- round(exp(y[20, 1]), 2)
UpperCL_ko_gather_conformity <- round(exp(y[20, 2]), 2)
pvalue_ko_gather_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_ko_gather_conformity, paste(LowerCL_ko_gather_conformity, UpperCL_ko_gather_conformity, sep = "-"), pvalue_ko_gather_conformity)

## 2. korea_recom_family
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_family, data = data_korea, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ko_gather_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ko_gather_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_ko_gather_recom_family <- round(exp(y[20, 2]), 2)
pvalue_ko_gather_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_ko_gather_recom_family, paste(LowerCL_ko_gather_recom_family, UpperCL_ko_gather_recom_family, sep = "-"), pvalue_ko_gather_recom_family)

## 3. korea_recom_doctors
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_doctors, data = data_korea, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ko_gather_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ko_gather_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_ko_gather_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_ko_gather_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_ko_gather_recom_doctors, paste(LowerCL_ko_gather_recom_doctors, UpperCL_ko_gather_recom_doctors, sep = "-"), pvalue_ko_gather_recom_doctors)

## 4. korea_recom_polit
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_polit, data = data_korea, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_ko_gather_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_ko_gather_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_ko_gather_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_ko_gather_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_ko_gather_recom_polit, paste(LowerCL_ko_gather_recom_polit, UpperCL_ko_gather_recom_polit, sep = "-"), pvalue_ko_gather_recom_polit)

korea_mask <- rbind(z1, z2, z3, z4)

##################################
##  8.5       uk             ##
##################################

## 1. uk_conformity
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + conformity, data = data_uk, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk_gather_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_uk_gather_conformity <- round(exp(y[20, 1]), 2)
UpperCL_uk_gather_conformity <- round(exp(y[20, 2]), 2)
pvalue_uk_gather_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_uk_gather_conformity, paste(LowerCL_uk_gather_conformity, UpperCL_uk_gather_conformity, sep = "-"), pvalue_uk_gather_conformity)

## 2. uk_recom_family
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_family, data = data_uk, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk_gather_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_uk_gather_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_uk_gather_recom_family <- round(exp(y[20, 2]), 2)
pvalue_uk_gather_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_uk_gather_recom_family, paste(LowerCL_uk_gather_recom_family, UpperCL_uk_gather_recom_family, sep = "-"), pvalue_uk_gather_recom_family)

## 3. uk_recom_doctors
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_doctors, data = data_uk, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk_gather_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_uk_gather_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_uk_gather_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_uk_gather_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_uk_gather_recom_doctors, paste(LowerCL_uk_gather_recom_doctors, UpperCL_uk_gather_recom_doctors, sep = "-"), pvalue_uk_gather_recom_doctors)

## 4. uk_recom_polit
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_polit, data = data_uk, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_uk_gather_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_uk_gather_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_uk_gather_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_uk_gather_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_uk_gather_recom_polit, paste(LowerCL_uk_gather_recom_polit, UpperCL_uk_gather_recom_polit, sep = "-"), pvalue_uk_gather_recom_polit)

uk_mask <- rbind(z1, z2, z3, z4)

##################################
##  8.6       us                ##
##################################

## 1. us_conformity
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + conformity, data = data_us, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us_gather_conformity <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_us_gather_conformity <- round(exp(y[20, 1]), 2)
UpperCL_us_gather_conformity <- round(exp(y[20, 2]), 2)
pvalue_us_gather_conformity <- x$coefficients[20, "Pr(>|z|)"]
z1 <- c(OR_us_gather_conformity, paste(LowerCL_us_gather_conformity, UpperCL_us_gather_conformity, sep = "-"), pvalue_us_gather_conformity)

## 2. us_recom_family
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_family, data = data_us, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us_gather_recom_family <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_us_gather_recom_family <- round(exp(y[20, 1]), 2)
UpperCL_us_gather_recom_family <- round(exp(y[20, 2]), 2)
pvalue_us_gather_recom_family <- x$coefficients[20, "Pr(>|z|)"]
z2 <- c(OR_us_gather_recom_family, paste(LowerCL_us_gather_recom_family, UpperCL_us_gather_recom_family, sep = "-"), pvalue_us_gather_recom_family)

## 3. us_recom_doctors
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_doctors, data = data_us, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us_gather_recom_doctors <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_us_gather_recom_doctors <- round(exp(y[20, 1]), 2)
UpperCL_us_gather_recom_doctors <- round(exp(y[20, 2]), 2)
pvalue_us_gather_recom_doctors <- x$coefficients[20, "Pr(>|z|)"]
z3 <- c(OR_us_gather_recom_doctors, paste(LowerCL_us_gather_recom_doctors, UpperCL_us_gather_recom_doctors, sep = "-"), pvalue_us_gather_recom_doctors)

## 4. us_recom_polit
result <- glm(gather_now ~ as.numeric(factor(region)) + age65 + gender + pubtrans + living_area + living_arrangement + income_group + neg_nonfin_anxiety + belief_you_got_infected + belief_inf_serious_hosp + belief_policy_effectiveness_gather + gather_before + recom_polit, data = data_us, family = binomial)

#Calculating odds ratios
x <- summary(ModelName)
y <- confint(ModelName, level=1-alpha)
OR_us_gather_recom_polit <- round(exp(x$coefficients[20, 1]), 2)
LowerCL_us_gather_recom_polit <- round(exp(y[20, 1]), 2)
UpperCL_us_gather_recom_polit <- round(exp(y[20, 2]), 2)
pvalue_us_gather_recom_polit <- x$coefficients[20, "Pr(>|z|)"]
z4 <- c(OR_us_gather_recom_polit, paste(LowerCL_us_gather_recom_polit, UpperCL_us_gather_recom_polit, sep = "-"), pvalue_us_gather_recom_polit)

us_mask <- rbind(z1, z2, z3, z4)

gather_behav_change <- cbind(china_mask, italy_mask, japan_mask, korea_mask, uk_mask, us_mask)
write.csv(gather_behav_change, "gather_behav_change.csv")
