#####################################################################
##     3. Countries-stratified analysis for wearing a mask         ##
#####################################################################

#Divide data into each country
data_china <- subset(data, country == "china")
data_italy <- subset(data, country == "italy")
data_japan <- subset(data, country == "japan")
data_korea <- subset(data, country == "korea")
data_uk <- subset(data, country == "uk")
data_us <- subset(data, country == "us")

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
