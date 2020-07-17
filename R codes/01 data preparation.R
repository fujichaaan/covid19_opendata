#The original data is publicly available on https://osf.io/aubkc/.
#Before using this code, please download this dataset on your local directory.

###########################################
##     1. Preparation for analysis       ##
###########################################

install.packages("tableone")
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