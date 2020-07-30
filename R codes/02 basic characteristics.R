###########################################
##     2. Basic characteristics          ##
###########################################

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
listvar <- c("conformity", "recom_family", "recom_doctors", "recom_polit")
table2 <- CreateTableOne(vars = listvar, factorVars = listcat, strata = "country", data = data)
table2 <- print(table2); write.csv(table2, "Table2.csv")
