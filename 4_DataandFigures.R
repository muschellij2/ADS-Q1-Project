#Read in Data
wd = "C:/Users/divya/Documents/JHU/Y2/Q1/Advanced Data Science 711/Term 1 Project/Data"
setwd(wd)
public_sgk_female_age35_race = read.csv(file.path(paste("public_sgk_female_age35_race_", "2016-10-02", "_raw.csv", sep = "")), header = TRUE) 
public_sgk_female_age35_race$from_id = gsub("\\s", "", format(public_sgk_female_age35_race$from_id, scientific = FALSE))

if (!require("knitr")) install.packages("knitr"); library(knitr)
if (!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)

par(mfrow = c(1,1))
#Race Distribution
print(ggplot(public_sgk_female_age35_race, aes(racepredict)) + geom_bar(fill="#880011") + ggtitle("Racial Distribution of Public Facebook Users") + labs(x="Race", y="Count by\nRacial Category"))
#Age Distribution
print(ggplot(public_sgk_female_age35_race, aes(age_cat)) + geom_bar(fill="#880011") + ggtitle("Age Distribution of Public Facebook Users") + labs(x="Age", y="Count by\nAge Category")) 

# barplot(table(public_sgk_female_age35_race$racepredict), main = "Racial Distribution of Public Facebook Users", ylab = "Frequency", xlab = "Race")
# barplot(table(public_sgk_female_age35_race$age_cat), main = "Age Distribution of Public Facebook Users", ylab = "Frequency", xlab = "Age Category")


#Variability estimates 
#What's the plausible range of predictions you could have given someone? 
