# source("../3_PublicIDandClean.R")
# source("../4_BCRA.R")
# source("../5_Bootstrap.R")

#Read in Data
wd = "C:/Users/divya/Documents/JHU/Y2/Q1/Advanced Data Science 711/Term 1 Project/Data"
setwd(wd)
public_sgk_female_age35_race = read.csv(file.path(paste("public_sgk_female_age35_race_", "2016-10-02", "_raw.csv", sep = "")), header = TRUE)
public_sgk_female_age35_race$from_id = gsub("\\s", "", format(public_sgk_female_age35_race$from_id, scientific = FALSE))
public_sgk_female_age35_race_BCRA = read.csv(file.path(paste("public_sgk_female_age35_race_BCRA_", "2016-10-02", "_raw.csv", sep = "")), header = TRUE)
public_sgk_female_age35_race_BCRA$from_id = gsub("\\s", "", format(public_sgk_female_age35_race_BCRA$from_id, scientific = FALSE))
public_sgk_female_age35_race_BCRA$race.predict = as.character(public_sgk_female_age35_race_BCRA$racepredict)
age_subset_merge = read.csv(file.path(paste("C:/Users/divya/Documents/JHU/Y2/Q1/Advanced Data Science 711/Term 1 Project/Data/age_subset_merge", "_", "2016-10-02", "_raw.csv", sep = "")))

library(ggplot2); library(lubridate)
par(mfrow = c(1,1))
#Race Distribution
# jpeg('../Figures/race_dist_plot.jpg')
print(ggplot(public_sgk_female_age35_race, aes(racepredict)) + geom_bar(fill="#880011") + ggtitle("Racial Distribution of Public Facebook Users") + labs(x="Race", y="Count by\nRacial Category"))
# dev.off()

#Age Distribution
# jpeg('../Figures/agecat_dist_plot.jpg')
print(ggplot(public_sgk_female_age35_race, aes(age_cat)) + geom_bar(fill="#880011") + ggtitle("Age Distribution of Public Facebook Users") + labs(x="Age", y="Count by\nAge Category"))
# dev.off()

#Distributional Plots of Bootstrap Names
age_bootstrap_plot = function(x){
  a = which(age_subset_merge$FirstName == x)
  bs.samp = lapply(1:1000, function(i)
    year(Sys.Date()) - sample(age_subset_merge$year[a], size = 1000, replace = T, prob = age_subset_merge$weighted.prob[a]))
  bs.median = sapply(bs.samp, median)
  se.bs.median = sqrt(var(bs.median))
  plot = qplot(bs.median, binwidth = 0.5, xlab = "Median Age", main = paste("Distribution of Bootstrap Ages of ", x, sep = ""))
  return(list(se.bs.median, print(plot)))#data.frame(bs.median, se.bs.median))
}
unique_names_final = as.character(unique(public_sgk_female_age35_race_BCRA$FirstName))
sampname = sample(unique_names_final, 4)
sapply(sampname, age_bootstrap_plot)

df = public_sgk_female_age35_race_BCRA
df$color = ifelse(df$Race == "white", "blue", ifelse(df$Race == "white", "red", ""))
risk_by_race = ggplot(df) + geom_histogram(aes(abs.risk)) + facet_wrap(~racepredict) + labs(title = "Distribution of Absolute Risk of Breast Cancer Across Races", x = "Absolute Risk of Breast Cancer")
print(risk_by_race)
rm(df)

#BS_name1 = #[[2]]$plot
# BS_name2 = sampname_plots[[4]]$plot 
# BS_name3 = sampname_plots[[6]]$plot
# BS_name4 = sampname_plots[[8]]$plot

