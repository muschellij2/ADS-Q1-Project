##Packages
ipak = function(pkg){
  new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Usage
packages = c("gender", "tidyr", "dplyr", "data.table", "wru", "readr", "knitr", "ggplot2", "BCRA")
ipak(packages)


#Read in Data
wd = "C:/Users/divya/Documents/JHU/Y2/Q1/Advanced Data Science 711/Term 1 Project/Data"
setwd(wd)
filedate = "2016-10-02" #dateDownloadYMD
sgk_data = read.csv(file.path(paste("sgk_", filedate, "_raw.csv", sep = "")), header = TRUE)
sgk_data = sgk_data[,-1]; 

#Cleaning up White space in IDs
sgk_data$from_id = gsub("\\s", "", format(sgk_data$from_id, scientific = FALSE))

#Extract IDs from public profiles 
extractPubID = function(ids){
  out = tryCatch(
    {
      extractid = getUsers(users = as.character(ids), token = fb_oauth)
      extractid$id
    }, 
    error = function(cond){
      # Choose a return value in case of error
      return(NA)
    }
  )
  return(out)
}

if(!file.exists(paste("idstatus_",filedate,"_raw.csv", sep = ""))){
  ptm_start = proc.time() #start time 
  idstatus = as.vector(unlist(sapply(sgk_data[,1], extractPubID)))
  ptm_stop = proc.time() - ptm_start #stop time 
  write_csv(as.data.frame(idstatus), paste("C:/Users/divya/Documents/JHU/Y2/Q1/Advanced Data Science 711/Term 1 Project/Data/idstatus", "_", filedate, "_raw.csv", sep = ""))
} else {
  idstatus = read.csv(file.path(paste("C:/Users/divya/Documents/JHU/Y2/Q1/Advanced Data Science 711/Term 1 Project/Data/idstatus", "_", filedate, "_raw.csv", sep = "")), header = TRUE) 
  idstatus = gsub("\\s", "", format(idstatus$idstatus, scientific = FALSE))
  # idstatus = format(idstatus$idstatus, scientific = FALSE)
}
#sum(!is.na(idstatus)) #Number of public profiles
#length(unique(idstatus[which(!is.na(idstatus))])) #Number of unique public profiles 
# which(sgk_data$from_id %in% unique(idstatus[which(!is.na(idstatus))])) #which are public in original DF

#Dataframe of Unique Public Profiles 
public_sgk_data = unique(sgk_data[which(sgk_data$from_id %in% unique(idstatus[which(!is.na(idstatus))])),c("from_id", "from_name")])

#Remove rows with non-ASCII characters and "Komen" in name
public_sgk_data$from_name = iconv(public_sgk_data$from_name, "UTF-8", "ASCII")
public_sgk_data = public_sgk_data[-c(which(is.na(public_sgk_data$from_name)), grep("Komen",public_sgk_data$from_name), grep("<", public_sgk_data$from_name)),]

#Identify genders of public individuals 
library(gender); library(tidyr); library(dplyr)

#Split names into first and last to identify gender and race
public_sgk_data_splitname = extract(public_sgk_data, from_name, c("FirstName", "LastName"), "([^ ]+) (.*)")
whichfirstNA = which(is.na(public_sgk_data_splitname))
#unique(whichfirstNA %% nrow(public_sgk_data_splitname)) #IDs of individual with only one name 
public_sgk_data_splitname$FirstName[unique(whichfirstNA %% nrow(public_sgk_data_splitname))] = public_sgk_data$from_name[unique(whichfirstNA %% nrow(public_sgk_data_splitname))]

#Merge split name data
public_sgk_data_merge = left_join(public_sgk_data, public_sgk_data_splitname)
#Remove individuals with only a first name 
public_sgk_data_merge = public_sgk_data_merge[-unique(whichfirstNA %% nrow(public_sgk_data_splitname)),] 

#Determine and recode gender. Male = 0, Female = 1
public_sgk_data_gender = gender(public_sgk_data_merge$FirstName, years = c(1930, 2012))
public_sgk_data_gender$gender[public_sgk_data_gender$gender %in% "male"] <- 0 
public_sgk_data_gender$gender[public_sgk_data_gender$gender %in% "female"] <- 1
public_sgk_data_gender = public_sgk_data_gender[,c("name", "gender")]
names(public_sgk_data_gender)[1] = "FirstName"

#Merge split name and gender data 
public_sgk_data_merge = unique(left_join(public_sgk_data_merge, public_sgk_data_gender))

#Subset by females
public_sgk_data_merge_female = public_sgk_data_merge %>% 
  select(from_id, from_name, FirstName, LastName, gender) %>% 
  filter(gender == 1)

#########################
### AGE DETERMINATION ###
#########################

#Source in Age Data
setwd("..")
source("2_AgeData.R")
setwd("..")

#Which names in the nameslist (SSA data) are in my Facebook dataset?
names_in_fem_merge = data.frame(FirstName = nameslist, InMerge = nameslist %in% unique(public_sgk_data_merge_female$FirstName))
unique_names_in_fem_merge = as.character(names_in_fem_merge$FirstName[which(names_in_fem_merge$InMerge)])

#Merge SSA data with FB dataset
age_sort_subset = left_join(age_sort, names_in_fem_merge)
age_sort_subset = age_sort_subset[-which(age_sort_subset$InMerge == F), ]

library(data.table)
age_sort_subset = data.table(age_sort_subset)

#Setup data to calculate median age between 1930 and 2015 based on SSA data
age_new = data.frame(FirstName = rep(unique_names_in_fem_merge, each = 86), year = rep(seq(1930,2015), times = length(unique_names_in_fem_merge)))

#Create frequence and cum.sum columns for age data 
age_subset_merge = right_join(age_sort_subset, age_new)
age_subset_merge$frequency[is.na(age_subset_merge$frequency)] = 0
age_subset_merge = data.table(age_subset_merge)
age_subset_merge = age_subset_merge[, Cum.Sum := cumsum(frequency), by=list(FirstName)]

#Calculate weighted probability of name in each year, based on Cum Sum 
age_subset_merge$weighted.prob = age_subset_merge$frequency / rep(age_subset_merge$Cum.Sum[seq(86, nrow(age_subset_merge), by = 86)], each = 86)

#Function to calculate grouped mean 
grouped_median = function(x){
  a = which(age_subset_merge$FirstName == unique_names_in_fem_merge[x])
  v = age_subset_merge$Cum.Sum[a] # create two bins [5,10) and [10,15)
  med_value = (age_subset_merge$Cum.Sum[max(a)]/2) + 0.5
  findInterval(med_value, v)
  
  L = age_subset_merge$year[findInterval(med_value, v)]+1 #Lower class boundary of med group
  n = v[length(a)] # number of obs
  B = v[findInterval(med_value, v)] #Cum freq of group before med group
  G = age_subset_merge$frequency[a[findInterval(med_value, v)+1]] #Freq of med group
  w = 1 #group width
  median_estimate = L + ((w/G)*((n/2) - B))
  return(list(FirstName = unique_names_in_fem_merge[x], MedianYr = median_estimate))
}

#Create temporary dataframe with grouped median per name, calculate age based on median yr 
output = as.data.frame(t(sapply(1:length(unique_names_in_fem_merge), grouped_median)))
output$age = year(Sys.Date()) - as.numeric(output$MedianYr)
output$FirstName = as.character(output$FirstName); output$MedianYr = as.numeric(output$MedianYr)

public_sgk_data_merge_female_age = left_join(public_sgk_data_merge_female, output)
# rm(output)
public_sgk_data_merge_female_age35 = public_sgk_data_merge_female_age[which(public_sgk_data_merge_female_age$age > 35),]

#Categorize age
public_sgk_data_merge_female_age35$age_cat = as.factor(findInterval(public_sgk_data_merge_female_age35$age, seq(35, 85, by = 5)))
levels(public_sgk_data_merge_female_age35$age_cat) = c("[35-40)", "[40-45)", "[45-50)", "[50-55)", "[55-60)", "[60-65)", "[65-70)", "[70-75)", "[75-80)", "[80-85)")

##########################
### RACE DETERMINATION ###
##########################

library(wru); library(readr)

names(public_sgk_data_merge_female) = c("from_id", "from_name", "FirstName", "surname", "sex")
race_pred = race.pred(public_sgk_data_merge_female, races = c("white", "black", "latino", "asian", "other"), surname.only = T)
names(public_sgk_data_merge_female)[which(names(public_sgk_data_merge_female) == "surname")] = "LastName"
names(race_pred)[which(names(race_pred) == "surname")] = "LastName"

#Determine race with max probability 
public_sgk_data_race = race_pred %>% 
  mutate_each(funs(round(.,3)), pred.whi, pred.bla, pred.his, pred.asi, pred.oth) %>% 
  rowwise() %>% 
  mutate(race_max = max(pred.whi, pred.bla, pred.his, pred.asi, pred.oth))
race_select = public_sgk_data_race %>% 
  select(pred.whi, pred.bla, pred.his, pred.asi, pred.oth)
public_sgk_data_race$racepredict = colnames(race_select)[max.col(race_select)]; rm(race_select)

#Rename the race prediction values
public_sgk_data_race$racepredict[public_sgk_data_race$racepredict %in% "pred.whi"] <- "white" 
public_sgk_data_race$racepredict[public_sgk_data_race$racepredict %in% "pred.bla"] <- "black"
public_sgk_data_race$racepredict[public_sgk_data_race$racepredict %in% "pred.his"] <- "hisp"
public_sgk_data_race$racepredict[public_sgk_data_race$racepredict %in% "pred.asi"] <- "asian"
public_sgk_data_race$racepredict[public_sgk_data_race$racepredict %in% "pred.oth"] <- "other"

public_sgk_data_race$racepredict = as.factor(public_sgk_data_race$racepredict)

## MERGE AGE AND RACE DATA ##
public_sgk_female_age35_race = left_join(public_sgk_data_merge_female_age35, public_sgk_data_race)
public_sgk_female_age35_race$MedianYr = as.numeric(public_sgk_female_age35_race$MedianYr)

write_csv(age_subset_merge, paste("C:/Users/divya/Documents/JHU/Y2/Q1/Advanced Data Science 711/Term 1 Project/Data/age_subset_merge", "_", filedate, "_raw.csv", sep = ""))
write_csv(public_sgk_female_age35_race, paste("C:/Users/divya/Documents/JHU/Y2/Q1/Advanced Data Science 711/Term 1 Project/Data/public_sgk_female_age35_race", "_", filedate, "_raw.csv", sep = ""))



getwd()
# setwd('../../Figures')
# jpeg(paste('race_dist_', dateDownloadYMD,'.jpg', sep = ""))
# barplot(table(public_sgk_female_age35_race$racepredict), main = "Racial Distribution of Public Facebook Users", ylab = "Frequency", xlab = "Race")
# dev.off()


# library(knitr)
# ## @knitr distPlots
# par(mfrow = c(2,1))
# #Race Distribution
# barplot(table(public_sgk_female_age35_race$racepredict), main = "Racial Distribution of Public Facebook Users", ylab = "Frequency", xlab = "Race")
# #Age Distribution
# barplot(table(public_sgk_female_age35_race$age_cat), main = "Age Distribution of Public Facebook Users", ylab = "Frequency", xlab = "Age Category")