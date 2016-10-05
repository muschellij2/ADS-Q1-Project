#Set Working Directory 
wd = "C:/Users/divya/Documents/JHU/Y2/Q1/Advanced Data Science 711/Term 1 Project"
setwd(wd)

#Download Names data from SSA website
download.file("https://www.ssa.gov/oact/babynames/names.zip","./Data/names.zip")
unzip("./Data/names.zip", exdir = "./Data/names_unzip")
file_list = list.files("./Data/names_unzip")
file.rename(from = file.path(wd, "Data", "names_unzip", "NationalReadMe.pdf"), to = file.path(wd, "NationalReadMe.pdf"))
setwd(file.path(wd, "Data", "names_unzip"))
file_list = list.files()
file_1930 = grep("1930", file_list); file_2015 = grep("2015", file_list) 

#Initiate Age Dataframe
age = data.frame(FirstName = c("Test"), gender=c("test"), frequency = c(0), year = c(2016))

yearcombine_start = proc.time()
for (file in file_list[file_1930:file_2015]){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("age")){
    age <- read.table(file, header=TRUE, sep="\t")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("age")){
    temp_dataset = read.table(file, header=FALSE, sep=",")
    fileyear = rep(as.numeric(substr(file, 4, 7)), nrow(temp_dataset))
    age_yr = cbind(temp_dataset, fileyear)
    names(age_yr) = names(age)
    age = rbind(age, age_yr)
    # Mrow = which(age$gender == "M")
    age = age[-which(age$gender == "M"),]
    rm(temp_dataset, age_yr)
  }
}
yearcombine_stop = proc.time() - yearcombine_start

age = age[-1,]
age_sort = age[order(age$FirstName, age$year),]
nameslist = unique(age_sort$FirstName)