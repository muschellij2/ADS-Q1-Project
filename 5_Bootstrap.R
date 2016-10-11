#################################################
### BOOTSTRAP FOR AGE: MEASURE OF UNCERTAINTY ###
#################################################
setwd(wd)

# age_subset_merge

age_bootstrap = function(x){
  a = which(age_subset_merge$FirstName == unique_names_in_fem_merge[x])
  
  bs.samp = lapply(1:1000, function(i)
    year(Sys.Date()) - sample(age_subset_merge$year[a], size = 1000, replace = T, prob = age_subset_merge$weighted.prob[a]))
  bs.median = sapply(bs.samp, median)
  se.bs.median = sqrt(var(bs.median))
  
  return(se.bs.median)#data.frame(bs.median, se.bs.median))
}
age_se = sapply(1:length(unique_names_in_fem_merge), age_bootstrap)
age_se_df = data.frame(FirstName = unique_names_in_fem_merge, SEestimate = age_se)
med_age_se = left_join(public_sgk_data_merge_female_age35, age_se_df)
med_age_se = med_age_se %>% select(from_id, FirstName, MedianYr, age, SEestimate)

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
sampname_plots = sapply(sampname, age_bootstrap_plot)

BS_name1 = sampname_plots[[2]]$plot
BS_name2 = sampname_plots[[4]]$plot 
BS_name3 = sampname_plots[[6]]$plot
BS_name4 = sampname_plots[[8]]$plot

getwd()
jpeg('../Figures/BS_name1.jpg')
sampname_plots[[2]]$plot
dev.off()

jpeg('../Figures/BS_name2.jpg')
sampname_plots[[4]]$plot
dev.off()

jpeg('../Figures/BS_name3.jpg')
sampname_plots[[6]]$plot
dev.off()

jpeg('../Figures/BS_name4.jpg')
sampname_plots[[8]]$plot
dev.off()