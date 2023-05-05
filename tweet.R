tweet_data <- list()

for (i in 1:1239){
  tweet_data[[i]] = read.csv(paste("timeline_users/timeline_", final_assignments$id_str[i], ".csv", sep=""))
}


read.csv(paste("timeline_users/timeline_", "918628508418449408", ".csv", sep=""))

