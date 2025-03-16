library(tidyverse)


above_cutoff <- function(x,cutoff){
  counts <- table(x)
  above_cutoff <- names(counts[counts>cutoff])
  x[x %in% above_cutoff]
}


clean_twitter <- function(twitter_data,cutoff=0,stopwords = stopwords::stopwords()){
  clean_text <- twitter_data$text %>% 
    str_replace_all("\\n"," ") %>% 
    str_replace_all("[^[:alnum:] \\#\\@]","") %>% #keep hashtags and @ symbols, remove all other non-alphanumerics
    str_to_lower() %>% 
    str_squish() 
  
  stopwords_clean <- stopwords %>% str_remove_all("[^[:alnum:]]")
  
  word_list <-  str_c(clean_text,collapse = " ") %>% 
    str_split(" ") %>% {.[[1]]} %>% 
    above_cutoff(cutoff) %>% 
    # unique %>%
    {.[!(. %in% stopwords_clean)]}
  
  dfm <- quanteda::dfm(clean_text)
  
  #drop words that did not parse into dfm (generally special characters or non-latin characters)
  word_list <- word_list[word_list %in% dfm@Dimnames$features]
  
  # Create logical vector indicating which elements to keep
  keep <- !grepl("^.$|^\\d$", word_list)
  
  # Subset the original list using the logical vector
  word_list <- word_list[keep]
  
  return(word_list)
}




user_timeline_file = list.files("../timeline_users", full.names = T)

user_timeline_id = gsub("[^0-9]", "", basename(user_timeline_file))

# user_timeline_id[938] = 701258499121029121


# for (i in 1:1143)
# {
#   user_ids.txt <- list.files(paste("../timeline_users/", all_days[i], sep=""), pattern = "[0-9].txt")
#   user_ids <- as.numeric(gsub("([0-9]+).*$", "\\1", user_ids.txt))
#   
#   filenames <- list.files(paste("All_Data/Friends/", all_days[i], sep=""), pattern = "[0-9].txt", full.names = T)
#   network_ids <- lapply(filenames, scan)
#   
#   user_friends_list <- vector(mode = "list", length = length(user_ids))
#   for (j in seq_along(user_ids))
#   {
#     user_friends_list[[j]] <- list(user_ids[j],
#                                    network_ids[[j]])
#   }
#   
# }


all_user_text = data.frame()

for (i in 1:1142)
{
  dat <- read.csv(paste0(user_timeline_file[i]))
  
  if (nrow(dat) != 0 & ncol(dat) == 43){
  clean_text = clean_twitter(twitter_data = dat)
  
  all_user_text = rbind(all_user_text,
                         data.frame(id_str = gsub("[^0-9]", "", basename(user_timeline_file[i])),
                                    text = clean_text))
  }
  print(i)
}




user_word_list = all_user_text %>% group_by(text) %>% summarise(n())


# Save data files
save(all_user_text, file = "all_user_text.RData")
save(user_word_list, file = "user_word_list.RData")


load("all_user_text.RData")
load("user_word_list.RData")


user_word_counts = all_user_text  %>% group_by(id_str, text) %>% summarise(word_counts = n())

# user_word_counts_matrix <- user_word_counts %>%
#   pivot_wider(names_from = id_str, values_from = word_counts)




library(tidytext)
library(Matrix)
user_sparse_W <- user_word_counts %>%
  cast_sparse(row = text, column = id_str, value = word_counts)


dim(user_sparse_W)

save(user_sparse_W, file = "user_sparse_W.RData")



colnames(user_sparse_W)


##### Remove rare words
all_row_sum_W = rowSums(user_sparse_W)
great_3_W = which(all_row_sum_W >= 3)
user_W = user_sparse_W[great_3_W, ]
dim(user_W)

##### Remove just numbers
user_W <- user_W[!grepl("^[0-9]+$", rownames(user_W)), ]
dim(user_W)


# word occurrence matrix 
WtW = t(user_W>0) %*% (user_W>0) 


# Apply Laplacian 
rsWtW = rowSums(WtW)
lap_WtW = diag(rsWtW^{-1/2})%*%WtW%*%diag(rsWtW^{-1/2})

# Spectral clustering and the high-dimensional stochastic blockmodel

dim(lap_WtW)




