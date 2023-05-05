load("All_Data/Final Data with Treatment Assignments.Rdata")
load("friends.RData")
load("experiment.RData")
load("full_experiment_assign.RData")

ggplot2::theme_set(ggplot2::theme_bw())


full_experiment_assign %>% filter(treat == "1", party_ID == "Democrat", Date %in% c("2017-10-16", "2017-11-27", "2018-02-01", "2022-11-13")) %>%
  ggplot(aes(x=ideal_point)) + geom_density(alpha=0.3) + labs(title = "Democrat Treatment", y = "Density", x = "Ideal Points") + facet_wrap(~Date) 

full_experiment_assign %>% filter(treat == "1", party_ID == "Republican", Date %in% c("2017-10-16", "2017-11-27", "2018-02-01", "2022-11-13")) %>%
  ggplot(aes(x=ideal_point)) + geom_density(alpha=0.3) + labs(title = "Republican Treatment", y = "Density", x = "Ideal Points") + facet_wrap(~Date) 

full_experiment_assign %>% filter(treat == "0", party_ID == "Democrat", Date %in% c("2017-10-16", "2017-11-27", "2018-02-01", "2022-11-13")) %>%
  ggplot(aes(x=ideal_point)) + geom_density(alpha=0.3) + labs(title = "Democrat Control", y = "Density", x = "Ideal Points") + facet_wrap(~Date) 

full_experiment_assign %>% filter(treat == "0", party_ID == "Republican", Date %in% c("2017-10-16", "2017-11-27", "2018-02-01", "2022-11-13")) %>%
  ggplot(aes(x=ideal_point)) + geom_density(alpha=0.3) + labs(title = "Republican Control", y = "Density", x = "Ideal Points") + facet_wrap(~Date) 



full_experiment_assign %>% filter(treat == "1", Date %in% c("2017-10-16", "2017-11-27", "2018-02-01", "2022-11-13") ) %>%
  ggplot(aes(x=ideal_point, color = party_ID)) + geom_density(alpha=0.3) + 
  labs(title = "All Treatment", y = "Density", x = "Ideal Points") + 
  facet_wrap(~Date) 


full_experiment_assign %>% filter(treat == "0", Date %in% c("2017-10-16", "2017-11-27", "2018-02-01", "2022-11-13") ) %>%
  ggplot(aes(x=ideal_point, color = party_ID)) + geom_density(alpha=0.3) + 
  labs(title = "All Control", y = "Density", x = "Ideal Points") + 
  facet_wrap(~Date) 



full_experiment_assign %>% filter(Date %in% c("2017-10-16", "2017-11-27", "2018-02-01", "2022-11-13") ) %>%
  ggplot(aes(x=ideal_point, color = party_ID)) + geom_density(alpha=0.3) + 
  labs(title = "All Control", y = "Density", x = "Ideal Points") + 
  facet_wrap(~Date) 



# just 

full_experiment_assign %>% filter(user_ids %in% new_id_avail, Date %in% c("2017-10-16", "2017-11-27", "2018-02-01", "2022-11-13") ) %>%
  ggplot(aes(x=ideal_point, color = party_ID)) + geom_density(alpha=0.3) + 
  labs(title = "All", y = "Density", x = "Ideal Points") + 
  facet_wrap(~Date) 

