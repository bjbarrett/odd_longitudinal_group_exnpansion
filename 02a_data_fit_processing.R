library(rethinking)
library(janitor)
library(dplyr)
library(tidyverse)

##group size
d_hr_gs <- read.csv("data/df_slpHRarea_group_size.csv")
str(d_hr_gs)
d_hr_gs$group_index <- as.integer(as.factor(d_hr_gs$group))
d_hr_gs$group_size_std <- standardize(d_hr_gs$group_size)

##home range overlap########
d_hr_ov <- read.csv("data/df_slpHR_dyadic_overlap.csv")
str(d_hr_ov)
d_hr_ov <- d_hr_ov[d_hr_ov$overlap_uds>0,]

# it looks like there are reps where groups are compared to themselves
# extract years and group IDs
d_hr_ov$g1 <- substr(d_hr_ov$p1, 1, 2)
d_hr_ov$g2 <- substr(d_hr_ov$p2, 1, 2)
d_hr_ov$y1 <- substr(d_hr_ov$p1, 4, 7)
d_hr_ov$y2 <- substr(d_hr_ov$p2, 4, 7)

#drop dyads where years do not match
# drop dyads where groups are identical

d_hr_ov <- subset(d_hr_ov, d_hr_ov$y1 == d_hr_ov$y2 & d_hr_ov$g1!= d_hr_ov$g2 )
#d_hr_gs$group_index <- as.integer(as.factor(d_hr_gs$group))

##for this paremterization it is importatnt that each group is representaed at least once in g1 g2

# sort(unique(d_hr_ov$g1))
# sort(unique(d_hr_ov$g2))
# sort(unique(d_hr_ov$g1))==sort(unique(d_hr_ov$g2))
# goblin_fart <- which(d_hr_ov$g2=="sp" & d_hr_ov$g1=="aa")
# d_hr_ov[goblin_fart[1],]$g2 <- "aa"
# d_hr_ov[goblin_fart[1],]$g1<- "sp"
# sort(unique(d_hr_ov$g1))==sort(unique(d_hr_ov$g2))

# sp missing from g1, aa missing from g2
#indexes for groups and dyads

# d_hr_ov$g1_index <- as.integer(as.factor(d_hr_ov$g1))
# d_hr_ov$g2_index <- as.integer(as.factor(d_hr_ov$g2))
# d_hr_ov$y1_index <- as.integer(as.factor(d_hr_ov$y1))
# d_hr_ov$y2_index <- as.integer(as.factor(d_hr_ov$y2))

# d_hr_ov[d_hr_ov$g1=="ce",]
# d_hr_ov[d_hr_ov$g1=="cu",]
# d_hr_ov[d_hr_ov$g1=="di",]
# 
# d_hr_ov$dyad <- apply(d_hr_ov[,5:6], 1, function(s) paste0(sort(s), collapse=''))
# d_hr_ov$dyad_index <- as.integer(as.factor(d_hr_ov$dyad))
# sort(unique(d_hr_ov$dyad))
# em <- factorial(11)/(factorial(2)*factorial(11-2)) #max possible dyad cobos
# em > length(unique(d_hr_ov$dyad)) # should be true
# 
# str(d_hr_ov)
# d_hr_gs_min <- d_hr_gs[,4:9]
# joined_df <- left_join(d_hr_ov, d_hr_gs_min, by=dplyr::join_by(p1 == id))


# Make weights for competitive ability using overlaps ------------------------


# pivot overlaps wider to make into weights for comp ability
# first need to pivot longer so that every group year is represented in p1
# this will create duplicate dyads 
# but then when we pivot wider, every unique group_year will have a column for every other group
d_weights <- d_hr_ov %>% # 
  pivot_longer(cols = p1:p2,
               names_to = "old_name",
               values_to = "p1") %>%
  mutate(p2 = ifelse(old_name == "p1", # create new p2 
                     str_c(g2, y2, sep = "_"), # if old name was p1, then p2 is g2_y2
                     str_c(g1, y1, sep = "_")), # if old name was p2, then p2 is g1_y1
         g1 = str_sub(p1, 1, 2), #give new g1 and g2
         g2 = str_sub(p2, 1, 2),
         neighbor_name = p2) %>% 
  group_by(g1) %>% 
  mutate(neighbor = str_c("neighbor", # give ids to neighbors based on g2
                          as.numeric(as.factor(g2)), # list neighbors numerically so there are no self comparisons
                          #g2,
                          sep = "_")) %>% 
  ungroup() %>%
  rename(id = p1,
         weight = overlap_uds) %>% 
  left_join(dplyr::select(d_hr_gs, id, group_size) %>% rename(neighbor_name = id), # merge group sizes of neighbors
            by = "neighbor_name") %>%
  arrange(id) %>% 
  pivot_wider(id_cols = id, # pivot wider to get weights and group sizes of each neighbor
              names_from = neighbor,
              values_from = c(weight, group_size)) %>%
  mutate(across(2:21, ~replace(., is.na(.), 0))) %>% # replace NAs with 0, all 0 overlaps and 0 group size will have no affect on weight
  left_join(dplyr::select(d_hr_gs,id, group, year, group_size, group_size_std, group_index, hr_area_mean), 
            by = "id") %>% 
  mutate(weighted_mean_neighbor_comp = (weight_neighbor_1*group_size_neighbor_1 + 
                            weight_neighbor_2*group_size_neighbor_2 +
                            weight_neighbor_3*group_size_neighbor_3 +
                            weight_neighbor_4*group_size_neighbor_4 + # calculate weighted mean
                            weight_neighbor_5*group_size_neighbor_5 +
                            weight_neighbor_6*group_size_neighbor_6 +
                            weight_neighbor_7*group_size_neighbor_7 +
                            weight_neighbor_8*group_size_neighbor_8 +
                            weight_neighbor_9*group_size_neighbor_9 +
                            weight_neighbor_10*group_size_neighbor_10)/10,
         weighted_mean_neighbor_comp_std = standardize(weighted_mean_neighbor_comp)) %>% # standardize weighted mean
  dplyr::select(c(id, group, group_index, 
                  year, group_size, group_size_std, 
                  hr_area_mean, weighted_mean_neighbor_comp,weighted_mean_neighbor_comp_std)) 

# which segments are in home range data frame that are missing from weights
d_hr_gs$id[!(d_hr_gs$id %in% weights$id)]

# aa 1990-93 had no neighbors so not in dataframe
# earlier years will be biased because did not have neighborig groups
# also no intergroup data at least on GPS to know what percent of encounter came from what group
# if we wanted to use encounters as weight instead

#.......................................................


# add group sizes by joining from home range area dataframe 
# (also could use group size dataframe just added to repository)
for(i in 15:19){
  names(joined_df)[i] <- paste0(names(joined_df) , "1")[i]
}
joined_df <- left_join(joined_df, d_hr_gs_min, by=join_by(p2 == id))
for(i in 20:24){
  names(joined_df)[i] <- paste0(names(joined_df) , "2")[i]
}
str(joined_df)
d_hr_ov <- joined_df
d_hr_ov$group_size1_std <- standardize(d_hr_ov$group_size1)
d_hr_ov$group_size2_std <- standardize(d_hr_ov$group_size2)
d_hr_ov$hr_area_mean1_std <- standardize(d_hr_ov$hr_area_mean1)
d_hr_ov$hr_area_mean2_std <- standardize(d_hr_ov$hr_area_mean2)

# add relative group sizes
d_hr_ov$rel_group_size1 <- d_hr_ov$group_size1 - d_hr_ov$group_size2
d_hr_ov$rel_group_size1_std <- standardize(d_hr_ov$rel_group_size1)
d_hr_ov$rel_group_size2 <- d_hr_ov$group_size2 - d_hr_ov$group_size1
d_hr_ov$rel_group_size2_std <- standardize(d_hr_ov$rel_group_size2)

##daily path length
d_dpl <- read.csv("data/df_GPS_daily_path_length.csv")
str(d_dpl)
d_hr_dpl <- clean_names(d_dpl)
str(d_dpl)
d_dpl$year <-as.integer(substr(d_dpl$date, 1, 4))
d_dpl$group_index <- as.integer(as.factor(d_dpl$group))

# get group sizes by group_year
annual_group_sizes <- read.csv("data/annual_group_sizes.csv") %>% # group size dataframe
  mutate(id = str_c(group,year,sep = "_")) %>% 
  dplyr::select(id, group_size) 

# give dpl dataframe group size column
d_dpl_gs <- d_dpl %>% 
  mutate(id = str_c(group, year, sep = "_")) %>% 
  left_join(annual_group_sizes, by = "id") %>% 
  dplyr::select(-id) %>% 
  mutate(group_size_std = standardize(group_size),
         dist.ML = dist.ML/1000) %>% # change dpl to km
  rename(dpl_mean = dist.ML) %>% 
  dplyr::select(group, dpl_mean, year, group_index, group_size, group_size_std)
