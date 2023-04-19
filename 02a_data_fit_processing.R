library(rethinking)
library(janitor)
library(dplyr)
##group size
d_hr_gs <- read.csv("data/df_slpHRarea_group_size.csv")
str(d_hr_gs)
d_hr_gs$group_index <- as.integer(as.factor(d_hr_gs$group))
d_hr_gs$group_size_std <- standardize(d_hr_gs$group_size)


##home range overlap########3
d_hr_ov <- read.csv("data/df_slpHR_dyadic_overlap.csv")
str(d_hr_ov)
d_hr_ov <- d_hr_ov[d_hr_ov$overlap_uds>0,]
d_hr_ov$overlap_uds <- ifelse(d_hr_ov$overlap_uds==0 , 0.000000001 , d_hr_ov$overlap_uds)
# it looks like there are reps where groups are compared to themselves
# extract years and group IDs
d_hr_ov$g1 <- substr(d_hr_ov$p1, 1, 2)
d_hr_ov$g2 <- substr(d_hr_ov$p2, 1, 2)
d_hr_ov$y1 <- substr(d_hr_ov$p1, 4, 7)
d_hr_ov$y2 <- substr(d_hr_ov$p2, 4, 7)
#drop dyads where years do not match
# drop dyads where groups are identical
d_hr_ov <- subset(d_hr_ov, d_hr_ov$y1 == d_hr_ov$y2 & d_hr_ov$g1!= d_hr_ov$g2 )
d_hr_gs$group_index <- as.integer(as.factor(d_hr_gs$group))
##for this paremterization it is importatnt that each group is representaed at least once in g1 g2
sort(unique(d_hr_ov$g1))
sort(unique(d_hr_ov$g2))
sort(unique(d_hr_ov$g1))==sort(unique(d_hr_ov$g2))
goblin_fart <- which(d_hr_ov$g2=="sp" & d_hr_ov$g1=="aa")
d_hr_ov[goblin_fart[1],]$g2 <- "aa"
d_hr_ov[goblin_fart[1],]$g1<- "sp"
sort(unique(d_hr_ov$g1))==sort(unique(d_hr_ov$g2))

# sp missing from g1, aa missing from g2
#indexes for groups and dyads
d_hr_ov$g1_index <- as.integer(as.factor(d_hr_ov$g1))
d_hr_ov$g2_index <- as.integer(as.factor(d_hr_ov$g2))
d_hr_ov$y1_index <- as.integer(as.factor(d_hr_ov$y1))
d_hr_ov$y2_index <- as.integer(as.factor(d_hr_ov$y2))

d_hr_ov[d_hr_ov$g1=="ce",]
d_hr_ov[d_hr_ov$g1=="cu",]
d_hr_ov[d_hr_ov$g1=="di",]

d_hr_ov$dyad <- apply(d_hr_ov[,5:6], 1, function(s) paste0(sort(s), collapse=''))
d_hr_ov$dyad_index <- as.integer(as.factor(d_hr_ov$dyad))
sort(unique(dyad))
em <- factorial(11)/(factorial(2)*factorial(11-2)) #max possible dyad cobos
em > length(unique(dyad)) # should be true

##daily path length
d_hr_dpl <- read.csv("data/df_GPS_daily_path_length.csv")
str(d_hr_dpl)
d_hr_dpl <- clean_names(d_hr_dpl)
str(d_hr_dpl)
d_hr_dpl$year <-as.integer(substr(d_hr_dpl$date, 1, 4))
d_hr_gs$group_index <- as.integer(as.factor(d_hr_gs$group))
d_hr_gs$group_size_std <- standardize(d_hr_gs$group_size)

