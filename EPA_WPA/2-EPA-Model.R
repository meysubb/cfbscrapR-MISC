library(cfbscrapR)
library(dplyr)
library(readr)
library(stringr)
source("6-Utils.R")

pbp_full_df <- readRDS(file = "data/pbp.rds")

## Filter out OT games for now
# Create the EP model dataset that only includes plays with basic seven
# types of next scoring events along with the following play types:
# Field Goal, No Play, Pass, Punt, Run, Sack, Spike

# Remove  [8] "Extra Point Missed","Extra Point Good","Timeout","End of Half","End of Game","Uncategorized"
remove_plays <- c("Extra Point Missed","Extra Point Good","Timeout","End of Half","End of Game","Uncategorized")

pbp_no_OT <-
  pbp_full_df %>% filter(period <= 4,down>0) %>% filter(!play_type %in% remove_plays) %>% filter(!is.na(down), !is.na(raw_secs)) %>%
  rename(TimeSecsRem = raw_secs)

## Use a GAM to build an EPA model, maybe?
## need to go over that course to refresh my GAM skills
## should I put down and
## because I use time secs remaining, I can't use OT games
ep_model <- mgcv::bam(NSH ~ s(TimeSecsRem) + down + s(distance) + s(adj_yd_line),
                      data = pbp_no_OT)

test = pbp_no_OT %>% select(TimeSecsRem,down,distance,adj_yd_line)

test$epa = mgcv::predict.bam(ep_model,test)


## pick an SEC game

tamu_18 = pbp_no_OT %>% filter(
  year == 2018,
  offense %in% c("Clemson", "Texas A&M"),
  defense %in% c("Clemson", "Texas A&M")
) %>% select(-X1)
