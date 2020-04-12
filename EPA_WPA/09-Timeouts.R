library(cfbscrapR)
library(dplyr)
library(readr)
library(stringr)
library(tidyverse)
library(nnet)
library(mgcv)

source("06-Data-Ingest-Utils.R")
source("07-CV-Utils.R")

pbp_full_df <- readRDS(file = "data/pbp.rds")
pbp_mismatched_crosswalk<- read.csv("pbp_mismatched_crosswalk.csv")

pbp_full_df <- pbp_full_df %>%
  group_by(game_id, half) %>%
  arrange(desc(year),id_play) %>%
  mutate(
    timeout_called = ifelse(play_type %in% c("Timeout"),1,0),
    timeout_team = ifelse(play_type %in% c("Timeout"),
                          ifelse(!is.na(str_extract(play_text, "timeout (.+)")),
                                 str_extract(play_text, "timeout (.+)"),
                                 str_extract(play_text, "Timeout (.+)")), NA)
  ) %>%
  mutate(timeout_team = str_remove(timeout_team, ",(.+)")) %>%
  mutate(
    timeout_team = str_to_lower(str_remove(timeout_team, "Timeout ")),
    timeout_team = case_when(
      timeout_team == "af" ~ "air force",
      timeout_team == "air force academy" ~ "air force",
      timeout_team == "air force falcons" ~ "air force",
      timeout_team == "arkansas st." ~ "arkansas state",
      timeout_team == "asu" ~ "arkansas state",
      timeout_team == "ball state cardinals" ~ "ball state",
      timeout_team == "bgsu" ~ "bowling green",
      timeout_team == "brigham young" ~ "byu",
      timeout_team == "byu cougars" ~ "byu",
      timeout_team == "centrl michigan" ~ "central michigan",
      timeout_team == "cmu" ~ "central michigan",
      timeout_team == "coastl carolina" ~ "coastal carolina",
      timeout_team == "cs" ~ "colorado state",
      timeout_team == "eastrn michigan" ~ "eastern michigan",
      timeout_team == "ecu" ~ "east carolina",
      timeout_team == "emu" ~ "eastern michigan",
      timeout_team == "fau" ~ "florida atlantic",
      timeout_team == "fiu" ~ "florida international",
      timeout_team == "fla atlantic" ~ "florida atlantic",
      timeout_team == "florida intl" ~ "florida international",
      timeout_team == "floridainternational" ~ "florida international",
      timeout_team == "fresno st." ~ "fresno state",
      timeout_team == "ga southern" ~ "georgia southern",
      timeout_team == "gsu" ~ "georgia state",
      timeout_team == "hawai`i" ~ "hawai'i",
      timeout_team == "hawaii" ~ "hawai'i",
      timeout_team == "iowa hawkeyes" ~ "iowa",
      timeout_team == "las vegas" ~ "unlv",
      timeout_team == "latech" ~ "louisiana tech",
      timeout_team == "louisiana lafayette" ~ "louisiana",
      timeout_team == "louisiana state" ~ "lsu",
      timeout_team == "louisiana-lafayette" ~ "louisiana",
      timeout_team == "massachusetts" ~ "umass",
      timeout_team == "miami (fla.)" ~ "miami",
      timeout_team == "miami (ohio)" ~ "miami (oh)",
      timeout_team == "miami fl" ~ "miami",
      timeout_team == "miami florida" ~ "miami",
      timeout_team == "miami oh" ~ "miami (oh)",
      timeout_team == "miami ohio" ~ "miami (oh)",
      timeout_team == "middle tenn st" ~ "middle tennessee",
      timeout_team == "minnesota gophers" ~ "minnesota",
      timeout_team == "mississippi" ~ "ole miss",
      timeout_team == "mt" ~ "middle tennessee",
      timeout_team == "n.c. state" ~ "nc state",
      timeout_team == "NA" ~ "",
      timeout_team == "niu" ~ "northern illinois",
      timeout_team == "nm state" ~ "new mexico state",
      timeout_team == "nmsu" ~ "new mexico state",
      timeout_team == "north carolina st" ~ "nc state",
      timeout_team == "northernil" ~ "northern illinois",
      timeout_team == "ohio bobcats" ~ "ohio",
      timeout_team == "ohio university" ~ "ohio",
      timeout_team == "olddominion" ~ "old dominion",
      timeout_team == "ole ole miss" ~ "ole miss",
      timeout_team == "oregon st." ~ "oregon state",
      timeout_team == "rice owls" ~ "rice",
      timeout_team == "san jose st" ~ "san josé state",
      timeout_team == "san jose state" ~ "san josé state",
      timeout_team == "sj" ~ "san josé state",
      timeout_team == "sjsu" ~ "san josé state",
      timeout_team == "smu mustangs" ~ "smu",
      timeout_team == "southern  miss" ~ "southern mississippi",
      timeout_team == "southern cal" ~ "usc",
      timeout_team == "southern methodist university" ~ "smu",
      timeout_team == "temple owls" ~ "temple",
      timeout_team == "temple university" ~ "temple",
      timeout_team == "texas el paso" ~ "utep",
      timeout_team == "texas state university" ~ "texas state",
      timeout_team == "texassan" ~ "ut san antonio",
      timeout_team == "texas-san antonio" ~ "ut san antonio",
      timeout_team == "tls" ~ "tulsa",
      timeout_team == "troy university" ~ "troy",
      timeout_team == "tulane green wave" ~ "tulane",
      timeout_team == "uh" ~ "hawai'i",
      timeout_team == "ui" ~ "idaho",
      timeout_team == "ul" ~ "louisiana",
      timeout_team == "ul lafayette" ~ "louisiana",
      timeout_team == "ul monroe" ~ "louisiana monroe",
      timeout_team == "ull" ~ "louisiana",
      timeout_team == "ulm" ~ "louisiana monroe",
      timeout_team == "university of idaho" ~ "idaho",
      timeout_team == "usa" ~ "south alabama",
      timeout_team == "usf" ~ "south florida",
      timeout_team == "usm" ~ "southern mississippi",
      timeout_team == "usu" ~ "utah state",
      timeout_team == "utsa" ~ "ut san antonio",
      timeout_team == "washington st." ~ "washington state",
      timeout_team == "west virginia university" ~ "west virginia",
      timeout_team == "westrn kentucky" ~ "western kentucky",
      timeout_team == "westrn michigan" ~ "western michigan",
      timeout_team == "wfu" ~ "wake forest",
      timeout_team == "wku" ~ "western kentucky",
      timeout_team == "wmu" ~ "western michigan",
      timeout_team == "wsu" ~ "washington state",
      timeout_team == "wyoming cowboys" ~ "wyoming",
      TRUE~timeout_team),
    home_timeout = ifelse(is.na(timeout_team),0,ifelse(str_detect(str_to_lower(home),fixed(timeout_team))==TRUE,1,0)), 
    away_timeout = ifelse(is.na(timeout_team),0,ifelse(str_detect(str_to_lower(away),fixed(timeout_team))==TRUE,1,0)),
    home_timeouts_rem_beg = NA,
    away_timeouts_rem_beg = NA
  )

# pbp_full_df <- pbp_full_df %>%
#               left_join(pbp_mismatched_crosswalk, by=c('timeout_team'),suffix=c("",".y")) %>%
#               mutate(
#                 timeout_team = ifelse(timeout_called==1&away_timeout==0&home_timeout==0,
#                                       str_to_lower(team_name),timeout_team),
#                 home_timeout = ifelse(is.na(timeout_team),0,ifelse(str_detect(str_to_lower(home),fixed(timeout_team))==TRUE,1,0)), 
#                 away_timeout = ifelse(is.na(timeout_team),0,ifelse(str_detect(str_to_lower(away),fixed(timeout_team))==TRUE,1,0))
#                 
#               )

pbp_full_df <-pbp_full_df %>%
  group_by(game_id,half) %>%
  mutate(
    home_timeouts_rem_end = 3 - cumsum(home_timeout),
    away_timeouts_rem_end = 3 - cumsum(away_timeout),
    home_timeouts_rem_beg = ifelse(!is.na(lag(home_timeouts_rem_end,order_by = id_play)),
                                   lag(home_timeouts_rem_end,order_by = id_play),3),
    away_timeouts_rem_beg = ifelse(!is.na(lag(away_timeouts_rem_end,order_by = id_play)),
                                   lag(away_timeouts_rem_end,order_by = id_play),3))

pbp_unmatched_list <- pbp_full_df %>%
  filter(timeout_called == 1, away_timeout ==0, home_timeout==0)


pbp_doublematched_df <- pbp_full_df %>%
  filter(timeout_called == 1, away_timeout ==1, home_timeout==1)

write.csv(pbp_unmatched_df,"data/pbp_mismatched_df.csv",row.names=F)
write.csv(pbp_doublematched_df,"data/pbp_doublematched_df.csv",row.names=F)

write.csv(pbp_full_df,"data/pbp_full_df.csv",row.names=F)
