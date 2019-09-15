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
remove_plays <-
  c(
    "Extra Point Missed",
    "Extra Point Good",
    "Timeout",
    "End of Half",
    "End of Game",
    "Uncategorized",
    "Penalty",
    "Kickoff",
    "Kickoff Return (Offense)",
    "Kickoff Return Touchdown"
  )

pbp_no_OT <-
  pbp_full_df %>% filter(period <= 4, down > 0) %>%
  filter(!play_type %in% remove_plays) %>%
  filter(!is.na(down), !is.na(raw_secs)) %>%
  filter(log_ydstogo != -Inf) %>%
  rename(TimeSecsRem = raw_secs) %>%
  mutate(
    Next_Score = forcats::fct_relevel(factor(Next_Score), "No_Score"),
    Goal_To_Go = ifelse(
      str_detect(play_type, "Field Goal"),
      distance == (adj_yd_line - 17),
      distance == adj_yd_line
    ),
    Under_two = TimeSecsRem <= 120
  )

# fg_contains = str_detect((pbp_no_OT$play_type),"Field Goal")
# fg_no_OT <- pbp_no_OT[fg_contains,]
# 
# fg_model <- mgcv::bam(scoring ~ s(adj_yd_line),
#                        data = fg_no_OT, family = "binomial")
# saveRDS(fg_model,"models/fg_model.rds")
# Load FG Model
fg_model = readRDS("models/fg_model.rds")
##+ Under_TwoMinute_Warning
## Create a weighting factor
# need
# ep_model <- nnet::multinom(Next_Score ~ TimeSecsRem + adj_yd_line + Under_two +
#                                down + log_ydstogo + log_ydstogo*down +
#                               adj_yd_line*down + Goal_To_Go, data = pbp_no_OT, maxit = 300)
# saveRDS(ep_model,"models/ep_model.rds")
# Load EPA Model
ep_model = readRDS("models/ep_model.rds")


## At this point, let's create a function to predict EP_before and EP_after, and calculate EPA

calculate_epa <- function(clean_pbp_dat,ep_mod,fg_mod){
  # constant vectors to be used again
  turnover_play_type = c(
    'Fumble Recovery (Opponent)',
    'Pass Interception Return',
    'Interception Return Touchdown',
    'Fumble Return Touchdown',
    'Safety',
    'Interception',
    'Pass Interception',
    'Punt',
    "Field Goal Missed",
    "Blocked Field Goal"
  )
  
  off_TD = c('Passing Touchdown','Rushing Touchdown')
  def_TD = c('Interception Return Touchdown','Fumble Return Touchdown',
             'Missed Field Goal Return Touchdown')
  
  
  pred_df = clean_pbp_dat %>% select(TimeSecsRem,down,distance,adj_yd_line,log_ydstogo,Under_two,Goal_To_Go)
  
  # ep_start
  ep_start = as.data.frame(predict(ep_mod,pred_df,type='prob'))
  ep_start_update = epa_fg_probs(dat = clean_pbp_dat,current_probs = ep_start,fg_mod = fg_mod)
  weights = c(0,3,-3,-2,-7,2,7)
  pred_df$ep_before = apply(ep_start_update, 1, function(row){
    sum(row * weights)
  })
  
  ## Prep for EP_after 
  prep_df_after = prep_df_epa2(clean_pbp_dat) 
  # prep_df_after = prep_df_epa(clean_pbp_dat) 
  # turnover_col = prep_df_after %>% pull(turnover)
  # prep_df_after = prep_df_after %>% select(-turnover)
  ep_end = predict(ep_model,prep_df_after,type='prob')
  pred_df$ep_after = apply(ep_end, 1, function(row){
    sum(row * weights)
  })
  
  pred_df = cbind(clean_pbp_dat,pred_df[,c("ep_before","ep_after")])
  #pred_df$turnover = turnover_col
  
  pred_df[(pred_df$play_type %in% off_TD),"ep_after"] = 7
  pred_df[(pred_df$play_type %in% turnover_play_type), "ep_after"] = -1 * pred_df[(pred_df$play_type %in% turnover_play_type), "ep_after"]
  pred_df[(pred_df$play_type %in% def_TD),"ep_after"] = -7
  
  pred_df[pred_df$play_type=="Safety","ep_after"] = -2
  pred_df[pred_df$play_type=="Field Goal Good","ep_after"] = 3
  # game end EP is 0
  pred_df[pred_df$end_half_game == 1,"ep_after"] = 0
  
  
  pred_df = pred_df %>% 
    mutate(
      EPA = ep_after - ep_before
    )
  return(pred_df)
}

## create a function to ingest the pbp data 
## and aid in figuring out probabilities for FG

epa_fg_probs <- function(dat,current_probs,fg_mod){
  
  fg_ind = str_detect((dat$play_type),"Field Goal")
  fg_dat = dat[fg_ind,]
  
  # we are setting everythign after 0 seconds to have 
  # 0 probs. 
  end_game_ind = which(dat$TimeSecsRem <= 0 )
  current_probs[end_game_ind,] <- 0
  
  make_fg_prob <- mgcv::predict.bam(fg_mod,newdata = fg_dat,
                                    type = "response")
  
  # add in the fg make prob into this 
  current_probs2 <- current_probs
  #current_probs2[fg_ind,] <- current_probs[fg_ind,] * (1-make_fg_prob)
  val = (1-make_fg_prob)
  ind <- dim(current_probs2[fg_ind,])[1]
  for(i in seq(1,ind)){
    temp = current_probs2[fg_ind,]
    temp[i,] = temp[i,] * val[i]
  }
  current_probs2[fg_ind,] =  temp
  
  
  # now to flip all the probs,
  current_probs2[fg_ind,"FG"] <- make_fg_prob + current_probs[fg_ind,"Opp FG"]
  current_probs2[fg_ind,"Opp FG"] <- current_probs[fg_ind,"FG"]
  current_probs2[fg_ind,"TD"] <- current_probs[fg_ind,"Opp TD"]
  current_probs2[fg_ind,"Opp TD"] <- current_probs[fg_ind,"TD"]
  current_probs2[fg_ind,"Safety"] <- current_probs[fg_ind,"Opp Safety"]
  current_probs2[fg_ind,"Opp Safety"] <- current_probs[fg_ind,"Safety"]
  return(current_probs2)
}

## Separate by Year into a list, then run EPA
all_years = split(pbp_no_OT,pbp_no_OT$year)
all_years_epa = lapply(all_years, function(x){
  year = unique(x$year)
  print(year)
  val = calculate_epa(x,ep_model,fg_model)
  return(val)
})

len = length(all_years_epa)

for(i in 1:len){
  df = all_years_epa[[i]]
  hist(df$EPA)
  Sys.sleep(5)
}

### Figure out who is who, so you can attribute players to it. 



lapply(names(all_years_epa),function(x){
  write.csv(all_years_epa[[x]],file=paste0("data/csv/EPA_calcs_",x,".csv"))
})

lapply(names(all_years_epa),function(x){
  saveRDS(all_years_epa[[x]],file=paste0("data/rds/EPA_calcs_",x,".RDS"),compress = T)
})
