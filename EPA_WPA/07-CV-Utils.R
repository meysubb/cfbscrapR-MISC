calc_ep_multinom_loso_cv <- function(ep_formula, weight_type = 3, 
                                     ep_model_data) {
  
  # Create vector of seasons to generate hold out results for:
  seasons <- unique(ep_model_data$year)
  
  # Generate the predictions for each holdout season:
  map_dfr(seasons, 
          function(x) {
            # Separate test and training data:
            test_data <- ep_model_data %>%
              filter(year == x)
            train_data <- ep_model_data %>%
              filter(year != x)
            
            # Create the various weight columns only using
            # the train_data:
            train_data <- train_data %>%
              # 1 - drive difference
              mutate(Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) / 
                       (max(Drive_Score_Dist) - min(Drive_Score_Dist)),
                     
                     # 2 - score differential
                     ScoreDiff_W = (max(abs_diff) - abs_diff) / 
                       (max(abs_diff) - min(abs_diff)),
                     
                     # 3 - combo of 1 and 2
                     Total_W = Drive_Score_Dist_W + ScoreDiff_W,
                     Total_W_Scaled = (Total_W - min(Total_W)) / 
                       (max(Total_W) - min(Total_W)),
                     
                     # 4 - difference from holdout season
                     Season_Diff = abs(year - x),
                     Season_Diff_W = (max(Season_Diff) - Season_Diff) / 
                       (max(Season_Diff) - min(Season_Diff)),
                     
                     # 5 - combo of 1, 2, and 4
                     Total_Season_W = Drive_Score_Dist_W + ScoreDiff_W + 
                       Season_Diff_W,
                     Total_Season_W_Scaled = (Total_Season_W - min(Total_Season_W)) / 
                       (max(Total_Season_W) - min(Total_Season_W)))
            
            # Type of weighting:
            if (weight_type == 1){
              # Drive difference weight
              train_data$model_weights <- train_data$Drive_Score_Dist_W
              
            } else if (weight_type == 2) {
              # Score differential weight
              train_data$model_weights <- train_data$ScoreDiff_W
              
            } else if (weight_type == 3) {
              # Combined weight
              train_data$model_weights <- train_data$Total_W_Scaled
              
            } else if (weight_type == 4) {
              # Season difference
              train_data$model_weights <- train_data$Season_Diff_W
              
            } else if (weight_type == 5) {
              # Combined with season
              train_data$model_weights <- train_data$Total_Season_W_Scaled
              
            } else {
              # No weights
              train_data$model_weights <- rep(1, nrow(train_data))
            }
            
            # Build model using maxit at 300 for now:
            ep_model <- nnet::multinom(ep_formula, 
                                       data = train_data,
                                       weights = model_weights, maxit = 300)
            print("Saving ep_model in RData and rds formats")
            rdat = glue::glue("models/ep_model_season_{x}.RData")
            rds = glue::glue("models/ep_model_season_{x}.rds")
            save(ep_model, file=rdat)
            saveRDS(ep_model,rds)
            
            # Generate and return prediction dataset (can add columns to
            # return from the test_data in the mutate function below but
            # only necessary variables are the predicted probabilities 
            # and the actual events):
            preds_ep <- data.frame(predict(ep_model, newdata = test_data, type = "probs")) %>%
              mutate(NSH = test_data$NSH,
                     Next_Score = test_data$Next_Score) 
            
            test_preds_ep<-cbind(test_data,preds_ep)
            file_name <- paste0("data/EPA_Calcs_",x,".csv")
            write.csv(test_preds_ep,file_name,row.names=FALSE)
            return(test_preds_ep)
            
          }) %>%
    return
}

calc_ep_multinom_fg_loso_cv <- function(ep_formula, fg_formula, weight_type = 3, 
                                        ep_model_data) {
  # Create vector of seasons to generate hold out results for:
  seasons <- unique(ep_model_data$year)
  
  # Generate the predictions for each holdout season:
  map_dfr(seasons, 
          function(x) {
            
            # Separate test and training data:
            test_data <- ep_model_data %>%
              filter(year == x)
            train_data <- ep_model_data %>%
              filter(year != x)
            
            # Create the various weight columns only using
            # the train_data:
            train_data <- train_data %>%
              
              mutate(
                # 1 - drive difference
                Drive_Score_Dist_W = (max(Drive_Score_Dist) - Drive_Score_Dist) / 
                  (max(Drive_Score_Dist) - min(Drive_Score_Dist)),
                
                # 2 - score differential
                ScoreDiff_W = (max(abs_diff) - abs_diff) / 
                  (max(abs_diff) - min(abs_diff)),
                
                # 3 - combo of 1 and 2
                Total_W = Drive_Score_Dist_W + ScoreDiff_W,
                Total_W_Scaled = (Total_W - min(Total_W)) / 
                  (max(Total_W) - min(Total_W)),
                
                # 4 - difference from holdout season
                Season_Diff = abs(year - x),
                Season_Diff_W = (max(Season_Diff) - Season_Diff) / 
                  (max(Season_Diff) - min(Season_Diff)),
                
                # 5 - combo of 1, 2, and 4
                Total_Season_W = Drive_Score_Dist_W + ScoreDiff_W + 
                  Season_Diff_W,
                Total_Season_W_Scaled = (Total_Season_W - min(Total_Season_W)) / 
                  (max(Total_Season_W) - min(Total_Season_W)))
            
            # Type of weighting:
            if (weight_type == 1){
              # Drive difference weight
              train_data$model_weights <- train_data$Drive_Score_Dist_W
              
            } else if (weight_type == 2) {
              # Score differential weight
              train_data$model_weights <- train_data$ScoreDiff_W
              
            } else if (weight_type == 3) {
              # Combined weight
              train_data$model_weights <- train_data$Total_W_Scaled
              
            } else if (weight_type == 4) {
              # Season difference
              train_data$model_weights <- train_data$Season_Diff_W
              
            } else if (weight_type == 5) {
              # Combined with season
              train_data$model_weights <- train_data$Total_Season_W_Scaled
              
            } else {
              # No weights
              train_data$model_weights <- rep(1, nrow(train_data))
            }
            
            # Build model using maxit at 300 for now:
            ep_model <- nnet::multinom(ep_formula, 
                                       data = train_data,
                                       weights = model_weights, maxit = 300)
            
            save(ep_model, file="models/ep_model_cv_cal.RData")
            saveRDS(ep_model,"models/ep_model.rds")
            # Generate prediction dataset (can add columns to
            # return from the test_data in the mutate function below but
            # only necessary variables are the predicted probabilities 
            # and the actual events) from only using the ep_model
            preds_ep <- data.frame(predict(ep_model, 
                                           newdata = test_data, 
                                           type = "probs")) %>%
              mutate(NSH = test_data$NSH,
                     Next_Score = test_data$Next_Score)

            
            # Build field goal model:
            fg_contains = str_detect((train_data$play_type),"Field Goal")
            fg_train_data <- train_data[fg_contains,]
            
            fg_model <- mgcv::bam(fg_formula, 
                                  data = fg_train_data, family = "binomial")
            
            save(fg_model, file="models/fg_model_cv_cal.RData")
            saveRDS(fg_model,"models/fg_model_cv_cal.rds")
            # Now make a copy of the test data to then get EP probabilites 
            # as if the field goals were missed:
            fg_test_contains = str_detect((test_data$play_type),"Field Goal")
            fg_test_data <- test_data[fg_test_contains,]
            missed_fg_test_data <- fg_test_data %>%
              # Subtract 5.065401 from TimeSecs since average time for FG att:
              mutate(TimeSecsRem = TimeSecsRem - 5.065401,
                     # Correct the yrdline100:
                     yards_to_goal = 100 - (yards_to_goal + 8),
                     # Not GoalToGo:
                     Goal_To_Go = rep(FALSE,n()),
                     # Now first down:
                     down = rep("1", n()),
                     # 10 yards to go
                     log_ydstogo = rep(log(10),n()),
                     # Create Under_TwoMinute_Warning indicator
                     Under_two = ifelse(TimeSecsRem < 120,
                                        TRUE, FALSE))
            
            
            
            
            # Get the missed test data predictions:
            missed_fg_ep_preds <- data.frame(predict(ep_model, 
                                                     newdata = missed_fg_test_data, 
                                                     type = "probs"))
            
            # Find the rows where TimeSecsRem became 0 or negative and 
            # make all the probs equal to 0:
            end_game_i <- which(missed_fg_test_data$TimeSecsRem <= 0)
            missed_fg_ep_preds[end_game_i,] <- rep(0,
                                                   ncol(missed_fg_ep_preds))
            
            # Get the probability of making the field goal:
            make_fg_prob <- as.numeric(mgcv::predict.bam(fg_model, 
                                                         newdata = test_data, 
                                                         type = "response"))
            
            # Multiply each value of the missed_fg_ep_preds by the 1 - make_fg_prob
            missed_fg_ep_preds <- missed_fg_ep_preds * (1 - make_fg_prob)
            
            # Find the FG attempts in the test data:
            fg_attempt_i <- which(str_detect((test_data$play_type),"Field Goal"))
            fg_test_data <- test_data[fg_attempt_i,]
            
            # Now update the probabilities for the FG attempts 
            # (also includes Opp_Field_Goal probability from missed_fg_ep_preds)
            preds_ep[fg_attempt_i, "FG"] <- make_fg_prob[fg_attempt_i] + 
              missed_fg_ep_preds[fg_attempt_i,"Opp_FG"]
            
            # Update the other columns based on the opposite possession:
            preds_ep[fg_attempt_i, "TD"] <- 
              missed_fg_ep_preds[fg_attempt_i,"Opp_TD"]
            
            preds_ep[fg_attempt_i, "Opp_FG"] <- 
              missed_fg_ep_preds[fg_attempt_i,"FG"]
            
            preds_ep[fg_attempt_i, "Opp_TD"] <-
              missed_fg_ep_preds[fg_attempt_i,"TD"]
            
            preds_ep[fg_attempt_i, "Safety"] <-
              missed_fg_ep_preds[fg_attempt_i,"Opp_Safety"]
            preds_ep[fg_attempt_i, "Opp_Safety"] <-
              missed_fg_ep_preds[fg_attempt_i,"Safety"]
            
            preds_ep[fg_attempt_i, "No_Score"] <- 
              missed_fg_ep_preds[fg_attempt_i,"No_Score"]

            
            test_preds_ep<-cbind(test_data,preds_ep)
            file_name = glue::glue("data/EPA_FG_Calcs_season_{x}.csv")
            write.csv(test_preds_ep,file_name,row.names=FALSE)
            
            return(test_preds_ep)
            
          }) %>%
    return
}

## At this point, let's create a function to predict EP_before and EP_after, and calculate EPA

calculate_epa_local <- function(clean_pbp_dat, ep_model, fg_model) {
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
  
  off_TD = c(
    'Passing Touchdown',
    'Rushing Touchdown',
    "Fumble Recovery (Own) Touchdown",
    "Pass Reception Touchdown"
  )
  def_TD = c(
    'Interception Return Touchdown',
    'Fumble Return Touchdown',
    'Missed Field Goal Return Touchdown',
    "Fumble Return Touchdown Touchdown",
    "Fumble Recovery (Opponent) Touchdown",
    "Punt Return Touchdown",
    "Blocked Punt Touchdown",
    "Sack Touchdown",
    "Blocked Field Goal Touchdown",
    "Missed Field Goal Return Touchdown",
    "Pass Interception Return Touchdown",
    "Kickoff Touchdown"
  )
  
  pred_df = clean_pbp_dat %>% arrange(id) %>%  select(
    id,
    drive_id,
    game_id,
    TimeSecsRem,
    down,
    distance,
    yards_to_goal,
    log_ydstogo,
    Under_two,
    Goal_To_Go
  )
  
  # ep_start
  ep_start = as.data.frame(predict(ep_model, pred_df, type = 'prob'))
  colnames(ep_start) <- ep_model$lev
  ep_start_update = epa_fg_probs(dat = clean_pbp_dat,
                                 current_probs = ep_start,
                                 fg_model = fg_model)
  weights = c(0, 3, -3, -2, -7, 2, 7)
  pred_df$ep_before = apply(ep_start_update, 1, function(row) {
    sum(row * weights)
  })
  
  ## Prep for EP_after
  prep_df_after = prep_df_epa2(clean_pbp_dat)
  if (length(unique(clean_pbp_dat$game_id)) > 1) {
    # if you are trying to deal with multiple games at once
    # then you have to get the after individually.
    prep_df_after = map_dfr(unique(clean_pbp_dat$game_id),
                            function(x) {
                              clean_pbp_dat %>%
                                filter(game_id == x) %>%
                                prep_df_epa2()
                            })
  }
  
  ep_end = predict(ep_model, prep_df_after, type = 'prob')
  colnames(ep_end) <- ep_model$lev
  pred_df$ep_after = apply(ep_end, 1, function(row) {
    sum(row * weights)
  })
  
  colnames(prep_df_after)[4:12] = paste0(colnames(prep_df_after)[4:12], "_end")
  pred_df = clean_pbp_dat %>% left_join(prep_df_after) %>% left_join(pred_df %>% select(id_play, drive_id, game_id, ep_before, ep_after))
  #pred_df$turnover = turnover_col
  ## kickoff plays
  ## calculate EP before at kickoff as what happens if it was a touchback
  ## 25 yard line in 2012 and onwards
  kickoff_ind = (pred_df$play_type =='Kickoff')
  new_kick = pred_df[kickoff_ind,]
  new_kick["yards_to_goal"] = 75
  new_kick["log_ydstogo"] = log(75)
  ep_kickoffs = as.data.frame(predict(ep_model, new_kick, type = 'prob'))
  pred_df[(pred_df$play_type =='Kickoff'),"ep_before"] = apply(ep_kickoffs,1,function(row){
    sum(row*weights)
  })
  
  turnover_plays = which(pred_df$turnover_end == 1 & !kickoff_ind)
  pred_df[turnover_plays, "ep_after"] = -1 * pred_df[turnover_plays, "ep_after"]
  
  # game end EP is 0
  pred_df[pred_df$end_half_game_end == 1, "ep_after"] = 0
  
  ## scoring plays from here on out
  pred_df[(pred_df$play_type %in% off_TD), "ep_after"] = 7
  pred_df[(pred_df$play_type %in% def_TD), "ep_after"] = -7
  pred_df[pred_df$play_type == "Safety", "ep_after"] = -2
  pred_df[pred_df$play_type == "Field Goal Good", "ep_after"] = 3
  
  
  
  pred_df = pred_df %>%
    mutate(EPA = ep_after - ep_before) %>%
    select(-yard_line,
           -coef,
           -log_ydstogo_end,-Goal_To_Go_end) %>% select(
             game_id,
             drive_id,
             id,
             offense,
             offense_conference,
             defense,
             defense_conference,
             home,
             away,
             period,
             half,
             clock.minutes,
             clock.seconds,
             offense_score,
             defense_score,
             play_type,
             play_text,
             scoring,
             TimeSecsRem,
             down,
             distance,
             yards_to_goal,
             yards_gained,
             TimeSecsRem_end,
             down_end,
             distance_end,
             yards_to_goal_end,
             turnover_end,
             Under_two_end,
             everything()
           ) %>%
    mutate(
      rz_play = ifelse((yards_to_goal <= 20), 1, 0),
      scoring_opp = ifelse((yards_to_goal <= 40), 1, 0),
      pass = if_else(
        play_type == "Pass Reception" | play_type == "Passing Touchdown" |
          play_type == "Sack" |
          play_type == "Pass Interception Return" |
          play_type == "Pass Incompletion" |
          play_type == "Sack Touchdown" |
          (play_type == "Safety" &
             str_detect(play_text, "sacked")) |
          (
            play_type == "Fumble Recovery (Own)" &
              str_detect(play_text, "pass")
          ) |
          (
            play_type == "Fumble Recovery (Opponent)" &
              str_detect(play_text, "pass")
          ),
        1,
        0
      ),
      rush = ifelse(
        play_type == "Rush" |
          (play_type == "Safety" &
             str_detect(play_text, "run")) |
          (
            play_type == "Fumble Recovery (Opponent)" &
              str_detect(play_text, "run")
          ) |
          (
            play_type == "Fumble Recovery (Own)" &
              str_detect(play_text, "run")
          ),
        1,
        0
      ),
      stuffed_run = ifelse((rush == 1 &
                              yards_gained <= 0), 1, 0),
      success = ifelse(
        yards_gained >= .5 * distance & down == 1,
        1,
        ifelse(
          yards_gained >= .7 * distance & down == 2,
          1,
          ifelse(
            yards_gained >= distance & down == 3,
            1,
            ifelse(yards_gained >= distance &
                     down == 4, 1, 0)
          )
        )
      ),
      success = ifelse(play_type %in% turnover_play_type, 0, success),
      epa_success = ifelse(EPA > 0, 1, 0)
    )
  return(pred_df)
}

