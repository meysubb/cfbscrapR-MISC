calc_ep_multinom_loso_cv <- function(ep_formula, weight_type = 3,
                                     ep_model_data) {
  # Create vector of seasons to generate hold out results for:
  seasons <- unique(ep_model_data$year)
  
  # Generate the predictions for each holdout season:
  map_dfr(seasons,
          function(x) {
            print(glue::glue("Working on data from {x}"))
            # Separate test and training data:
            test_data <- ep_model_data %>%
              filter(year == x)
            train_data <- ep_model_data %>%
              filter(year != x)
            
            # Create the various weight columns only using
            # the train_data:
            train_data <- train_data %>%
              # 1 - drive difference
              mutate(
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
                  (max(Total_Season_W) - min(Total_Season_W))
              )
            
            # Type of weighting:
            if (weight_type == 1) {
              # Drive difference weight
              train_data$model_weights <-
                train_data$Drive_Score_Dist_W
              
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
              train_data$model_weights <-
                train_data$Total_Season_W_Scaled
              
            } else {
              # No weights
              train_data$model_weights <- rep(1, nrow(train_data))
            }
            
            # Build model using maxit at 300 for now:
            ep_model <- nnet::multinom(ep_formula,
                                       data = train_data,
                                       weights = model_weights,
                                       maxit = 300)
            print("Saving ep_model in RData and rds formats")
            rdat = glue::glue("models/ep_model_season_{x}.RData")
            rds = glue::glue("models/ep_model_season_{x}.rds")
            save(ep_model, file = rdat)
            saveRDS(ep_model, rds)
            
            # Generate and return prediction dataset (can add columns to
            # return from the test_data in the mutate function below but
            # only necessary variables are the predicted probabilities
            # and the actual events):
            preds_ep <-
              data.frame(predict(ep_model, newdata = test_data, type = "probs")) 
            # %>%
            #   mutate(NSH = test_data$NSH,
            #          Next_Score = test_data$Next_Score)
            
            test_preds_ep <- cbind(test_data, preds_ep)
            print(glue::glue("Finish training model for {x}"))
            csvfn = glue::glue("data/EPA_Calcs_season_{x}.csv")
            write.csv(test_preds_ep, csvfn, row.names = FALSE)
            return(test_preds_ep)
            
          }) %>%
    return
}

calc_ep_multinom_fg_loso_cv <-
  function(ep_formula,
           fg_formula,
           weight_type = 3,
           ep_model_data) {
    # Create vector of seasons to generate hold out results for:
    seasons <- unique(ep_model_data$year)
    
    # Generate the predictions for each holdout season:
    map_dfr(seasons,
            function(x) {
              print(glue::glue("Working on data from {x}"))
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
                    (max(Total_Season_W) - min(Total_Season_W))
                )
              
              # Type of weighting:
              if (weight_type == 1) {
                # Drive difference weight
                train_data$model_weights <-
                  train_data$Drive_Score_Dist_W
                
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
                train_data$model_weights <-
                  train_data$Total_Season_W_Scaled
                
              } else {
                # No weights
                train_data$model_weights <- rep(1, nrow(train_data))
              }
              
              # Build model using maxit at 300 for now:
              ep_model <- nnet::multinom(ep_formula,
                                         data = train_data,
                                         weights = model_weights,
                                         maxit = 300)
              print("Saving ep_model in RData and rds formats")
              rdat = glue::glue("models/ep_cv_loso_model_season_{x}.RData")
              rds = glue::glue("models/ep_cv_loso_model_season_{x}.rds")
              save(ep_model, file = rdat)
              saveRDS(ep_model, rds)
              
              # Generate prediction dataset (can add columns to
              # return from the test_data in the mutate function below but
              # only necessary variables are the predicted probabilities
              # and the actual events) from only using the ep_model
              preds_ep <- data.frame(predict(ep_model,
                                             newdata = test_data,
                                             type = "probs")) 
              # %>%
              #   mutate(NSH = test_data$NSH,
              #          Next_Score = test_data$Next_Score)
              
              # Build field goal model:
              fg_contains = str_detect((train_data$play_type), "Field Goal")
              fg_train_data <- train_data[fg_contains, ]
              
              fg_model <- mgcv::bam(fg_formula,
                                    data = fg_train_data, family = "binomial")
              print("Saving fg_model in RData and rds formats")
              fg_rdat = glue::glue("models/fg_cv_loso_model_season_{x}.RData")
              fg_rds = glue::glue("models/fg_cv_loso_model_season_{x}.rds")
              save(fg_model, file = fg_rdat)
              saveRDS(fg_model, fg_rds)
              # Now make a copy of the test data to then get EP probabilites
              # as if the field goals were missed:
              fg_test_attempt_i = str_detect((test_data$play_type), "Field Goal")
              fg_test_data <- test_data[fg_test_attempt_i, ]
              missed_fg_test_data <- fg_test_data %>%
                # Subtract 5.065401 from TimeSecs since average time for FG att:
                mutate(
                  TimeSecsRem = TimeSecsRem - 5.065401,
                  # Correct the yards_to_goal:
                  yards_to_goal = 100 - (yards_to_goal - 9),
                  # Not Goal_To_Go:
                  Goal_To_Go = rep(FALSE, n()),
                  # Now first down:
                  down = rep("1", n()),
                  # 10 yards to go
                  log_ydstogo = rep(log(10), n()),
                  # Create Under_two indicator
                  Under_two = ifelse(TimeSecsRem < 120,
                                     TRUE, FALSE)
                )
              
              # Get the missed test data predictions:
              missed_fg_ep_preds <- data.frame(predict(ep_model,
                                                       newdata = missed_fg_test_data,
                                                       type = "probs"))
              
              # Find the rows where TimeSecsRem became 0 or negative and
              # make all the probs equal to 0:
              end_game_i <-
                which(missed_fg_test_data$TimeSecsRem <= 0)
              missed_fg_ep_preds[end_game_i, ] <- rep(0,
                                                      ncol(missed_fg_ep_preds))
              
              # Get the probability of making the field goal:
              make_fg_prob <- as.numeric(mgcv::predict.bam(fg_model,
                                                           newdata = fg_test_data,
                                                           type = "response"))
              
              # Multiply each value of the missed_fg_ep_preds by the 1 - make_fg_prob
              missed_fg_ep_preds <-
                missed_fg_ep_preds * (1 - make_fg_prob)
              
              # Now update the probabilities for the FG attempts
              # (also includes Opp_Field_Goal probability from missed_fg_ep_preds)
              preds_ep[fg_test_attempt_i, "FG"] <- make_fg_prob + 
                missed_fg_ep_preds[,"Opp_FG"]
              # Update the other columns based on the opposite possession:
              preds_ep[fg_test_attempt_i, "TD"] <- missed_fg_ep_preds[, "Opp_TD"]
              preds_ep[fg_test_attempt_i, "Opp_FG"] <- missed_fg_ep_preds[, "FG"]
              preds_ep[fg_test_attempt_i, "Opp_TD"] <- missed_fg_ep_preds[, "TD"]
              preds_ep[fg_test_attempt_i, "Safety"] <- missed_fg_ep_preds[, "Opp_Safety"]
              preds_ep[fg_test_attempt_i, "Opp_Safety"] <- missed_fg_ep_preds[, "Safety"]
              preds_ep[fg_test_attempt_i, "No_Score"] <- missed_fg_ep_preds[, "No_Score"]
              
              test_preds_ep <- cbind(test_data, preds_ep)
              print(glue::glue("Finish training model for {x}"))
              csvfn = glue::glue("data/EPA_FG_Calcs_season_{x}.csv")
              write.csv(test_preds_ep, csvfn, row.names = FALSE)
              return(test_preds_ep)
              
            }) %>%
      return
  }


calc_wp_gam_loso_cv <- function(wp_formula, wp_model_data) {
  # Create vector of seasons to generate hold out results for:
  seasons <- unique(wp_model_data$year)
  
  # Generate the predictions for each holdout season:
  map_dfr(seasons,
          function(x) {
            # Separate test and training data:
            print(glue::glue("Working on data from {x}"))
            test_data <- wp_model_data %>%
              filter(year == x)
            train_data <- wp_model_data %>%
              filter(year != x)
            
            print("Create MP cluster")
            cl <- makeCluster(detectCores()-1)
            
            # Build model:
            wp_model <-
              bam(wp_formula, data = train_data, family = "binomial", cluster = cl)
            
            print(glue::glue("Finish training model for {x}"))
            stopCluster(cl)
            # Generate and return prediction dataset (can add columns to
            # return from the test_data in the mutate function below but
            # only necessary variables are the predicted probabilities
            # and the actual events):
            data.frame(win_prob = predict(wp_model, newdata = test_data,
                                          type = "response")) %>%
              mutate(win_ind = test_data$Win_Indicator,
                     period = test_data$period) %>%
              return
            
          }) %>%
    return
}
