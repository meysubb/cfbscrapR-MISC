source('08-Pred-Utils.R')
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
            
            test_preds_ep <- cbind(test_data, preds_ep)
            csvfn = glue::glue("data/EPA_Calcs_season_{x}.csv")
            write.csv(test_preds_ep, csvfn, row.names = FALSE)
            print(glue::glue("Finish training model for {x}"))
            
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
              
              rdat = glue::glue("models/ep_cv_loso_model_season_{x}.RData")
              rds = glue::glue("models/ep_cv_loso_model_season_{x}.rds")
              save(ep_model, file = rdat)
              saveRDS(ep_model, rds)
              

              
              
              
              # Build field goal model:
              fg_contains = str_detect((train_data$play_type), "Field Goal")
              fg_train_data <- train_data[fg_contains, ]
              
              fg_model <- mgcv::bam(fg_formula,
                                    data = fg_train_data, family = "binomial")
              
              fg_rdat = glue::glue("models/fg_cv_loso_model_season_{x}.RData")
              fg_rds = glue::glue("models/fg_cv_loso_model_season_{x}.rds")
              save(fg_model, file = fg_rdat)
              saveRDS(fg_model, fg_rds)
              # Now make a copy of the test data to then get EP probabilites
              # as if the field goals were missed:
              
              g_ids = sort(unique(test_data$game_id))
              test_preds_ep = calculate_epa_local_alt(clean_pbp_dat=test_data,ep_model = ep_model, fg_model = fg_model)
                         
              
              csvfn = glue::glue("data/EPA_FG_Calcs_season_{x}.csv")
              write.csv(test_preds_ep, csvfn, row.names = FALSE)
              
              print(glue::glue("Finish training model for {x}"))
              
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
            
            print(glue::glue("Working on data from {x}"))
            
            # Separate test and training data:
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
