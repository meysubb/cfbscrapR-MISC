library(dplyr)
library(ggplot2)

success_by_distance_plot <- function(pbp_full_df, year = 2019, team, downs = c(3), primary.color = "#000000", secondary.color = "#FFFFFF", model.logging = FALSE) {
  filtered_df = pbp_full_df %>% filter(year == year) %>%  filter(offense == team) %>% filter(down %in% downs)
  
  if (nrow(filtered_df) == 0) {
    print(paste("No plays found for", team, "in the", year, "season", sep = " "))
    return()
  }
  
  mod.fit.team <- glm(epa_success ~ distance*pass, family = binomial(link = "logit"), data = filtered_df)
  if (model.logging) {
    print(summary(mod.fit.team))
    print(xtabs(formula = ~ epa_success + pass + distance, data = filtered_df))
  }
  
  new.data.pass = data.frame(distance = seq(0, 20), pass = 1)
  new.data.rush = data.frame(distance = seq(0, 20), pass = 0)
  
  prediction.pass <- predict(mod.fit.team, newdata = new.data.pass, type = "response")
  prediction.rush <- predict(mod.fit.team, newdata = new.data.rush, type = "response")
  ci.pass <- ci.pi(newdata = new.data.pass, mod.fit.team, 0.05)
  ci.rush <- ci.pi(newdata = new.data.rush, mod.fit.team, 0.05)
  
  display <- data.frame(x = seq(0,20), y.pass = prediction.pass, lower.pass = ci.pass$lower, upper.pass = ci.pass$upper,
                        y.rush = prediction.rush, lower.rush = ci.rush$lower, upper.rush = ci.rush$upper)
  

  p <- ggplot(display, aes(x=x, y=prediction.pass)) + 
    #geom_point() +
    xlim(0, 20) + ylim(0,1) +
    geom_ribbon(aes(ymin = lower.pass, ymax = upper.pass), fill = secondary.color) +
    geom_ribbon(aes(ymin = lower.rush, ymax = upper.rush), fill = secondary.color) +
    geom_line( aes(y = prediction.pass), col =  primary.color, linetype = "dotdash", lwd = 2) +
    geom_line( aes(y = lower.pass), col = primary.color, linetype = "dotdash") +
    geom_line( aes(y = upper.pass), col = primary.color, linetype = "dotdash") +
    geom_line( aes(y = prediction.rush), col =  primary.color, linetype = "solid", lwd = 2) +
    geom_line( aes(y = lower.rush), col = primary.color, linetype = "solid") +
    geom_line( aes(y = upper.rush), col = primary.color, linetype = "solid") +
    labs(x = "Distance To Go", y = "Probability of Successful Play", 
         title = paste(year, team, "probability of successful play", sep = " "), 
         subtitle = paste("Plays on downs", toString(downs), sep = " "),
         caption = "Data from collegefootballdataAPI, Plot by @arbitanalytics") +
    theme_bw(base_size = 16)
  
  print(p)
}


ci.pi <- function(newdata, mod.fit.obj, alpha) {
  linear.pred <- predict(object = mod.fit.obj, newdata = newdata, type = "link", se = TRUE)
  
  CI.lin.pred.lower <- linear.pred$fit - qnorm(p = 1-alpha/2)*linear.pred$se
  CI.lin.pred.upper <- linear.pred$fit + qnorm(p = 1-alpha/2)*linear.pred$se
  
  CI.pi.lower <- exp(CI.lin.pred.lower) / (1+exp(CI.lin.pred.lower))
  CI.pi.upper <- exp(CI.lin.pred.upper) / (1+exp(CI.lin.pred.upper))
  
  return(list(lower = CI.pi.lower, upper = CI.pi.upper))
}
