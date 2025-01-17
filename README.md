# cfbscrapR-MISC
Miscellaneous files for [cfbscrapR](https://github.com/meysubb/cfbscrapR "cfbscrapR GitHub repository") (also includes work for EPA/WPA models)
## Contributors 
- [Meyappan Subbaiah](https://github.com/meysubb "Meyappan's GitHub") ([@msubbaiah1](https://twitter.com/msubbaiah1?s=20 "Meyappan's Twitter"))
- [Saiem Gilani](https://github.com/saiemgilani "Saiem's GitHub") ([@saiemgilani](https://twitter.com/SaiemGilani?s=20 "Saiem's Twitter"))
- [Parker Fleming](https://github.com/spfleming "Parker's GitHub") ([@statsowar](https://twitter.com/statsowar?s=20 "Parker's Twitter"))
## The Data
- We will be acquiring data from [CollegeFootballData.com](https://collegefootballdata.com/ "CollegeFootballData.com"), courtesy of [@CFB_data](https://twitter.com/cfb_data "CollegeFootballData.com's Twitter"), using [cfbscrapR](https://github.com/meysubb/cfbscrapR "cfbscrapR GitHub repository"), created by [Meyappan Subbaiah](https://github.com/meysubb "Meyappan's GitHub") ([@msubbaiah1](https://twitter.com/msubbaiah1?s=20 "Meyappan's Twitter")).

# The Expected Points Model
## Outcome (target) variables
 This model generates probabilities for the possible types of next scoring events within the same half. The 7 scoring possibilities are:

- Touchdown (7)
- Field Goal (3)
- Safety (2)
- No Score (0)
- Opponent Safety (-2)
- Opponent Field Goal (-3)
- Opponent Touchdown (-7)
  
 For each play, the model calculates what the probability (p) of each of those scoring events is and gives you the expected points by multiplying each of the scoring event probabilities by their associated point values and summing the products, like so:
<a href="https://imgur.com/sJTs6j6"><img src="https://i.imgur.com/sJTs6j6.png" title="Figure: @SaiemGilani" /></a>

## Predictive variables

The model is fit using the following variables and interactions:

### Pure Distance Factors (24 variables)

- Yards from opponent’s end zone (6)
- log(yards to convert 1st/Goal) (6)
- Indicator for goal-to-go situations (6)
- Interaction between log(yards to convert 1st/Goal) and goal-to-go indicator (6)

### Down Factors and Down Interactions (54 variables)

- Down (18 since Down is a categorical factor)
- Interaction between log(yards to convert 1st/Goal) and down (18)
- Interaction between yards from opponent’s end zone and down (18)

### Time Factors and Intercept (18 variables)

- Seconds remaining in the current half (6)
- Indicator for under two-minutes to the end of half/game (6)
- Intercept for each next score type, used as the combined mean value for reference variables, in this case 1st Down, not a goal-to-go situation and outside of under two minutes left to the end of half or game. (6)

You might be asking at this point, “how are there 96 variables, if there are only 10 bullet points here?” To model this appropriately, one next score type is used as the reference target (we choose “No Score” as our reference) and the coefficients are fitted to the predictive variables for each of the remaining 6 target score types. This is why you see each factor comprising at least 6 variables. 

The additional complexity is that down must be treated as a categorical factor, meaning 1st down is treated as the reference and there are then 6 variables for 2nd down, 6 variables for 3rd down, and 6 variables for 4th down. The same logic would then also apply to each of the down interactions. The Boolean indicators for goal-to-go and under two-minutes are also factors that give relative coefficients to the ‘False’ case, but since there only two cases, only 6 additional variables are created for each. 

## Model Weights

Observations are weighted by both score differential and difference in number of drives between play and the next score. The former weighting is intended to accomplish the goal of valuing plays where the score differential is higher less than possessions where the score differential is closer to zero. 

Additionally, the latter model weighting places more emphasis on plays on drives that occur closer to the next scoring drive more than plays on prior drives. An example would be if a game begins with 6 scoreless drives to start before one team scores, the plays of the first drive would have a weight of 1/7*, the plays of the second drive a 2/7 weighting, with the plays of the first scoring drive (i.e. the 7th possession) having a full 7/7 weighting. 

*Full disclosure: it would be 7 as the denominator if 6 scoreless drives was the largest distance between scoring drives for the entire season across all games (it could be 12 for the games across the entire season, in which case the values would have been 1/12, 2/12, ..., 7/12.) 

This way the model weighs all games with the same denominator and scales the weight of each scoring drive distance to a value between 0 and 1. This same caveat also applies for score differential. Score difference is scaled by largest score differential across all games in a given season. Both of the score differential and scoring drive distance weightings are combined to create a single scaled weighting for each play across each season.

## Kicking, Kickoffs, and Punts

Field goals and kickoffs are treated separately. The probability that a field goal is made is calculated using a simple binomial model using the (smoothed) distance of the kick as the predictor, calculated as the yard-line plus an additional 17 yards. Each field goal attempt’s expected points are weighted by the probability it is made (resulting in 3 expected points) and adjusted by the probability of a missed field goal (i.e. Pr(Missed FG) = 1 - Pr(Made FG)) and the resulting change in EPA due to changes in field position, down, etc. that would occur from the missed field goal. 

Touch-backs are considered the standard expected outcome for kickoffs and anything returned past the 25 would be a points added situation and vice versa for those not returned to at least the 25. considered

PATs are not currently being treated separately, the extra point is treated as a given, which is less than ideal. 

Punts are not treated separately, since their primary effect lies mostly in the change in field position. It is a situation the expected points model is well suited to predict on. 

## Field Position and Expected Points

The model was trained on data from 2014-2019 using leave-one-season-out cross-validation. This means the model holds one season for test data validation and uses the remaining seasons as the training set, then uses the trained model to make predictions on the season held out. This process is then iterated for each season in the data.

<a href="https://imgur.com/g5rSRUX"><img src="https://i.imgur.com/g5rSRUX.png"  title="Figure: @SaiemGilani" /></a>

This is the plot of  expected points in relation to field position alone, grouped in 10 yard bins and labeled on the plot with the mean expected points for the bin. The point at the bottom left is roughly the expected points from having the ball at the offense’s own 1-yard line, -1.28 expected points. The observed mean expected value for receiving the ball from the offense’s own 21-30-yard line (or 70 to 79 yards from opponent’s end-zone) is 0.29 expected points.  

It should be no surprise that as offenses move closer to their opponent’s goal line, the expected points trend upward for the offense. Notice for the point representing the 30-39-yard line grouping, the expected points is right around the value of a field goal. This aligns with one of the common definitions of a scoring opportunity, giving support to the appropriateness of the definition.  

The observed average expected points in the red-zone are 4.05 in the 10-19-yard line bin and 5.17 inside the 10, with plays at the 1-yard line averaging 6.06 expected points. Note: There are some plays that were input as yard 0 that I interpreted as an “& inches” situation. There are 66 such plays (compared to 5.3k plays at the 1-yard line) and their observed mean expected points is 4.62.

## Field Position and Expected Points by Down

Recall that down and down interactions were three of the predictors in the model, so here is the field position view of expected points separated by down. The plot below demonstrates that your expected points are higher (on average) on 1st down than 2nd, and 2nd down than 3rd, etc at every point on the field, except possibly near the offense’s own goal line where there is risk of giving up a safety.

<a href="https://imgur.com/S7etCxQ"><img src="https://i.imgur.com/S7etCxQ.png" title="Figure: @SaiemGilani" /></a>

This graph should be somewhat intuitive, but to understand what the relative expected points difference for each down at the same field position, we are interested in the vertical distance between each of the lines. 

### 1st and 2nd Down

Noting that, we see that difference between 1st and 2nd down is steady across most of the field from left to right, starting wider near the offense’s own goal line and narrowing as the offense progresses toward the opponent’s end-zone. 

### 2nd and 3rd Down

Examining the difference between the 2nd and 3rd down lines, the distance is much wider, doubling the relative difference between 1st and 2nd for most portions of the middle 60 yards of the field. 

### 3rd and 4th Down

Further yet is the 4th down line, which has an interesting track, narrowing closest to the other down lines around the 25 yard line. 

Perhaps we should examine the next score probabilities underlying each of these down lines to see what drives these relative differences. 

<a href="https://imgur.com/bo9t1yp"><img src="https://i.imgur.com/bo9t1yp.png" title="Figure: @SaiemGilani" /></a>

#### Expected Points model calibration plots

![ep_fg_cv_loso_calibration_results.png](https://i.imgur.com/bOE4VOU.png)
 
#### Win Probability model calibration plots

![wp_cv_loso_calibration_results.png](https://i.imgur.com/4YgfphC.png)

<a href="https://imgur.com/EARlj5x"><img src="https://i.imgur.com/EARlj5x.png" title="Figure: @SaiemGilani" /></a>

<a href="https://imgur.com/7Ow4Xk3"><img src="https://i.imgur.com/7Ow4Xk3.png" title="Figure: @SaiemGilani" /></a>

<a href="https://imgur.com/MSkGOBY"><img src="https://i.imgur.com/MSkGOBY.png" title="Figure: @SaiemGilani" /></a>
