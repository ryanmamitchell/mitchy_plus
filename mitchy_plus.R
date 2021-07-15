#libraries
library(tidyverse)
library(splitstackshape)
library(caret)
setwd("/Users/Ryan/Desktop/rData")

##### Data Importing & Wrangling #####

#importing schedule data
schedule14 <- as.data.frame(read.csv(file = 'schedule_data_14.csv'))
schedule15 <- as.data.frame(read.csv(file = 'schedule_15.csv'))
schedule16 <- as.data.frame(read.csv(file = 'schedule_16.csv'))
schedule17 <- as.data.frame(read.csv(file = 'schedule_17.csv'))
schedule18 <- as.data.frame(read.csv(file = 'schedule_18.csv'))
schedule19 <- as.data.frame(read.csv(file = 'schedule_19.csv'))

schedule_data <- rbind(schedule14,schedule15,schedule16,schedule17,schedule18,schedule19)

#removing unneeded column data
schedule_data <- schedule_data[ -c(5:6,11:12) ]

#remove unnecessary rows
# vs FCS, duplicates
schedule_data <- schedule_data[!duplicated(schedule_data$id), ] 
schedule_data <- schedule_data[schedule_data$awayConference != "", ] 


#Importing betting data, removing duplicates, 
bet14 <- as.data.frame(read.csv(file = 'betting_data_14.csv'))
bet15 <- as.data.frame(read.csv(file = 'bet_15.csv'))
bet16 <- as.data.frame(read.csv(file = 'bet_16.csv'))
bet17 <- as.data.frame(read.csv(file = 'bet_17.csv'))
bet18 <- as.data.frame(read.csv(file = 'bet_18.csv'))
bet19 <- as.data.frame(read.csv(file = 'bet_19.csv'))


bet_data <- rbind(bet14,bet15,bet16,bet17,bet18,bet19)
bet_data <- bet_data[!duplicated(bet_data$id), ]

bet14 <- bet14[!duplicated(bet14$id), ]

#merge the two data frames

sb_data <- merge(schedule_data, bet_data, by = "id")

#remove duplicate / unneeded columns, creating new variable: diffSpread 
    #diffSpread: difference between vegas spread and actual 
sb_data <- sb_data %>%
  mutate(diffSpread = ifelse(spread >= 0, abs(homeScore - awayScore + abs(spread)), 
                             abs(homeScore - awayScore - abs(spread))) )
sb_data <- sb_data %>%
  select(id, spread, formattedSpread, diffSpread)

#Importing game score data, removing unneeded rows, duplicates

game14 <- as.data.frame(read.csv(file = 'game_data_14.csv'))
game15 <- as.data.frame(read.csv(file = 'game_15.csv'))
game16 <- as.data.frame(read.csv(file = 'game_16.csv'))
game17 <- as.data.frame(read.csv(file = 'game_17.csv'))
game18 <- as.data.frame(read.csv(file = 'game_18.csv'))
game19 <- as.data.frame(read.csv(file = 'game_19.csv'))

game_data <- rbind(game14,game15,game16,game17,game18,game19)

game_data <- game_data[game_data$away_conference != "", ]
game_data$home_team <- as.character(game_data$home_team)
game_data$away_team <- as.character(game_data$away_team)

#creating new dataframe for cummulative mean of points for and points against

#reorganizing home vs away games
home_stats_data <- game_data %>%
  group_by(home_team) %>%
  summarize(id, season, week, conference_game, home_id, away_id, oppponent = away_team, points_for = home_points, points_against = away_points)

away_stats_data <- game_data %>%
  group_by(away_team) %>%
  summarize(id, season, week, conference_game, home_id, away_id, opponent = home_team, points_for = away_points, points_against = home_points)

colnames(home_stats_data) <- c("team", "id", "season", "week", "conference_game", "home_id", "away_id", "opponent", "points_for", "points_against")
colnames(away_stats_data) <- c("team", "id", "season", "week", "conference_game", "home_id", "away_id", "opponent", "points_for", "points_against")

#adding logical operator for home vs away game indicator
home_stats_data$home = TRUE
away_stats_data$home = FALSE

#new dataframe and adding cummean
team_stats_data <- rbind(home_stats_data, away_stats_data)
team_stats_data <- team_stats_data %>%
  arrange(team, week)

team_stats_data <- team_stats_data %>%
  group_by(team) %>%
  summarize(season, week, opponent, avg_points_for = cummean(points_for), avg_points_against = cummean(points_against), home) %>%
  arrange(team, week)


#final data set with game and spread data. 1 iteration per game. game_stats_data has 2: 1 for each team
game_stats_data <- merge(game_data,sb_data, by = "id")
game_stats_data <- game_stats_data[!duplicated(game_stats_data$id), ]
game_stats_data <- game_stats_data %>%
                  select(id,season,week,conference_game,home_id,home_team,home_points,away_id,away_team,away_points,
                         spread,formattedSpread,diffSpread)

#renaming columns for consistency, adding actual point differential
names(game_stats_data)[12] <- "formatted_spread"
names(game_stats_data)[13] <- "spread_diff"

game_stats_data <- game_stats_data %>%
                  mutate(actual_diff = away_points - home_points)


##### Writing Function for Model Prediction #####

#function(home team, away team, week) that will internally produce score for each team, create Mitchy+ prediction

mitchy.plus <- function(ht, at, w, y, df = team_stats_data){
  
  
  #HOME TEAM RESULT 
  
  ht_oppdata <- df %>% #sets team to opponent
    filter(week < w & opponent == ht & season == y)
  
   ht_oppdata <- data.frame(mean(ht_oppdata$avg_points_for), mean(ht_oppdata$avg_points_against)) #means of opponents pf and pa
   colnames(ht_oppdata) <- c("points_for", "points_against")
   
  #extract cumulative averages for week = w
  ht_avg <- df %>%
    filter(week < w & team == ht & season == y) %>%
    arrange(week)
  ht_avg <- tail(ht_avg,1)
  ht_pf <- ht_avg[,5]
  ht_pa <- ht_avg[,6]
  #create Mitchy+ formulas
  ht_odiff <- as.numeric(ht_pf - ht_oppdata$points_against) #offensive differential
  ht_ddiff <- as.numeric(ht_pa - ht_oppdata$points_for) #defensive differential
  htppf <- as.numeric(ht_pf + ht_odiff) #predicted points for
  htppa <- as.numeric(ht_pa + ht_ddiff) #predicted points against
  htpp <- data.frame(ht,htppf,htppa)
  colnames(htpp) <- c("team", "points_for", "points_against")
  
  
  #AWAY TEAM RESULT
  
  at_oppdata <- df %>% #sets team to opponent
    filter(week < w & opponent == at & season == y)
  at_oppdata <- data.frame(mean(at_oppdata$avg_points_for), mean(at_oppdata$avg_points_against)) #means of opponents pf and pa
  colnames(at_oppdata) <- c("points_for", "points_against")
  #extract cumulative averages for week = w
  at_avg <- df %>%
    filter(week < w & team == at & season == y) %>%
    arrange(week)
  at_avg <- tail(at_avg,1)
  at_pf <- at_avg[,5]
  at_pa <- at_avg[,6]
  #create Mitchy+ formulas
  at_odiff <- as.numeric(at_pf - at_oppdata$points_against) #offensive differential
  at_ddiff <- as.numeric(at_pa - at_oppdata$points_for) #defensive differential
  atppf <- as.numeric(at_pf + at_odiff) #predicted points for
  atppa <- as.numeric(at_pa + at_ddiff) #predicted points against
  atpp <- data.frame(at,atppf,atppa)
  colnames(atpp) <- c("away_team", "points_for", "points_against")
  
  
  
  #MITCHY PLUS CALCULATION
  
  
  home_pred <-  htpp
  away_pred <-  atpp
  home_score <- (home_pred$points_for + away_pred$points_against)/2 + 1.5
  away_score <- (home_pred$points_against + away_pred$points_for)/2 - 1.5
  m_diff <- away_score - home_score
  
  mitchy_plus <- c(ht,at,y,w,home_score,away_score, m_diff)
  #colnames(mitchy_plus) <- c("home_team","away_team", "year", week","home_score","away_score","score_diff")
  mitchy_plus
}


mitchy.plus("Air Force","Navy",6, 2014)


##### Applying all Data to the Model #####


#converting input variables into vectors
  #so that mapply can be used

mp_ht <- as.vector(game_stats_data$home_team)
mp_at <- as.vector(game_stats_data$away_team)
mp_w <- as.vector(game_stats_data$week)
mp_y <- as.vector(game_stats_data$season)


#running the mapply function, converting to dataframe, renaming rows and columns,
mp_results_data <- as.data.frame(t(mapply(mitchy.plus, mp_ht, mp_at, mp_w,mp_y)))
rownames(mp_results_data) <- c(1:3915)
colnames(mp_results_data) <- c("home_team", "away_team","season", "week", "mp_home_score", "mp_away_score", "mp_diff")

#all columns are now factors: need to convert all to numeric or character

mp_results_data$home_team <- as.character(mp_results_data$home_team)
mp_results_data$away_team <- as.character(mp_results_data$away_team)

mp_results_data$week <- as.numeric(levels(mp_results_data$week))[mp_results_data$week]
mp_results_data$mp_home_score <- as.numeric(levels(mp_results_data$mp_home_score))[mp_results_data$mp_home_score]
mp_results_data$mp_away_score <- as.numeric(levels(mp_results_data$mp_away_score))[mp_results_data$mp_away_score]
mp_results_data$mp_diff <- as.numeric(levels(mp_results_data$mp_diff))[mp_results_data$mp_diff]


#limits decimals to 1
mp_results_data[,'mp_home_score']=round(mp_results_data[,'mp_home_score'], 1)
mp_results_data[,'mp_away_score']=round(mp_results_data[,'mp_away_score'], 1)
mp_results_data[,'mp_diff']=round(mp_results_data[,'mp_diff'], 1)


#combining the actual game / vegas data with the model predictions, removing NA's post join
game_mp_data <- merge(game_stats_data,mp_results_data)
game_mp_data <- na.omit(game_mp_data)

  

#removing spread_diff column: found to not be relevent.
game_mp_data <- subset(game_mp_data, select = -c(spread_diff,formatted_spread))


#adding comparison columns: Spread vs. Actual, MP vs. Actual, MP vs. Spread
        #spread_actual = how far away vegas was from the true score
        #mp_actual = how far mitchy+ was from the true score
        #mp_spread = difference between vegas prediction and mitchy+ prediction

        #if abs(mp_actual is less than abs(spread_actual), the bet wins

game_mp_data <- game_mp_data %>%
                mutate(mp_spread_variance = abs(mp_diff - spread)) %>%
                mutate(spread_actual = actual_diff - spread) %>%
                mutate(mp_actual = actual_diff - mp_diff) %>%
                mutate(win_loss = ifelse(abs(mp_actual) < abs(spread_actual), TRUE, FALSE))


##### Data Exploration and Visualization #####
  # IDEAS:
      # Experiment with different starting points (week 4-7)
      # Looking at conference games vs. all games
      # Analyze effects of incorporating 2nd half of last year's games to reduce variance

#Segmenting data into training and test segments
set.seed(1)
validation_index <- createDataPartition(game_mp_data$week, p=0.70, list = FALSE)
test_sample <- game_mp_data[-validation_index,]
training_sample <- game_mp_data[validation_index,]


#Statistical summaries

summary(training_sample[,c(8,10:19)]) #only included numerical data


#general visualization of variance metrics

hist(training_sample$mp_actual) #norm dist.
hist(training_sample$spread_actual) #norm dist.
hist(training_sample$mp_spread_variance) #decreases as week increases

plot(training_sample$mp_actual)
training_sample %>%
  filter(abs(mp_actual) <= abs(spread_actual))
1-mean(training_sample$win_loss)


#Conference game testing

c_test <- training_sample %>%
                    filter(conference_game == 'true' & week >4 & week <15) %>%
                    group_by(week) %>%
                    summarise(variance = mean(win_loss))
c_test


##### NEXT STEPS #####
  #1. general data exploration
  #2. create logistical regression analysis



