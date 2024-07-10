# Load the required libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)


# Reading the CSV files
deliveries <- read_csv("deliveries.csv")
matches <- read_csv("matches.csv")
matches
deliveries


# Most Successful Teams:

# Grouping the matches by the winning team and count the number of wins
team_wins <- matches %>%
  group_by(winner) %>%
  summarize(total_wins = n())

# Sorting the teams by the number of wins and display the top 5
top_teams <- team_wins %>%
  arrange(desc(total_wins)) %>%
  head(5)
print(top_teams)


#	Most Successful Players:

# Counting the number of times each player has been the player of the match
player_of_match <- matches %>%
  group_by(player_of_match) %>%
  summarize(total_potm = n())

# Sorting the players by the number of player of the match awards and display the top 5
top_players <- player_of_match %>%
  arrange(desc(total_potm)) %>%
  head(5)
print(top_players)


#	Factors Contributing to Win or Loss:
  
# Analyzing the relationship between toss decision and match result
toss_decision_result <- matches %>%
  group_by(toss_decision, result) %>%
  summarize(count = n()) %>%
  spread(result, count)

print(toss_decision_result)

# Analyzing the relationship between venue and match result
venue_result <- matches %>%
  group_by(venue, result) %>%
  summarize(count = n()) %>%
  spread(result, count)

print(venue_result)
print(tail(venue_result))

#	Endorsement Recommendations:

# Identifying the top players based on their performance
top_players_performance <- deliveries %>%
  group_by(batsman) %>%
  summarize(total_runs = sum(batsman_runs),
            total_balls = n()) %>%
  mutate(strike_rate = total_runs / total_balls * 100) %>%
  arrange(desc(strike_rate)) %>%
  head(5)

print(top_players_performance)

# Identifying the top teams based on their performance
top_teams_performance <- deliveries %>%
  group_by(batting_team) %>%
  summarize(total_runs = sum(total_runs)) %>%
  arrange(desc(total_runs)) %>%
  head(5)

print(top_teams_performance)


# Analyzing Team Performance:
 
# Calculating the win percentage for each team
team_performance <- matches %>%
  group_by(team1, team2) %>%
  summarize(team1_wins = sum(winner == team1),
            team2_wins = sum(winner == team2),
            total_matches = n()) %>%
  mutate(team1_win_percentage = team1_wins / total_matches * 100,
         team2_win_percentage = team2_wins / total_matches * 100)

# Displaying the top 5 teams by win percentage
top_teams_by_win_percentage <- team_performance %>%
  arrange(desc(team1_win_percentage), desc(team2_win_percentage)) %>%
  head(5)
print(top_teams_by_win_percentage)


#	Analyzing Player Performance:

# Calculating the batting average and strike rate for each player
player_performance <- deliveries %>%
  group_by(batsman) %>%
  summarize(total_runs = sum(batsman_runs),
            total_balls = n(),
            total_dismissals = sum(player_dismissed != ""),
            batting_average = total_runs / (total_balls - total_dismissals),
            strike_rate = total_runs / total_balls * 100) %>%
  arrange(desc(batting_average), desc(strike_rate))

# Displaying the top 5 players by batting average and strike rate
top_players_by_performance <- player_performance %>%
  head(5)
print(top_players_by_performance)

#	Analyzing Factors Affecting Match Outcome:
 
# Analyzing the impact of toss decision on match outcome
toss_decision_impact <- matches %>%
  group_by(toss_decision, result) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

print(toss_decision_impact)

# Analyzing the impact of venue on match outcome
venue_impact <- matches %>%
  group_by(venue, result) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

print(venue_impact)

#	Endorsement Recommendations:
 
# Identifying the top players based on their overall performance
top_players_for_endorsement <- player_performance %>%
  mutate(overall_performance_score = batting_average * strike_rate) %>%
  arrange(desc(overall_performance_score)) %>%
  head(5)

print(top_players_for_endorsement)

# Identifying the top teams based on their overall performance
top_teams_for_endorsement <- team_performance %>%
  mutate(overall_team_performance_score = (team1_win_percentage + team2_win_percentage) / 2) %>%
  arrange(desc(overall_team_performance_score)) %>%
  head(5)

print(top_teams_for_endorsement)

