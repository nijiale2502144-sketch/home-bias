library(tidytuesdayR)
library(tidyverse)

soccer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-04-04/soccer21-22.csv')

head(soccer,5)

#The total score for each team
team_points <- soccer |> 
  mutate(
    home_p = case_when(FTR == "H" ~ 3, FTR == "D" ~ 1, TRUE ~ 0),
    away_p = case_when(FTR == "A" ~ 3, FTR == "D" ~ 1, TRUE ~ 0)
  )
standing <- bind_rows(
  team_points |>  select(Team = HomeTeam, Pts = home_p),
  team_points |>  select(Team = AwayTeam, Pts = away_p)
) |>
  group_by(Team)  |> 
  summarise(Season_Pts = sum(Pts)) |>
  arrange(desc(Season_Pts))



#Different referees' home and away card ratios
bias_data_Referee <-soccer |> 
  group_by(Referee) |> 
  summarise(
    Average_Home_Fouls = sum (HF)/n(),
    Average_Home_Yellows = sum(HY)/n(),
    Average_Home_Reds = sum(HR)/n(),
    Average_Away_Fouls = sum (AF)/n(),
    Average_Away_Yellows = sum(AY)/n(),
    Average_Away_Reds = sum(AR)/n(),
    Home_rate = (Average_Home_Yellows+2*Average_Home_Reds)/Average_Home_Fouls,
    Away_rate = (Average_Away_Yellows+2*Average_Away_Reds)/Average_Away_Fouls,
    number_of_games =n()
  )


bias_data_Referee |> 
  filter(number_of_games>5) |> 
  pivot_longer(cols = c(Home_rate, Away_rate), names_to = "Role", values_to = "Card_Rate") |> 
  ggplot(aes(y=Referee,x=Card_Rate,colour = Role,group=Role))+
  geom_path()+
  geom_point()
  


match_analysis <-soccer |>
  mutate(
    # If there is no foul, the probability of giving a card is 0.
    Home_Rate = ifelse(HF > 0, (HY+2*HR) / HF, 0),
    Away_Rate = ifelse(AF > 0, (AY+2*AR) / AF, 0),
    Rate_Diff = Away_Rate - Home_Rate
  ) |> 
  left_join(standing, by = c("HomeTeam" = "Team")) |> 
  rename(Home_Season_Pts = Season_Pts) |> 
  left_join(standing, by = c("AwayTeam" = "Team")) |> 
  rename(Away_Season_Pts = Season_Pts) |> 
  mutate(Strength_Diff = Home_Season_Pts - Away_Season_Pts)
  
t.test(match_analysis$Home_Rate, match_analysis$Away_Rate, paired = TRUE)
wilcox.test(match_analysis$Home_Rate, match_analysis$Away_Rate, paired = TRUE)

referee_test <- match_analysis |>
  group_by(Referee) |>
  filter(n() >= 10) |>  # Filter out referees with insufficient officiating experience
  summarise(
    n_games = n(),
    mean_home = mean(Home_Rate),
    mean_away = mean(Away_Rate),
    # Calculate the p-value for a paired test.
    p_val = t.test(Home_Rate, Away_Rate, paired = TRUE)$p.value,
    bias_score = mean_away - mean_home
  ) |>
  arrange(p_val) 

referee_test |> 
  select(Referee,p_val) |> 
  print()


htr_impact <- match_analysis |>
  group_by(Referee, HTR) |>
  summarise(
    avg_home_rate = mean(Home_Rate),
    avg_away_rate = mean(Away_Rate),
    count = n()
  )

# Visualization difference in card rate under different half scores
ggplot(htr_impact, aes(x = HTR, y = avg_away_rate - avg_home_rate, fill = HTR)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Referee) +
  labs(title = "Difference in card rate under different half scores (Away - Home)")



# Test if there is bias when the home team is trailing at halftime.
home_trailing <- match_analysis |> filter(HTR == "A")

home_trailing |> summarise(total = sum(Rate_Diff, na.rm = TRUE))

t.test(home_trailing$Home_Rate, home_trailing$Away_Rate, paired = TRUE)

t.test(home_trailing$HF, home_trailing$AF, paired = TRUE)


mean(match_analysis$Away_Rate - match_analysis$Home_Rate, na.rm=TRUE)
mean(match_analysis$Rate_Diff, na.rm=TRUE)



bias_model <- lm(Rate_Diff ~ Referee + Strength_Diff, data = match_analysis)


summary(bias_model)


