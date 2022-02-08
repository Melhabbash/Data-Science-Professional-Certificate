# The code to create a table with player ID, their names, and their most played position:
  library(Lahman)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)
# The code to create a table with only the ROY award winners and add their batting statistics:
  ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")
# The code to keep only the rookie and sophomore seasons and remove players who did not play sophomore seasons:
  ROY <- ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)
# The code to use the spread function to have one column for the rookie and sophomore years batting
  # averages:
  ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY
#> # A tibble: 99 x 6
#>   playerID  rookie_year nameFirst nameLast rookie sophomore
#>   <chr>           <int> <chr>     <chr>     <dbl>     <dbl>
#> 1 mccovwi01        1959 Willie    McCovey   0.354     0.238
#> 2 suzukic01        2001 Ichiro    Suzuki    0.350     0.321
#> 3 bumbral01        1973 Al        Bumbry    0.337     0.233
#> 4 lynnfr01         1975 Fred      Lynn      0.331     0.314
#> 5 pujolal01        2001 Albert    Pujols    0.329     0.314
#> 6 troutmi01        2012 Mike      Trout     0.326     0.323
#> # ... with 93 more rows

# The code to calculate the proportion of players who have a lower batting average their 
# sophomore year:
  mean(ROY$sophomore - ROY$rookie <= 0)
#> [1] 0.677

# The code to do the similar analysis on all players that played the 2013 and 2014 seasons and batted more than 130 times (minimum to win Rookie of the Year):
  two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)
two_years
#> # A tibble: 312 x 4
#>   nameFirst nameLast `2013` `2014`
#>   <chr>     <chr>     <dbl>  <dbl>
#> 1 Miguel    Cabrera   0.348  0.313
#> 2 Hanley    Ramirez   0.345  0.283
#> 3 Michael   Cuddyer   0.331  0.332
#> 4 Scooter   Gennett   0.324  0.289
#> 5 Joe       Mauer     0.324  0.277
#> 6 Mike      Trout     0.323  0.287
#> # ... with 306 more rows
The code to see what happens to the worst performers of 2013:
  arrange(two_years, `2013`)
#> # A tibble: 312 x 4
#>   nameFirst nameLast `2013` `2014`
#>   <chr>     <chr>     <dbl>  <dbl>
#> 1 Danny     Espinosa  0.158  0.219
#> 2 Dan       Uggla     0.179  0.149
#> 3 Jeff      Mathis    0.181  0.2  
#> 4 Melvin    Upton     0.184  0.208
#> 5 Adam      Rosales   0.190  0.262
#> 6 Aaron     Hicks     0.192  0.215
#> # ... with 306 more rows
# The code to see  the correlation for performance in two separate years:
  qplot(`2013`, `2014`, data = two_years)

summarize(two_years, cor(`2013`,`2014`))
#> # A tibble: 1 x 1
#>   `cor(\`2013\`, \`2014\`)`
#>                       <dbl>
#> 1                     0.460

