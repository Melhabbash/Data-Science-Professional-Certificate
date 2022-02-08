# A way to actually pick the players for the team can be done using what computer scientists call 
# linear programming. Although we don't go into this topic in detail in this course, we include the 
# code anyway:

library(reshape2)
library(lpSolve)

players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE)

# This algorithm chooses these 9 players:
  
  our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

# nameFirst    nameLast POS   salary R_hat
# 1     Jason      Giambi  1B 10428571  7.99
# 2     Nomar Garciaparra  SS  9000000  7.51
# 3      Mike      Piazza   C 10571429  7.16
# 4      Phil       Nevin  3B  2600000  6.75
# 5      Jeff        Kent  2B  6000000  6.68
# We note that these players all have above average BB and HR rates while the same is not true 
# for singles.

my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))
# nameFirst    nameLast    BB singles doubles triples    HR  AVG R_hat
# 1     Jason      Giambi 3.317 -0.5315   0.754  -0.675 2.067 2.63  3.54
# 2     Nomar Garciaparra 0.284  1.7330   2.651   0.471 1.003 3.95  2.97
# 3      Mike      Piazza 0.596 -0.0499  -0.177  -1.335 2.682 1.70  2.56
# 4      Phil       Nevin 0.790 -0.6751   0.670  -1.137 2.103 1.09  2.07
# 5      Jeff        Kent 0.875 -0.2717   1.833   1.210 0.967 1.66  2.00