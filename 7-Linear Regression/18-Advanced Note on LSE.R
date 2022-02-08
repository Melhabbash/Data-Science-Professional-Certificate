# Although interpretation is not straight-forward, it is also useful to know that the LSE can 
# be strongly correlated, which can be seen using this code:
  
lse %>% summarize(cor(beta_0, beta_1))

# However, the correlation depends on how the predictors are defined or transformed.

# Here we standardize the father heights, which changes x_i to (x_i-x_bar).

B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})
# Observe what happens to the correlation in this case:
  
  cor(lse[1,], lse[2,]) 
  