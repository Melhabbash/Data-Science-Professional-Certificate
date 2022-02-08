# The code to use dslabs function rfalling_object to generate simulations of dropping balls:
  library(dslabs)
falling_object <- rfalling_object()
# The code to draw the trajectory of the ball:
  falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  xlab("Time in seconds")
# The code to use the lm() function to estimate the coefficients:
  fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)

tidy(fit)
#> # A tibble: 3 x 5
#>   term        estimate std.error statistic  p.value
#>   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
#> 1 (Intercept)    56.9      0.580     98.0  1.56e-17
#> 2 time           -1.04     0.829     -1.25 2.36e- 1
#> 3 time_sq        -4.73     0.246    -19.2  8.17e-10
# The code to check if the estimated parabola fits the data:
  augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")
# The code to see the summary statistic of the regression:
  tidy(fit, conf.int = TRUE)
#> # A tibble: 3 x 7
#>   term        estimate std.error statistic  p.value conf.low conf.high
#>   <chr>          <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
#> 1 (Intercept)    56.9      0.580     98.0  1.56e-17    55.6     58.2  
#> 2 time           -1.04     0.829     -1.25 2.36e- 1    -2.86     0.784
#> 3 time_sq        -4.73     0.246    -19.2  8.17e-10    -5.27    -4.19
  