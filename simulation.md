simulation
================
Tianshu Liu
2022-11-03

``` r
sim_mean_sd = function(n_obs, mu = 7, sigma = 3) {
  
  sim_data = tibble(
    x = rnorm(n_obs, mean = mu, sd = sigma),
  )
  
  sim_data %>% 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )
}
```

# Simulation

``` r
sim_mean_sd(n_obs = 50)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   6.59      3.70

``` r
output = vector("list", length = 100)

for (i in 1:100){
  output[[i]] = sim_mean_sd(n_obs = 30)
}

bind_rows(output)
```

    ## # A tibble: 100 × 2
    ##    mu_hat sigma_hat
    ##     <dbl>     <dbl>
    ##  1   7.19      2.63
    ##  2   6.45      2.83
    ##  3   5.62      2.72
    ##  4   6.70      3.29
    ##  5   6.89      2.93
    ##  6   7.40      2.95
    ##  7   7.01      3.49
    ##  8   7.17      2.45
    ##  9   7.15      2.67
    ## 10   6.78      2.58
    ## # … with 90 more rows

``` r
# Combination of ineration numbers
expand_grid(
  sample_size = 30,
  iteration = 1:100
)
```

    ## # A tibble: 100 × 2
    ##    sample_size iteration
    ##          <dbl>     <int>
    ##  1          30         1
    ##  2          30         2
    ##  3          30         3
    ##  4          30         4
    ##  5          30         5
    ##  6          30         6
    ##  7          30         7
    ##  8          30         8
    ##  9          30         9
    ## 10          30        10
    ## # … with 90 more rows

``` r
sim_results_df = 
  expand_grid(
    sample_size = 30,
    iteration = 1:100
  ) %>% 
  mutate(
    estimated_df = map(sample_size, sim_mean_sd)
  ) %>% 
  unnest(estimated_df)

sim_results_df
```

    ## # A tibble: 100 × 4
    ##    sample_size iteration mu_hat sigma_hat
    ##          <dbl>     <int>  <dbl>     <dbl>
    ##  1          30         1   7.77      3.06
    ##  2          30         2   6.34      2.56
    ##  3          30         3   6.62      2.25
    ##  4          30         4   8.32      2.78
    ##  5          30         5   7.23      3.01
    ##  6          30         6   7.97      3.24
    ##  7          30         7   7.05      2.78
    ##  8          30         8   7.54      2.50
    ##  9          30         9   6.48      2.95
    ## 10          30        10   7.02      2.70
    ## # … with 90 more rows

``` r
sim_results_df %>% 
  ggplot(aes(x = mu_hat)) + 
  geom_density()
```

<img src="simulation_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

``` r
sim_results_df = 
  expand_grid(
    sample_size = c(30, 60, 120, 240),
    iteration = 1:100
  ) %>% 
  mutate(
    estimated_df = map(sample_size, sim_mean_sd)
  ) %>% 
  unnest(estimated_df)

sim_results_df
```

    ## # A tibble: 400 × 4
    ##    sample_size iteration mu_hat sigma_hat
    ##          <dbl>     <int>  <dbl>     <dbl>
    ##  1          30         1   6.75      2.88
    ##  2          30         2   7.55      3.12
    ##  3          30         3   6.07      3.10
    ##  4          30         4   6.92      3.16
    ##  5          30         5   8.14      2.90
    ##  6          30         6   7.71      2.70
    ##  7          30         7   6.60      3.08
    ##  8          30         8   6.45      3.17
    ##  9          30         9   6.60      2.36
    ## 10          30        10   7.13      3.34
    ## # … with 390 more rows

``` r
sim_results_df %>% 
  mutate(
    sample_size = str_c("N = ", sample_size),
    sample_size = fct_inorder(sample_size)
  ) %>% 
  ggplot(aes(x = sample_size, y = mu_hat, fill = sample_size)) + 
  geom_violin()
```

<img src="simulation_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

``` r
sim_results_df = 
  expand_grid(
    sample_size = c(30, 60, 120, 240),
    true_sigma = c(6, 3),
    iteration = 1:100
  ) %>% 
  mutate(
    estimated_df = 
      map2(.x = sample_size, .y = true_sigma, ~sim_mean_sd(n_obs = .x, sigma = .y))
  ) %>% 
  unnest(estimated_df)

sim_results_df
```

    ## # A tibble: 800 × 5
    ##    sample_size true_sigma iteration mu_hat sigma_hat
    ##          <dbl>      <dbl>     <int>  <dbl>     <dbl>
    ##  1          30          6         1   6.00      4.97
    ##  2          30          6         2   7.79      5.03
    ##  3          30          6         3   6.22      5.51
    ##  4          30          6         4   7.82      6.63
    ##  5          30          6         5   8.28      6.34
    ##  6          30          6         6   6.82      5.55
    ##  7          30          6         7   6.78      4.64
    ##  8          30          6         8   8.32      5.65
    ##  9          30          6         9   7.84      5.51
    ## 10          30          6        10   6.49      4.82
    ## # … with 790 more rows

``` r
sim_results_df %>% 
  mutate(
    sample_size = str_c("N = ", sample_size),
    sample_size = fct_inorder(sample_size)
  ) %>% 
  ggplot(aes(x = sample_size, y = mu_hat, fill = sample_size)) + 
  geom_violin() + 
  facet_grid(.~true_sigma)
```

<img src="simulation_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />
