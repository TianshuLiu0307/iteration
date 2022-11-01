iteration_and_listcols
================
Tianshu Liu
2022-11-01

``` r
vec_logical = c(TRUE, FALSE, TRUE, TRUE)
```

``` r
l = list(
  vec_numeric = 5:8,
  mat         = matrix(1:8, 2, 4),
  vec_logical = c(TRUE, FALSE),
  summary     = summary(rnorm(1000))
)
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $vec_logical
    ## [1]  TRUE FALSE
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.14202 -0.68290  0.05669  0.01904  0.71824  2.78492

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[["mat"]]
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8

``` r
l[3]
```

    ## $vec_logical
    ## [1]  TRUE FALSE

``` r
l[[3]]
```

    ## [1]  TRUE FALSE

``` r
l[[3]][-1]
```

    ## [1] FALSE

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

``` r
list_norm = list(
  a = rnorm(20,5,4),
  b = rnorm(20,-12,3),
  c = rnorm(20,3,4),
  d = rnorm(20,8,1)
)
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.47  3.25

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -11.0  4.14

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.54  4.43

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  8.07  1.07

``` r
# for loop
output = vector("list", length = 4)
for (i in 1:4){
  output[[i]] = mean_and_sd(list_norm[[i]])
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.47  3.25
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -11.0  4.14
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.54  4.43
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  8.07  1.07

``` r
# map
map(list_norm, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.47  3.25
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -11.0  4.14
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.54  4.43
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  8.07  1.07

``` r
map(list_norm, summary)
```

    ## $a
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -1.901   3.768   5.188   5.465   7.103  12.969 
    ## 
    ## $b
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -18.669 -13.063 -11.069 -10.987  -7.938  -4.477 
    ## 
    ## $c
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.977   0.372   2.818   2.539   5.362  10.819 
    ## 
    ## $d
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   5.700   7.677   7.940   8.070   9.004   9.680

``` r
map_dbl(list_norm, median)
```

    ##          a          b          c          d 
    ##   5.188285 -11.068684   2.817647   7.940351

``` r
map_df(list_norm, mean_and_sd)
```

    ## # A tibble: 4 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1   5.47  3.25
    ## 2 -11.0   4.14
    ## 3   2.54  4.43
    ## 4   8.07  1.07

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    norm = list_norm
  )

listcol_df[["norm"]]
```

    ## $a
    ##  [1]  2.276844  6.555062  8.134206  5.788521 11.260349  7.037403  3.951809
    ##  [8]  4.602726  3.215695  2.724041 -1.900827  5.223395  7.477093  7.299045
    ## [15]  4.277442  5.153176  2.303978  6.348244 12.968938  4.612752
    ## 
    ## $b
    ##  [1]  -8.505407 -12.525220  -6.558852  -5.151813  -7.706858  -8.014835
    ##  [7] -12.902237 -12.352395 -13.545695 -14.687362 -12.492516 -16.335227
    ## [13] -12.501603 -18.669490  -8.351006  -4.476729  -7.115717 -18.607032
    ## [19]  -9.461951  -9.784972
    ## 
    ## $c
    ##  [1]  3.627337  5.290680 -4.977392  2.906280  5.574913 10.819453  1.921115
    ##  [8]  5.274040 -1.248676  0.912166  5.812301 -4.107500  4.757969 -2.201102
    ## [15]  1.378003  2.729015  6.196670  9.727575  1.199306 -4.811960
    ## 
    ## $d
    ##  [1] 7.703354 5.700095 9.331776 7.249223 9.679759 8.535097 7.774260 6.709970
    ##  [9] 7.769976 9.014833 7.739571 9.000245 7.611633 9.169433 6.315581 8.785488
    ## [17] 9.402218 7.699099 8.108975 8.106443

``` r
map_df(listcol_df[["norm"]], mean_and_sd)
```

    ## # A tibble: 4 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1   5.47  3.25
    ## 2 -11.0   4.14
    ## 3   2.54  4.43
    ## 4   8.07  1.07

``` r
listcol_df %>% 
  mutate(
    m_sd = map(norm, mean_and_sd)
  ) %>% 
  select(-norm) 
```

    ## # A tibble: 4 × 2
    ##   name  m_sd            
    ##   <chr> <named list>    
    ## 1 a     <tibble [1 × 2]>
    ## 2 b     <tibble [1 × 2]>
    ## 3 c     <tibble [1 × 2]>
    ## 4 d     <tibble [1 × 2]>

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

``` r
weather_nest_df = 
  weather_df %>% 
  nest(data = date:tmin)

weather_df %>% count(name, id)
```

    ## # A tibble: 3 × 3
    ##   name           id              n
    ##   <chr>          <chr>       <int>
    ## 1 CentralPark_NY USW00094728   365
    ## 2 Waikiki_HA     USC00519397   365
    ## 3 Waterhole_WA   USS0023B17S   365

``` r
weather_nest_df
```

    ## # A tibble: 3 × 3
    ##   name           id          data              
    ##   <chr>          <chr>       <list>            
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]>
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]>
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]>

``` r
weather_nest_df[["data"]]
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # … with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

``` r
weather_nest_df[["data"]][[1]]
```

    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows

``` r
# linear regression model
lm(tmax~tmin, data = weather_nest_df[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest_df[["data"]][[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
lm(tmax~tmin, data = weather_nest_df[["data"]][[2]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest_df[["data"]][[2]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509

``` r
lm(tmax~tmin, data = weather_nest_df[["data"]][[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest_df[["data"]][[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
weather_lm = function(df){
  
  lm(formula = tmax~tmin, data = df)
  
}

map(weather_nest_df[["data"]], weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
weather_nest_df %>% 
  mutate(
    models = map(data, weather_lm)
  )
```

    ## # A tibble: 3 × 4
    ##   name           id          data               models
    ##   <chr>          <chr>       <list>             <list>
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>  
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>  
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>

``` r
# unnest
weather_nest_df %>% 
  unnest(data)
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

``` r
read_page_reviews <- function(url) {
  
  html = read_html(url)
  
  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim() %>% 
    str_subset("The media could not be loaded.", negate = TRUE) %>% 
    str_subset("^$", negate = TRUE)
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}

base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

vec_urls = str_c(base_url, 1:5)

map(vec_urls, read_page_reviews)
```

    ## [[1]]
    ## # A tibble: 10 × 3
    ##    title                                stars text                              
    ##    <chr>                                <dbl> <chr>                             
    ##  1 Still the best                           5 Completely stupid, absolutely no …
    ##  2 70’s and 80’s Schtick Comedy             5 …especially funny if you have eve…
    ##  3 Amazon Censorship                        5 I hope Amazon does not censor my …
    ##  4 Watch to say you did                     3 I know it's supposed to be a cult…
    ##  5 Best Movie Ever!                         5 We just love this movie and even …
    ##  6 Quirky                                   5 Good family film                  
    ##  7 Funny movie - can't play it !            1 Sony 4k player won't even recogni…
    ##  8 A brilliant story about teenage life     5 Napoleon Dynamite delivers dry hu…
    ##  9 HUHYAH                                   5 Spicy                             
    ## 10 Cult Classic                             4 Takes a time or two to fully appr…
    ## 
    ## [[2]]
    ## # A tibble: 10 × 3
    ##    title                                         stars text                     
    ##    <chr>                                         <dbl> <chr>                    
    ##  1 Sweet                                             5 Timeless Movie. My Grand…
    ##  2 Cute                                              4 Fun                      
    ##  3 great collectible                                 5 one of the greatest movi…
    ##  4 Iconic, hilarious flick ! About friend ship .     5 Who doesn’t love this mo…
    ##  5 Funny                                             5 Me and my dad watched th…
    ##  6 Low budget but okay                               3 This has been a classic …
    ##  7 Disappointing                                     2 We tried to like this, b…
    ##  8 Favorite movie 🍿                                 5 This is one of my favori…
    ##  9 none                                              5 this movie was great Nap…
    ## 10 Great movie                                       5 Vote for pedro           
    ## 
    ## [[3]]
    ## # A tibble: 10 × 3
    ##    title                                                             stars text 
    ##    <chr>                                                             <dbl> <chr>
    ##  1 Get this to improve your nunchuck and bowstaff skills. Dancing i…     5 "Got…
    ##  2 Incredible Movie                                                      5 "Fun…
    ##  3 Always loved this movie!                                              5 "I h…
    ##  4 Great movie                                                           5 "Bou…
    ##  5 The case was damaged                                                  3 "It …
    ##  6 It’s classic                                                          5 "Cle…
    ##  7 Irreverent comedy                                                     5 "If …
    ##  8 Great classic!                                                        5 "Fun…
    ##  9 Most Awesomsomest Movie EVER!!!                                       5 "Thi…
    ## 10 Always a favorite                                                     5 "I r…
    ## 
    ## [[4]]
    ## # A tibble: 10 × 3
    ##    title                                                             stars text 
    ##    <chr>                                                             <dbl> <chr>
    ##  1 It’s not working the disc keeps showing error when I tried other…     1 "It’…
    ##  2 Gosh!                                                                 5 "Eve…
    ##  3 An Acquired Taste                                                     1 "Thi…
    ##  4 What is this ?                                                        4 "Nic…
    ##  5 Napoleon Dynamite                                                     2 "I w…
    ##  6 Great movie                                                           5 "Gre…
    ##  7 Good movie                                                            5 "Goo…
    ##  8 Came as Described                                                     5 "Cam…
    ##  9 Oddly on my list of keepers.                                          5 "Goo…
    ## 10 Low budget fun                                                        5 "Odd…
    ## 
    ## [[5]]
    ## # A tibble: 10 × 3
    ##    title                                                             stars text 
    ##    <chr>                                                             <dbl> <chr>
    ##  1 On a scale of 1 to 10 this rates a minus                              1 "Thi…
    ##  2 I always wondered...                                                  5 "wha…
    ##  3 Audio/video not synced                                                1 "I t…
    ##  4 Kind of feels like only a bully would actually laugh at this...       1 "...…
    ##  5 movie                                                                 5 "goo…
    ##  6 An Overdose of Comical Cringe                                         5 "Exc…
    ##  7 Glad I never wasted money on this                                     2 "I r…
    ##  8 A little disappointed                                                 3 "The…
    ##  9 An (almost) gem. Brought me back to the sweet awkwardness of hig…     5 "To …
    ## 10 How Could You Not Love Napoleon??                                     5 "I r…

``` r
napoleon_reviews = 
  tibble(
    page = 1:5,
    page_url = str_c(base_url, page)
  ) %>% 
  mutate(reviews = map(page_url, read_page_reviews))

tibble(
    page = 1:5,
    page_url = str_c(base_url, page),
    reviews = map(page_url, read_page_reviews))
```

    ## # A tibble: 5 × 3
    ##    page page_url                                                        reviews 
    ##   <int> <chr>                                                           <list>  
    ## 1     1 https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_ar… <tibble>
    ## 2     2 https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_ar… <tibble>
    ## 3     3 https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_ar… <tibble>
    ## 4     4 https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_ar… <tibble>
    ## 5     5 https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_ar… <tibble>
